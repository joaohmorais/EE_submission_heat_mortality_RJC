library(tidyverse)
library(patchwork)
library(mgcv)

do_df <- read_csv("data/do_data.csv", show_col_types = F)
feriados <- read_csv("data/feriados.csv", show_col_types = F)

cdc_groups <- get_cdc_groups()

do_df <- do_df %>% 
  left_join(cdc_groups %>% select(group_id, grupo),
            by="group_id") %>% 
  relocate(grupo, .after=group_id)

do_df <- do_df %>% 
  mutate(dia_sem = wday(data_obito, label = T),
         feriado = data_obito %in% feriados$data,
         dia_group = case_when(
           dia_sem == "Sun" ~ "Sun/Holiday",
           feriado ~ "Sun/Holiday",
           TRUE ~ as.character(dia_sem)
         ) %>% factor(levels = c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun/Holiday"), ordered = F))

summary_metrics <- summary_metrics %>% 
  mutate(
    
    # whether include in model
    include_in_model = median > 1 & perc_zero < 0.2,
    
    # likelihood choices
    likelihood = case_when(
      var_over_mean > 1.6 ~ "quasipoisson",
      var_over_mean <= 1.6 ~ "poisson"
    )
  )

do_df <- do_df %>% 
  left_join(summary_metrics %>% select(grupo, age_group, include_in_model, likelihood), by=c("grupo", "age_group")) %>% 
  filter(include_in_model==1) %>% 
  nest(.by=c(group_id, grupo, age_group, likelihood)) %>% 
  mutate(
    mod_gam = map2(data, likelihood, function(df, l) {
      gam(
        n ~ 1 + s(time_id, k=6, fx=F, bs = "cr") + s(dia_ano, k=6, bs="cc") + dia_group, 
        data = df,
        family = l
      )
    }
    ),
    terms = map(mod_gam, function(m) {
      predict(m, newdata=m$model, se.fit=TRUE, type="terms")
    }),
    terms_df = map2(mod_gam, terms, function(m, t) {
      
      tibble(
        m$model %>% select(time_id:dia_ano, dia_group),
        constant = attr(m, "constant"),
        fit_trend = t$fit[,2],
        se_trend = t$se.fit[,2],
        fit_dia_ano = t$fit[,3],
        se_dia_ano = t$se.fit[,3],
        fit_dia_group = t$fit[,1],
        se_dia_group = t$se.fit[,1]
      )
    }),
    trend = map2(terms_df,data, function(t, df) {
      t %>% 
        select(time_id, fit=fit_trend, se=se_trend) %>% 
        left_join(df %>% select(data_obito, time_id), by="time_id") %>% 
        mutate(low = fit - 1.96*se,
               upp = fit + 1.96*se) %>% 
        relocate(data_obito, .before = everything())
    }),
    season = map(terms_df, function(t) {
      t %>% 
        select(dia_ano, fit=fit_dia_ano, se=se_dia_ano) %>% 
        distinct() %>% 
        mutate(low = fit - 1.96*se,
               upp = fit + 1.96*se)
    }),
    dia_group = map(terms_df, function(t) {
      t %>% select(dia_group, fit=fit_dia_group, se=se_dia_group) %>% 
        distinct() %>% 
        arrange(dia_group) %>% 
        mutate(low = fit - 1.96*se, upp = fit + 1.96*se)
    })
    
  )

trend_df <- do_df %>% select(group_id:likelihood, trend) %>% unnest(trend)
season_df <- do_df %>% select(group_id:likelihood, season) %>% unnest(season)
dia_group_df <- do_df %>% select(group_id:likelihood, dia_group) %>% unnest(dia_group)

if (FALSE) {
  save(trend_df, season_df, dia_group_df, file = paste0("results/model_struc_results_", format(today(), "%Y_%m_%d"), ".Rdata"))
}

# Figure S5 - Trend ----

g_s5 <- trend_df %>% 
  mutate(
    grupo = translate_cdc_groups(grupo, reduced=2),
    age_group = factor(
      age_group, levels=c("Jovem", "Idoso"), labels=c("0-64", "65+")
    )
  ) %>% 
  ggplot(aes(x=data_obito, y=fit)) +
  scale_x_date(name="Date of death", breaks = ymd("2012-01-01") + years(seq(0,12,by=4)), date_labels = "%Y") +
  labs(y="Trend coefficient") +
  geom_ribbon(aes(ymin=low, ymax=upp, fill = age_group, group = age_group), alpha=0.2) + 
  geom_line(aes(group=age_group, color=age_group)) + 
  geom_hline(yintercept = 0, linetype="dashed") +
  labs(color = "Age group", fill = "Age group") +
  geom_vline(xintercept = ymd("2020-03-11"), linetype="dotted", linewidth=0.5, color = "gray20") +
  facet_wrap(~grupo) + 
  theme_bw() +
  theme(legend.position = "top")

ggsave(
  g_s5,
  filename = "img/supp_5.tiff",
  width = 8.4,
  height = 6,
  units = "in",
  dpi = 300,
  bg="white"
)

# Figure S6 - Season ----

g_s6 <- season_df %>% 
  mutate(
    grupo = translate_cdc_groups(grupo, reduced=2),
    age_group = factor(
      age_group, levels=c("Jovem", "Idoso"), labels=c("0-64", "65+")
    )
  ) %>% 
  ggplot(aes(x=dia_ano, y=fit)) +
  scale_x_continuous(breaks = c(1, seq(100, 300, by=100), 366), name = "Day of year (doy)") +
  labs(y="Seasonality coefficient") +
  geom_ribbon(aes(ymin=low, ymax=upp, fill = age_group, group = age_group), alpha=0.2) + 
  geom_line(aes(group=age_group, color=age_group)) + 
  geom_hline(yintercept = 0, linetype="dashed") +
  labs(color = "Age group", fill = "Age group") +
  facet_wrap(~grupo) + 
  theme_bw() +
  theme(legend.position = "top")

ggsave(
  g_s6,
  filename = "img/supp_6.tiff",
  width = 8.4,
  height = 6,
  units = "in",
  dpi = 300,
  bg="white"
)

# Figure S7 - Dow ----

g_s7 <- 
  dia_group_df %>% 
  mutate(
    grupo = translate_cdc_groups(grupo, reduced=2),
    age_group = factor(
      age_group, levels=c("Jovem", "Idoso"), labels=c("0-64", "65+")
    )
  ) %>% 
  filter(dia_group != "Mon") %>% 
  ggplot(aes(x=interaction(age_group, dia_group), y=fit)) + 
  geom_point(aes(group=age_group, color=age_group, shape=age_group)) + 
  geom_errorbar(aes(ymin=low, ymax=upp, color=age_group, group=age_group)) +
  scale_x_discrete(
    labels = function(x) {
      case_when(
        grepl("65+", x) ~ "",
        TRUE ~ paste0("", gsub("0-64.", "", x))
      )
    },
    name = "Day of week (dow)"
  ) +
  labs(y="Coefficient", color = "Age group", shape = "Age group") +
  geom_hline(yintercept = 0, linetype="dashed") +
  theme_minimal() +
  facet_wrap(~grupo) + 
  theme(axis.text.x = element_text(angle=90, hjust=1)) + 
  theme(legend.position = "top")

ggsave(
  g_s7,
  filename = "img/supp_7.tiff",
  width = 8.4,
  height = 6,
  units = "in",
  dpi = 300,
  bg="white"
)
