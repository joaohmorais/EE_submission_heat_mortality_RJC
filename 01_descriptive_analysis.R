library(tidyverse)
library(gt)

source("00_aux_functions.R")

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

# Table 2 - Summary metrics for deaths ----

summary_metrics <- 
  do_df %>% 
  summarise(total = sum(n),
            med = mean(n),
            median = median(n),
            variance = var(n),
            var_over_mean = variance/med,
            days_zero = sum(n==0),
            total_days = n(),
            perc_zero = days_zero/total_days,
            .by = c(grupo, age_group))

summary_metrics %>% 
  mutate(grupo = translate_cdc_groups(grupo),
         age_group = factor(
           age_group, levels=c("Jovem", "Idoso"), labels=c("0-64", "65+")
         )) %>% 
  arrange(age_group, grupo) %>% 
  select(-total_days, -days_zero, -age_group) %>% 
  gt() %>% 
  fmt_number(columns = med:var_over_mean, decimals = 2) %>% 
  fmt_percent(columns = perc_zero, decimals = 2) %>% 
  fmt_number(columns = c(total), decimals = 0) %>% 
  cols_label(
    grupo = "Cause of death",
    #age_group = "Age group",
    total = "Total deaths",
    med = "Mean",
    median = "Median",
    variance = "Var.",
    var_over_mean = "Var/Mean.",
    perc_zero = "Percent zero"
  ) %>% 
  gtsave(filename = "results/summary_deaths.docx")

# Figure S2 - Histogram for Deaths ----

g_s2 <- 
  do_df %>% 
  mutate(grupo = translate_cdc_groups(grupo, reduced = 2),
         age_group = factor(
           age_group, levels=c("Jovem", "Idoso"), labels=c("0-64", "65+")
         )) %>% 
  ggplot(aes(x=n)) + 
  geom_histogram(aes(group=age_group, fill=age_group), 
                 position = "identity",
                 binwidth = 1, 
                 boundary = 0, 
                 alpha=0.5) +
  facet_wrap(~grupo, scales="free") + 
  scale_fill_discrete(name = "Age group") +
  scale_y_continuous(name = "Frequency") +
  scale_x_continuous(name = "Daily count") +
  theme_bw() + 
  theme(legend.position = "top",
        axis.text = element_text(size=6))

ggsave(
  g_s2,
  filename = "img/supp_2.tiff",
  width = 8.4,
  height = 6,
  units = "in",
  dpi = 300
)

# Table 3 - Summary metrics for temp ----

temp_df <- read_csv("data/temp_df.csv", show_col_types = F) %>% 
  select(dia, ends_with("_med")) 

temp_df_long <- temp_df %>% 
  pivot_longer(cols = ends_with("_med"), names_to = "metric", values_to = "value")

hourly_temp <- 
  read_csv("data/hourly_temp_2012_2024.csv", show_col_types = F) %>% 
  mutate(
    dt_registro = with_tz(dt_registro, "America/Sao_Paulo")
  )

hourly_Q90 <- quantile(hourly_temp$heat_index, probs = 0.9, na.rm=T)
hourly_Q95 <- quantile(hourly_temp$heat_index, probs = 0.95, na.rm=T)
hourly_Q975 <- quantile(hourly_temp$heat_index, probs = 0.975, na.rm=T)

num_hr_df <- hourly_temp %>% 
  mutate(dia=as.Date(format(dt_registro, "%Y-%m-%d"))) %>% 
  summarise(
    num_hr_q90 = sum(heat_index >= hourly_Q90, na.rm=T),
    num_hr_q95 = sum(heat_index >= hourly_Q95, na.rm=T),
    num_hr_q975 = sum(heat_index >= hourly_Q975, na.rm=T),
    .by=dia
  )

num_hr_df <- num_hr_df %>% 
  mutate(
    hr_above_q90 = as.numeric(num_hr_q90 > quantile(num_hr_df$num_hr_q90, probs=0.9)),
    hr_above_q95 = as.numeric(num_hr_q95 > quantile(num_hr_df$num_hr_q95, probs=0.9)),
    hr_above_q975 = as.numeric(num_hr_q975 > quantile(num_hr_df$num_hr_q975, probs=0.9)),
  )


summary_metrics_temp <- 
  temp_df_long  %>% 
  mutate(dt_registro = as.POSIXct(dia)) %>% 
  select(dt_registro, metric, value) %>% 
  add_row(
    hourly_temp %>% 
      select(-num_estacoes) %>% 
      rename(hourly_t = temperatura, hourly_hi = heat_index) %>% 
      pivot_longer(cols = c(hourly_t, hourly_hi),
                   names_to = "metric", values_to = "value") %>% 
      filter(!is.na(value))
  ) %>% 
  add_row(
    num_hr_df %>% 
      select(dia, starts_with("num_hr")) %>% 
      pivot_longer(cols = starts_with("num_hr"),
                   names_to = "metric", values_to = "value") %>% 
      mutate(dt_registro = as.POSIXct(dia)) %>% 
      select(dt_registro, metric, value)
  ) %>% 
  arrange(dt_registro, metric) %>% 
  summarise(
    min = min(value),
    q01 = quantile(value, probs=0.01),
    q025 = quantile(value, probs=0.025),
    q05 = quantile(value, probs=0.05),
    q10 = quantile(value, probs=0.1),
    med = mean(value),
    q50 = quantile(value, probs=0.5),
    q90 = quantile(value, probs=0.9),
    q95 = quantile(value, probs=0.95),
    q975 = quantile(value, probs=0.975),
    q99 = quantile(value, probs=0.99),
    max = max(value),
    var = var(value),
    .by=c(metric)
  )

summary_metrics_temp %>% 
  mutate(metric = factor(metric,
                         levels = c("t_med", "hi_med", "hourly_t", "hourly_hi", "num_hr_q90", "num_hr_q95", "num_hr_q975"),
                         labels = c("Tmed", "HImed", "Hourly T", "Hourly HI", "Hours Q90", "Hours Q95", "Hours Q975")
  )) %>% 
  arrange(metric) %>% 
  select(metric, min, q10:var) %>% 
  gt() %>% 
  fmt_number(columns = min:var, decimals = 2) %>% 
  cols_label(
    metric = "Variable",
    min = "Min.",
    q10 = "Q10",
    med = "Mean",
    q50 = "Median",
    q90 = "Q90",
    q95 = "Q95",
    q975 = "Q975",
    q99 = "Q99",
    max = "Max.",
    var = "Var."
  ) %>% 
  gtsave(filename = "results/summary_temp.docx")

# Figure S3 - Histogram for temp ----

g_s3 <- 
  temp_df_long %>% 
  mutate(metric = factor(metric, levels = c("t_med", "hi_med"))) %>% 
  ggplot(aes(x=value)) + 
  geom_histogram(aes(fill = metric, group=metric), position = "identity", binwidth = 0.5, alpha=0.5) +
  scale_x_continuous(breaks = seq(14,40,by=2)) +
  labs(x="Temperature/Heat Index (ºC)", y="Frequency", fill = "Metric") + 
  scale_fill_discrete(labels = c("Tmed", "HImed")) +
  theme_minimal()

ggsave(
  g_s3,
  filename = "img/supp_3.tiff",
  width = 8.4,
  height = 6,
  units = "in",
  dpi = 300,
  bg="white"
)

# Figure S4 - Tmed vs Hours comparison ----

# Points of callout: 2017-12-20 and 2024-03-05

set.seed(1084)

g_s4 <- 
  temp_df %>% 
  left_join(
    num_hr_df %>% select(dia, starts_with("num_hr")), by="dia"
  ) %>% 
  mutate(callout = dia %in% c(ymd("2017-12-20"), ymd("2024-03-05"))) %>% 
  filter(num_hr_q90 > 0) %>% 
  ggplot(aes(y=t_med, x=num_hr_q90)) + 
  geom_jitter(aes(color=callout, alpha=callout), height = 0, width=0.2) + 
  scale_x_continuous(breaks = seq(0,24,by=2), name = "Amount of hours with Heat Index (HI) above 90th quantile (Q90)") +
  scale_y_continuous(name = "Tmed (°C)", breaks = seq(24,32, by=2)) +
  scale_alpha_manual(values = c(0.2, 1)) + 
  scale_color_manual(values = c("black", "red")) +
  guides(color = "none", alpha="none") +
  annotate("segment", x = 1.86, y = 27.68, xend = 1.86, yend = 30.6,
           linewidth=0.6,
           color = "red",
           arrow = arrow(type = "closed", length = unit(0.2, "cm"))) +
  annotate("text", x = 1.86 + 0.16, y = 31.1, 
           label = "December 20th, 2017\nTmed: 27.54°C\nHours with HI above Q90: 2",
           color = "red", fontface="bold",
           size=2.8) +
  annotate("segment", x = 12.4, y = 27.5, xend = 16, yend = 26.7,
           linewidth=0.6,
           color = "red",
           arrow = arrow(type = "closed", length = unit(0.2, "cm"))) +
  annotate("text", x = 18, y = 26.4, 
           label = "March 5th, 2024\nTmed: 27.55°C\nHours with HI above Q90: 12",
           color = "red",
           fontface = "bold",
           size=2.8) +
  theme_minimal()

ggsave(
  g_s4,
  filename = "img/supp_4.tiff",
  width = 8.4,
  height = 6,
  units = "in",
  dpi = 300,
  bg="white"
)

# Figure 1 - Selected deaths and HI ----

# Null model, only structure
# 6 to capture both peaks in year

do_selec <- do_df %>%
  filter(group_id == 16) %>%  
  summarise(n=sum(n), .by=c(data_obito, dia_ano, time_id, dia_group))

gam_null_selec <- gam(
  n ~ 1 + s(time_id, k=6, fx=F, bs = "cr") + s(dia_ano, k=6, bs="cc") ,#+ dia_group, 
  data = do_selec,
  family = quasipoisson()
)

do_selec <- do_selec %>% 
  mutate(fitted = exp(predict(gam_null_selec, newdata=do_selec)) %>% as.vector()) 

# Model for heat index

temp_df <- temp_df %>% 
  mutate(time_id = seq_len(nrow(.)),
         dia_ano = yday(dia))

gam_hi <- gam(
  hi_med ~ 1 + s(time_id, k=6, bs = "cr", fx=F) + s(dia_ano, k=6, bs="cc"),
  data = temp_df,
  family = "gaussian"
)

temp_df <- temp_df %>% 
  mutate(fitted_hi = predict(gam_hi, newdata=temp_df) %>% as.vector(),
         sd_hi = predict(gam_hi, newdata=temp_df, se.fit=T)$se.fit  %>% as.vector(),
         low_hi = fitted_hi - 1.96*sd_hi,
         upp_hi = fitted_hi + 1.96*sd_hi,
         diff_hi = hi_med - fitted_hi) 

g_do <- 
  do_selec %>%  
  mutate(diff = n - fitted) %>% 
  ggplot(aes(x=data_obito)) +
  geom_segment(aes(xend=data_obito, y=fitted, yend=n, alpha=diff^2, color=diff), linewidth=0.2) + 
  geom_point(aes(y=n, color=diff), size=0.6) +
  scale_color_gradient2(low="blue", high="red", name = "Deaths (Difference from baseline)") + 
  scale_alpha_continuous(range = c(0.6, 1)) +
  geom_line(aes(y=fitted, linetype="Baseline", group=1), linewidth=0.8) +
  scale_linetype_manual(values = c("solid"), name = "") +
  guides(alpha="none", 
         color = guide_colorbar(title.position="top"),
         linetype = guide_legend(direction = 'vertical', title.position = 'top')) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y", expand=c(0,0)) +
  scale_y_continuous(name = "Daily deaths", breaks = seq(80, 200, by=20)) +
  
  annotate("segment", x = ymd("2023-07-01"), y = 195, xend = ymd("2023-10-17"), yend = 190,
           linewidth=0.2,
           arrow = arrow(type = "closed", length = unit(0.05, "cm"))) +
  
  annotate("text", y = 194, x = ymd("2023-01-01"), label = "November 2023\nHeat Wave", size=2) +
  
  # influenza
  annotate("segment", x = ymd("2021-09-01"), y = 196, xend = ymd("2021-11-10"), yend = 190,
           linewidth=0.2,
           arrow = arrow(type = "closed", length = unit(0.05, "cm"))) +
  annotate("text", y = 200, x = ymd("2021-06-01"), label = "Influenza", size=2) +
  
  # covid 1
  annotate("segment", x = ymd("2020-08-01"), y = 190, xend = ymd("2020-06-01"), yend = 188,
           linewidth=0.2,
           arrow = arrow(type = "closed", length = unit(0.05, "cm"))) +
  # covid 2
  annotate("segment", x = ymd("2020-12-15"), y = 190, xend = ymd("2021-01-01"), yend = 180,
           linewidth=0.2,
           arrow = arrow(type = "closed", length = unit(0.05, "cm"))) +
  annotate("text", y = 194, x = ymd("2020-11-01"), label = "Covid-19", size=2) +
  
  
  annotate("segment", x = ymd("2016-09-01"), y = 186, xend = ymd("2016-07-01"), yend = 176,
           linewidth=0.2,
           arrow = arrow(type = "closed", length = unit(0.05, "cm"))) +
  annotate("text", y = 190, x = ymd("2016-09-01"), label = "Chikungunya", size=2) +
  
  
  # title
  
  annotate("text", y = 195, x = ymd("2014-01-01"), label = "A. Daily death count", fontface = "bold") +
  
  theme_minimal() + 
  theme(legend.position = "top",
        panel.grid.major = element_line(linewidth=0.2),
        panel.grid.minor.x = element_blank(),
        legend.key.width = unit(1.5, "cm"),
        legend.key.height = unit(0.2, "cm"),
        axis.text.x = element_blank(),
        axis.text.y = element_text(size=8),
        axis.line.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.title.x = element_blank(), 
        axis.title.y = element_text(size=8),
        plot.margin = unit(c(0.2,0.2,0,0.2), "cm"),
        axis.line.y = element_line(color = "black", linewidth = 1),
        legend.title = element_text(hjust=0.5, size=8),
        legend.text = element_text(hjust=0.5, size=8))

g_hi <- temp_df %>% 
  ggplot(aes(x=dia)) + 
  geom_segment(aes(xend=dia, y=fitted_hi, yend=hi_med,
                   alpha = hi_med >= quantile(temp_df$hi_med, probs = 0.95) | hi_med <= quantile(temp_df$hi_med, probs = 0.05),
                   color = hi_med
  ), linewidth=0.2) +
  geom_point(aes(y=hi_med, color = hi_med,
                 alpha = hi_med >= quantile(temp_df$hi_med, probs = 0.95) | hi_med <= quantile(temp_df$hi_med, probs = 0.05)
  ), size=0.6) +
  geom_line(aes(y=fitted_hi, linewidth = "Average HI (trend)")) + 
  scale_linewidth_manual(values = c(0.8)) +
  scale_alpha_manual(values = c(0.4, 1)) +
  guides(alpha="none") +
  scale_color_gradientn(
    values = scales::rescale(c(15, 18, 20, 25, 28, 30, 32)),
    colors = c("#3422f5", "#3ba1eb", "#96d0fa", "#fffa91", "#f5733b", "#ba1a35")
  ) + 
  scale_x_date(expand=c(0,0), date_breaks = "1 year", date_labels = "%Y") +
  scale_y_continuous(breaks = seq(16, 40, by=4), position = "right") +
  guides(color = guide_colorbar(title.position = "top")) +
  labs(x="Date", y = "Average HI (°C)", linewidth = "", color = "Daily average HI (°C)") +
  annotate("text", y = 39, x = ymd("2014-01-01"), label = "B. Daily average HI", fontface = "bold") +
  theme_minimal() +
  theme(legend.position = "bottom",
        legend.key.width = unit(1.5, "cm"),
        legend.key.height = unit(0.2, "cm"),
        legend.title = element_text(hjust=0.5, size=8),
        legend.text = element_text(hjust=0.5, size=8),
        plot.margin = unit(c(0,0.2,0.2,0.2), "cm"),
        panel.grid.major = element_line(linewidth=0.2),
        panel.grid.minor.x = element_blank(),
        axis.line.y = element_line(color = "black", linewidth = 1),
        axis.text.y = element_text(size=8),
        axis.title.y = element_text(size=8),
        axis.title.x = element_text(size=8)) 

g_composed <- g_do / g_hi

ggsave(
  g_composed, 
  filename = "img/fig1.tiff",
  width = 7.5,
  units = 'in',
  dpi = 300
)