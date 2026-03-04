library(tidyverse)
library(patchwork)
library(gt)

# Figure 2 - DLNM results for Tmed ----

load("results/results_dlnm_2026_01_30_20_31.Rdata")

g_2 <- 
  RR_df %>% 
  mutate(causa = translate_cdc_groups(causa, reduced=2)) %>% 
  filter(metric == "t_med") %>% 
  mutate(grupo = factor(grupo, levels=c("Jovem", "Idoso"), labels = c("0-64", "65+"))) %>% 
  ggplot(aes(x=at)) + 
  scale_x_continuous(breaks = seq(16, 32, by=4)) +
  geom_ribbon(aes(ymin=RR_low, ymax=RR_upp, fill=grupo), alpha=0.2) + 
  geom_line(aes(y=RR, group=grupo, color=grupo)) + 
  geom_hline(yintercept = 1, linetype="dashed") +
  labs(x="Tmed (ºC)", y="Relative Risk (RR)", fill = "Age group", color = "Age group") +
  facet_wrap(~causa, scales = "free_y") +
  theme_bw() + 
  theme(legend.position = "top")

ggsave(
  g_2, 
  filename = "img/fig2.tiff",
  width = 8.4,
  height = 6,
  units = "in",
  dpi = 300,
  bg="white"
)

# Figure S8 - DLNM results for HImed ----

g_s8 <- RR_df %>% 
  mutate(causa = translate_cdc_groups(causa, reduced=2)) %>% 
  filter(metric == "hi_med") %>% 
  mutate(grupo = factor(grupo, levels=c("Jovem", "Idoso"), labels = c("0-64", "65+"))) %>% 
  ggplot(aes(x=at)) + 
  scale_x_continuous(breaks = seq(16, 40, by=4)) +
  geom_ribbon(aes(ymin=RR_low, ymax=RR_upp, fill=grupo), alpha=0.2) + 
  geom_line(aes(y=RR, group=grupo, color=grupo)) + 
  geom_hline(yintercept = 1, linetype="dashed") +
  labs(x="HImed (ºC)", y="Relative Risk (RR)", fill = "Age group", color = "Age group") +
  facet_wrap(~causa, scales = "free_y") +
  theme_bw() + 
  theme(legend.position = "top")

ggsave(
  g_s8, 
  filename = "img/supp_8.tiff",
  width = 8.4,
  height = 6,
  units = "in",
  dpi = 300,
  bg="white"
)

# Table 4 - Tmed x HImed results ----

t_q75 <- quantile(temp_df$t_med, probs=0.75)
t_q90 <- quantile(temp_df$t_med, probs=0.90)
hi_q75 <- quantile(temp_df$hi_med, probs=0.75)
hi_q90 <- quantile(temp_df$hi_med, probs=0.90)

temp_ecdf <- ecdf(temp_df$t_med)
hi_ecdf <- ecdf(temp_df$hi_med)

RR_df <- RR_df %>% 
  group_by(causa, grupo, metric) %>% 
  arrange(model_id, at) %>% 
  mutate(hot = case_when(
    metric == "t_med" ~ at > mean(temp_df$t_med),
    metric == "hi_med" ~ at > mean(temp_df$hi_med)
  ))

crossing_points <- RR_df %>% 
  filter(hot, RR_low > 1) %>% 
  slice_min(order_by = at, n=1) %>% 
  ungroup() %>% 
  mutate(crossing_Q = case_when(
    metric == "t_med" ~ temp_ecdf(at),
    metric == "hi_med" ~ hi_ecdf(at)
  ), 
  crossing_label = paste0(
    format(round(at, 1), big.mark = ".", decimal.mark=","), " ºC<br>(Q", 100*round(crossing_Q,2), ")"
  )
  ) %>%  
  select(model_id, causa, grupo, metric, crossing=crossing_label)

rr_q75 <- RR_df %>% 
  mutate(diff_q75 = case_when(
    metric == "t_med" ~ abs(at - t_q75),
    metric == "hi_med" ~ abs(at - hi_q75)
  )) %>% 
  arrange(model_id, desc(at)) %>% 
  slice_min(order_by = diff_q75, with_ties = F, n=1) %>% 
  ungroup() %>% 
  mutate(sign = RR_low > 1, across(starts_with("RR"), function(x) {
    format(round(x, 2), big.mark=".", decimal.mark=",")
  }), RR_label = case_when(
    sign ~ sprintf("<b>%s* (%s-%s)<b>", RR, RR_low, RR_upp),
    TRUE ~ sprintf("%s (%s-%s)", RR, RR_low, RR_upp)
  )) %>% 
  select(model_id, causa, grupo, metric, RR_Q75 = RR_label)

rr_q90 <- RR_df %>% 
  mutate(diff_q90 = case_when(
    metric == "t_med" ~ abs(at - t_q90),
    metric == "hi_med" ~ abs(at - hi_q90)
  )) %>% 
  arrange(model_id, desc(at)) %>% 
  slice_min(order_by = diff_q90, with_ties = F, n=1) %>% 
  ungroup() %>% 
  mutate(sign = RR_low > 1, across(starts_with("RR"), function(x) {
    format(round(x, 2), big.mark=".", decimal.mark=",")
  }), RR_label = case_when(
    sign ~ sprintf("<b>%s* (%s-%s)<b>", RR, RR_low, RR_upp),
    TRUE ~ sprintf("%s (%s-%s)", RR, RR_low, RR_upp)
  )) %>% 
  select(model_id, causa, grupo, metric, RR_Q90 = RR_label)

table_results <- rr_q75 %>% 
  left_join(rr_q90, by=c("model_id", "causa", "grupo", "metric")) %>% 
  left_join(crossing_points, by=c("model_id", "causa", "grupo", "metric")) %>% 
  arrange(model_id)

table_results %>% 
  select(-model_id) %>% 
  pivot_wider(names_from = c(metric), values_from = c(crossing, RR_Q75, RR_Q90)) %>% 
  relocate(
    causa, grupo, contains("t_med"), contains("hi_med")
  ) %>% 
  mutate(causa = translate_cdc_groups(causa),
         grupo = factor(grupo, levels=c("Jovem", "Idoso"), labels=c("0-64", "65+"))
  ) %>% 
  arrange(grupo, causa) %>% 
  gt() %>% 
  cols_label(
    1 ~ "Causa",
    starts_with("crossing") & ends_with("t_med") ~ "T_Risco",
    starts_with("crossing") & ends_with("hi_med") ~ "HI_Risco",
    starts_with("RR_Q75") ~ "RR_Q75",
    starts_with("RR_Q90") ~ "RR_90"
  ) %>% 
  sub_missing(missing_text = "-") %>% 
  fmt_markdown(columns = -1) %>% 
  gtsave(filename = "results/dlnm_results_table.docx")

# Figure 3 - Tmed x lag effects ----

g_3 <- 
  RR_lag_df %>% 
  mutate(sign = case_when(
    RR_low > 1 & RR > 1.2 ~ "> 1.2",
    RR_low > 1 & RR > 1.1 ~ "> 1.1",
    RR_low > 1 ~ "> 1",
    RR_upp < 1 ~ "< 1",
    TRUE ~ "Non-signif"
  ) %>% factor(levels = c("< 1", "Non-signif",  "> 1", "> 1.1", "> 1.2")),
  causa = translate_cdc_groups(causa, reduced = 2)
  ) %>% 
  filter(grupo == "Idoso", metric == "t_med") %>% 
  ggplot(aes(x=lag, y=at)) + 
  geom_tile(aes(fill= sign), width = 1) + 
  geom_line(y= mean(temp_df$t_med), aes(linetype="Tmed (mean)")) +
  scale_linetype_manual(values = "dashed", name = "") +
  scale_x_continuous(expand=c(0,0), breaks = 0:10, name = "Lag") + 
  scale_y_continuous(expand=c(0,0), name = "Tmed (°C)") +
  annotate(
    "segment", x = seq(0.5,9.5,by=1), y=min(temp_df$t_med), xend = seq(0.5,9.5,by=1), yend = max(temp_df$t_med),
    color = "gray90"
  ) +
  guides(fill = guide_legend(
    override.aes = list(color = "black")
  )) +
  scale_fill_manual(values = c("#b8e186", "gray99", "#fa9fb5", "#c51b8a", "#7a0177"), name = "RR") +
  theme_bw() +
  facet_wrap(~causa) + 
  theme(legend.position = "top")

ggsave(
  g_3, 
  filename = "img/fig3.tiff",
  width = 8.4,
  height = 6,
  units = "in",
  dpi = 300,
  bg="white"
)

# Figure S10 - HImed x lag effects ----

g_s10 <- RR_lag_df %>% 
  mutate(sign = case_when(
    RR_low > 1 & RR > 1.2 ~ "> 1.2",
    RR_low > 1 & RR > 1.1 ~ "> 1.1",
    RR_low > 1 ~ "> 1",
    RR_upp < 1 ~ "< 1",
    TRUE ~ "Non-signif"
  ) %>% factor(levels = c("< 1", "Non-signif",  "> 1", "> 1.1", "> 1.2")),
  causa = translate_cdc_groups(causa, reduced = 2)
  ) %>% 
  filter(grupo == "Idoso", metric == "hi_med") %>% 
  ggplot(aes(x=lag, y=at)) + 
  geom_tile(aes(fill= sign), width = 1) + 
  geom_line(y= mean(temp_df$hi_med), aes(linetype="HImed (mean)")) +
  scale_linetype_manual(values = "dashed", name = "") +
  scale_x_continuous(expand=c(0,0), breaks = 0:10, name = "Lag") + 
  scale_y_continuous(expand=c(0,0), name = "HImed (°C)") +
  annotate(
    "segment", x = seq(0.5,9.5,by=1), y=min(temp_df$hi_med), xend = seq(0.5,9.5,by=1), yend = max(temp_df$hi_med),
    color = "gray90"
  ) +
  guides(fill = guide_legend(
    override.aes = list(color = "black")
  )) +
  scale_fill_manual(values = c("#b8e186", "gray99", "#fa9fb5", "#c51b8a", "#7a0177"), name = "RR") +
  theme_bw() +
  facet_wrap(~causa) + 
  theme(legend.position = "top")

ggsave(
  g_s10, 
  filename = "img/supp_10.tiff",
  width = 8.4,
  height = 6,
  units = "in",
  dpi = 300,
  bg="white"
)

# Figure S9 - Tmed x HImed comparisons ----

temp_df_limits <- temp_df_long %>%
  summarise(q75=quantile(value, probs=0.75),
            q90 = quantile(value, probs=0.9), .by=metric) %>% 
  pivot_longer(cols = c(q75, q90), values_to = "value", names_to = "quantile") %>% 
  mutate(metric = factor(metric, levels=c("t_med", "hi_med"), labels = c("Tmed", "HImed")))

g_s9 <- 
  RR_df %>% 
  filter(grupo == "Idoso") %>% 
  mutate(sign = RR_low > 1 | RR_upp < 1,
         RR_cat = case_when(
           !sign ~ "Non-significant",
           RR > 3 ~ "> 3",
           RR > 2 ~ "> 2",
           RR > 1.75 ~ "> 1.75",
           RR > 1.5 ~ "> 1.5",
           RR > 1.25 ~ "> 1.25",
           RR > 1 ~ "> 1",
           RR < 1 ~ "< 1"
         ) %>% factor(
           levels = rev(c("> 3", "> 2", "> 1.75", "> 1.5", "> 1.25", "> 1", "< 1", "Non-significant"))
         ),
         causa = translate_cdc_groups(causa, reduced = 1),
         metric = factor(metric, levels=c("t_med", "hi_med"), labels = c("Tmed", "HImed"))) %>% 
  ggplot(aes(x=at, y=fct_rev(causa))) + 
  geom_line(aes(color=RR_cat, group=causa), linewidth=6) + 
  geom_vline(
    data = temp_df_limits,
    aes(xintercept = value, linetype = quantile),
  ) +
  geom_text(
    data = temp_df_limits,
    aes(x=value, y=length(unique(RR_df$causa)), label=toupper(quantile)), nudge_y = 0.5, nudge_x = c(-1, 1, -1.2, 1.2), size=2
  ) +
  scale_color_manual(values = c("gray80", "#276419", "#feebe2", "#fcc5c0", "#fa9fb5", "#f768a1", "#c51b8a", "#7a0177"), 
                     drop=F, name = "RR") +
  scale_linetype_manual(values = c("dotted", "dashed")) +
  labs(x="Tmed/Himed (ºC)", y="Cause of death") +
  guides(linetype="none") +
  facet_wrap(~metric, ncol=2, scales = "free_x") +
  theme_minimal() +
  theme(panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        strip.text = element_text(face="bold"))


ggsave(
  g_s9, 
  filename = "img/supp_9.tiff",
  width = 8.4,
  height = 6,
  units = "in",
  dpi = 300,
  bg="white"
)

# Figure 4 - Hours effects ----

load("results/results_dlnm_hr_2026_01_30_22_00.Rdata")

g_4 <- 
  RR_df_hr %>% 
  filter(grepl("hr_above", metric), at == 1, !grepl("hi_med", metric)) %>% 
  mutate(causa = translate_cdc_groups(causa, reduced=2),
         metric = case_when(
           grepl("hr_above_q90", metric) ~ "Hours Above Q90",
           grepl("hr_above_q95", metric) ~ "Hours Above Q95",
           grepl("hr_above_q975", metric) ~ "Hours Above Q975"
         ),
         grupo = factor(grupo, levels=c("Jovem", "Idoso"), labels=c("0-64", "65+"))
  ) %>% 
  ggplot(aes(x=fct_rev(causa))) +
  geom_errorbar(aes(ymin=RR_low, ymax=RR_upp)) + 
  geom_point(aes(y=RR, color = RR_low > 1)) + 
  geom_hline(yintercept = 1, linetype="dashed") +
  coord_flip() + 
  guides(color = "none") +
  labs(y="Relative Risk (RR)", x="Cause of death") +
  scale_color_manual(values=c("black", "red")) +
  facet_wrap(grupo~metric, ncol=3) +
  theme_minimal() + 
  theme(strip.text = element_text(face="bold"))

ggsave(
  g_4, 
  filename = "img/fig4.tiff",
  width = 8.4,
  height = 6,
  units = "in",
  dpi = 300,
  bg="white"
)

# Figure S11 ----

g_s11 <- 
  RR_df_hr %>% 
  filter(grepl("hr_above", metric), at == 1, !grepl("t_med", metric)) %>% 
  mutate(causa = translate_cdc_groups(causa, reduced=2),
         metric = case_when(
           grepl("hr_above_q90", metric) ~ "Hours Above Q90",
           grepl("hr_above_q95", metric) ~ "Hours Above Q95",
           grepl("hr_above_q975", metric) ~ "Hours Above Q975"
         ),
         grupo = factor(grupo, levels=c("Jovem", "Idoso"), labels=c("0-64", "65+"))
  ) %>% 
  ggplot(aes(x=fct_rev(causa))) +
  geom_errorbar(aes(ymin=RR_low, ymax=RR_upp)) + 
  geom_point(aes(y=RR, color = RR_low > 1)) + 
  geom_hline(yintercept = 1, linetype="dashed") +
  coord_flip() + 
  guides(color = "none") +
  labs(y="Relative Risk (RR)", x="Cause of death") +
  scale_color_manual(values=c("black", "red")) +
  facet_wrap(grupo~metric, ncol=3) +
  theme_minimal() + 
  theme(strip.text = element_text(face="bold"))

ggsave(
  g_s11, 
  filename = "img/supp_11.tiff",
  width = 8.4,
  height = 6,
  units = "in",
  dpi = 300,
  bg="white"
)

# Figure 5 - AIC metrics - Elderly ----

metrics_df_hr_best <- metrics_df_hr %>% 
  mutate(aic_qaic = case_when(
    family == "poisson" ~ aic,
    family == "quasipoisson" ~ qaic
  )) %>%
  mutate(metric = case_when(
    grepl("hr_above_q90", metric) ~ "hr_above_q90",
    grepl("hr_above_q95", metric) ~ "hr_above_q95",
    grepl("hr_above_q975", metric) ~ "hr_above_q975"
  )) %>% 
  summarise(aic_qaic = min(aic_qaic),
            .by=c(causa, grupo, metric))

ranked_metrics_df_elderly <- 
  metrics_df %>% 
  mutate(aic_qaic = case_when(
    family == "poisson" ~ aic,
    family == "quasipoisson" ~ qaic
  )) %>% 
  select(causa, grupo, metric, aic_qaic) %>% 
  add_row(metrics_df_hr_best) %>% 
  filter(grupo == "Idoso") %>% 
  mutate(metric = factor(metric, 
                         levels = c("info", "t_med", "hi_med", "hr_above_q90", "hr_above_q95", "hr_above_q975"),
                         labels = c("info", "Tmed", "HImed", "Hours above Q90", "Hours above Q95", "Hours above Q975")
  ), causa = translate_cdc_groups(causa, reduced=2)) %>% 
  group_by(causa) %>% 
  mutate(rank = rank(aic_qaic),
         rank_cat = factor(rank, levels=c(1, 2, 3, 4, 5), labels = c("Best", "2nd", "3rd", "Other", "Other")),
         amplitude = max(aic_qaic) - min(aic_qaic))

amp_metrics_df_elderly <- ranked_metrics_df_elderly %>% 
  select(causa, amplitude, aic_qaic) %>% 
  slice_min(order_by = aic_qaic) %>% 
  mutate(metric = "info",
         metric = factor(metric, levels = c("info", "Tmed", "HImed", "Hours above Q90", "Hours above Q95", "Hours above Q975"))
  )

g_5 <- ranked_metrics_df_elderly %>% 
  ggplot(aes(x=aic_qaic, y=fct_rev(metric))) + 
  geom_point(shape=23, aes(fill = rank_cat), size=3) + 
  labs(x="AIC/qAIC", y="Variable") +
  geom_text(data=amp_metrics_df_elderly, 
            aes(x=aic_qaic + amplitude/2, y=metric, label=paste0(
              "AIC/qAIC difference amplitude: ", round(amplitude, 2)
            )), size=2) +
  scale_fill_manual(values = c("#ffc000", "#dedede", "#cd6d32", "gray20"),
                    name = "Model Rank") +
  scale_y_discrete(drop=F, labels = function(x) {
    case_when(
      x == "info" ~ "",
      TRUE ~ x
    )
  }) +
  facet_wrap(~causa, scales = "free_x") + 
  theme_bw() + 
  theme(legend.position = "top",
        axis.text.x = element_text(size=6))


ggsave(
  g_5, 
  filename = "img/fig5.tiff",
  width = 8.4,
  height = 6.4,
  units = "in",
  dpi = 300,
  bg="white"
)

# Figure S12 - AIC metrics - Young ----

ranked_metrics_df_young <- 
  metrics_df %>% 
  mutate(aic_qaic = case_when(
    family == "poisson" ~ aic,
    family == "quasipoisson" ~ qaic
  )) %>% 
  select(causa, grupo, metric, aic_qaic) %>% 
  add_row(metrics_df_hr_best) %>% 
  filter(grupo == "Jovem") %>% 
  mutate(metric = factor(metric, 
                         levels = c("info", "t_med", "hi_med", "hr_above_q90", "hr_above_q95", "hr_above_q975"),
                         labels = c("info", "Tmed", "HImed", "Hours above Q90", "Hours above Q95", "Hours above Q975")
  ), causa = translate_cdc_groups(causa, reduced=2)) %>% 
  group_by(causa) %>% 
  mutate(rank = rank(aic_qaic),
         rank_cat = factor(rank, levels=c(1, 2, 3, 4, 5), labels = c("Best", "2nd", "3rd", "Other", "Other")),
         amplitude = max(aic_qaic) - min(aic_qaic))

amp_metrics_df_young <- ranked_metrics_df_young %>% 
  select(causa, amplitude, aic_qaic) %>% 
  slice_min(order_by = aic_qaic) %>% 
  mutate(metric = "info",
         metric = factor(metric, levels = c("info", "Tmed", "HImed", "Hours above Q90", "Hours above Q95", "Hours above Q975"))
  )

g_s12 <- ranked_metrics_df_young %>% 
  ggplot(aes(x=aic_qaic, y=fct_rev(metric))) + 
  geom_point(shape=23, aes(fill = rank_cat), size=3) + 
  labs(x="AIC/qAIC", y="Variable") +
  geom_text(data=amp_metrics_df_young, 
            aes(x=aic_qaic + amplitude/2, y=metric, label=paste0(
              "AIC/qAIC difference amplitude: ", round(amplitude, 2)
            )), size=2) +
  scale_fill_manual(values = c("#ffc000", "#dedede", "#cd6d32", "gray20"),
                    name = "Model Rank") +
  scale_y_discrete(drop=F, labels = function(x) {
    case_when(
      x == "info" ~ "",
      TRUE ~ x
    )
  }) +
  facet_wrap(~causa, scales = "free_x") + 
  theme_bw() + 
  theme(legend.position = "top",
        axis.text.x = element_text(size=6))


ggsave(
  g_s12, 
  filename = "img/supp_12.tiff",
  width = 8.4,
  height = 6,
  units = "in",
  dpi = 300,
  bg="white"
)
