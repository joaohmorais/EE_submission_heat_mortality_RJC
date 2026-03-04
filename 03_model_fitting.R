library(tidyverse)
library(mgcv)
library(dlnm)
library(splines)

source("00_aux_functions.R")

cdc_groups <- get_cdc_groups()

do_df <- read_csv("data/do_data.csv", show_col_types = F)
temp_df <- read_csv("data/temp_df.csv", show_col_types = F)
do_metrics <- read_csv("do_summary_metrics.csv", show_col_types = F)
feriados <- read_csv("data/feriados.csv", show_col_types = F)

do_df <- do_df %>% 
  mutate(dia_sem = wday(data_obito, label = T),
         feriado = data_obito %in% feriados$data,
         dia_group = case_when(
           dia_sem == "Sun" ~ "Sun/Holiday",
           feriado ~ "Sun/Holiday",
           TRUE ~ as.character(dia_sem)
         ) %>% factor(levels = c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun/Holiday"), ordered = F))

## Initializing data frames ----

RR_lag_df <- tibble(
  model_id = integer(),
  causa = character(),
  grupo = character(),
  metric = character(),
  at = integer(),
  lag = integer(),
  RR = double(),
  RR_low = double(),
  RR_upp = double()
)

RR_df <- tibble(
  model_id = integer(),
  causa = character(),
  grupo = character(),
  metric = character(),
  at = integer(),
  RR = double(),
  RR_low = double(),
  RR_upp = double()
)

metrics_df <- tibble(
  model_id = integer(),
  causa = character(),
  grupo = character(),
  metric = character(),
  family = character(),
  aic = double(),
  qaic = double(),
  ubre = double(),
  deviance = double(),
  null_deviance = double(),
  edf_values = double(),
  df_residual = double(),
  deviance_explained = double(),
  log_likelihood = double(),
  converged = logical()
)

models_itinerary <- do_metrics %>% 
  filter(include_in_model == 1) %>% 
  select(grupo, age_group, likelihood) %>% 
  left_join(
    cdc_groups, by=c("grupo")
  ) %>% 
  cross_join(
    tibble(
      var = c("t_med", "hi_med")
    )
  ) %>% 
  mutate(model_id = seq_len(nrow(.))) %>% 
  select(model_id, group_id, grupo, age_group, cids, likelihood, var) 

# Fitting -----

for (i in models_itinerary$model_id) {
  
  cat(sprintf("[%s/%s] - %s - Fitting model for %s among %s; Variable: %s...\n",
              i,
              nrow(models_itinerary),
              format(Sys.time(), "%Y_%m_%d %H:%M:%S"),
              models_itinerary$grupo[i],
              models_itinerary$age_group[i],
              models_itinerary$var[i]
  ))
  
  do_i <- do_df %>% 
    filter(group_id == models_itinerary$group_id[i],
           age_group == models_itinerary$age_group[i]
    ) %>% 
    left_join(temp_df %>% select(all_of(c("dia", models_itinerary$var[i]))) %>% rename(exposure=2), by=c("data_obito"="dia"))
  
  # Crossbasis ----
  
  cb_i <- crossbasis(
    do_i$exposure,
    lag = 10,
    argvar = list(fun = "ns", df=4, knots = quantile(do_i$exposure, probs = c(0.1, 0.75, 0.9))),
    arglag = list(fun = "ns", knots = c(2, 5, 8))
    #arglag = list(fun = "ns")
  )
  
  gam_i <- gam(
    n ~ cb_i + s(time_id, k=6, fx=FALSE, bs = "cr") + s(dia_ano, k=6, bs="cc") + dia_group,
    data = do_i,
    family = models_itinerary$likelihood[i],
    method = "REML"
  )
  
  # Result gathering ----
  
  range_i <- seq(
    min(do_i$exposure),
    max(do_i$exposure),
    length.out = 100
  )
  
  pred1 <- gam_i %>% crosspred(basis = cb_i, at = range_i, cen = mean(do_i$exposure))
  
  pred1_temps <- pred1$predvar
  pred1_rr <- pred1$allRRfit
  pred1_mmt <- pred1_temps[which.min(pred1_rr)]
  
  #pred_i <- gam_i %>% crosspred(basis = cb_i, at = range_i, cen = pred1_mmt)
  pred_i <- gam_i %>% crosspred(basis = cb_i, at = range_i, cen = mean(do_i$exposure))
  
  ### Lagged RR df ----
  
  RR_lag_df <- RR_lag_df %>% 
    add_row(
      pred_i %>% 
        extract_RR_lag(range = range_i) %>% 
        mutate(model_id = models_itinerary$model_id[i],
               causa = models_itinerary$grupo[i], 
               grupo = models_itinerary$age_group[i], 
               metric = models_itinerary$var[i])
    )
  
  ### Overall RR df ----
  
  RR_df <- RR_df %>% 
    add_row(
      pred_i %>% 
        extract_RR(range = range_i) %>% 
        mutate(model_id = models_itinerary$model_id[i],
               causa = models_itinerary$grupo[i], 
               grupo = models_itinerary$age_group[i], 
               metric = models_itinerary$var[i])
    )
  
  ### Metrics df ----
  
  metrics_df <- metrics_df %>% 
    add_row(gam_i %>% 
              extract_fit_metrics(model_tag=models_itinerary$var[i], 
                                  cdc_cause=models_itinerary$grupo[i], 
                                  age_group=models_itinerary$age_group[i]) %>% 
              mutate(model_id = models_itinerary$model_id[i])) 
  
}

save(
  RR_df, RR_lag_df, metrics_df, file = paste0("results/results_dlnm_", format(Sys.time(), "%Y_%m_%d_%H_%M"), ".Rdata")
)

# Categorized number of hours -----

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

## Initializing data frames ----

RR_lag_df_hr <- tibble(
  model_id = integer(),
  causa = character(),
  grupo = character(),
  metric = character(),
  at = integer(),
  lag = integer(),
  RR = double(),
  RR_low = double(),
  RR_upp = double()
)

RR_df_hr <- tibble(
  model_id = integer(),
  causa = character(),
  grupo = character(),
  metric = character(),
  at = integer(),
  RR = double(),
  RR_low = double(),
  RR_upp = double()
)

metrics_df_hr <- tibble(
  model_id = integer(),
  causa = character(),
  grupo = character(),
  metric = character(),
  family = character(),
  aic = double(),
  qaic = double(),
  ubre = double(),
  deviance = double(),
  null_deviance = double(),
  edf_values = double(),
  df_residual = double(),
  deviance_explained = double(),
  log_likelihood = double(),
  converged = logical()
)

# Fitting -----

for (i in models_itinerary$model_id) {
  
  cat(sprintf("[%s/%s] - %s - Fitting model with num_hr for %s among %s; Variable: %s...\n",
              i,
              nrow(models_itinerary),
              format(Sys.time(), "%Y_%m_%d %H:%M:%S"),
              models_itinerary$grupo[i],
              models_itinerary$age_group[i],
              models_itinerary$var[i]
  ))
  
  do_i <- do_df %>% 
    filter(group_id == models_itinerary$group_id[i],
           age_group == models_itinerary$age_group[i]
    ) %>% 
    left_join(temp_df %>% select(all_of(c("dia", models_itinerary$var[i]))) %>% rename(exposure=2), by=c("data_obito"="dia")) %>% 
    left_join(num_hr_df %>% select(dia, starts_with("hr_above")), by=c("data_obito"="dia"))
  
  # Crossbasis ----
  
  for (hr_var in names(do_i %>% select(starts_with("hr_above")))) {
    do_j <- do_i %>% 
      rename(hr_var = !!hr_var) %>% 
      select(group_id:exposure, hr_var)
    
    cb_j <- crossbasis(
      do_j$exposure,
      lag = 10,
      argvar = list(fun = "ns", df=4, knots = quantile(do_i$exposure, probs = c(0.1, 0.75, 0.9))),
      arglag = list(fun = "ns", knots = c(2, 5, 8))
    )
    
    cb_hr <- crossbasis(
      do_j$hr_var,
      lag = 10,
      argvar = list(fun = "lin"),
      arglag = list(fun = "ns", knots = c(2, 5, 8))
    )
    
    gam_j <- gam(
      n ~ cb_j + 
        cb_hr + s(time_id, k=6, fx=FALSE, bs = "cr") + s(dia_ano, k=6, bs="cc") + dia_group,
      data = do_j,
      family = models_itinerary$likelihood[i],
      method = "REML"
    )
    
    # Result gathering ----
    
    range_j <- seq(
      min(do_j$exposure),
      max(do_j$exposure),
      length.out = 100
    )
    
    pred1 <- gam_j %>% crosspred(basis = cb_j, at = range_j, cen = mean(do_j$exposure))
    
    pred1_temps <- pred1$predvar
    pred1_rr <- pred1$allRRfit
    pred1_mmt <- pred1_temps[which.min(pred1_rr)]
    
    pred_j <- gam_j %>% crosspred(basis = cb_j, at = range_j, cen = pred1_mmt)
    
    range_hr <- c(0,1)
    
    pred_hr <- gam_j %>% crosspred(basis = cb_hr, at = range_hr, cen = 0)
    
    ### Lagged RR df ----
    
    RR_lag_df_hr <- RR_lag_df_hr %>%
      add_row(
        pred_j %>%
          extract_RR_lag(range = range_j) %>%
          mutate(model_id = models_itinerary$model_id[i],
                 causa = models_itinerary$grupo[i],
                 grupo = models_itinerary$age_group[i],
                 metric = models_itinerary$var[i])
      )
    
    RR_lag_df_hr <- RR_lag_df_hr %>% 
      add_row(
        pred_hr %>% 
          extract_RR_lag(range = range_hr) %>% 
          mutate(model_id = models_itinerary$model_id[i],
                 causa = models_itinerary$grupo[i], 
                 grupo = models_itinerary$age_group[i], 
                 metric = paste0(hr_var, " + ", models_itinerary$var[i]))
      )
    
    ### Overall RR df ----
    
    RR_df_hr <- RR_df_hr %>%
      add_row(
        pred_j %>%
          extract_RR(range = range_j) %>%
          mutate(model_id = models_itinerary$model_id[i],
                 causa = models_itinerary$grupo[i],
                 grupo = models_itinerary$age_group[i],
                 metric = models_itinerary$var[i])
      )
    
    RR_df_hr <- RR_df_hr %>% 
      add_row(
        pred_hr %>% 
          extract_RR(range = range_hr) %>% 
          mutate(model_id = models_itinerary$model_id[i],
                 causa = models_itinerary$grupo[i], 
                 grupo = models_itinerary$age_group[i], 
                 metric = paste0(hr_var, " + ", models_itinerary$var[i]))
      )
    
    ### Metrics df ----
    
    metrics_df_hr <- metrics_df_hr %>% 
      add_row(gam_j %>% 
                extract_fit_metrics(model_tag=paste0(hr_var, " + ", models_itinerary$var[i]), 
                                    cdc_cause=models_itinerary$grupo[i], 
                                    age_group=models_itinerary$age_group[i]) %>% 
                mutate(model_id = models_itinerary$model_id[i])) 
    
    
  }
  
}

save(
  RR_df_hr, RR_lag_df_hr, metrics_df_hr, file = paste0("results/results_dlnm_hr_", format(Sys.time(), "%Y_%m_%d_%H_%M"), ".Rdata")
)