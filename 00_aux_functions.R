require(tidyverse)

# CDC GROUPS ----

## ICD specification ----

get_cdc_groups <- function() {
  cdc_groups <- tribble(
    ~grupo, ~cids,
    "Influenza e Pneumonia", "J09|J1[2-8]",
    "Respiratórias Crônicas Inferiores", "J4[0-7]",
    "Outras doenças respiratórias", "J0[0-6]|J2[0-9]|J3[0-9]|J6[0-9]|J70|J8[0-6]|J9[0-6]|J9[7-9]|R092|U04",
    "Doenças hipertensivas", "I1[0-5]",
    "IAM", "I2[0-5]",
    "Parada Cardíaca", "I50",
    "AVC", "I6[0-9]",
    "Outras doenças do Aparelho Circulatório", "I0[0-9]|I2[6-9]|I3[0-9]|I4[0-9]|I51|I52|I7[0-9]|I8[0-9]|I9[0-9]",
    "Neoplasias", "C[0-8]|C9[0-7]",
    "Alzheimer e demência", "G3[0-1]|F01|F03",
    "Diabetes", "E1[0-4]",
    "Falência Renal", "N1[7-9]",
    "Sepse", "A4[0-1]",
    "Trato urinário", "N39",
    "Causas indeterminadas", "R"
  )
  
  cdc_groups <- cdc_groups %>% 
    add_row(
      tibble(
        grupo = "Causas selecionadas",
        cids = paste(cdc_groups$cids, collapse = "|")
      )
    ) %>% 
    add_row(
      tibble(
        grupo = "Causas naturais",
        cids = "^[^TVWXY]*$"
      )
    ) %>% 
    mutate(group_id = seq_len(nrow(.))) %>% 
    relocate(group_id, .before = everything())
  
  cdc_groups
}



## Translation ----

translate_cdc_groups <- function(label, reduced=0) {
  
  if (reduced==0) {
    case_when(
      label == "Influenza e Pneumonia" ~ "Influenza and pneumonia",
      label == "Respiratórias Crônicas Inferiores" ~ "Chronic lower respiratory diseases",
      label == "Outras doenças respiratórias" ~ "Other diseases of the respiratory system",
      label == "Doenças hipertensivas" ~ "Hypertensive diseases",
      label == "IAM" ~ "Ischemic heart disease",
      label == "Parada Cardíaca" ~ "Heart failure",
      label == "AVC" ~ "Cerebrovascular diseases",
      label == "Outras doenças do Aparelho Circulatório" ~ "Other disease of the circulatory system",
      label == "Neoplasias" ~ "Malignant neoplasms",
      label == "Alzheimer e demência" ~ "Alzheimer and dementia",
      label == "Diabetes" ~ "Diabetes",
      label == "Falência Renal" ~ "Renal failure",
      label == "Sepse" ~ "Sepsis",
      label == "Trato urinário" ~ "Urinary tract infections",
      label == "Causas indeterminadas" ~ "Undetermined deaths",
      label == "Causas selecionadas" ~ "Selected causes",
      label == "Causas naturais" ~ "Natural causes"
    ) 
  } else if (reduced==1) {
    case_when(
      label == "Influenza e Pneumonia" ~ "Flu/Pneu",
      label == "Respiratórias Crônicas Inferiores" ~ "Chronic Resp.",
      label == "Outras doenças respiratórias" ~ "Other Resp.",
      label == "Doenças hipertensivas" ~ "Hypertensive diseases",
      label == "IAM" ~ "Ischemic heart disease",
      label == "Parada Cardíaca" ~ "Heart failure",
      label == "AVC" ~ "CVD",
      label == "Outras doenças do Aparelho Circulatório" ~ "Other Circ.",
      label == "Neoplasias" ~ "Malig. Neo.",
      label == "Alzheimer e demência" ~ "Alz/Dem",
      label == "Diabetes" ~ "Diabetes",
      label == "Falência Renal" ~ "Renal failure",
      label == "Sepse" ~ "Sepsis",
      label == "Trato urinário" ~ "Urinary tract infections",
      label == "Causas indeterminadas" ~ "Undetermined deaths",
      label == "Causas selecionadas" ~ "Selected causes",
      label == "Causas naturais" ~ "Natural causes"
    ) 
  } else if (reduced==2) {
    case_when(
      label == "Influenza e Pneumonia" ~ "Flu/Pneu",
      label == "Respiratórias Crônicas Inferiores" ~ "Chronic Resp.",
      label == "Outras doenças respiratórias" ~ "Other Resp.",
      label == "Doenças hipertensivas" ~ "HTN diseases",
      label == "IAM" ~ "IHD",
      label == "Parada Cardíaca" ~ "Heart failure",
      label == "AVC" ~ "CVD",
      label == "Outras doenças do Aparelho Circulatório" ~ "Other Circ.",
      label == "Neoplasias" ~ "Malig. Neo.",
      label == "Alzheimer e demência" ~ "Alz/Dem",
      label == "Diabetes" ~ "Diabetes",
      label == "Falência Renal" ~ "Renal failure",
      label == "Sepse" ~ "Sepsis",
      label == "Trato urinário" ~ "Urinary tract",
      label == "Causas indeterminadas" ~ "Undetermined",
      label == "Causas selecionadas" ~ "Selected causes",
      label == "Causas naturais" ~ "Natural causes"
    ) 
  }
}

# HEAT INDEX CALCULATION ----

heat_index_standard_formula <- function(temperature_fahrenheit, humidity) {
  c1 <- -42.379
  c2 <- 2.04901523
  c3 <- 10.14333127
  c4 <- -0.22475541
  c5 <- -6.83783e-03
  c6 <- -5.481717e-02
  c7 <- 1.22874e-03
  c8 <- 8.5282e-04
  c9 <- -1.99e-06
  
  c1 + c2 * temperature_fahrenheit + c3 * humidity + c4 * temperature_fahrenheit * humidity +
    c5 * temperature_fahrenheit^2 + c6 * humidity^2 + c7 * temperature_fahrenheit^2 * humidity +
    c8 * temperature_fahrenheit * humidity^2 + c9 * temperature_fahrenheit^2 * humidity^2
}

calculate_heat_index_celsius <- function(temperature_celsius, humidity) {
  
  
  temperature_fahrenheit <- temperature_celsius * 9/5 + 32
  
  # Simpler formula
  
  HI <- 0.5 * (temperature_fahrenheit + 61 + ((temperature_fahrenheit-68)*1.2) + (humidity*0.094))
  
  HI <- case_when(
    HI < 80 ~ HI,
    HI >= 80 & humidity < 13 & temperature_fahrenheit >= 80 & temperature_fahrenheit <= 112 ~
      # Standard formula with first adjustment
      heat_index_standard_formula(temperature_fahrenheit, humidity) - ((13-humidity)/4)*sqrt((17-abs(temperature_fahrenheit-95))/17),
    HI >= 80 & humidity > 85 & temperature_fahrenheit >= 80 & temperature_fahrenheit <= 87 ~
      # Standard formula with second adjustment
      heat_index_standard_formula(temperature_fahrenheit, humidity) + ((humidity-85)/10)*((87-temperature_fahrenheit)/5),
    # Standard formula (Regression of Rothfusz)
    HI >= 80 ~ heat_index_standard_formula(temperature_fahrenheit, humidity)) %>% 
    suppressWarnings()
  
  # convert back to celsius
  HI_celsius <- (HI - 32) / 1.8
  
  return(HI_celsius)
}

# MODEL RESULTS EXTRACTION ----

get_null_fit <- function(df, model, cdc_cause, age_group) {
  df %>% 
    mutate(fitted = exp(predict(model, newdata=df))) %>% 
    select(data_obito, n, fitted) %>% 
    mutate(causa = cdc_cause, grupo = age_group)
}

extract_RR_lag <- function(gam_pred, range, label="at") {
  RR_lag <- gam_pred$matRRfit %>% 
    as.data.frame() %>% 
    as_tibble() %>% 
    mutate(at = range) %>% 
    pivot_longer(cols = starts_with("lag"), names_to = "lag", values_to = "RR", names_prefix = 'lag') %>% 
    mutate(lag = as.integer(lag)) %>% 
    left_join(
      gam_pred$matRRlow %>% 
        as.data.frame() %>% 
        as_tibble() %>% 
        mutate(at = range) %>% 
        pivot_longer(cols = starts_with("lag"), names_to = "lag", values_to = "RR_low", names_prefix = 'lag') %>% 
        mutate(lag = as.integer(lag))
    ) %>% 
    left_join(
      gam_pred$matRRhigh %>% 
        as.data.frame() %>% 
        as_tibble() %>% 
        mutate(at = range) %>% 
        pivot_longer(cols = starts_with("lag"), names_to = "lag", values_to = "RR_upp", names_prefix = 'lag') %>% 
        mutate(lag = as.integer(lag))
    ) 
  
  colnames(RR_lag)[colnames(RR_lag) == "at"] <- label
  RR_lag
}

extract_RR <- function(gam_pred, range, label="at") {
  RR <- gam_pred$allRRfit %>% 
    as.data.frame() %>% 
    as_tibble() %>% 
    rename(RR=1) %>% 
    mutate(at = range) %>% 
    left_join(
      gam_pred$allRRlow %>% 
        as.data.frame() %>% 
        as_tibble() %>% 
        rename(RR_low=1) %>% 
        mutate(at = range)
    ) %>% 
    left_join(
      gam_pred$allRRhigh %>% 
        as.data.frame() %>% 
        as_tibble() %>% 
        rename(RR_upp=1) %>% 
        mutate(at = range)
    )
  
  colnames(RR)[colnames(RR) == "at"] <- label
  RR
}

phi_hat <- function(mod){
  sum(resid(mod, type = "pearson")^2) / df.residual(mod)
}

poisson_loglik <- function(y, mu){
  sum(dpois(y, lambda = mu, log = TRUE))
}

QAIC <- function(mod, y){
  mu <- fitted(mod)
  phi <- phi_hat(mod)
  loglik <- poisson_loglik(y, mu)        # approximate quasi-likelihood using Poisson loglik
  K <- length(coef(mod))                 # # parameters: adjust if you want to add 1 for dispersion
  qaic <- -2 * loglik / phi + 2 * K
  return(qaic)
}

extract_fit_metrics <- function(model, model_tag, cdc_cause, age_group) {
  tibble(
    causa = cdc_cause,
    grupo = age_group,
    metric = model_tag,
    family = model$family$family,
    aic = model$aic,
    qaic = QAIC(model, model$model$n),
    ubre = model$gcv.ubre,
    deviance = model$deviance,
    null_deviance = model$null.deviance,
    edf_values = sum(model$edf),
    df_residual = model$df.residual,
    deviance_explained = summary(model)$dev.expl,
    log_likelihood = as.numeric(logLik(model)),
    converged = model$converged
  )
}
