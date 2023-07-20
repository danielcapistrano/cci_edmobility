library(MCM) #calculating estimates based on mobility contrast models
library(tidyverse)#data manipulation
library(purrr)#map regressions to different outcomes and subsamples
library(broom)#tidy regression outputs
library(broom.helpers)#extra functions for tidy
library(haven)#SPSS metadata

### Model specification ##################################################

# Creating a vector identifying the outcome vars and another with covariates (including cntry fixed effects)
v_outc <- c("equality", "equity", "need", "entitlement")

v_covar  <- c("eiscedp * eisced", "agea", "gender", "income_feeling", "cntry")

# Function to build a model for each outcome

get_model <- function(myoutcome){
  # generating the call formula for each outcome
  myformula <- as.formula(paste(myoutcome, paste(v_covar, collapse = " + "), sep = "~"))

  # building the model from the formula
  mcm(formula = myformula, weights = ess_cci$anweight, data = ess_cci, 
    origin = 'eiscedp', destination = 'eisced', displayresult = FALSE)
}

### Calculating estimates ##############################################
# Generating a model for each outcome
set.seed(12)
models <- map(v_outc, ~ get_model(.x)) |> setNames(v_outc)


### Tidying up the output #############################################

#Function to generate the tidy output tables

get_table <- function(mymodel){
  tidy_and_attach(mymodel) |>
  tidy_add_reference_rows() |>
  tidy_add_term_labels() |>
  filter(var_type != "interaction", variable != "cntry") |>
  mutate(conf = paste0("[", round(conf.low, 3), ", ",  round(conf.high, 3), "]")) |>
  mutate(conf = if_else(conf == "[NA, NA]", NA, conf)) |>
  mutate(p.value  = case_when(p.value <= 0.001 ~ "***", 
                              p.value > 0.001 & p.value <= 0.01 ~ "**",
                              p.value > 0.01 & p.value <= 0.05  ~ "*",
                              TRUE ~ "")) |>
  mutate(estimate = if_else(is.na(estimate), NA, paste0(round(estimate,3), p.value))) |>
  select(variable, label, estimate, conf) |>
  bind_rows(
    glance(models$equality$model) |>
    select(AIC, deviance, nobs) |>
    pivot_longer(cols = everything(), names_to = "label", values_to = "estimate") |>
    mutate(estimate = as.character(format(round(estimate), big.mark = ",")), variable = "Model fit"))
  }

# Tidying up the tables with model main effects
tab_output <- map(models, ~get_table(.x$model))
tab_output <- imap(tab_output, ~ rename_with(.x, function(x) paste(.y, x, sep= "_"),
                .cols = c("estimate", "conf")))
tab_output <- reduce(tab_output, left_join, by =  c("variable", "label"))


# Tidying  up the tables with mobility estimates and sig tests

get_mobile <- function(mymodeltable){
  data.frame(mymodeltable) |>
  rownames_to_column(var = "Origin") |>
   mutate(Origin = paste0("L", Origin)) |>
  rename_with(function(x) gsub("X", "L", x), .cols = starts_with("X"))
}

# Estimates
tab_est <- bind_rows(map(models, ~get_mobile(.x$mobility_estimates)), .id = "Outcome")
tab_est <- tab_est |> pivot_longer(cols= L1:L7, names_to = "Destination", values_to = "Estimate")
tab_est$Estimate <- as.numeric(tab_est$Estimate)

# Std. error
tab_se <- bind_rows(map(models, ~get_mobile(.x$mobility_se)), .id = "Outcome")
tab_se <- tab_se |> pivot_longer(cols= L1:L7, names_to = "Destination", values_to = "SE")
tab_se$SE <- as.numeric(tab_se$SE)

# Sig test

tab_sig <- bind_rows(map(models, ~get_mobile(.x$mobility_sig)), .id = "Outcome")
tab_sig <- tab_sig |> pivot_longer(cols= L1:L7, names_to = "Destination", values_to = "Sig")
tab_sig$Sig <- if_else(tab_sig$Sig == "-", "", str_trim(tab_sig$Sig))
