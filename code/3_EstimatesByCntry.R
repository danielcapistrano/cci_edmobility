library(MCM) #calculating estimates based on mobility contrast models
library(tidyverse)#data manipulation
library(purrr)#map regressions to different outcomes and subsamples
library(broom)#tidy regression outputs
library(broom.helpers)#extra functions for tidy
library(haven)#SPSS metadata



### Model specification ##################################################

# Creating a vector identifying the outcome vars and another with covariates
v_covar  <- c("geiscedp * geisced", "agea", "gender", "income_feeling")

v_outc <- c("equality", "equity", "need", "entitlement")

# If not defined yet, create a vector with a list of all countries to generate models for
if (!exists("l_country")){l_country <- unique(as.character(ess_cci$cntry))}


# Function to build a model for each outcome
get_model_c <- function(myoutcome, mycountry){
  # generating the call formula for each outcome
  myformula <- as.formula(paste(myoutcome, paste(v_covar, collapse = " + "), sep = "~"))

  # Filtering observations for each country
  mydata <- subset(ess_cci, cntry == mycountry)


  # building the model from the formula
  mcm(formula = myformula, weights = mydata$anweight, data = mydata, 
    origin = 'geiscedp', destination = 'geisced', displayresult = FALSE)
}

### Calculating estimates ##############################################
# Generating a model for each outcome
set.seed(12)
m_equal <- map(l_country, ~ get_model_c("equality", .x)) |> setNames(l_country)
m_equit <- map(l_country, ~ get_model_c("equity", .x)) |> setNames(l_country)
m_need <- map(l_country, ~ get_model_c("need", .x)) |> setNames(l_country)
m_entit <- map(l_country, ~ get_model_c("entitlement", .x)) |> setNames(l_country)

# Tidying  up the tables with mobility estimates and sig tests

get_mobile_c <- function(mymodeltable){
data.frame(mymodeltable) |>
  rownames_to_column(var = "Origin") |>
  mutate(Origin = paste0("L", Origin)) |>
  rename_with(function(x) gsub("X", "L", x), .cols = starts_with("X"))
}

# Estimates

get_tab_est <- function(mymodel){
  map(mymodel, ~get_mobile_c(.x$mobility_estimates)) |>
    bind_rows(.id = "cntry") |>
    pivot_longer(cols = L1:L3, names_to = "Destination", values_to = "Estimate") |>
    mutate(Estimate = as.numeric(if_else(Estimate == "-", "", Estimate)))
}

tab_est_equal <- get_tab_est(m_equal) |> mutate(Outcome = "Equality")
tab_est_equit <- get_tab_est(m_equit) |> mutate(Outcome = "Equity")
tab_est_need <- get_tab_est(m_need) |> mutate(Outcome = "Need")
tab_est_entit <- get_tab_est(m_entit) |> mutate(Outcome = "Entitlement")

tab_est_c <- bind_rows(tab_est_equal, tab_est_equit, tab_est_need, tab_est_entit)

# Std. error

get_tab_se <- function(mymodel){
  map(mymodel, ~get_mobile_c(.x$mobility_se)) |>
    bind_rows(.id = "cntry") |>
    pivot_longer(cols = L1:L3, names_to = "Destination", values_to = "SE") |>
    mutate(SE = as.numeric(if_else(SE == "-", "", SE)))
}

tab_se_equal <- get_tab_se(m_equal) |> mutate(Outcome = "Equality")
tab_se_equit <- get_tab_se(m_equit) |> mutate(Outcome = "Equity")
tab_se_need <- get_tab_se(m_need) |> mutate(Outcome = "Need")
tab_se_entit <- get_tab_se(m_entit) |> mutate(Outcome = "Entitlement")

tab_se_c <- bind_rows(tab_se_equal, tab_se_equit, tab_se_need, tab_se_entit)


# Sig test

get_tab_sig <- function(mymodel){
  map(mymodel, ~get_mobile_c(.x$mobility_sig)) |>
    bind_rows(.id = "cntry") |>
    pivot_longer(cols = L1:L3, names_to = "Destination", values_to = "Sig") |>
    mutate(Sig = if_else(Sig == "-", "", str_trim(Sig)))
}

tab_sig_equal <- get_tab_sig(m_equal) |> mutate(Outcome = "Equality")
tab_sig_equit <- get_tab_sig(m_equit) |> mutate(Outcome = "Equity")
tab_sig_need <- get_tab_sig(m_need) |> mutate(Outcome = "Need")
tab_sig_entit <- get_tab_sig(m_entit) |> mutate(Outcome = "Entitlement")

tab_sig_c <- bind_rows(tab_sig_equal, tab_sig_equit, tab_sig_need, tab_sig_entit)

tab_clabel  <- tibble(cntry = ess_cci$cntry, country = as_factor(ess_cci$cntry)) |> distinct()
