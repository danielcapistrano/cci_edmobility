library(tidyverse)#data manipulation
library(haven)#using SPSS metadata
library(Hmisc)#for weighted standard deviation


# Load data processed in 0_Processing.R
load("./code/data/ess_cci.RData")

#### Summary table #####################################################

# Defining vectors with variable names
v_outc <- c("equality", "equity", "need", "entitlement")
v_cov <- c("geisced", "geiscedp", "mobility", "age_group", "gender",  "income_feeling")


# Getting mean values for outcome variables by covariates

# Function to apply to each covariate
get_means <- function(mygroup){
  ess_cci |> 
  group_by(categories = !!sym(mygroup)) |>
  summarise(across(all_of(v_outc), ~weighted.mean(.x, w = anweight), .names = "{.col}_m"),
            across(all_of(v_outc), ~sqrt(wtd.var(.x, w = anweight))/sqrt(length(.x)), .names = "{.col}_se")) |>
  mutate(group = mygroup)
}

# Generating table
tab_desc <- bind_rows(map(v_cov, ~get_means(.x))) |> select(group, everything())


# Adding TOTAL row
tab_desc <-
  tab_desc |>
    bind_rows(ess_cci |>
        summarise(across(all_of(v_outc), ~weighted.mean(.x, w = anweight), .names = "{.col}_m"),
                across(all_of(v_outc), ~sqrt(wtd.var(.x, w = anweight))/sqrt(length(.x)), .names = "{.col}_se")) |>
        mutate(categories = "Full sample", group  = "total"))
