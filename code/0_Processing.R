### Code for TITLE HERE #########
##############################################################################

## 0. Processing data ##
# Here are the steps to import and process the full ESS data file and generate
# the working dataset. If you do not want to change anything in the analysis,
# you can skip this step and load the working dataset that is available at the
# replication package by running the code 1_Descriptives.R

#### Required packages ####

library(tidyverse)#data manipulation
library(haven)#load SPSS dataset and metadata

#### 1) Accessing the full dataset from the ESS website #######################

# This research is based on data from the European Social Survey (ESS)
# The ESS dataset is publicly available for free at their website.
# To run this code, download the "ESS9e03_1.sav" file and move it to
# the sub-directory "data"

#### 2) Importing the data ####################################################

# a)  Importing ess round 9
ess9_raw <- read_sav("./code/data/ESS9e03_1.sav", user_na = TRUE)

# b) Selecting relevant variables
# In this step, we select the variables used in the analysis

ess9 <-
  ess9_raw |>
  select(idno, cntry,  #id vars
      anweight, pspwght, pweight, #weight vars
      sofrdst, sofrwrk, sofrpr, sofrprv,  #conception justice
      eisced, eiscedf, eiscedm, #education levels
      agea, gndr, domicil, #demographics
      mnactic, #occupation
      hincfel)#household income

# c) Identifying education and outcome variables
v_educ <- c("eisced", "eiscedf", "eiscedm")
v_outc <- c("sofrdst", "sofrwrk", "sofrpr", "sofrprv")


# d) Recoding category "Other" to missing value

ess9 <- ess9 |> mutate(across(all_of(v_educ), ~if_else(.x == 55, NA, .x)))
ess9 <- ess9 |> mutate(agea = if_else(agea == 999, NA, agea))


###### 2a) Multiple imputation ###############################################
# # Generating a new dataset using multiple imputation for missing data
# # This algorithm is based on one implementation of Random Forest
# # It also makes use of predictive mean matching avoiding the imputation
# # of values not present in the observed data.
# # If lines in this sections are commented, then listwise deletion is applied

library(missRanger)

ess9 <- missRanger(ess9, pmm.k = 3, num.trees = 10, seed = 11, verbose =0)

## 3) Recoding variables #####################################################

# a) Renaming outcome variables and inverting the scales======================
# Higher values denote higher agreement with the outcome variable statement

ess9 <- ess9 |> mutate(across(all_of(v_outc), ~if_else(!is.na(.x), 6 - .x, NA_real_)))

# Renaming outcome variables 
ess9 <- ess9 |> rename(equality = sofrdst, equity = sofrwrk, need = sofrpr, entitlement = sofrprv)


# b) Creating highest parental educational level (eiscedp) ===================

ess9 <- ess9 |> mutate(across(all_of(v_educ), ~if_else(is.na(.x), NA, as.numeric(.x))))

ess9 <-
  ess9  |>
    mutate(eiscedp = case_when(
      is.na(eiscedf) ~ eiscedm, #if father is NA, then mother
      is.na(eiscedm) ~ eiscedf, #if mother is NA, then father
      TRUE ~ pmax(eiscedm, eiscedf)))# highest level between parents


# c) Grouping educational levels (geisced) ===================================
# Grouping levels to facilitate comparison and interpretation and also  
# to avoid empty cells in the mobility table for small samples
# eisced: respondent's education
# eiscedm: mother's education
# eiscedf: father's education

# Lists of countries with skewed distributions
lc_low <- c("PT", "IT", "ES")
lc_mid <- c("SK", "CZ", "DE")
lc_hi <- c("IS", "SE", "NO")
lc_lowhi <- c("LV", "EE", "IE")

# Recoding by group of countries
ess9 <-
ess9 |>
  mutate(across(all_of(v_educ),
  ~case_when(
      cntry %in% lc_low ~ case_match(.x, 1 ~ 1, c(2, 3, 4) ~ 2, c(5, 6, 7) ~ 3),
      cntry %in% lc_mid ~ case_match(.x, c(1, 2, 3) ~ 1, 4 ~ 2, c(5, 6, 7) ~ 3),
      cntry %in% lc_hi ~ case_match(.x, c(1,2) ~ 1, c(3, 4, 5) ~ 2, c(6, 7) ~ 3),
      cntry %in% lc_lowhi ~ case_match(.x, c(1, 2, 3) ~ 1, c(4, 5) ~ 2, c(6, 7) ~ 3),
      .default = case_match(.x, c(1, 2) ~ 1, c(3, 4) ~ 2, c(5, 6, 7) ~ 3)),
      .names = "g{.col}"))

# Combining father and mother education level into parental educ (geiscedp)
ess9 <-
  ess9  |>
    mutate(geiscedp = case_when(
      is.na(geiscedf) ~ geiscedm, #if father is NA, then mother
      is.na(geiscedm) ~ geiscedf, #if mother is NA, then father
      TRUE ~ pmax(geiscedm, geiscedf)))# highest level between parents


# Applying levels and labels

l_educ <- c("L1", "L2", "L3")

ess9$geisced <- factor(ess9$geisced, levels = c(1, 2, 3), labels = l_educ)
ess9$geiscedp <- factor(ess9$geiscedp, levels = c(1, 2, 3), labels = l_educ)


# d) Creating mobility variables =============================================

ess9 <-
  ess9  |>
  mutate(mobility = case_when(
    as.numeric(geisced) == as.numeric(geiscedp) ~ "Nonmobile",
    as.numeric(geisced) > as.numeric(geiscedp) ~ "Upward",
    as.numeric(geisced) < as.numeric(geiscedp) ~ "Downward",
    TRUE ~ NA_character_))

# Re-leveling to display categories better in output tables
ess9$mobility <- factor(ess9$mobility, levels = c("Downward", "Nonmobile",  "Upward"))



# e) Recoding control variables ===============================================

ess9 <-
  ess9 |>
  mutate(
    gender = factor((as_factor(gndr)), levels = c("Female", "Male")),
    age_group = case_when(
      agea  <= 44 ~ "25-44",
      agea  >= 45 & agea  <= 64 ~ "45-64",
      agea  >= 65 ~ "65+",
      is.na(agea) ~ NA_character_),
    location = case_when(
      domicil  < 4 ~ "Urban",
      domicil  %in% c(4, 5) ~ "Rural",
      is.na(domicil) ~ NA_character_),
    income_feeling = factor(case_when(
      hincfel == 1 ~ "Living comfortably",
      hincfel == 2 ~ "Coping",
      hincfel == 3 ~ "Difficult",
      hincfel == 4 ~ "Very difficult",
      is.na(hincfel) ~ NA_character_),
        levels = c("Very difficult", "Difficult", "Coping", "Living comfortably"))
        )



# 4) Selecting cases for the final sample by: #################################

# Keeping only the recoded variables
ess_cci <-
  ess9 |>
    select(idno, anweight, equality, equity, need, entitlement, cntry, 
            eiscedp, eiscedf, eiscedm, eisced, geisced, geiscedp, agea,
            mobility, gender, location, age_group, income_feeling)

# Selecting only those aged 25 or older
ess_cci <- ess_cci |> filter(agea > 24)

# Keeping only the complete cases (listwise deletion)
ess_cci <-
  ess_cci |>
    drop_na(geisced, geiscedp, gender, age_group, location,
            equality, equity, need, entitlement, income_feeling)


# Saving data file with the final sample
save(ess_cci, file = "./code/data/ess_cci.RData")

