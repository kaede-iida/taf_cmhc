# Trying to find the right state to do this project 
# Use the 2021 and 2022 data 

# Variables that I will use for the project: 
# BENE_ID, POS_CD, DGNS_CD_1 (I will use 2 too but missing 2 might just mean they only have one diagnosis), 
# AGE, starts_with(DUAL_ELGBL_CD_), starts_with(MDCD_ENRLMT_DAYS)
# For all vars, I want to know if there is too much NA values, but for the POS_CD
# I should also check that there is enough claims (because some states do not use the CMHC classification)

# Other criteria of states 
# Big population 
# Uses the CMHC classification 

###################################################################### Libraries 
library(tidyverse)
library(arrow)
library(here)

###################################################################### Functions 
# For each state and minimum year, find out the number of NA values
state_checker <- function(state, min_year){
  # Other services variables 
  path_to_ot <- paste0("/gpfs/milgram/pi/medicaid_lab/data/cms/ingested/TMSIS_taf/taf_other_services_header/year=", 
                       as.character(min_year), "/state=", state, "/data.parquet")
  next_year <- min_year + 1
  path_ot_next <- paste0("/gpfs/milgram/pi/medicaid_lab/data/cms/ingested/TMSIS_taf/taf_other_services_header/year=", 
                         as.character(next_year), "/state=", state, "/data.parquet")
  
  ot_head <- open_dataset(path_to_ot) |>
    select(BENE_ID, POS_CD, DGNS_CD_1) |>
    summarise(
      n_total = n(),
      bene_na = sum(is.na(BENE_ID)),
      pos_na  = sum(is.na(POS_CD)),
      dgns_na = sum(is.na(DGNS_CD_1))
    ) |>
    mutate(
      bene_na_prop = bene_na / n_total,
      pos_na_prop  = pos_na  / n_total,
      dgns_na_prop = dgns_na / n_total
    ) |>
    collect() |>
    mutate(state = state,
           year = min_year)
    
  oth_next <- open_dataset(path_ot_next) |>
    select(BENE_ID, POS_CD, DGNS_CD_1) |>
    summarise(
      n_total = n(),
      bene_na = sum(is.na(BENE_ID)),
      pos_na  = sum(is.na(POS_CD)),
      dgns_na = sum(is.na(DGNS_CD_1))
    ) |>
    mutate(
      bene_na_prop = bene_na / n_total,
      pos_na_prop  = pos_na  / n_total,
      dgns_na_prop = dgns_na / n_total
    ) |>
    collect() |>
    mutate(state = state,
           year = next_year)
  
  other_both <- rbind(ot_head, oth_next) |>
    select(n_total, bene_na_prop, pos_na_prop, dgns_na_prop, state, year) |>
    rename(n_claim = n_total)
  
  
  # Demographics and eligibility file 
  path_demo_base <- paste0("/gpfs/milgram/pi/medicaid_lab/data/cms/ingested/TMSIS_taf/taf_demog_elig_base/year=", 
                           as.character(min_year), "/state=", state, "/data.parquet")
  
  demo_elig <- open_dataset(path_demo_base) |>
    select(BENE_ID, AGE, all_of(starts_with("DUAL_ELGBL_CD_"))) |>
    select(-c("DUAL_ELGBL_CD_LTST")) |>
    collect() |>
    mutate(
      n_dual_na = rowSums(is.na(across(all_of(starts_with("DUAL_ELGBL_CD_"))))),
      any_dual_na_row = if_else(n_dual_na > 0, 1, 0),
      all_dual_na_row = if_else(n_dual_na == 12, 1, 0)
    ) |>
    summarise(
      n_total = n(),
      age_na = sum(is.na(AGE)),
      any_dual_na = sum(any_dual_na_row),
      all_dual_na = sum(all_dual_na_row)
    ) |>
    mutate(
      age_na_prop = age_na / n_total,
      any_dual_prop = any_dual_na / n_total,
      all_dual_prop = all_dual_na / n_total
    ) |>
    mutate(state = state,
           year = min_year)
  
  path_demo_next <- paste0("/gpfs/milgram/pi/medicaid_lab/data/cms/ingested/TMSIS_taf/taf_demog_elig_base/year=", 
                           as.character(next_year), "/state=", state, "/data.parquet")
  
  demo_elig_next <- open_dataset(path_demo_next) |>
    select(BENE_ID, AGE, all_of(starts_with("DUAL_ELGBL_CD_"))) |>
    select(-c("DUAL_ELGBL_CD_LTST")) |>
    collect() |>
    mutate(
      n_dual_na = rowSums(is.na(across(all_of(starts_with("DUAL_ELGBL_CD_"))))),
      any_dual_na_row = if_else(n_dual_na > 0, 1, 0),
      all_dual_na_row = if_else(n_dual_na == 12, 1, 0)
    ) |>
    summarise(
      n_total = n(),
      age_na = sum(is.na(AGE)),
      any_dual_na = sum(any_dual_na_row),
      all_dual_na = sum(all_dual_na_row)
    ) |>
    mutate(
      age_na_prop = age_na / n_total,
      any_dual_prop = any_dual_na / n_total,
      all_dual_prop = all_dual_na / n_total
    ) |>
    mutate(state = state,
           year = next_year)
  
  demo_elig_both <- rbind(demo_elig, demo_elig_next) |>
    rename(n_bene = n_total) |>
    select(n_bene, age_na_prop, any_dual_prop, all_dual_prop, state, year)
  
  # Combine the other service and demographic and eligibility 
  this_state <- left_join(other_both, demo_elig_both, by = c("state", "year"))
  
  return(this_state)
}

########################################################### Using the functions 
# Check the biggest population states - the states do not
states <- c("TX", "PA", "IL", "OH", "NC", "MI", "VA", "WA", "AZ", "TN")
states <- c("TX", "NY", "PA", "IL", "OH", "GA", "NC", "MI", "VA", "AZ", "TN")

all_states <- vector("list", length = length(states))

for(st in seq_along(states)){
  this_state <- state_checker(states[[st]], 2018)
  all_states[[st]] <- this_state
  print(paste0(states[[st]], " done"))
}

all_states <- bind_rows(all_states)

all_states <- all_states %>% 
  mutate(
    across(
      ends_with("_prop"),
      ~ round(. * 100, 2)
    )
  ) %>% 
  select(state, year, n_claim, bene_na_prop, pos_na_prop, dgns_na_prop, n_bene, age_na_prop, all_dual_prop, any_dual_prop)

# write_csv(all_states, here("Output", "Checks", "NA_vars_2021.csv"))
write_csv(all_states, here("Output", "Checks", "NA_vars_2018.csv"))