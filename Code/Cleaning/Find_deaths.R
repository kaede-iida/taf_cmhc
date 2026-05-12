###################################################################### Libraries
library(tidyverse)
library(arrow)
library(here)
library(readxl)


###################################################################### Functions 
death_after_cmhc <- function(state, year){
  
  # First find the death 
  path_to_deb <- paste0("/gpfs/milgram/pi/medicaid_lab/data/cms/ingested/TMSIS_taf/taf_demog_elig_base/year=", 
                        as.character(year), "/state=", state, "/data.parquet")
  death <- open_dataset(path_to_deb) |>
    select(BENE_ID, DEATH_DT, DEATH_IND) |>
    # DEATH_IND is only measured at some point in the year, so if death date is 
    # populated that means someone was dead
    filter(!is.na(DEATH_DT) | DEATH_IND == 1) |>
    collect()
  
  # Were they continuously enrolled in 2021, and fit the age and eligibility?
  pre_year <- year - 1
  path_to_deb_pre <-  paste0("/gpfs/milgram/pi/medicaid_lab/data/cms/ingested/TMSIS_taf/taf_demog_elig_base/year=", 
                             as.character(pre_year), "/state=", state, "/data.parquet")
  
  elig_death <- open_dataset(path_to_deb_pre) |>
    select(BENE_ID, AGE, SEX_CD, all_of(starts_with("MDCD_ENRLMT_DAYS")), all_of(starts_with("DUAL_ELGBL_CD_"))) |>
    select(-c("DUAL_ELGBL_CD_LTST", "MDCD_ENRLMT_DAYS_YR")) |>
    filter(BENE_ID %in% death$BENE_ID) |>
    filter(AGE >= 13 & AGE <= 62) |>
    collect() |>
    # Check for dual eligibility and continuous enrollment
    mutate(across(starts_with("DUAL_ELGBL_CD_"), ~ as.numeric(.))) |>
    mutate(dual_months = rowSums(pick(starts_with("DUAL_ELGBL_CD_")), na.rm = TRUE),
           enrolled_days = rowSums(pick(starts_with("MDCD_ENRLMT_DAYS")), na.rm = TRUE)) |>
    filter(dual_months == 0,
           enrolled_days >= 360) |>
    select(BENE_ID, AGE, SEX_CD) |>
    left_join(death, by = "BENE_ID")
  
  return(elig_death)

}

# Then use the new_patient function from Find_patient.R
# This will find patients in jan, and look back for no visits in 2021 

# Identifying new patients + diagnosis at first visit + identifying repeaters
new_patient <- function(min_year, state, full_sample){
  # First limit to people who had a visit to a CMHC in jan 2022
  # If visited multiple CMHC, keep everything 
  # Keep the diagnosis at first visit
  next_year <- min_year + 1
  path_ot_next <- paste0("/gpfs/milgram/pi/medicaid_lab/data/cms/ingested/TMSIS_taf/taf_other_services_header/year=", 
                         as.character(next_year), "/state=", state, "/data.parquet")
  
  only_jan <- open_dataset(path_ot_next) %>% 
    select(BENE_ID, BLG_PRVDR_NPI, DGNS_CD_1, DGNS_CD_2, SRVC_BGN_DT, CLM_ID) %>% 
    filter(BENE_ID %in% full_sample$BENE_ID) %>% 
    filter(BLG_PRVDR_NPI %in% cmhc_npis$NPI) %>%
    mutate(Visit_mo = month(SRVC_BGN_DT)) %>% 
    filter(Visit_mo == 1) %>% 
    collect() %>% 
    # Only leave the first visit for each org
    left_join(cmhc_npis, by = c("BLG_PRVDR_NPI" = "NPI")) %>% 
    group_by(BENE_ID, org_ID) %>% 
    arrange(SRVC_BGN_DT) %>% 
    slice(1) %>% 
    ungroup()
  
  # Next look back to 2020 and check that there were no CMHC visits 
  # Do this check for every patient - CMHC pair 
  # Check for BH related ED, and any ED 
  
  # Find unique pairs of BENE_ID and CMHC org_ID
  bene_cmhc <- only_jan %>% 
    distinct(BENE_ID, org_ID)
  
  path_to_ot <- paste0("/gpfs/milgram/pi/medicaid_lab/data/cms/ingested/TMSIS_taf/taf_other_services_header/year=", 
                       as.character(min_year), "/state=", state, "/data.parquet")
  
  last_year <- open_dataset(path_to_ot) %>% 
    select(BENE_ID, BLG_PRVDR_NPI) %>% 
    filter(BENE_ID %in% bene_cmhc$BENE_ID) %>% 
    filter(BLG_PRVDR_NPI %in% only_jan$BLG_PRVDR_NPI) %>% 
    collect() %>% 
    left_join(cmhc_npis, by = c("BLG_PRVDR_NPI" = "NPI")) %>% 
    distinct(BENE_ID, org_ID) 
  
  new_patient <- bene_cmhc %>% 
    anti_join(last_year, by = c("BENE_ID", "org_ID")) %>%  # Remove if the cmhc-patient pair was found in the previous year 
    left_join(only_jan, by = c("BENE_ID", "org_ID")) %>%   # Add information about the jan 2022 visit 
    group_by(BENE_ID, BLG_PRVDR_NPI) %>%
    filter()
  
  
  # Now look after the CMHC and look at the repeats 
  this_year <- open_dataset(path_ot_next) |>
    select(BENE_ID, BLG_PRVDR_NPI, CLM_ID, SRVC_BGN_DT) |>
    filter(BENE_ID %in% new_patient$BENE_ID) |>
    filter(BLG_PRVDR_NPI %in% new_patient$BLG_PRVDR_NPI) |>
    filter(!(CLM_ID %in% new_patient$CLM_ID)) |>  # Don't look at the first CMHC visit
    collect() |>
    left_join(cmhc_npis, by = c("BLG_PRVDR_NPI" = "NPI")) |>
    # Change this part later when I need a more granular check of repeaters 
    group_by(BENE_ID, org_ID) |>
    mutate(repeat_visits = n_distinct(SRVC_BGN_DT)) |> # Repeat is counted by days rather than claims 
    slice(1) |>
    ungroup() |>
    select(BENE_ID, org_ID, repeat_visits)
  
  # Add this to the new patient info 
  new_patient_repeat <- new_patient %>% 
    left_join(this_year, by = c("BENE_ID", "org_ID")) %>% 
    mutate(repeat_visits = if_else(is.na(repeat_visits), 0, repeat_visits))
  
  # Add age and sex from the full sample dataset
  new_patient_demog <- new_patient_repeat %>% 
    left_join(full_sample, by = "BENE_ID") %>% 
    rename(visit1_date = SRVC_BGN_DT)
  
  return(new_patient_demog)
}

# Check for 2022 any mental health treatment (presence and location)
# This is almost the same as the one in Other_BH.R but removed unnecessary info
post_cmhc_checker <- function(data, state, year){
  path_to_oth <- paste0("/gpfs/milgram/pi/medicaid_lab/data/cms/ingested/TMSIS_taf/taf_other_services_header/year=", 
                        as.character(year), "/state=", state, "/data.parquet")
  
  # Mental helath tx, BH treatment, 
  ot_head <- open_dataset(path_to_oth) |>
    select(BENE_ID, MH_DGNS_IND, SUD_DGNS_IND, BLG_PRVDR_NPI, SRVC_BGN_DT) |>
    filter(BENE_ID %in% data$BENE_ID) |>
    filter(MH_DGNS_IND == 1 | SUD_DGNS_IND == 1) |>
    collect() |>
    mutate(any_bh = if_else((MH_DGNS_IND == 1 | SUD_DGNS_IND == 1), 1, 0)) |>
    # Add the org_ID 
    left_join(cmhc_npis, by = c("BLG_PRVDR_NPI" = "NPI")) |>
    select(-c("state", "SUD_DGNS_IND")) |>
    rename(followup_org_ID = org_ID)
  
  dt_ot_bh <- data |>
    left_join(ot_head, by = "BENE_ID", relationship = "many-to-many") |>
    group_by(BENE_ID, org_ID, visit1_date) |>
    summarise(
      mh_other_cmhc = as.integer(any(
        SRVC_BGN_DT > visit1_date & MH_DGNS_IND == 1 & 
          !is.na(follow_up_org_ID) &      # Is a CMHC
          follow_up_org_ID != org_ID      # Is not the same CMHC
      )),
      mh_non_cmhc = as.integer(any(
        SRVC_BGN_DT > visit1_date & MH_DGNS_IND == 1 & 
          is.na(followup_org_ID)          # Is not a CMHC
      )),
      any_mental = as.integer(any(
        SRVC_BGN_DT > visit1_date & MH_DGNS_IND == 1 & 
          
      ))
    )
    
    summarise(
      mh_other_facility = as.integer(any(
        SRVC_BGN_DT > visit1_date & MH_DGNS_IND == 1 & (is.na(followup_org_ID) | followup_org_ID != org_ID),
        na.rm = TRUE
      )),
      mh_other_cmhc = as.integer(any(
        SRVC_BGN_DT > visit1_date & MH_DGNS_IND == 1 & !is.na(followup_org_ID) & followup_org_ID != org_ID,
        na.rm = TRUE
      )),
      any_bh_visit = as.integer(any(
        SRVC_BGN_DT > visit1_date & any_bh == 1,      # This was not good because it includes same visits at CMHCi
        na.rm = TRUE
      )),
      .groups = "drop"
    ) |>
    select(-visit1_date) |>
    right_join(data, by = c("BENE_ID", "org_ID")) |>
    mutate(across(c(mh_other_facility, mh_other_cmhc, any_bh_visit), ~replace_na(.x, 0)))
  
  
  return(dt_ot_bh)
}

########################## Use the function, add it to the tab3 (post CMHC) data
cmhc_npis <- read_csv(here("Trunk", "Derived", "for_stata_matching", "address_groups.csv")) %>% 
  mutate(NPI = as.character(NPI))

# Lists 
states <- c("IL")

all_states <- vector("list", length = length(states))
for(st in seq_along(states)){
  
  dead_ppl <- death_after_cmhc(states[[st]], 2022)
  
  death_sample <- new_patient(2021, states[[st]], dead_ppl)
  
  post_cmhc <- post_cmhc_checker(death_sample, states[[st]], 2022)
  
  all_states[[st]] <- post_cmhc
}

all_states <- bind_rows(all_states)
write_csv(all_states, here("Trunk", "Derived", "new_patient_char", "post_cmhc", "dead_post_cmhc.csv"))