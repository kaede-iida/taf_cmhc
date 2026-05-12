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
  
  # All January visitors and their diagnosis 
  # Keep all unique patient - org pair 
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
  
  # Next limit the Jan visitors to only new paitents (look back to 2021) 
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
    left_join(only_jan, by = c("BENE_ID", "org_ID"))       # Add information about the jan 2022 visit 
  
  
  # Determining spell length + differentiate between one month and one-time visitors!!!!
  new_bene_cmhc <- new_patient %>% 
    distinct(BENE_ID, org_ID)
  
  this_year <- open_dataset(path_ot_next) |>
    select(BENE_ID, BLG_PRVDR_NPI, CLM_ID, SRVC_BGN_DT) |>
    left_join(cmhc_npis, by = c("BLG_PRVDR_NPI" = "NPI")) |>   # add org_ID
    semi_join(new_bene_cmhc, by = c("BENE_ID", "org_ID")) |>   # only keep if new patient jan visitor 
    filter(SRVC_BGN_DT >= "2022-01-01" & SRVC_BGN_DT <= "2022-12-31") |>
    collect() |>
    mutate(visit_mo = month(SRVC_BGN_DT)) |>
    # Check for one-time visits 
    group_by(BENE_ID, org_ID) |>
    mutate(repeat_visits = n_distinct(SRVC_BGN_DT),
           one_time_visit = if_else(repeat_visits == 1, 1, 0)) |>
    ungroup() 
  
  month_unique <- this_year %>% 
    distinct(BENE_ID, org_ID, visit_mo)
  
  patient_spell <- this_year %>% 
    group_by(BENE_ID, org_ID) %>%
    summarise(consecutive_months = {
      months_present <- sort(unique(visit_mo))
      streak <- 0L
      for (m in 1:12) {
        if (m %in% months_present) streak <- streak + 1L
        else break
      }
      streak}) %>%
    right_join(this_year, by = c("BENE_ID", "org_ID")) %>%  
    # Remove all the visits that happen after the spell break 
    filter(consecutive_months >= month(SRVC_BGN_DT)) %>% 
    group_by(BENE_ID, org_ID) %>% 
    mutate(last_visit_date = max(SRVC_BGN_DT)) %>% 
    ungroup() %>% 
    # all info needed in one row, only keep the first visit
    filter(CLM_ID %in% new_patient$CLM_ID) %>% 
    select(BENE_ID, org_ID, consecutive_months, last_visit_date, repeat_visits, one_time_visit)
  
  # Add this to the new patient info 
  new_patient_all <- new_patient %>% 
    left_join(patient_spell, by = c("BENE_ID", "org_ID")) %>% 
    left_join(full_sample, by = "BENE_ID")
  
  return(new_patient_all)
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
    left_join(cmhc_npis, by = c("BLG_PRVDR_NPI" = "NPI")) |>
    select(-c("state")) |>
    rename(followup_org_ID = org_ID)
  
  dt_ot_bh <- data %>% 
    left_join(ot_head, by = "BENE_ID", relationship = "many-to-many") %>% 
    group_by(BENE_ID, org_ID) %>% 
    summarise(
      mh_other_cmhc = as.integer(any(
        SRVC_BGN_DT > last_visit_date & MH_DGNS_IND == 1 & 
          !is.na(followup_org_ID) &      # Is a CMHC
          followup_org_ID != org_ID       # Is not the same CMHC (> last_visit_date takes care of this)
      )),
      mh_non_cmhc = as.integer(any(
        SRVC_BGN_DT > last_visit_date & MH_DGNS_IND == 1 & 
          is.na(followup_org_ID)          # Is not a CMHC
      )),
      any_mental = as.integer(any(
        SRVC_BGN_DT > last_visit_date & MH_DGNS_IND == 1 
      ))
    ) %>% 
    right_join(data, by = c("BENE_ID", "org_ID")) %>% 
    mutate(across(c(mh_other_cmhc, mh_non_cmhc, any_mental), ~replace_na(.x, 0)))
  
  
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
  
  # Rename variables 
  # I do this in the health_history function for the alive people
  # But I don't include dead people for the health hisotry <- is this fine? 
  
  death_sample <- death_sample %>% 
    rename(visit1_clm_id = CLM_ID,
           visit1_date = SRVC_BGN_DT, 
           visit1_dgns_1 = DGNS_CD_1,
           visit1_dgns_2 = DGNS_CD_2)
  
  post_cmhc <- post_cmhc_checker(death_sample, states[[st]], 2022)
  
  all_states[[st]] <- post_cmhc
}

all_states <- bind_rows(all_states)
write_csv(all_states, here("Trunk", "Derived", "new_patient_char", "post_cmhc", "dead_post_cmhc.csv"))