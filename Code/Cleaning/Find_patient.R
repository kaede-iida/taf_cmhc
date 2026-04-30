###################################################################### Libraries
library(tidyverse)
library(arrow)
library(here)
library(readxl)

###################################################################### Functions
# Find eligible patients 
patient_finder <- function(min_year, state){
  # Look at those who have been treated at CMHC at anytime during 2021-2022
  path_to_ot <- paste0("/gpfs/milgram/pi/medicaid_lab/data/cms/ingested/TMSIS_taf/taf_other_services_header/year=", 
                       as.character(min_year), "/state=", state, "/data.parquet")
  next_year <- min_year + 1
  path_ot_next <- paste0("/gpfs/milgram/pi/medicaid_lab/data/cms/ingested/TMSIS_taf/taf_other_services_header/year=", 
                         as.character(next_year), "/state=", state, "/data.parquet")
  
  ot_head <- open_dataset(path_to_ot) |>
    select(BENE_ID, BLG_PRVDR_NPI) |>
    filter(BLG_PRVDR_NPI %in% cmhc_npis$NPI) |>                # I will have this list ready
    collect() 
  
  oth_next <- open_dataset(path_ot_next) |>
    select(BENE_ID, BLG_PRVDR_NPI) |>
    filter(BLG_PRVDR_NPI %in% cmhc_npis$NPI) |>                # I will have this list ready
    collect()  
  
  bene_both_year <- unique(c(ot_head$BENE_ID, oth_next$BENE_ID))
  print(paste0(length(bene_both_year), " ", state, "Unfiltered Patient Count"))
  
  # Check these demographics 
  # First year (2021?)
  path_demo_base <- paste0("/gpfs/milgram/pi/medicaid_lab/data/cms/ingested/TMSIS_taf/taf_demog_elig_base/year=", 
                           as.character(min_year), "/state=", state, "/data.parquet")
  
  demo_base <- open_dataset(path_demo_base) |>
    select(BENE_ID, AGE, SEX_CD, all_of(starts_with("MDCD_ENRLMT_DAYS")), all_of(starts_with("DUAL_ELGBL_CD_"))) |>
    select(-c("DUAL_ELGBL_CD_LTST", "MDCD_ENRLMT_DAYS_YR")) |>
    filter(BENE_ID %in% bene_both_year) |>
    filter(AGE >= 13 & AGE <= 62) |>
    collect() |>
    # Check for dual eligibility and continuous enrollment
    mutate(across(starts_with("DUAL_ELGBL_CD_"), ~ as.numeric(.))) |>
    mutate(dual_months = rowSums(pick(starts_with("DUAL_ELGBL_CD_")), na.rm = TRUE),
           enrolled_days = rowSums(pick(starts_with("MDCD_ENRLMT_DAYS")), na.rm = TRUE)) |>
    filter(dual_months == 0,
           enrolled_days >= 360) |>
    select(BENE_ID, AGE, SEX_CD)
  
  # Next year (2022??)
  # No need to check age 
  path_demo_next <- paste0("/gpfs/milgram/pi/medicaid_lab/data/cms/ingested/TMSIS_taf/taf_demog_elig_base/year=", 
                           as.character(next_year), "/state=", state, "/data.parquet")
  
  demo_next <- open_dataset(path_demo_next) |>
    select(BENE_ID, all_of(starts_with("MDCD_ENRLMT_DAYS")), all_of(starts_with("DUAL_ELGBL_CD_"))) |>
    select(-c("DUAL_ELGBL_CD_LTST", "MDCD_ENRLMT_DAYS_YR")) |>
    filter(BENE_ID %in% demo_base$BENE_ID) |>
    collect() |>
    # Check for dual eligibility and continuous enrollment
    mutate(across(starts_with("DUAL_ELGBL_CD_"), ~ as.numeric(.))) |>
    mutate(dual_months = rowSums(pick(starts_with("DUAL_ELGBL_CD_")), na.rm = TRUE),
           enrolled_days = rowSums(pick(starts_with("MDCD_ENRLMT_DAYS")), na.rm = TRUE)) |>
    filter(dual_months == 0,
           enrolled_days >= 360) |>
    select(BENE_ID)
    
  # Combine the other services file, and then filter for eligibility
  sample <- demo_base %>% 
    filter(BENE_ID %in% demo_next$BENE_ID) %>% 
    group_by(BENE_ID) %>% 
    slice(1) %>% 
    ungroup()
  
  print(paste0(length(sample$BENE_ID), " ", state, "Filtered Total Sample"))
  
  return(sample)
}

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
    left_join(full_sample, by = "BENE_ID")
  
  return(new_patient_demog)
}

# Function to use on new patients: previous year health history 
health_history <- function(min_year, state, data){
  # BH related inpatient care
  path_to_ip <- paste0("/gpfs/milgram/pi/medicaid_lab/data/cms/ingested/TMSIS_taf/taf_inpatient_header/year=", 
                                     as.character(min_year), "/state=", state, "/data.parquet")
  
  inpatient <- open_dataset(path_to_ip) |>
    select(BENE_ID, IP_MH_DGNS_IND, IP_SUD_DGNS_IND) |>
    filter(BENE_ID %in% data$BENE_ID) |>
    filter(IP_MH_DGNS_IND == 1 | IP_SUD_DGNS_IND == 1) |>
    select(BENE_ID) |>
    collect() |>
    distinct(BENE_ID) |>
    mutate(bh_inpatient = 1) 
  
  # ED visits (check if BH related)
  # How to identify ED visits 
  ed_prcdr <- read_xlsx(here("Trunk", "Raw", "HEDIS_all_value_set_to_code.xlsx")) |>
    filter(`Value Set Name` == "ED") |>
    filter(`Code System` == "CPT") |>
    pull(Code)
  
  ed_pos <- "23"
  
  path_to_oth <- paste0("/gpfs/milgram/pi/medicaid_lab/data/cms/ingested/TMSIS_taf/taf_other_services_header/year=", 
                       as.character(min_year), "/state=", state, "/data.parquet")
  
  path_to_otl <- paste0("/gpfs/milgram/pi/medicaid_lab/data/cms/ingested/TMSIS_taf/taf_other_services_line/year=", 
                        as.character(min_year), "/state=", state, "/data.parquet")
  
  ed_lines <- open_dataset(path_to_otl) |>
    select(BENE_ID, CLM_ID, LINE_PRCDR_CD) |>
    filter(BENE_ID %in% data$BENE_ID) |>
    filter(LINE_PRCDR_CD %in% ed_prcdr) |>
    collect()
  
  ed_visits <- open_dataset(path_to_oth) |>
    select(CLM_ID, BENE_ID, MH_DGNS_IND, SUD_DGNS_IND, POS_CD) |>
    filter(POS_CD %in% ed_pos) |>
    filter(CLM_ID %in% ed_lines$CLM_ID) |>
    select(BENE_ID, MH_DGNS_IND, SUD_DGNS_IND) |>
    collect() |>
    mutate(bh_ed = if_else(MH_DGNS_IND == 1 | SUD_DGNS_IND == 1, 1, 0),
           any_ed = 1) |>
    select(BENE_ID, bh_ed, any_ed) |>
    # One patient one row
    group_by(BENE_ID) |>
    mutate(bh_ed_year = if_else(max(bh_ed) >= 1, 1, 0),
           any_ed_year = if_else(max(any_ed) >= 1, 1, 0)) %>% 
    slice(1) %>% 
    select(BENE_ID, bh_ed_year, any_ed_year)
  
  # Combine all information 
  last_year_health <- data %>% 
    left_join(inpatient, by = "BENE_ID") %>% 
    left_join(ed_visits, by = "BENE_ID") %>% 
    # If bene does not exist in the dataset, means there were no history 
    mutate(bh_inpatient = if_else(is.na(bh_inpatient), 0, bh_inpatient),
           bh_ed_year = if_else(is.na(bh_ed_year), 0, bh_ed_year),
           any_ed_year = if_else(is.na(any_ed_year), 0, any_ed_year))
  
  return(last_year_health)
}


############################################################ Using the function
# Lists 
# First try with only Illinois 
cmhc_npis <- read_csv(here("Trunk", "Derived", "for_stata_matching", "address_groups.csv")) %>% 
  mutate(NPI = as.character(NPI))

states <- c("IL")

# Run
for(st in states){
  # Get the CMHC visitors (filter for enrollment and eligibility)
  full_sample <- patient_finder(2021, st)
  
  # Identifying new patients, finding out their 
  new_patients <- new_patient(2021, st, full_sample)
  
  # Find out previous year health history 
  last_year <- health_history(2021, st, new_patients)
  
  # Clean up: change var names, etc
  all_info <- last_year %>% 
    rename(visit1_clm_id = CLM_ID,
           visit1_date = SRVC_BGN_DT, 
           visit1_dgns_1 = DGNS_CD_1,
           visit1_dgns_2 = DGNS_CD_2,
           last_year_bh_ip = bh_inpatient,
           last_year_bh_ed = bh_ed_year,
           last_year_any_ed = any_ed_year) %>% 
    select(BENE_ID, org_ID, AGE, SEX_CD, BLG_PRVDR_NPI, state, repeat_visits,
           visit1_clm_id, visit1_date, visit1_dgns_1, visit1_dgns_2, 
           last_year_bh_ip, last_year_bh_ed, last_year_any_ed)
  
  print(paste0(st, " done"))
  save_name <- paste0("tab1_chars_", st, ".csv")
  write_csv(all_info, here("Trunk", "Derived", "new_patient_char", save_name))
}

