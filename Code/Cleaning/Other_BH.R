###################################################################### Libraries
library(tidyverse)
library(arrow)
library(here)
library(readxl)


# This is for making table 3 
# Finding other BH encounters (during 2022) for the 2022 new patients 

###################################################################### Functions 
# Finding things from the other services file 
# crisis services
# mental health treatment (visit with mental health diagnosis code) 
# outpatient bh treatment in general
# ED BH visit 

# Death - DE base file 
# IP BH visit - Inpatient file 

post_cmhc_checker <- function(data, state, year){
  path_to_oth <- paste0("/gpfs/milgram/pi/medicaid_lab/data/cms/ingested/TMSIS_taf/taf_other_services_header/year=", 
                       as.character(year), "/state=", state, "/data.parquet")
  path_to_otl <- paste0("/gpfs/milgram/pi/medicaid_lab/data/cms/ingested/TMSIS_taf/taf_other_services_line/year=", 
                         as.character(year), "/state=", state, "/data.parquet")
  path_to_deb <- paste0("/gpfs/milgram/pi/medicaid_lab/data/cms/ingested/TMSIS_taf/taf_demog_elig_base/year=", 
                        as.character(year), "/state=", state, "/data.parquet")
  path_to_iph <- paste0("/gpfs/milgram/pi/medicaid_lab/data/cms/ingested/TMSIS_taf/taf_inpatient_header/year=", 
                       as.character(year), "/state=", state, "/data.parquet")
  
  # Crisis services & ED visit 
  ot_line <- open_dataset(path_to_otl) |>
    select(CLM_ID, BENE_ID, LINE_PRCDR_CD) |>
    filter(BENE_ID %in% data$BENE_ID) |>
    filter(LINE_PRCDR_CD %in% crisis_prcdr | LINE_PRCDR_CD %in% ed_prcdr) |>
    collect() 
  
  # Have to find out the date, also filter further for the ED visits  
  crisis <- open_dataset(path_to_oth) |>
    select(CLM_ID, SRVC_BGN_DT, POS_CD, MH_DGNS_IND, SUD_DGNS_IND) |>
    right_join(ot_line, by = "CLM_ID") |>
    collect() |>
    # If ED, it also has to have the correct POS CD 
    filter(LINE_PRCDR_CD %in% crisis_prcdr | (LINE_PRCDR_CD %in% ed_prcdr & POS_CD %in% ed_pos)) |>
    # If ED, it also has to have to be for MH or SUD 
    filter(LINE_PRCDR_CD %in% crisis_prcdr | (LINE_PRCDR_CD %in% ed_prcdr & (MH_DGNS_IND == 1 | SUD_DGNS_IND == 1))) |>
    # only keep the latest dates (because the only thing we care is that it happens after the first visit to cmhc)
    # only leaving the latest date means that there is max chance of it happening after visit 1 
    group_by(BENE_ID) |>
    mutate(crisis_date = if_else(LINE_PRCDR_CD %in% crisis_prcdr, max(SRVC_BGN_DT), NA),
           ed_date = if_else(LINE_PRCDR_CD %in% ed_prcdr, max(SRVC_BGN_DT), NA)) %>% 
    fill(crisis_date, .direction = "downup") %>% 
    fill(ed_date, .direction = 'downup') %>% 
    slice(1) %>% 
    ungroup() %>% 
    select(BENE_ID, crisis_date, ed_date)
  
  dt_crisis <- data %>% 
    left_join(crisis, by = "BENE_ID", relationship = "many-to-many") %>%  # Multiple BENE if different org_ID 
    # Is there a crisis service after the first visit
    mutate(crisis_services = if_else(visit1_date < crisis_date, 1, 0),
           crisis_services = if_else(is.na(crisis_services), 0, crisis_services),
           bh_ed_visit = if_else(visit1_date < ed_date, 1, 0),
           bh_ed_visit = if_else(is.na(bh_ed_visit), 0, bh_ed_visit))
  
  
  # Mental helath tx, BH treatment, 
  ot_head <- open_dataset(path_to_oth) |>
    select(BENE_ID, MH_DGNS_IND, SUD_DGNS_IND, BLG_PRVDR_NPI) |>
    filter(BENE_ID %in% data$BENE_ID) |>
    filter(MH_DGNS_IND == 1 | SUD_DGNS_IND == 1) |>
    collect() |>
    mutate(any_bh = if_else((MH_DGNS_IND == 1 | SUD_DGNS_IND == 1), 1, 0)) |>
    # Add the org_ID 
    left_join(cmhc_npis, by = c("BLG_PRVDR_NPI" = "NPI")) |>
    select(-c("state", "SUD_DGNS_IND"))
  
  check <- ot_head |>
    ### Think about how to filter out the same CMHCs in the morning
    
  
  # Inpatient 
  inpatient <- open_dataset(path_to_iph) |>
    select(BENE_ID, SRVC_BGN_DT, IP_MH_DGNS_IND, IP_SUD_DGNS_IND) |>
    filter(BENE_ID %in% data$BENE_ID) |>
    filter(IP_MH_DGNS_IND == 1 | IP_SUD_DGNS_IND == 1) |>
    collect() |>
    group_by(BENE_ID) |>
    mutate(ip_bh_date = max(SRVC_BGN_DT)) |>
    slice(1) |>
    ungroup() |>
    select(BENE_ID, ip_bh_date)
  
  dt_inpatient <- dt_crisis %>%             ### Change this after finishing the MH part 
    left_join(inpatient, by = "BENE_ID", relationship = "many-to-many") %>% 
    mutate(bh_inpatient = if_else(visit1_date < ip_bh_date, 1, 0),
           bh_inpatient = if_else(is.na(bh_inpatient), 0, bh_inpatient))
  
  # Death 
  death <- open_dataset(path_to_deb) |>
    select(BENE_ID, DEATH_DT, DEATH_IND) |>
    filter(BENE_ID %in% data$BENE_ID) |>
    collect() |>
    group_by(BENE_ID) |>
    slice(1)
  
  dt_death <- dt_inpatient %>% 
    left_join(death, by = "BENE_ID") 
}


############################################################### Use the function 
# Lists 
states <- c("IL")

# Crisis services 
crisis_prcdr <- c("H0007", "H0030", "H2011", "S9484", "S9485", "T2034", "90839", "90840")

# ED Visit 
ed_prcdr <- read_xlsx("/gpfs/milgram/project/busch/kei9/ccbhc/trunk/raw/hedis/HEDIS_all_value_set_to_code.xlsx") |>
  filter(`Value Set Name` == "ED") |>
  filter(`Code System` == "CPT") |>
  pull(Code)

ed_pos <- "23"

# First try with only Illinois 
cmhc_npis <- read_csv(here("Trunk", "Derived", "for_stata_matching", "address_groups.csv")) %>% 
  mutate(NPI = as.character(NPI))

# Prepare new patient data 
all_states <- vector("list", length = length(states))
for(st in seq_along(states)){
  
  save_name <- paste0("tab1_chars_", states[[st]], ".csv")
  state <- read_csv(here("Trunk", "Derived", "new_patient_char", save_name)) |>
    select(BENE_ID, org_ID, state, BLG_PRVDR_NPI, repeat_visits, visit1_date)
  
  all_states[[st]] <- state
}

all_states <- bind_rows(all_states)

