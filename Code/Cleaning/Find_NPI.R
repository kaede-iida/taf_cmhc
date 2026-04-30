# Since a lot of states have lot of missing POS_CD, I will find NPIs that are 
# already asigned the CMHC POS_CD and put this into NPESS to get the full NPI list

###################################################################### Libraries 
library(tidyverse)
library(arrow)
library(here)

###################################################################### Functions
cmhc_npi_finder <- function(state, year){
  # Outpatient 
  path_ot_h <- paste0("/gpfs/milgram/pi/medicaid_lab/data/cms/ingested/TMSIS_taf/taf_other_services_header/year=", 
                      as.character(year), "/state=", state, "/data.parquet")
  
  cmhc_npi_y <- open_dataset(path_ot_h) |>
    select(POS_CD, BLG_PRVDR_NPI) |>
    filter(POS_CD == "53") |>
    distinct(BLG_PRVDR_NPI) |>
    collect()
  
  return(cmhc_npi_y)
}

########################################################################## Using
states <- c("NY", "PA", "IL", "OH", "MI", "VA", "AZ")
years <- c(2016:2022)

all_states <- vector("list", length = length(states))

for(st in seq_along(states)){
  all_years <- vector("list", length = length(years))
  
  for(y in seq_along(years)){
    this_y <- cmhc_npi_finder(states[[st]], years[[y]])
    
    all_years[[y]] <- this_y
  }
  
  all_years <- bind_rows(all_years)
  
  # Remove duplicates 
  this_state <- all_years %>% 
    distinct(BLG_PRVDR_NPI) %>% 
    mutate(state = states[[st]])
  
  all_states[[st]] <- this_state
  
  print(paste0(states[[st]], " done"))
}

all_states <- bind_rows(all_states)
# write_parquet(all_states, here("Trunk", "Derived", "all_states_cmhc.parquet"))


############################################################# Combine with NPPES
library(haven)

all_states <- open_dataset(here("Trunk", "Derived", "all_states_cmhc.parquet")) |>
  collect() |>
  filter(!is.na(BLG_PRVDR_NPI))

# Want to know how many NPIs found per state 
found_npi_count <- all_states %>% 
  group_by(state) %>% 
  summarise(count = n_distinct(BLG_PRVDR_NPI)) 
  

# NPPES main 
main_dt <- read_csv(here("Trunk", "Raw", "type_2_npi.csv")) %>% 
  rename(org_name = `Provider Organization Name (Legal Business Name)`,
         other_org_name = `Provider Other Organization Name`,
         address_first_line = `Provider First Line Business Practice Location Address`,
         address_second_line = `Provider Second Line Business Practice Location Address`,
         city_name = `Provider Business Practice Location Address City Name`,
         state = `Provider Business Practice Location Address State Name`,
         postal_code = `Provider Business Practice Location Address Postal Code`,
         deact_date = `NPI Deactivation Date`) %>% 
  select(NPI, org_name, other_org_name, address_first_line, address_second_line, city_name, state, postal_code, deact_date) %>% 
  mutate(NPI = as.character(NPI)) 

# This is the known CMHCs
cmhc_address <- all_states %>% 
  distinct(BLG_PRVDR_NPI, state) %>% 
  rename(clm_state = state) %>% 
  left_join(main_dt, by = c("BLG_PRVDR_NPI" = "NPI")) %>% 
  mutate(matched = if_else(is.na(org_name), 0, 1))

# Number of matches per state
match_npi_count <- cmhc_address %>% 
  group_by(clm_state) %>% 
  summarise(total = n_distinct(BLG_PRVDR_NPI),
            match = sum(matched)) %>% 
  mutate(prop = (match/total)*100)

write_dta(cmhc_address, here("Trunk", "Derived", "for_stata_matching", "known_cmhc.dta"))

# I could filter to the postal codes that appear in the cmhc_address
# The matching will always include a state so this will not drop anything 
# Should only look at the first five digits (some rows use the longer postal code)
cmhc_postal <- cmhc_address %>% 
  filter(!is.na(postal_code)) %>% 
  mutate(first_five = str_sub(postal_code, 1, 5)) %>% 
  pull(first_five)

main_dt <- main_dt %>% 
  mutate(first_five = str_sub(postal_code, 1, 5)) %>% 
  filter(first_five %in% cmhc_postal) %>% 
  select(-c("first_five"))

write_dta(main_dt, here("Trunk", "Derived", "for_stata_matching", "possible_cmhc.dta"))


# Do the matching in stata 
  