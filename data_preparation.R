# Independent sector provision of elective surgery

library(tidyverse)
library(janitor)

# Data preparation 

# Import data ----
lsoa11_stp21 <- 
  read_csv("LSOA(2011)_CCG(21)_STP(21).csv") %>% 
  clean_names()

inpatient_data <- 
  read_csv("grouped_data_IP.csv") %>% 
  clean_names() %>% 
  left_join(lsoa11_stp21 %>% 
              select(lsoa11cd,stp21nm, lad21nm), 
            by = c("der_postcode_lsoa_2011_code" = "lsoa11cd")) %>% 
  mutate(sum_costs = as.numeric(sum_cost))

outpatient_data <- 
  read_csv("grouped_data_OP.csv") %>% 
  clean_names() %>% 
  left_join(lsoa11_stp21 %>% 
              select(lsoa11cd,stp21nm, lad21nm), 
            by = c("der_postcode_lsoa_2011_code" = "lsoa11cd")) %>% 
  rename(ethnic_group = ethnic_category) %>% 
  mutate(sum_costs = as.numeric(sum_costs))
# Group and clean ----
hrg_lookup <-
  tribble(
    ~hrg_3, ~procedure_desc, ~procedure_desc_short, ~procedure_group,
    'BZ3', "Cataract and Lens Procedures",       "Cataract",            "Subchapter BZ – Eyes and Periorbita Procedures and Disorders",
    'BZ4', "Oculoplastics Procedures",           "Oculoplastics",       "Subchapter BZ – Eyes and Periorbita Procedures and Disorders",
    'BZ5', "Orbit and Lacrimal Procedures",      "Orbit and Lacrimal",  "Subchapter BZ – Eyes and Periorbita Procedures and Disorders",
    'BZ6', "Cornea and Sclera Procedures",       "Cornea and Sclera",   "Subchapter BZ – Eyes and Periorbita Procedures and Disorders",
    'BZ7', "Ocular Motility Procedures",         "Ocular Motility",     "Subchapter BZ – Eyes and Periorbita Procedures and Disorders",
    'BZ8', "Vitreous Retinal Procedures",        "Vitreous Retinal",    "Subchapter BZ – Eyes and Periorbita Procedures and Disorders",
    'BZ9', "Glaucoma Procedures",                "Glaucoma",            "Subchapter BZ – Eyes and Periorbita Procedures and Disorders",
    
    'HN1', "Hip Procedures for Non-Trauma",      "Hip",                 "Subchapter HN – Orthopaedic Non-Trauma Procedures",
    'HN2', "Knee Procedures for Non-Trauma",     "Knee",                "Subchapter HN – Orthopaedic Non-Trauma Procedures",
    'HN3', "Foot Procedures for Non-Trauma",     "Foot",                "Subchapter HN – Orthopaedic Non-Trauma Procedures",
    'HN4', "Hand Procedures for Non-Trauma",     "Hand",                "Subchapter HN – Orthopaedic Non-Trauma Procedures",
    'HN5', "Shoulder Procedures for Non-Trauma", "Shoulder",            "Subchapter HN – Orthopaedic Non-Trauma Procedures", 
    'HN6', "Elbow Procedures for Non-Trauma",    "Elbow",               "Subchapter HN – Orthopaedic Non-Trauma Procedures"
  )

#hrg_lookup_full_22_23 <- 
#  read_csv("HRG_22_23_lookup.csv") %>% 
#  clean_names()


inpatient_grouped <-
  inpatient_data %>% 
  group_by(der_activity_month, age_range, sex, ethnic_group, imd_decile,
           der_provider_site_code, duration_elective_wait_range, hrg_3, provider_type_1, stp21nm, lad21nm) %>% 
  summarise(n_spells = sum(spell_count),
            n_individuals = sum(individual_count),
            sum_cost = sum(sum_costs)
            ) %>% 
  ungroup() %>% 
  mutate(speciality = case_when(str_sub(hrg_3,1,1) == "H" ~ "Orthopaedic",
                                str_sub(hrg_3,1,1) == "B" ~ "Ophthalmology")) %>% 
  drop_na(speciality) %>% 
  mutate(type = case_when(provider_type_1 %in% c("Acute", 
                                                 "Health & Care",
                                                 "Mental Health") ~ "NHS",
                          provider_type_1 %in% c("Independent Sector",
                                                 "Non NHS")~ "Independent Sector")) %>% 
  drop_na(type) %>% 
  mutate(der_activity_month = 
           as.Date(
             paste0(str_sub(der_activity_month,1,4),
                    "-",
                    str_sub(der_activity_month,5,6),
                    "-01"
                    )
             )
         ) %>% 
  left_join(hrg_lookup, by = "hrg_3")

## Check exclusions/deletions
#inpatient_data %>% 
#  group_by(provider_type_1) %>% 
#  summarise(spell_count = sum(spell_count)) %>% 
#  arrange(desc(spell_count)) %>% 
#  mutate(prop = spell_count/sum(spell_count)*100)
#
#inpatient_data %>% 
#  mutate(speciality = case_when(str_sub(hrg_3,1,1) == "H" ~ "Orthopaedic",
#                              str_sub(hrg_3,1,1) == "B" ~ "Ophthalmology")) %>% 
#  group_by(speciality) %>% 
#  summarise(spell_count = sum(spell_count)) %>% 
#  arrange(desc(spell_count)) %>% 
#  mutate(prop = spell_count/sum(spell_count)*100)

outpatient_grouped <-
  outpatient_data %>% 
  group_by(der_activity_month, age_range, sex, ethnic_group, imd_decile,
           der_provider_site_code, appointment_wait_range, hrg_3, provider_type_1, stp21nm, lad21nm) %>% 
  summarise(n_spells = sum(attendence_count),
            n_individuals = sum(individual_count),
            sum_cost = sum(sum_costs)
            ) %>% 
  ungroup() %>% 
  mutate(speciality = case_when(str_sub(hrg_3,1,1) == "H" ~ "Orthopaedic",
                                str_sub(hrg_3,1,1) == "B" ~ "Ophthalmology")) %>% 
  drop_na(speciality) %>% 
  mutate(type = case_when(provider_type_1 %in% c("Acute", 
                                                 "Health & Care",
                                                 "Mental Health") ~ "NHS",
                          provider_type_1 %in% c("Independent Sector",
                                                 "Non NHS")~ "Independent Sector")) %>% 
  drop_na(type) %>% 
  mutate(der_activity_month = 
           as.Date(
             paste0(str_sub(der_activity_month,1,4),
                    "-",
                    str_sub(der_activity_month,5,6),
                    "-01")
             )
         ) %>% 
  left_join(hrg_lookup, by = "hrg_3")

# Write national data file ----
national_data_IP <-
  inpatient_grouped %>% 
  group_by(der_activity_month, age_range, sex, ethnic_group, imd_decile,
           duration_elective_wait_range, procedure_desc_short, type, speciality
           ) %>% 
  summarise(n_spells_IP = sum(n_spells),
            cost_IP = sum(sum_cost)) %>% 
  ungroup()

national_data_OP <-
  outpatient_grouped %>% 
  group_by(der_activity_month, age_range, sex, ethnic_group, imd_decile,
           appointment_wait_range, procedure_desc_short, type, speciality
           ) %>% 
  summarise(n_spells_OP = sum(n_spells),
            cost_OP = sum(sum_cost)) %>% 
  ungroup()

national_data <-
  national_data_IP %>%  # 1,973,563
  full_join(national_data_OP, 
            by = c("der_activity_month",
                   "age_range",
                   "sex",
                   "ethnic_group",
                   "imd_decile",
                   "duration_elective_wait_range" = "appointment_wait_range",
                   "procedure_desc_short",
                   "type",
                   "speciality"
                   ))

rm(national_data_IP,
   national_data_OP)

write_csv(national_data, "national_data.csv")


# Write local file (STP? ICB?) ----
stp_data_IP <-
  inpatient_grouped %>% 
  #head(1000) %>% 
  # Clean and reduce data variables were possible
  mutate(ethnic_group = case_when(ethnic_group %in% c("NULL", "99") ~ "Z", TRUE ~ ethnic_group)) %>% 
  mutate(ethnic_group = str_sub(ethnic_group, 1,1)) %>% 
  left_join(ethnicity_lookup %>% select(Code, ethnicity_broad), by = c("ethnic_group" = "Code")) %>% 
  mutate(imd_quintile = 
           case_when(imd_decile %in% c(1,2) ~ 1,
                     imd_decile %in% c(3,4) ~ 2,
                     imd_decile %in% c(5,6) ~ 3,
                     imd_decile %in% c(7,8) ~ 4,
                     imd_decile %in% c(9,10) ~ 5)) %>% 
  mutate(duration_elective_wait_range = 
           factor(duration_elective_wait_range, 
                  levels = 
                    c("0-50","50-100","100-150","150-200","200-250","250-300", 
                      "300-350","350-400","400-450","450-500","500+"))) %>% 
  # Group and sum
  group_by(der_activity_month, 
         stp21nm, 
         age_range, sex, ethnicity_broad, imd_quintile,
         duration_elective_wait_range, procedure_desc_short, type, speciality
         ) %>% 
  summarise(n_spells_IP = sum(n_spells),
            cost_IP = sum(sum_cost)) %>% 
  ungroup()

stp_data_OP <-
  outpatient_grouped %>% 
  #head(1000) %>% 
  # Clean and reduce data variables were possible
  mutate(ethnic_group = case_when(ethnic_group %in% c("NULL", "99") ~ "Z", TRUE ~ ethnic_group)) %>% 
  mutate(ethnic_group = str_sub(ethnic_group, 1,1)) %>% 
  left_join(ethnicity_lookup %>% select(Code, ethnicity_broad), by = c("ethnic_group" = "Code")) %>% 
  mutate(imd_quintile = 
           case_when(imd_decile %in% c(1,2) ~ 1,
                     imd_decile %in% c(3,4) ~ 2,
                     imd_decile %in% c(5,6) ~ 3,
                     imd_decile %in% c(7,8) ~ 4,
                     imd_decile %in% c(9,10) ~ 5)) %>% 
  mutate(appointment_wait_range = 
           factor(appointment_wait_range, 
                  levels = 
                    c("0-50","50-100","100-150","150-200","200-250","250-300", 
                      "300-350","350-400","400-450","450-500","500+"))) %>% 
  # Group and sum
  group_by(der_activity_month, 
           stp21nm, 
           age_range, sex, ethnicity_broad, imd_quintile,
           appointment_wait_range, procedure_desc_short, type, speciality
           ) %>% 
  
  summarise(n_spells_OP = sum(n_spells),
            cost_OP = sum(sum_cost)) %>% 
  ungroup()

stp_data <-
  stp_data_IP %>%  # 3,874,326
  full_join(stp_data_OP, #5,184,420
            by = c("der_activity_month",
                   "stp21nm",
                   "age_range",
                   "sex",
                   "ethnicity_broad",
                   "imd_quintile",
                   "duration_elective_wait_range" = "appointment_wait_range",
                   "procedure_desc_short",
                   "type",
                   "speciality"
                   )) 


rm(national_data_IP,
   national_data_OP)

write_csv(stp_data, "stp_data.csv")



