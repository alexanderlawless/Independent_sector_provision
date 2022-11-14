# Independent sector provision of elective surgery

library(tidyverse)
library(janitor)
library(scales)

# Set SU theme ####
SU_colours <- c (
  `orange`                     = grDevices::rgb(248,191,7, maxColorValue = 255),# "#f9bf07",
  `charcoal`                   = grDevices::rgb(44,40,37, maxColorValue = 255),# "#2c2825",
  `slate`                      = grDevices::rgb(104,111,115, maxColorValue = 255), # "#686f73",
  `blue`                       = grDevices::rgb(88,29,193, maxColorValue = 255), # "#5881c1",
  `red`                        = grDevices::rgb(236,101,85, maxColorValue = 255), # "#ec6555",
  #additional accent colours from word doc template
  `yellow`                     = grDevices::rgb(252,229,155, maxColorValue = 255),
  `grey`                       = grDevices::rgb(163,168,172, maxColorValue = 255),
  `white`                      = grDevices::rgb(255,255,255, maxColorValue = 255),
  #light and dark ends from colour theme in word doc
  `light orange`               = grDevices::rgb(253,242,205, maxColorValue = 255),
  `dark orange`                = grDevices::rgb(124,95,3, maxColorValue = 255),
  `light charcoal`             = grDevices::rgb(235,233,231, maxColorValue = 255),
  `dark charcoal`              = 	"#000000",#black
  `light slate`                = grDevices::rgb(224,226,227, maxColorValue = 255),
  `dark slate`                 = grDevices::rgb(51,55,57, maxColorValue = 255),
  `light blue`                 = grDevices::rgb(221,229,242, maxColorValue = 255),
  `dark blue`                  = grDevices::rgb(38,61,102, maxColorValue = 255),
  `light red`                  = grDevices::rgb(251,224,220, maxColorValue = 255),
  `dark red`                   = grDevices::rgb(144,29,16, maxColorValue = 255),
  `light yellow`               = grDevices::rgb(254,249,235, maxColorValue = 255),
  `dark yellow`                = grDevices::rgb(197,152,5, maxColorValue = 255),
  `light grey`                 = grDevices::rgb(236,237,238, maxColorValue = 255),
  `dark grey`                  = grDevices::rgb(79,84,88, maxColorValue = 255),
  `light white`                = grDevices::rgb(242,242,242, maxColorValue = 255),
  `dark white`                 = grDevices::rgb(127,127,127, maxColorValue = 255),
  `red2`                       = grDevices::rgb(215,25,28, maxColorValue = 255),
  `orange2`                    = grDevices::rgb(253,174,97, maxColorValue = 255),
  `yellow2`                    = grDevices::rgb(255,255,191, maxColorValue = 255),
  `green2`                     = grDevices::rgb(171,221,164, maxColorValue = 255),
  `blue2`                      = grDevices::rgb(43,131,186, maxColorValue = 255) #"#2b83ba"
)

SU_cols <- function(...) {
  cols <- c(...)
  
  if (is.null(cols))
    return (SU_colours)
  
  SU_colours[cols]
}

SU_palettes <- list(
  `main` = SU_cols("orange","charcoal","slate","blue","red"),
  `oranges` = SU_cols("light orange","orange","dark orange"),
  `slates` = SU_cols("light slate","slate","dark slate"),
  `mixed` = SU_cols("dark red","orange","yellow","light blue","slate"),
  `oj_coal` = SU_cols("yellow","orange","red","dark red","dark charcoal"),
  `oj_red` = SU_cols("yellow","orange","red","dark red"),
  `white_oj_coal` = SU_cols("white","yellow","orange","red","dark red","dark charcoal"),#added since shared
  `lyellow_oj_coal` = SU_cols("light yellow","orange","red","dark red","dark charcoal"),#added since shared
  `wy_oj_coal` = SU_cols("white","light yellow","yellow","orange","red","dark red","charcoal","dark charcoal"),
  `red_coal` = SU_cols("red","dark red","charcoal","dark charcoal"),
  `blue_yellow_red` = SU_cols("red2","orange2","yellow2","green2","blue2"),
  `red_yellow_blue` = SU_cols("blue2","green2","yellow2","orange2","red2")
)


SU_pal <- function(palette = "main", reverse = FALSE, ...) {
  pal <- SU_palettes[[palette]]
  
  if (reverse) pal <- rev(pal)
  
  colorRampPalette(pal, ...)
}


scale_color_SU <- function(palette = "main", discrete = TRUE, reverse = FALSE, ...) {
  pal <- SU_pal(palette = palette, reverse = reverse)
  
  if (discrete) {
    discrete_scale("colour", paste0("SU_", palette), palette = pal, ...)
  } else {
    scale_color_gradientn(colours = pal(256), ...)
  }
}

scale_fill_SU <- function(palette = "main", discrete = TRUE, reverse = FALSE, ...) {
  pal <- SU_pal(palette = palette, reverse = reverse)
  
  if (discrete) {
    discrete_scale("fill", paste0("SU_", palette), palette = pal, ...)
  } else {
    scale_fill_gradientn(colours = pal(256), ...)
  }
}  

theme_SU <-   function (base_size){
  theme_minimal(
    #base_family = "Segoe UI", 
    base_size=12
  ) %+replace% 
    theme(axis.title = element_text(size=11, face="bold",colour=SU_cols("charcoal")),
          plot.title = element_text(hjust=0,face="bold",size=12,colour=SU_cols("charcoal"),margin=margin(b=4,unit="pt")),
          plot.subtitle = element_text(hjust=0,face="italic",size=10,colour=SU_cols("charcoal"),margin=margin(b=4,unit="pt")),
          plot.caption = element_text(hjust = 0,face="italic",size=9,colour=SU_cols("slate"),margin=margin(b=4,unit="pt")),
          legend.text = element_text(size=10,colour=SU_cols("charcoal")),
          legend.title = element_text(face="bold",size=11,colour=SU_cols("charcoal"),margin=margin(b=4,unit="pt")))
}

theme_set(theme_SU())

# Import data ----
lsoa11_stp21 <- 
  read_csv("LSOA(2011)_CCG(21)_STP(21).csv") %>% 
  clean_names()

inpatient_data <- 
  read_csv("grouped_data_IP.csv") %>% 
  clean_names() %>% 
  left_join(lsoa11_stp21 %>% 
              select(lsoa11cd,stp21nm, lad21nm), 
            by = c("der_postcode_lsoa_2011_code" = "lsoa11cd")) 

outpatient_data <- 
  read_csv("grouped_data_OP.csv") %>% 
  clean_names() %>% 
  left_join(lsoa11_stp21 %>% 
              select(lsoa11cd,stp21nm, lad21nm), 
            by = c("der_postcode_lsoa_2011_code" = "lsoa11cd")) %>% 
  rename(ethnic_group = ethnic_category)


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
            n_individuals = sum(individual_count)
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
            n_individuals = sum(individual_count)
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
  
# National trends ----
national_data_IP <-
  inpatient_grouped %>% 
  group_by(der_activity_month, age_range, sex, ethnic_group, imd_decile,
           duration_elective_wait_range, procedure_desc_short, type, speciality
           ) %>% 
  summarise(n_spells_IP = sum(n_spells)) %>% 
  ungroup()

national_data_OP <-
  outpatient_grouped %>% 
  group_by(der_activity_month, age_range, sex, ethnic_group, imd_decile,
           appointment_wait_range, procedure_desc_short, type, speciality
           ) %>% 
  summarise(n_spells_OP = sum(n_spells)) %>% 
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

# Time series by speciality 
national_data %>% 
  group_by(der_activity_month, speciality) %>% 
  summarise(n_spells_IP = sum(n_spells_IP, na.rm = TRUE),
            n_spells_OP = sum(n_spells_OP, na.rm = TRUE)) %>%
  group_by(der_activity_month, speciality) %>% 
  mutate(all_activity = sum(n_spells_IP, n_spells_OP)) %>% 
  filter(der_activity_month > "2018-01-01" &
           der_activity_month < "2022-07-01") %>%
  
  ggplot(aes(x = der_activity_month, y = all_activity)) +
  geom_point(alpha = 0.4) +
  geom_smooth(method = "loess", span = 0.2, colour = "#5881c1") +
  facet_grid(~speciality, scales = "free") +
  scale_color_SU() +
  scale_y_continuous(labels = comma, oob = squish) +
  theme(strip.background = element_rect(fill = NA, colour = "grey"),
        strip.text = element_text(face = "bold")
        ) +
  labs(x = "Month", y = "Admissions",
       title = "Trends in activity by speciality",
       subtitle = "Monthly elective inpatient and outpatient activity | National | 2017-22")


# Time series by speciality and sector
national_data %>% 
  group_by(der_activity_month, type, speciality) %>% 
  summarise(n_spells_IP = sum(n_spells_IP, na.rm = TRUE),
            n_spells_OP = sum(n_spells_OP, na.rm = TRUE)) %>%
  group_by(der_activity_month, type, speciality) %>% 
  mutate(all_activity = sum(n_spells_IP, n_spells_OP)) %>% 
  filter(der_activity_month > "2018-01-01" &
           der_activity_month < "2022-07-01") %>%
  
  ggplot(aes(x = der_activity_month, y = all_activity)) +
  geom_point(alpha = 0.4) +
  geom_smooth(method = "loess", span = 0.2, colour = "#5881c1") +
  facet_grid(type~speciality#, scales = "free"
             ) +
  scale_color_SU() +
  scale_y_continuous(labels = comma, oob = squish) +
  theme(strip.background = element_rect(fill = NA, colour = "grey"),
        strip.text = element_text(face = "bold")
        ) +
  labs(x = "Month", y = "Admissions",
       title = "Trends in activity by speciality and sector",
       subtitle = "Monthly elective inpatient and outpatient activity | National | 2017-22")

# Activity by speciality, sector and setting
national_data %>% 
  group_by(der_activity_month, type, speciality) %>% 
  summarise(n_spells_IP = sum(n_spells_IP, na.rm = TRUE),
            n_spells_OP = sum(n_spells_OP, na.rm = TRUE)) %>% 
  pivot_longer(cols = c(-der_activity_month, -type, -speciality)) %>% 
  mutate(name = case_when(str_detect(name, "IP") ~ "Inpatient", TRUE ~ "Outpatient")) %>% 
  filter(der_activity_month > "2018-01-01" &
           der_activity_month < "2022-07-01") %>%
  
  ggplot(aes(x = der_activity_month, y = value, colour = name, group = name)) +
  geom_point(alpha = 0.4) +
  geom_smooth(method = "loess", span = 0.2) +
  facet_grid(type~speciality, scales = "free") +
  scale_color_SU() +
  scale_y_continuous(labels = comma, oob = squish) +
  theme(strip.background = element_rect(fill = NA, colour = "grey"),
        strip.text = element_text(face = "bold"),
        legend.position = "bottom"
        ) +
  labs(x = "Month", y = "Admissions", colour = "Activity",
       title = "Trends in admissions by speciality and sector",
       subtitle = "Monthly elective inpatient admissions | National | 2017-22")

# National trends by procedure
national_data_procedure <-
  national_data %>% 
  group_by(der_activity_month, type, speciality, procedure_desc_short) %>% 
  summarise(n_spells_IP = sum(n_spells_IP, na.rm = TRUE),
            n_spells_OP = sum(n_spells_OP, na.rm = TRUE)) %>% 
  drop_na(procedure_desc_short)

# Orthopaedic trends
national_data_procedure %>%  
  filter(speciality == "Orthopaedic") %>% 
  group_by(der_activity_month, type, speciality, procedure_desc_short) %>% 
  mutate(all_activity = sum(n_spells_IP, n_spells_OP)) %>% 
  filter(der_activity_month > "2018-01-01" &
           der_activity_month < "2022-07-01") %>% 
  
  ggplot(aes(x = der_activity_month, y = all_activity, colour = type)) +
  geom_smooth(method = "loess", span = 0.2) +
  facet_wrap(~procedure_desc_short#, scales = "free"
             ) +
  scale_color_SU() +
  scale_y_continuous(labels = comma) +
  theme(strip.background = element_rect(fill = NA, colour = "grey"),
        strip.text = element_text(face = "bold"),
        legend.position = "bottom"
        ) +
  labs(x = "Month", y = "Admissions", colour = "Sector",
       title = "Trends in Orthopaedic activity by sector and procedure",
       subtitle = "Monthly elective inpatient admissions and outpatient appointments | National | 2017-22")

# Ophthalmology trends
national_data_procedure %>%  
  filter(speciality == "Ophthalmology") %>% 
  group_by(der_activity_month, type, speciality, procedure_desc_short) %>% 
  mutate(all_activity = sum(n_spells_IP, n_spells_OP)) %>% 
  filter(der_activity_month > "2018-01-01" &
           der_activity_month < "2022-07-01") %>% 
  
  ggplot(aes(x = der_activity_month, y = all_activity, colour = type)) +
  geom_smooth(method = "loess", span = 0.2) +
  facet_wrap(~procedure_desc_short, scales = "free"
             ) +
  scale_color_SU() +
  scale_y_continuous(labels = comma) +
  theme(strip.background = element_rect(fill = NA, colour = "grey"),
        strip.text = element_text(face = "bold"),
        legend.position = c(0.7,0.1)
        ) +
  labs(x = "Month", y = "Activity", colour = "Sector",
       title = "Trends in Ophthalmology activity by sector and procedure",
       subtitle = "Monthly elective inpatient admissions and outpatient appointments | National | 2017-22")

# Cataract and Vitreous Retinal procedures
national_data_procedure %>%  
  filter(speciality == "Ophthalmology", 
         procedure_desc_short %in% c("Cataract", 
                                     "Vitreous Retinal")) %>% 
  group_by(der_activity_month, type, speciality, procedure_desc_short) %>% 
  mutate(all_activity = sum(n_spells_IP, n_spells_OP)) %>% 
  filter(der_activity_month > "2018-01-01" &
           der_activity_month < "2022-07-01") %>% 
  
  ggplot(aes(x = der_activity_month, y = all_activity, colour = type)) +
  geom_smooth(method = "loess", span = 0.2) +
  facet_wrap(~procedure_desc_short, scales = "free"
             ) +
  scale_color_SU() +
  scale_y_continuous(labels = comma) +
  theme(strip.background = element_rect(fill = NA, colour = "grey"),
        strip.text = element_text(face = "bold"),
        #legend.position = c(0.7,0.1)
        ) +
  labs(x = "Month", y = "Activity", colour = "Sector",
       title = "Trends in Ophthalmology activity by sector and procedure",
       subtitle = "Monthly elective inpatient admissions and outpatient appointments | National | 2017-22")


# Ratio of IS:NHS provision
national_data %>%
  #head(10000) %>% 
  group_by(der_activity_month, type, speciality) %>% 
  summarise(n_spells_IP = sum(n_spells_IP, na.rm = TRUE),
            n_spells_OP = sum(n_spells_OP, na.rm = TRUE)) %>% 
  group_by(der_activity_month, type, speciality) %>% 
  mutate(total_activity = sum(n_spells_IP, n_spells_OP)) %>% 
  pivot_longer(cols = c(-der_activity_month, -type, -speciality)) %>% 
  pivot_wider(id_cols = c(der_activity_month, speciality, name), 
              names_from = type,
              values_from = value
              ) %>% 
  mutate(ratio = `Independent Sector`/ `NHS`) %>% 
  mutate(ratio = case_when(is.nan(ratio) ~ 0, TRUE ~ ratio)) %>% 
  mutate(name = case_when(str_detect(name, "IP") ~ "Inpatient", 
                          str_detect(name, "OP") ~ "Outpatient",
                          TRUE ~ "All activity")) %>% 
  filter(der_activity_month > "2018-01-01" &
           der_activity_month < "2022-07-01") %>% 
  
  ggplot(aes(x = der_activity_month, y = ratio, colour = name, group = name)) +
  geom_point(alpha = 0.4) +
  geom_smooth(method = "loess", span = 0.2) +
  facet_grid(~speciality, scales = "free") +
  scale_color_SU() +
  scale_y_continuous(labels = comma, oob = squish) +
  theme(strip.background = element_rect(fill = NA, colour = "grey"),
        strip.text = element_text(face = "bold"),
        legend.position = "bottom"
        ) +
  labs(x = "Month", y = "IS:NHS Ratio", colour = "Activity type", 
       title = "Independent sector to NHS provsion ratio",
       subtitle = "Monthly elective inpatient and outpatient activity | National | 2017-22")


# IS market share (proportion)
national_data %>%
  #head(10000) %>% 
  group_by(der_activity_month, type, speciality) %>% 
  summarise(n_spells_IP = sum(n_spells_IP, na.rm = TRUE),
            n_spells_OP = sum(n_spells_OP, na.rm = TRUE)) %>% 
  group_by(der_activity_month, type, speciality) %>% 
  mutate(total_activity = sum(n_spells_IP, n_spells_OP)) %>% 
  pivot_longer(cols = c(-der_activity_month, -type, -speciality)) %>% 
  pivot_wider(id_cols = c(der_activity_month, speciality, name), 
              names_from = type,
              values_from = value
              ) %>% 
  mutate(prop = `Independent Sector`/ sum(`Independent Sector`, `NHS`) * 100) %>% 
  mutate(name = case_when(str_detect(name, "IP") ~ "Inpatient", 
                          str_detect(name, "OP") ~ "Outpatient",
                          TRUE ~ "All activity")) %>% 
  filter(der_activity_month > "2018-01-01" &
           der_activity_month < "2022-07-01") %>% 
  
  ggplot(aes(x = der_activity_month, y = prop, colour = name, group = name)) +
  geom_point(alpha = 0.4) +
  geom_smooth(method = "loess", span = 0.2) +
  facet_wrap(~speciality#, scales = "free_y"
             ) +
  scale_color_SU() +
  scale_y_continuous(labels = comma, oob = squish) +
  theme(strip.background = element_rect(fill = NA, colour = "grey"),
        strip.text = element_text(face = "bold"),
        legend.position = "bottom"
        ) +
  labs(x = "Month", y = "Independent sector market share (%)", colour = "Activity type", 
       title = "Share of activity delivered by the independent sector",
       subtitle = "Monthly elective inpatient and outpatient activity | National | 2017-22")
    




#
national_data %>% 
  filter(speciality == "Ophthalmology") %>% 
  mutate(year = lubridate::year(der_activity_month)) %>% 
  drop_na(procedure_desc_short) %>% 
  group_by(year, procedure_desc_short) %>% 
  summarise(n_spells_IP = sum(n_spells_IP, na.rm = TRUE),
            n_spells_OP = sum(n_spells_OP, na.rm = TRUE)
            )
    



# Notes ----

# PHIN data 
# https://www.phin.org.uk/data 
# Markdown, Seb HF -  https://rpubs.com/sgpeytrignet/958780 

# E-referrals 
# Optomotrists directly referring to Independent sector 
# https://digital.nhs.uk/dashboards/ers-open-data 



  

  
            
            
            
            