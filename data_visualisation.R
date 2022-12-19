# Independent sector provision of elective surgery

library(tidyverse)
library(janitor)
library(scales)
library(patchwork)

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

# Read in data ----

national_data <- 
  read_csv("national_data.csv") %>% 
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
                    "300-350","350-400","400-450","450-500","500+")))
  
national_data_ortho <- 
  national_data %>% 
  filter(speciality == "Orthopaedic")

national_data_ophthal <- 
  national_data %>% 
  filter(speciality == "Ophthalmology")


# Function(s)
sum_spells_function <- function(data) {
  
  data %>%  
    summarise(n_spells_IP = sum(n_spells_IP, na.rm = TRUE),
              n_spells_OP = sum(n_spells_OP, na.rm = TRUE),
              cost = sum(cost_IP, cost_OP, na.rm = TRUE)
              )
  
}


## Visualise national data ----
# Time series by speciality 
national_data %>% 
  group_by(der_activity_month, speciality) %>% 
  sum_spells_function(.) %>% 
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

# + cost trend
national_data %>% 
  group_by(der_activity_month, speciality) %>% 
  sum_spells_function(.) %>% 
  group_by(der_activity_month, speciality) %>% 
  mutate(all_activity = sum(n_spells_IP, n_spells_OP)) %>% 
  filter(der_activity_month > "2018-01-01" &
           der_activity_month < "2022-11-01") %>%
  select(-contains("n")) %>% 
  pivot_longer(c("cost", "all_activity")) %>% 
  
  ggplot(aes(x = der_activity_month, y = value)) +
  geom_point(alpha = 0.4) +
  geom_smooth(method = "loess", span = 0.2, colour = "#5881c1") +
  facet_grid(name~speciality, scales = "free") +
  scale_color_SU() +
  scale_y_continuous(labels = comma, oob = squish) +
  theme(strip.background = element_rect(fill = NA, colour = "grey"),
        strip.text = element_text(face = "bold")
        ) +
  labs(x = "Month", y = "",
       title = "Trends in activity by speciality",
       subtitle = "Monthly elective inpatient and outpatient activity | National | 2017-22")


# Time series by speciality and sector
national_data %>% 
  group_by(der_activity_month, type, speciality) %>% 
  sum_spells_function(.) %>% 
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
  sum_spells_function(.) %>% 
  pivot_longer(cols = c(-der_activity_month, -type, -speciality)) %>% 
  mutate(name = case_when(str_detect(name, "IP") ~ "Inpatient", TRUE ~ "Outpatient")) %>% 
  filter(der_activity_month > "2018-01-01" &
           der_activity_month < "2022-11-01") %>%
  
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
       title = "Trends in activity by speciality and sector",
       subtitle = "Monthly elective activity | National | 2017-22")


# + Costs 
# Ophthalmology
national_data %>% 
  group_by(der_activity_month, type, speciality) %>% 
  sum_spells_function(.) %>% 
  pivot_longer(cols = c(-der_activity_month, -type, -speciality)) %>% 
  mutate(name = case_when(str_detect(name, "IP") ~ "Inpatient admissions", 
                          str_detect(name, "OP") ~ "Outpatient appointments",
                          TRUE ~ "Cost (£)")) %>% 
  filter(der_activity_month > "2018-01-01" &
           der_activity_month < "2022-11-01") %>% 
  filter(speciality == "Ophthalmology") %>% 
  
  ggplot(aes(x = der_activity_month, y = value, colour = name, group = name)) +
  geom_point(alpha = 0.4) +
  geom_smooth(method = "loess", span = 0.2) +
  facet_grid(str_wrap(name,15) ~ type, scales = "free") +
  scale_color_SU() +
  scale_y_continuous(labels = comma, oob = squish) +
  theme(strip.background = element_rect(fill = NA, colour = "grey"),
        strip.text = element_text(face = "bold"),
        legend.position = "none"
        ) +
  labs(x = "Month", y = "", colour = "Activity",
       title = "Trends in activity and cost by speciality and sector: Ophthalmology",
       subtitle = "Monthly elective activity | National | 2018-22")


# Orthopaedic
national_data %>% 
  group_by(der_activity_month, type, speciality) %>% 
  sum_spells_function(.) %>%  
  pivot_longer(cols = c(-der_activity_month, -type, -speciality)) %>% 
  mutate(name = case_when(str_detect(name, "IP") ~ "Inpatient admissions", 
                          str_detect(name, "OP") ~ "Outpatient appointments",
                          TRUE ~ "Cost (£)")) %>% 
  filter(der_activity_month > "2018-01-01" &
           der_activity_month < "2022-11-01") %>% 
  filter(speciality == "Orthopaedic") %>% 
  
  ggplot(aes(x = der_activity_month, y = value, colour = name, group = name)) +
  geom_point(alpha = 0.4) +
  geom_smooth(method = "loess", span = 0.2) +
  facet_grid(str_wrap(name,15) ~ type, scales = "free") +
  scale_color_SU() +
  scale_y_continuous(labels = comma, oob = squish) +
  theme(strip.background = element_rect(fill = NA, colour = "grey"),
        strip.text = element_text(face = "bold"),
        legend.position = "none"
        ) +
  labs(x = "Month", y = "", colour = "Activity",
       title = "Trends in activity and cost by speciality and sector: Orthopaedic",
       subtitle = "Monthly elective activity | National | 2018-22")

# National trends by procedure
national_data_procedure <-
  national_data %>% 
  group_by(der_activity_month, type, speciality, procedure_desc_short) %>% 
  sum_spells_function(.) %>% 
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
        legend.position = "bottom"
        ) +
  labs(x = "Month", y = "Activity", colour = "Sector",
       title = "Trends in Ophthalmology activity by sector and procedure",
       subtitle = "Monthly elective inpatient admissions and outpatient appointments | National | 2017-22")


# Ratio of IS:NHS provision
national_data %>%
  group_by(der_activity_month, type, speciality) %>% 
  sum_spells_function(.) %>% 
  group_by(der_activity_month, type, speciality) %>% 
  mutate(total_activity = sum(n_spells_IP, n_spells_OP)) %>% 
  pivot_longer(cols = c(-der_activity_month, -type, -speciality)) %>% 
  pivot_wider(id_cols = c(der_activity_month, speciality, name), 
              names_from = type,
              values_from = value
              ) %>% 
  group_by(der_activity_month, speciality, name) %>% 
  mutate(ratio = `Independent Sector`/ `NHS`) %>% 
  mutate(ratio = case_when(is.nan(ratio) ~ 0, TRUE ~ ratio)) %>% 
  mutate(name = case_when(str_detect(name, "IP") ~ "Inpatient", 
                          str_detect(name, "OP") ~ "Outpatient",
                          str_detect(name, "cost") ~ "Cost",
                          TRUE ~ "All activity")) %>% 
  filter(der_activity_month > "2018-01-01" &
           der_activity_month < "2022-11-01") %>% 
  filter(name != "Cost") %>% 

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
  group_by(der_activity_month, type, speciality) %>% 
  sum_spells_function(.) %>% 
  group_by(der_activity_month, type, speciality) %>% 
  mutate(total_activity = sum(n_spells_IP, n_spells_OP)) %>% 
  pivot_longer(cols = c(-der_activity_month, -type, -speciality)) %>% 
  pivot_wider(id_cols = c(der_activity_month, speciality, name), 
              names_from = type,
              values_from = value
              ) %>% 
  group_by(der_activity_month, speciality, name) %>% 
  mutate(prop = `Independent Sector`/ sum(`Independent Sector`, `NHS`) * 100) %>% 
  mutate(name = case_when(str_detect(name, "IP") ~ "Inpatient", 
                          str_detect(name, "OP") ~ "Outpatient",
                          str_detect(name, "cost") ~ "Cost",
                          TRUE ~ "All activity")) %>% 
  filter(der_activity_month > "2018-01-01" &
           der_activity_month < "2022-11-01") %>%
  filter(name %in% c("Inpatient", "Outpatient")) %>% 
  
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


# IS proportion by speciality and activity type (IP/OP)
# Ophthalmology
national_data_ophthal %>% 
  group_by(der_activity_month, type, procedure_desc_short) %>% 
  summarise(n_spells_IP = sum(n_spells_IP, na.rm = TRUE),
            n_spells_OP = sum(n_spells_OP, na.rm = TRUE),
            #all_activity = sum(n_spells_IP, n_spells_OP, na.rm = TRUE)
            ) %>% 
  pivot_longer(cols = c(n_spells_IP, n_spells_OP)) %>% 
  pivot_wider(id_cols = c(der_activity_month,  procedure_desc_short,name), 
              names_from = type,
              values_from = value
              ) %>% 
  group_by(der_activity_month, procedure_desc_short, name) %>% 
  mutate(IS_prop = `Independent Sector`/ sum(`Independent Sector`, `NHS`, na.rm = TRUE) * 100) %>% 
  mutate(name = case_when(str_detect(name, "IP") ~ "Inpatient admissions", TRUE ~ "Outpatient appointments")) %>% 
  filter(der_activity_month > "2018-01-01" & der_activity_month < "2022-11-01") %>% 
  drop_na(procedure_desc_short) %>% 
  
  ggplot(aes(x = der_activity_month, y = IS_prop, colour = name)) +
  geom_smooth(method = "loess", span = 0.2) +
  facet_wrap(~procedure_desc_short#, scales = "free"
             ) +
  theme(strip.background = element_rect(fill = NA, colour = "grey"),
        strip.text = element_text(face = "bold"),
        legend.position = c(0.75,0.2)) +
  scale_color_SU() +
  labs(x = "Month", y = "Independent sector market share (%)", colour = "Activity type", 
       title = "Share of activity delivered by the independent sector: Ophthalmology",
       subtitle = "Monthly elective inpatient and outpatient activity | National | 2017-22")
 
# Orthopaedics 
national_data_ortho %>% 
  group_by(der_activity_month, type, procedure_desc_short) %>% 
  summarise(n_spells_IP = sum(n_spells_IP, na.rm = TRUE),
            n_spells_OP = sum(n_spells_OP, na.rm = TRUE)) %>% 
  pivot_longer(cols = c(n_spells_IP, n_spells_OP)) %>% 
  pivot_wider(id_cols = c(der_activity_month,  procedure_desc_short,name), 
              names_from = type,
              values_from = value) %>% 
  group_by(der_activity_month, procedure_desc_short, name) %>% 
  mutate(IS_prop = `Independent Sector`/ sum(`Independent Sector`, `NHS`, na.rm = TRUE) * 100) %>% 
  mutate(name = case_when(str_detect(name, "IP") ~ "Inpatient admissions", TRUE ~ "Outpatient appointments")) %>% 
  filter(der_activity_month > "2018-01-01" & der_activity_month < "2022-11-01") %>% 
  drop_na(procedure_desc_short) %>% 
  
  ggplot(aes(x = der_activity_month, y = IS_prop, colour = name)) +
  geom_smooth(method = "loess", span = 0.2) +
  facet_wrap(~procedure_desc_short#, scales = "free"
  ) +
  theme(strip.background = element_rect(fill = NA, colour = "grey"),
        strip.text = element_text(face = "bold"),
        legend.position = "bottom") +
  scale_color_SU() +
  labs(x = "Month", y = "Independent sector market share (%)", colour = "Activity type", 
       title = "Share of activity delivered by the independent sector: Orthopaedic",
       subtitle = "Monthly elective inpatient and outpatient activity | National | 2017-22")



## National data by demographics ---- 

wrangle_function <- function(data, var_1) {
  
  data <- 
    data %>% 
    mutate(var_1 = {{var_1}})
  
  data %>%   
    group_by(der_activity_month, var_1, type) %>% 
    sum_spells_function(.) %>% 
    group_by(der_activity_month, type, var_1) %>% 
    mutate(total_activity = sum(n_spells_IP, n_spells_OP)) %>% 
    pivot_longer(cols = c(-der_activity_month, -type, -var_1)) %>% 
    pivot_wider(id_cols = c(der_activity_month, var_1, name), 
                names_from = type,
                values_from = value
                ) %>% 
    mutate(`Independent Sector` = case_when(is.na(`Independent Sector`) ~ 0, TRUE ~ `Independent Sector`)) %>% 
    group_by(der_activity_month, var_1, name) %>% 
    mutate(prop = `Independent Sector`/ sum(`Independent Sector`, `NHS`) * 100) %>% 
    mutate(name = case_when(str_detect(name, "IP") ~ "Inpatient admissions", 
                            str_detect(name, "OP") ~ "Outpatient appointments",
                            str_detect(name, "cost") ~ "Costs",
                            TRUE ~ "All activity")) %>% 
    filter(der_activity_month > "2018-01-01" &
             der_activity_month < "2022-11-01") %>%
    filter(name != "All activity") %>% 
    ungroup() %>% 
    filter(name != "Costs")
}

graph_function <- function(data, subtitle_speciality ) {
  
  data %>% 
    ggplot(aes(x = der_activity_month, y = prop, colour = name)) +
    geom_smooth(method = "loess", span = 0.3) +
    #facet_grid(imd_quintile~ethnicity_broad) +
    scale_color_SU() +
    scale_y_continuous(labels = comma, oob = squish) +
    theme(strip.background = element_rect(fill = NA, colour = "grey"),
          strip.text = element_text(face = "bold"), 
          legend.position = "bottom",
          axis.text.x = element_text(angle = 90)
          ) +
    labs(x = "Month", y = "Independent sector proportion (%)",
         colour = "",
         title = "Independent sector proportion of activity",
         subtitle = paste0(subtitle_speciality, " elective procedures | National | 2018-22")
    )
}

# Age range
age_range_order <-
  tribble(
    ~age_order, ~age_range,   
    1,  "0-9",    
    2,  "10-19",    
    3,  "20-29",    
    4,  "30-39",    
    5,  "40-49",    
    6,  "50-59",    
    7,  "60-69",    
    8,  "70-79",    
    9,  "80-89",   
    10, "90-99",    
    11, "100+"
    )

national_data_ortho %>% 
  wrangle_function(., age_range) %>% 
  filter(var_1 != "100+") %>% 
  graph_function(., "Orthopaedic") +
  facet_grid(~var_1, scales = "free")

national_data_ophthal %>% 
  wrangle_function(., age_range) %>% 
  filter(var_1 != "100+") %>% 
  graph_function(., "Ophthalmology") +
  facet_grid(~var_1, scales = "free")

# Ethnicity
ethnicity_lookup <- read_csv("ethnicity_lookup.csv")

national_data_ortho %>%
  mutate(ethnic_group = case_when(ethnic_group %in% c("NULL", "99") ~ "Z", TRUE ~ ethnic_group)) %>% 
  mutate(ethnic_group = str_sub(ethnic_group, 1,1)) %>% 
  left_join(ethnicity_lookup, by = c("ethnic_group" = "Code")) %>% 
  wrangle_function(., ethnicity_broad) %>% 
  filter(var_1 != "Not stated_broad") %>% 
  graph_function(., "Orthopaedic") +
  facet_grid(~var_1, scales = "free")

national_data_ophthal %>% 
  mutate(ethnic_group = case_when(ethnic_group %in% c("NULL", "99") ~ "Z", TRUE ~ ethnic_group)) %>% 
  mutate(ethnic_group = str_sub(ethnic_group, 1,1)) %>% 
  left_join(ethnicity_lookup, by = c("ethnic_group" = "Code")) %>% 
  wrangle_function(., ethnicity_broad) %>% 
  filter(var_1 != "Not stated_broad") %>% 
  graph_function(., "Ophthalmology") +
  facet_grid(~var_1, scales = "free")

# Deprivation
national_data_ortho %>%
  wrangle_function(., imd_quintile) %>% 
  mutate(var_1 = case_when(var_1 == 1 ~ "Deprivation quintile 1",
                           var_1 == 2 ~ "2",
                           var_1 == 3 ~ "3",
                           var_1 == 4 ~ "4",
                           var_1 == 5 ~ "Deprivation quintile 5"
                           )) %>% 
  mutate(var_1 = factor(var_1, 
                        levels = c("Deprivation quintile 1","2","3","4","Deprivation quintile 5")
                        )) %>% 
  graph_function(., "Orthopaedic") +
  facet_grid(~var_1, scales = "free")

national_data_ophthal %>% 
  wrangle_function(., imd_quintile) %>% 
  graph_function(., "Ophthalmology") +
  facet_grid(~var_1, scales = "free")

# Gender
national_data_ortho %>%
  wrangle_function(., sex) %>% 
  filter(var_1 %in% c(1,2)) %>% 
  graph_function(., "Orthopaedic") +
  facet_grid(~var_1, scales = "free")

national_data_ophthal %>% 
  wrangle_function(., sex) %>% 
  filter(var_1 %in% c(1,2)) %>% 
  graph_function(., "Ophthalmology") +
  facet_grid(~var_1, scales = "free")


# Duration of wait
national_data_ortho %>%
  wrangle_function(., duration_elective_wait_range) %>%
  graph_function(., "Orthopaedic") +
  facet_grid(~var_1, scales = "free") 

national_data_ophthal %>% 
  wrangle_function(., duration_elective_wait_range) %>% 
  graph_function(., "Ophthalmology") +
  facet_grid(~var_1, scales = "free") 


# Bar chart - spell volumes 
national_data_ortho %>% 
  wrangle_function(., duration_elective_wait_range) %>% 
  pivot_longer(cols = c(`Independent Sector`, NHS),
               names_to = "sector",
               values_to = "value"
                 ) %>% 
  mutate(year = lubridate::year(der_activity_month)) %>% 
  group_by(year, var_1, name, sector) %>% 
  summarise(value = sum(value)) %>% 
  
  ggplot(aes(x = var_1, y = value, fill = var_1)) +
  geom_col(position = "dodge", colour = "grey") +
  facet_grid(name~year) +
  scale_fill_SU() +
  scale_y_continuous(labels = comma) +
  theme(axis.text.x = element_text(angle = 90),
        legend.position = "none",
        strip.background = element_rect(fill = NA, colour = "grey"),
        strip.text = element_text(face = "bold")
        ) +
  labs(x = "Wait duration (days)", y = "Activity",
       title = "Change in wait time for treatment",
       subtitle = "Elective orthopaedic care | National | 2018-22"
       )

national_data_ophthal %>% 
  wrangle_function(., duration_elective_wait_range) %>% 
  pivot_longer(cols = c(`Independent Sector`, NHS),
               names_to = "sector",
               values_to = "value"
               ) %>% 
  mutate(year = lubridate::year(der_activity_month)) %>% 
  group_by(year, var_1, name, sector) %>% 
  summarise(value = sum(value)) %>%
  
  ggplot(aes(x = var_1, y = value, fill = var_1)) +
  geom_col(position = "dodge", colour = "grey") +
  facet_grid(name~year) +
  scale_fill_SU() +
  scale_y_continuous(labels = comma) +
  theme(axis.text.x = element_text(angle = 90),
        legend.position = "none",
        strip.background = element_rect(fill = NA, colour = "grey"),
        strip.text = element_text(face = "bold")
        ) +
  labs(x = "Wait duration (days)", y = "Activity",
       title = "Change in wait time for treatment",
       subtitle = "Elective ophthalmic care | National | 2018-22"
       )

  
# Stacked bar chart proportions
national_data_ortho %>% 
  wrangle_function(., duration_elective_wait_range) %>% 
  filter(var_1 != "500+") %>% 
  pivot_longer(cols = c(`Independent Sector`, NHS),
               names_to = "sector",
               values_to = "value"
               ) %>% 
  mutate(year = lubridate::year(der_activity_month)) %>% 
  group_by(year, var_1, name, sector) %>% 
  summarise(value = sum(value)) %>% 
  group_by(year, name, sector) %>% 
  mutate(prop = value/sum(value)*100) %>% 
  
  ggplot(aes(x = year, y = prop, fill = var_1)) +
  geom_col(position = "stack", colour = "grey", alpha = 0.9, width = 0.7) +
  facet_grid(name~sector) +
  scale_fill_SU() +
  theme(#legend.position = "none",
        strip.background = element_rect(fill = NA, colour = "grey"),
        strip.text = element_text(face = "bold")
        ) +
  labs(x = "Year", y = "Proportion (%)",
       fill = "Wait duration (days)",
       title = "Change in wait time for treatment",
       subtitle = "Proportion of elective orthopaedic care contacts by wait time | National | 2018-22"
       )

national_data_ophthal %>% 
  wrangle_function(., duration_elective_wait_range) %>% 
  filter(var_1 != "500+") %>% 
  pivot_longer(cols = c(`Independent Sector`, NHS),
               names_to = "sector",
               values_to = "value"
               ) %>% 
  mutate(year = lubridate::year(der_activity_month)) %>% 
  group_by(year, var_1, name, sector) %>% 
  summarise(value = sum(value)) %>% 
  group_by(year, name, sector) %>% 
  mutate(prop = value/sum(value)*100) %>% 
  
  ggplot(aes(x = year, y = prop, fill = var_1)) +
  geom_col(position = "stack", colour = "grey", alpha = 0.9, width = 0.7) +
  facet_grid(name~sector) +
  scale_fill_SU() +
  theme(#legend.position = "none",
    strip.background = element_rect(fill = NA, colour = "grey"),
    strip.text = element_text(face = "bold")
    ) +
  labs(x = "Year", y = "Proportion (%)",
       fill = "Wait duration (days)",
       title = "Change in wait time for treatment",
       subtitle = "Proportion of elective ophthalmology care contacts by wait time | National | 2018-22"
       )
  


# Ethnicity and deprivation 
national_data_ortho %>%
  filter(imd_quintile %in% c(1,5)) %>% 
  mutate(ethnic_group = case_when(ethnic_group %in% c("NULL", "99") ~ "Z", TRUE ~ ethnic_group)) %>% 
  mutate(ethnic_group = str_sub(ethnic_group, 1,1)) %>% 
  left_join(ethnicity_lookup, by = c("ethnic_group" = "Code")) %>% 
  
  group_by(der_activity_month, imd_quintile, ethnicity_broad, type) %>% 
  sum_spells_function(.) %>% 
  group_by(der_activity_month, type, imd_quintile, ethnicity_broad) %>% 
  mutate(total_activity = sum(n_spells_IP, n_spells_OP)) %>% 
  pivot_longer(cols = c(-der_activity_month, -type, -ethnicity_broad, -imd_quintile)) %>% 
  pivot_wider(id_cols = c(der_activity_month, ethnicity_broad, imd_quintile, name), 
              names_from = type,
              values_from = value
              ) %>% 
  mutate(`Independent Sector` = case_when(is.na(`Independent Sector`) ~ 0, TRUE ~ `Independent Sector`)) %>% 
  group_by(der_activity_month, ethnicity_broad, imd_quintile, name) %>% 
  mutate(prop = `Independent Sector`/ sum(`Independent Sector`, `NHS`) * 100) %>% 
  mutate(name = case_when(str_detect(name, "IP") ~ "Inpatient admissions", 
                          str_detect(name, "OP") ~ "Outpatient appointments",
                          str_detect(name, "cost") ~ "Costs",
                          TRUE ~ "All activity")) %>% 
  filter(der_activity_month > "2018-01-01" &
           der_activity_month < "2022-11-01") %>%
  filter(name != "All activity") %>% 
  ungroup() %>% 
  filter(name != "Costs") %>%
  filter(ethnicity_broad != "Not stated_broad") %>% 
  filter(is.finite(prop)) %>% 
  mutate(imd_quintile = case_when(imd_quintile == 1 ~ "Deprivation quintile 1",
                           imd_quintile == 2 ~ "2",
                           imd_quintile == 3 ~ "3",
                           imd_quintile == 4 ~ "4",
                           imd_quintile == 5 ~ "Deprivation quintile 5"
  )) %>% 
  mutate(imd_quintile = factor(imd_quintile, 
                        levels = c("Deprivation quintile 1","2","3","4","Deprivation quintile 5")
  )) %>% 
  
  ggplot(aes(x = der_activity_month, y = prop, colour = name)) +
  geom_smooth(method = "loess", span = 0.3, se = FALSE
              ) +
  facet_grid(ethnicity_broad~imd_quintile#, scales = "free"
             ) +
  scale_color_SU() +
  scale_y_continuous(labels = comma, oob = squish) +
  theme(strip.background = element_rect(fill = NA, colour = "grey"),
        strip.text = element_text(face = "bold"), 
        axis.text.x = element_text(angle = 90),
        legend.position = "bottom"
        ) +
  labs(x = "Month", y = "Independent sector proportion (%)",
       colour = "",
       title = "Independent sector proportion of activity by deprivation quintile and ethnic group",
       subtitle = "Orthopaedic elective procedures | National | 2018-22")

# Opthal
national_data_ophthal %>%
  mutate(ethnic_group = case_when(ethnic_group %in% c("NULL", "99") ~ "Z", TRUE ~ ethnic_group)) %>% 
  mutate(ethnic_group = str_sub(ethnic_group, 1,1)) %>% 
  left_join(ethnicity_lookup, by = c("ethnic_group" = "Code")) %>% 
  
  group_by(der_activity_month, imd_quintile, ethnicity_broad, type) %>% 
  sum_spells_function(.) %>% 
  group_by(der_activity_month, type, imd_quintile, ethnicity_broad) %>% 
  mutate(total_activity = sum(n_spells_IP, n_spells_OP)) %>% 
  pivot_longer(cols = c(-der_activity_month, -type, -ethnicity_broad, -imd_quintile)) %>% 
  pivot_wider(id_cols = c(der_activity_month, ethnicity_broad, imd_quintile, name), 
              names_from = type,
              values_from = value
  ) %>% 
  mutate(`Independent Sector` = case_when(is.na(`Independent Sector`) ~ 0, TRUE ~ `Independent Sector`)) %>% 
  group_by(der_activity_month, ethnicity_broad, imd_quintile, name) %>% 
  mutate(prop = `Independent Sector`/ sum(`Independent Sector`, `NHS`) * 100) %>% 
  mutate(name = case_when(str_detect(name, "IP") ~ "Inpatient admissions", 
                          str_detect(name, "OP") ~ "Outpatient appointments",
                          str_detect(name, "cost") ~ "Costs",
                          TRUE ~ "All activity")) %>% 
  filter(der_activity_month > "2018-01-01" &
           der_activity_month < "2022-11-01") %>%
  filter(name != "All activity") %>% 
  ungroup() %>% 
  filter(name != "Costs") %>%
  filter(ethnicity_broad != "Not stated_broad") %>% 
  
  ggplot(aes(x = der_activity_month, y = prop, colour = name)) +
  geom_smooth(method = "loess", span = 0.3, size = 0.75) +
  facet_grid(str_wrap(ethnicity_broad,13)~imd_quintile) +
  scale_color_SU() +
  theme(strip.background = element_rect(fill = NA, colour = "grey"),
        strip.text = element_text(face = "bold", size = 8), 
        axis.text.x = element_text(angle = 90),
        legend.position = "bottom",
        axis.text = element_text(size = 7),
        axis.title.x = element_blank()
        ) +
  labs(#x = "", 
       y = "Independent sector proportion (%)",
       colour = "",
       title = "Independent sector proportion of activity by deprivation quintile and ethnic group",
       subtitle = "Ophthalmology elective procedures | National | 2018-22")



# Trends by local geography ----

# Look up(s)
stp_region_lookup <- 
  read_csv("STP to NHS England Region (April 2020).csv") %>% 
  clean_names()

# Data
stp_data <- 
  read_csv("stp_data.csv") %>% 
  left_join(stp_region_lookup, by = c("stp21nm" = "stp20nm")) %>% 
  mutate(nhser20nm = case_when(str_detect(stp21nm, "Sussex")~ "South East", TRUE ~ nhser20nm))

stp_data_ortho <-
  stp_data %>% 
  filter(speciality == "Orthopaedic")

stp_data_ophthal <-
  stp_data %>% 
  filter(speciality == "Ophthalmology")


# Region
# IS share
stp_data_ortho %>% 
  wrangle_function(., nhser20nm) %>% 
  filter(!is.nan(prop)) %>% 
  graph_function(., "Orthopaedic") +
  facet_grid(~str_wrap(var_1,15))

stp_data_ophthal %>% 
  wrangle_function(., nhser20nm) %>% 
  filter(!is.nan(prop)) %>% 
  graph_function(., "Ophthalmology") +
  facet_grid(~str_wrap(var_1,30))

# Volume
stp_data_ortho %>% 
  wrangle_function(., nhser20nm) %>% 
  filter(!is.nan(prop)) %>% 
  pivot_longer(cols = c(`Independent Sector`, NHS),
               names_to = "sector") %>% 
  ggplot(aes(x = der_activity_month, y = value, colour = var_1)) +
  geom_smooth(method = 'loess', span = 0.4, se = FALSE, size = 1.25) +
  facet_grid(name~sector) +
  scale_color_SU() +
  scale_y_continuous(labels = comma) +
  theme(strip.text = element_text(face = "bold"),
        strip.background = element_rect(fill = NA, colour = "grey"),
        axis.title.x = element_blank()
        ) +
  labs(y = "Activity", 
       colour = "NHSE region",
       title = "Regional activity volumes by care type and sector: Orthopaedic",
       subtitle = "Monthly elective activity counts | NHSE region | 2018-22")

stp_data_ophthal %>% 
  wrangle_function(., nhser20nm) %>% 
  filter(!is.nan(prop)) %>% 
  pivot_longer(cols = c(`Independent Sector`, NHS),
               names_to = "sector") %>% 
  ggplot(aes(x = der_activity_month, y = value, colour = var_1)) +
  geom_smooth(method = 'loess', span = 0.4, se = FALSE, size = 1.25) +
  facet_grid(name~sector, scales = "free") +
  scale_color_SU() +
  scale_y_continuous(labels = comma) +
  theme(strip.text = element_text(face = "bold"),
        strip.background = element_rect(fill = NA, colour = "grey"),
        axis.title.x = element_blank()
        ) +
  labs(y = "Activity", 
       colour = "NHSE region",
       title = "Regional activity volumes by care type and sector: Ophthalmology",
       subtitle = "Monthly elective activity counts | NHSE region | 2018-22")


# STP 

stp_name_short_lookup <-
  tribble(
  ~stp_name, ~stp_name_short,
  
  "Bath and North East Somerset, Swindon and Wiltshire",  "Bath and North East Somerset, Swindon and Wiltshire",
  "Bedfordshire, Luton and Milton Keynes", "Bedfordshire, Luton and Milton Keynes",
  "Birmingham and Solihull", "Birmingham and Solihull",
  "Bristol, North Somerset and South Gloucestershire", "Bristol, North Somerset and South Gloucestershire",
  "Buckinghamshire, Oxfordshire and Berkshire West", "Buckinghamshire, Oxfordshire and Berkshire West",
  "Cambridgeshire and Peterborough", "Cambridgeshire and Peterborough",
  "Cheshire and Merseyside", "Cheshire and Merseyside",
  "Cornwall and the Isles of Scilly Health and Social Care Partnership", "Cornwall and the Isles of Scilly",
  "Coventry and Warwickshire", "Coventry and Warwickshire",
  "Cumbria and North East", "Cumbria and North East",
  "Devon", "Devon",
  "Dorset", "Dorset",
  "East London Health and Care Partnership", "East London",
  "Frimley Health and Care ICS", "Frimley ",
  "Gloucestershire", "Gloucestershire",
  "Greater Manchester Health and Social Care Partnership", "Greater Manchester",
  "Hampshire and the Isle of Wight", "Hampshire and the Isle of Wight",
  "Healthier Lancashire and South Cumbria", "Healthier Lancashire and South Cumbria",
  "Herefordshire and Worcestershire", "Herefordshire and Worcestershire",
  "Hertfordshire and West Essex", "Hertfordshire and West Essex",
  "Humber, Coast and Vale", "Humber, Coast and Vale",
  "Joined Up Care Derbyshire", "Joined Up Care Derbyshire",
  "Kent and Medway", "Kent and Medway",
  "Leicester, Leicestershire and Rutland", "Leicester, Leicestershire and Rutland",
  "Lincolnshire", "Lincolnshire",
  "Mid and South Essex", "Mid and South Essex",
  "Norfolk and Waveney Health and Care Partnership", "Norfolk and Waveney",
  "North London Partners in Health and Care", "North London",
  "North West London Health and Care Partnership", "North West London",
  "Northamptonshire", "Northamptonshire",
  "Nottingham and Nottinghamshire Health and Care", "Nottingham and Nottinghamshire",
  "Our Healthier South East London", "South East London",
  "Shropshire and Telford and Wrekin", "Shropshire and Telford and Wrekin",
  "Somerset", "Somerset",
  "South West London Health and Care Partnership", "South West London",
  "South Yorkshire and Bassetlaw", "South Yorkshire and Bassetlaw",
  "Staffordshire and Stoke on Trent", "Staffordshire and Stoke on Trent",
  "Suffolk and North East Essex", "Suffolk and North East Essex",
  "Surrey Heartlands Health and Care Partnership", "Surrey Heartlands",
  "Sussex Health and Care Partnership", "Sussex",
  "The Black Country and West Birmingham", "The Black Country and West Birmingham",
  "West Yorkshire and Harrogate (Health and Care Partnership)", "West Yorkshire and Harrogate"
  )






## Facet grid/wraps
stp_data_ophthal %>% 
  wrangle_function(., stp21nm) %>% 
  left_join(stp_region_lookup, by = c("var_1" = "stp20nm")) %>% 
  filter(!is.nan(prop)) %>% 
  graph_function(., "Ophthalmology") +
  facet_wrap(nhser20nm~str_wrap(var_1,30))


stp_data_ophthal %>% 
  wrangle_function(., stp21nm) %>% 
  left_join(stp_region_lookup, by = c("var_1" = "stp20nm")) %>% 
  mutate(nhser20nm = case_when(str_detect(var_1, "Sussex")~ "South East", TRUE ~ nhser20nm)) %>% 
  filter(!is.nan(prop)) %>% 
  graph_function(., "Ophthalmology") +
  facet_grid(nhser20nm~str_wrap(var_1,30), space = "free")

## geom_jitter approach
# Ortho
stp_data_ortho %>% 
  wrangle_function(., stp21nm) %>% 
  pivot_longer(cols = c(`Independent Sector`, NHS),
               names_to = "sector",
               values_to = "value"
  ) %>% 
  mutate(year = lubridate::year(der_activity_month)) %>% 
  group_by(year, var_1, name, sector) %>% 
  summarise(value = sum(value)) %>% 
  group_by(year, var_1, name) %>% 
  mutate(prop = value/sum(value)*100) %>% 
  left_join(stp_data %>% 
              select(stp21nm, nhser20nm) %>% 
              distinct(),
            by = c("var_1" = "stp21nm")) %>% 
  #filter(year == 2022) %>% 
  ungroup() %>% 
  filter(sector == "Independent Sector") %>% 
  
  ggplot(aes(x = prop, y = 1, fill = nhser20nm)) +
  geom_jitter(shape = 21, height = 0.75, size = 5) +
  ylim(0,2) +
  xlim(0,100) +
  facet_grid(year~name) +
  scale_fill_SU() +
  theme(axis.text.y = element_blank(),
        axis.title.y = element_blank(),
        strip.text = element_text(face = "bold"),
        strip.background = element_rect(fill = NA, colour = "grey")
  ) +
  labs(x = "Independent sector proportion (%)", 
       fill = "NHSE region",
       title = "Share of activity delivered by the independent sector: Orthopaedic",
       subtitle = "Annual elective activity | STP/ICB's grouped by NHSE region | 2018-22")

# Ophthal
stp_data_ophthal %>% 
  wrangle_function(., stp21nm) %>% 
  pivot_longer(cols = c(`Independent Sector`, NHS),
               names_to = "sector",
               values_to = "value"
  ) %>% 
  mutate(year = lubridate::year(der_activity_month)) %>% 
  group_by(year, var_1, name, sector) %>% 
  summarise(value = sum(value)) %>% 
  group_by(year, var_1, name) %>% 
  mutate(prop = value/sum(value)*100) %>% 
  left_join(stp_data %>% 
              select(stp21nm, nhser20nm) %>% 
              distinct(),
            by = c("var_1" = "stp21nm")) %>% 
  #filter(year == 2022) %>% 
  ungroup() %>% 
  filter(sector == "Independent Sector") %>% 
  
  ggplot(aes(x = prop, y = 1, fill = nhser20nm)) +
  geom_jitter(shape = 21, height = 0.75, size = 5) +
  ylim(0,2) +
  xlim(0,100) +
  facet_grid(year~name, scales = "free") +
  scale_fill_SU() +
  theme(axis.text.y = element_blank(),
        axis.title.y = element_blank(),
        strip.text = element_text(face = "bold"),
        strip.background = element_rect(fill = NA, colour = "grey")
  ) +
  labs(x = "Independent sector proportion (%)", 
       fill = "NHSE region",
       title = "Share of activity delivered by the independent sector: Ophthalmology",
       subtitle = "Annual elective activity | STP/ICB's grouped by NHSE region | 2018-22")


# Patch work facetgrid approach
ICB_graph_function <- function(data_input, speciality) {
  
  data <- data_input
  
  function_1 <- function(region){
    data %>% 
      wrangle_function(., stp21nm) %>% 
      left_join(stp_region_lookup, by = c("var_1" = "stp20nm")) %>% 
      mutate(nhser20nm = case_when(str_detect(var_1, "Sussex")~ "South East", TRUE ~ nhser20nm)) %>% 
      filter(nhser20nm == region) %>% 
      filter(!is.nan(prop)) %>% 
      left_join(stp_name_short_lookup, by = c("var_1" = "stp_name")) %>% 
      mutate(stp_name_short = case_when(nchar(stp_name_short) <= 20 ~ stp_name_short, 
                                        TRUE ~ paste0(stringr::str_sub(stp_name_short, 0, 20), "..."))
      ) %>%
      
      ggplot(aes(x = der_activity_month, y = prop, colour = name)) +
      geom_smooth(method = "loess", span = 0.3, se = FALSE) +
      facet_grid(str_wrap(nhser20nm, 12) ~ str_wrap(stp_name_short, 15)) +
      scale_color_SU() +
      scale_y_continuous(limits = c(0,100), oob = squish) +
      scale_x_date(date_breaks = "2 years", labels = label_date_short()) +
      theme(strip.background = element_rect(fill = NA, colour = "grey"),
            strip.text.y = element_text(face = "bold", angle = 0, size = 8), 
            strip.text.x = element_text(size = 6.5), 
            axis.text.x = element_text(angle = 90),
            legend.position = "none",
            plot.title = element_blank(),
            plot.subtitle = element_blank(),
            axis.title = element_blank())
  }  
  
  plot_1 <- function_1("East of England")
  plot_2 <- function_1("London")
  plot_3 <- function_1("Midlands")
  plot_4 <- function_1("North East and Yorkshire")
  plot_5 <- function_1("North West")
  plot_6 <- function_1("South East")
  plot_7 <- function_1("South West") +
    theme(legend.position = "bottom") + 
    labs(colour = "")
  
  patch_1 <-
    plot_3 +
    (plot_1 + 
       plot_2 +
       plot_4 +
       plot_5 +
       plot_6 +
       plot_7 +
       plot_layout(ncol = 2)) +
    plot_layout(nrow = 2, 
                heights = c(0.2, 0.8)) +    
    plot_annotation(title = "Independent sector provision share by ICB and region", 
                    subtitle = paste0(speciality, " elective care | 2018-22")
    )
  
  patch_1
  
}


ICB_graph_function(stp_data_ophthal, "Ophthalmology")


# Procedure specific 
ICB_graph_function(stp_data_ophthal %>% 
                     filter(procedure_desc_short == "Cataract"), 
                   "Ophthalmology - Cataract")

ICB_graph_function(stp_data_ophthal %>% 
                     filter(procedure_desc_short == "Vitreous Retinal"), 
                   "Ophthalmology - Vitreous Retinal")

ICB_graph_function(stp_data_ophthal %>% 
                     filter(procedure_desc_short == "Glaucoma"), 
                   "Ophthalmology - Glaucoma")


# Demographic - deprivation 
ICB_graph_function(stp_data_ophthal %>% 
                     filter(imd_quintile == 1), 
                   "Ophthalmology - IMD quintile 1")

ICB_graph_function(stp_data_ophthal %>% 
                     filter(imd_quintile == 2), 
                   "Ophthalmology - IMD quintile 2")

ICB_graph_function(stp_data_ophthal %>% 
                     filter(imd_quintile == 3), 
                   "Ophthalmology - IMD quintile 3")

ICB_graph_function(stp_data_ophthal %>% 
                     filter(imd_quintile == 4), 
                   "Ophthalmology - IMD quintile 4")

ICB_graph_function(stp_data_ophthal %>% 
                     filter(imd_quintile == 5), 
                   "Ophthalmology - IMD quintile 5")



stp_data_ortho %>% 
  select(procedure_desc_short) %>% 
  distinct()


ICB_graph_function(stp_data_ortho, "Orthopaedic")





# Deprivation by region plot




# Deprivation by region and ICB

stp_data_ophthal %>%
  group_by(der_activity_month, imd_quintile, nhser20nm, type) %>% 
  sum_spells_function(.) %>% 
  group_by(der_activity_month, type, imd_quintile, nhser20nm) %>% 
  mutate(total_activity = sum(n_spells_IP, n_spells_OP)) %>% 
  pivot_longer(cols = c(-der_activity_month, -type, -nhser20nm, -imd_quintile)) %>% 
  pivot_wider(id_cols = c(der_activity_month, nhser20nm, imd_quintile, name), 
              names_from = type,
              values_from = value
              ) %>% 
  mutate(`Independent Sector` = case_when(is.na(`Independent Sector`) ~ 0, TRUE ~ `Independent Sector`)) %>% 
  group_by(der_activity_month, nhser20nm, imd_quintile, name) %>% 
  mutate(prop = `Independent Sector`/ sum(`Independent Sector`, `NHS`) * 100) %>% 
  mutate(name = case_when(str_detect(name, "IP") ~ "Inpatient admissions", 
                          str_detect(name, "OP") ~ "Outpatient appointments",
                          str_detect(name, "cost") ~ "Costs",
                          TRUE ~ "All activity")) %>% 
  filter(der_activity_month > "2018-01-01" &
           der_activity_month < "2022-11-01") %>%
  filter(name != "All activity") %>% 
  ungroup() %>% 
  filter(name != "Costs") %>%
  
  ggplot(aes(x = der_activity_month, y = prop, colour = name)) +
  geom_smooth(method = "loess", span = 0.3, size = 0.75) +
  facet_grid(str_wrap(nhser20nm,13)~imd_quintile) +
  scale_color_SU() +
  #scale_x_date(date_breaks = "2 years", labels = label_date_short()) +
  theme(strip.background = element_rect(fill = NA, colour = "grey"),
        strip.text = element_text(face = "bold", size = 8), 
        legend.position = "bottom",
        axis.title.x = element_blank()
        ) +
  labs(y = "Independent sector proportion (%)",
       colour = "",
       title = "Independent sector proportion of activity by deprivation quintile and region",
       subtitle = "Ophthalmology elective procedures | National | 2018-22")

stp_data_ortho %>%
  group_by(der_activity_month, imd_quintile, nhser20nm, type) %>% 
  sum_spells_function(.) %>% 
  group_by(der_activity_month, type, imd_quintile, nhser20nm) %>% 
  mutate(total_activity = sum(n_spells_IP, n_spells_OP)) %>% 
  pivot_longer(cols = c(-der_activity_month, -type, -nhser20nm, -imd_quintile)) %>% 
  pivot_wider(id_cols = c(der_activity_month, nhser20nm, imd_quintile, name), 
              names_from = type,
              values_from = value) %>% 
  mutate(`Independent Sector` = case_when(is.na(`Independent Sector`) ~ 0, TRUE ~ `Independent Sector`)) %>% 
  group_by(der_activity_month, nhser20nm, imd_quintile, name) %>% 
  mutate(prop = `Independent Sector`/ sum(`Independent Sector`, `NHS`) * 100) %>% 
  mutate(name = case_when(str_detect(name, "IP") ~ "Inpatient admissions", 
                          str_detect(name, "OP") ~ "Outpatient appointments",
                          str_detect(name, "cost") ~ "Costs",
                          TRUE ~ "All activity")) %>% 
  filter(der_activity_month > "2018-01-01" &
           der_activity_month < "2022-11-01") %>%
  filter(name != "All activity") %>% 
  ungroup() %>% 
  filter(name != "Costs") %>%
  
  ggplot(aes(x = der_activity_month, y = prop, colour = name)) +
  geom_smooth(method = "loess", span = 0.3, size = 0.75, se = FALSE) +
  facet_grid(str_wrap(nhser20nm,13)~imd_quintile) +
  scale_color_SU() +
  scale_x_date(date_breaks = "2 years", labels = label_date_short()) +
  theme(strip.background = element_rect(fill = NA, colour = "grey"),
        strip.text = element_text(face = "bold", size = 8), 
        axis.text.x = element_text(angle = 90),
        legend.position = "bottom",
        axis.title.x = element_blank()) +
  labs(y = "Independent sector proportion (%)",
       colour = "",
       title = "Independent sector proportion of activity by deprivation quintile and region",
       subtitle = "Orthopaedic elective procedures | National | 2018-22")




















# Notes ----

# PHIN data 
# https://www.phin.org.uk/data 
# Markdown, Seb HF -  https://rpubs.com/sgpeytrignet/958780 

# E-referrals 
# Optomotrists directly referring to Independent sector 
# https://digital.nhs.uk/dashboards/ers-open-data 
  

  

  



  

  
            
            
            
            