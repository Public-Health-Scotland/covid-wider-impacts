# Code to create charts and tables for publication

###############################################.
## Packages ----
###############################################.
library(dplyr)
library(readr)
library(ggplot2)
library(tidyr)

###############################################.
## Data ----
###############################################.
# where the charts and tables are going to be saved
outputs <- "/conf/PHSCOVID19_Analysis/Publication outputs/"

# RAPID data
rap_pub <- readRDS("shiny_app/data/rapid_data.rds") %>% 
  filter(area_name == "Scotland")

###############################################.
## Palettes and plot parameters ----
###############################################.
pal_age <- c('#543005', '#8c510a', '#bf812d',  '#d0d1e6',
             '#74add1', '#4575b4', '#313695')
pal_depr <- c('#abd9e9', '#74add1', '#4575b4', '#313695', '#022031')
pal_overall <- c('#000000', '#08519c','#bdd7e7')

plot_subtitle <- "22 March 2020 - 21 April 2020" 

###############################################.
## RAPID charts and tables ----
###############################################.
###############################################.
# Table
# Table for sex and age
rap_table <- readRDS("/conf/PHSCOVID19_Analysis/Admissions_by_category_24_Apr.rds") %>% 
  janitor::clean_names() %>% 
  # taking out aggregated values, not clear right now
  filter(!(substr(hosp,3,5) == "All" | (substr(hscp_name,3,5) == "All")))

rap_table <- rap_table %>% 
  filter(between(date_adm, as.Date("2020-03-22"), as.Date("2020-04-21"))) %>% 
  group_by(age_group, sex) %>% 
  summarise(count= sum(count)) %>% ungroup ()

rap_table <- rap_table %>% ungroup() %>% 
  mutate(age_group = recode(age_group, "5_thru_14" = "5 - 14", "15_thru_44" = "15 - 44", "45_thru_64" = "45 - 64",
                           "65_thru_74" = "65 -74", "75_thru_84" = "75 -84",
                           "85+" = "Over 85", "Under_5" = "Under 5")) %>% 
  mutate(age_group = factor(age_group, levels = c("Under 5", "5 - 14", "15 - 44", "45 - 64", "65 -74", 
                                                "75 -84", "Over 85"))) %>%
  mutate(sex = recode(sex, "male" = "Male", "female" = "Female", .missing = "Unknown")) %>% 
  pivot_wider(names_from = sex, values_from = count) %>% 
  arrange(age_group) %>% 
  rename("Age group" = age_group)

write_csv(rap_table, paste0(outputs, "rapid_agesex_table.csv"))

###############################################.
# Specialty bullet point 
# What specialty got most admissions
rap_spec_bullet <- rap_pub %>% 
  filter(type == "sex" & admission_type == "All" & category == "All" 
           between(date, as.Date("2020-03-22"), as.Date("2020-04-21"))) %>% 
  group_by(spec) %>% summarise(count = sum(count)) %>% 
  arrange(-count)

write_csv(rap_spec_bullet, paste0(outputs, "rapid_spec_table.csv"))

###############################################.
# Chart for age groups
rap_age_chart_data <- rap_pub %>% 
  filter(spec == "All" & type == "age" & admission_type == "All" &
           between(date, as.Date("2020-03-22"), as.Date("2020-04-21"))) %>% 
  mutate(category = factor(category, levels = c("Under 5", "5 - 14", "15 - 44", "45 - 64", "65 -74", 
                                                "75 -84", "Over 85"))) %>% 
  group_by(category) %>% summarise(count = sum(count))

rap_age_chart <- ggplot(rap_age_chart_data, aes(x=category, y = count))+
  geom_bar(stat = "identity", fill = "#004785")+
  # Axis, plot  and legend titles
  labs(y = "Number of admissions", x = "Age group",
       title = "Number of admissions to hospital in Scotland by age group",
       subtitle = plot_subtitle)+
  theme_classic() +
  theme(axis.text = element_text(colour = "black")) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 10000))

rap_age_chart

ggsave(paste0(outputs, "rapid_age_chart.png"), rap_age_chart)

###############################################.
# Chart for overall/elective/emergency 
rap_overall_chart_data <- rap_pub %>% 
  filter(spec == "All" & type == "sex" & category == "All" &
           between(date, as.Date("2020-03-22"), as.Date("2020-04-21"))) 

rap_overall_chart <- ggplot(rap_overall_chart_data, aes(x=date, y = count, color = admission_type))+
  geom_line()+
  scale_colour_manual(values = pal_overall) +
  # Axis, plot  and legend titles
  labs(x = "Date (2020)", y = "Number of admissions", color = "Age group",
       title = "Number of admissions to hospital in Scotland by type",
       subtitle = plot_subtitle)+
  theme_classic() +
  theme(axis.text = element_text(colour = "black")) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 1600))

rap_overall_chart

ggsave(paste0(outputs, "rapid_overall_chart.png"), rap_overall_chart)

###############################################.
# Chart for deprivation
rap_depr_chart_data <- rap_pub %>% 
  filter(spec == "All" & type == "depr" & admission_type == "All" &
           between(date, as.Date("2020-03-22"), as.Date("2020-04-21"))) %>% 
  group_by(category) %>% summarise(count = sum(count))

rap_depr_chart <- ggplot(rap_depr_chart_data, aes(x=category, y = count))+
  geom_bar(stat = "identity", fill = "#004785")+
  # Axis, plot  and legend titles
  labs(x = "SIMD deprivation quintile", y = "Number of admissions", 
       title = "Number of admissions to hospital in Scotland \nby SIMD deprivation quintile",
       subtitle = plot_subtitle)+
  theme_classic() +
  theme(axis.text = element_text(colour = "black")) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 10000))

rap_depr_chart

ggsave(paste0(outputs, "rapid_simd_chart.png"), rap_depr_chart)



