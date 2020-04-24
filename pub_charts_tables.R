# Code to create charts and tables for publication

###############################################.
## Packages ----
###############################################.
library(dplyr)
library(readr)
library(ggplot2)

###############################################.
## Data ----
###############################################.
# where the charts and tables are going to be saved
outputs <- "/conf/PHSCOVID19_Analysis/Publication outputs/"

rap_pub <- readRDS("shiny_app/data/rapid_data.rds") %>% 
  filter(area_name == "Scotland")

###############################################.
## Palettes and plot parameters ----
###############################################.
pal_age <- c('#543005', '#8c510a', '#bf812d',  '#d0d1e6',
             '#74add1', '#4575b4', '#313695')
pal_depr <- c('#abd9e9', '#74add1', '#4575b4', '#313695', '#022031')


###############################################.
## RAPID charts and tables ----
###############################################.
###############################################.
# Table
# Table for sex and age
rap_table <- rap_pub %>% filter(type != "depr" & spec == "All")

###############################################.
# Chart for age groups
rap_age_chart_data <- rap_pub %>% 
  filter(spec == "All" & type == "age" & admission_type == "All" &
           between(date, as.Date("2020-03-22"), as.Date("2020-04-21"))) %>% 
  mutate(category = factor(category, levels = c("Under 5", "15 - 44", "45 - 64", "65 -74", 
                                                "75 -84", "Over 85"))) 

rap_age_chart <- ggplot(rap_age_chart_data, aes(x=date, y = count, color = category))+
  geom_line()+
  scale_colour_manual(values = pal_age) +
  # Axis, plot  and legend titles
  labs(x = "Date (2020)", y = "Number of admissions", color = "Age group",
       title = "Number of admissions to hospital in Scotland by age group",
       subtitle = "22 March 2020 - 21 April 2020" )+
  theme_classic() +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 500))

rap_age_chart

ggsave(paste0(outputs, "rapid_age_chart.png"), rap_age_chart)


###############################################.
# Chart for age groups
rap_depr_chart_data <- rap_pub %>% 
  filter(spec == "All" & type == "depr" & admission_type == "All" &
           between(date, as.Date("2020-03-22"), as.Date("2020-04-21"))) 

rap_depr_chart <- ggplot(rap_depr_chart_data, aes(x=date, y = count, color = category))+
  geom_line()+
  scale_colour_manual(values = pal_depr) +
  # Axis, plot  and legend titles
  labs(x = "Date (2020)", y = "Number of admissions", color = "SIMD deprivation quintile",
       title = "Number of admissions to hospital in Scotland \nby SIMD deprivation quintile",
       subtitle = "22 March 2020 - 21 April 2020" )+
  theme_classic() +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 400))

rap_depr_chart

ggsave(paste0(outputs, "rapid_simd_chart.png"), rap_depr_chart)



