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
rap_pub <- readRDS("data/rapid_data_pub.rds") %>% 
  filter(area_name == "Scotland")

###############################################.
## Palettes and plot parameters ----
###############################################.
pal_age <- c('#543005', '#8c510a', '#bf812d',  '#d0d1e6',
             '#74add1', '#4575b4', '#313695')
pal_depr <- c('#abd9e9', '#74add1', '#4575b4', '#313695', '#022031')
pal_overall <- c('#000000', '#08519c','#bdd7e7')

plot_subtitle <- "Week ending 12 January 2020 - Week ending 19 April 2020" 

###############################################.
## RAPID charts and tables ----
###############################################.
###############################################.
# Specialty bullet point 
# What specialty got most admissions
rap_spec_bullet <- rap_pub %>% 
  filter(type == "sex" & admission_type == "All" & category == "All" &
           between(week_ending, as.Date("2020-01-12"), as.Date("2020-04-19"))) %>% 
  group_by(spec) %>% summarise(count = sum(count)) %>% 
  arrange(-count)

write_csv(rap_spec_bullet, paste0(outputs, "rapid_spec_table.csv"))

###############################################.
# Chart and table for age and sex groups
# Preparing the data
rap_agesex <- readRDS("/conf/PHSCOVID19_Analysis/Admissions_by_category_24_Apr.rds") %>% 
  janitor::clean_names() %>% 
  # taking out aggregated values, not clear right now
  filter(!(substr(hosp,3,5) == "All" | (substr(hscp_name,3,5) == "All")))

rap_agesex <- rap_agesex %>% 
  mutate(week_ending = ceiling_date(date_adm, "week")) %>% #end of week
  filter(between(week_ending, as.Date("2020-01-12"), as.Date("2020-04-21"))) %>% 
  group_by(age_group, sex) %>% 
  summarise(count= sum(count)) %>% ungroup () %>% 
  mutate(age_group = recode(age_group, "5_thru_14" = "5 - 14", "15_thru_44" = "15 - 44", "45_thru_64" = "45 - 64",
                            "65_thru_74" = "65 -74", "75_thru_84" = "75 -84",
                            "85+" = "85 and over", "Under_5" = "Under 5")) %>% 
  mutate(age_group = factor(age_group, levels = c("Under 5", "5 - 14", "15 - 44", "45 - 64", "65 -74", 
                                                  "75 -84", "85 and over"))) %>%
  mutate(sex = recode(sex, "male" = "Male", "female" = "Female", .missing = "Unknown")) 

rap_table <- rap_agesex %>% 
  pivot_wider(names_from = sex, values_from = count) %>% 
  arrange(age_group) %>% 
  rename("Age group" = age_group)

write_csv(rap_table, paste0(outputs, "rapid_agesex_table.csv"))

#Bringing population to calculate rates
pop_lookup <- readRDS("/conf/linkage/output/lookups/Unicode/Populations/Estimates/HB2019_pop_est_1981_2018.rds") %>% 
  janitor::clean_names() %>% 
  filter(year == 2018) %>% 
  mutate(sex = recode(sex_name, "M" = "Male", "F" = "Female"),
         age_group = case_when(age < 5 ~ "Under 5", age>=5 & age<15 ~ "5 - 14", 
                               age>=15 & age<45 ~ "15 - 44", age>=45 & age<65 ~ "45 - 64", 
                               age>=65 & age<75 ~ "65 -74", 
                               age>=75 & age<85 ~ "75 -84", age>=85 ~ "85 and over")) %>% 
  group_by(sex, age_group) %>% 
  summarise(pop = sum(pop))

rap_agesex <- rap_agesex %>% filter(sex != "Unknown")

rap_agesex <- left_join(rap_agesex, pop_lookup, by = c("sex", "age_group"))
# Rates per 100,000 people
rap_agesex <- rap_agesex %>% mutate(rate = round((count/pop)*100000)) %>% 
  mutate(age_group = factor(age_group, levels = c("Under 5", "5 - 14", "15 - 44", "45 - 64", "65 -74", 
                                                  "75 -84", "85 and over"))) 

write_csv(rap_agesex, paste0(outputs, "rapid_agesex_chart_data.csv"))

rap_age_chart <- ggplot(rap_agesex, aes(x=age_group, y = ifelse(sex == "Male", -rate, rate),
                        fill = sex)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = c("#004785", "#bdd7e7")) +
  # Axis, plot  and legend titles
  labs(y = "Age group", x = "Admissions to hospital per 100,000 people",
       title = "Rate of admissions to hospital by age group and sex",
       subtitle = plot_subtitle, fill = "Sex")+
  scale_y_continuous(labels = abs, limits = max(rap_agesex$rate) * c(-1,1)) +
  theme_classic() +
  theme(axis.text = element_text(colour = "black")) +
  coord_flip()

rap_age_chart

ggsave(paste0(outputs, "rapid_age_chart.png"), rap_age_chart)

###############################################.
# Chart for overall/elective/emergency 
rap_overall_chart_data <- rap_pub %>% 
  filter(spec == "All" & type == "sex" & category == "All" &
           between(week_ending, as.Date("2020-01-12"), as.Date("2020-04-21"))) 

write_csv(rap_overall_chart_data, paste0(outputs, "rap_admissions_chart_data.csv"))

rap_overall_2019_chart_data <- rap_pub %>% 
  filter(spec == "All" & type == "sex" & category == "All" &
           between(week_ending, as.Date("2019-01-12"), as.Date("2019-04-21"))) 

write_csv(rap_overall_2019_chart_data, paste0(outputs, "rap_admissions_2019_chart_data.csv"))

rap_overall_chart <- ggplot(rap_overall_chart_data, aes(x=week_ending, y = count, color = admission_type))+
  geom_line()+
  scale_colour_manual(values = pal_overall) +
  # Axis, plot  and legend titles
  labs(x = "Date (2020)", y = "Number of admissions", color = "Age group",
       title = "Number of admissions to hospital in Scotland by type",
       subtitle = plot_subtitle)+
  theme_classic() +
  theme(axis.text = element_text(colour = "black")) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 18000))

rap_overall_chart

ggsave(paste0(outputs, "rapid_overall_chart.png"), rap_overall_chart)

# COmparing against same period previous year
rap_overall_2019_chart <- ggplot(rap_overall_2019_chart_data, aes(x=week_ending, y = count, color = admission_type))+
  geom_line()+
  scale_colour_manual(values = pal_overall) +
  # Axis, plot  and legend titles
  labs(x = "Date (2020)", y = "Number of admissions", color = "Age group",
       title = "Number of admissions to hospital in Scotland by type",
       subtitle = "12 January 2019 - 21 April 2019")+
  theme_classic() +
  theme(axis.text = element_text(colour = "black")) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 18000))

rap_overall_2019_chart

ggsave(paste0(outputs, "rapid_overall_2019_chart.png"), rap_overall_2019_chart)

###############################################.
# Day of the week admissions bullet point
# Numbers per admission type for the whole period
rap_weekday_bullet <- rap_overall_chart_data %>% 
  mutate(weekday = weekdays(date)) %>% 
  group_by(admission_type, weekday) %>% summarise(count = sum(count))

write_csv(rap_overall_bullet, paste0(outputs, "rapid_overall_table.csv"))


###############################################.
# Overall admissions bullet point
# Numbers per admission type for the whole period
rap_overall_bullet <- rap_overall_chart_data %>% 
  group_by(admission_type) %>% summarise(count = sum(count)) %>% 
  arrange(-count)

write_csv(rap_overall_bullet, paste0(outputs, "rapid_overall_table.csv"))

###############################################.
# Chart for deprivation
rap_depr_chart_data <- rap_pub %>% 
  filter(spec == "All" & type == "depr" & admission_type == "All" &
           # Dates of lockdown
           between(week_ending, as.Date("2020-03-24"), as.Date("2020-04-19"))) %>% 
  group_by(category) %>% summarise(count = sum(count))

write_csv(rap_depr_chart_data, paste0(outputs, "rap_simd_chart_data.csv"))

rap_depr_2019_chart_data <- rap_pub %>% 
  filter(spec == "All" & type == "depr" & admission_type == "All" &
           # Dates of lockdown
           between(date, as.Date("2019-03-24"), as.Date("2019-04-21"))) %>% 
  group_by(category) %>% summarise(count = sum(count))

write_csv(rap_depr_2019_chart_data, paste0(outputs, "rap_simd_2019_chart_data.csv"))

rap_depr_chart <- ggplot(rap_depr_chart_data, aes(x=category, y = count))+
  geom_bar(stat = "identity", fill = "#004785")+
  # Axis, plot  and legend titles
  labs(x = "SIMD deprivation quintile", y = "Number of admissions", 
       title = "Number of admissions to hospital in Scotland \nby SIMD deprivation quintile",
       subtitle = "24 March 2020 - 21 April 2020")+
  theme_classic() +
  theme(axis.text = element_text(colour = "black")) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 10000))

rap_depr_chart

ggsave(paste0(outputs, "rapid_simd_chart.png"), rap_depr_chart)

# Cheking same period in 2019
rap_depr_19_chart <- ggplot(rap_depr_2019_chart_data, aes(x=category, y = count))+
  geom_bar(stat = "identity", fill = "#004785")+
  # Axis, plot  and legend titles
  labs(x = "SIMD deprivation quintile", y = "Number of admissions", 
       title = "Number of admissions to hospital in Scotland \nby SIMD deprivation quintile",
       subtitle = "24 March 2019 - 21 April 2019")+
  theme_classic() +
  theme(axis.text = element_text(colour = "black")) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 18000))

rap_depr_19_chart

ggsave(paste0(outputs, "rapid_simd_2019_chart.png"), rap_depr_19_chart)

##END
