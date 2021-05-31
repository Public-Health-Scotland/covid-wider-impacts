# Data preparation for mental health tab

###############################################.
## Functions/Packages/filepaths/lookups ----
###############################################.
source("data_prep/functions_packages_data_prep.R")

###############################################.
## Prescribing - Mental health ----
###############################################.

### historic file for MH drugs ##
# Files contain rolling weeks so attention needs to be paid to not miss or 
# duplicate any week. This is not an ideal method, to get the most recent data
# for each week we would need to select the first week from each file, apart
# from for the historic one, the last one and those covered by the last one = tricky
mentalhealth_drugs <- rbind(read_xlsx(paste0(data_folder, "prescribing_mh/Weekly new incident emessage - Multi-condition Jan 18-Jun 20.xlsx")),
                                     read_xlsx(paste0(data_folder, "prescribing_mh/Weekly new incident emessage - Multi-condition_57_05-07-2020 to 04-10-2020.xlsx")),
                                     read_xlsx(paste0(data_folder, "prescribing_mh/2021-01-14-Weekly new incident emessage - Multi-condition_11-10-2020 to 10-01-2021.xlsx")) %>% 
                                       filter(between(as.Date(`Week Ending`), as.Date("2020-10-11"), as.Date("2020-10-18"))), 
                                     read_xlsx(paste0(data_folder, "prescribing_mh/2021-01-28-Weekly new incident emessage - Multi-condition_25-10-2020 to 24-01-2021.xlsx")) %>%
                                       filter(between(as.Date(`Week Ending`), as.Date("2020-10-25"), as.Date("2020-11-01"))), 
                                     read_xlsx(paste0(data_folder, "prescribing_mh/2021-02-11-Weekly new incident emessage - Multi-condition.xlsx")) %>%
                                      filter(between(as.Date(`Week Ending`), as.Date("2020-11-08"), as.Date("2020-11-15"))), 
                                     read_xlsx(paste0(data_folder, "prescribing_mh/2021-02-11-Weekly new incident emessage - Multi-condition.xlsx")) %>%
                                      filter(between(as.Date(`Week Ending`), as.Date("2020-11-22"), as.Date("2021-01-17"))),
                                     read_xlsx(paste0(data_folder, "prescribing_mh/2021-04-29-Weekly new incident emessage - Multi-condition.xlsx"))) %>%
  
  select(1:5) %>% 
  clean_names() %>% 
  filter(condition %in% c("Anxiolytic",
                          "Hypnotic",
                          "SSRI SNRI")) %>% 
  mutate(week_ending = as.Date(week_ending),
         area_type = case_when(substr(area_code,1,3) == "S37" ~ "HSC partnership",
                               substr(area_code,1,3) == "S08" ~ "Health board",
                               substr(area_code,1,3) == "S00" ~ "Scotland"),
         area_name = case_when(area_type == "Health board" ~ stringr::str_to_title(area_name),
                               area_type == "Scotland" ~ stringr::str_to_title(area_name),
                               TRUE ~ area_name),
         area_name = case_when(area_type == "Health board" ~ gsub("Nhs", "NHS", area_name),
                               TRUE ~ area_name),
         type = "condition") %>% 
  rename(category = condition,
         count = incident_cases_week_01) %>% 
  select(week_ending, area_name, area_type, type, category, count)

mentalhealth_drugs_all <- mentalhealth_drugs %>% 
  group_by(week_ending, area_name, area_type, type) %>% 
  summarise(count = sum(count),
            category = "All") %>% 
  ungroup() %>% 
  select(week_ending, area_name, area_type, type, category, count)

mentalhealth_drugs <- rbind(mentalhealth_drugs, mentalhealth_drugs_all) %>% 
  arrange(area_name, area_type, category, week_ending) # so plotly works correctly

prepare_final_data(mentalhealth_drugs, "mentalhealth_drugs", last_week = "2021-04-25")

###############################################.
## A&E - mental health ----
###############################################.
# mh_aye_hist <- read_csv(paste0(data_folder, "A&E_mh/A&E_Extract_-_Mental_Health_Wider_impacts.csv"))
# saveRDS(mh_aye_hist, paste0(data_folder, "A&E_mh/A&E_mh_2018to310502020.rds"))
mh_aye <- rbind(readRDS(paste0(data_folder, "A&E_mh/A&E_mh_2018to310502020.rds")) %>% 
                  filter(as.Date(`Arrival Date`) < as.Date("2020-06-01")) %>%
                  mutate(`Arrival Date`=as.Date(`Arrival Date`,format="%Y/%m/%d")),
                read_csv(paste0(data_folder, "A&E_mh/A&E_Extract_-_Mental_Health_Wider_impacts 01062020to23052021.csv"),
                         col_types="nnccccccnnccccccccccccccc")) %>% # col spec needed to avoid parse errors for disease 3 fields
  clean_names() 

# List of terms used to identify mh cases
mh_aye_freetext <- toupper(paste0("overdose|'OD|O/D|drug od|drugs od|drug overdose|", 
                                  "self harm|self-harm|selfharm|depress|psych|", 
                                  "mental health|mentalhealth|mental-health|",
                                  "mental illness|mentalillness|mental-illness|",
                                  "suicid|suicdal|eating disorder|eatingdisorder|eating-disorder|",
                                  " MHAT| CAHMS|behavioural disorder|mental disorder|",
                                  "personality disorder|personalitydisorder|personality-disorder|", 
                                  "anxiety|bipolar|schizophren|schizoaffective|",
                                  "anorexi|bulimi|adhd|dissociative| dsh|",
                                  "adjustment disorder|emotional disorder|bereavement|",
                                  "self-poison|selfpoison|self poison|",
                                  "delusional|hallucination|alcohol withdrawal|withdrawal from alcohol|",
                                  "drug withdrawal|drugs withdrawal|withdrawal drug|",
                                  "manic episode|panic|recreational drug use|intoxication"))

# List of terms used to identify false positives
mh_text_notincluded <- paste0("ACCIDENTAL OVERDOSE|ACCIDENTAL POISONING|",
                              "ACCIDENTAL CHILD POISONING|TYMPANIC")

mh_aye %<>%
  mutate(diagnosis_1_text = toupper(diagnosis_1_text),
         diagnosis_2_text = toupper(diagnosis_2_text),
         diagnosis_3_text = toupper(diagnosis_3_text),
         presenting_complaint_text = toupper(presenting_complaint_text)) %>% 
  # Creating variable for those case identified through codes and no free text
  mutate(def_yes = case_when((substr(intent_of_injury_code,1,2) == "02" | #intentional self-harm
                                diagnosis_1_code %in% c("16") | #16 is psychiatry
                                diagnosis_2_code %in% c("16") |
                                diagnosis_3_code %in% c("16") |
                                # Including all Fs apart from dementia, delirium and learning disabilities
                                # Including R44-R46: hallucinations, emotional states
                                # Includinx X60-x84: intentional self-harm
                                substr(disease_1_code, 1, 2) %in% c("F1", "F2", "F3", "F4", "F5", "F6", "F8", "F9", "X6", "X7") |
                                substr(disease_2_code, 1, 2) %in% c("F1", "F2", "F3", "F4", "F5", "F6", "F8", "F9", "X6", "X7") |
                                substr(disease_3_code, 1, 2) %in% c("F1", "F2", "F3", "F4", "F5", "F6", "F8", "F9", "X6", "X7") |
                                substr(disease_1_code, 1, 3) %in% c("R44", "R45", "R46", "X80", "X81", "X82", "X83", "X84",
                                                                    "F06", "F07", "F08", "F09") |
                                substr(disease_2_code, 1, 3) %in% c("R44", "R45", "R46", "X80", "X81", "X82", "X83", "X84",
                                                                    "F06", "F07", "F08", "F09") |
                                substr(disease_3_code, 1, 3) %in% c("R44", "R45", "R46", "X80", "X81", "X82", "X83", "X84",
                                                                    "F06", "F07", "F08", "F09") |
                                # sequalae and personal historyof self-harm and psych traum
                                substr(disease_1_code, 1, 4) %in% c("Y871", "Z914", "Z915", "Z004", "Z046") | 
                                substr(disease_2_code, 1, 4) %in% c("Y871", "Z914", "Z915", "Z004", "Z046") |
                                substr(disease_3_code, 1, 4) %in% c("Y871", "Z914", "Z915", "Z004", "Z046") ) ~ 1, T ~0)) 

# Filtering based on conditions above and free text search terms
mh_aye %<>% 
  filter(def_yes == 1 |
           grepl(mh_aye_freetext, diagnosis_1_text) |
           grepl(mh_aye_freetext, diagnosis_2_text) |
           grepl(mh_aye_freetext, diagnosis_3_text) |
           grepl(mh_aye_freetext, presenting_complaint_text) )

# Excluding false positives
mh_aye %<>% 
  filter(!(def_yes == 0 & (
    grepl(mh_text_notincluded, presenting_complaint_text)|
      grepl(mh_text_notincluded, diagnosis_1_text)|
      grepl(mh_text_notincluded, diagnosis_2_text)|
      grepl(mh_text_notincluded, diagnosis_3_text))))

mh_aye %<>% #excluding very young kids as mostly false positives
  filter(pat_age>4) 

#Now another round excluding accidental poisonings, etc
mh_aye %<>% 
  # Formatting dataset
  rename(dep=prompt_dataset_deprivation_scot_quintile, age=pat_age,
         sex=pat_gender_description, count=number_of_attendances,
         hb=treatment_nhs_board_description_as_at_date_of_episode) %>%
  proper() %>% #fixing formatting of names
  mutate(area_type = "Health board",
         week_ending = ceiling_date(as.Date(arrival_date), "week", change_on_boundary = F),
         age_grp = as.character(case_when(between(age, 0, 17) ~ "5 - 17",
                                          between(age, 18, 44) ~ "18 - 44", 
                                          between(age, 45, 64) ~ "45 - 64", 
                                          between(age, 65, 200) ~ "65 and over", 
                                          T ~ "Missing"))) %>%
  create_depgroups() %>%
  group_by(week_ending, area_name, area_type,  age_grp, sex, dep) %>%
  summarise(count=sum(count, na.rm = T)) %>% #aggregating
  ungroup() 

# Generate scotland level dataset
mh_aye_scot <- mh_aye %>%
  group_by(week_ending, age_grp, sex, dep) %>%
  summarise(count=sum(count, na.rm = T)) %>%
  mutate(area_name="Scotland", area_type="Scotland") %>% ungroup()

# Joining together
mh_aye <- rbind(mh_aye, mh_aye_scot) %>% 
  rename(age=age_grp) %>%  mutate(week_ending=as.Date(week_ending,format="%d/%m/%Y")) 

#Use aggregation function to aggregate data files into format
mh_aye_all <- mh_aye %>% agg_cut(grouper=NULL) %>% mutate(type = "sex", category = "All")
mh_aye_sex <- agg_cut(dataset=mh_aye, grouper="sex") %>% rename(category=sex)
mh_aye_dep <- agg_cut(dataset=mh_aye, grouper="dep") %>% rename(category=dep)
mh_aye_age <- agg_cut(dataset=mh_aye, grouper="age") %>% rename(category=age)

# Add final aggregation files to one master file
mh_aye <- rbind(mh_aye_all, mh_aye_sex, mh_aye_dep, mh_aye_age) 

# Filtering out cuts with very small counts for HBS
mh_aye %<>% 
  filter(!(area_name %in% c("NHS Western Isles", "NHS Orkney", "NHS Shetland"))) %>% 
  filter(area_name == "Scotland" | category == "All")

prepare_final_data(mh_aye, "mh_A&E", last_week = "2021-04-25")

###############################################.
## OOH - mental health ----
###############################################.

mh_ooh <- read_tsv(paste0(data_folder, "GP_OOH_mh/2021-05-03-GP OOH MH WIDER IMPACT.txt")) %>%
  janitor::clean_names() %>%
  rename(hb=patient_nhs_board_description_current, 
         dep=patient_prompt_dataset_deprivation_scot_quintile,sex=gender_description,
         count=gp_ooh_number_of_cases, age_group=age_band, week_ending=gp_ooh_sc_start_date) %>%
  mutate(week_ending = as.Date(week_ending, format= "%d/%m/%Y"), 
         week_ending = ceiling_date(week_ending, "week", change_on_boundary = F),
         # Query excludes under 5s
         age = recode_factor(age_group, "0-12" = "5 - 17", "13 to 17" = "5 - 17",  
                             "18 to 24" = "18 - 44", "25 to 34" = "18 - 44", "35 to 44" = "18 - 44", "45 to 54" = "45 - 64", 
                             "55 to 64" = "45 - 64", "65 to 74" = "65 and over", "75 to 84" = "65 and over",
                             "85plus" = "65 and over"),
         sex = recode(sex, "MALE" = "Male", "FEMALE" = "Female", "0" = NA_character_, "9" = NA_character_),
         dep = recode(dep, 
                      "1" = "1 - most deprived", "2" = "2",  "3" = "3", 
                      "4" = "4", "5" = "5 - least deprived"),
         #week_ending = as.Date(week_ending, "%d/%m/%Y"), #formatting date
         scot = "Scotland") %>%
  proper() #convert HB names to correct format

# Aggregate up to get figures for each area type.
mh_ooh %<>% gather(area_type, area_name, c(area_name, scot)) %>% ungroup() %>% 
  mutate(area_type = recode(area_type, "area_name" = "Health board", 
                            "scot" = "Scotland")) %>% 
  # Aggregating to make it faster to work with
  group_by(week_ending, sex, dep, age, area_name, area_type) %>% 
  summarise(count = sum(count, na.rm = T))  %>% ungroup() 

mh_ooh_all <- mh_ooh %>% agg_cut(grouper=NULL) %>% mutate(type = "sex", category = "All")
mh_ooh_sex <- mh_ooh %>% agg_cut(grouper="sex") %>% rename(category = sex)
mh_ooh_dep <- mh_ooh %>% agg_cut(grouper="dep") %>% rename(category = dep)
mh_ooh_age <- mh_ooh %>% agg_cut(grouper="age") %>% rename(category = age)

mh_ooh <- rbind(mh_ooh_all, mh_ooh_sex, mh_ooh_dep, mh_ooh_age)

mh_ooh %<>% 
  filter(!(area_name %in% c("NHS Western Isles", "NHS Orkney", "NHS Shetland"))) %>% 
  filter(area_name == "Scotland" | category == "All")

prepare_final_data(mh_ooh, "mh_ooh", last_week = "2021-04-25")
