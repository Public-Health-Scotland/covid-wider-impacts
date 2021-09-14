##### SMR01 Unintentional Injuries data preparation for Wider Impacts app

###############################################.
## Functions/Packages/filepaths ----
###############################################.
source("data_prep/functions_packages_data_prep.R")
library(odbc)
library(tools)
library(tidyverse)
library(stringi)
library(lubridate)


smra_connect <- suppressWarnings(dbConnect(odbc(),  dsn="SMRA",
                                      uid=.rs.askForPassword("SMRA Username:"), 
                                      pwd=.rs.askForPassword("SMRA Password:")))

###############################################.
## Lookups ----
###############################################.

# SIMD quintile to datazone lookup
cl_out <- "/conf/linkage/output/lookups/Unicode/"
# import deprivation lookup
SIMD_lookup <- readRDS(paste0(cl_out,"Deprivation/postcode_2021_1_simd2020v2.rds")) %>% 
  clean_names() %>% 
  select(pc7, hb2019name, simd2020v2_sc_quintile) %>% 
  rename(dep = simd2020v2_sc_quintile)
# hscp lookup with datazone2011
hscp <- readRDS(paste0(cl_out, "Geography/Scottish Postcode Directory/Scottish_Postcode_Directory_2021_1.rds")) %>%
  clean_names() %>%
  select(pc7, hscp2019name) %>%
  rename(hscp = hscp2019name)
# Diagnoses lookup
# V01 - X59 categories
accident_a <- c(c("V01", "V02", "V03", "V04", "V05", "V06", "V07", "V08", "V09"), sprintf("V%d", (10:99)), 
                c("Y85", "Y86"))
accident_b <- c(c("W00", "W01", "W02", "W03", "W04", "W05", "W06", "W07", "W08", "W09"), sprintf("W%d", (10:99)),
                c("X00", "X01", "X02", "X03", "X04", "X05", "X06", "X07", "X08", "X09"), sprintf("X%d", (10:59)))
all_ui <- c(accident_a, accident_b)
poison_ui <- sprintf('X%d', 40:49)
falls_ui <- c(c("W00", "W01", "W02", "W03", "W04", "W05", "W06", "W07", "W08", "W09"), sprintf("W%d", (10:19)))
firearms_ui <- sprintf('W%d', 32:34)
cut_ui <- c(sprintf('W%d', 25:29), "W45")
struck_ui <- c(sprintf('W%d', 20:22), sprintf('W%d', 50:52))
crushing_ui <- "W23"
machinery_ui <- c("W24", "W30", "W31")
scalds_ui <- sprintf('X%d', 10:19)
accexp_ui <- c("X58", "X59")
other_ui <- c(c("V01", "V02", "V03", "V04", "V05", "V06", "V07", "V08", "V09"), sprintf("V%d", (10:99)),
              c("Y85", "Y86"), sprintf('W%d', 35:44), sprintf('W%d', 46:49), sprintf('W%d', 53:99),
              c("X00", "X01", "X02", "X03", "X04", "X05", "X06", "X07", "X08", "X09"), sprintf('X%d', 20:39),
              sprintf('X%d', 50:57))
# assault categories
all_assault_ui <- c(sprintf('X%d', 85:99), c("Y00", "Y01", "Y02", "Y03", "Y04", "Y05", "Y06", "Y07", "Y08", "Y09"))
other_assault_ui <- c(sprintf('X%d', 85:98), c("Y00", "Y01", "Y02", "Y03", "Y04", "Y05", "Y06", "Y07", "Y08", "Y09"))

###############################################.
## Extract UI_SMR01 data from SMRA ----
###############################################.
# Define SQL query
Query_SMR01 <- paste("SELECT LINK_NO, DR_POSTCODE, SEX, ADMISSION_DATE, ADMISSION_TYPE, DISCHARGE_DATE, MAIN_CONDITION,
                     OTHER_CONDITION_1, OTHER_CONDITION_2, OTHER_CONDITION_3, OTHER_CONDITION_4, OTHER_CONDITION_5,
                     HBRES_CURRENTDATE, AGE_IN_YEARS, CIS_MARKER, LENGTH_OF_STAY, INPATIENT_DAYCASE_IDENTIFIER,
                     COUNCIL_AREA_2019 FROM ANALYSIS.SMR01_PI WHERE INPATIENT_DAYCASE_IDENTIFIER = 'I' 
                     AND ADMISSION_TYPE >= 32 AND admission_type <= 35")

data_UI_SMR01 <- as_tibble(dbGetQuery(smra_connect, Query_SMR01)) %>%
  setNames(tolower(names(.))) %>% 
  filter(discharge_date >= lubridate::dmy(01012018),
         hbres_currentdate != "S27000001", hbres_currentdate != "S08100001", hbres_currentdate != "S08200001",
         hbres_currentdate != "S08200002", hbres_currentdate != "S08200003", hbres_currentdate != "S08200004") %>%
  rename(pc7=dr_postcode)  %>%
# Formatting variables
  mutate(week_ending = ceiling_date(as.Date(discharge_date), "week", change_on_boundary = F)) %>% 
  mutate(sex = recode(sex, "1" = "Male", "2" = "Female", "0" = NA_character_, "9" = NA_character_),
         hb = paste("NHS", match_area(hbres_currentdate), sep = " "),
         nca= match_area(council_area_2019),
         age = case_when(age_in_years >=0 & age_in_years <5 ~ "0-4",
                                age_in_years >=5 & age_in_years <12 ~ "5-11", age_in_years >=12 & age_in_years <18 ~ "12-17",
                                age_in_years >=18 & age_in_years <25 ~ "18-24", age_in_years >=25 & age_in_years <45 ~ "25-44",
                                age_in_years >=45 & age_in_years <65 ~ "45-64", age_in_years >=65 & age_in_years <80 ~ "65-79",
                                age_in_years >=80 ~ "80 and over"),
         year = year(week_ending)) %>% #to allow merging
         mutate_at(vars(7:12), ~replace_na(., "000ZZZ")) %>%
         mutate_at(vars(7:12),  .funs = list(diag = ~substr(.,1,3))) %>% 
         mutate_at(vars(7:12),  .funs = list(loc = ~substr(.,4,4))) %>% 
         rename_at(vars(24:29), funs(c("diag1", "diag2", "diag3", "diag4", "diag5", "diag6"))) %>% 
         rename_at(vars(30:35), funs(c("loc1", "loc2", "loc3", "loc4", "loc5", "loc6"))) %>%
         select(-c(7:12))
         ### - 3 - BASEFILE FOR UI ADMISSION TABLES ----
         
         # filter episodes where any UI was recorded in any diagnostic position
         # Transport accidents (V01-X59) - Sequelae of transport accidents (Y85) - Sequelae of other accidents (Y86) - RTA (admission_type = 32)
         Tab1 <- data_UI_SMR01 %>%
           mutate(all_diag = if_else(diag1 %in% all_ui | diag2 %in% all_ui | diag3  %in% all_ui |
                                       diag4 %in% all_ui | diag5 %in% all_ui |diag6 %in% all_ui |
                                       admission_type == "32",1,0)) %>%
           filter(all_diag ==1)
         
         # flag up cause of injury for 'Other external causes of accidental injury' (W00-X59) and admission type between 33 and 35 
         # each episode could have more than 1 type of injury. Count separately.
         ui_codes <- function(codes) {
           df <- Tab1
           output <- if_else ((df$diag1 %in% codes | df$diag2 %in% codes | df$diag3 %in% codes |
                                 df$diag4 %in% codes | df$diag5 %in% codes | df$diag6 %in% codes) 
                              & (df$admission_type >= "33" & df$admission_type <= "35"),1,0)
         }
         
         Tab1 <- Tab1 %>% 
           mutate(poison = ui_codes(poison_ui),
                  falls = ui_codes(falls_ui),
                  firearms = ui_codes(firearms_ui),
                  cut = ui_codes(cut_ui),
                  struck = ui_codes(struck_ui),
                  crushing = ui_codes(crushing_ui),
                  machinery = ui_codes(machinery_ui),
                  scalds = ui_codes(scalds_ui),
                  accexp = ui_codes(accexp_ui),
                  rta = if_else(admission_type ==32, 1,0),
                  other = if_else ((diag1 %in% other_ui | diag2 %in% other_ui | diag3 %in% other_ui | diag4 %in% other_ui |
                                      diag5 %in% other_ui | diag6 %in% other_ui) & admission_type != "32",1,0)) 
         
         # recode location to home, Undisclosed and Other according to ICD10 chapter XX - this assignes location also to admissions that are not relevant for convinience
         ui_loc <- function(loc) {
           df <- Tab1
           output <- if_else(loc !=0 & loc != 9,"Other",
                             if_else(loc ==9,"Undisclosed",
                                     if_else(loc ==0,"Home", "0")))
           
         }
         
         Tab1 <- Tab1 %>% 
           mutate(loc1 = ui_loc(loc1),
                  loc2 = ui_loc(loc2),
                  loc3 = ui_loc(loc3),
                  loc4 = ui_loc(loc4),
                  loc5 = ui_loc(loc5),
                  loc6 = ui_loc(loc6))
         
# for diagnosis of 'Other external causes of accidental injury'(W00 to X59), with admission type between 33 and 35
# identify the place of occurance, according to ICD10. RTA are excluded
Tab1 <- Tab1 %>%
           mutate(diagwithloc1 = if_else (diag1 %in% accident_b & (admission_type >= "33" & admission_type <= "35"), loc1,
                                          if_else (diag1 %in% accident_a, "NA", "0")),
                  diagwithloc2 = if_else (diag2 %in% accident_b & (admission_type >= "33" & admission_type <= "35"), loc2,
                                          if_else (diag2 %in% accident_a, "NA", "0")),
                  diagwithloc3 = if_else (diag3 %in% accident_b & (admission_type >= "33" & admission_type <= "35"), loc3,
                                          if_else (diag3 %in% accident_a, "NA", "0")),
                  diagwithloc4 = if_else (diag4 %in% accident_b & (admission_type >= "33" & admission_type <= "35"), loc4,
                                          if_else (diag4 %in% accident_a, "NA", "0")),
                  diagwithloc5 = if_else (diag5 %in% accident_b & (admission_type >= "33" & admission_type <= "35"), loc5,
                                          if_else (diag5 %in% accident_a, "NA", "0")),
                  diagwithloc6 = if_else (diag6 %in% accident_b & (admission_type >= "33" & admission_type <= "35"), loc6,
                                          if_else (diag6 %in% accident_a, "NA", "0")))
         
         # if more than 1 UI is recorded per episode, the location is taken from the injury closer to the primary position
         Tab1 <- Tab1 %>%
           mutate(diagwithloc = if_else(diagwithloc1 !="0", diagwithloc1,
                                        if_else(diagwithloc2 !="0", diagwithloc2,
                                                if_else(diagwithloc3 !="0", diagwithloc3,
                                                        if_else(diagwithloc4 !="0", diagwithloc4,
                                                                if_else(diagwithloc5 !="0", diagwithloc5,
                                                                        if_else(diagwithloc6 !="0", diagwithloc6, "0")))))))
         
         # if RTA but also poison, falls, struck, crushing, scalds and accepx, fix location to undisclosed instead of NA
         Tab1 <- Tab1 %>%
           mutate(injurylocation = if_else(admission_type == "32", "NA",
                                           if_else (diagwithloc =="NA" & (poison ==1 | falls ==1 | struck ==1 | crushing ==1 | scalds ==1 | accexp ==1), "Undisclosed", diagwithloc))) 
         
# newother flags if episode is other, firearms, cut or machinery
         Tab1 <- Tab1 %>%
           mutate(newother = if_else(other ==1 | firearms ==1 | cut ==1 | machinery ==1, 1,0))
         
# basefile for all tables excluding assaults
 #saveRDS(Tab1, (here::here("data","ui_analysis_Tab1.rds")))
         

### check that last week is a complete week. complete weeks have 6 or 7 days of data in them. 
# There could be more recent data available in SMR than what we want
#as.integer(Tab1 %>% 
    #filter( week_ending==max(week_ending)) %>%
    #group_by(discharge_date) %>%
    #summarise() %>% ungroup() %>% count())

# Match on SIMD quintiles & hscp from the postcode files
Tab1 <- left_join(Tab1, SIMD_lookup, by='pc7')
Tab1 <- left_join(Tab1, hscp, by='pc7')
#Pivoting so one row per diagnosis grouping
Tab2 <- Tab1 %>%
  mutate(month_ending = floor_date(as.Date(discharge_date), "month")) %>% 
  relocate(newother,before=diagwithloc1) %>%
  select(year,month_ending,sex,age,hb,hscp,injurylocation,dep,all_diag:other,newother) %>%
  pivot_longer(cols = all_diag:newother, names_to = "class", values_to = "admissions") %>%
  filter(class!="machinery" & class!="firearms" & class!="cut" & class!="other" & class!="struck" &
           class!="crushing" & class!="scalds" & class!="accexp" ) %>%
  group_by(year,month_ending,sex,age,hb,hscp,injurylocation,dep,class) %>%
  summarise(admissions= sum(admissions))  %>%
  mutate(class = recode(class, "all_diag" = "all unintentional injuries",
                  "rta" = "road transport accidents",
                  "poison" = "poisoning",
                  "falls" = "falls",
                  "newother" = "other"))

  #Pivoting so one row per area
Tab3 <- Tab2 %>%
  mutate(scot = "Scotland") %>%
  pivot_longer(cols = c(hb, hscp, scot), names_to="area_type", values_to="area_name") %>% 
  #filtering out NA duplicates (which are counted in Scotland totals, but not elsewhere)
  #filter(!is.na(value)) %>% 
  # More formatting
  mutate(dep = recode(dep,"1" = "1 - most deprived", "2" = "2",  "3" = "3", 
                      "4" = "4", "5" = "5 - least deprived"),
         area_type = recode(area_type, "hb" = "Health board", 
                            "hscp" = "HSC partnership", "scot" = "Scotland")) %>% 
  # Aggregating to make it faster to work with
  mutate(all="All") %>%
  group_by(year,month_ending,all,sex, dep, age, injurylocation, area_name, area_type, class) %>% 
  summarise(admissions= sum(admissions)) %>% ungroup()

#Pivoting so one row per area
Tab4 <- Tab3 %>%
  pivot_longer(cols = c(all,sex, dep, age, injurylocation), names_to="type", values_to="category") %>% 
  #filter(class=="all unintentional injuries") %>%
  # More formatting
  group_by(year,area_type, category, type,month_ending, area_name, class) %>%
  filter(!(area_type != "Scotland" & area_type != "Health board" & type == "dep")) %>% #SIMD only at Scotland level
  summarise(count= sum(admissions)) %>% ungroup()

#Creating a table for each diagnosis grouping
ui_all <- Tab4 %>%
  filter(class=="all unintentional injuries" & category!='NA') %>%
  group_by(area_type, category, type, month_ending, area_name) %>%
  #filter((area_name !="Shetland Islands" & area_name !="NHS Shetland" & area_name !="NHS Orkney" & area_name !="Orkney Islands")) %>%
  summarise(count= sum(count)) %>% ungroup()

ui_rta <- Tab4 %>%
  filter(class=="road transport accidents") %>%
  group_by(area_type, category, type, month_ending, area_name) %>%
  #filter((area_name !="Shetland Islands" & area_name !="NHS Shetland" & area_name !="NHS Orkney" & area_name !="Orkney Islands")) %>%
  summarise(count= sum(count)) %>% ungroup()

ui_poison <- Tab4 %>%
  filter(class=="poisoning") %>%
  group_by(area_type, category, type, month_ending, area_name) %>% 
  #filter((area_name !="Shetland Islands" & area_name !="NHS Shetland" & area_name !="NHS Orkney" & area_name !="Orkney Islands")) %>%
  summarise(count= sum(count)) %>% ungroup()

ui_falls <- Tab4 %>%
  filter(class=="falls") %>%
  group_by(area_type, category, type, month_ending, area_name) %>%
  #filter((area_name !="Shetland Islands" & area_name !="NHS Shetland" & area_name !="NHS Orkney" & area_name !="Orkney Islands")) %>%
  summarise(count= sum(count)) %>% ungroup()

ui_other <- Tab4 %>%
  filter(class=="other") %>%
  group_by(area_type, category, type, month_ending, area_name) %>% 
  #filter((area_name !="Shetland Islands" & area_name !="NHS Shetland" & area_name !="NHS Orkney" & area_name !="Orkney Islands")) %>%
  summarise(count= sum(count)) %>% ungroup()


# Running final functions
prepare_final_data_m(dataset = ui_all, filename = "ui_smr01_all", 
                   last_month = "2021-05-01", aver = 3)

# Dealing with variation to replicate previous output. 
# This might not be needed in future if we set a standard way of dealing with this.
final_UI_SMR01 <- final_data %>%
  rename (week_ending=month_ending) %>%
  mutate(variation = round(-1 * ((count_average - count)/count_average * 100), 1),
        # Dealing with infinite values from historic average = 0
        variation =  case_when(count_average == 0 & count == 0 ~ 0, T ~ variation),
        variation =  ifelse(is.infinite(variation), NA_integer_, variation))
       
#save weekly data        
saveRDS(final_UI_SMR01, paste0("shiny_app/data/ui_smr01_all.rds"))
saveRDS(final_UI_SMR01, paste0(data_folder,"final_app_files/ui_smr01_all_", 
                                format(Sys.Date(), format = '%d_%b_%y'), ".rds"))
saveRDS(final_UI_SMR01, paste0(open_data, "ui_smr01_data.rds"))

#filter only on rta
# Running final functions
prepare_final_data_m(dataset = ui_rta, filename = "ui_smr01_rta", 
                   last_month = "2021-05-01", aver = 3)

final_rta_SMR01 <- final_data %>% 
  rename (week_ending=month_ending) %>%
  mutate(variation = round(-1 * ((count_average - count)/count_average * 100), 1),
         # Dealing with infinite values from historic average = 0
         variation =  case_when(count_average == 0 & count == 0 ~ 0, T ~ variation),
         variation =  ifelse(is.infinite(variation), NA_integer_, variation))


saveRDS(final_rta_SMR01, paste0("shiny_app/data/ui_smr01_rta.rds"))
saveRDS(final_rta_SMR01, paste0(data_folder,"final_app_files/ui_smr01_rta_", 
                               format(Sys.Date(), format = '%d_%b_%y'), ".rds"))
saveRDS(final_rta_SMR01, paste0(open_data, "ui_smr01_rta.rds"))

#filter only on poison
# Running final functions
prepare_final_data_m(dataset = ui_poison, filename = "ui_smr01_poison", 
                   last_month = "2021-05-01", aver = 3)

final_poison_SMR01 <- final_data %>% 
  rename (week_ending=month_ending) %>%
  mutate(variation = round(-1 * ((count_average - count)/count_average * 100), 1),
         # Dealing with infinite values from historic average = 0
         variation =  case_when(count_average == 0 & count == 0 ~ 0, T ~ variation),
         variation =  ifelse(is.infinite(variation), NA_integer_, variation))


saveRDS(final_poison_SMR01, paste0("shiny_app/data/ui_smr01_poison.rds"))
saveRDS(final_poison_SMR01, paste0(data_folder,"final_app_files/ui_smr01_poison_", 
                                format(Sys.Date(), format = '%d_%b_%y'), ".rds"))
saveRDS(final_poison_SMR01, paste0(open_data, "ui_smr01_poison.rds"))

#filter only on falls
# Running final functions
prepare_final_data_m(dataset = ui_falls, filename = "ui_smr01_falls", 
                   last_month = "2021-05-01", aver = 3)

final_falls_SMR01 <- final_data %>% 
  rename (week_ending=month_ending) %>%
  mutate(variation = round(-1 * ((count_average - count)/count_average * 100), 1),
         # Dealing with infinite values from historic average = 0
         variation =  case_when(count_average == 0 & count == 0 ~ 0, T ~ variation),
         variation =  ifelse(is.infinite(variation), NA_integer_, variation))


saveRDS(final_falls_SMR01, paste0("shiny_app/data/ui_smr01_falls.rds"))
saveRDS(final_falls_SMR01, paste0(data_folder,"final_app_files/ui_smr01_falls_", 
                                   format(Sys.Date(), format = '%d_%b_%y'), ".rds"))
saveRDS(final_falls_SMR01, paste0(open_data, "ui_smr01_falls.rds"))

#filter only on other
# Running final functions
prepare_final_data_m(dataset = ui_other, filename = "ui_smr01_other", 
                   last_month = "2021-05-01", aver = 3)

final_other_SMR01 <- final_data %>% 
  rename (week_ending=month_ending) %>%
  mutate(variation = round(-1 * ((count_average - count)/count_average * 100), 1),
         # Dealing with infinite values from historic average = 0
         variation =  case_when(count_average == 0 & count == 0 ~ 0, T ~ variation),
         variation =  ifelse(is.infinite(variation), NA_integer_, variation))


saveRDS(final_other_SMR01, paste0("shiny_app/data/ui_smr01_other.rds"))
saveRDS(final_other_SMR01, paste0(data_folder,"final_app_files/ui_smr01_other_", 
                                  format(Sys.Date(), format = '%d_%b_%y'), ".rds"))
saveRDS(final_other_SMR01, paste0(open_data, "ui_smr01_other.rds"))


# only assault with admission type =32, each assault category counted separately, but the total include only 1 assault per episode
ui_assault <- data_UI_SMR01 %>%
  mutate(month_ending = floor_date(as.Date(discharge_date), "month")) %>% 
  mutate(assault_all = if_else(diag1 %in% all_assault_ui | diag2 %in% all_assault_ui | diag3  %in% all_assault_ui |
                                 diag4 %in% all_assault_ui | diag5 %in% all_assault_ui |diag6 %in% all_assault_ui,1,0)) %>%
  filter(assault_all ==1) %>%
  filter(admission_type != 32) %>%
  mutate(assault_all = 1,
         knife = if_else (diag1 =="X99" | diag2 =="X99" | diag3 =="X99" | diag4 =="X99" | diag5 =="X99" | diag6 =="X99",1,0),
         other_assault = if_else (diag1 %in% other_assault_ui | diag2 %in% other_assault_ui | diag3 %in% other_assault_ui | 
                                    diag4 %in% other_assault_ui | diag5 %in% other_assault_ui | diag6 %in% other_assault_ui,1,0),
         qrt= quarter(discharge_date)) %>%
  mutate(all="All",area_type="Scotland", area_name="Scotland") %>%
  pivot_longer(cols = c(all,sex, dep, age, injurylocation), names_to="type", values_to="category") %>%
  group_by(area_type, category, type, month_ending, area_name) %>%
  summarise_at(c("assault_all"), sum) %>%
  rename(count=assault_all)  %>% ungroup()
  
#filter only on assault
# Running final functions
prepare_final_data_m(dataset = ui_assault, filename = "ui_smr01_assaults", 
                   last_month = "2021-05-01", aver = 3)

final_assault_SMR01 <- final_data %>% 
  rename (week_ending=month_ending) %>%
  mutate(variation = round(-1 * ((count_average - count)/count_average * 100), 1),
         # Dealing with infinite values from historic average = 0
         variation =  case_when(count_average == 0 & count == 0 ~ 0, T ~ variation),
         variation =  ifelse(is.infinite(variation), NA_integer_, variation))


saveRDS(final_assault_SMR01, paste0("shiny_app/data/ui_smr01_assaults.rds"))
saveRDS(final_assault_SMR01, paste0(data_folder,"final_app_files/ui_smr01_assaults_", 
                                format(Sys.Date(), format = '%d_%b_%y'), ".rds"))
saveRDS(final_assault_SMR01, paste0(open_data, "ui_smr01_assaults.rds"))

### END
