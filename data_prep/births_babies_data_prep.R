# Data preparation for births and babies tab

###############################################.
## Functions/Packages/filepaths/lookups ----
###############################################.
source("data_prep/functions_packages_data_prep.R")

###############################################.
## Mode of delivery ----
###############################################.

create_delivery <- function(folderdate) {
  #field with date data files prepared
  delivery_folder <- gsub("-", "", folderdate)
  delivery_date <- folderdate
  
  ##mode of delivery data supplied in 4 files: runchart data, line charts for scotland (age and dep split), line charts for NHS board and data download
  
  ## 1-RUNCHART DATA
  ## mod data for run chart (scotland and nhs board) - monthly
  mod_runchart <- readRDS(paste0(data_folder, "pregnancy/mode_of_delivery/",delivery_folder,"/WI_DELIVERIES_RUNCHART_mode_",delivery_date,".rds")) %>%  
    janitor::clean_names() %>%
    rename(area_name = hbres, month = date) %>%
    mutate(month = as.Date(month),
           type = case_when(substr(area_name,1,3)=="NHS" ~ "Health board",
                            area_name=="Scotland" ~ "Scotland"),
           area_type = type, 
           category = "All") %>%
    # the median column is used to assess shifts or trends - dataset contains NA cells which need to filled
    # ext_ columns are extended median which are blank before projection time period
    mutate(ext_csection_all = case_when(is.na(ext_csection_all) ~ median_csection_all,
                                        TRUE ~ ext_csection_all),
           ext_csection_elec = case_when(is.na(ext_csection_elec) ~ median_csection_elec,
                                         TRUE ~ ext_csection_elec),
           ext_csection_emer = case_when(is.na(ext_csection_emer) ~ median_csection_emer,
                                         TRUE ~ ext_csection_emer)) %>% 
    group_by(area_name, area_type, type) %>%   #sort data to ensure trends/shifts compare correct data points
    #call function to add flags for runchart shifts and trends
    #shift: name for new field where shift is flagged
    #trend: name for new field where trend is flagged
    #value: which column in dataset contains value being evaluated
    #median: which column in dataset contains the median against which value is tested
    runchart_flags(shift="csection_all_shift", trend="csection_all_trend", 
                   value=perc_csection_all, median=ext_csection_all) %>%
    runchart_flags(shift="csection_emer_shift", trend="csection_emer_trend", 
                   value=perc_csection_emer, median=ext_csection_emer) %>%
    runchart_flags(shift="csection_elec_shift", trend="csection_elec_trend", 
                   value=perc_csection_elec, median=ext_csection_elec) %>%
    ungroup()
  
  saveRDS(mod_runchart, "shiny_app/data/mod_runchart_data.rds")
  saveRDS(mod_runchart, paste0(data_folder,"final_app_files/mod_runchart_data_", 
                               format(Sys.Date(), format = '%d_%b_%y'), ".rds"))
  
  print("File mod_runchart_data.rds produced and saved")
  
  mod_runchart <<- mod_runchart
  
  ## 2- LINECHART DATA mode of delivery for Scotland only by age and dep
  mod_scot <- readRDS(paste0(data_folder, "pregnancy/mode_of_delivery/",delivery_folder,"/WI_DELIVERIES_SCOT_CHARTS_mode_",delivery_date,".rds")) %>%  
    janitor::clean_names() %>%
    rename(area_name=hbres, month=date, category=variable) %>%
    mutate(month=as.Date(month),
           area_type="Scotland",
           type=case_when(subgroup=="AGEGRP" ~ "age",subgroup=="SIMD" ~ "dep"),
           category=as.character(category))
  
  saveRDS(mod_scot, "shiny_app/data/mod_scot_data.rds")
  saveRDS(mod_scot, paste0(data_folder,"final_app_files/mod_scot_data_", 
                           format(Sys.Date(), format = '%d_%b_%y'), ".rds"))
  
  print("File mod_scot_data.rds produced and saved")
  
  ## 3- LINECHART DATA mode of delivery for Scotland & NHS board
  mod_linechart <- readRDS(paste0(data_folder, "pregnancy/mode_of_delivery/",delivery_folder,"/WI_DELIVERIES_LINECHART_mode_",delivery_date,".rds")) %>%  
    janitor::clean_names() %>%
    rename(area_name=hbres, month=date) %>%
    mutate(month=as.Date(month, format="%Y-%m-%d "),
           #month=format(month,"%b %Y"),
           type=case_when(substr(area_name,1,3)=="NHS" ~ "Health board",
                          area_name=="Scotland" ~ "Scotland", TRUE ~ "Other"),
           area_type = type, 
           category="All") %>%
    group_by(area_name, month) %>% 
    mutate(tot_births=sum(births/2), # divide by two because total births already a row in the dataset
           percent_births=(births/tot_births)*100) %>% 
    ungroup()
  
  mod_linechart <- mod_linechart %>%
    mutate(mode = recode(mode, "Spontaneous" = "Spontaneous vaginal", "Assisted" = "Assisted vaginal", "Caesarean - Emergency" = "Emergency caesarean",
                         "Caesarean - Elective" = "Elective caesarean"))
  
  saveRDS(mod_linechart, "shiny_app/data/mod_linechart_data.rds") 
  saveRDS(mod_linechart, paste0(data_folder,"final_app_files/mod_linechart_data_", 
                                format(Sys.Date(), format = '%d_%b_%y'), ".rds"))
  
  print("File mod_linechart_data.rds produced and saved")  
  
  ## 4- Mode of delivery DATA DOWNLOAD FILE FOR SHINY APP
  mod_download <- read_csv(paste0(data_folder, "pregnancy/mode_of_delivery/",delivery_folder,"/WI_DELIVERIES_DOWNLOAD_mode_",delivery_date,".csv"))%>%  
    janitor::clean_names() %>%
    mutate(month_of_discharge=as.Date(month_of_discharge,format="%Y-%m-%d"),
           month_of_discharge=format(month_of_discharge,"%b %Y")) %>%
    rename(area_name=nhs_board_of_residence,
           centreline_csection_all = median_csection_all,
           centreline_csection_emer = median_csection_emer,
           centreline_csection_elec = median_csection_elec,
           dottedline_csection_all = ext_csection_all,
           dottedline_csection_emer = ext_csection_emer,
           dottedline_csection_elec = ext_csection_elec) %>% 
    mutate(area_type=case_when(substr(area_name,1,3)=="NHS" ~ "Health board",
                               area_name=="Scotland" ~ "Scotland"),
           chart_category="All",
           chart_type= area_type)
  
  saveRDS(mod_download, "shiny_app/data/mod_download_data.rds")  
  saveRDS(mod_download, paste0(data_folder,"final_app_files/mod_download_data_", 
                               format(Sys.Date(), format = '%d_%b_%y'), ".rds"))
  
  print("File mod_download_data.rds produced and saved")
  
  # Saving data for open data platform
  mod_download <- mod_download %>% 
    select(-c(chart_type, chart_category, dottedline_csection_all, dottedline_csection_elec,
              dottedline_csection_emer, indicator, centreline_csection_all, centreline_csection_elec,
              centreline_csection_emer, perc_denominator)) %>% 
    rename("Number of births - All births" = births_all, 
           "Number of births - Caesarean section" = csection_all,
           "Number of births - emergency Caesarean section" = csection_emer,
           "Number of births - elective Caesarean section" = csection_elec,
           "Number of births - Other/Not Known" = other_not_known,
           "Number of births - assisted vaginal delivery including breech" = assisted_vaginal_inc_breech, 
           "Number of births - spontaneous_vaginal_delivery" = spontaneous_vaginal,
           "Percentage (%) of births - assisted vaginal delivery including breech" = perc_assisted_vaginal_inc_breech, 
           "Percentage (%) of births - Caesarean section" = perc_csection_all,
           "Percentage (%) of births - emergency Caesarean section" = perc_csection_emer,
           "Percentage (%) of births - elective Caesarean section" = perc_csection_elec,
           "Percentage (%) of births - spontaneous vaginal delivery" = perc_spontaneous_vaginal,
           "Percentage (%) of births - other/not known" = perc_other_not_known) %>% 
    mutate(variable = case_when(variable %in% c("20-24", "25-29", "30-34", "35-39", 
                                                "40 and over", "Under 20", "1 - most deprived", "2", "3", "4", 
                                                "5 - least deprived", "Unknown") ~ paste0(variable),
                                TRUE ~ "All")) %>% 
    mutate(subgroup = case_when(subgroup %in% c("SIMD", "AGEGRP") ~ paste0(subgroup),
                                TRUE ~ "All"))
  
  file.remove(paste0(open_data,"method_delivery.rds")) #to avoid permission issues
  
  saveRDS(mod_download, paste0(open_data,"method_delivery.rds"))
  
  print("File for open data team produced and saved")
  
  
  ###############################################.
  ## Inductions ----
  ###############################################.
  
  ## 1-RUNCHART DATA
  ## mod data for run chart (scotland and nhs board) - monthly
  induct_runchart <- readRDS(paste0(data_folder, "pregnancy/inductions/",delivery_folder,"/WI_DELIVERIES_RUNCHART_induced_",delivery_date,".rds")) %>%
    janitor::clean_names() %>%
    rename(area_name = hbres, month = date) %>%
    mutate(month = as.Date(month),
           type = case_when(substr(area_name,1,3)=="NHS" ~ "Health board",
                            area_name=="Scotland" ~ "Scotland"),
           area_type = type,
           category = "All") %>%
    # the median column is used to assess shifts or trends - dataset contains NA cells which need to filled
    # ext_ columns are extended median which are blank before projection time period
    mutate(ext_ind_37_42 = case_when(is.na(ext_ind_37_42) ~ median_ind_37_42,
                                     TRUE ~ ext_ind_37_42)) %>%
    group_by(area_name, area_type, type) %>%   #sort data to ensure trends/shifts compare correct data points
    #call function to add flags for runchart shifts and trends
    #shift: name for new field where shift is flagged
    #trend: name for new field where trend is flagged
    #value: which column in dataset contains value being evaluated
    #median: which column in dataset contains the median against which value is tested
    runchart_flags(shift="induction_shift", trend="induction_trend",
                   value=perc_ind_37_42, median=ext_ind_37_42) %>%
    ungroup()
  
  saveRDS(induct_runchart, "shiny_app/data/induct_runchart_data.rds")
  saveRDS(induct_runchart, paste0(data_folder,"final_app_files/induct_runchart_data_",
                                  format(Sys.Date(), format = '%d_%b_%y'), ".rds"))
  
  print("File induct_runchart_data.rds produced and saved")
  
  induct_runchart <<- induct_runchart
  
  ## 2- LINECHART DATA inductions for Scotland only by age and dep
  induct_scot <- readRDS(paste0(data_folder, "pregnancy/inductions/",delivery_folder,"/WI_DELIVERIES_SCOT_CHARTS_induced_",delivery_date,".rds")) %>%
    janitor::clean_names() %>%
    rename(area_name=hbres, month=date, category=variable) %>%
    mutate(month=as.Date(month),
           area_type="Scotland",
           type=case_when(subgroup=="AGEGRP" ~ "age",subgroup=="SIMD" ~ "dep"),
           category=as.character(category))
  
  saveRDS(induct_scot, "shiny_app/data/induct_scot_data.rds")
  saveRDS(induct_scot, paste0(data_folder,"final_app_files/induct_scot_data_",
                              format(Sys.Date(), format = '%d_%b_%y'), ".rds"))
  
  print("File induct_scot_data.rds produced and saved")
  
  ## 3- LINECHART DATA inductions for Scotland & NHS board
  induct_linechart <- readRDS(paste0(data_folder, "pregnancy/inductions/",delivery_folder,"/WI_DELIVERIES_LINECHART_induced_",delivery_date,".rds")) %>%
    janitor::clean_names() %>%
    mutate(tot_births_37_42=births_37_42) %>%
    #reshape data file for ease of creation of line chart with percentages
    pivot_longer(cols = ind_37_42:births_37_42, names_to = "ind",values_to = "births") %>%
    rename(area_name=hbres, month=date) %>%
    mutate(month=as.Date(month, format="%Y-%m-%d "),
           type=case_when(substr(area_name,1,3)=="NHS" ~ "Health board",
                          area_name=="Scotland" ~ "Scotland", TRUE ~ "Other"),
           area_type = type,
           category="All",
           percent_births=((births/tot_births_37_42)*100),
           #NOTE the gestation categories are not mutually exclusive - <37 contains <32
           ind=case_when(ind=="ind_37_42" ~ "Births that followed induction",
                         ind=="births_37_42" ~ "All births",
                         TRUE~as.character(ind)))
  
  saveRDS(induct_linechart, "shiny_app/data/induct_linechart_data.rds")
  saveRDS(induct_linechart, paste0(data_folder,"final_app_files/induct_linechart_data_",
                                   format(Sys.Date(), format = '%d_%b_%y'), ".rds"))
  
  print("File induct_linechart_data.rds produced and saved")
  
  
  ## 4- Mode of delivery DATA DOWNLOAD FILE FOR SHINY APP
  induct_download <- read_csv(paste0(data_folder, "pregnancy/inductions/",delivery_folder,"/WI_DELIVERIES_DOWNLOAD_induced_",delivery_date,".csv"))%>%
    janitor::clean_names() %>%
    mutate(month_of_discharge=as.Date(month_of_discharge,format="%Y-%m-%d"),
           month_of_discharge=format(month_of_discharge,"%b %Y")) %>%
    rename(area_name=nhs_board_of_residence,
           centreline_induced_37_42 = median_induced_37_42,
           dottedline_induced_37_42 = ext_induced_37_42) %>%
    mutate(area_type=case_when(substr(area_name,1,3)=="NHS" ~ "Health board",
                               area_name=="Scotland" ~ "Scotland"),
           chart_category="All",
           chart_type= area_type)
  
  saveRDS(induct_download, "shiny_app/data/induct_download_data.rds")
  saveRDS(induct_download, paste0(data_folder,"final_app_files/induct_download_data_",
                                  format(Sys.Date(), format = '%d_%b_%y'), ".rds"))
  
  print("File induct_download_data.rds produced and saved")
  
  
  # Saving data for open data platform
  induct_download <- induct_download %>%
    select(area_name, area_type, month_of_discharge, subgroup, variable,
           induced_37_42, not_induced_37_42, unknown_induced_37_42, births_37_42,
           perc_induced_37_42, perc_not_induced_37_42, perc_unknown_induced_37_42) %>%
    rename("Total number of singleton live births at 37-42 weeks gestation" = births_37_42,
           "Number of singleton live births at 37-42 weeks gestation that followed induction of labour" = induced_37_42,
           "Number of singleton live births at 37-42 weeks gestation that were not induced" = not_induced_37_42,
           "Number of singleton live births at 37-42 weeks gestation unknown" = unknown_induced_37_42,
           "Percentage (%) of singleton live births at 37-42 weeks gestation that followed induction of labour" = perc_induced_37_42,
           "Percentage (%) of singleton live births at 37-42 weeks gestation that were not induced" = perc_not_induced_37_42,
           "Percentage (%) of singleton live births at 37-42 weeks gestation unknown" = perc_unknown_induced_37_42) %>%
    mutate(variable = case_when(variable %in% c("20-24", "25-29", "30-34", "35-39",
                                                "40 and over", "Under 20", "1 - most deprived", "2", "3", "4",
                                                "5 - least deprived", "Unknown") ~ paste0(variable),
                                TRUE ~ "All")) %>%
    mutate(subgroup = case_when(subgroup %in% c("SIMD", "AGEGRP") ~ paste0(subgroup),
                                TRUE ~ "All"))
  
  file.remove(paste0(open_data,"induction_labour.rds")) #to avoid permission issues
  saveRDS(induct_download, paste0(open_data,"induction_labour.rds"))
  
  print("Open data file produced and saved")
  
  
  ###############################################.
  ## Gestation at delivery ----
  ###############################################.
  
  ## 1-RUNCHART DATA
  gestation_runchart <- readRDS(paste0(data_folder,"pregnancy/gestation_at_delivery/",delivery_folder,"/WI_DELIVERIES_RUNCHART_gestation_",delivery_date,".rds")) %>%
    janitor::clean_names() %>%
    rename(area_name = hbres, month = date) %>%
    mutate(month = as.Date(month),
           type = case_when(substr(area_name,1,3)=="NHS" ~ "Health board",
                            area_name=="Scotland" ~ "Scotland"),
           area_type = type,
           category = "All") %>%
    # the median column is used to assess shifts or trends - dataset contains NA cells which need to filled
    # ext_ columns are extended median which are blank before projection time period
    mutate(ext_under32 = case_when(is.na(ext_under32) ~ median_under32,
                                   TRUE ~ ext_under32),
           ext_under37 = case_when(is.na(ext_under37) ~ median_under37,
                                   TRUE ~ ext_under37),
           ext_32_36 = case_when(is.na(ext_32_36) ~ median_32_36,
                                 TRUE ~ ext_32_36),
           ext_42plus = case_when(is.na(ext_42plus) ~ median_42plus,
                                  TRUE ~ ext_42plus)) %>%
    group_by(area_name, area_type, type) %>%   #sort data to ensure trends/shifts compare correct data points
    #call function to add flags for runchart shifts and trends
    #shift: name for new field where shift is flagged
    #trend: name for new field where trend is flagged
    #value: which column in dataset contains value being evaluated
    #median: which column in dataset contains the median against which value is tested
    runchart_flags(shift="gest_under32_shift", trend="gest_under32_trend",
                   value=perc_under32, median=ext_under32) %>%
    runchart_flags(shift="gest_under37_shift", trend="gest_under37_trend",
                   value=perc_under37, median=ext_under37) %>%
    runchart_flags(shift="gest_32_36_shift", trend="gest_32_36_trend",
                   value=perc_32_36, median=ext_32_36) %>%
    runchart_flags(shift="gest_42plus_shift", trend="gest_42plus_trend",
                   value=perc_42plus, median=ext_42plus) %>%
    ungroup()
  
  saveRDS(gestation_runchart, "shiny_app/data/gestation_runchart_data.rds")
  saveRDS(gestation_runchart, paste0(data_folder,"final_app_files/gestation_runchart_data_",
                                     format(Sys.Date(), format = '%d_%b_%y'), ".rds"))
  
  print("File gestation_runchart_data.rds produced and saved")
  gestation_runchart <<- gestation_runchart
  
  ## 2- LINECHART DATA inductions for Scotland only by age and dep
  gestation_scot <- readRDS(paste0(data_folder, "pregnancy/gestation_at_delivery/",delivery_folder,"/WI_DELIVERIES_SCOT_CHARTS_gestation_",delivery_date,".rds")) %>%
    janitor::clean_names() %>%
    rename(area_name=hbres, month=date, category=variable) %>%
    mutate(month=as.Date(month),
           area_type="Scotland",
           type=case_when(subgroup=="AGEGRP" ~ "age",subgroup=="SIMD" ~ "dep"),
           category=as.character(category))
  
  saveRDS(gestation_scot, "shiny_app/data/gestation_scot_data.rds")
  saveRDS(gestation_scot, paste0(data_folder,"final_app_files/gestation_scot_data_",
                                 format(Sys.Date(), format = '%d_%b_%y'), ".rds"))
  
  print("File gestation_scot_data.rds produced and saved")
  
  
  ## 3- LINECHART DATA gestation for Scotland & NHS board
  gestation_linechart <- readRDS(paste0(data_folder, "pregnancy/gestation_at_delivery/",delivery_folder,"/WI_DELIVERIES_LINECHART_gestation_",delivery_date,".rds")) %>%
    janitor::clean_names() %>%
    mutate(tot_births_18_44=births_18_44) %>%
    #reshape data file for ease of creation of line chart with percentages
    pivot_longer(cols = births_under32:births_18_44, names_to = "gest",values_to = "births") %>%
    rename(area_name=hbres, month=date) %>%
    mutate(month=as.Date(month, format="%Y-%m-%d "),
           type=case_when(substr(area_name,1,3)=="NHS" ~ "Health board",
                          area_name=="Scotland" ~ "Scotland", TRUE ~ "Other"),
           area_type = type,
           category="All",
           percent_births=format(((births/tot_births_18_44)*100),digits=1, nsmall=1),
           #NOTE the gestation categories are not mutually exclusive - <37 contains <32
           gest=case_when(gest=="births_under32" ~ "Under 32 weeks",
                          gest=="births_under37" ~ "Under 37 weeks",
                          gest=="births_32_36" ~ "32 to 36 weeks",
                          gest=="births_37_41" ~ "37 to 41 weeks",
                          gest=="births_18_44" ~ "All gestations (18-44 weeks)",
                          gest=="births_42plus" ~ "42 weeks plus",
                          TRUE~as.character(gest)))
  
  saveRDS(gestation_linechart, "shiny_app/data/gestation_linechart_data.rds")
  saveRDS(gestation_linechart, paste0(data_folder,"final_app_files/gestation_linechart_data_",
                                      format(Sys.Date(), format = '%d_%b_%y'), ".rds"))
  
  print("File gestation_linechart_data.rds produced and saved")
  
  
  ## 4- DATA DOWNLOAD FILE FOR SHINY APP
  gestation_download <- read_csv(paste0(data_folder, "pregnancy/gestation_at_delivery/",delivery_folder,"/WI_DELIVERIES_DOWNLOAD_gestation_",delivery_date,".csv"))%>%
    janitor::clean_names() %>%
    mutate(month_of_discharge=as.Date(month_of_discharge,format="%Y-%m-%d"),
           month_of_discharge=format(month_of_discharge,"%b %Y")) %>%
    rename(area_name=nhs_board_of_residence,
           centreline_under32 = median_under32,
           centreline_32_36 = median_32_36,
           centreline_under37 = median_under37,
           centreline_42plus = median_42plus,
           dottedline_under32 = ext_under32,
           dottedline_32_36 = ext_32_36,
           dottedline_under37 = ext_under37,
           dottedline_42plus = ext_42plus) %>%
    mutate(area_type=case_when(substr(area_name,1,3)=="NHS" ~ "Health board",
                               area_name=="Scotland" ~ "Scotland"),
           chart_category="All",
           chart_type= area_type)
  
  saveRDS(gestation_download, "shiny_app/data/gestation_download_data.rds")
  saveRDS(gestation_download, paste0(data_folder,"final_app_files/gestation_download_data_",
                                     format(Sys.Date(), format = '%d_%b_%y'), ".rds"))
  
  print("File gestation_download_data.rds produced and saved")
  
  
  # Saving data for open data platform
  gestation_download %<>%
    select(-c(chart_type, chart_category, dottedline_32_36, dottedline_42plus, dottedline_under32,
              dottedline_under37, indicator, centreline_32_36, centreline_42plus, centreline_under32,
              centreline_under37, perc_denominator)) %>%
    rename("Number of births - All births (18-44 weeks gestation)" = births_18_44,
           "Number of births - 32-36 weeks gestation" = births_32_36,
           "Number of births - 37-41 weeks gestation" = births_37_41,
           "Number of births - At or over 42 weeks gestation" = births_42plus,
           "Number of births - Unknown gestation" = births_gest_unknown,
           "Number of births - Under 32 weeks gestation" = births_under32,
           "Number of births - Under 37 weeks gestation" = births_under37,
           "Number of births - All births" = births_all,
           "Percentage (%) of births - 32-36 weeks gestation" = perc_32_36,
           "Percentage (%) of births - 37-41 weeks gestation" = perc_37_41,
           "Percentage (%) of births - At or over 42 weeks gestation" = perc_42plus,
           "Percentage (%) of births - Under 32 weeks gestation" = perc_under32,
           "Percentage (%) of births - Under 37 weeks gestation" = perc_under37) %>%
    mutate(variable = case_when(variable %in% c("20-24", "25-29", "30-34", "35-39",
                                                "40 and over", "Under 20", "1 - most deprived", "2", "3", "4",
                                                "5 - least deprived", "Unknown") ~ paste0(variable),
                                TRUE ~ "All")) %>%
    mutate(subgroup = case_when(subgroup %in% c("SIMD", "AGEGRP") ~ paste0(subgroup),
                                TRUE ~ "All"))
  
  file.remove(paste0(open_data,"gestation.rds")) #to avoid permission issues
  saveRDS(gestation_download, paste0(open_data,"gestation.rds"))
  
  print("Open data file produced and saved")
  print("#############################################")
  print("Remember to change final_app_files script")
  file.edit("data_prep/final_app_files.R")
  
}

###############################################.
## Perinatal mortality ----
###############################################.
create_perinatal <- function(foldermonth) {
  # P CHART PERINATAL DATA
  p_perinatal <- bind_rows(read_excel(paste0(data_folder,"perinatal/Pchart - SB NND EXTPERI_", foldermonth, "update.xlsx"),
                                      sheet = "Stillbirth", skip = 2) %>% mutate(type = "stillbirths"),
                           read_excel(paste0(data_folder,"perinatal/Pchart - SB NND EXTPERI_", foldermonth, "update.xlsx"),
                                      sheet = "NND", skip = 2) %>% mutate(type = "nnd"),
                           read_excel(paste0(data_folder,"perinatal/Pchart - SB NND EXTPERI_", foldermonth, "update.xlsx"),
                                      sheet = "Extended perinatal", skip = 2) %>% mutate(type = "extperi")) %>%
    janitor::clean_names() %>%
    select(month_of_year=sample_2, number_of_deaths_in_month=observation, sample_size, rate, centreline, stdev = binomial_st_dev_16,
           upper_cl_3_std_dev:type)
  
  u_perinatal <- bind_rows(read_excel(paste0(data_folder,"perinatal/Uchart - PNND INFANT DEATHS_", foldermonth, "update.xlsx"),
                                      sheet = "ID", skip = 2) %>% mutate(type = "infantdeaths"),
                           read_excel(paste0(data_folder,"perinatal/Uchart - PNND INFANT DEATHS_", foldermonth, "update.xlsx"),
                                      sheet = "PNND", skip = 2) %>% mutate(type = "pnnd")) %>%
    janitor::clean_names() %>%
    select(month_of_year=sample,  number_of_deaths_in_month=observation, sample_size=ao_o_size, rate, centreline, stdev = poisson_st_dev_16,
           upper_cl_3_std_dev:type)
  
  # Mergin both datasets together
  perinatal <- rbind(p_perinatal, u_perinatal) %>%
    mutate(area_name="Scotland", #creating geo variables
           area_type="Scotland",
           month_of_year = gsub(" ", "0", month_of_year), #formatting date
           month_of_year = as.Date(paste0(month_of_year,"1"), format="%Y%m%d"))
  
  # Creating rules for spc charts
  perinatal %<>%
    arrange(type, area_name, month_of_year) %>%
    mutate(upper_sigma1 = rate + stdev,
           lower_sigma1 = rate + stdev) %>%
    group_by(type, area_name) %>%
    # for rules: outliers when over or under 3 sigma limit
    mutate(outlier = case_when(rate>upper_cl_3_std_dev | rate< lower_cl_3_std_dev ~ T, T ~ F),
           # Shift: run of 8or more consecutive data points above or below the centreline
           # First id when this run is happening and then iding all points part of it
           shift_i = case_when((rate > centreline & lag(rate, 1) > centreline
                                & lag(rate, 2) > centreline & lag(rate, 3) > centreline
                                & lag(rate, 4) > centreline & lag(rate, 5) > centreline
                                & lag(rate, 6) > centreline & lag(rate, 7) > centreline) |
                                 (rate < centreline & lag(rate, 1) < centreline
                                  & lag(rate, 2) < centreline & lag(rate, 3) < centreline
                                  & lag(rate, 4) < centreline & lag(rate, 5) < centreline
                                  & lag(rate, 6) < centreline & lag(rate, 7) < centreline) ~ T , T ~ F),
           shift = case_when(shift_i == T | lead(shift_i, 1) == T | lead(shift_i, 2) == T
                             | lead(shift_i, 3) == T | lead(shift_i, 4) == T
                             | lead(shift_i, 5) == T | lead(shift_i, 6) == T
                             | lead(shift_i, 7) == T ~ T, T ~ F),
           # Trend: A run of 6 or more consecutive data points
           trend_i = case_when((rate > lag(rate ,1) & lag(rate, 1) > lag(rate, 2)
                                & lag(rate, 2) > lag(rate, 3)  & lag(rate, 3) > lag(rate, 4)
                                & lag(rate, 4) > lag(rate, 5) ) |
                                 (rate < lag(rate ,1) & lag(rate, 1) < lag(rate, 2)
                                  & lag(rate, 2) < lag(rate, 3)  & lag(rate, 3) < lag(rate, 4)
                                  & lag(rate, 4) < lag(rate, 5) )
                               ~ T , T ~ F),
           trend = case_when(trend_i == T | lead(trend_i, 1) == T | lead(trend_i, 2) == T
                             | lead(trend_i, 3) == T | lead(trend_i, 4) == T
                             | lead(trend_i, 5) == T  ~ T, T ~ F),
           #Outer One –Third: Two out of three consecutive data points which sit close to one of the control limits(within 2 and 3 sigma)
           outer_i = case_when((rate > upper_wl_2_std_dev & rate < upper_cl_3_std_dev) &
                                 ((lag(rate,1) > upper_wl_2_std_dev & lag(rate,1) < upper_cl_3_std_dev) |
                                    (lag(rate,2) > upper_wl_2_std_dev & lag(rate,2) < upper_cl_3_std_dev)) ~ T, T ~ F),
           outer = case_when(outer_i == T | lead(outer_i, 1) == T | lead(outer_i, 2) == T ~ T, T ~ F),
           # Inner One -Third: 15 or more consecutive data points that lie close to the centreline(within 1 sigma).
           inner_i = case_when(rate < upper_sigma1 & rate > lower_sigma1 &
                                 lag(rate, 1) < upper_sigma1 & lag(rate, 1) > lower_sigma1 &
                                 lag(rate, 2) < upper_sigma1 & lag(rate, 2) > lower_sigma1 &
                                 lag(rate, 3) < upper_sigma1 & lag(rate, 3) > lower_sigma1 &
                                 lag(rate, 4) < upper_sigma1 & lag(rate, 4) > lower_sigma1 &
                                 lag(rate, 5) < upper_sigma1 & lag(rate, 5) > lower_sigma1 &
                                 lag(rate, 6) < upper_sigma1 & lag(rate, 6) > lower_sigma1 &
                                 lag(rate, 7) < upper_sigma1 & lag(rate, 7) > lower_sigma1 &
                                 lag(rate, 8) < upper_sigma1 & lag(rate, 8) > lower_sigma1 &
                                 lag(rate, 9) < upper_sigma1 & lag(rate, 9) > lower_sigma1 &
                                 lag(rate, 10) < upper_sigma1 & lag(rate, 10) > lower_sigma1 &
                                 lag(rate, 11) < upper_sigma1 & lag(rate, 11) > lower_sigma1 &
                                 lag(rate, 12) < upper_sigma1 & lag(rate, 12) > lower_sigma1 &
                                 lag(rate, 13) < upper_sigma1 & lag(rate, 13) > lower_sigma1 &
                                 lag(rate, 14) < upper_sigma1 & lag(rate, 14) > lower_sigma1 ~ T, T ~F),
           inner = case_when(inner_i == T | lead(inner_i, 1) == T | lead(inner_i, 2) == T
                             | lead(inner_i, 3) == T | lead(inner_i, 4) == T
                             | lead(inner_i, 5) == T | lead(inner_i, 6) == T
                             | lead(inner_i, 7) == T | lead(inner_i, 8) == T
                             | lead(inner_i, 9) == T | lead(inner_i, 10) == T
                             | lead(inner_i, 11) == T | lead(inner_i, 12) == T
                             | lead(inner_i, 13) == T | lead(inner_i, 14) == T ~T, T ~ F)) %>%
    ungroup %>%
    select(-shift_i, -trend_i, -outer_i, -inner_i)
  
  saveRDS(perinatal, "shiny_app/data/perinatal.rds")
  saveRDS(perinatal, paste0(data_folder,"final_app_files/perinatal_",
                            format(Sys.Date(), format = '%d_%b_%y'), ".rds"))
  
  print("File perinatal.rds produced and saved")
  
  perinatal <<- perinatal
  
  # saving perinatal open data files
  perinatal %<>%
    select(area_name, month_of_year, type, number_of_deaths_in_month, rate,
           relevant_births = sample_size) %>%
    mutate(type = recode_factor(type, "extperi" = "Extended perinatal deaths", "infantdeaths" = "Infant deaths", "nnd" = "Neonatal deaths",
                                "pnnd" = "Post-neonatal deaths", "stillbirths" = "Stillbirths"))
  
  file.remove(paste0(open_data,"perinatal_data.rds")) # to avoid permission issues
  saveRDS(perinatal, paste0(open_data, "perinatal_data.rds"))
  
  print("Open data file produced and saved")
  print("#############################################")
  print("Remember to change final_app_files script")
  file.edit("data_prep/final_app_files.R")
}

###############################################.
## Apgar ----
###############################################.

create_apgar <- function(folderdate, max_date) {
  
  apgar_folder <- gsub("_", "", folderdate)
  apgar_date <- folderdate
  
  ## 1-RUNCHART DATA
  ## apgar data for run chart (scotland and nhs board) - monthly
  apgar_runchart_scot <- readRDS(paste0(data_folder, "births_babies/apgar/",apgar_folder,"/WI_DELIVERIES_RUNCHART_Scotland_Apgar5_",apgar_date,".rds")) %>%
    rename(area = HBRES) %>%
    janitor::clean_names() %>%
    mutate(date = gsub("-", "", date), #formatting date
           date = as.Date(paste0(date,"1"), format="%Y%m%d"),
           date_label = format(strptime(date, format = "%Y-%m-%d"), "%B %Y"),
           area_name = area,
           date_type = "Month")
  
  
  apgar_runchart <- readRDS(paste0(data_folder, "births_babies/apgar/",apgar_folder,"/WI_DELIVERIES_RUNCHART_Apgar5_",apgar_date,".rds")) %>%
    rename(area = HBRES) %>%
    mutate(area_name = case_when(area == "NHS Forth valley" ~ "NHS Forth Valley",
                                 area == "NHS Highlands" ~ "NHS Highland",
                                 TRUE ~ as.character(area)),
           date_type = "Quarter") %>%
    janitor::clean_names() %>%
    mutate(date=as.Date(date),
           date_label=phsmethods::qtr(date, format="short")) %>%
    bind_rows(apgar_runchart_scot) %>%
    mutate(type = case_when(substr(area_name,1,3)=="NHS" ~ "Health board",
                            area_name=="Scotland" ~ "Scotland"),
           area_type = type,
           category = "All") %>%
    filter(date <= max_date) %>% 
    # the median column is used to assess shifts or trends - dataset contains NA cells which need to filled
    # ext_ columns (don't exist in data file) are extended median which are blank before projection time period
    # group_by(area_name) %>%
    # mutate(ext_apgar5_37plus = max(median_apgar5_37plus, na.rm = T)) %>%
    # ungroup() %>%
    mutate(ext_median_apgar5_37plus = case_when(is.na(ext_median_apgar5_37plus) ~ median_apgar5_37plus,
                                     TRUE ~ ext_median_apgar5_37plus)) %>%
    group_by(area_name, area_type, type) %>%   #sort data to ensure trends/shifts compare correct data points
    #call function to add flags for runchart shifts and trends
    #shift: name for new field where shift is flagged
    #trend: name for new field where trend is flagged
    #value: which column in dataset contains value being evaluated
    #median: which column in dataset contains the median against which value is tested
    runchart_flags(shift="apgar_shift", trend="apgar_trend",
                   value=perc_low_apgar5_37plus, median=ext_median_apgar5_37plus) %>%
    ungroup() %>%
    filter(!(area_name %in% c("NHS Orkney", "NHS Shetland", "NHS Western Isles")))
  
  saveRDS(apgar_runchart, "shiny_app/data/apgar_runchart_data.rds")
  saveRDS(apgar_runchart, paste0(data_folder,"final_app_files/apgar_runchart_data_",
                                  format(Sys.Date(), format = '%d_%b_%y'), ".rds"))
  
  apgar_runchart <<- apgar_runchart
  
  print("File apgar_runchart_data.rds produced and saved")
  

  ## 2- LINECHART DATA apgar for Scotland only by age and dep
  apgar_scot <- readRDS(paste0(data_folder, "births_babies/apgar/",apgar_folder,"/WI_DELIVERIES_SCOT_CHARTS_Apgar5_",apgar_date,".rds")) %>%
    janitor::clean_names() %>%
    rename(area_name=hbres, quarter = month_of_discharge, category=variable, tot_apgar5_37plus = total_exc_unknown) %>%
    mutate(quarter=as.Date(quarter),
           quarter_label=phsmethods::qtr(quarter, format="short"),
           area_type = case_when(substr(area_name,1,3)=="NHS" ~ "Health board",
                            area_name=="Scotland" ~ "Scotland"),
           type=case_when(subgroup=="AGEGRP" ~ "age",subgroup=="SIMD" ~ "dep"),
           category=as.character(category),
           category = case_when(category == "-under 20" ~ "Under 20",
                                category == "40+" ~ "40 and over",
                                category == "1 - Most deprived" ~ "1 - most deprived",
                                category == "5 - Least deprived" ~ "5 - least deprived",
                                       TRUE ~ as.character(category)))
  
  print("File apgar_scot_data.rds produced and saved")
  
  saveRDS(apgar_scot, "shiny_app/data/apgar_scot_data.rds")
  saveRDS(apgar_scot, paste0(data_folder,"final_app_files/apgar_scot_data_",
                              format(Sys.Date(), format = '%d_%b_%y'), ".rds"))
  
  ## 3- LINECHART DATA apgar for Scotland & NHS board
  apgar_linechart_scot <- readRDS(paste0(data_folder, "births_babies/apgar/",apgar_folder,"/WI_DELIVERIES_LINECHART_Scotland_Apgar5_",apgar_date,".rds")) %>%
    rename(area = HBRES) %>%
    janitor::clean_names() %>%
    mutate(tot_apgar5_37plus=total_exc_unknown) %>%
    #reshape data file for ease of creation of line chart with percentages
    pivot_longer(cols = low_apgar5_37plus:total_exc_unknown, names_to = "ind",values_to = "apgar5") %>%
    mutate(date = gsub("-", "", date), #formatting date
           date = as.Date(paste0(date,"1"), format="%Y%m%d"),
           date_label = format(strptime(date, format = "%Y-%m-%d"), "%B %Y"),
           # date_label = as.Date(date, format="%b %Y"),
           # date_label = as.character(date_label),
           area_name = area,
           date_type = "Month") %>% 
    filter(date <= max_date)
  
  
  apgar_linechart <- readRDS(paste0(data_folder, "births_babies/apgar/",apgar_folder,"/WI_DELIVERIES_LINECHART_Apgar5_",apgar_date,".rds")) %>%
    rename(area=HBRES) %>%
    janitor::clean_names() %>% ungroup %>%
    mutate(tot_apgar5_37plus=total_exc_unknown) %>%
    mutate(area_name = case_when(area == "NHS Forth valley" ~ "NHS Forth Valley",
                                 area == "NHS Highlands" ~ "NHS Highland",
                                 TRUE ~ as.character(area))) %>%
    #reshape data file for ease of creation of line chart with percentages
    pivot_longer(cols = low_apgar5_37plus:total_exc_unknown, names_to = "ind",values_to = "apgar5") %>%
    mutate(date=as.Date(date, format="%Y-%m-%d "),
           date_label=phsmethods::qtr(date, format="short"),
           date_type="Quarter") %>%
    bind_rows(apgar_linechart_scot) %>%
    mutate(type=case_when(substr(area_name,1,3)=="NHS" ~ "Health board",
                          area_name=="Scotland" ~ "Scotland", TRUE ~ "Other"),
           area_type = type,
           category="All",
           percent_apgar=((apgar5/tot_apgar5_37plus)*100),
           ind=case_when(ind=="low_apgar5_37plus" ~ "Babies with Apgar 5 < 7",
                         ind=="total_exc_unknown" ~ "Babies with known Apgar 5",
                         TRUE~as.character(ind))) %>%
    filter(area_name != "NHS Orkney",
           area_name != "NHS Shetland",
           area_name != "NHS Western Isles") %>%
    select(-ext_median_apgar5_37plus)
  
  saveRDS(apgar_linechart, "shiny_app/data/apgar_linechart_data.rds")
  saveRDS(apgar_linechart, paste0(data_folder,"final_app_files/apgar_linechart_data_",
                                   format(Sys.Date(), format = '%d_%b_%y'), ".rds"))
  
  print("File apgar_linechart_data.rds produced and saved")

  ## 4- Apgar DATA DOWNLOAD FILE FOR SHINY APP
  apgar_download <- readRDS(paste0(data_folder, "births_babies/apgar/",apgar_folder,"/WI_Apgar5_DOWNLOAD_",apgar_date,".rds")) %>%
    janitor::clean_names() %>%
    mutate(subgroup = case_when(substr(hbres,1,3) == "NHS" ~ "board",
                                substr(hbres,1,3) == "Not" ~ "board",
                                is.na(subgroup) ~ "scotland",
                                T ~ as.character(subgroup)),
           month_of_discharge=as.Date(month_of_discharge,format="%Y-%m-%d"),
           date_of_discharge=case_when(subgroup == "scotland" ~ as.character(format(month_of_discharge, "%B %Y")),
                                          T ~ phsmethods::qtr(month_of_discharge, format="short")
           )) %>%
    filter(month_of_discharge <= max_date) %>% 
    rename(area_name=hbres,
           centreline_apgar5_37plus = median_apgar5_37plus,
           dottedline_apgar5_37plus = ext_median_apgar5_37plus) %>%
    mutate(area_type=case_when(substr(area_name,1,3)=="NHS" ~ "Health board",
                               area_name=="Scotland" ~ "Scotland"),
           chart_category="All",
           chart_type= area_type,
           births_37_42 = total_exc_unknown + unknown,
           perc_denominator = "births_37_42_apgar5_known") %>%
    select(indicator, subgroup, variable, area_name, date_of_discharge,
           births_37_42_apgar5_0_6 = low_apgar5_37plus,
           births_37_42_apgar5_7_10 = high_apgar5_37plus,
           births_37_42_apgar5_known = total_exc_unknown,
           perc_births_37_42_apgar5_0_6 = perc_low_apgar5_37plus,
           perc_births_37_42_apgar5_7_10 = perc_high_apgar5_37plus,
           centreline_apgar5_0_6 = centreline_apgar5_37plus,
           dottedline_apgar5_0_6 = dottedline_apgar5_37plus,
           perc_denominator,
           area_type,
           chart_category,
           chart_type,
           births_37_42_apgar5_unknown = unknown,
           births_37_42)
  
  saveRDS(apgar_download, "shiny_app/data/apgar_download_data.rds")
  saveRDS(apgar_download, paste0(data_folder,"final_app_files/apgar_download_data_",
                                  format(Sys.Date(), format = '%d_%b_%y'), ".rds"))
  
  print("File apgar_download_data.rds produced and saved")
  print("#############################################")
  print("Remember to change final_app_files script")
  file.edit("data_prep/final_app_files.R")
}

###############################################.
## Preterm babies in NICU ----
###############################################.

create_preterm <- function(preterm_date, max_date) {
  preterm_folder <- gsub("_", "", preterm_date)
  
  # P CHART PRETERM DATA
  preterm <- read_excel(paste0(data_folder,"births_babies/preterm/", preterm_folder,
                               "/Table_SMR02_WIDI_02_Subset_Neonate_p-chart_Qtr_", preterm_date, ".xlsx"),
                                      sheet = "P_Chart", skip = 102) %>%
    janitor::clean_names() %>%
    select(quarter_of_year=sample_2, N_deliveries_23_26_NICU_site=observation, N_deliveries_23_26=sample_size, rate, centreline, stdev = binomial_st_dev_16,
           upper_cl_3_std_dev:lower_wl_2_std_dev) %>%
    mutate(area_name="Scotland", #creating geo variables
           area_type="Scotland",
           quarter_of_year = gsub("-", "", quarter_of_year), #formatting date
           quarter_of_year = gsub("Q1", "01", quarter_of_year), #formatting date
           quarter_of_year = gsub("Q2", "04", quarter_of_year), #formatting date
           quarter_of_year = gsub("Q3", "07", quarter_of_year), #formatting date
           quarter_of_year = gsub("Q4", "10", quarter_of_year), #formatting date
           quarter_of_year = as.Date(paste0(quarter_of_year,"1"), format="%Y%m%d"),
           quarter_label=phsmethods::qtr(quarter_of_year, format="short")) %>%
    filter(!is.na(quarter_of_year))
  
  # Creating rules for spc charts
  preterm %<>%
    arrange(area_name, quarter_of_year) %>%
    mutate(upper_sigma1 = rate + stdev,
           lower_sigma1 = rate + stdev) %>%
    group_by(area_name) %>%
    # for rules: outliers when over or under 3 sigma limit
    mutate(outlier = case_when(rate>upper_cl_3_std_dev | rate< lower_cl_3_std_dev ~ T, T ~ F),
           # Shift: run of 8or more consecutive data points above or below the centreline
           # First id when this run is happening and then iding all points part of it
           shift_i = case_when((rate > centreline & lag(rate, 1) > centreline
                                & lag(rate, 2) > centreline & lag(rate, 3) > centreline
                                & lag(rate, 4) > centreline & lag(rate, 5) > centreline
                                & lag(rate, 6) > centreline & lag(rate, 7) > centreline) |
                                 (rate < centreline & lag(rate, 1) < centreline
                                  & lag(rate, 2) < centreline & lag(rate, 3) < centreline
                                  & lag(rate, 4) < centreline & lag(rate, 5) < centreline
                                  & lag(rate, 6) < centreline & lag(rate, 7) < centreline) ~ T , T ~ F),
           shift = case_when(shift_i == T | lead(shift_i, 1) == T | lead(shift_i, 2) == T
                             | lead(shift_i, 3) == T | lead(shift_i, 4) == T
                             | lead(shift_i, 5) == T | lead(shift_i, 6) == T
                             | lead(shift_i, 7) == T ~ T, T ~ F),
           # Trend: A run of 6 or more consecutive data points
           trend_i = case_when((rate > lag(rate ,1) & lag(rate, 1) > lag(rate, 2)
                                & lag(rate, 2) > lag(rate, 3)  & lag(rate, 3) > lag(rate, 4)
                                & lag(rate, 4) > lag(rate, 5) ) |
                                 (rate < lag(rate ,1) & lag(rate, 1) < lag(rate, 2)
                                  & lag(rate, 2) < lag(rate, 3)  & lag(rate, 3) < lag(rate, 4)
                                  & lag(rate, 4) < lag(rate, 5) )
                               ~ T , T ~ F),
           trend = case_when(trend_i == T | lead(trend_i, 1) == T | lead(trend_i, 2) == T
                             | lead(trend_i, 3) == T | lead(trend_i, 4) == T
                             | lead(trend_i, 5) == T  ~ T, T ~ F),
           #Outer One –Third: Two out of three consecutive data points which sit close to one of the control limits(within 2 and 3 sigma)
           outer_i = case_when((rate > upper_wl_2_std_dev & rate < upper_cl_3_std_dev) &
                                 ((lag(rate,1) > upper_wl_2_std_dev & lag(rate,1) < upper_cl_3_std_dev) |
                                    (lag(rate,2) > upper_wl_2_std_dev & lag(rate,2) < upper_cl_3_std_dev)) ~ T, T ~ F),
           outer = case_when(outer_i == T | lead(outer_i, 1) == T | lead(outer_i, 2) == T ~ T, T ~ F),
           # Inner One -Third: 15 or more consecutive data points that lie close to the centreline(within 1 sigma).
           inner_i = case_when(rate < upper_sigma1 & rate > lower_sigma1 &
                                 lag(rate, 1) < upper_sigma1 & lag(rate, 1) > lower_sigma1 &
                                 lag(rate, 2) < upper_sigma1 & lag(rate, 2) > lower_sigma1 &
                                 lag(rate, 3) < upper_sigma1 & lag(rate, 3) > lower_sigma1 &
                                 lag(rate, 4) < upper_sigma1 & lag(rate, 4) > lower_sigma1 &
                                 lag(rate, 5) < upper_sigma1 & lag(rate, 5) > lower_sigma1 &
                                 lag(rate, 6) < upper_sigma1 & lag(rate, 6) > lower_sigma1 &
                                 lag(rate, 7) < upper_sigma1 & lag(rate, 7) > lower_sigma1 &
                                 lag(rate, 8) < upper_sigma1 & lag(rate, 8) > lower_sigma1 &
                                 lag(rate, 9) < upper_sigma1 & lag(rate, 9) > lower_sigma1 &
                                 lag(rate, 10) < upper_sigma1 & lag(rate, 10) > lower_sigma1 &
                                 lag(rate, 11) < upper_sigma1 & lag(rate, 11) > lower_sigma1 &
                                 lag(rate, 12) < upper_sigma1 & lag(rate, 12) > lower_sigma1 &
                                 lag(rate, 13) < upper_sigma1 & lag(rate, 13) > lower_sigma1 &
                                 lag(rate, 14) < upper_sigma1 & lag(rate, 14) > lower_sigma1 ~ T, T ~F),
           inner = case_when(inner_i == T | lead(inner_i, 1) == T | lead(inner_i, 2) == T
                             | lead(inner_i, 3) == T | lead(inner_i, 4) == T
                             | lead(inner_i, 5) == T | lead(inner_i, 6) == T
                             | lead(inner_i, 7) == T | lead(inner_i, 8) == T
                             | lead(inner_i, 9) == T | lead(inner_i, 10) == T
                             | lead(inner_i, 11) == T | lead(inner_i, 12) == T
                             | lead(inner_i, 13) == T | lead(inner_i, 14) == T ~T, T ~ F)) %>%
    ungroup %>%
    select(-shift_i, -trend_i, -outer_i, -inner_i) %>%
    rename(percentage_NICU_site=rate, quarter=quarter_label)
  
  saveRDS(preterm, "shiny_app/data/preterm.rds")
  saveRDS(preterm, paste0(data_folder,"final_app_files/preterm_",
                            format(Sys.Date(), format = '%d_%b_%y'), ".rds"))
  
  print("File preterm_data.rds produced and saved")
  
  preterm <<- preterm

    ## 2- LINECHART DATA preterm for Scotland
  preterm_linechart <- readRDS(paste0(data_folder, "births_babies/preterm/", preterm_folder, 
                                      "/WI_DELIVERIES_LINECHART_Neonate_", preterm_date,".rds")) %>%
    rename(area_name=HBRES, quarter=DATE) %>%
    janitor::clean_names() %>%
    mutate(tot_neonate_23_26=neonate_23_26) %>%
    #reshape data file for ease of creation of line chart with percentages
    pivot_longer(cols = nicu_23_26:neonate_23_26, names_to = "ind",values_to = "mats") %>%
    mutate(quarter=as.Date(quarter, format="%Y-%m-%d "),
           quarter_label=phsmethods::qtr(quarter, format="short"),
           type="Scotland",
           area_type = type,
           category="All",
           percent_nicu=((mats/tot_neonate_23_26)*100),
           ind=case_when(ind=="nicu_23_26" ~ "Deliveries 23-26w in hosp with NICU",
                         ind=="neonate_23_26" ~ "All deliveries 23-26w",
                         TRUE~as.character(ind))) %>%
    filter(quarter < max_date)
  
  saveRDS(preterm_linechart, "shiny_app/data/preterm_linechart_data.rds")
  saveRDS(preterm_linechart, paste0(data_folder,"final_app_files/preterm_linechart_data_",
                                  format(Sys.Date(), format = '%d_%b_%y'), ".rds"))
  
  print("File preterm_linechart_data.rds produced and saved")
  print("#############################################")
  print("Remember to change final_app_files script")
  file.edit("data_prep/final_app_files.R")
}

###############################################.
## Perineal Tears ----
###############################################.

create_tears <- function(tears_date, max_date) {
  
tears_folder <- gsub("_", "", tears_date)

## 1-RUNCHART DATA
## apgar data for run chart (scotland and nhs board) - monthly
tears_runchart_scot <- readRDS(paste0(data_folder, "births_babies/tears/",tears_folder,"/WI_DELIVERIES_RUNCHART_Scotland_Tears_",tears_date,".rds")) %>% 
  rename(area = HBRES) %>%
  janitor::clean_names() %>% 
  mutate(date = gsub("-", "", date), #formatting date
         date = as.Date(paste0(date,"1"), format="%Y%m%d"),
         date_label = format(strptime(date, format = "%Y-%m-%d"), "%B %Y"),
         area_name = area,
         date_type = "Month")

tears_runchart <- readRDS(paste0(data_folder, "births_babies/tears/",tears_folder,"/WI_DELIVERIES_RUNCHART_Tears_",tears_date,".rds")) %>%
  rename(area = HBRES) %>%
  mutate(area_name = case_when(area == "NHS Forth valley" ~ "NHS Forth Valley",
                               area == "NHS Highlands" ~ "NHS Highland",
                               TRUE ~ as.character(area)),
         date_type = "Quarter") %>%
  janitor::clean_names() %>% 
  mutate(date=as.Date(date),
         date_label=phsmethods::qtr(date, format="short")) %>% 
  bind_rows(tears_runchart_scot) %>% 
  mutate(type = case_when(substr(area_name,1,3)=="NHS" ~ "Health board",
                          area_name=="Scotland" ~ "Scotland"),
         area_type = type,
         category = "All") %>%
  filter(date <= max_date) %>% 
  # the median column is used to assess shifts or trends - dataset contains NA cells which need to filled
  mutate(ext_median_tears_37plus = case_when(is.na(ext_median_tears_37plus) ~ median_tears_37plus,
                                              TRUE ~ ext_median_tears_37plus)) %>%
  group_by(area_name, area_type, type) %>%   #sort data to ensure trends/shifts compare correct data points
  #call function to add flags for runchart shifts and trends
  #shift: name for new field where shift is flagged
  #trend: name for new field where trend is flagged
  #value: which column in dataset contains value being evaluated
  #median: which column in dataset contains the median against which value is tested
  runchart_flags(shift="tears_shift", trend="tears_trend", 
                 value=perc_3rd4th_tears_37plus, median=ext_median_tears_37plus) %>%
  ungroup() %>% 
  filter(!(area_name %in% c("NHS Orkney", "NHS Shetland", "NHS Western Isles")))


saveRDS(tears_runchart, "shiny_app/data/tears_runchart_data.rds")
saveRDS(tears_runchart, paste0(data_folder,"final_app_files/tears_runchart_data_", 
                               format(Sys.Date(), format = '%d_%b_%y'), ".rds"))

tears_runchart <<- tears_runchart

## 2- LINECHART DATA apgar for Scotland only by age and dep
tears_scot <- readRDS(paste0(data_folder, "births_babies/tears/",tears_folder,"/WI_Tears_DOWNLOAD_Qtr_",tears_date,".rds")) %>%  
  janitor::clean_names() %>%
  rename(area_name=nhs_board_of_residence, quarter=month_of_discharge, category=variable, tot_tears_37plus = nbr_3_4_degree_tear_37plus) %>%
  mutate(quarter=as.Date(quarter),
         quarter_label=phsmethods::qtr(quarter, format="short"),
         area_type = case_when(substr(area_name,1,3)=="NHS" ~ "Health board",
                               area_name=="Scotland" ~ "Scotland"),
         type=case_when(subgroup=="AGEGRP" ~ "age",subgroup=="SIMD" ~ "dep"),
         category=as.character(category),
         category = case_when(category == "-under 20" ~ "Under 20",
                              category == "40+" ~ "40 and over",
                              category == "1 - Most deprived" ~ "1 - most deprived",
                              category == "5 - Least deprived" ~ "5 - least deprived",
                              TRUE ~ as.character(category))) 

saveRDS(tears_scot, "shiny_app/data/tears_scot_data.rds")
saveRDS(tears_scot, paste0(data_folder,"final_app_files/tears_scot_data_", 
                           format(Sys.Date(), format = '%d_%b_%y'), ".rds"))

print("File tears_scot_data.rds produced and saved")

## 3- LINECHART DATA apgar for Scotland & NHS board
tears_linechart_scot <- readRDS(paste0(data_folder, "births_babies/tears/",tears_folder,"/WI_DELIVERIES_LINECHART_Scotland_Tears_",tears_date,".rds")) %>% 
  rename(area = HBRES) %>%
  janitor::clean_names() %>% 
  mutate(tot_tears_37plus=total_exc_unknown) %>%
  #reshape data file for ease of creation of line chart with percentages
  pivot_longer(cols = nbr_3_4_degree_tear_37plus:total_exc_unknown, names_to = "ind",values_to = "tears") %>%
  mutate(date = gsub("-", "", date), #formatting date
         date = as.Date(paste0(date,"1"), format="%Y%m%d"),
         date_label = format(strptime(date, format = "%Y-%m-%d"), "%B %Y"),
         # date_label = as.Date(date, format="%b %Y"),
         # date_label = as.character(date_label),
         area_name = area,
         date_type = "Month") 

tears_linechart <- readRDS(paste0(data_folder, "births_babies/tears/",tears_folder,"/WI_DELIVERIES_LINECHART_Tears_",tears_date,".rds")) %>% 
  rename(area=HBRES) %>%
  janitor::clean_names() %>% ungroup %>% 
  mutate(tot_tears_37plus=total_exc_unknown) %>%
  mutate(area_name = case_when(area == "NHS Forth valley" ~ "NHS Forth Valley",
                               area == "NHS Highlands" ~ "NHS Highland",
                               TRUE ~ as.character(area))) %>%  
  #reshape data file for ease of creation of line chart with percentages
  pivot_longer(cols = nbr_3_4_degree_tear_37plus:total_exc_unknown, names_to = "ind",values_to = "tears") %>%
  mutate(date=as.Date(date, format="%Y-%m-%d "),
         date_label=phsmethods::qtr(date, format="short"),
         date_type="Quarter") %>% 
  bind_rows(tears_linechart_scot) %>% 
  mutate(type=case_when(substr(area_name,1,3)=="NHS" ~ "Health board",
                        area_name=="Scotland" ~ "Scotland", TRUE ~ "Other"),
         area_type = type, 
         category="All",
         percent_tears=((tears/tot_tears_37plus)*100),
         ind=case_when(ind=="nbr_3_4_degree_tear_37plus" ~ "Women who have a 3rd or 4th degree perineal tear",
                       ind=="total_exc_unknown" ~ "Women with known perineal tear status",
                       TRUE~as.character(ind))) %>% 
  filter(!(area_name %in% c("NHS Orkney", "NHS Shetland", "NHS Western Isles"))) %>% 
  select(-ext_median_tears_37plus) %>% 
  filter(date <= max_date) 
  
saveRDS(tears_linechart, "shiny_app/data/tears_linechart_data.rds") 
saveRDS(tears_linechart, paste0(data_folder,"final_app_files/tears_linechart_data_", 
                                format(Sys.Date(), format = '%d_%b_%y'), ".rds"))

print("File tears_linechart_data.rds produced and saved")

## 4- Apgar DATA DOWNLOAD FILE FOR SHINY APP
tears_download <- readRDS(paste0(data_folder, "births_babies/tears/",tears_folder,"/WI_Tears_DOWNLOAD_Qtr_",tears_date,".rds")) %>%  
  janitor::clean_names() %>%
  mutate(subgroup = case_when(substr(nhs_board_of_residence,1,3) == "NHS" ~ "board",
                              substr(nhs_board_of_residence,1,3) == "Not" ~ "board",
                              is.na(subgroup) ~ "scotland",
                              T ~ as.character(subgroup)),
         month_of_discharge=as.Date(month_of_discharge,format="%Y-%m-%d"),
         date_of_discharge=case_when(subgroup == "scotland" ~ as.character(format(month_of_discharge, "%B %Y")),
                                     T ~ phsmethods::qtr(month_of_discharge, format="short")
         )) %>%
  rename(area_name=nhs_board_of_residence,
         centreline_tears_37_42 = median_tears_37plus,
         dottedline_tears_37_42 = ext_median_tears_37plus) %>% 
  mutate(area_type=case_when(substr(area_name,1,3)=="NHS" ~ "Health board",
                             area_name=="Scotland" ~ "Scotland"),
         chart_category="All",
         chart_type= area_type,
         perc_denominator = "births_37plus_tears_known") %>% 
  filter(month_of_discharge <= max_date) %>% 
  select(-month_of_discharge) %>% 
  select(indicator, subgroup, variable, area_name, date_of_discharge, 
         no_tear_37_plus = no_perineal_tear_37plus,
         "1st_2nd_degree_tear_37_42" = nbr_1_2_degree_tear_37plus, 
         "3rd_4th_degree_tear_37_42" = nbr_3_4_degree_tear_37plus, 
         unspecified_tear_37_42 = unspecified_tear_37plus,
         known_tear_status_37_42 = total_exc_unknown, 
         perc_no_tears_37_42 = perc_no_tears_37plus,
         perc_1st2nd_tears_37_42 = perc_1st2nd_tears_37plus, 
         perc_3rd4th_tears_37_42 = perc_3rd4th_tears_37plus, 
         perc_unspecified_tears_37_42 = perc_unspecified_tears_37plus,
         centreline_tears_37_42,
         dottedline_tears_37_42,
         perc_denominator,
         area_type,
         chart_category,
         chart_type,
         unknown_tear_status_37_42 = not_known_if_tear_37plus,
         total_37_42 = total_inc_unknown)

saveRDS(tears_download, "shiny_app/data/tears_download_data.rds")  
saveRDS(tears_download, paste0(data_folder,"final_app_files/tears_download_data_", 
                               format(Sys.Date(), format = '%d_%b_%y'), ".rds"))

print("File tears_download_data.rds produced and saved")
print("#############################################")
print("Remember to change final_app_files script")
file.edit("data_prep/final_app_files.R")

}

##END
