## This code runs most scripts that produces data for the dashboard
# The functions return an object per dataset, maybe this needs tweaked so it's a better one for checking

# Cancer, Outpatients, Immunisation and MH datasets still not working this way.

# After running these scripts, update the final script files date and the 
# relevant bits in the tabs of the app (e.g. update date)

###############################################.
## Birth and babies datasets ----
###############################################.

source("data_prep/births_babies_data_prep.R") # This sources the functions for the section
#This is perhaps a very big function and should be split into three (one for each delivery indicator)
create_delivery(folderdate = "2021-06-15") # Mode of delivery, induction and gestation data
create_perinatal(foldermonth = "july21") # Stillbirths and perinatal mortality
create_apgar(folderdate = "2021_06_21") # Apgar scores
create_preterm(preterm_date = "2021_06_21", max_date = "2021-03-01") # Preterm
create_tears(tears_date = "2021_06_21", max_date = "2021-03-01") # Perineal tears


# Add here what needs to be changed in the shiny scripts

###############################################.
## Cardiovascular datasets ----
###############################################.
source("data_prep/cardio_data_prep.R") # This sources the functions for the section
# Filedate: date on filename for each update, last week = last week of data to be included
create_aecardio(filedate = "2021-05-27", last_week =  "2021-05-23")
create_oohcardio(filedate = "2021-05-31", last_week =  "2021-05-23")
create_sascardio(filedate = "2021-05-31", last_week =  "2021-05-23")
create_cardiodrugs(filedate = "2021-05-27", last_week =  "2021-05-23")

############## Remember to change final_app_files script dates
# Then you need to change the update date in the cardio_tab script 
file.edit("data_prep/final_app_files.R")
file.edit("shiny_app/cardio_tab.R")

# This function will need some work if we update again this dataset, but at the 
# moment it is not updated in any regular basis
create_cathlab()

###############################################.
## Child health datasets ----
###############################################.
source("data_prep/childhealth_data_prep.R") # This sources the functions for the section
create_chreview(ch_date_file = "20210628") # Child health reviews. #date included in filepath name
create_childdev(filedate = "28thJun")
create_breastfeeding(filedate = "28thJun")

############## Remember to change final_app_files script dates
file.edit("data_prep/final_app_files.R")
# Add here what needs to be changed in the shiny scripts

###############################################.
## Summary datasets ----
###############################################.
source("data_prep/summary_data_prep.R") # This sources the functions for the section
# Filedate: date on filename for each update, last week = last week of data to be included
# Change extract to F if you just want to run the data prep and not the extraction 
# (quicker once the extraction has been done once)
create_rapid(last_week =  "2021-05-23", extract = T) # this requires access to the RAPID dataset
create_ae(filedate = "2021-07-01", last_week =  "2021-06-27")
create_ooh(filename = "WIDER IMPACT PC OOH Data_53_1531979385625197123", last_week =  "2021-06-27")
create_nhs24(filedate = "2021-07-05", last_week =  "2021-06-27")
create_sas(filedate = "2021-07-05", last_week =  "2021-06-27")

# Deaths require access to deaths catalogue
source("data_prep/deaths_data_preparation.R") # And the deaths function
create_deaths(last_week =  "2021-05-23")

############## Remember to change final_app_files script dates
# Then you need to change the update date in the summary_tab script 
file.edit("data_prep/final_app_files.R")
file.edit("shiny_app/summary_tab.R")

# Outpatients data created by Secondary care team

###############################################.
## Pregnancy datasets ----
###############################################.
source("data_prep/pregnancy_data_prep.R") # functions for section
create_antebooking(booking_date = "17062021", max_book_date = "2021-06-14")
create_terminations(top_date = "2021-06-15")


# Add here what needs to be changed in the shiny scripts

###############################################.
## Mental health datasets ----
###############################################.
source("data_prep/mh_data_prep.R") # This sources the functions for the section
create_aemh(filedate = "2021-07-04", last_week =  "2021-06-27") #takes a while
create_oohmh(filedate = "2021-07-05", last_week =  "2021-06-27")
create_drugsmh(last_week =  "2021-06-27")

# Change update date in mental_health_tab.R script
file.edit("shiny_app/mental_health_tab.R")

###############################################.
## Immunisation datasets ----
###############################################.
source("data_prep/imm_data_prep.R") # functions for section
#To be added, check with team if this would work for them

# Add here what needs to be changed in the shiny scripts

##END

