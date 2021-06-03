## This code runs most scripts that produces data for the dashboard
# The functions return an object per dataset, maybe this needs tweaked so it's a better one for checking

###############################################.
## Birth and babies datasets ----
###############################################.

source("data_prep/births_babies_data_prep.R") # This sources the functions for the section
#This is perhaps a very big function and should be split into three (one for each delivery indicator)
create_delivery(folderdate = "2021-05-18") # Mode of delivery, induction and gestation data
create_perinatal(foldermonth = "june") # Stillbirths and perinatal mortality
create_apgar(folderdate = "2021_05_13") # Apgar scores
create_preterm(preterm_date = "2021_03_18") # Apgar scores

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
file.edit("data_prep/final_app_files.R")

# This function will need some work if we update again this dataset, but at the 
# moment it is not updated in any regular basis
create_cathlab()

###############################################.
## Child health datasets ----
###############################################.
source("data_prep/childhealth_data_prep.R") # This sources the functions for the section
create_chreview(ch_date_file = "20210524") # Child health reviews. #date included in filepath name
create_childdev(filedate = "24thMay")
create_breastfeeding(filedate = "24thMay")

############## Remember to change final_app_files script dates
file.edit("data_prep/final_app_files.R")

###############################################.
## Mental health datasets ----
###############################################.
# Still to be done, drugs need some thinking

##END

