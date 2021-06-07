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
## Summary datasets ----
###############################################.
# Still to be done, drugs need some thinking
source("data_prep/summary_data_prep.R") # This sources the functions for the section
# Filedate: date on filename for each update, last week = last week of data to be included
create_rapid(last_week =  "2021-05-23") # this requires access to the RAPID dataset
create_ae(filedate = "2021-05-27", last_week =  "2021-05-23")
create_ooh(filename = "WIDER IMPACT PC OOH Data_53_1531979385625197123", last_week =  "2021-05-23")
create_nhs24(filedate = "2021-05-31", last_week =  "2021-05-23")
create_sas(filedate = "2021-05-31", last_week =  "2021-05-23")
# Deaths require access to deaths catalogue
source("data_prep/deaths_data_preparation.R") # And the deaths function
create_deaths(last_week =  "2021-05-23")

############## Remember to change final_app_files script dates
file.edit("data_prep/final_app_files.R")

# Outpatients data created by Secondary care team

###############################################.
## Pregnancy datasets ----
###############################################.
source("data_prep/pregnancy_data_prep.R") # functions for section

create_antebooking(booking_date = "18052021")
create_terminations(top_date = "2021-05-11")

###############################################.
## Immunisation datasets ----
###############################################.
source("data_prep/imm_data_prep.R") # functions for section
#To be added, check with team if this would work for them

###############################################.
## Mental health datasets ----
###############################################.
# Still to be done, drugs need some thinking

##END

