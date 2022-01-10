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

create_delivery(folderdate = "2021-11-15") # Mode of delivery, induction and gestation data
create_perinatal(foldermonth = "nov21") # Stillbirths and perinatal mortality
create_apgar(folderdate = "2021_11_17") # Apgar scores
create_preterm(preterm_date = "2021_10_14", max_date = "2021-07-01") # Preterm
create_tears(tears_date = "2021_11_17", max_date = "2021-08-01") # Perineal tears

# Add here what needs to be changed in the shiny scripts

###############################################.
## Cardiovascular datasets ----
###############################################.
source("data_prep/cardio_data_prep.R") # This sources the functions for the section
# Filedate: date on filename for each update, last week = last week of data to be included
create_aecardio(filedate = "2021-11-25", last_week =  "2021-11-21")
create_sascardio(filedate = "2021-11-29", last_week =  "2021-11-21")
create_cardiodrugs(filedate = "2021-11-25", last_week =  "2021-11-21")
create_oohcardio(filedate = "2021-11-29", last_week =  "2021-11-21")

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
create_chreview(ch_date_file = "20211122") # Child health reviews. #date included in filepath name
create_childdev(filedate = "22ndNov2021")
create_breastfeeding(filedate = "22ndNov2021")

############## Remember to change final_app_files script dates
file.edit("data_prep/final_app_files.R")
# Add here what needs to be changed in the shiny scripts
file.edit("shiny_app/global.R")

###############################################.
## Summary datasets ----
###############################################.
source("data_prep/summary_data_prep.R") # This sources the functions for the section
# Filedate: date on filename for each update, last week = last week of data to be included
# Change extract to F if you just want to run the data prep and not the extraction
# (quicker once the extraction has been done once)
create_rapid(last_week =  "2022-01-02", extract = F) # this requires access to the RAPID dataset
create_ae(filedate = "2022-01-06", last_week =  "2022-01-02")
create_ooh(filename = "WIDER IMPACT PC OOH Data_52_3843723932621185282", last_week = "2021-11-21")
create_nhs24(filedate = "2022-01-10", last_week =  "2022-01-02")
create_sas(filedate = "2021-11-29", last_week =  "2021-11-21")

# Deaths require access to deaths catalogue
source("data_prep/deaths_data_preparation.R") # And the deaths function
# Note: there will be a warning from match_area() about a few hundred thousand
# geography codes not 9 characters in length - this is caused by the "Scotland"
# rows, and is not a problem.
create_deaths(last_week =  "2021-11-21")

############## Remember to change final_app_files script dates
# Then you need to change the update date in the summary_tab script
file.edit("data_prep/final_app_files.R")
file.edit("shiny_app/summary_tab.R")

# Outpatients data created by Secondary care team

###############################################.
## Pregnancy datasets ----
###############################################.
source("data_prep/pregnancy_data_prep.R") # functions for section

create_terminations(top_date = "2021-11-15")
create_antebooking(booking_date = "11112021", max_book_date = "2021-11-07")

# Add here what needs to be changed in the shiny scripts

###############################################.
## Mental health datasets ----
###############################################.
source("data_prep/mh_data_prep.R") # This sources the functions for the section
create_aemh(filedate = "2021-11-29", last_week =  "2021-11-21") #takes a while
create_oohmh(filedate = "2021-11-29", last_week =  "2021-11-21")
# There will be a warning about duplicate rows, 63 rows removed - this is fine,
# Glasgow City HSCP appears with 2 different codes earlier in the data, summing
# count is correct.
create_drugsmh(last_week =  "2021-11-21")

# Change update date in mental_health_tab.R script
file.edit("shiny_app/mental_health_tab.R")

###############################################.
## Immunisation datasets ----
###############################################.
source("data_prep/imm_data_prep.R") # functions for section
#To be added, check with team if this would work for them

# Add here what needs to be changed in the shiny scripts

##END

