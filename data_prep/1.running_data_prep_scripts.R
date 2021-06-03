## This code runs most scripts that produces data for the dashboard

###############################################.
## Birth and babies datasets ----
###############################################.

source("data_prep/births_babies_data_prep.R") # This sources the functions for the section
#The function returns one object per dataset for checking purposes, maybe not the best one or more needed
#This is perhaps a very big function and should be split into three (one for each delivery indicator)
create_delivery(folderdate = "2021-05-18") # Mode of delivery, induction and gestation data
create_perinatal(foldermonth = "june") # Stillbirths and perinatal mortality
create_apgar(folderdate = "2021_05_13") # Apgar scores
create_preterm(preterm_date = "2021_03_18") # Apgar scores

###############################################.
## Birth and babies datasets ----
###############################################.

##END

