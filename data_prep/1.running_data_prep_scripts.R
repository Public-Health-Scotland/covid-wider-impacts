## This code runs most scripts that produces data for the dashboard

###############################################.
## Birth and babies datasets ----
###############################################.

source("data_prep/births_babies_data_prep.R") # This sources the functions for the section
create_delivery(folderdate = "2021-04-13") # Mode of delivery data
#The function returns one object per dataset for checking purposes, maybe not the best one or more needed
#This is perhaps a very big function and should be split into three (one for each delivery indicator)

#You could have then functions for each dataset/group of them

##END