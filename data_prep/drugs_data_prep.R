###############################################.
## Functions/Packages/filepaths/lookups ----
###############################################.
source("data_prep/functions_packages_data_prep.R")

library(readxl)
library(lubridate)
library(tidyr)
library(ISOweek)
library(tidyverse)
library(reshape2)
library(zoo)

###############################################.
## Drug and alcohol treatment referrals ----
###############################################.
#When updating for future updates the following lines must be updated:
#Line 22: Update filepath to data


# Reading data
Referrals_breakdown <- read_excel(paste0(data_folder,"drugs/Referrals_23052022_breakdown.xlsx"),
                                  col_types = c("text", "text", "date", "numeric")) %>% 
  #Extracting isoweek and year from date
  mutate(Week = isoweek(DateReferralReceived),
         Year = isoyear(DateReferralReceived)) %>% 
  rename('Type' = 'ClientType',
         'Board'= 'location',
         'Date' = 'DateReferralReceived',
         'DTR'  = 'n') %>% 
  filter(Year >= 2018)


# Create an 'All' category. 
Referrals_breakdown_All <- Referrals_breakdown %>% 
  group_by(Board,Date,Week,Year) %>% 
  summarise(DTR = sum(DTR)) %>% 
  mutate(Type = 'All')

Referrals_breakdown <- bind_rows(Referrals_breakdown,Referrals_breakdown_All)

# Pivot data so that two year averages across 2018 and 2019 can be calculated

Ref_2018_2019_Average <- Referrals_breakdown %>% 
  filter(Year %in% c(2018,2019)) %>% 
  select(-Date) %>% 
  complete(Type,Board,Week,Year) %>% 
  pivot_wider(names_from = Year, values_from = DTR) %>% 
  mutate(across(c('2018':'2019'), ~replace_na(.x, 0))) %>% 
  rowwise() %>% 
  mutate(two_year_avg = mean(c(`2018`,`2019`)))

# Complete data set across types 
# As we have dates this is easiest done separately. 
# Use date and add Isoweek back in

Referrals_breakdown_2020 <- Referrals_breakdown %>% 
  filter(Year == 2020) %>% 
  complete(Date,Type,Board,Year,fill = list(DTR = 0)) %>% 
  mutate(Week = isoweek(Date))

Referrals_breakdown_2021 <- Referrals_breakdown %>% 
  filter(Year == 2021) %>% 
  complete(Date,Type,Board,Year,fill = list(DTR = 0)) %>% 
  mutate(Week = isoweek(Date))

Referrals_breakdown_2022 <- Referrals_breakdown %>% 
  filter(Year == 2022) %>% 
  complete(Date,Type,Board,Year,fill = list(DTR = 0)) %>% 
  mutate(Week = isoweek(Date))

Referrals_breakdown <- bind_rows(Referrals_breakdown_2020,Referrals_breakdown_2021,Referrals_breakdown_2022)

# Match on averages. 

Referrals_breakdown <- left_join(Referrals_breakdown,Ref_2018_2019_Average)

# We need to add in averages for a 53 week.
# We do this by duplicating week 52. 
Referrals_breakdown <- Referrals_breakdown %>% 
  arrange(Board,Type,Date) %>% 
  mutate(two_year_avg_check = if_else(Week == 53, lag(two_year_avg),two_year_avg))

# Create percentage change variable. 

Referrals_breakdown <- Referrals_breakdown %>% 
  mutate(Change = round_half_up(x = (DTR-two_year_avg_check)/two_year_avg_check*100,digits = 1))


# Select necessary variables and rename as necessary
Referrals_breakdown <- Referrals_breakdown %>% 
  select(Date,Board,Type,two_year_avg_check,DTR,Change) %>% 
  rename('Average 2018 & 2019' = two_year_avg_check,
         '2020 & 2021' = DTR)

# Change all codepency averages and change values to 0. 
Referrals_breakdown<-Referrals_breakdown %>% 
  mutate(`Average 2018 & 2019` = replace(`Average 2018 & 2019`, Type=='Co-dependency', NA))
Referrals_breakdown<-Referrals_breakdown %>% 
  mutate(Change = replace(Change, Type=='Co-dependency', NA))
# And values before 2020-12-01
Referrals_breakdown<-Referrals_breakdown %>% 
  mutate(`2020 & 2021` = replace(`2020 & 2021`,Date<'2020-12-01' & Type=='Co-dependency', NA))

# Just to keep consistent with previous data prep. 
Referrals_breakdown <- Referrals_breakdown %>% 
  mutate(Change = case_when(is.nan(Change)   ~ 0,
                            is.infinite(Change) ~ NA_real_,
                            TRUE ~ Change),
         Date = as.Date(Date))

# Change name of 2020/2021 variable
Referrals_breakdown <- Referrals_breakdown %>% 
  rename(`2020, 2021 & 2022` = `2020 & 2021`)

# Save out data. 
saveRDS(Referrals_breakdown,file="shiny_app/data/DTR_data.rds")
saveRDS(Referrals_breakdown, paste0(data_folder,"final_app_files/DTR_data_",
                                    format(Sys.Date(), format = '%d_%b_%y'), ".rds"))

###############################################.
## Take Home Naloxone ----
###############################################.
#When updating for future updates the following lines must be updated:
#Line 156: Update filepath to data
#Line 177: Update number of months of 2021 data 

# Reading file
dashboard_monthly_data <- readRDS(paste0(data_folder, "drugs/dashboard_monthly_data_2021_Q3.rds"))

HB_data<-dashboard_monthly_data[order(dashboard_monthly_data$month),] #ordering by date

HB_data<-HB_data[,c(3,2,1,4:8)]#Reordering columns

colnames(HB_data)[1:3]<-c('Date','Board','Type')

hb<-unique(HB_data$Board)
types<-unique(HB_data$Type)
dates<-unique(HB_data$Date)

#Completing the data with all combinations of NHS board, Types and Dates
#Filling missing combinations with 0
comp.data<-complete(HB_data, Board=hb, Type=types, Date=dates,
                    fill=list(`2020`=0,`2021`=0,`2019`=0,`2018`=0,two_year_avg=0))


comp.data<-comp.data[order(comp.data$Date),]#Ordering by date
comp.data<-comp.data[,c(3,1,2,4:8)]#Reordering columns

block<-nrow(subset(comp.data,Date<=12))#CHANGE WHEN UPDATING DATA:replace with number of months of 2021 data available
comp.data.temp<-rbind(comp.data[,c(1:3,8,6)],comp.data[c(1:block),c(1:3,8,6)])
comp.data.temp[c((nrow(comp.data)+1):(nrow(comp.data)+block)),5]<-comp.data[c(1:block),7]

colnames(comp.data.temp)[c(4,5)]<-c('Average 2018 & 2019', '2020 & 2021')

Change<-(comp.data.temp$`2020 & 2021`-comp.data.temp$`Average 2018 & 2019`)*100/comp.data.temp$`Average 2018 & 2019`
Change[which(is.nan(Change))]<-0
Change[which(is.infinite(Change))]<-NA
THN_by_Hb<-cbind(comp.data.temp, 'Change'=Change)
THN_by_Hb$Date<-month.abb[THN_by_Hb$Date]#Changing from numbers into abbreviated month names

THN_by_Hb$Date[1:nrow(comp.data)]<-paste(THN_by_Hb$Date[1:nrow(comp.data)],'2020')
THN_by_Hb$Date[(nrow(comp.data)+1):nrow(THN_by_Hb)]<-paste(THN_by_Hb$Date[(nrow(comp.data)+1):nrow(THN_by_Hb)],'2021')

#Adding proportion column
proportion_av<-rep(0,nrow(THN_by_Hb))
proportion_current<-rep(0,nrow(THN_by_Hb))

iter<-seq(1,nrow(THN_by_Hb),5)

av<-THN_by_Hb$`Average 2018 & 2019`
cur<-THN_by_Hb$`2020 & 2021`
for (i in iter){
  proportion_av[i:(i+4)]<-av[i:(i+4)]/av[i]
  proportion_current[i:(i+4)]<-cur[i:(i+4)]/cur[i]
}

proportion_av[which(is.nan(proportion_av))]<-NA
proportion_current[which(is.nan(proportion_current))]<-NA

#Want to make proportion into percentage instead
proportion_current<-proportion_current*100

new_THN<-cbind(THN_by_Hb,'Proportion 20/21'=proportion_current)

new_THN$Change<-as.numeric(format(round(new_THN$Change, 1), nsmall = 1) )
new_THN$`Proportion 20/21`<-as.numeric(format(round(new_THN$`Proportion 20/21` ,1),nsmall=1))

# # SAVING files
saveRDS(new_THN, "shiny_app/data/THN_by_HB.rds")
saveRDS(new_THN, paste0(data_folder,"final_app_files/THN_by_HB_",
                          format(Sys.Date(), format = '%d_%b_%y'), ".rds"))


###############################################.
## SAS data prep ----
###############################################.
#When updating for future updates the following lines must be updated:
#Line 230: Update filepath to data
#Line 319: Update number of weeks of data available

# Reading file
SAS_data <- read_excel(paste0(data_folder, "drugs/2022_1_SAS_reformat_April.xlsx"))

SAS_data<- SAS_data %>%
  rename(Date='Week Commencing')

#Gathering board names into NHS board column
SAS_long<-melt(SAS_data,
               id.vars= c('Date'),
               measure.vars=c(2:17),
               variable.name = 'Board',
               value.name='Number')

SAS_long<- SAS_long %>%
  mutate('Year'=isoyear(SAS_long$Date))

SAS_long<-SAS_long[order(SAS_long$Date),]#Ordering by date
SAS_long$Number<-as.numeric(SAS_long$Number)#Changing to numeric format
SepYears<-split(SAS_long, SAS_long$Year)#Separating by year

data.2018<-SepYears$`2018`
data.2019<-SepYears$`2019`
data.2020<-SepYears$`2020`
data.2021<-SepYears$`2021`
data.2022<-SepYears$`2022`
#Variable for length of a weeks worth of data as have to repeat last week of 2018/19 data to
#Match up with 2020 data
diff<-nrow(data.2020)-nrow(data.2018)

#Repeating last week of 2018
data.2018<-rbind(data.2018,data.2018[c((nrow(data.2018)-diff+1):nrow(data.2018)),])
data.2019<-rbind(data.2019,data.2019[c((nrow(data.2019)-diff+1):nrow(data.2019)),])

#Adding 2021 and 2022 block to end of all data frames
data.2018<-rbind(data.2018, data.2018[c(1:nrow(data.2021)),],data.2018[c(1:nrow(data.2022)),])
data.2019<-rbind(data.2019, data.2019[c(1:nrow(data.2021)),],data.2019[c(1:nrow(data.2022)),])
data.2020.2021<-rbind(data.2020,data.2021,data.2022)

#0 data comes in as NA from excel so needs to be changed to 0
data.2020.2021$Number[which(is.na(data.2020.2021$Number))]<-0
data.2018$Number[which(is.na(data.2018$Number))]<-0
data.2019$Number[which(is.na(data.2019$Number))]<-0

#Calculating 2018 and 2019 Average
average.1819<-(data.2018$Number+data.2019$Number)/2
 

data<- data.2020.2021 %>%
  mutate(average= average.1819)

data<-data %>% 
  select(-Year)#Removing Year column

data<- data %>%
  rename(`2020 & 2021`=Number,
         `Average 2018 & 2019`=average)

#Adding NHS to the beginning of health board names
levels(data$Board)<-c('NHS Ayrshire & Arran',
                      'NHS Borders',
                      'NHS Dumfries & Galloway',
                      'NHS Fife',
                      'NHS Forth Valley',
                      'NHS Grampian',
                      'NHS Greater Glasgow & Clyde',
                      'NHS Highland',
                      'NHS Lanarkshire',
                      'NHS Lothian',
                      'NHS Orkney',
                      'NHS Shetland',
                      'NHS Tayside',
                      'NHS Western Isles',
                      'Unassigned',
                      'Scotland')

data<-data %>%  
  arrange(Board)#Ordering by board

#Rounding to 1dp
data$`2020 & 2021`<-round(data$`2020 & 2021`,1)
data$`Average 2018 & 2019`<-round(data$`Average 2018 & 2019`,1)

#Removed 'unassigned' group of observations 
data<- data %>%  
  filter(Board!='Unassigned')
#Creating variables for rolling average
rolling.18.19<-rep(0,nrow(data))
rolling.20.21<-rep(0,nrow(data))


section<-nrow(data)/15 #number of weeks that data is collected for= total number of observations/number of locations(15)
loop<-seq(1,nrow(data),section) #For applying rolling average to each section

for (i in loop){
  'Loop for 3 week rolling average'
  'Using roll apply function from zoo package: Carries out rolling average on each NHS board '
  rolling.18.19[i:(i+section-1)]<-rollapply(data$`Average 2018 & 2019`[i:(i+section-1)], width= 3, fill = NA,FUN=function(x) mean(x,na.rm=T))
  rolling.20.21[i:(i+section-1)]<-rollapply(data$`2020 & 2021`[i:(i+section-1)], width= 3, fill = NA,FUN=function(x) mean(x,na.rm=T))
}

data<-data.frame(data, rolling1819=rolling.18.19, rolling2021=rolling.20.21)
data <- data %>%
  rename(`2020 & 2021`=rolling2021,
         `Average 2018 & 2019`=rolling1819,
         `Raw 20/21`= X2020...2021,
         `Raw 2018/19`= Average.2018...2019)
data$`Average 2018 & 2019`<-round(data$`Average 2018 & 2019`,1)
data$`2020 & 2021`<- round(data$`2020 & 2021`,1)

# Change name of 2020/2021 variable
data <- data %>% 
  rename(`2020, 2021 & 2022` = `2020 & 2021`)

# Reformat date variable
data <- data %>% 
  mutate(Date = as.Date(Date))

# SAVING Files
saveRDS(data, "shiny_app/data/SASdata.rds")
saveRDS(data, paste0(data_folder,"final_app_files/SASdata_",
                        format(Sys.Date(), format = '%d_%b_%y'), ".rds"))


###############################################.
## OST data prep ----
###############################################.
#When updating for future updates the following lines must be updated:
#Line 352 to line 368: Update filepaths to data
#Check formatting code as raw data comes slightly differently every time

# Methadone HB data
raw.meth.paid <- read_excel(paste0(data_folder, "drugs/we01052022_Methadone 1mg.ml-1.xlsx"),
                                   sheet = "Paid Items", 
                            col_types = c("text","text", "text", "numeric", "numeric", "numeric"))                                                                           

# Methadone scot data
scot.raw.meth.paid <- read_excel(paste0(data_folder, "drugs/we01052022_Methadone 1mg.ml-1.xlsx"),
                                        sheet = "Context ePr Vs Paid", col_types = c("text",
                                                                                     "text", "numeric", "text", "numeric"))

# Buprenorphine HB data
raw.bup.paid <- read_excel(paste0(data_folder, "drugs/we01052022 Buprenorphine_2MG_8MG_16MG.xlsx"),
                                  sheet = "Paid Items", col_types = c("text",
                                                                      "text", "text", "numeric", "text",
                                                                      "numeric", "text", "numeric"))

# Buprenorphine scot data
scot.raw.bup.paid <- read_excel(paste0(data_folder, "drugs/we01052022 Buprenorphine_2MG_8MG_16MG.xlsx"),
                                       sheet = "Context ePr Vs Paid", col_types = c("text",
                                                                                    "text", "text", "text", "numeric",
                                                                                    "text", "numeric", "text", "numeric"))

#formatting meth Paid
raw.meth.paid<-raw.meth.paid[-c(1:6,nrow(raw.meth.paid)),]  #Removing first 6 rows
colnames(raw.meth.paid)<-c('Board','Year month','Type1','Items','Quantity','Quantity per item')

scot.raw.meth.paid<-scot.raw.meth.paid[-c(1:3,(nrow(scot.raw.meth.paid)-1):nrow(scot.raw.meth.paid)),c(1,3,5)]#Removing rows and keeping columns for scotland data
scot.raw.meth.paid<-data.frame('Board'=rep('Scotland',nrow(scot.raw.meth.paid)),scot.raw.meth.paid)
colnames(scot.raw.meth.paid)<-c('Board','Year month','Quantity','Items')

#formatting bup paid
raw.bup.paid<-raw.bup.paid[-c(1:6),c(1:4,6,8)]
colnames(raw.bup.paid)<-c('Board','Year month','Type1','Items','Quantity','Quantity per item')

scot.raw.bup.paid<-scot.raw.bup.paid[-c(1:3,(nrow(scot.raw.bup.paid)-1):nrow(scot.raw.bup.paid)),c(1,5,7)]
scot.raw.bup.paid<-data.frame('Board'=rep('Scotland',nrow(scot.raw.meth.paid)),scot.raw.bup.paid)
colnames(scot.raw.bup.paid)<-c('Board','Year month','Quantity','Items')


#Creating new Type variable for Methadone and Buprenorphine
meth.paid<-cbind(raw.meth.paid,'Type'=rep('Methadone',nrow(raw.meth.paid)))
bup.paid<-cbind(raw.bup.paid,'Type'=rep('Buprenorphine',nrow(raw.bup.paid)))
scot.meth.paid<-cbind(scot.raw.meth.paid,'Type'=rep('Methadone',nrow(scot.raw.meth.paid)))
scot.bup.paid<-cbind(scot.raw.bup.paid,'Type'=rep('Buprenorphine',nrow(scot.raw.bup.paid)))

scot<-rbind(scot.meth.paid,scot.bup.paid)#Scotland data

paid<-rbind(meth.paid,bup.paid)#NHS board data

paid1<-paid


paid1<-paid1 %>% 
  separate(`Year month`, c("Year", "Month"),' ')

scot<- scot %>% 
  separate(`Year month`, c("Year", "Month"),' ')

Date<-as.character(as.yearmon(paste(paid1$Year, paid1$Month, sep="-")))#Changing year and month into date
paid1<-data.frame(paid1, 'Date'=Date)
# scot$Date<-paste(scot$Month,scot$Year,  sep=" ")

Date_scot<-as.character(as.yearmon(paste(scot$Year, scot$Month, sep="-")))#Changing year and month into date
scot1<-data.frame(scot, 'Date'=Date_scot)

#Getting all variables to create complete data frame
paid.dates<-unique(Date)
types<-unique(paid1$Type)
locations<-unique(paid1$Board) 
types1<-unique(paid1$Type1)#Form type code: to be removed later


paid.comp<-complete(paid1,Date=paid.dates,Board=locations,Type=types,Type1=types1,
                    fill=list(Items=0,Quantity=0,`Quantity per item`=0))

# New Aggregating method


# 4a) Quantity/Items with two year average

# Health Board Level
HB_Level <- paid.comp %>% 
  filter(!is.na(Year)) %>% 
  group_by(Board, Date, Month,Year, Type) %>% 
  summarise(Items = sum(Items),
            Quantity= sum(Quantity)) %>% 
  ungroup()

# Pivot and calulate two year average for items

Items_Average <- HB_Level %>%  
  select(-Date,-Quantity) %>% 
  pivot_wider(names_from = Year, values_from = Items) %>% 
  mutate(across(c(`2018`:`2020`), ~replace_na(.x, 0))) %>% 
  rowwise() %>% 
  mutate('Average 2018 & 2019' = mean(c(`2018`,`2019`))) %>% 
  # pivot back to years
  pivot_longer(cols = c('2020':'Average 2018 & 2019'), names_to = 'Year', values_to = 'Items' ) %>% 
  select(-'2018',-'2019')

# Pivot and calulate two year average for quantity
Quantity_Average <- HB_Level %>%  
  select(-Date,-Items) %>% 
  pivot_wider(names_from = Year, values_from = Quantity) %>% 
  mutate(across(c(`2018`:`2020`), ~replace_na(.x, 0))) %>% 
  rowwise() %>% 
  mutate('Average 2018 & 2019' = mean(c(`2018`,`2019`))) %>% 
  # pivot back to years
  pivot_longer(cols = c('2020':'Average 2018 & 2019'), names_to = 'Year', values_to = 'Quantity' ) %>% 
  select(-'2018',-'2019')

# Match them back together

HB_Level <- full_join(Items_Average,Quantity_Average)

# Calculate quantity per item
HB_Level <- HB_Level %>% 
  mutate('Quantity per item' = Quantity/Items)

# Pivot to correct format
HB_Level <- HB_Level %>% 
  pivot_longer(cols = c('Items':'Quantity per item'), names_to = 'Measurement', values_to = 'Number' ) %>% 
  # pivot out all years
  pivot_wider(names_from ='Year', values_from = 'Number') %>% 
  # pivot back 2020 and 2021 only
  pivot_longer(cols = c('2020':'2022'), names_to = 'Year', values_to = '2020 & 2021' ) %>% 
  # Due to pivoting extra months/years have been created - filter down to Feb 2022
  filter(!(as.numeric(Year) == 2022  & as.numeric(Month) >=3))

# Create date variable
HB_Level <- HB_Level %>% 
  arrange(Board,Type,Measurement,Year,Month) %>% 
  mutate(Date = str_c(month.abb[as.numeric(Month)],' ', Year)) %>% 
  select(-Year,-Month)

# Check structure
str(HB_Level)
# Change to factors were necessary
HB_Level <- HB_Level %>% 
  mutate(Date = factor(Date),
         Measurement = factor(Measurement))
str(HB_Level)
# So this is seeming OK for HB level at the moment. 

# Scotland level

# Pivot and calulate two year average for items

Items_Average <- scot %>%  
  select(-Quantity) %>% 
  pivot_wider(names_from = Year, values_from = Items) %>% 
  mutate(across(c(`2018`:`2020`), ~replace_na(.x, 0))) %>% 
  rowwise() %>% 
  mutate('Average 2018 & 2019' = mean(c(`2018`,`2019`))) %>% 
  # pivot back to years
  pivot_longer(cols = c('2020':'Average 2018 & 2019'), names_to = 'Year', values_to = 'Items' ) %>% 
  select(-'2018',-'2019')

# Pivot and calulate two year average for quantity
Quantity_Average <- scot %>%  
  select(-Items) %>% 
  pivot_wider(names_from = Year, values_from = Quantity) %>% 
  mutate(across(c(`2018`:`2020`), ~replace_na(.x, 0))) %>% 
  rowwise() %>% 
  mutate('Average 2018 & 2019' = mean(c(`2018`,`2019`))) %>% 
  # pivot back to years
  pivot_longer(cols = c('2020':'Average 2018 & 2019'), names_to = 'Year', values_to = 'Quantity' ) %>% 
  select(-'2018',-'2019')

# Match them back together

Scotland_Level <- full_join(Items_Average,Quantity_Average)

# Calculate quantity per item
Scotland_Level <- Scotland_Level %>% 
  mutate('Quantity per item' = Quantity/Items)

# Pivot to correct format
Scotland_Level <- Scotland_Level %>% 
  pivot_longer(cols = c('Items':'Quantity per item'), names_to = 'Measurement', values_to = 'Number' ) %>% 
  # pivot out all years
  pivot_wider(names_from ='Year', values_from = 'Number') %>% 
  # pivot back 2020 and 2021 only
  pivot_longer(cols = c('2020':'2022'), names_to = 'Year', values_to = '2020 & 2021' ) %>% 
  # Due to pivoting extra months/years have been created - filter down to Feb 2022
  filter(!(as.numeric(Year) == 2022  & as.numeric(Month) >=3))

# Create date variable
Scotland_Level <- Scotland_Level %>% 
  arrange(Board,Type,Measurement,Year,Month) %>% 
  mutate(Date = str_c(month.abb[as.numeric(Month)],' ', Year)) %>% 
  select(-Year,-Month)

# Check structure
str(Scotland_Level)
# Change to factors were necessary
Scotland_Level <- Scotland_Level %>% 
  mutate(Date = factor(Date),
         Measurement = factor(Measurement)) 
str(Scotland_Level)

# Add Scotland and HB together

Totals <- rbind(Scotland_Level,HB_Level)

str(Totals)
# Some final changes to this file
Totals <- Totals %>% 
  mutate(Board =str_to_title(Board)) %>% 
  mutate(Board = str_replace(Board,'Nhs','NHS')) %>% 
  mutate(across(c(`Average 2018 & 2019`:`2020 & 2021`), ~replace_na(.x, 0))) %>% 
  mutate(`Average 2018 & 2019` = round_half_up(`Average 2018 & 2019`, 2),
         `2020 & 2021` = round_half_up(`2020 & 2021`, 1)) %>% 
  select(Date,Board,Type,Measurement,'2020 & 2021',	'Average 2018 & 2019')

str(Totals)



# 4b) Quantity over time from January 2018. 

# Health Board Level
HB_Level_Quant <- paid.comp %>% 
  filter(!is.na(Year)) %>% 
  group_by(Board, Date, Month,Year, Type) %>% 
  summarise(Items = sum(Items),
            Quantity= sum(Quantity)) %>% 
  ungroup() %>% 
  select(Date,Board,Type,Quantity)

# Scotland level is available in scot file already created. 
Scot_Level_Quant <- scot %>% 
  mutate( Date = str_c(month.abb[as.numeric(Month)],' ', Year)) %>% 
  select(Date,Board,Type,Quantity) %>% 
  filter(!is.na(Quantity))

# Combine
Total_Quant <- rbind(Scot_Level_Quant,HB_Level_Quant)
str(Total_Quant)


Total_Quant <- Total_Quant %>% 
  mutate(Board =str_to_title(Board)) %>% 
  mutate(Board = str_replace(Board,'Nhs','NHS')) %>% 
  mutate(Date = factor(Date))  

# Create skeleton to match on data to
skeleton <- expand.grid(Date = factor(unique(Total_Quant$Date)),
                        Board = unique(Total_Quant$Board),
                        Type = factor(unique(Total_Quant$Type)))

str(skeleton)
unique(skeleton$Date)
# Correct levels of factors
skeleton <- skeleton %>% 
  mutate(Date = fct_relevel(Date,c(as.character(unique(Total_Quant$Date)))))

unique(skeleton$Date)

Total_Quant <- full_join(skeleton,Total_Quant)

str(Total_Quant)

Total_Quant <- Total_Quant %>% 
  mutate(across(c(Quantity), ~replace_na(.x, 0)))


# Change name of 2020/2021 variable
Totals <- Totals %>% 
  rename(`2020, 2021 & 2022` = `2020 & 2021`)

# SAVING files
saveRDS(Totals, "shiny_app/data/OST_paid.rds")
saveRDS(Totals, paste0(data_folder,"final_app_files/OST_paid_", 
                            format(Sys.Date(), format = '%d_%b_%y'), ".rds"))

saveRDS(Total_Quant, "shiny_app/data/OST_paid_quantity.rds")
saveRDS(Total_Quant, paste0(data_folder,"final_app_files/OST_paid_quantity_",
                               format(Sys.Date(), format = '%d_%b_%y'), ".rds"))



###############################################.
## A&E data prep ----
###############################################.

Drug_AE_attendances <- readRDS(paste0(data_folder,"drugs/A&E_Scotland_HB_Gender_2020_to_30-04-2022.RDS"))

Drug_AE_attendances <- Drug_AE_attendances %>%
  rename(Type = DrugsAlcBothNone,
         `Average 2018 & 2019` = avg_1819_ma,
         `2020 & 2021` = Observed.20.21.22_ma,
         Board = Geography,
         Date = WeekBeginning) %>%
  mutate(Type = as.factor(Type),
         Board = as.factor(Board)) %>%
  mutate(Type = forcats::fct_recode(Type, 
                                    "Drug Overdose/Intoxication" = "Drug",
                                    'Alcohol Overdose/Intoxication' = "Alcohol",
                                    'Drug and Alcohol Overdose/Intoxication' = "Both"))

## Restricting to only Drug attendences (includes Drug+OD and Drug+Alc+OD attendances)
Drug_AE_attendances <- Drug_AE_attendances %>%
  filter(Type == "Drug Overdose/Intoxication") %>%
  # mutate(`2020 & 2021` = plyr::round_any(`2020 & 2021`, 5, f = round),
  #        `Average 2018 & 2019`= plyr::round_any(`Average 2018 & 2019`, 5, f = round))
  mutate(`2020 & 2021` = if_else(`2020 & 2021` > 0 & `2020 & 2021` < 5, NA_real_, `2020 & 2021`),
         `Average 2018 & 2019`= if_else(`Average 2018 & 2019` > 0 & `Average 2018 & 2019` < 5, NA_real_, `Average 2018 & 2019`)) %>%
  group_by(Board) %>%
  mutate(avg = mean(`2020 & 2021`[Gender == "All"], na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(`2020 & 2021` = if_else(avg < 10, NA_real_, `2020 & 2021`),
         `Average 2018 & 2019` = if_else(avg < 10, NA_real_, `Average 2018 & 2019`))


#calculating percent change
Drug_AE_attendances <- Drug_AE_attendances %>%
  rowwise() %>%
  mutate(Change = (`2020 & 2021`- `Average 2018 & 2019`)/`Average 2018 & 2019`*100,
         Change = replace_na(Change, 0),
         Change = replace(Change, is.nan(Change), 0),
         Change = replace(Change, is.infinite(Change), NA_real_),
         Change = as.numeric(format(Change, nsmall = 1)))





saveRDS(Drug_AE_attendances, "shiny_app/data/Drug_AE_attendances.rds")
saveRDS(Drug_AE_attendances, paste0(data_folder,"final_app_files/Drug_AE_attendances_", 
                       format(Sys.Date(), format = '%d_%b_%y'), ".rds"))

## END
