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
#Line 52 and 68: Update number of weeks

# Reading data
Referrals_breakdown <- read_excel(paste0(data_folder,"drugs/Referrals_25022022_breakdown.xlsx"),
                                  col_types = c("text", "text", "date", "numeric")) %>% 
  #Extracting isoweek and year from date
  mutate(Week = isoweek(DateReferralReceived),
         Year = isoyear(DateReferralReceived))

colnames(Referrals_breakdown)[1:4]<-c('Type','Board','Date','DTR')

#Splitting data frame by years 
SepYears<-split(Referrals_breakdown, Year)

#Using all combinations of NHS board, type of treatment and date to make complete data frames
Hb<-unique(SepYears$`2018`$Board)#using 2018 data as 2015/2016 has NA as an option for health board
type<-c(unique(SepYears$`2021`$Type), 'All')#want to include co-dependency
dates<-unique(SepYears$`2020`$Week)#want to include leap year

#Forming complete data frames for each year
comp.2018<-complete(SepYears$`2018`, Board=Hb, Type=type, Week=dates,
                    fill=list(DTR=0)) %>% select(Week, Board, Type, `2018` = DTR)
comp.2019<-complete(SepYears$`2019`, Board=Hb, Type=type, Week=dates,
                    fill=list(DTR=0)) %>% select(`2019` = DTR)
comp.2020<-complete(SepYears$`2020`, Board=Hb, Type=type, Week=dates,
                    fill=list(DTR=0)) %>% select (`2020` = DTR)
comp.2021<-complete(SepYears$`2021`, Board=Hb, Type=type, Week=dates,
                    fill=list(DTR=0)) %>% select(`2021` = DTR)

#Merging the all years into one data set
full <- cbind(comp.2018, comp.2019, comp.2020, comp.2021) %>% 
  arrange(Week)

full$`2021`[(full$Week>51)]<-NA #MUST UPDATE THIS WITH FUTURE UPDATES
full$`2018`[(full$Week==53)]<-full$`2018`[(full$Week==52)] #REPEATING LAST 2018 & 2019 OBSERVATION TO MATCH 2020 DATA
full$`2019`[(full$Week==53)]<-full$`2019`[(full$Week==52)]

# rownames(full)<-seq(1:nrow(full))#Row names get jumbled up when data is reordered so just ordering it again here
full <- full %>% #Taking 2018/19 average
  mutate('Average 2018 & 2019' = (`2018`+`2019`)/2)

###adding % change column 
full <- full %>%
  mutate('Change' = (`2020` - `Average 2018 & 2019`)/`Average 2018 & 2019`*100)

full$Change[is.nan(full$Change)]<-NA # NaNs to NA

##### Formatting for long x axis 
####MUST UPDATE BLOCK FOR EACH UPDATE OF DATA WITH WEEK NUMBER
block <- nrow(subset(full, Week<=51))
long.axis <- rbind(full[,c(1:3,8,6)],full[c(1:(block+1)),c(1:3,8,6)])
long.axis$`2020`[c((nrow(full)+1):nrow(long.axis))]<-full$`2021`[1:(block+1)]


date_in_week <- function(year, week, weekday){
  'FUNCTION FOR CONVERTING ISO WEEK TO DATE'
  w <- paste0(year, "-W", sprintf("%02d", week), "-", weekday)
  ISOweek2date(w)
}

Date <- date_in_week(2020,sub.full[,1],1)
Date2 <- date_in_week(2021,sub.full[1:(block+1),1],1)
Date<-append(Date,Date2)

long.axis$Week<-Date

long.axis <- long.axis %>% rename(Date = Week, `2020 & 2021` = `2020`)
long.axis<-long.axis[-nrow(long.axis),]#removing last row 
#######Edit to add 'All' option###

iter<-seq(2,(nrow(long.axis)-2),4)

for(i in iter){
  'Adding drug and alcohol to All type'
  long.axis$`Average 2018 & 2019`[i]<-sum(long.axis$`Average 2018 & 2019`[i-1],long.axis$`Average 2018 & 2019`[i+1],long.axis$`Average 2018 & 2019`[i+2],na.rm=T)
  long.axis$`2020 & 2021`[i]<-sum(long.axis$`2020 & 2021`[i-1],long.axis$`2020 & 2021`[i+1],long.axis$`2020 & 2021`[i+2],na.rm=T)
}

# iter2<-seq(1,(nrow(long.axis)-3),4)
# for (i in iter2){
#   'This loop adds codependency values to both the alcohol and drug values'
#   'Co-dependency will not be shown as an option'
#   short.subset<-long.axis[c(i:(i+3)),]
#   a<-which(short.subset$Type=='Alcohol')
#   d<-which(short.subset$Type=='Drug')
#   c<-which(short.subset$Type=='Co-dependency')
#   short.subset$`Average 2018 & 2019`[a]<-short.subset$`Average 2018 & 2019`[a]+short.subset$`Average 2018 & 2019`[c]
#   short.subset$`Average 2018 & 2019`[d]<-short.subset$`Average 2018 & 2019`[d]+short.subset$`Average 2018 & 2019`[c]
#   short.subset$`2020 & 2021`[a]<-short.subset$`2020 & 2021`[a]+short.subset$`2020 & 2021`[c]
#   short.subset$`2020 & 2021`[d]<-short.subset$`2020 & 2021`[d]+short.subset$`2020 & 2021`[c]
#   long.axis[c(i:(i+3)),]<-short.subset
# }

#calculating percent change
long.axis<-cbind(long.axis,(long.axis$`2020 & 2021`- long.axis$`Average 2018 & 2019`)/long.axis$`Average 2018 & 2019`*100)
colnames(long.axis)[6]<-'Change'
long.axis$Change[is.nan(long.axis$Change)]<-0
long.axis$Change[is.infinite(long.axis$Change)]<-NA

long.axis$Change<-as.numeric(format(round(long.axis$Change, 1), nsmall = 1) )#rounding to 1dp

###Making all pre-DAISY co-dependency entries NA

long.axis<-long.axis %>% 
  mutate(`Average 2018 & 2019` = replace(`Average 2018 & 2019`, Type=='Co-dependency', NA))

long.axis<-long.axis %>% 
  mutate(`2020 & 2021` = replace(`2020 & 2021`,Date<'2020-12-01' & Type=='Co-dependency', NA))


# long.axis<-long.axis[which(long.axis$Type != 'Co-dependency'),] #removing co-dependency 

###making a file for names of health boards and names of ADPs###
Health_board<-Hb[grep('NHS',Hb)]
ADP_names<-Hb[grep('ADP',Hb)]

#SAVING FILES
saveRDS(ADP_names, "shiny_app/data/ADP_names.rds")
saveRDS(ADP_names, paste0(data_folder,"final_app_files/ADP_names_",
                             format(Sys.Date(), format = '%d_%b_%y'), ".rds"))

saveRDS(Health_board, "shiny_app/data/Health_board.rds")
saveRDS(Health_board, paste0(data_folder,"final_app_files/Health_board_",
                          format(Sys.Date(), format = '%d_%b_%y'), ".rds"))

saveRDS(long.axis,file="shiny_app/data/DTR_data.rds")
saveRDS(long.axis, paste0(data_folder,"final_app_files/DTR_data_",
                          format(Sys.Date(), format = '%d_%b_%y'), ".rds"))

###############################################.
## Take Home Naloxone ----
###############################################.
#When updating for future updates the following lines must be updated:
#Line 156: Update filepath to data
#Line 177: Update number of months of 2021 data 

# Reading file
dashboard_monthly_data <- readRDS(paste0(data_folder, "drugs/dashboard_monthly_data_2021_Q2.rds"))

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

block<-nrow(subset(comp.data,Date<=9))#CHANGE WHEN UPDATING DATA:replace with number of months of 2021 data available
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
SAS_data <- read_excel(paste0(data_folder, "drugs/2022_1_SAS_reformat_January.xlsx"))

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
raw.meth.paid <- read_excel(paste0(data_folder, "drugs/we06032022_Methadone 1mg.ml-1.xlsx"),
                                   sheet = "Paid Items", 
                            col_types = c("text","text", "text", "numeric", "numeric", "numeric"))                                                                           

# Methadone scot data
scot.raw.meth.paid <- read_excel(paste0(data_folder, "drugs/we06032022_Methadone 1mg.ml-1.xlsx"),
                                        sheet = "Context ePr Vs Paid", col_types = c("text",
                                                                                     "text", "numeric", "text", "numeric"))

# Buprenorphine HB data
raw.bup.paid <- read_excel(paste0(data_folder, "drugs/we06032022 Buprenorphine_2MG_8MG_16MG.xlsx"),
                                  sheet = "Paid Items", col_types = c("text",
                                                                      "text", "text", "numeric", "text",
                                                                      "numeric", "text", "numeric"))

# Buprenorphine scot data
scot.raw.bup.paid <- read_excel(paste0(data_folder, "drugs/we06032022 Buprenorphine_2MG_8MG_16MG.xlsx"),
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
  pivot_longer(cols = c('2020':'2021'), names_to = 'Year', values_to = '2020 & 2021' )

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
  pivot_longer(cols = c('2020':'2021'), names_to = 'Year', values_to = '2020 & 2021' )

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
         Measurement = factor(Measurement)) %>% 
  select(-'2022')
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

Drug_AE_attendances <- readRDS("//PHI_conf/SubstanceMisuse1/Topics/Surveillance/COVID/Dashboard/Drug_related_AE_attendances/A&E_Scotland_HB_2020_to_28-02-2022.RDS")

Drug_AE_attendances <- Drug_AE_attendances %>%
  # filter(Geography_type == "Scotland") %>%
  # select(-Geography_type) %>%
  # rename(Board = Geography) %>%
  # bind_rows(Drug_AE_attendances %>%
  #             filter(Geography_type != "Scotland") %>%
  #             select(-Geography_type) %>%
  #             rename(Board = Geography)) %>%
  rename(Type = DrugsAlcBothNone,
         `Average 2018 & 2019` = avg_1819_ma,
         `2020 & 2021` = Observed.20.21.22_ma,
         Board = Geography,
         Date = WeekBeginning) %>%
  mutate(Type = as.factor(Type),
         Board = as.factor(Board)) %>%
  mutate(Type = forcats::fct_recode(Type, 
                                    "Drug Overdoses" = "Drug",
                                    'Alcohol Overdoses' = "Alcohol",
                                    'Drug and Alcohol Overdoses' = "Both"))


# # Censor data cells < 5
# Drug_AE_attendances <- Drug_AE_attendances %>%
#   filter(!(`2020 & 2021` <= 5 | `Average 2018 & 2019` <=5))

saveRDS(Drug_AE_attendances, "shiny_app/data/Drug_AE_attendances.rds")
saveRDS(Drug_AE_attendances, paste0(data_folder,"final_app_files/Drug_AE_attendances_", 
                       format(Sys.Date(), format = '%d_%b_%y'), ".rds"))

## END
