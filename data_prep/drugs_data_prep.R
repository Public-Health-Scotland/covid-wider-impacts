###############################################.
## Functions/Packages/filepaths/lookups ----
###############################################.
source("data_prep/functions_packages_data_prep.R")


##########################################################
# Name of file - Drug and Alcohol Treatment referral data 
# Original author(s) - Niamh Graham
# Original date - 12/07/2021
# Latest update author - Niamh Graham
# Latest update date - N/A
# Latest update description - 
# Type of script - Data preparation 
# Written/run on - R Studio Desktop - 1.1.456
# Version of R - 3.6.1

# Script for data preparation for drug treatment referrals for 
# covid wider impacts dashboard

# 
# Results are data in format for use in the dashboard 
# put into long format for subsetting specific locations etc
# 


####Drug and alcohol treatment referrals####

library(readxl)
# FOR SUBSTANCE USE TEAM TO RUN SCRIPT
# Referrals_breakdown <- read_excel("/PHI_conf/SubstanceMisuse1/Topics/Surveillance/COVID/Dashboard/Niamh/DrugTreatmentReferrals/Referrals_20210715_breakdown.xlsx", 
#                                   col_types = c("text", "text", "date", 
#                                                 "numeric"))

# FOR WIDER IMPACTS TEAM TO RUN SCRIPT
Referrals_breakdown <- read_excel(paste0(data_folder,"drugs/Referrals_20210715_breakdown.xlsx"),
                                         col_types = c("text", "text", "date", 
                                                       "numeric"))


library(lubridate)
#Need to change dates to isoweeks and years for comparison and to standardise with 
#other plots etc
Week<-isoweek(Referrals_breakdown$DateReferralReceived)
Year<-isoyear(Referrals_breakdown$DateReferralReceived)

Referrals_breakdown<-data.frame(data.frame(Referrals_breakdown,'Week'=Week),'Year'=Year)

colnames(Referrals_breakdown)[1:4]<-c('Type','Board','Date','DTR')

SepYears<-split(Referrals_breakdown, Year)

data.2018<-SepYears$`2018`
data.2019<-SepYears$`2019`
data.2020<-SepYears$`2020`
data.2021<-SepYears$`2021`


library(tidyr)


Hb<-unique(data.2018$Board)#using 2018 data as 2015/2016 has NA as an option for health board
type<-c(unique(data.2021$Type), 'All')#want to include co-dependency
dates<-unique(data.2020$Week)#want to include leap year

comp.2018<-complete(data.2018, Board=Hb, Type=type, Week=dates,
                    fill=list(DTR=0))
comp.2019<-complete(data.2019, Board=Hb, Type=type, Week=dates,
                    fill=list(DTR=0))
comp.2020<-complete(data.2020, Board=Hb, Type=type, Week=dates,
                    fill=list(DTR=0))
comp.2021<-complete(data.2021, Board=Hb, Type=type, Week=dates,
                    fill=list(DTR=0))


full<-merge(comp.2018,comp.2019,by=c('Week','Board','Type'))
full<-merge(full,comp.2020,by=c('Week','Board','Type'))
full<-merge(full,comp.2021,by=c('Week','Board','Type'))

full<-full[order(full$Week),]
sub.full<-full[,c(1:3,5,8,11,14)]
colnames(sub.full)[4:7]<-c('2018','2019','2020','2021')

sub.full$`2021`[(sub.full$Week>28)]<-NA
sub.full$`2018`[(sub.full$Week==53)]<-sub.full$`2018`[(sub.full$Week==52)] #REPEATING LAST 2018 & 2019 OBSERVATION TO MATCH 2020 DATA
sub.full$`2019`[(sub.full$Week==53)]<-sub.full$`2019`[(sub.full$Week==52)]



rownames(sub.full)<-seq(1:nrow(sub.full))

average<-(sub.full$`2018`+sub.full$`2019`)/2
sub.full<-cbind(sub.full,average)
colnames(sub.full)[8]<-'Average 2018 & 2019'

###adding % change column 
sub.full<-cbind(sub.full,(sub.full$`2020`- sub.full$`Average 2018 & 2019`)/sub.full$`Average 2018 & 2019`*100)
colnames(sub.full)[9]<-'Change'
sub.full$Change[is.nan(sub.full$Change)]<-NA


##### Formatting for long x axis 

long.axis<-rbind(sub.full[,c(1:3,8,6)],sub.full[c(1:5153),c(1:3,8,6)])
#long.axis$Week[1:nrow(download.DTR)]<-date_in_week(2020,long.axis$Week[1:nrow(download.DTR)],1)
long.axis$`2020`[c(9753:nrow(long.axis))]<-sub.full$`2021`[1:5153]

library(ISOweek)
date_in_week <- function(year, week, weekday){
  'FUNCTION FOR CONVERTING ISO WEEK TO DATE'
  w <- paste0(year, "-W", sprintf("%02d", week), "-", weekday)
  ISOweek2date(w)
}

Date<-date_in_week(2020,sub.full[,1],1)
Date2<-date_in_week(2021,sub.full[1:5153,1],1)
Date<-append(Date,Date2)

long.axis$Week<-Date

colnames(long.axis)[c(1,5)]<-c('Date','2020 & 2021')

#######edit to add 'All' option###
#want to add codependency to alcohol and drug after all has been calculated
iter<-seq(2,14903,4)

for(i in iter){
  long.axis$`Average 2018 & 2019`[i]<-sum(long.axis$`Average 2018 & 2019`[i-1],long.axis$`Average 2018 & 2019`[i+1],long.axis$`Average 2018 & 2019`[i+2],na.rm=T)
  long.axis$`2020 & 2021`[i]<-sum(long.axis$`2020 & 2021`[i-1],long.axis$`2020 & 2021`[i+1],long.axis$`2020 & 2021`[i+2],na.rm=T)
}

iter2<-seq(1,14902,4)
for (i in iter2){
  'This loop adds codependency values to both the alcohol and drug values'
  'Co-dependency will not be shown as an option'
  short.subset<-long.axis[c(i:(i+3)),]
  a<-which(short.subset$Type=='Alcohol')
  d<-which(short.subset$Type=='Drug')
  c<-which(short.subset$Type=='Co-dependency')
  short.subset$`Average 2018 & 2019`[a]<-short.subset$`Average 2018 & 2019`[a]+short.subset$`Average 2018 & 2019`[c]
  short.subset$`Average 2018 & 2019`[d]<-short.subset$`Average 2018 & 2019`[d]+short.subset$`Average 2018 & 2019`[c]
  short.subset$`2020 & 2021`[a]<-short.subset$`2020 & 2021`[a]+short.subset$`2020 & 2021`[c]
  short.subset$`2020 & 2021`[d]<-short.subset$`2020 & 2021`[d]+short.subset$`2020 & 2021`[c]
  long.axis[c(i:(i+3)),]<-short.subset
}


long.axis<-cbind(long.axis,(long.axis$`2020 & 2021`- long.axis$`Average 2018 & 2019`)/long.axis$`Average 2018 & 2019`*100)
colnames(long.axis)[6]<-'Change'
long.axis$Change[is.nan(long.axis$Change)]<-0
long.axis$Change[is.infinite(long.axis$Change)]<-NA

long.axis$Change<-as.numeric(format(round(long.axis$Change, 1), nsmall = 1) )

###Making all pre-DAISY co-dependency entries NA
library(tidyverse)
long.axis<-long.axis %>% 
  mutate(`Average 2018 & 2019` = replace(`Average 2018 & 2019`, Type=='Co-dependency', NA))

long.axis<-long.axis %>% 
  mutate(`2020 & 2021` = replace(`2020 & 2021`,Date<'2020-10-12' & Type=='Co-dependency', NA))




long.axis<-long.axis[which(long.axis$Type != 'Co-dependency'),] #removing co-dependency
long.axis<-long.axis[-nrow(long.axis),]
saveRDS(long.axis,file='DTR_July_update.rds')
###making a file for names of health boards and names of ADPs###



Health_board<-Hb[grep('NHS',Hb)]
ADP_names<-Hb[grep('ADP',Hb)]

#SAVING FOR SUBSTANCE USE TEAM
saveRDS(Health_board,file='Health_board.rds')
saveRDS(ADP_names,file='ADP_names.rds')

#SAVING FOR WIDER IMPACTS TEAM
saveRDS(Health_board, "shiny_app/data/Health_board.rds")
saveRDS(ADP_names, "shiny_app/data/ADP_names.rds")



#### take home naloxone####

# FOR SUBSTANCE USE TEAM TO RUN SCRIPT
# dashboard_monthly_data <- readRDS("/PHI_conf/SubstanceMisuse1/Topics/Naloxone/Projects/20200515-COVID19-Naloxone/Temp/dashboard_monthly_data.rds")

# FOR WIDER IMPACTS TEAM TO RUN SCRIPT
dashboard_monthly_data <- readRDS(paste0(data_folder, "drugs/dashboard_monthly_data.rds"))

HB_data<-dashboard_monthly_data[order(dashboard_monthly_data$month),] #ordering by date

HB_data<-HB_data[,c(3,2,1,4:8)]



colnames(HB_data)[1:3]<-c('Date','Board','Type')

hb<-unique(HB_data$Board)
types<-unique(HB_data$Type)
dates<-unique(HB_data$Date)
library(tidyr)
comp.data<-complete(HB_data, Board=hb, Type=types, Date=dates,
                    fill=list(`2020`=0,`2021`=0,`2019`=0,`2018`=0,two_year_avg=0))


comp.data<-comp.data[order(comp.data$Date),]
comp.data<-comp.data[,c(3,1,2,4:8)]

comp.data.temp<-rbind(comp.data[,c(1:3,8,6)],comp.data[c(1:225),c(1:3,8,6)])
comp.data.temp[c(901:1125),5]<-comp.data[c(1:225),7]

colnames(comp.data.temp)[c(4,5)]<-c('Average 2018 & 2019', '2020 & 2021')

Change<-(comp.data.temp$`2020 & 2021`-comp.data.temp$`Average 2018 & 2019`)*100/comp.data.temp$`Average 2018 & 2019`
Change[which(is.nan(Change))]<-0
THN_by_Hb<-cbind(comp.data.temp, 'Change'=Change)
THN_by_Hb$Date<-month.abb[THN_by_Hb$Date]

THN_by_Hb$Change[which(is.infinite(THN_by_Hb$Change))]<-NA

#SAVING FOR SUBSTANCE USE TEAM
saveRDS(THN_by_Hb,'THN_by_HB.rds')

# SAVING FOR WIDER IMPACTS TEAM
saveRDS(THN_by_Hb, "shiny_app/data/THN_by_Hb.rds")

#####adding proportion of type of THN### 

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

### want to make proportion into percentage instead####
proportion_current<-proportion_current*100

new_THN<-cbind(THN_by_Hb,'Proportion 20/21'=proportion_current)

new_THN$Change<-as.numeric(format(round(new_THN$Change, 1), nsmall = 1) )
new_THN$`Proportion 20/21`<-as.numeric(format(round(new_THN$`Proportion 20/21` ,1),nsmall=1))
'Now want to save new_THN as THN_by_Hb for use in the dashboard'

#SAVING FOR SUBSTANCE USE TEAM
saveRDS(new_THN,'THN_by_HB.rds')

# SAVING FOR WIDER IMPACTS TEAM
saveRDS(new_THN, "shiny_app/data/THN_by_Hb.rds")

