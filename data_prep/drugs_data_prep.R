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

# FOR SUBSTANCE USE TEAM TO RUN SCRIPT
Referrals_breakdown <- read_excel("/PHI_conf/SubstanceMisuse1/Topics/Surveillance/COVID/Dashboard/Niamh/DrugTreatmentReferrals/Referrals_20211026_breakdown.xlsx", 
                                   col_types = c("text", "text", "date", 
                                                 "numeric"))

# FOR WIDER IMPACTS TEAM TO RUN SCRIPT
# Referrals_breakdown <- read_excel(paste0(data_folder,"drugs/Referrals_20210715_breakdown.xlsx"),
#                                          col_types = c("text", "text", "date", 
#                                                        "numeric"))



#Extracting isoweek and year from date
Week<-isoweek(Referrals_breakdown$DateReferralReceived)
Year<-isoyear(Referrals_breakdown$DateReferralReceived)
Referrals_breakdown<-data.frame(Referrals_breakdown,'Week'=Week,'Year'=Year)

colnames(Referrals_breakdown)[1:4]<-c('Type','Board','Date','DTR')

#Splitting data frame by years 
SepYears<-split(Referrals_breakdown, Year)
data.2018<-SepYears$`2018`
data.2019<-SepYears$`2019`
data.2020<-SepYears$`2020`
data.2021<-SepYears$`2021`


#Using all combinations of NHS board, type of treatment and date to make complete data frames
Hb<-unique(data.2018$Board)#using 2018 data as 2015/2016 has NA as an option for health board
type<-c(unique(data.2021$Type), 'All')#want to include co-dependency
dates<-unique(data.2020$Week)#want to include leap year


#Forming complete data frames for each year
comp.2018<-complete(data.2018, Board=Hb, Type=type, Week=dates,
                    fill=list(DTR=0))
comp.2019<-complete(data.2019, Board=Hb, Type=type, Week=dates,
                    fill=list(DTR=0))
comp.2020<-complete(data.2020, Board=Hb, Type=type, Week=dates,
                    fill=list(DTR=0))
comp.2021<-complete(data.2021, Board=Hb, Type=type, Week=dates,
                    fill=list(DTR=0))

#Merging the all years into one data set
full<-merge(comp.2018,comp.2019,by=c('Week','Board','Type'))
full<-merge(full,comp.2020,by=c('Week','Board','Type'))
full<-merge(full,comp.2021,by=c('Week','Board','Type'))


full<-full[order(full$Week),]
sub.full<-full[,c(1:3,5,8,11,14)]
colnames(sub.full)[4:7]<-c('2018','2019','2020','2021')

sub.full$`2021`[(sub.full$Week>12)]<-NA #MUST UPDATE THIS WITH FUTURE UPDATES
sub.full$`2018`[(sub.full$Week==53)]<-sub.full$`2018`[(sub.full$Week==52)] #REPEATING LAST 2018 & 2019 OBSERVATION TO MATCH 2020 DATA
sub.full$`2019`[(sub.full$Week==53)]<-sub.full$`2019`[(sub.full$Week==52)]

rownames(sub.full)<-seq(1:nrow(sub.full))#Row names get jumbled up when data is reordered so just ordering it again here

average<-(sub.full$`2018`+sub.full$`2019`)/2 #Taking 2018/19 average
sub.full<-cbind(sub.full,average)
colnames(sub.full)[8]<-'Average 2018 & 2019'

###adding % change column 
sub.full<-cbind(sub.full,(sub.full$`2020`- sub.full$`Average 2018 & 2019`)/sub.full$`Average 2018 & 2019`*100)
colnames(sub.full)[9]<-'Change'
sub.full$Change[is.nan(sub.full$Change)]<-NA


##### Formatting for long x axis 
####MUST UPDATE BLOCK FOR EACH UPDATE OF DATA WITH WEEK NUMBER
block<-nrow(subset(sub.full,Week<=12))
long.axis<-rbind(sub.full[,c(1:3,8,6)],sub.full[c(1:(block+1)),c(1:3,8,6)])
long.axis$`2020`[c((nrow(sub.full)+1):nrow(long.axis))]<-sub.full$`2021`[1:(block+1)]


date_in_week <- function(year, week, weekday){
  'FUNCTION FOR CONVERTING ISO WEEK TO DATE'
  w <- paste0(year, "-W", sprintf("%02d", week), "-", weekday)
  ISOweek2date(w)
}

Date<-date_in_week(2020,sub.full[,1],1)
Date2<-date_in_week(2021,sub.full[1:(block+1),1],1)
Date<-append(Date,Date2)

long.axis$Week<-Date

colnames(long.axis)[c(1,5)]<-c('Date','2020 & 2021')
long.axis<-long.axis[-nrow(long.axis),]#removing last row 
#######Edit to add 'All' option###
#Want to add codependency to alcohol and drug after all has been calculated
iter<-seq(2,(nrow(long.axis)-2),4)

for(i in iter){
  'Adding drug and alcohol to All type'
  long.axis$`Average 2018 & 2019`[i]<-sum(long.axis$`Average 2018 & 2019`[i-1],long.axis$`Average 2018 & 2019`[i+1],long.axis$`Average 2018 & 2019`[i+2],na.rm=T)
  long.axis$`2020 & 2021`[i]<-sum(long.axis$`2020 & 2021`[i-1],long.axis$`2020 & 2021`[i+1],long.axis$`2020 & 2021`[i+2],na.rm=T)
}

iter2<-seq(1,(nrow(long.axis)-3),4)
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
  mutate(`2020 & 2021` = replace(`2020 & 2021`,Date<'2020-10-12' & Type=='Co-dependency', NA))


long.axis<-long.axis[which(long.axis$Type != 'Co-dependency'),] #removing co-dependency 


###making a file for names of health boards and names of ADPs###
Health_board<-Hb[grep('NHS',Hb)]
ADP_names<-Hb[grep('ADP',Hb)]

#SAVING FOR SUBSTANCE USE TEAM
saveRDS(Health_board,file='Health_board.rds')
saveRDS(ADP_names,file='ADP_names.rds')
saveRDS(long.axis,file='DTR_data.rds')

#SAVING FOR WIDER IMPACTS TEAM
saveRDS(Health_board, "shiny_app/data/Health_board.rds")
saveRDS(ADP_names, "shiny_app/data/ADP_names.rds")
saveRDS(long.axis,file="shiny_app/data/DTR_data.rds")

###############################################.
## Take Home Naloxone ----
###############################################.

# FOR SUBSTANCE USE TEAM TO RUN SCRIPT
dashboard_monthly_data <- readRDS("/PHI_conf/SubstanceMisuse1/Topics/Naloxone/Projects/20200515-COVID19-Naloxone/Temp/dashboard_monthly_data.rds")

# FOR WIDER IMPACTS TEAM TO RUN SCRIPT
#dashboard_monthly_data <- readRDS(paste0(data_folder, "drugs/dashboard_monthly_data.rds"))

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

block<-nrow(subset(comp.data,Date<=3))#CHANGE WHEN UPDATING DATA:replace 3 with number of months of data available
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


#SAVING FOR SUBSTANCE USE TEAM
saveRDS(new_THN,'THN_by_HB.rds')

# SAVING FOR WIDER IMPACTS TEAM
saveRDS(new_THN, "shiny_app/data/THN_by_HB.rds")

###############################################.
## SAS data prep ----
###############################################.

SAS_data<- read_excel("/PHI_conf/SubstanceMisuse1/Topics/Surveillance/COVID/Dashboard/Niamh/SAS/SAS_reformat.xlsx", 
                      sheet = "HB-DATA")


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

#Variable for length of a weeks worth of data as have to repeat last week of 2018/19 data to 
#Match up with 2020 data
diff<-nrow(data.2020)-nrow(data.2018)

#Repeating last week of 2018
data.2018<-rbind(data.2018,data.2018[c((nrow(data.2018)-diff+1):nrow(data.2018)),])
data.2019<-rbind(data.2019,data.2019[c((nrow(data.2019)-diff+1):nrow(data.2019)),])

#Adding 2021 block to end of all data frames
data.2018<-rbind(data.2018, data.2018[c(1:nrow(data.2021)),])
data.2019<-rbind(data.2019, data.2019[c(1:nrow(data.2021)),])
data.2020.2021<-rbind(data.2020,data.2021)

#0 data comes in as NA from excel so needs to be changed to 0
data.2020.2021$Number[which(is.na(data.2020.2021$Number))]<-0
data.2018$Number[which(is.na(data.2018$Number))]<-0
data.2019$Number[which(is.na(data.2019$Number))]<-0

#Calculating 2018 and 2019 Average 
average.1819<-(data.2018$Number+data.2019$Number)/2

data<- data.2020.2021 %>% 
  mutate(average= average.1819)

data<-data[,-4]#Removing Year column

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

data<-data[order(data$Board),]#Ordering by board

#Rounding to 1dp
data$`2020 & 2021`<-round(data$`2020 & 2021`,1)
data$`Average 2018 & 2019`<-round(data$`Average 2018 & 2019`,1)

#Creating variables for rolling average
rolling.18.19<-rep(0,nrow(data))
rolling.20.21<-rep(0,nrow(data))


section<-78 #number of weeks that data is collected for:MUST BE CHANGED MANUALLY WHEN DATA IS UPDATES
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

saveRDS(data, 'SASdata.rds')

# SAVING FOR WIDER IMPACTS TEAM
saveRDS(data, "shiny_app/data/SASdata.rds")


###############################################.
## OST data prep ----
###############################################.

raw.meth.paid<- read_excel("/PHI_conf/SubstanceMisuse1/Topics/Surveillance/COVID/Data/OST/200821/IR2020-00580 - Output Methadone 1mg.ml 200821_LB.xlsx", 
                           sheet = "Paid Items", col_types = c("text", 
                                                               "text", "text", "numeric", "numeric", 
                                                               "numeric"))
scot.raw.meth.paid<-read_excel("/PHI_conf/SubstanceMisuse1/Topics/Surveillance/COVID/Data/OST/200821/IR2020-00580 - Output Methadone 1mg.ml 200821_LB.xlsx", 
                               sheet = "Context ePr Vs Paid", col_types = c("text", 
                                                                            "date", "text", "numeric", "text", 
                                                                            "numeric", "numeric", "text", "numeric", 
                                                                            "numeric", "numeric", "numeric", 
                                                                            "numeric", "numeric", "numeric", 
                                                                            "numeric", "numeric", "numeric", 
                                                                            "numeric", "numeric", "text", "text", 
                                                                            "numeric"))

raw.bup.paid<-read_excel("/PHI_conf/SubstanceMisuse1/Topics/Surveillance/COVID/Data/OST/200821/IR2020-00580 - Output Buprenorphine 2MG_8MG_16MG 200821_LB.xlsx", 
                         sheet = "Paid Items", col_types = c("text", 
                                                             "text", "text", "numeric", "numeric", 
                                                             "numeric"))
scot.raw.bup.paid<-read_excel("/PHI_conf/SubstanceMisuse1/Topics/Surveillance/COVID/Data/OST/200821/IR2020-00580 - Output Buprenorphine 2MG_8MG_16MG 200821_LB.xlsx", 
                              sheet = "Context ePr Vs Paid", col_types = c("text", 
                                                                           "date", "text", "text", "numeric", 
                                                                           "numeric", "text", "numeric", "text", 
                                                                           "numeric"))

#formatting meth Paid
raw.meth.paid<-raw.meth.paid[-c(1:6,nrow(raw.meth.paid)),]  #Removing first 6 rows
colnames(raw.meth.paid)<-c('Board','Year month','Type1','Items','Quantity','Quantity per item')

scot.raw.meth.paid<-scot.raw.meth.paid[-c(1:3,45:46),c(1,2,4,6,9)]#Removing rows and keeping columns for scotland data
scot.raw.meth.paid<-data.frame('Board'=rep('Scotland',nrow(scot.raw.meth.paid)),scot.raw.meth.paid)
colnames(scot.raw.meth.paid)<-c('Board','Year month','Date','Quantity','Items','Quantity per item')

#formatting bup paid
raw.bup.paid<-raw.bup.paid[-c(1:6),]
colnames(raw.bup.paid)<-c('Board','Year month','Type1','Items','Quantity','Quantity per item')

scot.raw.bup.paid<-scot.raw.bup.paid[-c(1:3,45:46),c(1,2,6,8,10)]
scot.raw.bup.paid<-data.frame('Board'=rep('Scotland',nrow(scot.raw.meth.paid)),scot.raw.bup.paid)
colnames(scot.raw.bup.paid)<-c('Board','Year month','Date','Quantity','Items','Quantity per item')


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
scot$Date<-as.character(as.yearmon(paste(scot$Year, scot$Month, sep="-")))

#Getting all variables to create complete data frame
paid.dates<-unique(Date)
types<-unique(paid1$Type)
locations<-unique(paid1$Board) 
types1<-unique(paid1$Type1)#Form type code: to be removed later


paid.comp<-complete(paid1,Date=paid.dates,Board=locations,Type=types,Type1=types1,
                    fill=list(Items=0,Quantity=0,`Quantity per item`=0))

remove.types1<-data.frame(matrix(nrow=nrow(paid.comp)/length(types1),ncol=3))#Making new data frame to remove form type code
colnames(remove.types1)<-c('Items','Quantity','Quantity per item')

#Aggregating data over the form type codes
remove.types1$Items<-colSums(matrix(paid.comp$Items, nrow=length(types1)))
remove.types1$Quantity<-colSums(matrix(paid.comp$Quantity, nrow=length(types1)))
remove.types1$`Quantity per item`<-colSums(matrix(paid.comp$Quantity.per.item, nrow=length(types1)),na.rm=TRUE)

#Making skeleton with all combinations of variables
skel<-expand.grid(types,locations,paid.dates)

paid.comp1<-data.frame(skel,remove.types1)
colnames(paid.comp1)[c(1:3,6)]<-c('Type','Board','Date','Quantity per item')

paid.comp1<-paid.comp1 %>% 
  separate(`Date`, c("Month", "Year"),' ')

scot<-scot %>% 
  separate(`Date`,c('Month','Year'),' ')

paid.comp1<-rbind(paid.comp1,scot)#Adding scotland level data
paid.comp1$Date<-as.yearmon(paste(paid.comp1$Month, paid.comp1$Year)) 
paid.comp1<-paid.comp1[order(paid.comp1$Date),]

#Splitting up into years
sep.paid<-split(paid.comp1,paid.comp1$Year)

paid.2018<-sep.paid$`2018`
paid.2019<-sep.paid$`2019`
paid.2020<-sep.paid$`2020`
paid.2021<-sep.paid$`2021`

paid.2020.2021<-rbind(paid.2020,paid.2021)#Adding 2020 and 2021 data
extended.paid.2018<-rbind(paid.2018,paid.2018[c(1:nrow(paid.2021)),])#Repeating 2018 data to match length of 2020/21 data
extended.paid.2019<-rbind(paid.2019,paid.2019[c(1:nrow(paid.2021)),])

extended.paid.average<-data.frame(extended.paid.2018[,1:4],
                                  'Items'=(extended.paid.2018$Items+extended.paid.2019$Items)/2,
                                  'Quantity'=(extended.paid.2018$Quantity+extended.paid.2019$Quantity)/2)

extended.paid.average<-data.frame(extended.paid.average,  #Calculating quantity per item 
                                  'Quantity per item'=extended.paid.average$Quantity/extended.paid.average$Items)

paid.2020.2021$`Quantity per item` <-paid.2020.2021$Quantity/paid.2020.2021$Items #Calculating quantity per item

extended.paid.average$Quantity.per.item[which(is.nan(extended.paid.average$Quantity.per.item))]<-0
paid.2020.2021$`Quantity per item`[which(is.nan(paid.2020.2021$`Quantity per item`))]<-0

Date<-paste(paid.2020.2021$Month,paid.2020.2021$Year,  sep=" ")
paid.2020.2021<-data.frame(paid.2020.2021,Date)
extended.paid.average<-data.frame(extended.paid.average,Date)

#Ordering data sets correctly for matching up
sorted.average<-extended.paid.average[
  with(extended.paid.average, order(Board,Type)),
  ]

sorted.20.21<-paid.2020.2021[
  with(paid.2020.2021, order(Board,Type)),
  ]

rownames(sorted.average)<-seq(1:nrow(sorted.average))
rownames(sorted.20.21)<-seq(1:nrow(sorted.20.21))

ave.long<-melt(data=sorted.average,
               id.vars=c('Date','Board','Type'),
               measure.vars=c('Items','Quantity','Quantity.per.item'),
               variable.name = 'Measurement',
               value.name='Value') 

pres.long<-melt(data=sorted.20.21,
                id.vars=c('Date.1','Board','Type'),
                measure.vars=c('Items','Quantity','Quantity.per.item'),
                variable.name = 'Measurement',
                value.name='Value')

paid.final<-data.frame(pres.long,ave.long$Value)
colnames(paid.final)[c(5,6)]<-c('2020 & 2021','Average 2018 & 2019')
levels(paid.final$Measurement)<-c('Items','Quantity','Quantity per item')
paid.final$Board<-gsub('Nhs','NHS',str_to_title(paid.final$Board)) #changing the case of the Board names

paid.final$`2020 & 2021`<-round(paid.final$`2020 & 2021`,1)
paid.final$`Average 2018 & 2019`<-round(paid.final$`Average 2018 & 2019`,1)

paid.quantity<- paid.comp1 %>% 
  select(Date, 
         Board, 
         Type, 
         Quantity)
paid.quantity$Quantity<-round(paid.quantity$Quantity)
paid.quantity$Board<-gsub('Nhs','NHS',str_to_title(paid.quantity$Board))


saveRDS(paid.final,'OST_paid.rds')
saveRDS(paid.final, "shiny_app/data/OST_paid.rds")
saveRDS(paid.quantity,'OST_paid_quantity.rds')
saveRDS(paid.quantity, "shiny_app/data/OST_paid_quantity.rds")



