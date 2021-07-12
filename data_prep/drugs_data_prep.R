
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


library(readxl)
data<- read_excel("/PHI_conf/SubstanceMisuse1/Topics/Surveillance/COVID/Data/DATWT/2019_21_DATWTs_data.xlsx", 
                  col_types = c("date", "text", "text", 
                                "text", "numeric"))
###Formatting Entire data set####

data<-data[-c(1:6),]

colnames(data)<-c('Date','Board','Prison','Type','Number of treatment referrals')
data$`Number of treatment referrals`<-as.numeric(data$`Number of treatment referrals`)

data<-data[which(data$Prison==FALSE),] #REMOVING PRISON NUMBERS
data<-data[,-3]

library(lubridate)
#Need to change dates to isoweeks and years for comparison and to standardise with 
#other plots etc
Week<-isoweek(data$Date)
Year<-isoyear(data$Date)

data<-data.frame(data.frame(data,Week),Year)



SepYears<-split(data, format(as.Date(data$Date), "%Y"))
SepYears<-split(data, Year)

data.2018<-SepYears$`2018`
data.2019<-SepYears$`2019`
data.2020<-SepYears$`2020`
data.2021<-SepYears$`2021`



library(tidyr)


Hb<-unique(data.2018$Board)#using 2018 data as 2015/2016 has NA as an option for health board
type<-unique(data$Type)
dates<-unique(data$Week)



###completing all the data sets with a 0 ####
comp.2018<-complete(data.2018, Board=Hb, Type=type, Week=dates,
                    fill=list(Number.of.treatment.referrals=0))
comp.2019<-complete(data.2019, Board=Hb, Type=type, Week=dates,
                    fill=list(Number.of.treatment.referrals=0))
comp.2020<-complete(data.2020, Board=Hb, Type=type, Week=dates,
                    fill=list(Number.of.treatment.referrals=0))
comp.2021<-complete(data.2021, Board=Hb, Type=type, Week=dates,
                    fill=list(Number.of.treatment.referrals=0))



full<-merge(comp.2018,comp.2019,by=c('Week','Board','Type'))
full<-merge(full,comp.2020,by=c('Week','Board','Type'))
full<-merge(full,comp.2021,by=c('Week','Board','Type'))

full<-full[order(full$Week),]
sub.full<-full[,c(1:3,5,8,11,14)]
colnames(sub.full)[4:7]<-c('2018','2019','2020','2021')

sub.full$`2021`[(sub.full$Week>14)]<-NA
sub.full$`2018`[(sub.full$Week==53)]<-sub.full$`2018`[(sub.full$Week==52)] #REPEATING LAST 2018 & 2019 OBSERVATION TO MATCH 2020 DATA
sub.full$`2019`[(sub.full$Week==53)]<-sub.full$`2019`[(sub.full$Week==52)]


average<-(sub.full$`2018`+sub.full$`2019`)/2
sub.full<-cbind(sub.full,average)
colnames(sub.full)[8]<-'Average 2018 & 2019'

###adding % change column 
sub.full<-cbind(sub.full,(sub.full$`2020`- sub.full$`Average 2018 & 2019`)/sub.full$`Average 2018 & 2019`*100)
colnames(sub.full)[9]<-'Change'
sub.full$Change[is.nan(sub.full$Change)]<-NA

###Need to put in long format for plotting #### 
library(reshape2)

DTR_drug_data<-melt(data=sub.full,
                    id.vars=c('Week','Board','Type'),
                    measure.vars=c('Average 2018 & 2019','2020','2021'),
                    variable.name = 'Year',
                    value.name='DTR')


##### Formatting for long x axis 
long.axis<-rbind(sub.full[,c(1:3,8,6)],sub.full[c(1:1032),c(1:3,8,6)])
Date<-as.Date(paste(2020, long.axis$Week[1:8428], 1, sep="-"), "%Y-%U-%u")
Date<-rbind(Date,as.Date(paste(2021, long.axis$Week[1:1032], 1, sep="-"), "%Y-%U-%u"))

library(ggplot2)
ggplot(subset(DTR_drug_data, (Board=='Scotland') & (Type=='Co-dependency')))+
  geom_line(aes(x=Week,y=DTR,col=Year))

#just need to get x axis to be months - can fix this 
months(comp.2020$Date , format = "%Y-%m-%d",
       abbreviate = TRUE)



