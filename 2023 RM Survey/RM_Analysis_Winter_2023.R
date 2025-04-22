################################################################
# Processing winter 2023 Responsive Management Crabber interview data
# Based on recommendations from the JSTWG 
#
# Created by: Katelyn Bosley/Blair Winnacott
# Date: 02/25/2025
# Updated 
#################################


#####################################
# Import Packages
library(haven)
library(dplyr)
library(tidyr)
library(data.table)
library(plyr)
library(writexl)
library(readxl)


######################################
# Import Data (Set working directory)

#Set the current directory of interest
current.directory<-paste0(getwd(),"/2023 RM Input Data/Winter")

#read in the raw data files

#Interview data
data.full=read_excel(paste0(current.directory,paste0("/2023_Winter_Non_Full.xlsx")))

#trip data
data.trips=read_excel(paste0(current.directory,paste0("/2023_Winter_Non_Trip.xlsx")))

###########################################
read=read_sav(paste0(current.directory,"/","WACRABwinterdataset.sav"))
attr(read$var210, 'label')
attr(read$var210, 'labels')

###########################################
# Data clean up

#Identify individuals that were incorrectly entered as a harvester or vice versa. 
#They were entered as NO "I did not attempt to harvest" but entered Yes "I caught crab" but no harvest or trip data exists in the data.Convert them from a 1 to a 2 for var619.


#finding out which crabbers said they did NOT go fishing but then entered that they caught and kept crab.
check=data.full %>% filter(var210==2 & var619==1)
#None with this data entry error

#do the don't knows have any catch?
check2=data.full %>% filter(var619==3)
dont.know<-check2$rmcase
test<-data.trips %>% filter(rmcase %in% dont.know) 
#No records are pulled

#Are any crabbers present in the trips file that are marked as not crabbing in the full data?
huh<-unique(data.trips$rmcase)
why<-data.full %>% filter(rmcase%in%huh)
check3<-why %>% filter(var619==2) 
#None



#####################################################
# Recommendation 1: Change individuals that harvested 0 crab in the trips file to 'did not catch and keep crab' in the full data file.

#find zero and NA entries in the 'trips' data set
which(data.trips$nmcrb==0 | is.na(data.trips$nmcrb))
#None


#####################################################
# Recommendation 2: Dealing with the 'Other' locations

# Area codes that need to be removed include: 12 and 13. 
# Area codes that will be retained are 14 and 15.

# Area code 12 == "Coastal Marine Area"
# Area code 13 == "Area outside of Puget Sound"
# Area code 14 == "Other" (None input in 2023 winter data set)
# Area code 15 == "Don't know"

unique(data.trips$wchma)
table(data.trips$wchma)
# 11 trips with area code 12 (Coastal Marine Area) need to be removed
# 1 trip with area code 13 (Outside Puget Sound) need to be removed

trip.tally<-data.trips%>%group_by(rmcase,wchma)%>%dplyr::summarize(totaltrips=length(rmcase))
trip.tally.wide<-spread(trip.tally, key=wchma,value=totaltrips)

trip.tally.wide$PS.MA=0
trip.tally.wide$PS.MA=rowSums(trip.tally.wide[,c(2:9,12)],na.rm = T)

noPStrips<-trip.tally.wide[which(trip.tally.wide$PS.MA==0),]
#5 non Puget Sound harvesters

#create a vector of interviews with no PS trips to remove from trips and crabber data tables.
noPStrips.rm<-noPStrips$rmcase


#Change any interviews where crab were only harvested from coastal or outside PS areas to 'did not catch and keep crab' in the crabber interview data
data.full$var619[which(data.full$rmcase%in%noPStrips.rm)]=2

#remove trips where no PS crab was harvested.
data.trips<-data.trips[!data.trips$wchma%in%c(12,13),]
table(data.trips$wchma)

#####################################################################
# Recommendation 3: Convert any non-integer entries in the crab caught and date to unknown in the trips file

unique(data.trips$nmcrb)
unique(data.trips$whtdy)
unique(data.trips$month)

#none present for winter 2023

###############################################################
# Recommendation 4: Handling of records that are greater than 10 crab retained

#No data entry issue for winter 2023 data

###############################################################
# Recommendation 5: Handling of '98' crab entries
 
data.trips %>% filter(nmcrb==98)

#None present for winter 2023

###############################################################
# Recommendation 6: Handling of '32' crab entries

data.trips %>% filter(nmcrb==32)

#None present for winter 2023

##############################################################
# Recommendation 6: Handling of '99' (unknown) records.

# Replace the unknown catches with the median number of crab caught per trip, not including 99's.

no.unknown<-data.trips$nmcrb[!data.trips$nmcrb==99]
#plotting the data to look at the distribution
hist(no.unknown) 
mean(no.unknown) #arithmetic mean
median<-median(no.unknown) #median

#converting the unknowns to the median
data.trips$nmcrb<-ifelse(data.trips$nmcrb==99,median,data.trips$nmcrb)

#######################################################################
# Summarizing and calculation of overall catch per non-respondent. 

# Interview summary statistics
length(unique(data.full$rmcase)) #2,044 total non-respondents interviewed
length(unique(data.trips$rmcase)) #403 harvested Dungeness crab
nrow(data.trips) #737 successful trips
sum(data.trips$nmcrb)   #2,940 crabs harvested

403/2044 #19.72% of non-respondents harvested crabs

# Generating primary statistic for generating the estimate
# *Overall crab kept per non-respondent*
# total non-respondent reported crabs/total number of non-respondent crabbers interviewed (mean crabs per non-respondent)
N.crab.NR<-2940/2044 #1.44 crabs per non-respondent

#Why winter is 1.44 and summer is 2.05: No 98's, 32's, and smaller percent of NR harvested crab for winter compared to summer.

##################################################################
# Extra calculations #
# Determining summary stats of successful crabbers
# Trips per successful crabber
# Number of successful trips/number of successful crabbers
737/403       #1.83 trips/harvester

# Total number of crabs harvested by successful crabbers
# Crabs per harvester
# total reported crabs/total number of successful crabbers
2940/403               #7.30 crabs/harvester

#Crabs per trip
2940/737               #3.99 crabs/trip


##########################################################################
# Determining the total catch for non-respondents in winter 2023

# number of winter Crabbers in 2024 (licences issued)
licenses=33626

# number of winter 2024 crabbers who reported 
reported=16654


#number of non-respondent crabbers
N.NR<-licenses-reported  #16,972


# Total non-respondent crabs
N.NR * N.crab.NR

#24,411 crabs caught by non-respondents




