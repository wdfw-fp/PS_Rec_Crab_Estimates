################################################################
# Processing 2023 Responsive Management Crabber interview data
# Based on recommendations from the JSTWG 
#
# Created by: Katelyn Bosley/Blair Winnacott
# Date: 12/2/2024
# Updated 12/10/2024
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
current.directory<-paste0(getwd(),"/2023 RM Input Data/Summer")

#read in the raw data files

#Interview data
data.full=read_excel(paste0(current.directory,paste0("/2023_Nonresponse_Full_Data.xlsx")))

#trip data
data.trips=read_excel(paste0(current.directory,paste0("/2023_Nonresponse_Trip_Data_Updated.xlsx")))


###########################################
# Data clean up

#Identify individuals that were incorrectly entered as a harvester or vice versa. 
#They were entered as NO "I did not attempt to harvest" but entered Yes "I caught crab" but no harvest or trip data exists in the data.Convert them from a 1 to a 2 for var619.


#finding out which crabbers said they did NOT go fishing but then entered that they caught and kept crab.
check=data.full %>% filter(var210==2 & var619==1)

#Define vector of harvesters that incorrectly entered as Yes "I caught crab" [774,858]
no.catch=check$rmcase

data.full$var619[no.catch]=2

#these don't exist in the trips data
test<-data.trips %>% filter(rmcase==no.catch)

#find out how many crabbers were listed as crabbing and how many did not catch
table(data.full$var619)
length(unique(data.trips$rmcase))

#do the don't knows have any catch?
check2=data.full %>% filter(var619==3)
dont.know<-check2$rmcase
test2<-data.trips %>% filter(rmcase %in% dont.know) #no records are pulled

#There are a couple var619 in the trips file that are marked as not crabbing in the full data. Finding and fixing
huh<-unique(data.trips$rmcase)
why<-data.full %>% filter(rmcase%in%huh)
dim(why)

table(why$var619)
check3<-why %>% filter(var619==2) # rmcase 27 and 664 caught crab
did.catch<-check3$rmcase

test3<-data.trips %>% filter(rmcase%in%did.catch)
test3

#adjusting the ones that are supposed the be catching and keeping
data.full$var619[which(data.full$rmcase%in%did.catch)]=1
table(data.full$var619) # all fixed!
length(unique(data.trips$rmcase))


#####################################################
# Recommendation 1: Change individuals that harvested 0 crab in the trips file to 'did not catch and keep crab' in the full data file.

#find zero and NA entries in the 'trips' data set
which(data.trips$nmcrboriginal==0)
which(is.na(data.trips$nmcrboriginal))



#####################################################
# Recommendation 2: Dealing with the 'Other' locations

#areas codes that need to be removed include: 12,13. In the 2023 data, there is no text field for the 'other' category so all records are assumed to be in Puget Sound. The 'don't know' category is also retained.

unique(data.trips$wchma)
table(data.trips$wchma)


other.ma=data.trips %>% filter(wchma==12 | wchma==13) #Filter for "coastal (12) or area outside PS (13)"
#31 trips 

trip.tally<-data.trips%>%group_by(rmcase,wchma)%>%dplyr::summarize(totaltrips=length(rmcase))
trip.tally.wide<-spread(trip.tally, key=wchma,value=totaltrips)

trip.tally.wide$PS.MA=0
trip.tally.wide$PS.MA=rowSums(trip.tally.wide[,c(2:12,15:16)],na.rm = T)

noPStrips<-trip.tally.wide[which(trip.tally.wide$PS.MA==0),]

#create a vector of interviews with no PS trips to remove from trips and crabber data tables.
noPStrips.rm<-noPStrips$rmcase


#Change any interviews where crab were only harvested from coastal or outside PS areas to 'did not catch and keep crab' in the crabber interview data
data.full$var619[which(data.full$rmcase%in%noPStrips.rm)]=2

#remove trips where no PS crab was harvested.
data.trips<-data.trips[!data.trips$wchma%in%c(12,13),]
table(data.trips$wchma)

#####################################################################
# Recommendation 3: Convert any non-integer entries in the crab caught and date to unknown in the trips file

sapply(data.trips,class)

#converting odd decimal days
data.trips$day.format<-data.trips$whtdy - as.integer(data.trips$whtdy)
edit.day<-which(((data.trips$day.format>0 & data.trips$day.format<1) | is.na(data.trips$day.format)))
data.trips$whtdy[edit.day]=32

#converting odd catch records
data.trips$catch.format<-data.trips$nmcrboriginal - as.integer(data.trips$nmcrboriginal)
edit.catch<-which(((data.trips$catch.format>0 & data.trips$catch.format<1) | is.na(data.trips$catch.format)))
data.trips$nmcrboriginal[edit.catch]=99
#no records with decimal catch in the 2023 data. 

###############################################################
# Recommendation 4: Handling of records that are greater than 10 crab retained

#All records are initially kept as entered under 'nmcrboriginal'
data.trips$crab.catch<-data.trips$nmcrboriginal


###############################################################
# Recommendation 5: Handling of '98' crab entries
 
### 12/20/2024 Update  - this will get added back in if responsive management indicates that the survey requires people to input values manually. This would increase support for a key-in error.


# Convert all 98 crab entries to 99
#data.trips$crab.catch<-ifelse(data.trips$crab.catch==98,99,data.trips$nmcrboriginal)

###############################################################
# Recommendation 6: Handling of '32' crab entries

#Not making any additional modification to the crab catch. Maintaining all the '32' catch entries as is.


##############################################################
# Recommendation 6: Handling of '99' (unknown) records.

# Replace the unknown catches with the median number of crab caught per trip.

no.unknown<-data.trips$crab.catch[!data.trips$crab.catch==99]
#plotting the data to look at the distribution
hist(no.unknown) 
mean(no.unknown) #arithmetic mean
median<-median(no.unknown) #median


#converting the unknowns to the geometric mean
data.trips$crab.catch<-ifelse(data.trips$crab.catch==99,median,data.trips$crab.catch)

#######################################################################
# Summarizing and calculation of overall catch per non-respondent. 

# Interview summary statistics
length(unique(data.full$rmcase)) #4684 total interviewed
length(unique(data.trips$rmcase)) #1105 harvesters
nrow(data.trips) #2273 trips
sum(data.trips$nmcrb)   #9598 crabs harvested


# Generating primary statistic for generating the estimate
# *Overall crab kept per non-respondent*
# total reported crabs/total number of crabbers interviewed (mean crabs per non-respondent)
N.crab.NR<-9598/4684 #2.05

##################################################################
# Extra calculations #
# Determining summary stats of successful crabbers
# Trips per successful crabber
# Number of successful trips/number of successful crabbers
2273/1105       #2.06 trips/harvester

# Total number of crabs harvested by successful crabbers
# Crabs per harvester
# total reported crabs/total number of successful crabbers
9598/1105               #8.69 crabs/harvester



#Crabs per trip
9598/2273               #4.22 crabs/trip


##########################################################################
# Determining the total catch for non-respondents in 2023

# number of summer Crabbers in 2024 (licences issued)
187214

# number of crabbers who reported 
81091


#number of non-respondent crabbers
N.NR<-187214-81091  #106123


# Total non-respondent crabs
N.NR * N.crab.NR

#217457 crabs caught by non-repondents




