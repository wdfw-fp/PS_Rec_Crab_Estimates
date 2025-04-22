
#####Load Packages
library(dplyr)
library(readxl)

#Set the current directory where data is housed
current.directory<-paste0(getwd())

#########MA9############

#Bring in MA9 survey data
MA9=read_excel(paste0(current.directory,"/Marine Area 9 Dungeness Crab Catch Survey 2024.xlsx"))

#Set catch columns as numeric
MA9$ma9=as.numeric(MA9$ma9)
MA9$`25c`=as.numeric(MA9$`25c`)

#Pre QAQC # of responses
entries.pre.QAQC=nrow(MA9)

######Start QAQC Process

##QAQC1
#Identify and remove entries where catch in 25C is greater than catch in all of MA9
MA9=MA9 %>% filter(!(ma9 < `25c`))

##QAQC2
#Identify and remove entries that report zero catch in both 25C and MA9
MA9=MA9 %>% filter(!(ma9==0 & `25c`==0))

##QAQC3
#Identify and remove false catch reports:
#The max reported crab by an individual on a CRC in MA9 for summer 2024 was 134 crabs. 
#Since some of these entries are from memory, give a 20% buffer to 161 crabs for individuals reporting for themselves. 
#For 2024, remove clearly false entries any entries that report greater than 161 crabs for individuals reporting for only themselves.
MA9=MA9 %>% filter(!(ma9 > 299)) #This will remove entries from the above criteria.

###End QAQC

##Analysis

#Post QAQC # of responses
entries.post.QAQC=nrow(MA9)

#Calculation of 25C crab
total.9=sum(MA9$ma9) #Total in MA9
total.25C=sum(MA9$`25c`) #Total in 25C

percent.25C=(total.25C/total.9)*100 #Express as a percent
percent.25C=round(percent.25C,1) #Round
percent.25C

###################################################


#########MA6############

#Bring in MA6 survey data
MA6=read_excel(paste0(current.directory,"/Marine Area 6 Dungeness Crab Catch Survey 2024.xlsx"))

#Set catch columns as numeric
MA6$ma6=as.numeric(MA6$ma6)
MA6$`6A`=as.numeric(MA6$`6A`)
MA6$`6B`=as.numeric(MA6$`6B`)
MA6$`6C`=as.numeric(MA6$`6C`)
MA6$`6D`=as.numeric(MA6$`6D`)

#Pre QAQC # of responses
entries.pre.QAQC=nrow(MA6)

######Start QAQC Process

##QAQC1
MA6$subarea=MA6$`6A`+MA6$`6B`+MA6$`6C`+MA6$`6D` #Create column adding all sub-areas together
#Sub-area catch should equal MA6 catch
#Identify and remove these entries
MA6=MA6 %>% filter(!(ma6!=subarea))

##QAQC2
#Identify and remove entries that report:
#Zero catch in all of MA6 and sum of sub-areas
MA6=MA6 %>% filter(!(ma6==0 & subarea==0))

##QAQC3
#Identify and remove false catch reports:
#The max reported crab by an individual on a CRC in MA6 for summer 2024 was 142 crabs. 
#Since some of these entries are from memory, give a 20% buffer to 170 crabs for individuals reporting for themselves. 
#For 2024, remove clearly false entries any entries that report greater than 170 crabs for individuals reporting for only themselves.
MA6=MA6 %>% filter(!(ma6 > 170))

##End QAQC

##Analysis

#Post QAQC # of responses
entries.post.QAQC=nrow(MA6)

#Total crab in MA6
total.6=sum(MA6$ma6) #Total in MA6

#Calculations of 6A (Region 1 MA6 portion) crab
total.6A=sum(MA6$`6A`) #Total in 6A
percent.6A=(total.6A/total.6)*100 #Express as percent
percent.6A=round(percent.6A,1) #Round
percent.6A

#Calculations of 3-1 crab (6B portion)
total.6B=sum(MA6$`6B`) #Total in 3-1
percent.6B=(total.6B/total.6)*100 #Express as percent
percent.6B=round(percent.6B,1) #Round
percent.6B

#Calculations of 3-2 crab (6C portion)
total.6C=sum(MA6$`6C`) #Total in 3-2
percent.6C=(total.6C/total.6)*100 #Express as percent
percent.6C=round(percent.6C,2) #Round
percent.6C=round(percent.6C,1) #Round
percent.6C

#Calculations of 3-3 crab (6D portion)
total.6D=sum(MA6$`6D`) #Total in 3-3
percent.6D=(total.6D/total.6)*100 #Express as percent
percent.6D=round(percent.6D,1) #Round
percent.6D

#################################################

#########MA7############

#Bring in MA9 survey data
MA7=read_excel(paste0(current.directory,"/Marine Area 7 Dungeness Crab Catch Survey 2024.xlsx"))

#Set catch columns as numeric
MA7$ma7=as.numeric(MA7$ma7)
MA7$`7A`=as.numeric(MA7$`7A`)

#Pre QAQC # of responses
entries.pre.QAQC=nrow(MA7)

######Start QAQC Process

##QAQC1
#Identify and remove entries where catch in 7A is greater than catch in all of MA7
MA7=MA7 %>% filter(!(ma7 < `7A`))

##QAQC2
#Identify and remove entries that report zero catch in both 7A and MA7
MA7=MA7 %>% filter(!(ma7==0 & `7A`==0))

##QAQC3
#Identify and remove false catch reports:
#The max reported crab by an individual on a CRC in MA7 for summer 2024 was 105 crabs. 
#Since some of these entries are from memory, give a 20% buffer to 126 crabs for individuals reporting for themselves. 
#For 2024, remove clearly false entries any entries that report greater than 126 crabs for individuals reporting for only themselves.
MA7=MA7 %>% filter(!(ma7 > 250)) #This will remove entries from the above criteria.

###End QAQC

##Analysis

#Post QAQC # of responses
entries.post.QAQC=nrow(MA7)

#Calculation of 7A (3-1 portion of MA7) crab
total.7=sum(MA7$ma7) #Total in MA7
total.7A=sum(MA7$`7A`) #Total in 7A

percent.7A=(total.7A/total.7)*100 #Express as a percent
percent.7A=round(percent.7A,1) #Round
percent.7A

#####################################################################
