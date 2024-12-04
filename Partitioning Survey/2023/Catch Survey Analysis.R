
#####Load Packages
library(readxl)
library(dplyr)

#Set the current directory where data is housed
current.directory<-paste0(getwd())

#########Marine Area 9############

#Bring in MA9 survey data
MA9=read_excel(paste0(current.directory,"/2023_MA9_Survey_Data.xlsx"))

#Set catch columns as numeric
MA9$ma9=as.numeric(MA9$ma9)
MA9$`25c`=as.numeric(MA9$`25c`)

#Explore range of catch values
range(MA9$ma9)
range(MA9$`25c`)

######Start QAQC Process

##QAQC1
#A few individuals participated multiple times. Identify and remove their second entry.
MA9=MA9 %>% filter(repeat.entry=="no")

##QAQC2
#Identify and remove entries where catch in 25C is greater than catch in all of MA9
MA9=MA9 %>% filter(!(ma9 < `25c`))

##QAQC3
#Identify and remove entries that report zero catch in both 25C and MA9
MA9=MA9 %>% filter(!(ma9==0 & `25c`==0))

##QAQC4
#Identify and remove false catch reports:
#The max reported crab by an individual on their CRC in MA9 for summer 2023 was 139 crabs. Since this survey might be from memory, 
#give a 20% buffer (167 crabs).
MA9=MA9 %>% filter(!(ma9 > 167)) 

###End QAQC

##Analysis

#Calculation of 25C crab
total.9=sum(MA9$ma9) #Total in MA9
total.25C=sum(MA9$`25c`) #Total in 25C

percent.25C=(total.25C/total.9)*100 #Express as a percent
percent.25C=round(percent.25C,2) #Round
percent.25C

###################################################


#########Marine Area 6############

#Bring in MA6 survey data
MA6=read_excel(paste0(current.directory,"/2023_MA6_Survey_Data.xlsx"))

#Set catch columns as numeric
MA6$ma6=as.numeric(MA6$ma6)
MA6$`3.1`=as.numeric(MA6$`3.1`)
MA6$`3.2`=as.numeric(MA6$`3.2`)
MA6$`3.3`=as.numeric(MA6$`3.3`)

#Range of catch values
range(MA6$ma6)
range(MA6$`3.1`)
range(MA6$`3.2`)
range(MA6$`3.3`)

######Start QAQC Process

MA6$subarea=MA6$`3.1`+MA6$`3.2`+MA6$`3.3` #Create column adding all sub-areas together

##QAQC1
#Some individuals participated multiple times. Identify and remove their second entry.
MA6=MA6 %>% filter(repeat.entry=="no")

##QAQC2
#Sub-area catch should equal MA6 catch
#Identify and remove these entries
MA6=MA6 %>% filter(!(ma6!=subarea))

##QAQC3
#Identify and remove entries that report:
#a) Zero catch in all of MA6 and sum of sub-areas
MA6=MA6 %>% filter(!(ma6==0 & subarea==0))
#b) Zero catch in sum of sub-areas but input catch in MA6
MA6=MA6 %>% filter(!(ma6 > 0 & subarea==0))

##QAQC4
#Identify and remove false catch reports:
#The max reported crab by an individual on their CRC in MA6 was 108 crabs. Since the survey might be from memory, 
#give a buffer 20% buffer (130 crabs).
MA6=MA6 %>% filter(!(ma6 > 130))

##End QAQC


##Analysis

#Total crab in MA6
total.6=sum(MA6$ma6) #Total in MA6

#Calculations of 3-1 crab
total.3.1=sum(MA6$`3.1`) #Total in 3-1
percent.3.1=(total.3.1/total.6)*100 #Express as percent
percent.3.1=round(percent.3.1,1) #Round
percent.3.1

#Calculations of 3-2 crab
total.3.2=sum(MA6$`3.2`) #Total in 3-2
percent.3.2=(total.3.2/total.6)*100 #Express as percent
percent.3.2=round(percent.3.2,1) #Round
percent.3.2

#Calculations of 3-3 crab
total.3.3=sum(MA6$`3.3`) #Total in 3-3
percent.3.3=(total.3.3/total.6)*100 #Express as percent
percent.3.3=round(percent.3.3,1) #Round
percent.3.3

#################################################
