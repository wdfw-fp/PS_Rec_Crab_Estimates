# Data Code
# October 28, 2024
# S. Thurner
# We can reorganize this code for easier use

library(tidyverse)
library(fitdistrplus)
library(stats4)
library(MASS)
library(EnvStats)

# Read in Data ------------------------------------------------------------

# Required Data Files:
# Survey Data (3 files): "RM_NonResponse_Data_Summer_2022_ST.csv", "2023_Summer_Nonresponse_Trip_Data.csv", "2023_Summer_Nonresponse_Full_Data.csv"
# Late Card Data: "Late_Card_Data_Summary.csv"
# Totals Data: "CRCTotals_Summer2023Memo_Thurner.csv"
# CRC Data: "Merged_CRC_Data_years_seasons.csv"


## Survey Data  --------------------------------------------------------

### 2022 --------------------------------------------------------------------
# This is the original version, not the version from Lucas - could be switched
# File only contains people who fished
# ST version - I made the blanks = zeroes (except one row that had a blank in original)


Survey2022<- read_csv("OriginalData/RM_NonResponse_Data_Summer_2022_ST.csv", col_types = list(ID = col_factor()))
Survey2022<- Survey2022[,1:9] #remove unused columns


#From Lucas (Tulalip) presentation = 2,543 returned and did NOT fish
#Generate fake data 
NoFishNum<- 2543
GibsSeq<- rep("Gibson", NoFishNum)
IDseq<- 1:2543
FakeCatch<- rep(0, NoFishNum)

STFakeData<- tibble(Gibs = GibsSeq, No = IDseq, TotalCrabID = FakeCatch)
STFakeData<- STFakeData %>% unite(ID, c("Gibs", "No"))


#### Group data by wild id ------------------------------------------------------
# This would allow looking at catch/card (i.e. catch/wild id) by year, only one year of data

# Group by ID-- should be equivalent to a CRC as it combines all trips by year/id
Survey2022byID<- Survey2022 %>% 
  group_by(ID) %>% 
  summarise(TotalCrabID = sum(nmcrb, na.rm = TRUE))

nrow(Survey2022byID) #933 respondents (matches lucas presentation)
Survey2022byIDbyIDTest<- as_tibble(Survey2022byID) #gets rid of the grouping so I can regroup, if needed

MeanSurveyTotalCrab<- mean(Survey2022byID$TotalCrabID) #8.366527
SDSurveyTotalCrab<- sd(Survey2022byID$TotalCrabID) #8.088617



#### Merge with Fake Data ----------------------------------------------------


Survey2022byIDAll<- bind_rows(Survey2022byID, STFakeData)

933 + 2543 - nrow(Survey2022byIDAll) #should be 0

Survey2022byIDAll %>% filter(TotalCrabID == 5.97)



#### Clean Data --------------------------------------------------------------

# Remove decimals so I can use a NB
# The rows for this ID all look wonky
Survey2022byIDAll %>% filter(TotalCrabID == 5.97)
# A tibble: 1 × 2
#ID          TotalCrabID
#<chr>             <dbl>
# 0.659844727        5.97


Survey2022byIDAll<- Survey2022byIDAll %>%
  mutate(TotalCrabIDNew = case_when(ID == 0.659844727  ~ round(TotalCrabID),
                                    ID != 0.659844727 ~ TotalCrabID))


### 2023 --------------------------------------------------------------------
# This is the original version, not from Lucas - could be switched

# Trip file only contains people who fished
Survey2023<- read_csv("OriginalData/2023_Summer_Nonresponse_Trip_Data.csv", col_types = list(rmcase = col_factor()))

# Full data
Survey2023Full<- read_csv("OriginalData/2023_Summer_Nonresponse_Full_Data.csv") #warnings when you read this in
Survey2023Full %>% count(wcat) 
#cat 1 and 3 are people who harvested = 1124
#cat 2 and 4 are people who didnt harvest = 3560


#From Full Data = 3560 responded and did NOT fish
#Generate fake data 
NoFishNum23<- 3560
GibsSeq23<- rep("Gibson", NoFishNum23)
IDseq23<- 2544:6103
FakeCatch23<- rep(0, NoFishNum23)

STFakeData23<- tibble(Gibs = GibsSeq23, No = IDseq23, TotalCrabID = FakeCatch23)
STFakeData23<- STFakeData23 %>% unite(rmcase, c("Gibs", "No"))


#### Group data by wild id ------------------------------------------------------
# This would allow looking at catch/card (i.e. catch/wild id) by year, only one year of data

# Group by ID-- should be equivalent to a CRC as it combines all trips by year/id
Survey2023byID<- Survey2023 %>% 
  group_by(rmcase) %>% 
  summarise(TotalCrabID = sum(nmcrb, na.rm = TRUE))

nrow(Survey2023byID) #1124 respondents
Survey2023byIDTest<- as_tibble(Survey2023byID) #gets rid of the grouping so I can regroup, if needed

MeanSurvey2023TotalCrab<- mean(Survey2023byID$TotalCrabID) #8.63
SDSurvey2023TotalCrab<- sd(Survey2023byID$TotalCrabID) #9.29


#### Merge with Fake Data ----------------------------------------------------


Survey2023byIDAll<- bind_rows(Survey2023byID, STFakeData23)

3560 + 1124 - nrow(Survey2023byIDAll) #should be 0



### 2022+2023 ---------------------------------------------------------------
Temp2023<- rename(Survey2023byIDAll, ID = rmcase)
Temp2023<- Temp2023 %>% mutate(Year = 2023)
Temp2022<- Survey2022byIDAll[,c(1,3)]
Temp2022<- rename(Temp2022, TotalCrabID = TotalCrabIDNew)
Temp2022<- Temp2022 %>% mutate(Year = 2022)



SurveyData2223<- bind_rows(Temp2022, Temp2023)

nrow(Survey2023byIDAll) + nrow(Survey2022byIDAll) - nrow(SurveyData2223) #should be 0



## Percent late  --------------------------------------------------------
# 
LateSummary<- read_csv("OriginalData/Late_Card_Data_Summary.csv", col_types = list(Year = col_factor()))
head(LateSummary)

MinSummerRatio<- min(LateSummary$SummerRatio) #0.17
MaxSummerRatio<- max(LateSummary$SummerRatio) #0.34
MeanSummerRatio<- mean(LateSummary$SummerRatio) #0.238
SDSummerRatio<- sd(LateSummary$SummerRatio) #0.06795423





## Percent on time/Card Type --------------------------------------------------------
# From Memo
# Check to see if this matches dataset

CRCTotals<- read_csv("OriginalData/CRCTotals_Summer2023Memo_Thurner.csv", na = c("", "NA"), col_types = list(Year = col_factor(), Season = col_factor(), Issued = col_integer(), Mail_Reported = col_integer(), Online_Reported = col_integer(), Total_Reported = col_integer(), Unreported = col_integer(), Endorsements = col_integer()))
head(CRCTotals)

CRCTotals<- CRCTotals %>% 
  mutate(Perc_Reported = Total_Reported/Issued, 
         Perc_Unreported = Unreported/Issued,
         Perc_Mail = Mail_Reported/Total_Reported,
         Perc_Online = Online_Reported/Total_Reported)





## CRC Data --------------------------------------------------------

# Read in Data (Merged, multi year, by season)
AllCRC<- read_csv("OriginalData/Merged_CRC_Data_years_seasons.csv", na = c("", "NA"), col_types = list(area = col_factor(), month = col_integer(), day = col_integer(), year = col_integer(), num_crab = col_double(), cardtype = col_factor(), ID = col_integer()))
# area, month, day, year, num_crab, cardtype, ID
# columns were reading in weird, specified in readr call

# Read in Data with YEARS as FACTOR instead of INTEGER
AllCRC2<- read_csv("OriginalData/Merged_CRC_Data_years_seasons.csv", na = c("", "NA"), col_types = list(area = col_factor(), month = col_integer(), day = col_integer(), year = col_factor(), num_crab = col_double(), cardtype = col_factor(), ID = col_integer()))
AllCRC2 <- AllCRC2 %>%
  mutate(year = fct_rev(year)) #Reverse the year factor so that it plots in chronological order




### Process Data ------------------------------------------------------------
## The only processing I currently do is removing coastal catch


# Unique catch areas
unique(AllCRC2$area)

# Remove 519, Set NAs as 192
AllCRC2$area[AllCRC2$area==519]=192
AllCRC2$area[is.na(AllCRC2$area)]=192

#Select only Puget Sound MA's (including unknown's)
AllCRC2<- AllCRC2 %>% filter(area==4|area==5|area==6|area==7|area==8|area==81|area==82|area==9|
                               area==10|area==11|area==12|area==13|area==192)



#### Summer Only -------------------------------------------------------------

# Filter Summer
SummerCRC2<- AllCRC2 %>% filter(season == "S")




#### Group data by wild id and year ------------------------------------------------------
# This would allow looking at catch/card (i.e. catch/wild id) by year

# Group by year, ID, Card Type -- should be equivalent to a CRC as it combines all trips by year/id
SummerCRCbyID<- SummerCRC2 %>% 
  group_by(year, ID, cardtype) %>% 
  summarise(TotalCrabID = sum(num_crab, na.rm = TRUE))


SummerCRCbyIDTest<- as_tibble(SummerCRCbyID) #gets rid of the grouping so I can regroup

# number of CRCs by year
SummerCRCbyIDTest %>% 
  count(year) 

# number of CRCs by card type
CRCTotalsDatabase<- SummerCRCbyIDTest %>% 
  count(year, cardtype) 


# write_csv(CRCTotalsDatabase, "CRCTotalsDatabase.csv")




#### Filter by Card Type -----------------------------------------------------

# Remove 2007 since it has a third card type
# Could include 2007 if made a decision about where to include Telephone catch
SummerCRCbyID0823<- SummerCRCbyID %>% 
  filter(year != 2007)

# Subset mail in cards
SummerCRCbyID0823S<- SummerCRCbyID0823 %>% 
  filter(cardtype == "S")

# Subset internet cards
SummerCRCbyID0823I<- SummerCRCbyID0823 %>% 
  filter(cardtype == "I")





# Create Tables -----------------------------------------------------------

# Total Catch by Year and Card Type - from database
TotalsbyYearS<- SummerCRC2 %>% 
  group_by(year, cardtype) %>% 
  summarise(TotalCrabCardType = sum(num_crab, na.rm = TRUE))

# Percent of catch by each card type by year - from database
PercCatchCardTypeS<- TotalsbyYearS %>% 
  pivot_wider(names_from = cardtype, values_from = TotalCrabCardType) %>% 
  rowwise() %>% 
  mutate(PercS = S/sum(S, I, T, na.rm = TRUE), PercI = I/sum(S, I, T, na.rm = TRUE), PercT = T/sum(S, I, T, na.rm = TRUE))

# write.csv(PercCatchCardTypeS, "PercCardTypeSummer.csv")

# Percent of each card type by year - from database
CRCTotalsDatabase<- SummerCRCbyIDTest %>% 
  count(year, cardtype) 

PercCardData<- CRCTotalsDatabase %>% 
  pivot_wider(names_from = cardtype, values_from = n) %>% 
  rowwise() %>% 
  mutate(TotalCards = sum(S, I, T, na.rm = TRUE), PercS = S/sum(S, I, T, na.rm = TRUE), PercI = I/sum(S, I, T, na.rm = TRUE), PercT = T/sum(S, I, T, na.rm = TRUE), Year = year)

# Compare total cards and percent card type between database and memo
CompareTables<- full_join(CRCTotals, PercCardData, by = c("Year"))

CompareTables<- CompareTables %>% 
  rowwise() %>% 
  mutate(TotalDiff = Total_Reported - TotalCards, 
         MailPercDiff = Perc_Mail*100 - PercS*100, 
         IntDiff = Perc_Online*100 - PercI*100,
         PercRepData = TotalCards/Issued)


# SAVE RDS ----------------------------------------------------------------

saveRDS(SummerCRCbyID, "DataRDS/SummerCRCbyID.rds")
saveRDS(SummerCRCbyID0823S, "DataRDS/SummerCRCbyID0823S.rds")
saveRDS(SummerCRCbyID0823I, "DataRDS/SummerCRCbyID0823I.rds")
saveRDS(SurveyData2223, "DataRDS/SurveyData2223.rds")
saveRDS(LateSummary, "DataRDS/LateSummary.rds")
saveRDS(CRCTotals, "DataRDS/CRCTotals.rds")
