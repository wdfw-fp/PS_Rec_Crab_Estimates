# 10/24/2024
# S. Thurner, NWIFC
# Simulation Data Project
# Starting with SUMMER ONLY

# Draft code -- I will clean this up! But wanted folks to see it now.
# Email me with questions/comments/concerns/suggestions/mistakes/etc

library(tidyverse)
library(fitdistrplus)
library(stats4)
library(MASS)
library(EnvStats)



# Read in Data ------------------------------------------------------------

# I will make a list of files needed/move reading in all files up at some point




## Survey Data  --------------------------------------------------------

### 2022 --------------------------------------------------------------------
# This is the original version, not the version from Lucas - could be switched
# File only contains people who fished
# ST version - I made the blanks = zeroes (except one row that had a blank in original)


Survey2022<- read_csv("RM_NonResponse_Data_Summer_2022_ST.csv", col_types = list(ID = col_factor()))
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
Survey2023<- read_csv("2023_Summer_Nonresponse_Trip_Data.csv", col_types = list(rmcase = col_factor()))

# Full data
Survey2023Full<- read_csv("2023_Summer_Nonresponse_Full_Data.csv")
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
LateSummary<- read_csv("Late_Card_Data_Summary.csv", col_types = list(Year = col_factor()))
head(LateSummary)

MinSummerRatio<- min(LateSummary$SummerRatio) #0.17
MaxSummerRatio<- max(LateSummary$SummerRatio) #0.34
MeanSummerRatio<- mean(LateSummary$SummerRatio) #0.238
SDSummerRatio<- sd(LateSummary$SummerRatio) #0.06795423





## Percent on time/Card Type --------------------------------------------------------
# From Memo
# Check to see if this matches dataset

CRCTotals<- read_csv("CRCTotals_Summer2023Memo_Thurner.csv", na = c("", "NA"), col_types = list(Year = col_factor(), Season = col_factor(), Issued = col_integer(), Mail_Reported = col_integer(), Online_Reported = col_integer(), Total_Reported = col_integer(), Unreported = col_integer(), Endorsements = col_integer()))
head(CRCTotals)

CRCTotals<- CRCTotals %>% 
  mutate(Perc_Reported = Total_Reported/Issued, 
         Perc_Unreported = Unreported/Issued,
         Perc_Mail = Mail_Reported/Total_Reported,
         Perc_Online = Online_Reported/Total_Reported)





## CRC Data --------------------------------------------------------

# Read in Data (Merged, multi year, by season)
AllCRC<- read_csv("Merged_CRC_Data_years_seasons.csv", na = c("", "NA"), col_types = list(area = col_factor(), month = col_integer(), day = col_integer(), year = col_integer(), num_crab = col_double(), cardtype = col_factor(), ID = col_integer()))
# area, month, day, year, num_crab, cardtype, ID
# columns were reading in weird, specified in readr call

# Read in Data with YEARS as FACTOR instead of INTEGER
AllCRC2<- read_csv("Merged_CRC_Data_years_seasons.csv", na = c("", "NA"), col_types = list(area = col_factor(), month = col_integer(), day = col_integer(), year = col_factor(), num_crab = col_double(), cardtype = col_factor(), ID = col_integer()))
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






# Visualize Data ----------------------------------------------------------

# defining an axis sequence for a later plot

labelseq<- c(0)
for(i in 1:50){
  if(i%%5 == 0){
    labelseq<- c(labelseq, i*5)
  } else {
    labelseq<- c(labelseq, "")
  }
}


## CRC ---------------------------------------------------------

# pdf("PrelimSummerCRCVis_092024.pdf", height = 8.5, width = 11)

# Total Crab reported by card type and year for Summer Cards
ggplot(SummerCRCbyID, aes(fill = cardtype, x = year, y = TotalCrabID)) + 
  geom_bar(position = "stack", stat = "identity")+
  labs(title = "Summer Cards, Total Crab Reported by Card Type", 
       x = "Year",
       y = "Total crab reported") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  scale_fill_discrete(name = "Card Type", labels=c('Mail', 'Internet', 'Telephone'))



# Histogram of all Summer CRCs, not split by year or card type
ggplot(SummerCRCbyID, aes(x = TotalCrabID)) +
  geom_histogram(binwidth=1, fill = "white", color = "black") +
  labs(title = "Summer Cards", x = "Num Crab per CRC", y = "Number of CRCs")



# Histogram of all Summer CRCs, all years, split by card type
ggplot(SummerCRCbyID, aes(x = TotalCrabID, fill = cardtype, color = cardtype)) +
  geom_histogram(binwidth=1, alpha = 0.5, position = "dodge") + #could use identity instead of dodge 
  labs(title = "Summer Cards", x = "Num Crab per CRC", y = "Number of CRCs")



ggplot(SummerCRCbyID, aes(x = TotalCrabID)) +
  geom_histogram(binwidth=1, alpha = 0.5) + #could use identity instead of dodge 
  labs(title = "Summer Cards", x = "Num Crab per CRC", y = "Number of CRCs")  +
  scale_x_continuous(name ="Num Crab per CRC", breaks = seq(0, 250, by = 5), 
                     labels = labelseq) +
  facet_wrap(~cardtype)
# note that #'s which end in 5 are much more likely 

# Histogram of all Summer CRCs, all card types, split by year
ggplot(SummerCRCbyID, aes(x=TotalCrabID)) +
  geom_histogram(binwidth=1) +
  labs(title = "Summer Cards", x = "Num Crab per CRC", y = "Number of CRCs") +
  facet_wrap(~year)


# Histogram of all Summer CRCs, split by year and cardtype
ggplot(SummerCRCbyID, aes(x=TotalCrabID)) +
  geom_histogram(binwidth=1) +
  labs(title = "Summer Cards", x = "Num Crab per CRC", y = "Number of CRCs") +
  facet_wrap(~year+cardtype)

# Boxplot of all Summer CRCs, all card types, split by year
ggplot(SummerCRCbyID, aes(x=year,y=TotalCrabID)) +
  geom_boxplot() +
  labs(title = "Summer Cards", x = "Year", y = "Num Crab per ID")

# Boxplot of all Summer CRCs, split by year and card type
ggplot(SummerCRCbyID, aes(x=year,y=TotalCrabID,fill = factor(cardtype))) +
  geom_boxplot(aes(fill = factor(cardtype))) +
  labs(title = "Summer Cards", x = "Year", y = "Num Crab per ID") +
  scale_fill_discrete(name = "Card Type", labels=c('Mail', 'Internet', 'Telephone'))





# Histogram % Reported - Memo and database

ggplot(CRCTotals, aes(x=Perc_Reported)) +
  geom_histogram(binwidth=0.05, fill = "white", color = "black") +
  labs(title = "Summer Cards", x = "% Reported from memo", y = "Number of years")


ggplot(CompareTables, aes(x=PercRepData)) +
  geom_histogram(binwidth=0.05, fill = "white", color = "black") +
  labs(title = "Summer Cards", x = "% Reported from data", y = "Number of years")



## Survey ---------------------------------------------------------

ggplot(Survey2022byID, aes(x = TotalCrabID)) +
  geom_histogram(binwidth=1, fill = "white", color = "black") +
  labs(title = "Summer 2022 NR Survey - Fished", x = "Num Crab per ID", y = "Number of IDs")


ggplot(Survey2022byIDAll, aes(x = TotalCrabIDNew)) +
  geom_histogram(binwidth=1, fill = "white", color = "black") +
  labs(title = "Summer 2022 NR Survey - All Surveyed", x = "Num Crab per ID", y = "Number of IDs")


ggplot(Survey2023byID, aes(x = TotalCrabID)) +
  geom_histogram(binwidth=1, fill = "white", color = "black") +
  labs(title = "Summer 2023 NR Survey - Fished", x = "Num Crab per ID", y = "Number of IDs")


ggplot(Survey2023byIDAll, aes(x = TotalCrabID)) +
  geom_histogram(binwidth=1, fill = "white", color = "black") +
  labs(title = "Summer 2023 NR Survey - All Surveyed", x = "Num Crab per ID", y = "Number of IDs")


ggplot(SurveyData2223, aes(x = TotalCrabID)) +
  geom_histogram(binwidth=1, fill = "white", color = "black") +
  labs(title = "Combined Summer 2022 and 2023 NR Survey - All Surveyed", x = "Num Crab per ID", y = "Number of IDs")



## Proportion data ---------------------------------------------------------


### Late Cards --------------------------------------------------------------


ggplot(LateSummary, aes(x=SummerRatio)) +
  geom_histogram(binwidth=0.025, fill = "white", color = "black") +
  labs(title = "Summer Cards, Proportion Late", x = "Summer Ratio (Late/On Time Returned by Mail)", y = "Number of years")


ggplot(LateSummary, aes(x=SummerRatio)) +
  geom_histogram(binwidth=0.025, fill = "white", color = "black") +
  labs(title = "Summer Cards, Proportion Late", x = "Summer Ratio (Late/On Time Returned by Mail)", y = "Number of years")



### On Time Cards -----------------------------------------------------------

ggplot(CRCTotals, aes(x=Perc_Reported)) +
  geom_histogram(binwidth=0.025, fill = "white", color = "black") +
  labs(title = "Summer Cards, Proportion Reported", x = "Proportion Reported", y = "Number of years")


ggplot(CRCTotals, aes(x=Perc_Mail)) +
  geom_histogram(binwidth=0.02, fill = "white", color = "black") +
  labs(title = "Summer Cards, Proportion Mail In of Reported", x = "Proportion Mail In of Reported", y = "Number of years")






# Cumulative distribution plots -------------------------------------------

# Cumulative distribution plot (All years, all card types)

plotdist(SummerCRCbyID$TotalCrabID, histo = TRUE, demp = TRUE)





# Fit Distributions -------------------------------------------------------

## All CRC -----------------------------------------------------------------

# Calculate mean and variance
mean(SummerCRCbyID$TotalCrabID) # 6.631119
var(SummerCRCbyID$TotalCrabID)  # 145.3419

#max catch in database, for reference
max(SummerCRCbyID$TotalCrabID) #240

#number of cards with zero catch, for reference
nrow(SummerCRCbyID %>% filter(TotalCrabID == 0)) #831364

#mean != variance --> negative binomial better accounts for overdispersion than poisson where mean = var


# Fit a negative binomial distribution (not zero inflated)
fitdistr(SummerCRCbyID$TotalCrabID, "Negative Binomial")
# size = 0.2322050638, MU = 6.6303789031 

simrnbinom<- rnbinom(n = 1701968, size = 0.2322050638, mu = 6.6303789031)
max(simrnbinom) # 313
table(simrnbinom) # 775345 cards with zero catch

hist(simrnbinom, breaks = max(simrnbinom))

# Fit a pois distribution (not zero inflated) - not a good match
epois(SummerCRCbyID$TotalCrabID, ci = TRUE, conf.level = 0.9) 
# lambda = 6.6
# LCL = 6.627872, UCL = 6.634366


## Mail In CRC -----------------------------------------------------------------

# Calculate mean and variance
mean(SummerCRCbyID0823S$TotalCrabID) # 8.580006
var(SummerCRCbyID0823S$TotalCrabID)  # 208.1442
#mean != variance --> negative binomial better accounts for overdispersion than poisson where mean = var


#max catch in database, for reference
max(SummerCRCbyID0823S$TotalCrabID) #232

#number of cards with zero catch, for reference
nrow(SummerCRCbyID0823S %>% filter(TotalCrabID == 0)) #184033


# Fit a negative binomial distribution (not zero inflated)
fitdistr(SummerCRCbyID0823S$TotalCrabID, "Negative Binomial")
# size = 0.2819724178, MU = 8.5764380596 

simrnbinomS<- rnbinom(n = 440459, size = 0.2819724178, mu = 8.5764380596 )
max(simrnbinomS) # 336
table(simrnbinomS) # 166435 cards with zero catch

hist(simrnbinomS, breaks = max(simrnbinomS))



## Internet CRC -----------------------------------------------------------------

# Calculate mean and variance
mean(SummerCRCbyID0823I$TotalCrabID) # 5.989386
var(SummerCRCbyID0823I$TotalCrabID)  # 122.5015
#mean != variance --> negative binomial better accounts for overdispersion than poisson where mean = var


#max catch in database, for reference
max(SummerCRCbyID0823I$TotalCrabID) #240

#number of cards with zero catch, for reference
nrow(SummerCRCbyID0823I %>% filter(TotalCrabID == 0)) #611019


# Fit a negative binomial distribution (not zero inflated)
fitdistr(SummerCRCbyID0823I$TotalCrabID, "Negative Binomial")
# size = 0.2196628430, MU = 5.9893598086 

simrnbinomI<- rnbinom(n = 1195471, size = 0.2196628430, mu = 5.9893598086 )
max(simrnbinomI) # 344
table(simrnbinomI) # 574258 cards with zero catch

hist(simrnbinomI, breaks = max(simrnbinomI)) 


## Survey Cards ------------------------------------------------------------

# Calculate mean and variance
mean(SurveyData2223$TotalCrabID) # 2.145588
var(SurveyData2223$TotalCrabID)  # 33.02
#mean != variance --> negative binomial better accounts for overdispersion than poisson where mean = var

#max catch in database, for reference
max(SurveyData2223$TotalCrabID) #92

#number of cards with zero catch, for reference
nrow(SurveyData2223 %>% filter(TotalCrabID == 0)) #6110


# Fit a negative binomial distribution (not zero inflated)
fitdistr(SurveyData2223$TotalCrabID, "Negative Binomial")
#      size           mu     
#0.095892276   2.145592254 
#(0.002636081) (0.078398021)

simrnbinomSurv<- rnbinom(n = 5000, size = 0.095892276, mu = 2.145592254 )
max(simrnbinomSurv) # 120
table(simrnbinomSurv) # 3725 cards with zero catch

hist(simrnbinomSurv, breaks = max(simrnbinomSurv)) 


# another idea here if you had more years of data you could:
# simulate probability didn't fish, pois for people who did fish



# Resample to generate new data -------------------------------------------

resample_function <- function(ogdata, nsamp) {
  ncrc<- nrow(ogdata)
  samplerowid<- sample(1:ncrc, nsamp, replace = TRUE) # do we want replace True or False?
  resampledata<- ogdata[samplerowid, ]
  return (list(samplerowid = samplerowid, resampledata = resampledata))
}

plothistresample_function <- function(resampleoutput){
  resampledata<- resampleoutput[[2]]
  hist(resampledata$TotalCrabID, breaks = max(resampledata$TotalCrabID))
}

## All CRC -----------------------------------------------------------------

resampledata_all<- resample_function(ogdata = SummerCRCbyID, nsamp = 100000)
plothistresample_function(resampledata_all)

RowAll<- nrow(SummerCRCbyID)

NumSampAll<- 100000

samplerowid<- sample(1:RowAll, NumSampAll, replace = TRUE) # do we want replace True or False?

hist(SummerCRCbyID[samplerowid, ]$TotalCrabID, breaks = max(SummerCRCbyID[samplerowid, ]$TotalCrabID))



## Mail in CRC -----------------------------------------------------------------

RowS<- nrow(SummerCRCbyID0823S)

NumSampS<- 100000

samplerowidS<- sample(1:RowS, NumSampS, replace = TRUE) # do we want replace True or False?

hist(SummerCRCbyID0823S[samplerowidS, ]$TotalCrabID, breaks = max(SummerCRCbyID0823S[samplerowidS, ]$TotalCrabID))


## Online in CRC -----------------------------------------------------------------

RowI<- nrow(SummerCRCbyID0823I)

NumSampI<- 100000

samplerowidI<- sample(1:RowI, NumSampI, replace = TRUE) # do we want replace True or False?

hist(SummerCRCbyID0823I[samplerowidI, ]$TotalCrabID, breaks = max(SummerCRCbyID0823I[samplerowidI, ]$TotalCrabID))



## Survey in Survey Data ---------------------------------------------------

RowSurv<- nrow(SurveyData2223)
NumSampSurv<- 10000
samplerowidSurv<- sample(1:RowSurv, NumSampSurv, replace = TRUE) # I selected Replace = True since we have less data
hist(SurveyData2223[samplerowidSurv, ]$TotalCrabID, breaks = max(SurveyData2223[samplerowidSurv, ]$TotalCrabID))






# Simulate Data EXAMPLE -----------------------------------------------------------


## Cards per category ------------------------------------------------------


# Define Issued Number of CRCs
Issued <- 50000



# Sample proportion returned on time - Could draw this uniformly, or do a series of simulations where this is incremental
PRet <- .45
PRet<- runif(1, min = min(CRCTotals$Perc_Reported), max (CRCTotals$Perc_Reported))
Returned<- PRet * Issued



# Sample proportion mail/online
PMail <- .3
PMail<- runif(1, min = min(CRCTotals$Perc_Mail), max (CRCTotals$Perc_Mail))
ReturnedMail<- PMail*Returned
ReturnedOnline<- Returned - ReturnedMail


# Sample proportion late
PLate<- .2
PLate <- runif(1, min = min(LateSummary$SummerRatio), max = max(LateSummary$SummerRatio))
Late<- ReturnedMail*PLate


# Calculate Number Not Returned
NonResponse<- Issued - Returned




## Simulate catch ----------------------------------------------------------



# Simulate Mail CRCs
resampledata_Mail<- resample_function(ogdata = SummerCRCbyID0823S, nsamp = ReturnedMail)
SimMailCatch<- resampledata_Mail[[2]]
SimMailCatch_total<- sum(SimMailCatch$TotalCrabID)

# Simulate Online CRCs
resampledata_Online<- resample_function(ogdata = SummerCRCbyID0823I, nsamp = ReturnedOnline)
SimOnlineCatch<- resampledata_Online[[2]]
SimOnlineCatch_total<- sum(SimOnlineCatch$TotalCrabID)


# Simulate Late (Assume matches mail for now)
resampledata_Late<- resample_function(ogdata = SummerCRCbyID0823S, nsamp = Late)
SimLateCatch<- resampledata_Late[[2]]
SimLateCatch_total<- sum(SimLateCatch$TotalCrabID)


# Simulate Didn't Respond
nbinom_NR<-  rnbinom(n = NonResponse,  size = 0.095892276, mu = 2.145592254)
SimNRCatch_total<- sum(nbinom_NR)

# Combine Totals into tibble
SimulatedCatch<- tibble(Mail = SimMailCatch_total, Online = SimOnlineCatch_total, Late = SimLateCatch_total, NR = SimNRCatch_total)

# Calculate Total Catch
SimulatedCatch<- SimulatedCatch %>% 
  mutate(Total = Mail + Online + Late + NR)



