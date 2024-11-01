# Simulate Data
# 10/28/24
# S. Thurner
# Fit negative binomial distributions to the data

library(tidyverse)
library(fitdistrplus)
library(stats4)
library(MASS)
library(EnvStats)

# Read in Data
SummerCRCbyID<- readRDS("DataRDS/SummerCRCbyID.rds")
SummerCRCbyID0823S<- readRDS("DataRDS/SummerCRCbyID0823S.rds")
SummerCRCbyID0823I<- readRDS("DataRDS/SummerCRCbyID0823I.rds")
SurveyData2223<- readRDS("DataRDS/SurveyData2223.rds")
SurveyData2223<- readRDS("DataRDS/SurveyData2223.rds")
LateSummary<- readRDS("DataRDS/LateSummary.rds")
CRCTotals<- readRDS("DataRDS/CRCTotals.rds")

# Fit Distributions -------------------------------------------------------

simnbinom<- function(param, nsamp){
  simrnbinom<- rnbinom(n = nsamp, size = param[1], mu = param[2])
  return()
}


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
nbinom_SummerCRCbyID<- fitdistr(SummerCRCbyID$TotalCrabID, "Negative Binomial")
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
nbinom_SummerCRCbyID0823S<- fitdistr(SummerCRCbyID0823S$TotalCrabID, "Negative Binomial")
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
nbinom_SummerCRCbyID0823I<- fitdistr(SummerCRCbyID0823I$TotalCrabID, "Negative Binomial")
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
nbinom_SurveyData2223<- fitdistr(SurveyData2223$TotalCrabID, "Negative Binomial")
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
# Creates resample function -- samples nsamp number of cards from specified original data set

resample_function <- function(ogdata, nsamp) {
  ncrc<- nrow(ogdata)
  samplerowid<- sample(1:ncrc, nsamp, replace = TRUE) # do we want replace True or False?
  resampledata<- ogdata[samplerowid, ]
  return (list(samplerowid = samplerowid, resampledata = resampledata))
}

# plots a histogram of the resampled data
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



# Save Distribution information -------------------------------------------

saveRDS(nbinom_SummerCRCbyID, "nbinomRDS/nbinom_SummerCRCbyID.rds")
saveRDS(nbinom_SummerCRCbyID0823S, "nbinomRDS/nbinom_SummerCRCbyID0823S.rds")
saveRDS(nbinom_SummerCRCbyID0823I, "nbinomRDS/nbinom_SummerCRCbyID0823I.rds")
saveRDS(nbinom_SurveyData2223, "nbinomRDS/nbinom_SurveyData2223.rds")

