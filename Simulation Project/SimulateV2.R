# Simulation Code
# S. Thurner
# 11/5/24

# Setting up crab simulations, contains calculations
# Look at "VisualizeSimulateData_ST.R" for data visualization

library(tidyverse)
library(fitdistrplus)
library(stats4)
library(MASS)
library(EnvStats)


# Read in Data ------------------------------------------------------------

# Look at "Data.R" for reading in/cleaning data
# Look at "SimulateData.R" for generating negative binomial distributions


## Negative binomial dist parameters -------------------------------
# reads in parameters that were saved
nbinom_SummerCRCbyID<- readRDS("nbinomRDS/nbinom_SummerCRCbyID.rds")
nbinom_SummerCRCbyID2<- nbinom_SummerCRCbyID[1]
allsize<- nbinom_SummerCRCbyID2$estimate[1]
allmu<- nbinom_SummerCRCbyID2$estimate[2]

nbinom_SummerCRCbyID0823S<- readRDS("nbinomRDS/nbinom_SummerCRCbyID0823S.rds")
nbinom_SummerCRCbyID0823S2<- nbinom_SummerCRCbyID0823S[1]
mailsize<- nbinom_SummerCRCbyID0823S2$estimate[1]
mailmu<- nbinom_SummerCRCbyID0823S2$estimate[2]

nbinom_SummerCRCbyID0823I<- readRDS("nbinomRDS/nbinom_SummerCRCbyID0823I.rds")
nbinom_SummerCRCbyID0823I2<- nbinom_SummerCRCbyID0823I[1]
onlinesize<- nbinom_SummerCRCbyID0823I2$estimate[1]
onlinemu<- nbinom_SummerCRCbyID0823I2$estimate[2]

nbinom_SurveyData2223<- readRDS("nbinomRDS/nbinom_SurveyData2223.rds")
nbinom_SurveyData22232<- nbinom_SurveyData2223[1]
surveysize<- nbinom_SurveyData22232$estimate[1]
surveymu<- nbinom_SurveyData22232$estimate[2]

## Data Tibbles ------------------------------------------------------
# reads in tibbles

SummerCRCbyID<- readRDS("DataRDS/SummerCRCbyID.rds")
SummerCRCbyID0823S<- readRDS("DataRDS/SummerCRCbyID0823S.rds")
SummerCRCbyID0823I<- readRDS("DataRDS/SummerCRCbyID0823I.rds")
SurveyData2223<- readRDS("DataRDS/SurveyData2223.rds")
LateSummary<- readRDS("DataRDS/LateSummary.rds")
CRCTotals<- readRDS("DataRDS/CRCTotals.rds")



# Generate Functions ------------------------------------------------------


## Resample data function --------------------------------------------------

resample_function <- function(ogdata, nsamp) {
  ncrc<- nrow(ogdata)
  samplerowid<- sample(1:ncrc, nsamp, replace = TRUE) # do we want replace True or False?
  resampledata<- ogdata[samplerowid, ]
  return (list(samplerowid = samplerowid, resampledata = resampledata))
}



# Simulate Catch Function -------------------------------------------------


## Parameter definitions ---------------------------------------------------

# nissued = # of CRCs issued
# setpret, if = 99 use runif to draw percent returned, else = the percent returned specified
# samptype for mail/online/late if = 1 resample, if = 2 nbinom
# latecat = should you sample the late population from mail or survey group = 3 for mail, 4 for survey
# samptypenr for nonresponse survey type, same parameters as samp type


SimulateCatch<- function(nissued, setpret, setpmail, setplate, samptype, latecat, samptypenr){
  

## Define number of cards in each category ---------------------------------

  # Define Issued Number of CRCs
  Issued <- nissued
  
  
  # Sample proportion returned on time - Could draw this uniformly, or do a series of simulations where this is incremental
  
  if(setpret == 99){
    PRet<- runif(1, min = min(CRCTotals$Perc_Reported), max (CRCTotals$Perc_Reported))
  } else (PRet<- setpret)
  

 
  Returned<- round(PRet * Issued, digits = 0) #calculate # cards returned, round to whole number
  
  
  # Sample proportion mail/online from uniform distribution if = 99
  if(setpmail == 99){
    PMail<- runif(1, min = min(CRCTotals$Perc_Mail), max (CRCTotals$Perc_Mail))
  } else{PMail <- setpmail}
  
  ReturnedMail<- round(PMail*Returned, digits = 0)  #calculate # returned via mail, round
  ReturnedOnline<- Returned - ReturnedMail  #calculate # returned via online
  
  
  # Sample proportion late from uniform distribution if = 99
  
  if(setplate== 99){
    PLate <- runif(1, min = min(LateSummary$SummerRatio), max = max(LateSummary$SummerRatio))
  } else( PLate<- setplate)
  
  Late<- round(ReturnedMail*PLate, digits = 0)  #calculate # returned late, round to whole number
  
  TempCalc<- Late + Returned
  
  if(Issued < TempCalc){
    Late <- Issued - Returned
    print("Assigned more late cards than cards left, rethink parameters")
  }
  
  
  # Calculate Number Not Returned
  NoResponse<- Issued - Returned - Late # corrected this on 11/5/24
  
  if(NoResponse < 0){
    print("ERROR, NO RESPONSE < 0")
    break
  }
  
  TempCalc<- NoResponse + Returned + Late
  
  if(TempCalc != Issued){
    print("ERROR, INCORRECT TOTAL NUMBER OF CARDS")
    break
  }
  
  
  # Create Tibble to return
  
  NumCat<- tibble(Issued = Issued, Returned = Returned, ReturnedMail = ReturnedMail,
                  ReturnedOnline = ReturnedOnline, Late = Late, NoResponse = NoResponse)
  
  
  
  ## Simulate catch ----------------------------------------------------------
  
  # type 1 = resample
  # type 2 = nbinom
  

  ### Mail and online ---------------------------------------------------------

  
  if(samptype == 1){
    # Simulate Mail CRCs using resample function
    resampledata_Mail<- resample_function(ogdata = SummerCRCbyID0823S, nsamp = ReturnedMail)
    SimMailCatch<- resampledata_Mail[[2]] # Catch tibble
    SimMailCatch_total<- sum(SimMailCatch$TotalCrabID) #calculates total mail catch
    MeanMailCatch<- mean(SimMailCatch$TotalCrabID)  #calculates mean catch/id
    
    # Simulate Online CRCs using resample function
    resampledata_Online<- resample_function(ogdata = SummerCRCbyID0823I, nsamp = ReturnedOnline)
    SimOnlineCatch<- resampledata_Online[[2]] #catch tibble
    SimOnlineCatch_total<- sum(SimOnlineCatch$TotalCrabID) #calculates total online catch
    
    MeanOnTimeCatch<- mean(c(SimMailCatch$TotalCrabID, SimOnlineCatch$TotalCrabID)) #mean catch from all people who responded on time
    
  } else if (samptype == 2){
    
    # Simulate Mail CRCs from nbinom distribution
    simrnbinomS<- rnbinom(n = ReturnedMail, size = mailsize, mu = mailmu )
    SimMailCatch_total<- sum(simrnbinomS) #total mail catch
    MeanMailCatch<- mean(simrnbinomS) #average mail catch/id
    
    # Simulate Online CRCs from nbinom
    simrnbinomI<- rnbinom(n = ReturnedOnline, size = onlinesize, mu = onlinemu )
    SimOnlineCatch_total <- sum(simrnbinomI)
    
    MeanOnTimeCatch<- mean(c(simrnbinomS, simrnbinomI)) #mean catch/id from all on time respondents
  }
  

  ### Late Catch --------------------------------------------------------------

  # type 3 = mail
  # type 4 = survey
  
  if(latecat == 3){
    
    # Assume comes from mail distribution
    
    if(samptype == 1){
       # resample from on time mail data
      resampledata_Late<- resample_function(ogdata = SummerCRCbyID0823S, nsamp = Late)
      SimLateCatch<- resampledata_Late[[2]]
      SimLateCatch_total<- sum(SimLateCatch$TotalCrabID)
      
    } else if (samptype == 2){
      # sample from on time mail nbinom dist
      simrnbinomL<- rnbinom(n = Late, size = mailsize, mu = mailmu )
      SimLateCatch_total<- sum(simrnbinomL)
      
    }
    

  } else if (latecat == 4){
    # Assume comes from Nonresponse survey distribution
    
    if(samptypenr == 1){
      # resample from 2022-23 survey data
      resampledata_Late <- resample_function(ogdata = SurveyData2223, nsamp = Late)
      SimLateCatch<- resampledata_Late[[2]]
      SimLateCatch_total<- sum(SimLateCatch$TotalCrabID)
      
    } else if (samptypenr == 2){
      # sample from 2022-23 survey data nbinom dist
      simrnbinomL<-  rnbinom(n = Late,  size = surveysize, mu = surveymu)
      SimLateCatch_total<- sum(simrnbinomL)
      
    }
    
  }
  


  ### Non-Respondants ------------------------------------------------------------
  # Simulate people who didnt turn in CRC using survey data
  # type 1 = resample
  # type 2 = nbinom
  
  
  if(samptypenr == 1){
    # resample from 2022-23 survey data
    resampledata_NR<- resample_function(ogdata = SurveyData2223, nsamp = NoResponse)
    SimNRCatch<- resampledata_NR[[2]]
    SimNRCatch_total<- sum(SimNRCatch$TotalCrabID)
    
    
  } else if(samptypenr == 2){
    # sample from 2022-23 survey data nbinom distribution
    nbinom_NR<-  rnbinom(n = NoResponse,  size = surveysize, mu = surveymu)
    SimNRCatch_total<- sum(nbinom_NR)
    
  }
  
  # Combine Totals into tibble
  SimulatedCatch<- tibble(Mail = SimMailCatch_total, Online = SimOnlineCatch_total, Late = SimLateCatch_total, NR = SimNRCatch_total)
  
  # Calculate Total Catch
  SimulatedCatch<- SimulatedCatch %>% 
    mutate(Total = Mail + Online + Late + NR)
  


  # NWIFC Estimate ----------------------------------------------------------
  # Calculate NWIFC Proposed Catch
  
  #Calculate TR: Total crabs caught by on-time respondents
  TR<- SimMailCatch_total + SimOnlineCatch_total
  
  #Calculate TL: Total crabs caught by late-respondents
  #Assumes that this population is more similar to mail-in respondents
  NL<- ReturnedMail * mean(LateSummary$SummerRatio)
  TL<- NL * MeanMailCatch
  
  # Calculate TN: Total Crabs Caught by non-respondents
  TN<- (Issued - Returned - NL)*mean(SurveyData2223$TotalCrabID)
  
  
  EstimatedCatchNWIFC<- tibble(TR = TR, NL = NL, TL = TL, TN = TN)
  EstimatedCatchNWIFC<- EstimatedCatchNWIFC %>% mutate(TotalNWIFC = TR + TL + TN)
  
  

# WDFW Estimated Catch ----------------------------------------------------

  # Bias correction, from equation Y= -0.50(X)2 + 1.39(X) + 0.097 in 2023 Summer Memo
  
  BC<- -0.50*(PRet)^2 + 1.39*(PRet) + 0.097
  
  # Calculate Total Catch WDFW
  
  TotalWDFW<- Issued * TR/Returned * BC
  
  # TotalWDFW2<-  Issued * MeanOnTimeCatch * BC #should be same as TotalWDFW, is.
  
  
  EstimatedCatchWDFW<- tibble(BC = BC, TotalWDFW = TotalWDFW)
  
  
  return(list(NumCat, SimulatedCatch, EstimatedCatchNWIFC, EstimatedCatchWDFW))
}

# nissued = # of CRCs issued
# setpret/setpmail/setplate, if = 99 use runif to draw percent returned, else = the percent returned specified
# samptype for mail/online/late if = 1 resample, if = 2 nbinom
# latecat = 3 for mail, 4 for survey
# samptypenr for nonresponse survey type

# Running different parameters just to make sure that it is working
#TESTING<- SimulateCatch(nissued = 10000, setpret = .50, setpmail = .5, setplate = .05, samptype = 1, latecat = 3, samptypenr = 1)
#TESTING<- SimulateCatch(nissued = 10000, setpret = .50, setpmail = .5, setplate = .05, samptype = 2, latecat = 3, samptypenr = 1)
#TESTING<- SimulateCatch(nissued = 10000, setpret = 99, setpmail = 99, setplate = 99, samptype = 1, latecat = 3, samptypenr = 1)
#TESTING<- SimulateCatch(nissued = 10000, setpret = 99, setpmail = 99, setplate = 99, samptype = 1, latecat = 4, samptypenr = 1)
#TESTING<- SimulateCatch(nissued = 10000, setpret = 99, setpmail = 99, setplate = 99, samptype = 1, latecat = 4, samptypenr = 2)


# 
# nloop <- 5
# 
# #Initialize empty list
# Simulation1<- vector("list", length = nloop)
# 
# for(sim in 1:nloop){
#   
#   SimRes<- SimulateCatch(nissued = 20000, setpret = 99, setpmail = 99, setplate = 99, samptype = 2, latecat = 3, samptypenr = 2)
#   
#   ErrorinCatch<-  SimRes[[3]]$TotalNWIFC - SimRes[[2]]$Total  # estimated - observed, + would be overestimate, - would be underestimate
#   PercentError<- ErrorinCatch/SimRes[[2]]$Total #error as % of true catch
#   
#   ErrorTibble<- tibble(ErrorinCatch = ErrorinCatch, PercentError = PercentError)
#   
#   SimRes<- list(SimRes, ErrorTibble)
#   
#   Simulation1[[sim]]<- SimRes
#   
# }
