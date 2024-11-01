# Run Simulations
# S. Thurner
# 10/29/24

source("SimulateV2.R") #loads data, functions

library(gridExtra)
library(grid)
library(reshape2)

# Reminder of Simulation Function Parameters
# nissued = # of CRCs issued
# setpret/setpmail/setplate, if = 99 use runif to draw percent returned, else = the percent returned specified
# samptype for mail/online/late if = 1 resample, if = 2 nbinom
# latecat = 3 for mail, 4 for survey
# samptypenr for nonresponse survey type

# function(nissued, setpret, setpmail, setplate, samptype, latecat, samptypenr)



# Function to run simulations ---------------------------------------------

# Loop function
RunSims<- function(nloop, nissued, setpret, setpmail, setplate, samptype, latecat, samptypenr){

  
  #Initialize empty list
  StoreSimResults<- vector("list", length = nloop)
  
  for(sim in 1:nloop){
    
    # simulate one year of catch
    SimRes<- SimulateCatch(nissued = nissued, setpret = setpret, setpmail = setpmail, setplate = setplate, samptype = samptype, latecat = latecat, samptypenr = samptypenr)
    
    # calculate 'error' 
    ErrorinCatchNWIFC<-  SimRes[[3]]$TotalNWIFC - SimRes[[2]]$Total  # estimated - observed, + would be overestimate, - would be underestimate
    ErrorinCatchWDFW<- SimRes[[4]]$TotalWDFW - SimRes[[2]]$Total  # estimated - observed, + would be overestimate, - would be underestimate
    
    #calculate percent error
    PercentErrorNWIFC<- ErrorinCatchNWIFC/SimRes[[2]]$Total #error as % of true catch
    PercentErrorWDFW<- ErrorinCatchWDFW/SimRes[[2]]$Total #error as % of true catch
    
    # save error as tibble
    ErrorTibble<- tibble(ErrorinCatchNWIFC = ErrorinCatchNWIFC, PercentErrorNWIFC = PercentErrorNWIFC, ErrorinCatchWDFW = ErrorinCatchWDFW, PercentErrorWDFW = PercentErrorWDFW)
    
    # store catch and error tible
    SimRes<- list(SimRes, ErrorTibble)
    
    StoreSimResults[[sim]]<- SimRes
    
  }
  
  return (StoreSimResults)
}


# Function to plot simulation results -------------------------------------

PlotSims<- function(StoreSimResults, PlotTitle){
  
  NSim<- length(StoreSimResults)
  
  Resultstoplot<- tibble(Total = rep(NA, NSim), TotalNWIFC = rep(NA, NSim), ErrorinCatchNWIFC = rep(NA, NSim), PercentErrorNWIFC = rep(NA, NSim), TotalWDFW = rep(NA, NSim), ErrorinCatchWDFW = rep(NA, NSim), PercentErrorWDFW= rep(NA, NSim))
  
  for(i in 1:NSim){
    Temp<- StoreSimResults[[i]]
    Resultstoplot[i, 1]<- Temp[[1]][[2]][5]  # Total
    Resultstoplot[i, 2]<-Temp[[1]][[3]][5]  #Total NWIFC
    Resultstoplot[i, 3]<- Temp[[2]][1]  #Error in Catch NWIFC
    Resultstoplot[i, 4]<- Temp[[2]][2]*100  #Percent Error NWIFC
    Resultstoplot[i, 5]<-Temp[[1]][[4]][2]  #Total WDFW
    Resultstoplot[i, 6]<- Temp[[2]][3]  #Error in Catch WDFW
    Resultstoplot[i, 7]<- Temp[[2]][4]*100  #Percent Error WDFW
  }
 
 # NWIFC PLOTS 
  
  # Histogram of Error in Catch
  plot1<- ggplot(Resultstoplot, aes(x = ErrorinCatchNWIFC)) +
    geom_histogram(binwidth = 50, fill = "white", color = "black") +
    labs(title = "NWIFC", x = "Error in Catch", y = "Number of Simulations")
  
  # Histogram of Error in Catch
  plot2<- ggplot(Resultstoplot, aes(x = PercentErrorNWIFC)) +
    geom_histogram(binwidth = 0.5, fill = "white", color = "black") +
    labs(title = "NWIFC", x = "Percent Error in Catch", y = "Number of Simulations")
  
  # Scatterplot of total catch vs error
  plot3<- ggplot(Resultstoplot, aes(x = Total, y = TotalNWIFC)) +
    geom_point() +
    labs(title = "NWIFC", x = "Total Catch", y = "NWIFC Estimated Catch")
  
  # WDFW PLOTS 
  
  # Histogram of Error in Catch
  plot4<- ggplot(Resultstoplot, aes(x = ErrorinCatchWDFW)) +
    geom_histogram(binwidth = 50, fill = "white", color = "black") +
    labs(title = "WDFW Original", x = "Error in Catch", y = "Number of Simulations")
  
  # Histogram of Error in Catch
  plot5<- ggplot(Resultstoplot, aes(x = PercentErrorWDFW)) +
    geom_histogram(binwidth = 0.5, fill = "white", color = "black") +
    labs(title = "WDFW Original", x = "Percent Error in Catch", y = "Number of Simulations")
  
  # Scatterplot of total catch vs error
  plot6<- ggplot(Resultstoplot, aes(x = Total, y = TotalWDFW)) +
    geom_point() +
    labs(title = "WDFW Original", x = "Total Catch", y = "WDFW Estimated Catch")
  
  plot7<- ggplot(Resultstoplot, aes(x = ErrorinCatchWDFW, y = ErrorinCatchNWIFC)) +
    geom_point() +
    labs(x = "Error in Catch WDFW", y = "Error in Catch NWIFC")
  
  plot8<- ggplot(Resultstoplot, aes(x = TotalWDFW, y = TotalNWIFC)) +
    geom_point() +
    labs(x = "Total WDFW", y = "Total NWIFC")
  
  grid.arrange(plot1, plot2, plot3, plot4, plot5, plot6, plot7,plot8, ncol=3, top = textGrob(PlotTitle))
  
  
}





# Simulation 1: Everything Random, All nbinom, Late from Mail -----------------------------

Simulation1<- RunSims(nloop = 1000, nissued = 20000, setpret = 99, setpmail = 99, setplate = 99, samptype = 2, latecat = 3, samptypenr = 2)

#pdf("Plots/Simulation1.pdf")
PlotSims(StoreSimResults = Simulation1, PlotTitle = "Everything Random, All Nbinom, Late from Mail")
#dev.off()

# Simulation 2: Everything Random, All nbinom, Late from survey -----------

Simulation2<- RunSims(nloop = 1000, nissued = 20000, setpret = 99, setpmail = 99, setplate = 99, samptype = 2, latecat = 4, samptypenr = 2)

#pdf("Plots/Simulation2.pdf")
PlotSims(StoreSimResults = Simulation2, PlotTitle = "Everything Random, All Nbinom, Late from Survey")
#dev.off()






















