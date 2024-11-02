# Run Sensitivity Analysis
# S. Thurner
# 11/1/24

# WORK IN PROGRESS!!!!!!!!!!!

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



# Sensitivity Testing - P LATE --------------------------------------------

#WHICHPARAM
# 2 = Percent who return on time
# 4 = Percent late

RunSensitivity<- function(whichparam, sensseq, NSim, nissued, setpret, setpmail, setplate, samptype, latecat, samptypenr){
  
  # percent who return on time 
  
  if(whichparam == 2){
    
    SensResults<- tibble(pret = rep(NA, length(sensseq)), NWIFC_meanerror = rep(NA, length(sensseq)), NWIFC_sderror = rep(NA, length(sensseq)), NWIFC_meanpercerror= rep(NA, length(sensseq)), WDFW_meanerror = rep(NA, length(sensseq)), WDFW_sderror = rep(NA, length(sensseq)), WDFW_meanpercerror = rep(NA, length(sensseq)))
    
    for(p in 1:length(sensseq)){
      
      SensResults[p, 1]<- sensseq[p]
      
      TempSim<- RunSims(nloop = NSim, nissued = nissued, setpret = sensseq[p], setpmail = setpmail, setplate = setplate, samptype = samptype, latecat = latecat, samptypenr = samptypenr)
      
      Resultstoplot<- tibble(Total = rep(NA, NSim), NWIFCTotal = rep(NA, NSim), NWIFCErrorinCatch = rep(NA, NSim), NWIFCPercentError = rep(NA, NSim), WDFWTotal = rep(NA, NSim), WDFWErrorinCatch = rep(NA, NSim), WDFWPercentError= rep(NA, NSim))
      
      for(i in 1:500){
        Temp<- TempSim[[i]]
        Resultstoplot[i, 1]<- Temp[[1]][[2]][5]  # Total
        Resultstoplot[i, 2]<-Temp[[1]][[3]][5]  #Total NWIFC
        Resultstoplot[i, 3]<- Temp[[2]][1]  #Error in Catch NWIFC
        Resultstoplot[i, 4]<- Temp[[2]][2]*100  #Percent Error NWIFC
        Resultstoplot[i, 5]<-Temp[[1]][[4]][2]  #Total WDFW
        Resultstoplot[i, 6]<- Temp[[2]][3]  #Error in Catch WDFW
        Resultstoplot[i, 7]<- Temp[[2]][4]*100  #Percent Error WDFW
      }
      
      SensResults[p, 2]<- mean(Resultstoplot$NWIFCErrorinCatch)
      SensResults[p, 3]<- sd(Resultstoplot$NWIFCErrorinCatch)
      SensResults[p, 4]<-  mean(Resultstoplot$NWIFCPercentError)
      SensResults[p, 5]<- mean(Resultstoplot$WDFWErrorinCatch)
      SensResults[p, 6]<- sd(Resultstoplot$WDFWErrorinCatch)
      SensResults[p, 7]<-  mean(Resultstoplot$WDFWPercentError)
      
    }
    
  }
  
  
  
  
  
  
  if(whichparam == 4){
    
    SensResults<- tibble(plate = rep(NA, length(sensseq)), NWIFC_meanerror = rep(NA, length(sensseq)), NWIFC_sderror = rep(NA, length(sensseq)), NWIFC_meanpercerror= rep(NA, length(sensseq)), WDFW_meanerror = rep(NA, length(sensseq)), WDFW_sderror = rep(NA, length(sensseq)), WDFW_meanpercerror = rep(NA, length(sensseq)))
    
    for(p in 1:length(sensseq)){
      
      SensResults[p, 1]<- sensseq[p]
      
      TempSim<- RunSims(nloop = NSim, nissued = nissued, setpret = setpret, setpmail = setpmail, setplate = sensseq[p], samptype = samptype, latecat = latecat, samptypenr = samptypenr)
      
      Resultstoplot<- tibble(Total = rep(NA, NSim), NWIFCTotal = rep(NA, NSim), NWIFCErrorinCatch = rep(NA, NSim), NWIFCPercentError = rep(NA, NSim), WDFWTotal = rep(NA, NSim), WDFWErrorinCatch = rep(NA, NSim), WDFWPercentError= rep(NA, NSim))
      
      for(i in 1:500){
        Temp<- TempSim[[i]]
        Resultstoplot[i, 1]<- Temp[[1]][[2]][5]  # Total
        Resultstoplot[i, 2]<-Temp[[1]][[3]][5]  #Total NWIFC
        Resultstoplot[i, 3]<- Temp[[2]][1]  #Error in Catch NWIFC
        Resultstoplot[i, 4]<- Temp[[2]][2]*100  #Percent Error NWIFC
        Resultstoplot[i, 5]<-Temp[[1]][[4]][2]  #Total WDFW
        Resultstoplot[i, 6]<- Temp[[2]][3]  #Error in Catch WDFW
        Resultstoplot[i, 7]<- Temp[[2]][4]*100  #Percent Error WDFW
      }
      
      SensResults[p, 2]<- mean(Resultstoplot$NWIFCErrorinCatch)
      SensResults[p, 3]<- sd(Resultstoplot$NWIFCErrorinCatch)
      SensResults[p, 4]<-  mean(Resultstoplot$NWIFCPercentError)
      SensResults[p, 5]<- mean(Resultstoplot$WDFWErrorinCatch)
      SensResults[p, 6]<- sd(Resultstoplot$WDFWErrorinCatch)
      SensResults[p, 7]<-  mean(Resultstoplot$WDFWPercentError)
      
    }
    
  }
  
  return(SensResults)
}




# Sensitivity Test: Sensitivity of results to % Late, Late same as mail

plateseq<- seq(from = 0, to = 0.5, by = .01)
Sensitivity1<- RunSensitivity(whichparam = 4, sensseq = plateseq, NSim = 500, nissued = 20000, setpret = 99, setpmail = 99, setplate = NA, samptype = 2, latecat = 3, samptypenr = 2)
  
# Sensitivity1melt<- melt(Sensitivity1[,c(1,2,5)], id.vars="plate")

Sensitivity1longer<- Sensitivity1 %>% 
  pivot_longer(-plate, 
               names_to = c("method", ".value"), 
               names_sep="_" )

Sensitivity1longer<- Sensitivity1longer %>% 
  mutate(LowerCI = meanerror - 1.96*sderror, UpperCI = meanerror + 1.96*sderror)

Sensitivity1longerEVENLONGER<- Sensitivity1longer[,-c(4,5)] %>% pivot_longer(-c(plate, method))

# ggplot(Sensitivity1longer, aes(x = plate, y = meanerror, col = method))+
#   geom_line() +
#   labs(title = "Late same as mail", x = "plate", y = "error")


# total catch vs error -- plotted as lines
pdf("Plots/sensitivity1_v2.pdf")
ggplot(Sensitivity1longerEVENLONGER, aes(x = plate, y = value, col = method))+
  geom_line(aes(linetype = name), size = 1) +
  scale_linetype_manual(values = c("dotdash", "solid", "dotdash")) + 
  labs(title = "Late same as mail", x = "plate", y = "error")
dev.off()




# Sensitivity Test: Sensitivity of results to % Late, Late same as Survey

plateseq<- seq(from = 0, to = 0.5, by = .01)
Sensitivity2<- RunSensitivity(whichparam = 4, sensseq = plateseq, NSim = 500, nissued = 20000, setpret = 99, setpmail = 99, setplate = NA, samptype = 2, latecat = 4, samptypenr = 2)

Sensitivity2longer<- Sensitivity2 %>% 
  pivot_longer(-plate, 
               names_to = c("method", ".value"), 
               names_sep="_" )

Sensitivity2longer<- Sensitivity2longer %>% 
  mutate(LowerCI = meanerror - 1.96*sderror, UpperCI = meanerror + 1.96*sderror)

Sensitivity2longerEVENLONGER<- Sensitivity2longer[,-c(4,5)] %>% pivot_longer(-c(plate, method))


# Scatterplot of total catch vs error
pdf("Plots/sensitivity2_v2.pdf")
ggplot(Sensitivity2longerEVENLONGER, aes(x = plate, y = value, col = method))+
  geom_line(aes(linetype = name), size = 1) +
  scale_linetype_manual(values = c("dotdash", "solid", "dotdash")) + 
  labs(title = "Late same as survey", x = "plate", y = "error")
dev.off()


# Sensitivity Test: Sensitivity of results to % returned on time

pretseq<- seq(from = 0, to = .8, by = .02)
Sensitivity3<- RunSensitivity(whichparam = 2, sensseq = pretseq, NSim = 500, nissued = 20000, setpret = NA, setpmail = 99, setplate = 99, samptype = 2, latecat = 3, samptypenr = 2)

Sensitivity3longer<- Sensitivity3 %>% 
  pivot_longer(-pret, 
               names_to = c("method", ".value"), 
               names_sep="_" )

Sensitivity3longer<- Sensitivity3longer %>% 
  mutate(LowerCI = meanerror - 1.96*sderror, UpperCI = meanerror + 1.96*sderror)

Sensitivity3longerEVENLONGER<- Sensitivity3longer[,-c(4,5)] %>% pivot_longer(-c(pret, method))


# Scatterplot of total catch vs error
pdf("Plots/sensitivity3v2.pdf")
ggplot(Sensitivity3longerEVENLONGER, aes(x = pret, y = value, col = method))+
  geom_line(aes(linetype = name), size = 1) +
  scale_linetype_manual(values = c("dotdash", "solid", "dotdash")) + 
  labs(title = "Late same as survey", x = "plate", y = "error")
dev.off()
