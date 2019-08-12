setwd("C:/Users/chico/Documents/Math 388")
source("Graphs and weights functions.R")
require(dplyr)
require(ggplot2)
# The following lines are to install the patchwork package
# install.packages("devtools")
# library(devtools)
# install_github("thomasp85/patchwork")
require(patchwork)
require(reshape2)
require(ggrepel)

# UNCOMMENT FOR 2019 DATA
AYTM <- read.csv("Clean_AYTM.csv",header=TRUE)
CATI <- read.csv("Clean_CATI.csv",header=TRUE)
RICS <- read.csv("Clean_RICS.csv",header=TRUE)

# UNCOMMENT FOR 2018 DATA
# AYTM <- read.csv("Clean_AYTM18.csv",header=TRUE)
# CATI <- read.csv("Clean_CATI18.csv",header=TRUE)
# RICS <- read.csv("Clean_RICS18.csv",header=TRUE)

SMALLEST <- 50
LARGEST <- 550
STEP <- 25
NUMSIMS <- 100
ITERATIONS <- 26
SAMPLE_SIZE <- 600

# CRcFrame <- data.frame(rbind(total = c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)))
# names(CRcFrame) <- c("male","female","genderU","age1","age2","age3","age4","ageU","white","black","otherR","raceU","cell",
#                      "land","region1","region2","region3","region4","region5","region6","regionU")
# ARcFrame <- data.frame(rbind(total = c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)))
# names(ARcFrame) <- c("male","female","genderU","age1","age2","age3","age4","ageU","white","black","otherR","raceU","cell",
#                      "land","region1","region2","region3","region4","region5","region6","regionU")
# CAcFrame <- data.frame(rbind(total = c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)))
# names(CAcFrame) <- c("male","female","genderU","age1","age2","age3","age4","ageU","white","black","otherR","raceU","cell",
#                      "land","region1","region2","region3","region4","region5","region6","regionU")
ACRcFrame <- data.frame(rbind(total = c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)))
names(ACRcFrame) <- c("male","female","genderU","age1","age2","age3","age4","ageU","white","black","otherR","raceU","cell",
                     "land","region1","region2","region3","region4","region5","region6","regionU")


# CRaFrame <- data.frame(cbind(names = c("male","female","age1","age2","age3","age4","white","black","otherR","cell",
#                                        "land","region1","region2","region3","region4","region5","region6")))
# ARaFrame <- data.frame(cbind(names = c("male","female","age1","age2","age3","age4","white","black","otherR","cell",
#                                        "land","region1","region2","region3","region4","region5","region6")))
# CAaFrame <- data.frame(cbind(names = c("male","female","age1","age2","age3","age4","white","black","otherR","cell",
#                                        "land","region1","region2","region3","region4","region5","region6")))
ACRaFrame <- data.frame(cbind(names = c("male","female","age1","age2","age3","age4","white","black","otherR","cell",
                                        "land","region1","region2","region3","region4","region5","region6")))

# CRdesign <- c(rep(0,NUMSIMS))
# 
# ARdesign <- c(rep(0,NUMSIMS))
# 
# CAdesign <- c(rep(0,NUMSIMS))

ACRdesign <- c(rep(0,NUMSIMS))

# CRICS <- c(rep(0,NUMSIMS))
# CRICC <- c(rep(0,NUMSIMS))
# CRICE <- c(rep(0,NUMSIMS))
# 
# ARICS <- c(rep(0,NUMSIMS))
# ARICC <- c(rep(0,NUMSIMS))
# ARICE <- c(rep(0,NUMSIMS))
# 
# CAICS <- c(rep(0,NUMSIMS))
# CAICC <- c(rep(0,NUMSIMS))
# CAICE <- c(rep(0,NUMSIMS))

ACRICS <- c(rep(0,NUMSIMS))
ACRICC <- c(rep(0,NUMSIMS))
ACRICE <- c(rep(0,NUMSIMS))

# CRgenderFrame <- data.frame(cbind(size = seq(SMALLEST,LARGEST,STEP)))
# CRageFrame <- data.frame(cbind(size = seq(SMALLEST,LARGEST,STEP)))
# CRraceFrame <- data.frame(cbind(size = seq(SMALLEST,LARGEST,STEP)))
# CRregionFrame <- data.frame(cbind(size = seq(SMALLEST,LARGEST,STEP)))
# CRphoneFrame <- data.frame(cbind(size = seq(SMALLEST,LARGEST,STEP)))
# CRdesignAverages <- data.frame(cbind(size = seq(SMALLEST,LARGEST,STEP)))
# CRdesignQuants <- data.frame(cbind(size = seq(SMALLEST,LARGEST,STEP)))
# CRaverageICS <- data.frame(cbind(size = seq(SMALLEST,LARGEST,STEP)))
# CRaverageICC <- data.frame(cbind(size = seq(SMALLEST,LARGEST,STEP)))
# CRaverageICE <- data.frame(cbind(size = seq(SMALLEST,LARGEST,STEP)))
# CRquantsICS <- data.frame(cbind(size = seq(SMALLEST,LARGEST,STEP)))
# CRquantsICC <- data.frame(cbind(size = seq(SMALLEST,LARGEST,STEP)))
# CRquantsICE <- data.frame(cbind(size = seq(SMALLEST,LARGEST,STEP)))
# 
# ARgenderFrame <- data.frame(cbind(size = seq(SMALLEST,LARGEST,STEP)))
# ARageFrame <- data.frame(cbind(size = seq(SMALLEST,LARGEST,STEP)))
# ARraceFrame <- data.frame(cbind(size = seq(SMALLEST,LARGEST,STEP)))
# ARregionFrame <- data.frame(cbind(size = seq(SMALLEST,LARGEST,STEP)))
# ARphoneFrame <- data.frame(cbind(size = seq(SMALLEST,LARGEST,STEP)))
# ARdesignAverages <- data.frame(cbind(size = seq(SMALLEST,LARGEST,STEP)))
# ARdesignQuants <- data.frame(cbind(size = seq(SMALLEST,LARGEST,STEP)))
# ARaverageICS <- data.frame(cbind(size = seq(SMALLEST,LARGEST,STEP)))
# ARaverageICC <- data.frame(cbind(size = seq(SMALLEST,LARGEST,STEP)))
# ARaverageICE <- data.frame(cbind(size = seq(SMALLEST,LARGEST,STEP)))
# ARquantsICS <- data.frame(cbind(size = seq(SMALLEST,LARGEST,STEP)))
# ARquantsICC <- data.frame(cbind(size = seq(SMALLEST,LARGEST,STEP)))
# ARquantsICE <- data.frame(cbind(size = seq(SMALLEST,LARGEST,STEP)))
# 
# CAgenderFrame <- data.frame(cbind(size = seq(SMALLEST,LARGEST,STEP)))
# CAageFrame <- data.frame(cbind(size = seq(SMALLEST,LARGEST,STEP)))
# CAraceFrame <- data.frame(cbind(size = seq(SMALLEST,LARGEST,STEP)))
# CAregionFrame <- data.frame(cbind(size = seq(SMALLEST,LARGEST,STEP)))
# CAphoneFrame <- data.frame(cbind(size = seq(SMALLEST,LARGEST,STEP)))
# CAdesignAverages <- data.frame(cbind(size = seq(SMALLEST,LARGEST,STEP)))
# CAdesignQuants <- data.frame(cbind(size = seq(SMALLEST,LARGEST,STEP)))
# CAaverageICS <- data.frame(cbind(size = seq(SMALLEST,LARGEST,STEP)))
# CAaverageICC <- data.frame(cbind(size = seq(SMALLEST,LARGEST,STEP)))
# CAaverageICE <- data.frame(cbind(size = seq(SMALLEST,LARGEST,STEP)))
# CAquantsICS <- data.frame(cbind(size = seq(SMALLEST,LARGEST,STEP)))
# CAquantsICC <- data.frame(cbind(size = seq(SMALLEST,LARGEST,STEP)))
# CAquantsICE <- data.frame(cbind(size = seq(SMALLEST,LARGEST,STEP)))

# The lines above this comment are just set-up, setwd sets the workspace to the folder I'm working in, so I don't
# have to type in the full file path for any file, I can simply just reference the file names.
# The "require" lines cause the script to require the use of certain packages. The script won't run without installing
# these packages first. The other variables I've defined for use later on.

# Creating samples
# Samples between CATI and RICS
# CRgraphs <- list()
# CRcFrames <- list()
# CRaFrames <- list()
# CRdesignEffects <- list()
# CRtotalICS <- list()
# CRtotalICC <- list()
# CRtotalICE <- list()
# index <- 1
# for (size in seq(SMALLEST, LARGEST, STEP)){
#   CRcFrame <- reset_counter(CRcFrame)
#   CRaFrame <- reset_average(CRaFrame)
#   for (asim in 1:NUMSIMS){
#     CATIsample <- head(CATI[sample(1:nrow(CATI)),],size)
#     RICSsample <- head(RICS[sample(1:nrow(RICS)),],600-size)
#     fullSample <- rbind(CATIsample,RICSsample)
#     CRcFrame <- count_it(fullSample,CRcFrame)
#     fullSample <- calculate_weights_and_design(fullSample)
#     designEffect <-calculate_design_effect(fullSample)
#     indices <- calculate_indices(fullSample)
#     CRdesign[asim] <- designEffect
#     CRICS[asim] <- indices[1]
#     CRICC[asim] <- indices[2]
#     CRICE[asim] <- indices[3]
#   }
#   CRaFrame <- compute_averages(CRaFrame,CRcFrame)
#   CRcFrames[[index]] <- CRcFrame
#   CRaFrames[[index]] <- CRaFrame
#   CRdesignEffects[[index]] <- CRdesign
#   CRtotalICS[[index]] <- CRICS
#   CRtotalICC[[index]] <- CRICC
#   CRtotalICE[[index]] <- CRICE
#   index <- index + 1
# }
# CRdesignAverages <- average_design(CRdesignAverages,CRdesignEffects,index)
# CRdesignQuants <- design_quantile(CRdesignQuants,CRdesignEffects,index)
# CRaverageICS <- average_indices(CRaverageICS,CRtotalICS,index)
# CRaverageICC <- average_indices(CRaverageICC,CRtotalICC,index)
# CRaverageICE <- average_indices(CRaverageICE,CRtotalICE,index)
# CRquantsICS <- index_quantile(CRquantsICS,CRtotalICS,index)
# CRquantsICC <- index_quantile(CRquantsICC,CRtotalICC,index)
# CRquantsICE <- index_quantile(CRquantsICE,CRtotalICE,index)
# 
# CRgraphs <- create_graphs(CRgenderFrame,CRageFrame,CRraceFrame,CRphoneFrame,CRregionFrame,
#                           CRaFrames,index,CRgraphs,NUMSIMS,"CATI","RICS")
# 
# CRdesignGraphA <- ggplot(CRdesignAverages,aes(x=size,y=average))+
#   geom_line(stat="identity")+
#   ggtitle("Average Design Effects Using CATI and RICS Data Sets")+
#   labs(x="Size",y=paste("Average design effect after",NUMSIMS,"simulations",sep=" "),
#        subtitle="(CATI Sample of Size *Size*, RICS Sample of Size *600-Size*)\nError Bars Made Using Mean and Std. Dev.")+
#   geom_text(aes(label=round(average,digits=2)),nudge_y=.12)+
#   geom_errorbar(aes(ymin=average-sd,ymax=average+sd),width=10)
# 
# CRdesignGraphB <- ggplot(CRdesignQuants,aes(x=size,y=med))+
#   geom_line(stat="identity")+
#   ggtitle("Median Design Effects Using CATI and RICS Data Sets")+
#   labs(x="Size",y=paste("Median design effect after",NUMSIMS,"simulations",sep=" "),
#        subtitle="(CATI Sample of Size *Size*, RICS Sample of Size *600-Size*)\nError Bars Made Using Quantiles")+
#   geom_text(aes(label=round(med,digits=2)),nudge_y=.1)+
#   geom_errorbar(aes(ymin=lower,ymax=upper),width=10)
# 
# CRindexGraphA <- ggplot(CRaverageICS,aes(x=size,y=mean))+
#   geom_line(stat="identity",aes(x=size,y=mean,color="ICS"))+
#   geom_text(data=CRaverageICS,aes(label=round(mean,digits=1)),nudge_y=1)+
#   geom_errorbar(data=CRaverageICS,aes(ymin=mean-sd,ymax=mean+sd,color="ICS"),width=10)+
#   geom_line(data=CRaverageICC,aes(x=size,y=mean,color="ICC"),stat="identity")+
#   geom_text(data=CRaverageICC,aes(label=round(mean,digits=1)),nudge_y=1)+
#   geom_errorbar(data=CRaverageICC,aes(ymin=mean-sd,ymax=mean+sd,color="ICC"),width=10)+
#   geom_line(data=CRaverageICE,aes(x=size,y=mean,color="ICE"),stat="identity")+
#   geom_text(data=CRaverageICE,aes(label=round(mean,digits=1)),nudge_y=-1)+
#   geom_errorbar(data=CRaverageICE,aes(ymin=mean-sd,ymax=mean+sd,color="ICE"),width=10)+
#   ggtitle("Average Index Values Using CATI and RICS Data Sets")+
#   labs(x="Size",y=paste("Average index value after",NUMSIMS,"simulations",sep=" "),
#        subtitle="(CATI Sample of Size *Size*, RICS Sample of Size *600-Size*)\nError Bars Made Using Mean and Std. Dev.")+
#   scale_color_discrete(name = "Index")
# 
# 
# CRindexGraphB <- ggplot(CRquantsICS,aes(x=size,y=med))+
#   geom_line(stat="identity",aes(x=size,y=med,color="ICS"))+
#   geom_text(data=CRquantsICS,aes(label=round(med,digits=1)),nudge_y=1)+
#   geom_errorbar(data=CRquantsICS,aes(ymin=lower,ymax=upper,color="ICS"),width=10)+
#   geom_line(data=CRquantsICC,aes(x=size,y=med,color="ICC"),stat="identity")+
#   geom_text(data=CRquantsICC,aes(label=round(med,digits=1)),nudge_y=1)+
#   geom_errorbar(data=CRquantsICC,aes(ymin=lower,ymax=upper,color="ICC"),width=10)+
#   geom_line(data=CRquantsICE,aes(x=size,y=med,color="ICE"),stat="identity")+
#   geom_text(data=CRquantsICE,aes(label=round(med,digits=1)),nudge_y=-1)+
#   geom_errorbar(data=CRquantsICE,aes(ymin=lower,ymax=upper,color="ICE"),width=10)+
#   ggtitle("Median Index Values Using CATI and RICS Data Sets")+
#   labs(x="Size",y=paste("Median index value after",NUMSIMS,"simulations",sep=" "),
#        subtitle="(CATI Sample of Size *Size*, RICS Sample of Size *600-Size*)\nError Bars Made Using Quantiles")+
#   scale_color_discrete(name = "Index")
# 
# # Samples between CATI and AYTM
# CAgraphs <- list()
# CAcFrames <- list()
# CAaFrames <- list()
# CAdesignEffects <- list()
# CAtotalICS <- list()
# CAtotalICC <- list()
# CAtotalICE <- list()
# index <- 1
# for (size in seq(SMALLEST, LARGEST, STEP)){
#   CAcFrame <- reset_counter(CAcFrame)
#   CAaFrame <- reset_average(CAaFrame)
#   for (asim in 1:NUMSIMS){
#     CATIsample <- head(CATI[sample(1:nrow(CATI)),],size)
#     AYTMsample <- head(AYTM[sample(1:nrow(AYTM)),],600-size)
#     AYTMsample$Phone <- NA
#     fullSample <- rbind(CATIsample,AYTMsample)
#     CAcFrame <- count_it(fullSample,CAcFrame)
#     fullSample <- calculate_weights_and_design(fullSample)
#     designEffect <-calculate_design_effect(fullSample)
#     indices <- calculate_indices(fullSample)
#     CAdesign[asim] <- designEffect
#     CAICS[asim] <- indices[1]
#     CAICC[asim] <- indices[2]
#     CAICE[asim] <- indices[3]
#   }
#   CAaFrame <- compute_averages(CAaFrame,CAcFrame)
#   CAcFrames[[index]] <- CAcFrame
#   CAaFrames[[index]] <- CAaFrame
#   CAdesignEffects[[index]] <- CAdesign
#   CAtotalICS[[index]] <- CAICS
#   CAtotalICC[[index]] <- CAICC
#   CAtotalICE[[index]] <- CAICE
#   index <- index + 1
# }
# CAdesignAverages <- average_design(CAdesignAverages,CAdesignEffects,index)
# CAdesignQuants <- design_quantile(CAdesignQuants,CAdesignEffects,index)
# CAaverageICS <- average_indices(CAaverageICS,CAtotalICS,index)
# CAaverageICC <- average_indices(CAaverageICC,CAtotalICC,index)
# CAaverageICE <- average_indices(CAaverageICE,CAtotalICE,index)
# CAquantsICS <- index_quantile(CAquantsICS,CAtotalICS,index)
# CAquantsICC <- index_quantile(CAquantsICC,CAtotalICC,index)
# CAquantsICE <- index_quantile(CAquantsICE,CAtotalICE,index)
# 
# CAgraphs <- create_graphs(CAgenderFrame,CAageFrame,CAraceFrame,CAphoneFrame,CAregionFrame,
#                           CAaFrames,index,CAgraphs,NUMSIMS,"CATI","AYTM")
# 
# CAdesignGraphA <- ggplot(CAdesignAverages,aes(x=size,y=average))+
#   geom_line(stat="identity")+
#   ggtitle("Average Design Effects Using CATI and AYTM Data Sets")+
#   labs(x="Size",y=paste("Average design effect after",NUMSIMS,"simulations",sep=" "),
#        subtitle="(CATI Sample of Size *Size*, AYTM Sample of Size *600-Size*)\nError Bars Made Using Mean and Std. Dev.")+
#   geom_text(aes(label=round(average,digits=2)),nudge_y=.1)+
#   geom_errorbar(aes(ymin=average-sd,ymax=average+sd),width=10)
# 
# CAdesignGraphB <- ggplot(CAdesignQuants,aes(x=size,y=med))+
#   geom_line(stat="identity")+
#   ggtitle("Average Design Effects Using CATI and AYTM Data Sets")+
#   labs(x="Size",y=paste("Average design effect after",NUMSIMS,"simulations",sep=" "),
#        subtitle="(CATI Sample of Size *Size*, RICS Sample of Size *600-Size*)\nError Bars Made Using Quantiles")+
#   geom_text(aes(label=round(med,digits=2)),nudge_y=.1)+
#   geom_errorbar(aes(ymin=lower,ymax=upper),width=10)
# 
# CAindexGraphA <- ggplot(CAaverageICS,aes(x=size,y=mean))+
#   geom_line(stat="identity",aes(x=size,y=mean,color="ICS"))+
#   geom_text(data=CAaverageICS,aes(label=round(mean,digits=1)),nudge_y=1)+
#   geom_errorbar(data=CAaverageICS,aes(ymin=mean-sd,ymax=mean+sd,color="ICS"),width=10)+
#   geom_line(data=CAaverageICC,aes(x=size,y=mean,color="ICC"),stat="identity")+
#   geom_text(data=CAaverageICC,aes(label=round(mean,digits=1)),nudge_y=1)+
#   geom_errorbar(data=CAaverageICC,aes(ymin=mean-sd,ymax=mean+sd,color="ICC"),width=10)+
#   geom_line(data=CAaverageICE,aes(x=size,y=mean,color="ICE"),stat="identity")+
#   geom_text(data=CAaverageICE,aes(label=round(mean,digits=1)),nudge_y=-1)+
#   geom_errorbar(data=CAaverageICE,aes(ymin=mean-sd,ymax=mean+sd,color="ICE"),width=10)+
#   ggtitle("Average Index Values Using CATI and AYTM Data Sets")+
#   labs(x="Size",y=paste("Average index value after",NUMSIMS,"simulations",sep=" "),
#        subtitle="(CATI Sample of Size *Size*, AYTM Sample of Size *600-Size*)\nError Bars Made Using Mean and Std. Dev.")+
#   scale_color_discrete(name = "Index")
# 
# CAindexGraphB <- ggplot(CAquantsICS,aes(x=size,y=med))+
#   geom_line(stat="identity",aes(x=size,y=med,color="ICS"))+
#   geom_text(data=CAquantsICS,aes(label=round(med,digits=1)),nudge_y=1)+
#   geom_errorbar(data=CAquantsICS,aes(ymin=lower,ymax=upper,color="ICS"),width=10)+
#   geom_line(data=CAquantsICC,aes(x=size,y=med,color="ICC"),stat="identity")+
#   geom_text(data=CAquantsICC,aes(label=round(med,digits=1)),nudge_y=1)+
#   geom_errorbar(data=CAquantsICC,aes(ymin=lower,ymax=upper,color="ICC"),width=10)+
#   geom_line(data=CAquantsICE,aes(x=size,y=med,color="ICE"),stat="identity")+
#   geom_text(data=CAquantsICE,aes(label=round(med,digits=1)),nudge_y=-1)+
#   geom_errorbar(data=CAquantsICE,aes(ymin=lower,ymax=upper,color="ICE"),width=10)+
#   ggtitle("Median Index Values Using CATI and AYTM Data Sets")+
#   labs(x="Size",y=paste("Median index value after",NUMSIMS,"simulations",sep=" "),
#        subtitle="(CATI Sample of Size *Size*, AYTM Sample of Size *600-Size*)\nError Bars Made Using Quantiles")+
#   scale_color_discrete(name = "Index")
# 
# # Samples between AYTM and RICS
# ARgraphs <- list()
# ARcFrames <- list()
# ARaFrames <- list()
# ARdesignEffects <- list()
# ARtotalICS <- list()
# ARtotalICC <- list()
# ARtotalICE <- list()
# index <- 1
# for (size in seq(SMALLEST, LARGEST, STEP)){
#   ARcFrame <- reset_counter(ARcFrame)
#   ARaFrame <- reset_average(ARaFrame)
#   for (asim in 1:NUMSIMS){
#     AYTMsample <- head(AYTM[sample(1:nrow(AYTM)),],size)
#     RICSsample <- head(RICS[sample(1:nrow(RICS)),],600-size)
#     AYTMsample$Phone <- NA
#     fullSample <- rbind(AYTMsample,RICSsample)
#     ARcFrame <- count_it(fullSample,ARcFrame)
#     fullSample <- calculate_weights_and_design(fullSample)
#     designEffect <-calculate_design_effect(fullSample)
#     indices <- calculate_indices(fullSample)
#     ARdesign[asim] <- designEffect
#     ARICS[asim] <- indices[1]
#     ARICC[asim] <- indices[2]
#     ARICE[asim] <- indices[3]
#   }
#   ARaFrame <- compute_averages(ARaFrame,ARcFrame)
#   ARcFrames[[index]] <- ARcFrame
#   ARaFrames[[index]] <- ARaFrame
#   ARdesignEffects[[index]] <- ARdesign
#   ARtotalICS[[index]] <- ARICS
#   ARtotalICC[[index]] <- ARICC
#   ARtotalICE[[index]] <- ARICE
#   index <- index + 1
# }
# ARdesignAverages <- average_design(ARdesignAverages,ARdesignEffects,index)
# ARdesignQuants <- design_quantile(ARdesignQuants,ARdesignEffects,index)
# ARaverageICS <- average_indices(ARaverageICS,ARtotalICS,index)
# ARaverageICC <- average_indices(ARaverageICC,ARtotalICC,index)
# ARaverageICE <- average_indices(ARaverageICE,ARtotalICE,index)
# ARquantsICS <- index_quantile(ARquantsICS,ARtotalICS,index)
# ARquantsICC <- index_quantile(ARquantsICC,ARtotalICC,index)
# ARquantsICE <- index_quantile(ARquantsICE,ARtotalICE,index)
# 
# ARgraphs <- create_graphs(ARgenderFrame,ARageFrame,ARraceFrame,ARphoneFrame,ARregionFrame,
#                           ARaFrames,index,ARgraphs,NUMSIMS,"AYTM","RICS")
# 
# ARdesignGraphA <- ggplot(ARdesignAverages,aes(x=size,y=average))+
#   geom_line(stat="identity")+
#   ggtitle("Average Design Effects Using AYTM and RICS Data Sets")+
#   labs(x="Size",y=paste("Average design effect after",NUMSIMS,"simulations",sep=" "),
#        subtitle="(AYTM Sample of Size *Size*, RICS Sample of Size *600-Size*)\nError Bars Made Using Mean and Std. Dev.")+
#   geom_text(aes(label=round(average,digits=2)),nudge_y=.05)+
#   geom_errorbar(aes(ymin=average-sd,ymax=average+sd),width=10)
# 
# ARdesignGraphB <- ggplot(ARdesignQuants,aes(x=size,y=med))+
#   geom_line(stat="identity")+
#   ggtitle("Average Design Effects Using AYTM and RICS Data Sets")+
#   labs(x="Size",y=paste("Average design effect after",NUMSIMS,"simulations",sep=" "),
#        subtitle="(CATI Sample of Size *Size*, RICS Sample of Size *600-Size*)\nError Bars Made Using Quantiles")+
#   geom_text(aes(label=round(med,digits=2)),nudge_y=.05)+
#   geom_errorbar(aes(ymin=lower,ymax=upper),width=10)
# 
# ARindexGraphA <- ggplot(ARaverageICS,aes(x=size,y=mean))+
#   geom_line(stat="identity",aes(x=size,y=mean,color="ICS"))+
#   geom_text(data=ARaverageICS,aes(label=round(mean,digits=1)),nudge_y=1)+
#   geom_errorbar(data=ARaverageICS,aes(ymin=mean-sd,ymax=mean+sd,color="ICS"),width=10)+
#   geom_line(data=ARaverageICC,aes(x=size,y=mean,color="ICC"),stat="identity")+
#   geom_text(data=ARaverageICC,aes(label=round(mean,digits=1)),nudge_y=1)+
#   geom_errorbar(data=ARaverageICC,aes(ymin=mean-sd,ymax=mean+sd,color="ICC"),width=10)+
#   geom_line(data=ARaverageICE,aes(x=size,y=mean,color="ICE"),stat="identity")+
#   geom_text(data=ARaverageICE,aes(label=round(mean,digits=1)),nudge_y=-1)+
#   geom_errorbar(data=ARaverageICE,aes(ymin=mean-sd,ymax=mean+sd,color="ICE"),width=10)+
#   ggtitle("Average Index Values Using AYTM and RICS Data Sets")+
#   labs(x="Size",y=paste("Average index value after",NUMSIMS,"simulations",sep=" "),
#        subtitle="(AYTM Sample of Size *Size*, RICS Sample of Size *600-Size*)\nError Bars Made Using Mean and Std. Dev.")+
#   scale_color_discrete(name = "Index")
# 
# ARindexGraphB <- ggplot(ARquantsICS,aes(x=size,y=med))+
#   geom_line(stat="identity",aes(x=size,y=med,color="ICS"))+
#   geom_text(data=ARquantsICS,aes(label=round(med,digits=1)),nudge_y=1)+
#   geom_errorbar(data=ARquantsICS,aes(ymin=lower,ymax=upper,color="ICS"),width=10)+
#   geom_line(data=ARquantsICC,aes(x=size,y=med,color="ICC"),stat="identity")+
#   geom_text(data=ARquantsICC,aes(label=round(med,digits=1)),nudge_y=1)+
#   geom_errorbar(data=ARquantsICC,aes(ymin=lower,ymax=upper,color="ICC"),width=10)+
#   geom_line(data=ARquantsICE,aes(x=size,y=med,color="ICE"),stat="identity")+
#   geom_text(data=ARquantsICE,aes(label=round(med,digits=1)),nudge_y=-1)+
#   geom_errorbar(data=ARquantsICE,aes(ymin=lower,ymax=upper,color="ICE"),width=10)+
#   ggtitle("Median Index Values Using AYTM and RICS Data Sets")+
#   labs(x="Size",y=paste("Median index value after",NUMSIMS,"simulations",sep=" "),
#        subtitle="(AYTM Sample of Size *Size*, RICS Sample of Size *600-Size*)\nError Bars Made Using Quantiles")+
#   scale_color_discrete(name = "Index")

# CONTOUR PLOT
ACRtopList <- list()
topIndex <- 1
for (size in seq(SMALLEST, LARGEST, STEP)){
  ACRcFrames <- list()
  ACRaFrames <- list()
  ACRdesignEffects <- list()
  ACRtotalICS <- list()
  ACRtotalICC <- list()
  ACRtotalICE <- list()
  index <- 1
  if (size <= (LARGEST-50)){
    for (size2 in seq(SMALLEST, (LARGEST - size), STEP)){
      ACRcFrame <- reset_counter(ACRcFrame)
      ACRaFrame <- reset_average(ACRaFrame)
      for (asim in 1:NUMSIMS){
        CATIsample <- head(CATI[sample(1:nrow(CATI)),],size)
        RICSsample <- head(RICS[sample(1:nrow(RICS)),],size2)
        AYTMsample <- head(AYTM[sample(1:nrow(AYTM)),],600-size-size2)
        AYTMsample$Phone <- NA
        fullSample <- rbind(CATIsample,RICSsample,AYTMsample)
        ACRcFrame <- count_it(fullSample,ACRcFrame)
        fullSample <- calculate_weights_and_design(fullSample)
        designEffect <-calculate_design_effect(fullSample)
        indices <- calculate_indices(fullSample)
        ACRdesign[asim] <- designEffect
        ACRICS[asim] <- indices[1]
        ACRICC[asim] <- indices[2]
        ACRICE[asim] <- indices[3]
      }
      ACRaFrame <- compute_averages(ACRaFrame,ACRcFrame)
      ACRcFrames[[index]] <- ACRcFrame
      ACRaFrames[[index]] <- ACRaFrame
      ACRdesignEffects[[index]] <- ACRdesign
      ACRtotalICS[[index]] <- ACRICS
      ACRtotalICC[[index]] <- ACRICC
      ACRtotalICE[[index]] <- ACRICE
      index <- index + 1
    }
    ACRtopList[[topIndex]] <- list("cFrame"=ACRcFrames,
                                   "aFrame"=ACRaFrames,
                                   "design"=ACRdesignEffects,
                                   "ICS"=ACRtotalICS,
                                   "ICC"=ACRtotalICC,
                                   "ICE"=ACRtotalICE)
    topIndex <- topIndex + 1
  }
}
ACRdesignAverages <- average_design_triple(ACRtopList,topIndex,SMALLEST,STEP)
ACRdesignQuants <- design_quantile_triple(ACRtopList,topIndex,SMALLEST,STEP)
ACRaverageIndices <- average_indices_triple(ACRtopList,topIndex,SMALLEST,STEP)
ACRquantsIndices <- index_quantile_triple(ACRtopList,topIndex,SMALLEST,STEP)

ACRdesignGraphA <- ggplot(ACRdesignAverages,aes(x=size,y=size2,z=average))+
  geom_contour()+
  ggtitle(paste("Average Design Effects Using CATI, RICS, and AYTM Data Sets\nData After",NUMSIMS,"Simulations",sep=" "))+
  labs(x="Size of CATI sample (size1)",y="Size of RICS sample (size2)",
       subtitle="(ATYM Sample of Size 600-size1-size2, Full Sample of Size 600)")+
  geom_text(aes(label=round(average,digits=2)))

ACRdesignGraphB <- ggplot(ACRdesignQuants,aes(x=size,y=size2,z=med))+
  geom_contour()+
  ggtitle(paste("Median Design Effects Using CATI, RICS, and AYTM Data Sets\nData After",NUMSIMS,"Simulations",sep=" "))+
  labs(x="Size of CATI sample (size1)",y="Size of RICS sample (size2)",
       subtitle="(ATYM Sample of Size 600-size1-size2, Full Sample of Size 600)")+
  geom_text(aes(label=round(med,digits=2)))

ACRindexGraphA <- ggplot(ACRaverageIndices,aes(x=size,y=size2,z=average,color=factor(index)))+
  geom_contour()+
  ggtitle(paste("Average Index Values Using CATI, RICS, and AYTM Data Sets\nData After",NUMSIMS,"Simulations",sep=" "))+
  labs(x="Size of CATI sample (size1)",y="Size of RICS sample (size2)",
       subtitle="(ATYM Sample of Size 600-size1-size2, Full Sample of Size 600)")+
  scale_color_discrete(name = "Index")+
  geom_text_repel(aes(label=round(average,digits=2)))

ACRindexGraphB <- ggplot(ACRquantsIndices,aes(x=size,y=size2,z=med,color=factor(index)))+
  geom_contour()+
  ggtitle(paste("Median Index Values Using CATI, RICS, and AYTM Data Sets\nData After",NUMSIMS,"Simulations",sep=" "))+
  labs(x="Size of CATI sample (size1)",y="Size of RICS sample (size2)",
       subtitle="(ATYM Sample of Size 600-size1-size2, Full Sample of Size 600)")+
  scale_color_discrete(name = "Index")+
  geom_text_repel(aes(label=round(med,digits=2)))

# PLOT COMMANDS
# UNCOMMENT AND RUN A SECTION OF CODE

# DEMOGRPAHIC PLOTS
# CRgraphs[[1]] + CAgraphs[[1]] + ARgraphs[[1]] + plot_layout(nrow=3)
# CRgraphs[[2]] + CAgraphs[[2]] + ARgraphs[[2]] + plot_layout(nrow=3)
# CRgraphs[[3]] + CAgraphs[[3]] + ARgraphs[[3]] + plot_layout(nrow=3)
# CRgraphs[[4]] + CAgraphs[[4]] + ARgraphs[[4]] + plot_layout(nrow=3)
# CRgraphs[[5]] + CAgraphs[[5]] + ARgraphs[[5]] + plot_layout(nrow=3)

# DESIGN EFFECT PLOTS
# (CRdesignGraphA + CAdesignGraphA + ARdesignGraphA + plot_layout(nrow=3)) |
#   (CRdesignGraphB + CAdesignGraphB + ARdesignGraphB + plot_layout(nrow=3))
 
# INDEX PLOTS
# (CRindexGraphA + CAindexGraphA + ARindexGraphA + plot_layout(nrow=3)) |
#   (CRindexGraphB + CAindexGraphB + ARindexGraphB + plot_layout(nrow=3))

# CONTOUR PLOTS
# (ACRdesignGraphA + ACRdesignGraphB + plot_layout(nrow=2)) |
#   (ACRindexGraphA + ACRindexGraphB + plot_layout(nrow=2))