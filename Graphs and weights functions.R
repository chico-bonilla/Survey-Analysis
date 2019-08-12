count_it <- function(sample, cFrame){
  cFrame$male <- sum(length(which(sample$GENDER_RECODE == 1)), cFrame$male)
  cFrame$female <- sum(length(which(sample$GENDER_RECODE == 2)), cFrame$female)
  cFrame$genderU <- sum(length(which(sample$GENDER_RECODE == 3)), cFrame$genderU)
  
  cFrame$age1 <- sum(length(which(sample$AGE_RECODE == 1)), cFrame$age1)
  cFrame$age2 <- sum(length(which(sample$AGE_RECODE == 2)), cFrame$age2)
  cFrame$age3 <- sum(length(which(sample$AGE_RECODE == 3)), cFrame$age3)
  cFrame$age4 <- sum(length(which(sample$AGE_RECODE == 4)), cFrame$age4)
  cFrame$ageU <- sum(length(which(sample$AGE_RECODE == 5)), cFrame$ageU)
  
  cFrame$white <- sum(length(which(sample$RACE_RECODE == 1)), cFrame$white)
  cFrame$black <- sum(length(which(sample$RACE_RECODE == 2)), cFrame$black)
  cFrame$otherR <- sum(length(which(sample$RACE_RECODE == 3)), cFrame$otherR)
  cFrame$raceU <- sum(length(which(sample$RACE_RECODE == 4)), cFrame$raceU)
  
  cFrame$cell <- sum(length(which(sample$Phone == 2)), cFrame$cell)
  cFrame$land <- sum(length(which(sample$Phone == 1)), cFrame$land)
  
  cFrame$region1 <- sum(length(which(sample$REGION_PY == 1)), cFrame$region1)
  cFrame$region2 <- sum(length(which(sample$REGION_PY == 2)), cFrame$region2)
  cFrame$region3 <- sum(length(which(sample$REGION_PY == 3)), cFrame$region3)
  cFrame$region4 <- sum(length(which(sample$REGION_PY == 4)), cFrame$region4)
  cFrame$region5 <- sum(length(which(sample$REGION_PY == 5)), cFrame$region5)
  cFrame$region6 <- sum(length(which(sample$REGION_PY == 6)), cFrame$region6)
  cFrame$regionU <- sum(length(which(sample$REGION_PY == 7)), cFrame$regionU)
  
  return(cFrame)
}

compute_averages <- function(aFrame,cFrame){
  aFrame$average[1] <- cFrame$male / NUMSIMS
  aFrame$average[2] <- cFrame$female / NUMSIMS
  
  aFrame$average[3] <- cFrame$age1 / NUMSIMS
  aFrame$average[4] <- cFrame$age2 / NUMSIMS
  aFrame$average[5] <- cFrame$age3 / NUMSIMS
  aFrame$average[6] <- cFrame$age4 / NUMSIMS
  
  aFrame$average[7] <- cFrame$white / NUMSIMS
  aFrame$average[8] <- cFrame$black / NUMSIMS
  aFrame$average[9] <- cFrame$otherR / NUMSIMS
  
  aFrame$average[10] <- cFrame$cell / NUMSIMS
  aFrame$average[11] <- cFrame$land / NUMSIMS
  
  aFrame$average[12] <- cFrame$region1 / NUMSIMS
  aFrame$average[13] <- cFrame$region2 / NUMSIMS
  aFrame$average[14] <- cFrame$region3 / NUMSIMS
  aFrame$average[15] <- cFrame$region4 / NUMSIMS
  aFrame$average[16] <- cFrame$region5 / NUMSIMS
  aFrame$average[17] <- cFrame$region6 / NUMSIMS
  
  return(aFrame)
}

reset_counter <- function(cFrame){
  cFrame$male <- 0
  cFrame$female <- 0
  
  cFrame$age1 <- 0
  cFrame$age2 <- 0
  cFrame$age3 <- 0
  cFrame$age4 <- 0
  
  cFrame$white <- 0
  cFrame$black <- 0
  cFrame$otherR <- 0
  
  cFrame$cell <- 0
  cFrame$land <- 0
  
  cFrame$region1 <- 0
  cFrame$region2 <- 0
  cFrame$region3 <- 0
  cFrame$region4 <- 0
  cFrame$region5 <- 0
  cFrame$region6 <- 0
  
  return(cFrame)
}

reset_average <- function(aFrame){
  aFrame$average[1] <- 0
  aFrame$average[2] <- 0
  
  aFrame$average[3] <- 0
  aFrame$average[4] <- 0
  aFrame$average[5] <- 0
  aFrame$average[6] <- 0
  
  aFrame$average[7] <- 0
  aFrame$average[8] <- 0
  aFrame$average[9] <- 0
  
  aFrame$average[10] <- 0
  aFrame$average[11] <- 0
  
  aFrame$average[12] <- 0
  aFrame$average[13] <- 0
  aFrame$average[14] <- 0
  aFrame$average[15] <- 0
  aFrame$average[16] <- 0
  aFrame$average[17] <- 0
  
  return(aFrame)
}

create_graphs <- function(genderFrame,ageFrame,raceFrame,phoneFrame,regionFrame,
                          aFrames,index,graphs,NUMSIMS,sample1,sample2){
  # Combining the averages into five separate frames based on demographic
  for (iframe in 1:(index-1)){
    genderFrame$male[iframe] <- (aFrames[[iframe]]$average[1] / 600) * 100
    genderFrame$female[iframe] <- (aFrames[[iframe]]$average[2] / 600) * 100
    
    ageFrame$age1[iframe] <- (aFrames[[iframe]]$average[3] / 600) * 100
    ageFrame$age2[iframe] <- (aFrames[[iframe]]$average[4] / 600) * 100
    ageFrame$age3[iframe] <- (aFrames[[iframe]]$average[5] / 600) * 100
    ageFrame$age4[iframe] <- (aFrames[[iframe]]$average[6] / 600) * 100
    
    raceFrame$white[iframe] <- (aFrames[[iframe]]$average[7] / 600) * 100
    raceFrame$black[iframe] <- (aFrames[[iframe]]$average[8] / 600) * 100
    raceFrame$otherR[iframe] <- (aFrames[[iframe]]$average[9] / 600) * 100
    
    phoneFrame$cell[iframe] <- (aFrames[[iframe]]$average[10] / 600) * 100
    phoneFrame$land[iframe] <- (aFrames[[iframe]]$average[11] / 600) * 100
    
    regionFrame$region1[iframe] <- (aFrames[[iframe]]$average[12] / 600) * 100
    regionFrame$region2[iframe] <- (aFrames[[iframe]]$average[13] / 600) * 100
    regionFrame$region3[iframe] <- (aFrames[[iframe]]$average[14] / 600) * 100
    regionFrame$region4[iframe] <- (aFrames[[iframe]]$average[15] / 600) * 100
    regionFrame$region5[iframe] <- (aFrames[[iframe]]$average[16] / 600) * 100
    regionFrame$region6[iframe] <- (aFrames[[iframe]]$average[17] / 600) * 100
  }
  
  # melt is a function that changes wide frames to long frames. This is useful for graphing with ggplot
  gfl <- melt(genderFrame,id.vars="size",variable.name="sex",value.name="percentage")
  afl <- melt(ageFrame,id.vars="size",variable.name="age_group",value.name="percentage")
  rafl <- melt(raceFrame,id.vars="size",variable.name="race",value.name="percentage")
  pfl <- melt(phoneFrame,id.vars="size",variable.name="phone_type",value.name="percentage")
  refl <- melt(regionFrame,id.vars="size",variable.name="region",value.name="percentage")
  
  # Creating and storing all the graphs
  graphs[[1]] <- ggplot(gfl,aes(x=size,y=percentage,color=factor(sex)))+
    geom_line(stat="identity")+
    ggtitle(paste("Average percentage of genders in random samples\n(",sample1," and ",sample2,")",sep=""))+
    labs(x="Size",y=paste("Average percentage after",NUMSIMS,"simulations",sep=" "))+
    scale_color_discrete(name="Gender",labels=c("Male","Female"))+
    geom_hline(yintercept=48.63,linetype="dashed",color="red")+
    geom_hline(yintercept=51.36,linetype="dashed",color="deepskyblue")
  
  graphs[[2]] <- ggplot(afl,aes(x=size,y=percentage,color=factor(age_group)))+
    geom_line(stat="identity")+
    ggtitle(paste("Average percentage of ages in random samples\n(",sample1," and ",sample2,")",sep=""))+
    labs(x="Size",y=paste("Average percentage after",NUMSIMS,"simulations",sep=" "))+
    scale_color_discrete(name="Age Group",labels=c("Group 1","Group 2","Group 3","Group 4"))+
    geom_hline(yintercept=22.48,linetype="dashed",color="red")+
    geom_hline(yintercept=27.15,linetype="dashed",color="green")+
    geom_hline(yintercept=34.81,linetype="dashed",color="deepskyblue")+
    geom_hline(yintercept=15.54,linetype="dashed",color="purple")
  
  graphs[[3]] <- ggplot(rafl,aes(x=size,y=percentage,color=factor(race)))+
    geom_line(stat="identity")+
    ggtitle(paste("Average percentage of races in random samples\n(",sample1," and ",sample2,")",sep=""))+
    labs(x="Size",y=paste("Average percentage after",NUMSIMS,"simulations",sep=" "))+
    scale_color_discrete(name="Race",labels=c("White","Black","Other"))+
    geom_hline(yintercept=76.22,linetype="dashed",color="red")+
    geom_hline(yintercept=18.54,linetype="dashed",color="green")+
    geom_hline(yintercept=5.32,linetype="dashed",color="blue")
  
  graphs[[4]] <- ggplot(refl,aes(x=size,y=percentage,color=factor(region)))+
    geom_line(stat="identity")+
    ggtitle(paste("Average percentage of regions in random samples\n(",sample1," and ",sample2,")",sep=""))+
    labs(x="Size",y=paste("Average percentage after",NUMSIMS,"simulations",sep=" "))+
    scale_color_discrete(name="Region",labels=c("Region 1","Region 2","Region 3","Region 4","Region 5","Region 6"))+
    geom_hline(yintercept=11,linetype="dashed",color="red")+
    geom_hline(yintercept=8.16,linetype="dashed",color="khaki4")+
    geom_hline(yintercept=6.83,linetype="dashed",color="green")+
    geom_hline(yintercept=20.16,linetype="dashed",color="deepskyblue")+
    geom_hline(yintercept=30.66,linetype="dashed",color="blue")+
    geom_hline(yintercept=23.66,linetype="dashed",color="purple")
  
  graphs[[5]] <- ggplot(pfl,aes(x=size,y=percentage,color=factor(phone_type)))+
    geom_line(stat="identity")+
    ggtitle(paste("Average percentage of cellphones and landlines in random samples\n(",sample1," and ",sample2,")",sep=""))+
    labs(x="Size",y=paste("Average percentage after",NUMSIMS,"simulations",sep=" "))+
    scale_color_discrete(name="Phone Type",labels=c("Cellphone","Landline"))
  
  return(graphs)
}

calculate_weights_and_design <- function(sample){
  # Setting constants
  GENDER <- c(0.4863377, 0.5136623)
  AGE <- c(0.2248895, 0.2715101, 0.3481435, 0.1554569)
  RACE <- c(0.7622227, 0.1845024, 0.0532749)
  REGION <- c(0.11, 0.0816667, 0.0683333, 0.2016667, 0.3066667, 0.2366667)
  ITERATIONS <- 26
  SAMPLE_SIZE <- 600
  
  cFrame <- data.frame(rbind(total = c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)))
  names(cFrame) <- c("male","female","genderU","age1","age2","age3","age4","ageU","white","black","otherR","raceU","cell",
                     "land","region1","region2","region3","region4","region5","region6","regionU")
  cFrame <- count_it(sample,cFrame)
  
  # This is a 4-D array that holds all the current frequencies based on the current sample
  freqs <- list(
    Male=list(
      Age1=list(
        White=c(rep(0,7)),Black=c(rep(0,7)),OtherR=c(rep(0,7)),RaceU=c(rep(0,7))),
      Age2=list(
        White=c(rep(0,7)),Black=c(rep(0,7)),OtherR=c(rep(0,7)),RaceU=c(rep(0,7))),
      Age3=list(
        White=c(rep(0,7)),Black=c(rep(0,7)),OtherR=c(rep(0,7)),RaceU=c(rep(0,7))),
      Age4=list(
        White=c(rep(0,7)),Black=c(rep(0,7)),OtherR=c(rep(0,7)),RaceU=c(rep(0,7))),
      AgeU=list(
        White=c(rep(0,7)),Black=c(rep(0,7)),OtherR=c(rep(0,7)),RaceU=c(rep(0,7)))),
    Female=list(
      Age1=list(
        White=c(rep(0,7)),Black=c(rep(0,7)),OtherR=c(rep(0,7)),RaceU=c(rep(0,7))),
      Age2=list(
        White=c(rep(0,7)),Black=c(rep(0,7)),OtherR=c(rep(0,7)),RaceU=c(rep(0,7))),
      Age3=list(
        White=c(rep(0,7)),Black=c(rep(0,7)),OtherR=c(rep(0,7)),RaceU=c(rep(0,7))),
      Age4=list(
        White=c(rep(0,7)),Black=c(rep(0,7)),OtherR=c(rep(0,7)),RaceU=c(rep(0,7))),
      AgeU=list(
        White=c(rep(0,7)),Black=c(rep(0,7)),OtherR=c(rep(0,7)),RaceU=c(rep(0,7)))),
    GenderU=list(
      Age1=list(
        White=c(rep(0,7)),Black=c(rep(0,7)),OtherR=c(rep(0,7)),RaceU=c(rep(0,7))),
      Age2=list(
        White=c(rep(0,7)),Black=c(rep(0,7)),OtherR=c(rep(0,7)),RaceU=c(rep(0,7))),
      Age3=list(
        White=c(rep(0,7)),Black=c(rep(0,7)),OtherR=c(rep(0,7)),RaceU=c(rep(0,7))),
      Age4=list(
        White=c(rep(0,7)),Black=c(rep(0,7)),OtherR=c(rep(0,7)),RaceU=c(rep(0,7))),
      AgeU=list(
        White=c(rep(0,7)),Black=c(rep(0,7)),OtherR=c(rep(0,7)),RaceU=c(rep(0,7)))))
  
  # This loop simply populates the 4-D array
  for (aline in 1:SAMPLE_SIZE){
    freqs[[sample$GENDER_RECODE[aline]]][[sample$AGE_RECODE[aline]]][[sample$RACE_RECODE[aline]]][sample$REGION_PY[aline]] <- 
      freqs[[sample$GENDER_RECODE[aline]]][[sample$AGE_RECODE[aline]]][[sample$RACE_RECODE[aline]]][sample$REGION_PY[aline]] + 1
  }
  
  # Creating demographic frequencies
  genderf <- c()
  for (i in 1:length(GENDER)){
    genderf[i] <- (GENDER[i] * (1-cFrame$genderU/SAMPLE_SIZE)*SAMPLE_SIZE)
  }
  genderf <- c(genderf, cFrame$genderU)
  
  agef <- c()
  for (i in 1:length(AGE)){
    agef[i] <- (AGE[i] * (1-cFrame$ageU/SAMPLE_SIZE)*SAMPLE_SIZE)
  }
  agef <- c(agef, cFrame$ageU)
  
  racef <- c()
  for (i in 1:length(RACE)){
    racef[i] <- (RACE[i] * (1-cFrame$raceU/SAMPLE_SIZE)*SAMPLE_SIZE)
  }
  racef <- c(racef, cFrame$raceU)
  
  regionf <- c()
  for (i in 1:length(REGION)){
    regionf[i] <- (REGION[i] * (1-cFrame$regionU/SAMPLE_SIZE)*SAMPLE_SIZE)
  }
  regionf <- c(regionf, cFrame$regionU)
  mod_freqs <- freqs
  
  # IPF algorithm
  for (z in 1:ITERATIONS){
    gender_count <- c(0,0,0)
    gender_mult <- c(0,0,0)
    age_count <- c(0,0,0,0,0)
    age_mult <- c(0,0,0,0,0)
    race_count <- c(0,0,0,0)
    race_mult <- c(0,0,0,0)
    region_count <- c(0,0,0,0,0,0,0)
    region_mult <- c(0,0,0,0,0,0,0)
    
    #Gender Count
    for (i in 1:length(mod_freqs)){
      for (j in 1:length(mod_freqs[[i]])){
        for (k in 1:length(mod_freqs[[i]][[j]])){
          for (m in 1:length(mod_freqs[[i]][[j]][[k]])){
            gender_count[i] <- gender_count[i] + mod_freqs[[i]][[j]][[k]][m]
          }
        }
      }
    }
    #Gender Multiplier
    for (i in 1:length(genderf)){
      if (gender_count[i] == 0){
        gender_mult[i] <- 0
      }
      else {
        gender_mult[i] <- genderf[i] / gender_count[i]
      }
    }
    for (i in 1:length(mod_freqs)){
      for (j in 1:length(mod_freqs[[i]])){
        for (k in 1:length(mod_freqs[[i]][[j]])){
          for (m in 1:length(mod_freqs[[i]][[j]][[k]])){
            mod_freqs[[i]][[j]][[k]][m] <- mod_freqs[[i]][[j]][[k]][m] * gender_mult[i]
          }
        }
      }
    }
    
    #Age Count
    for (i in 1:length(mod_freqs)){
      for (j in 1:length(mod_freqs[[i]])){
        for (k in 1:length(mod_freqs[[i]][[j]])){
          for (m in 1:length(mod_freqs[[i]][[j]][[k]])){
            age_count[j] <- age_count[j] + mod_freqs[[i]][[j]][[k]][m]
          }
        }
      }
    }
    #Age Multiplier
    for (i in 1:length(agef)){
      if (age_count[i] == 0){
        age_mult[i] <- 0
      }
      else {
        age_mult[i] <- agef[i] / age_count[i]
      }
    }
    for (i in 1:length(mod_freqs)){
      for (j in 1:length(mod_freqs[[i]])){
        for (k in 1:length(mod_freqs[[i]][[j]])){
          for (m in 1:length(mod_freqs[[i]][[j]][[k]])){
            mod_freqs[[i]][[j]][[k]][m] <- mod_freqs[[i]][[j]][[k]][m] * age_mult[j]
          }
        }
      }
    }
    
    #Race Count
    for (i in 1:length(mod_freqs)){
      for (j in 1:length(mod_freqs[[i]])){
        for (k in 1:length(mod_freqs[[i]][[j]])){
          for (m in 1:length(mod_freqs[[i]][[j]][[k]])){
            race_count[k] <- race_count[k] + mod_freqs[[i]][[j]][[k]][m]
          }
        }
      }
    }
    #Race Multiplier
    for (i in 1:length(racef)){
      if (race_count[i] == 0){
        race_mult[i] <- 0
      }
      else {
        race_mult[i] <- racef[i] / race_count[i]
      }
    }
    for (i in 1:length(mod_freqs)){
      for (j in 1:length(mod_freqs[[i]])){
        for (k in 1:length(mod_freqs[[i]][[j]])){
          for (m in 1:length(mod_freqs[[i]][[j]][[k]])){
            mod_freqs[[i]][[j]][[k]][m] <- mod_freqs[[i]][[j]][[k]][m] * race_mult[k]
          }
        }
      }
    }
    
    #Region Count
    for (i in 1:length(mod_freqs)){
      for (j in 1:length(mod_freqs[[i]])){
        for (k in 1:length(mod_freqs[[i]][[j]])){
          for (m in 1:length(mod_freqs[[i]][[j]][[k]])){
            region_count[m] <- region_count[m] + mod_freqs[[i]][[j]][[k]][m]
          }
        }
      }
    }
    #Region Multiplier
    for (i in 1:length(regionf)){
      if (region_count[i] == 0){
        region_mult[i] <- 0
      }
      else {
        region_mult[i] <- regionf[i] / region_count[i]
      }
    }
    for (i in 1:length(mod_freqs)){
      for (j in 1:length(mod_freqs[[i]])){
        for (k in 1:length(mod_freqs[[i]][[j]])){
          for (m in 1:length(mod_freqs[[i]][[j]][[k]])){
            mod_freqs[[i]][[j]][[k]][m] <- mod_freqs[[i]][[j]][[k]][m] * region_mult[m]
          }
        }
      }
    }
  }
  
  # Adding weights to sample as a new column
  for (i in 1:SAMPLE_SIZE){
    cur_freq <- freqs[[sample$GENDER_RECODE[i]]][[sample$AGE_RECODE[i]]][[sample$RACE_RECODE[i]]][sample$REGION_PY[i]]
    mod_freq <- mod_freqs[[sample$GENDER_RECODE[i]]][[sample$AGE_RECODE[i]]][[sample$RACE_RECODE[i]]][sample$REGION_PY[i]]
    if (cur_freq > 0){
      sample$RAKE[i] <- mod_freq / cur_freq
    }
    else {
      sample$RAKE[i] <- 1.0
    }
  }
  return(sample)
}

calculate_design_effect <- function(sample){
  numerator <- 0
  denominator <- 0
  for (i in 1:600){
    numerator <- numerator + sample$RAKE[i]
    denominator <- denominator + (sample$RAKE[i] ^ 2)
  }
  numerator <- numerator ^ 2
  effSampleSize <- numerator / denominator
  design <- 600 / effSampleSize
  return(design)
}

average_design <- function(designAverages,designEffects,index){
  for (i in 1:(index-1)){
    designAverages$average[i] <- mean(designEffects[[i]])
    designAverages$sd[i] <- sd(designEffects[[i]])
  }
  
  return (designAverages)
}

average_design_triple <- function(topList,index,SMALLEST,STEP){
  size <- SMALLEST
  size2 <- SMALLEST
  runningIndex <- 1
  sizeList <- list()
  size2List <- list()
  averageList <- list()
  sdList <- list()
  for (i in 1:(index-1)){
    for (j in 1:length(topList[[i]]$design)){
      sizeList[runningIndex] <- size
      size2List[runningIndex] <- size2
      averageList[runningIndex] <- mean(topList[[i]]$design[[j]])
      sdList[runningIndex] <- sd(topList[[i]]$design[[j]])
      runningIndex <- runningIndex + 1
      size2 <- size2 + STEP
    }
    size <- size + STEP
    size2 <- SMALLEST
  }
  designAverages <- data.frame(cbind(size = sizeList,
                                     size2 = size2List,
                                     average = averageList))
  designAverages <- as.data.frame(lapply(designAverages,unlist))
  
  return (designAverages)
}

design_quantile <- function(designQuants,designEffects,index){
  for (i in 1:(index-1)){
    designQuants$lower[i] <- quantile(designEffects[[i]],c(.25))
    designQuants$med[i] <- quantile(designEffects[[i]],c(.5))
    designQuants$upper[i] <- quantile(designEffects[[i]],c(.75))
  }
  
  return (designQuants)
}

design_quantile_triple <- function(topList,index,SMALLEST,STEP){
  size <- SMALLEST
  size2 <- SMALLEST
  runningIndex <- 1
  sizeList <- list()
  size2List <- list()
  lowerList <- list()
  medList <- list()
  upperList <- list()
  for (i in 1:(index-1)){
    for (j in 1:length(topList[[i]]$design)){
      sizeList[runningIndex] <- size
      size2List[runningIndex] <- size2
      lowerList[runningIndex] <- quantile(topList[[i]]$design[[j]],c(.25))
      medList[runningIndex] <- quantile(topList[[i]]$design[[j]],c(.5))
      upperList[runningIndex] <- quantile(topList[[i]]$design[[j]],c(.75))
      runningIndex <- runningIndex + 1
      size2 <- size2 + STEP
    }
    size <- size + STEP
    size2 <- SMALLEST
  }
  designQuants <- data.frame(cbind(size = sizeList,
                                   size2 = size2List,
                                   med = medList))
  designQuants <- as.data.frame(lapply(designQuants,unlist))
  
  return (designQuants)
}

calculate_indices <- function(sample){
  current_N1 <- 0
  pos_res1 <- 0
  neg_res1 <- 0
  
  current_N2 <- 0
  pos_res2 <- 0
  neg_res2 <- 0
  
  current_N3 <- 0
  pos_res3 <- 0
  neg_res3 <- 0
  
  current_N4 <- 0
  pos_res4 <- 0
  neg_res4 <- 0
  
  current_N5 <- 0
  pos_res5 <- 0
  neg_res5 <- 0
  
  for (i in 1:600){
    # YEARAGO
    if(sample$YEARAGO[i] != 4){
      if(sample$YEARAGO[i] == 1){
        pos_res1 <- pos_res1 + sample$RAKE[i]
      }
      else if(sample$YEARAGO[i] == 3){
        neg_res1 <- neg_res1 + sample$RAKE[i]
      }
      current_N1 <- current_N1 + sample$RAKE[i]
    }
    
    # NEXTYEAR
    if(sample$NEXTYEAR[i] != 4){
      if(sample$NEXTYEAR[i] == 1){
        pos_res2 <- pos_res2 + sample$RAKE[i]
      }
      else if(sample$NEXTYEAR[i] == 3){
        neg_res2 <- neg_res2 + sample$RAKE[i]
      }
      current_N2 <- current_N2 + sample$RAKE[i]
    }
    
    # BUSNXTYR
    if(sample$BUSNXTYR[i] != 4){
      if(sample$BUSNXTYR[i] == 1){
        pos_res3 <- pos_res3 + sample$RAKE[i]
      }
      else if(sample$BUSNXTYR[i] == 3){
        neg_res3 <- neg_res3 + sample$RAKE[i]
      }
      current_N3 <- current_N3 + sample$RAKE[i]
    }
    
    # FIVEYEAR
    if(sample$FIVEYEAR[i] != 4){
      if(sample$FIVEYEAR[i] == 1){
        pos_res4 <- pos_res4 + sample$RAKE[i]
      }
      else if(sample$FIVEYEAR[i] == 3){
        neg_res4 <- neg_res4 + sample$RAKE[i]
      }
      current_N4 <- current_N4 + sample$RAKE[i]
    }
    
    # BIGITEM
    if(sample$BIGITEM[i] != 4){
      if(sample$BIGITEM[i] == 1){
        pos_res5 <- pos_res5 + sample$RAKE[i]
      }
      else if(sample$BIGITEM[i] == 3){
        neg_res5 <- neg_res5 + sample$RAKE[i]
      }
      current_N5 <- current_N5 + sample$RAKE[i]
    }
  }
  pos_res1 <- pos_res1 / current_N1
  neg_res1 <- neg_res1 / current_N1
  X_1 <- 100*(pos_res1 - neg_res1) + 100
  
  pos_res2 <- pos_res2 / current_N2
  neg_res2 <- neg_res2 / current_N2
  X_2 <- 100*(pos_res2 - neg_res2) + 100
  
  pos_res3 <- pos_res3 / current_N3
  neg_res3 <- neg_res3 / current_N3
  X_3 <- 100*(pos_res3 - neg_res3) + 100
  
  pos_res4 <- pos_res4 / current_N4
  neg_res4 <- neg_res4 / current_N4
  X_4 <- 100*(pos_res4 - neg_res4) + 100
  
  pos_res5 <- pos_res5 / current_N5
  neg_res5 <- neg_res5 / current_N5
  X_5 <- 100*(pos_res5 - neg_res5) + 100
  
  ICS <- ((X_1 + X_2 + X_3 + X_4 + X_5) / 6.7558) + 2.0
  ICC <- ((X_1 + X_5) / 2.6424) + 2.0
  ICE <- ((X_2 + X_3 + X_4) / 4.1134) + 2.0
  
  indices <- c(ICS,ICC,ICE)
  return(indices)
}

average_indices <- function(averageIndices,indices,index){
  for(i in 1:(index-1)){
    averageIndices$mean[i] <- mean(indices[[i]])
    averageIndices$sd[i] <- sd(indices[[i]])
  }
  return(averageIndices)
}

average_indices_triple <- function(topList,index,SMALLEST,STEP){
  size <- SMALLEST
  size2 <- SMALLEST
  runningIndex <- 1
  sizeList <- list()
  size2List <- list()
  ICSaverageList <- list()
  ICCaverageList <- list()
  ICEaverageList <- list()
  ICSsdList <- list()
  ICCsdList <- list()
  ICEsdList <- list()
  for (i in 1:(index-1)){
    for (j in 1:length(topList[[i]]$ICS)){
      sizeList[runningIndex] <- size
      size2List[runningIndex] <- size2
      
      ICSaverageList[runningIndex] <- mean(topList[[i]]$ICS[[j]])
      
      ICCaverageList[runningIndex] <- mean(topList[[i]]$ICC[[j]])
      
      ICEaverageList[runningIndex] <- mean(topList[[i]]$ICE[[j]])
      
      runningIndex <- runningIndex + 1
      size2 <- size2 + STEP
    }
    size <- size + STEP
    size2 <- SMALLEST
  }
  averageIndices <- data.frame(cbind(size = sizeList,
                                     size2 = size2List,
                                     ICS = ICSaverageList,
                                     ICC = ICCaverageList,
                                     ICE = ICEaverageList))
  averageIndex <- as.data.frame(lapply(averageIndices,unlist))
  averageIndices <- melt(averageIndex,id.vars=c("size","size2"),variable.name="index",value.name="average")
  
  return (averageIndices)
}

index_quantile <- function(indexQuants,indices,index){
  for(i in 1:(index-1)){
    indexQuants$lower[i] <- quantile(indices[[i]],c(.25))
    indexQuants$med[i] <- quantile(indices[[i]],c(.5))
    indexQuants$upper[i] <- quantile(indices[[i]],c(.75))
  }
  return(indexQuants)
}

index_quantile_triple <- function(topList,index,SMALLEST,STEP){
  size <- SMALLEST
  size2 <- SMALLEST
  runningIndex <- 1
  sizeList <- list()
  size2List <- list()
  ICSlowerList <- list()
  ICSmedList <- list()
  ICSupperList <- list()
  ICClowerList <- list()
  ICCmedList <- list()
  ICCupperList <- list()
  ICElowerList <- list()
  ICEmedList <- list()
  ICEupperList <- list()
  for (i in 1:(index-1)){
    for (j in 1:length(topList[[i]]$design)){
      sizeList[runningIndex] <- size
      size2List[runningIndex] <- size2
      
      ICSmedList[runningIndex] <- quantile(topList[[i]]$ICS[[j]],c(.5))
      
      ICCmedList[runningIndex] <- quantile(topList[[i]]$ICC[[j]],c(.5))
      
      ICEmedList[runningIndex] <- quantile(topList[[i]]$ICE[[j]],c(.5))
      
      runningIndex <- runningIndex + 1
      size2 <- size2 + STEP
    }
    size <- size + STEP
    size2 <- SMALLEST
  }
  indexQuants <- data.frame(cbind(size = sizeList,
                                  size2 = size2List,
                                  ICS = ICSmedList,
                                  ICC = ICCmedList,
                                  ICE = ICEmedList))
  indexQuant <- as.data.frame(lapply(indexQuants,unlist))
  indexQuants <- melt(indexQuant,id.vars=c("size","size2"),variable.name="index",value.name="med")
  
  return (indexQuants)
}