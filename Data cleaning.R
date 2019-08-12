require(dplyr)
# Reading data into data frames
RICS <- read.csv("RICS_2019DATA.csv",header=TRUE)
AYTM <- read.csv("AYTM_2019DATA.csv",header=TRUE)
CATI <- read.csv("CATI_2019DATA.csv",header=TRUE)

RICS18 <- read.csv("RICS_2018DATA.csv",header=TRUE)
AYTM18 <- read.csv("AYTM_2018DATA.csv",header=TRUE)
CATI18 <- read.csv("CATI_2018DATA.csv",header=TRUE)

# Cleaning data frames to only have the variables I want to keep, and then re-coding those variables to be numeric
# instead of text

# The ".clean" data frames contain only the variables we care about, and in the case of the AYTM set, renames
# some of the columns to be consistent with the other data sets.
AYTM.clean <- select(AYTM,COUNTY_PY,REGION_PY,AGE_RECODE,RACE_RECODE,GENDER_RECODE,contains("Q"))
AYTM.clean <- rename(AYTM.clean,YEARAGO=Q1Currentlywouldyousaythatyouandyourfamilyarebetter,
                     NEXTYEAR=Q2Lookingaheadoneyearfromnowwouldyousaythatyouand,
                     BUSNXTYR=Q3Oneyearfromtodaydoyouexpectthatbusinessconditions,
                     FIVEYEAR=Q4Duringthenextfiveyearswhichwouldyousayismorelike,
                     BIGITEM=Q5Doyouthinknowisgenerallyagoodorabadtimeforpeopl)

AYTM18.clean <- select(AYTM18,COUNTY_PY,REGION_PY,AGE_RECODE,RACE_RECODE,GENDER_RECODE,contains("Q"))
AYTM18.clean <- rename(AYTM18.clean,YEARAGO=Q1Currentlywouldyousaythatyouandyourfamilyarebetter,
                     NEXTYEAR=Q2Lookingaheadoneyearfromnowwouldyousaythatyouand,
                     BUSNXTYR=Q3Oneyearfromtodaydoyouexpectthatbusinessconditions,
                     FIVEYEAR=Q4Duringthenextfiveyearswhichwouldyousayismorelike,
                     BIGITEM=Q5Doyouthinknowisgenerallyagoodorabadtimeforpeopl)

CATI.clean <- select(CATI,COUNTY,REGION_PY,AGE_RECODE,RACE_RECODE,GENDER_RECODE,CELL,YEARAGO,NEXTYEAR,
                     BUSNXTYR,FIVEYEAR,BIGITEM)
# This is so the variable name for phones is consistent with RICS and it makes more sense than CELL simply
# because we're looking at both cellphones and landlines. The COUNTY_PY change is also consistent with the
# other data sets.
CATI.clean <- rename(CATI.clean,Phone=CELL,COUNTY_PY=COUNTY)

CATI18.clean <- select(CATI18,COUNTY,REGION,AGECAT,RACE,SEX,CELL,YEARAGO,NEXTYEAR,
                     BUSNXTYR,FIVEYEAR,BIGITEM)
CATI18.clean <- rename(CATI18.clean,REGION_PY=REGION,
                       AGE_RECODE=AGECAT,
                       RACE_RECODE=RACE,
                       GENDER_RECODE=SEX,
                       Phone=CELL,
                       COUNTY_PY=COUNTY)

RICS.clean <- select(RICS,Phone,YEARAGO,NEXTYEAR,BUSNXTYR,FIVEYEAR,BIGITEM,COUNTY_PY,REGION_PY,GENDER_RECODE,
                     RACE_RECODE,AGE_RECODE)

RICS18.clean <- select(RICS18,Cellphone,Q1,Q2,Q3,Q4,Q5,COUNTY_PY,REGION_PY,GENDER_RECODE,
                     RACE_RECODE,AGE_RECODE)
RICS18.clean <- rename(RICS18.clean,Phone=Cellphone,
                       YEARAGO=Q1,
                       NEXTYEAR=Q2,
                       BUSNXTYR=Q3,
                       FIVEYEAR=Q4,
                       BIGITEM=Q5)

# The following lines reformat the data frames to have only numeric values consistent with the codes we're using.

# Changes all the empty values for CELL to the code for landlines
for (i in 1:length(CATI.clean$Phone)){
  if (is.na(CATI.clean$Phone[i])){
    CATI.clean$Phone[i] <- 1;
  }
}

for (i in 1:length(CATI18.clean$Phone)){
  if (is.na(CATI18.clean$Phone[i])){
    CATI18.clean$Phone[i] <- 1;
  }
}

# Swaps values to be consistent with the other data sets
for (i in 1:length(RICS.clean$FIVEYEAR)){
  if (RICS.clean$FIVEYEAR[i] == 2){
    RICS.clean$FIVEYEAR[i] <- 3;
  }
  else if (RICS.clean$FIVEYEAR[i] == 3){
    RICS.clean$FIVEYEAR[i] <- 2;
  }
}

for (i in 1:length(RICS18.clean$FIVEYEAR)){
  if (RICS18.clean$FIVEYEAR[i] == 2){
    RICS18.clean$FIVEYEAR[i] <- 3;
  }
  else if (RICS18.clean$FIVEYEAR[i] == 3){
    RICS18.clean$FIVEYEAR[i] <- 2;
  }
}
# Swaps values to be consistent with the other data sets
for (i in 1:length(RICS.clean$BIGITEM)){
  if (RICS.clean$BIGITEM[i] == 2){
    RICS.clean$BIGITEM[i] <- 3;
  }
  else if (RICS.clean$BIGITEM[i] == 3){
    RICS.clean$BIGITEM[i] <- 2;
  }
}

for (i in 1:length(RICS18.clean$BIGITEM)){
  if (RICS18.clean$BIGITEM[i] == 2){
    RICS18.clean$BIGITEM[i] <- 3;
  }
  else if (RICS18.clean$BIGITEM[i] == 3){
    RICS18.clean$BIGITEM[i] <- 2;
  }
}
# Changes text values to numeric values
levels(RICS.clean$Phone)[levels(RICS.clean$Phone) == "CELL"] <- '2'
levels(RICS.clean$Phone)[levels(RICS.clean$Phone) == "LAND"] <- '1'
levels(RICS.clean$Phone)[levels(RICS.clean$Phone) == "NOTSURE"] <- '3'
RICS.clean$Phone <- as.numeric(levels(RICS.clean$Phone))[RICS.clean$Phone]

levels(RICS18.clean$Phone)[levels(RICS18.clean$Phone) == "YES"] <- '2'
levels(RICS18.clean$Phone)[levels(RICS18.clean$Phone) == "NO"] <- '1'
levels(RICS18.clean$Phone)[levels(RICS18.clean$Phone) == "NOTSURE"] <- '3'
RICS18.clean$Phone <- as.numeric(levels(RICS18.clean$Phone))[RICS18.clean$Phone]

# Changes text values to numeric values
levels(AYTM.clean$YEARAGO)[levels(AYTM.clean$YEARAGO) == "Better Off"] <- '1'
levels(AYTM.clean$YEARAGO)[levels(AYTM.clean$YEARAGO) == "Same"] <- '2'
levels(AYTM.clean$YEARAGO)[levels(AYTM.clean$YEARAGO) == "Worse Off"] <- '3'
levels(AYTM.clean$YEARAGO)[levels(AYTM.clean$YEARAGO) == "No Response"] <- '4'
AYTM.clean$YEARAGO <- as.numeric(levels(AYTM.clean$YEARAGO))[AYTM.clean$YEARAGO]

levels(AYTM18.clean$YEARAGO)[levels(AYTM18.clean$YEARAGO) == "Better Off"] <- '1'
levels(AYTM18.clean$YEARAGO)[levels(AYTM18.clean$YEARAGO) == "Same"] <- '2'
levels(AYTM18.clean$YEARAGO)[levels(AYTM18.clean$YEARAGO) == "Worse Off"] <- '3'
levels(AYTM18.clean$YEARAGO)[levels(AYTM18.clean$YEARAGO) == "No Response"] <- '4'
AYTM18.clean$YEARAGO <- as.numeric(levels(AYTM18.clean$YEARAGO))[AYTM18.clean$YEARAGO]

# Changes text values to numeric values
levels(AYTM.clean$NEXTYEAR)[levels(AYTM.clean$NEXTYEAR) == "Better Off"] <- '1'
levels(AYTM.clean$NEXTYEAR)[levels(AYTM.clean$NEXTYEAR) == "Same"] <- '2'
levels(AYTM.clean$NEXTYEAR)[levels(AYTM.clean$NEXTYEAR) == "Worse Off"] <- '3'
levels(AYTM.clean$NEXTYEAR)[levels(AYTM.clean$NEXTYEAR) == "No Response"] <- '4'
AYTM.clean$NEXTYEAR <- as.numeric(levels(AYTM.clean$NEXTYEAR))[AYTM.clean$NEXTYEAR]

levels(AYTM18.clean$NEXTYEAR)[levels(AYTM18.clean$NEXTYEAR) == "Better Off"] <- '1'
levels(AYTM18.clean$NEXTYEAR)[levels(AYTM18.clean$NEXTYEAR) == "Same"] <- '2'
levels(AYTM18.clean$NEXTYEAR)[levels(AYTM18.clean$NEXTYEAR) == "Worse Off"] <- '3'
levels(AYTM18.clean$NEXTYEAR)[levels(AYTM18.clean$NEXTYEAR) == "No Response"] <- '4'
AYTM18.clean$NEXTYEAR <- as.numeric(levels(AYTM18.clean$NEXTYEAR))[AYTM18.clean$NEXTYEAR]

# Changes text values to numeric values
levels(AYTM.clean$BUSNXTYR)[levels(AYTM.clean$BUSNXTYR) == "Better Off"] <- '1'
levels(AYTM.clean$BUSNXTYR)[levels(AYTM.clean$BUSNXTYR) == "Same"] <- '2'
levels(AYTM.clean$BUSNXTYR)[levels(AYTM.clean$BUSNXTYR) == "Worse Off"] <- '3'
levels(AYTM.clean$BUSNXTYR)[levels(AYTM.clean$BUSNXTYR) == "No Response"] <- '4'
AYTM.clean$BUSNXTYR <- as.numeric(levels(AYTM.clean$BUSNXTYR))[AYTM.clean$BUSNXTYR]

levels(AYTM18.clean$BUSNXTYR)[levels(AYTM18.clean$BUSNXTYR) == "Better Off"] <- '1'
levels(AYTM18.clean$BUSNXTYR)[levels(AYTM18.clean$BUSNXTYR) == "Same"] <- '2'
levels(AYTM18.clean$BUSNXTYR)[levels(AYTM18.clean$BUSNXTYR) == "Worse Off"] <- '3'
levels(AYTM18.clean$BUSNXTYR)[levels(AYTM18.clean$BUSNXTYR) == "No Response"] <- '4'
AYTM18.clean$BUSNXTYR <- as.numeric(levels(AYTM18.clean$BUSNXTYR))[AYTM18.clean$BUSNXTYR]

# Changes text values to numeric values
levels(AYTM.clean$FIVEYEAR)[levels(AYTM.clean$FIVEYEAR) == "Continuous Good Times"] <- '1'
levels(AYTM.clean$FIVEYEAR)[levels(AYTM.clean$FIVEYEAR) == "Both"] <- '2'
levels(AYTM.clean$FIVEYEAR)[levels(AYTM.clean$FIVEYEAR) == "Periods of Unemployment/Depression"] <- '3'
levels(AYTM.clean$FIVEYEAR)[levels(AYTM.clean$FIVEYEAR) == "No Answer"] <- '4'
AYTM.clean$FIVEYEAR <- as.numeric(levels(AYTM.clean$FIVEYEAR))[AYTM.clean$FIVEYEAR]

levels(AYTM18.clean$FIVEYEAR)[levels(AYTM18.clean$FIVEYEAR) == "Continuous Good Times"] <- '1'
levels(AYTM18.clean$FIVEYEAR)[levels(AYTM18.clean$FIVEYEAR) == "Both"] <- '2'
levels(AYTM18.clean$FIVEYEAR)[levels(AYTM18.clean$FIVEYEAR) == "Periods of Unemployment/Depression"] <- '3'
levels(AYTM18.clean$FIVEYEAR)[levels(AYTM18.clean$FIVEYEAR) == "No Answer"] <- '4'
AYTM18.clean$FIVEYEAR <- as.numeric(levels(AYTM18.clean$FIVEYEAR))[AYTM18.clean$FIVEYEAR]

# Changes text values to numeric values
levels(AYTM.clean$BIGITEM)[levels(AYTM.clean$BIGITEM) == "Good Time"] <- '1'
levels(AYTM.clean$BIGITEM)[levels(AYTM.clean$BIGITEM) == "Not Sure"] <- '2'
levels(AYTM.clean$BIGITEM)[levels(AYTM.clean$BIGITEM) == "Bad Time"] <- '3'
levels(AYTM.clean$BIGITEM)[levels(AYTM.clean$BIGITEM) == "No Answer"] <- '4'
AYTM.clean$BIGITEM <- as.numeric(levels(AYTM.clean$BIGITEM))[AYTM.clean$BIGITEM]

levels(AYTM18.clean$BIGITEM)[levels(AYTM18.clean$BIGITEM) == "Good Time"] <- '1'
levels(AYTM18.clean$BIGITEM)[levels(AYTM18.clean$BIGITEM) == "Not Sure"] <- '2'
levels(AYTM18.clean$BIGITEM)[levels(AYTM18.clean$BIGITEM) == "Bad Time"] <- '3'
levels(AYTM18.clean$BIGITEM)[levels(AYTM18.clean$BIGITEM) == "No Answer"] <- '4'
AYTM18.clean$BIGITEM <- as.numeric(levels(AYTM18.clean$BIGITEM))[AYTM18.clean$BIGITEM]

#Writing clean data to new .csv files
write.csv(AYTM.clean, file = "Clean_AYTM.csv",row.names=FALSE)
write.csv(CATI.clean, file = "Clean_CATI.csv",row.names=FALSE)
write.csv(RICS.clean, file = "Clean_RICS.csv",row.names=FALSE)

write.csv(AYTM18.clean, file = "Clean_AYTM18.csv",row.names=FALSE)
write.csv(CATI18.clean, file = "Clean_CATI18.csv",row.names=FALSE)
write.csv(RICS18.clean, file = "Clean_RICS18.csv",row.names=FALSE)