### Working Script for EEID Publishing Project

# Read in Data

JClass <- read.csv('JournalsClassification.csv', header = T)
head (JClass)
sum(is.na(JClass))

DataRaw <- read.csv('MainDataESA2017.csv', header = T)
head(DataRaw)

# Assemble a new data frame for analysis

ESA2017 <- data.frame('Talk' = 1:NROW(DataRaw))

for (N in 1:NROW(ESA2017)){
  
  ESA2017$EEIDSession[N] <- DataRaw$EEIDSession[N]
  
  ESA2017$Located[N] <- DataRaw$PublicationFound[N]
  
}

ESA2017 <- na.omit(ESA2017)

for (N in 1:NROW(ESA2017)){
  
  n <- ESA2017$Talk[N]
  
  ESA2017$Year[N] <- DataRaw$Year[n]
  
  ESA2017$Journal[N] <- DataRaw$Journal[n]
  
  if(is.na(ESA2017$Journal[N])){
    
    ESA2017$GenBio[N] <- NA
    ESA2017$EEIDSpec[N] <- NA
    ESA2017$EcoSpec[N] <- NA
    
  }else{
    
    ESA2017$GenBio[N] <- JClass$GeneralBiology[which(JClass$Journal == ESA2017$Journal[N])]
    ESA2017$EEIDSpec[N] <- JClass$EEIDSpecific[which(JClass$Journal == ESA2017$Journal[N])]
    ESA2017$EcoSpec[N] <- JClass$EcologySpecific[which(JClass$Journal == ESA2017$Journal[N])]
    
  }
}

# Steps for Tyler:
# see if you can figure out roughly how the above works
# think about what the data types are, and how one section of the data will need 'relabelling'
# relabel certain parts of the data as appropriate
# have a go at some form of statistical analysis





