### Working Script for EEID Publishing Project

# Read in Data

JClass <- read.csv('JournalsClassification.csv', header = T)
head (JClass)
sum(is.na(JClass))

DataRaw <- read.csv('MainDataESA2017.csv', header = T)
head(DataRaw)

# Assemble new data frame for analysis

ESA2017 <- data.frame('Talk' = 1:NROW(DataRaw))

for (N in 1:NROW(ESA2017)){
  
  ESA2017$EEIDSession[N] <- DataRaw$EEIDSession[N]
  
  ESA2017$Located[N] <- DataRaw$PublicationFound[N]
  
}

ESA2017 <- na.omit(ESA2017)

for (N in ESA2017$Talk){
  
  for (N in 1:NROW(ESA2017)){
    
    ESA2017$Year[N] <- DataRaw$Year[N]
    
    ESA2017$Located[N] <- DataRaw$PublicationFound[N]
    
  }
  
}