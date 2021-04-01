### Working Script for EEID Publishing Project

# Read in Data

JClass <- read.csv('JournalsClassification.csv', header = T, stringsAsFactors = FALSE)
head (JClass)
sum(is.na(JClass))

DataRaw <- read.csv('MainDataESA2017.csv', header = T, stringsAsFactors = FALSE)
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

ESA2017[ESA2017 == "N"] <- FALSE
ESA2017[ESA2017 == "Y"] <- TRUE

ESA2017$GenBio <- as.logical(ESA2017$GenBio)
ESA2017$EEIDSpec <- as.logical(ESA2017$EEIDSpec)
ESA2017$EcoSpec <- as.logical(ESA2017$EcoSpec)

GenbioTotal <- sum(ESA2017$GenBio, na.rm = TRUE)
EEIDSpecTotal <- sum(ESA2017$EEIDSpec, na.rm = TRUE)
EcoSpecTotal <- sum(ESA2017$EcoSpec, na.rm = TRUE)

GenBioEEID <- sum(ESA2017$GenBio[which(ESA2017$EEIDSession == "YES")], na.rm = TRUE)
EEIDSpecEEID <- sum(ESA2017$EEIDSpec[which(ESA2017$EEIDSession == "YES")], na.rm = TRUE)
EcoSpecEEID <- sum(ESA2017$EcoSpec[which(ESA2017$EEIDSession == "YES")], na.rm = TRUE)


# Run a binomial GLM looking at whether being published in an ecology-specific journal is predicted in part by being presented at an EEID session or not.
EcoModel1 <- glm(formula = EcoSpec ~ EEIDSession,
                 family = 'binomial',
                 data = na.omit(ESA2017)
)
# Look at a summary of the model outputs: most important here is the 'estimate' values of the 'coefficient' section
# (that tells us effect size and direction)
summary(EcoModel1)

# get the p-value from a Chi-Squared anova (don't look at the p-values in the summary :) )
anova(EcoModel1, test = 'Chisq')



