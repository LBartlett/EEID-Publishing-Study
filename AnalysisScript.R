### Collaborative project w/ Tyler Washburn & Lewis Bartlett

## Contact lewis.bartlett@uga.edu / https://scholar.google.com/citations?user=PV5ca5UAAAAJ&hl

###############

#Read in Data

JClass <- read.csv('JournalsClassification.csv', header = T, stringsAsFactors = FALSE)
head (JClass)
sum(is.na(JClass))

DataRaw <- read.csv('MainData.csv', header = T, stringsAsFactors = FALSE)

head(DataRaw)

# Assemble a new data frame for analysis

ESAData <- data.frame('Talk' = 1:NROW(DataRaw))

for (N in 1:NROW(ESAData)){
  
  ESAData$EEIDSession[N] <- DataRaw$EEIDSession[N]
  
  ESAData$Located[N] <- DataRaw$PublicationFound[N]
  
}

ESAData <- na.omit(ESAData)

for (N in 1:NROW(ESAData)){
  
  n <- ESAData$Talk[N]
  
  ESAData$Year[N] <- DataRaw$Year[n]
  
  ESAData$Journal[N] <- DataRaw$Journal[n]
  
  if(is.na(ESAData$Journal[N])){
    
    ESAData$GenBio[N] <- NA
    ESAData$IDSpec[N] <- NA
    ESAData$EcoSpec[N] <- NA
    
  }else{
    
    ESAData$GenBio[N] <- JClass$GeneralBiology[which(JClass$Journal == ESAData$Journal[N])]
    ESAData$IDSpec[N] <- JClass$IDSpecific[which(JClass$Journal == ESAData$Journal[N])]
    ESAData$EcoSpec[N] <- JClass$EcologySpecific[which(JClass$Journal == ESAData$Journal[N])]
    
  }
}

ESAData[ESAData == "N"] <- FALSE
ESAData[ESAData == "Y"] <- TRUE

ESAData$GenBio <- as.logical(ESAData$GenBio)
ESAData$IDSpec <- as.logical(ESAData$IDSpec)
ESAData$EcoSpec <- as.logical(ESAData$EcoSpec)

GenbioTotal <- sum(ESAData$GenBio, na.rm = TRUE)
IDSpecTotal <- sum(ESAData$IDSpec, na.rm = TRUE)
EcoSpecTotal <- sum(ESAData$EcoSpec, na.rm = TRUE)

GenBioEEID <- sum(ESAData$GenBio[which(ESAData$EEIDSession == "YES")], na.rm = TRUE)
IDSpecEEID <- sum(ESAData$IDSpec[which(ESAData$EEIDSession == "YES")], na.rm = TRUE)
EcoSpecEEID <- sum(ESAData$EcoSpec[which(ESAData$EEIDSession == "YES")], na.rm = TRUE)

### Analysis

# Run a binomial GLM looking at whether being published in an ecology-specific journal is predicted in part by being presented at an EEID session or not.
EcoModel1 <- glm(formula = EcoSpec ~ EEIDSession,
                 family = 'binomial',
                 data = na.omit(ESAData)
)
# Look at a summary of the model outputs: most important here is the 'estimate' values of the 'coefficient' section
# (that tells us effect size and direction)
summary(EcoModel1)

# get the p-value from a Chi-Squared anova (don't look at the p-values in the summary :) )
anova(EcoModel1, test = 'Chisq')

# ID sessions singificantly less likely to assocociated with pubs in ecology journals

EcoModel2 <- glm(formula = IDSpec ~ EEIDSession,
                 family = 'binomial',
                 data = na.omit(ESAData))
summary(EcoModel2)
anova(EcoModel2, test = 'Chisq')

# unsurprisingly, more likely to be associated with ID-journals

EcoModel3 <- glm(formula = GenBio ~ EEIDSession,
                 family = 'binomial',
                 data = na.omit(ESAData))
summary(EcoModel3)
anova(EcoModel3, test = 'Chisq')

# Significantly more likely than their other ecology session counterparts to be published in general bio journals

ESAData[ESAData == "NO"] <- FALSE
ESAData[ESAData == "YES"] <- TRUE
ESAData$Located <- as.logical(ESAData$Located)
ESAData$EEIDSession <- as.logical(ESAData$EEIDSession)

EcoModel4 <- glm(formula = Located ~ EEIDSession,
                 family = 'binomial',
                 data = ESAData)
summary(EcoModel4)
anova(EcoModel4, test = 'Chisq')

# no apparent bias in 'finding rates'


1/((31/74)/(49/78))
# non-EEID sessions 1.5x more likely to be in ecology journals

(19/74)/(6/78)
# EEID sessions 3.3x more likely to be in general bio journals

# Pie charts

# nicer ones for publication made in different software

par(mfrow = c(1,2))

SLID <- c(31, 19, 18, 6)
SLNID<- c(49, 6, 0, 23)
Labs1 <- c('Ecology Specific', 'General Biology', 'ID Specific', 'Other')
Cols1 <- c('Green4', 'Blue3','Orange3','Grey3')
Labs2 <- c('Ecology Specific', 'General Biology', 'Other')
Cols2 <- c('Green4', 'Blue3','Grey3')
pie(SLID, 
    labels = Labs1,
    col = Cols1,
    main="ID Abstracts")
pie(SLNID,
    labels = Labs2,
    col = Cols2,
    main="Non-ID Abstracts")





