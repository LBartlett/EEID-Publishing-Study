### Working Script for EEID Publishing Project

# Read in Data

JClass <- read.csv('JournalsClassification.csv', header = T)
head (JClass)

# Tyler - load in main spreadsheet after you've data cleaned and see if it will read in ok

sum(is.na(JClass))
