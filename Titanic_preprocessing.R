#' ---
#' Title: 'Titanic - Kaggle - Some exploration'
#' subTitle: "R file for data preprocessing, called by Titanic_Explore"
#' author:  "Bruno Fischer Colonimos"
#' date: "`r format(Sys.Date(), '%d %B %Y')`"
#' ---


# libraries

library(knitr)
library(pander)
library(caret)
library(ggplot2)
library(dplyr)




# Number of missing values
numna_before <- sapply(tdf,
                       FUN = function(x){sum(is.na(x))} )

# pander(t(numna_before), caption = "Before modifications, Number of missing values")


# Text --> Factors
# ------------------

# preserve binary survived
tdf$Survive_bin <- tdf$Survived
# make factor Survived
tdf$Survived <- local({
        sf <- factor(tdf$Survived, labels=c("No", "Yes"))
        sf
}) 

# make factor class
tdf$Pclass <- factor(tdf$Pclass)

# combine SibSp + Parch --> Famply
tdf$Famly <- tdf$SibSp + tdf$Parch

# Embarked
tdf$Embarked[which(tdf$Embarked == "")] <- datmis # replace text missing values (empty strings) with `datmis`
tdf$Embarked <-  factor(tdf$Embarked)
# levels(tdf$Embarked)
# levels(tdf$Embarked)

# ajout de variables
tdf <- tdf %>%
        mutate(Sex.Pclass = paste(Sex, Pclass, sep ="."))

# age status
tdf$Agestat <- cut(tdf$Age, breaks=c(0,12,18,100), labels=c("child", "teen", "adult"))

tdf$Agestat <- as.character(tdf$Agestat)
tdf$Agestat <- ifelse(is.na(tdf$Agestat), datmis, tdf$Agestat)
tdf$Agestat <- factor(tdf$Agestat)
# levels(tdf$Agestat)
# table(tdf$Agestat)

# Title: retrieve the Title from the name

tdf$Title <- local({
        begcivil <- unlist(regexec(pattern = ",", 
                                   text= tdf$Name,
                                   fixed = TRUE)
        )
        
        endcivil <- unlist(regexec(pattern = "\\. ", 
                                   text =substring(text = tdf$Name, first = begcivil + 2 ) )
        )
        
        substring(text = tdf$Name, 
                  first = begcivil + 2, 
                  last = begcivil + endcivil)
})

table(tdf$Agestat, tdf$Title)
# table(tdf$Sex, tdf$Title)
# table(tdf$Pclass, tdf$Title)


# Using Title to correct Agestat:

# "Master" is only used for (male) children
tdf <- local({
        index <- which(tdf$Title == "Master" & tdf$Agestat == datmis)
        tdf[index, "Agestat"] <- "child"
        # print(head(tdf[index, ] ))
        tdf
})

# "Mrs" means a female adult status, even if the age is "teen" (7 cases), 2 with 18 and 14 years of age)
tdf <- local({
        index <- which(tdf$Title == "Mrs" & tdf$Agestat != "adult")
        tdf[index, "Agestat"] <- "adult"
        # print(head(tdf[index, ] ))
        tdf
})

# "Dr" means an adult status (1 case Unknown)
tdf <- local({
        index <- which(tdf$Title == "Dr" & tdf$Agestat != "adult")
        tdf[index, "Agestat"] <- "adult"
        # print(head(tdf[index, ] ))
        tdf
})





# Ticket number begins with letters
tdf$Letticket <- as.numeric(substr(tdf$Ticket,start = 1, stop = 1 ) %in% LETTERS)


# Hascabin is the cabin number nkown?
tdf$Hascabin <- as.numeric(tdf$Cabin != "")

# table(tdf$Hascabin, tdf$Pclass)
# table(tdf$Letticket, tdf$Pclass)

# Deck (from https://triangleinequality.wordpress.com/2013/09/08/basic-feature-engineering-with-the-titanic-data/)

#This is going be very similar, we have a ‘Cabin’ column not doing much, only
#1st class passengers have cabins, the rest are ‘Unknown’. A cabin number looks
#like ‘C123’. The letter refers to the deck, and so we’re going to extract these
#just like the Titles. Turning cabin number into Deck
#
# cabin_list <- c('A', 'B', 'C', 'D', 'E', 'F', 'T', 'G', 'Unknown')
# df[,'Deck']=df[,'Cabin'].map(lambda x: substrings_in_string(x, cabin_list))

tdf$Deck <- factor(ifelse( tdf$Cabin != "", 
                           substr(tdf$Cabin,start = 1,stop = 1), 
                           datmis))
tdf$Deck <- relevel(tdf$Deck, ref = "T")
# levels(tdf$Deck)


# table(tdf$Deck, tdf$Pclass)
# table(tdf$Deck, tdf$Survived)


numna <- sapply(tdf,
                FUN = function(x){sum(is.na(x))} )








pander(t(numna), caption = "After modifications, Number of missing values")



