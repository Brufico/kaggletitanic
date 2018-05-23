#' ---
#' title: 'Titanic - Kaggle - Case study'
#' subtitle: "Explore for classroom use"
#' author:  "Bruno Fischer Colonimos"
#' date: "`r format(Sys.Date(), '%d %B %Y')`"
#' ---
#' 


#' 
#' Preliminary code
#' ================
#' 
        
#+ r setup0, include=FALSE

## code config parameters
# adjust (?) Gggplot2 theme and color palette
bwtheme <- TRUE         # use bw theme
specialpalette <- FALSE # use the special palette for color-blinds
showarning <- FALSE     # show the warnings
datmis <- "Ukn"         # Value to replace the NA's with 
outresults <- TRUE      # Output the results (printing, etc)



#+libs_prep, echo =FALSE, warning=FALSE, message=FALSE, results='hide'
## Required packages (install before running): "caret", "ggplot2", pander
library(knitr)
library(pander)
library(caret)
library(ggplot2)
library(dplyr)

# ggplot element
ggnolegend <- theme(legend.position = "none")


# Source preparatory code "preparation.R"
source("preparation.R")

# Set aliases
knitr::set_alias(w = "fig.width", a = "fig.asp")

#+ setup, echo = FALSE
# setup chunk : default options
knitr::opts_chunk$set(echo = FALSE, fig.width = w.13, fig.asp = a.11 , fig.pos = 'H', fig.align = "center", fig.show = "hold", warning = showarning)



#' 
#' Data
#' =====
#' 

#' Get it
#' -------
#' 

#' The data is downloaded from Kaggle (https://www.kaggle.com/c/titanic) and saved. It is loaded here from disk.

#+ dataget
# read
datadir <-  "data"
fname <-  "train.csv"
tdf <- read.csv(file.path(datadir, fname), stringsAsFactors = FALSE)



#' * The data head is shown in table `r .ref("tab:datafirst")`
#' 

#+ datafirst
if (outresults) pander(head(tdf), caption = tabcap("A glimpse of the data"))


#' * Quick dataframe summary: table `r .ref("tab:datasum")`

if (outresults) pander(summary(tdf), caption = tabcap("Data summaries"))


#' Data modifications {#modifs}
#' ----------------------------
#'                 
#'                 * Make `Survived` , `Pclass` and `Embark` factors, 
#'         * Create `Famly` = `SibSp + Parch` 
#'         * Create  `Sex.Pclass`
#'         * Substitute missing values with `r datmis` in variable `Embarked`
#'         * added variables
#'         * `Agestat` = age status : "child", "teen", "adult" (cutoff ages = 12, 18)
#'         * `Title` = civility.
#'         * `Letticket` = Ticket number begins with letters (yes = 1, no = 0)
#'         * `Hascabin` = is the cabin number known? (yes = 1, no = 0)
#'         * `Deck`= if the cabin is known, the first letter is the Deck (T, A, B, C...), otherwise  "`r datmis`" 
#'         
        
#+ datamod, warning=FALSE
source("Titanic_preprocessing.R")
# missing values before modifications
if (outresults) pander(t(numna_before), caption = "Before modifications, Number of missing values")

#' 
#' Data Analysis {#analysis}
#' ==================
#' 
#' Passenger Identity {#demographic}
#' -------------------
#' 
#' ### Sex and Age {#sexage}
#' 
#' * Sex
#' 

#+ ident1, warning=FALSE, fig.cap="Gender distribution"
tb_sex <- local({
        tbsex <- table(tdf$Sex)
        ptbsex <- prop.table(tbsex)
        tbsex2 <- rbind(tbsex, ptbsex)
        row.names(tbsex2) <- c( "Frequency", "Rel.Frequency")
        tbsex2
})

if (outresults) pander(tb_sex, digits = 3, caption = tabcap("Gender distribution"))

p_sex <- ggplot(tdf, aes(Sex)) + geom_bar(aes(y= ..prop.., group = 1))
if (outresults) p_sex


#' Figure `r .ref("fig:ident1")` shows the gender distribution

#' 
#' * Age
#'
#+ age, warning=FALSE, fig.cap="Age"  
sum_age <- summary(tdf$Age)
if (outresults) pander(sum_age)
p_age <- local({
        nc <- nclass.FD(tdf$Age[!is.na(tdf$Age)])
        ggplot(tdf, aes(Age)) + geom_histogram(bins=nc)
})
if (outresults) p_age

#' 
#' * Age by sex

#+ ident3,  warning=FALSE, fig.cap="Age and Gender"
p_age_sex <- local({
        nc <- nclass.FD(tdf$Age[!is.na(tdf$Age)])
        ggplot(tdf, aes(Age)) + geom_histogram(aes(y=..density..),bins=nc) + 
                facet_grid(Sex ~ .)
})
if (outresults) p_age_sex       

#' 
#' Figure `r .ref("fig:", "ident3")` shows the age distributions of both genders


#' 
#' * Age Status
#' 

#+ Agestatus
p_agestat_by <- list(
        none = ggplot(tdf, aes(Agestat)) + geom_bar(),
        
        sex = ggplot(tdf, aes(Agestat)) + 
                geom_bar(aes(y = ..prop.., group = Sex, fill = Sex)) + 
                facet_grid(Sex ~ .) + ggnolegend,
        
        class = ggplot(tdf, aes(Agestat)) + 
                geom_bar(aes(y = ..prop.., group = Pclass)) +
                facet_grid(. ~ Pclass)+
                theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) + ggnolegend,
        
        sex_class =ggplot(tdf, aes(Agestat)) + 
                geom_bar(aes(y = ..prop.., group = Pclass)) + 
                facet_grid(Sex ~ Pclass) +
                theme(axis.text.x = element_text(angle = 90))
)

if (outresults) local({xxx <- sapply(p_agestat_by, print)} ) # a revoir



#' 
#' Family members on board {#fmly}
#' ---------------
#' 

#+ family2, fig.cap="Family members on board", warning=FALSE
p_famly <- list(
        sibsp = ggplot(tdf) + geom_bar(aes(SibSp)),
        parch = ggplot(tdf) + geom_bar(aes(Parch)),
        famly = ggplot(tdf) + geom_bar(aes(Famly))
)


if (outresults) for (i in seq_along(p_famly)) p_famly[[i]]
                

#' 
#' Passenger class {#pclass}
#' -------------------
#'                 
#' figure `r .ref("fig:", "ship")` Shows that the third class accounts for about 55% of the passengers. 
        
#+ ship, fig.cap="Passenger Classes Distribution", w = w.13, a = a.13}

tb_pclass <- local({
        tb <- rbind(table(tdf$Pclass),
                    prop.table(table(tdf$Pclass)))
        row.names(tb) = c("frequency", "rel.frequency")
        tb})

if (outresults) pander(tb_pclass,caption = tabcap("Passenger Class (Pclass)"))


p_pclass <- local({
        per3class <- 100 * round(tb_pclass[2,3], 2)
        ptitle = paste0( per3class, "% of the passengers are 3rd class")
        ggplot(tdf) + geom_bar(aes(Pclass, fill = Pclass, y = ..prop.. , group=1)) +
        labs(title = ptitle)
})        


