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
        
#+ ship, fig.cap="Passenger Classes Distribution", w = w.13, a = a.13

tb_pclass <- local({
        tb <- rbind(table(tdf$Pclass),
                    prop.table(table(tdf$Pclass)))
        row.names(tb) = c("frequency", "rel.frequency")
        tb})

if (outresults) {pander(tb_pclass, caption = tabcap("Passenger Class (Pclass)"))}


p_pclass <- local({
        per3class <- 100 * round(tb_pclass[2,3], 2)
        ptitle = paste0( per3class, "% of the passengers are 3rd class")
        ggplot(tdf) + geom_bar(aes(Pclass, fill = Pclass, y = ..prop.. , group=1)) +
        labs(title = ptitle)
})        

if (outresults) p_pclass


#' ### Passengers deographics by class

p_age_by <- local({
        nc <- nclass.FD(tdf$Age[!is.na(tdf$Age)])
        
        list(
                sex = ggplot(tdf, aes(Sex)) + 
                        geom_bar(aes(y = ..prop.., fill = Pclass, group=Pclass)) + 
                        facet_grid(Pclass ~ .) +
                        labs(y = "Proportion", title = "Sex by Class") + 
                        ggnolegend,
                
                age = ggplot(tdf, aes(Age)) + 
                        geom_histogram(aes(y=..density..,fill=Pclass),bins=nc) +
                        facet_grid(Pclass ~ .) +
                        ggnolegend,
                famly = ggplot(tdf) + 
                        geom_bar(aes(Famly, y = ..prop.., fill=Pclass)) +
                        facet_grid(Pclass ~ .)+ 
                        scale_x_continuous(breaks=seq(0,10,by=2)) +
                        ggnolegend,
                by_famly = ggplot(data = tdf, mapping = aes(Pclass,  fill= Pclass)) + 
                        geom_bar() +
                        facet_grid(factor(Famly) ~ .)+
                        ggnolegend
        )
})



Deck
----
        
#+ Deck, fig.cap= "Distribution of decks"
p_deck = list(
        deck_all = ggplot(tdf) +
                geom_bar(aes(Deck, y=..prop.., group = 1)),
        deck_known = ggplot(tdf[tdf$Deck != datmis,] ) +
                geom_bar(aes(Deck, y=..prop.., group = 1)),
        by_class = ggplot(tdf[tdf$Deck != datmis,] ) +
                geom_bar(aes(Deck, y=..prop.., fill = Pclass, group = Pclass)) + 
                facet_grid(Pclass ~ .) + ggnolegend
)



#' Figure `r .ref("fig:", "Deck") shows a very partial distribution of decks:
#' the deck of nearly 80% of the passengers is unknown. However, the third graph
#' reveals the strong link between the passencger class and the deck.



#' Embarkation point
#' ----

# table
tdf$Embarked <- factor(tdf$Embarked, levels = c("S", "C", "Q"))

tb_embark <- local({
        temb <- table(tdf$Embarked)
        # proportion table
        ptemb <- prop.table(temb)
        df <- rbind(frequency = temb , Rfreq = ptemb) 
})

if (outresults) pander(tb_embark, digits=2, caption = "Embarkation port")


# na_emb <- sum(is.na(tdf$Embarked)) #  2 passengers have unknown embarkation port
# tdfx <- tdf[complete.cases(tdf), ] 

p_embark <- local({
        list(
                all = ggplot(tdf) + 
                        geom_bar(aes(Embarked, y=..prop.., group=1)),
                by_sex = ggplot(tdf) + 
                        geom_bar(aes(Embarked, y=..prop.., group=Sex)) +
                        facet_grid(.~Sex) + ggnolegend,
                by_class = ggplot(tdfx) + geom_bar(aes(Embarked, y=..prop.., fill = Pclass,  group=Pclass)) + 
                        facet_grid(Pclass~.) +
                        ggnolegend,
                by_sex_class = ggplot(tdfx) + 
                        geom_bar(aes(Embarked, y=..prop.., 
                                     fill = Pclass, group=Pclass)) +
                        facet_grid(Pclass~Sex) + ggnolegend
        )
})

if (outresults) p_embark


#' Figure `r .ref("fig:", "Embarked")` shows that Southampton was the major
#' embarkation point (`r  100 * df["Rfreq", "S"]`% of the passengers). However,
#' this is not as true for the first-class passengers, particularly for the
#' women of the first-class : only about 50% of them embarked at
#' Southampton, and about 50% embarked at Cherbourg



#' The Fare
#' --------
        
#+ fare, w=w.12,a=a.11, fig.cap="Fare by class and Sex"}

p_fare <- local({
        # ncf <- nclass.FD(tdf$Fare) 
        ncf <- 50
        list(
                all_hist = ggplot(tdf) + geom_histogram(aes(Fare), bins = ncf),
                class_hist = ggplot(tdf) +
                        geom_histogram(aes(Fare, y=..density.., 
                                   fill=Pclass), bins = ncf),
                class_facet_hist = ggplot(tdf) + 
                        geom_histogram(aes(Fare, y=..density.., 
                                           fill=Pclass), bins = ncf) +
                        facet_grid(Pclass ~.),
                class_box <- ggplot(tdf) + 
                        geom_boxplot(aes(Pclass,Fare, 
                                         fill = Pclass), color = "grey50") + 
                        scale_x_discrete(limits = rev(levels(tdf$Pclass))) +
                        coord_flip() + ggnolegend,
                class_sex_box = ggplot(tdf) + 
                        geom_boxplot(aes(Pclass,Fare, 
                                         fill = Pclass), color = "grey50") + 
                        scale_x_discrete(limits = rev(levels(tdf$Pclass))) +
                        coord_flip() +
                        facet_grid(Sex ~.) 
        )
})

if (outresults) p_fare


#'  

#' survival
#' ============
#' 



#' ### overall

#+ survival, fig.cap="Overall survival"}
tb_survived <- prop.table(table(tdf$Survived))

if (outresults) pander(tb_survived, digits=2, 
                       caption = "Suvival global distribution")

p_survived <- ggplot(tdf) + geom_bar(aes(Survived, y = ..prop.., group = 1))


if (outresults) p_survived
        
#' ### By category

#+ survival-by, fig.cap="Survival by sex or Passenger Class"}
# by sex

# tables
tb_survived_by <- list( sex =prop.table(table(tdf$Survived, tdf$Sex),
                             margin = 2),
                        class =prop.table(table(tdf$Survived, tdf$Pclass),
                                          margin = 2)
                        )

if (outresults) {pander(tb_survived_by, digits=2,
                       caption = "Survival by Sex , class")
}


# plots

p_survived_by <- local({
        list(
                sex = ggplot(tdf) + 
                        geom_bar(aes(Survived, y = ..prop.., group = Sex)) + 
                        facet_grid(Sex ~.),
                class = ggplot(tdf) + 
                        geom_bar(aes(Survived, y = ..prop.., group = Pclass, fill = Pclass)) + 
                        facet_grid(Pclass ~.) + ggnolegend,
                class_sex = ggplot(tdf) +
                        geom_bar(aes(Survived, y = ..prop.., 
                                     group = Pclass, fill = Pclass)) + 
                        facet_grid(Pclass ~ Sex) + ggnolegend,
                class_sex_south = ggplot(tdfx[tdfx$Embarked == "S", ]) + 
                        geom_bar(aes(Survived, y = ..prop.., 
                                     group = Pclass, fill = Pclass))+ 
                        facet_grid(Pclass ~ Sex)+
                        labs(title = "Emb. Southampton") + ggnolegend,
                class_sex_cherbourg = ggplot(tdfx[tdfx$Embarked == "C", ]) + 
                        geom_bar(aes(Survived, y = ..prop.., 
                                     group = Pclass, fill = Pclass))+ 
                        facet_grid(Pclass ~ Sex)+
                        labs(title = "Emb. Cherbourg") + ggnolegend,,
                class_sex_queenstown = ggplot(tdfx[tdfx$Embarked == "Q", ]) + 
                        geom_bar(aes(Survived, y = ..prop.., 
                                     group = Pclass, fill = Pclass))+ 
                        facet_grid(Pclass ~ Sex)+
                        labs(title = "Emb. Queenstown") + ggnolegend
                
                
        )
})




#+ survival-by4, fig.cap="Age by Survival  and Class", warning=FALSE

# not interesting ??
local({
        nc <- nclass.FD(tdf$Age[!is.na(tdf$Age)])
 
        ggplot(tdf) + 
                geom_histogram(aes(Age, y= ..density.., fill = Pclass), bins = nc) +
                facet_grid(Pclass ~ Sex) +
                labs(title= "All") + ggnolegend
        
        
        ggplot(tdf[tdf$Survived == "Yes", ]) + 
                geom_histogram(aes(Age, y= ..density.., fill = Pclass), bins = nc) +
                facet_grid(Pclass ~ Sex) +
                labs(title= "Survivors") + ggnolegend
        
        ggplot(tdf[tdf$Survived == "No", ]) + 
                geom_histogram(aes(Age, y= ..density.., fill = Pclass), bins = nc) +
                facet_grid(Pclass ~ Sex) +
                labs(title= "Non-Survivors") + ggnolegend
        
})





#' * Class and Agestat, Agestat and sex

#+ survival-by5, fig.cap="Survival by age status, Class and sex"

agesex_class <- table(tdf$Agestat, tdf$Sex, tdf$Pclass)

pander(agesex_class, caption = "Distribution of age-status vs sex and Pclass" )




ggplot(tdf) + 
        geom_bar(aes(Survived, y = ..prop.., 
                     group = Pclass, fill = Pclass))+ 
        facet_grid(Pclass ~ Agestat) + 
        labs(title = "Sex = All") +
        theme(legend.position = "none")

ggplot(tdf[tdf$Sex == "female" ,]) + 
        geom_bar(aes(Survived, y = ..prop.., 
                     group = Pclass, fill = Pclass))+ 
        facet_grid(Pclass ~ Agestat) +
        labs(title = "Sex ='female'") + 
        theme(legend.position = "none")

ggplot(tdf[tdf$Sex == "male" ,]) + 
        geom_bar(aes(Survived, y = ..prop.., 
                     group = Pclass, fill = Pclass))+ 
        facet_grid(Pclass ~ Agestat)+
        labs(title = "Sex = 'male'") + 
        theme(legend.position = "none")




#+  survival_by6, w = w.11, fig.asp = 1.5, fig.cap="Survival by Deck, age and class"}

ggplot(tdf) + 
        geom_bar(aes(Survived, y = ..prop.., 
                     group = Deck, fill = Pclass))+ 
        facet_grid(Deck ~ Agestat)+
        labs(title = "by Deck") # + theme(legend.position = "none")


#+ survival_by7, w = w.11, fig.asp = 1.5, fig.cap="Survival by Deck, Age and Sex"}

ggplot(tdf) + 
        geom_bar(aes(Survived, y = ..prop.., 
                     group = Deck, fill = Sex))+ 
        facet_grid(Deck ~ Agestat)+
        labs(Title = "by Deck") # + theme(legend.position = "none")






