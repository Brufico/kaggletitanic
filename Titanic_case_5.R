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
# Execution
echo_code <- TRUE       # Echo the code chunks
outresults <- FALSE      # Output the results (printing, etc)
saveresults <- TRUE     # save the results in Rda file for reuse in report


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
knitr::opts_chunk$set(echo = echo_code, fig.width = w.13, fig.asp = a.11 , fig.pos = 'H', fig.align = "center", fig.show = "hold", warning = showarning)



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
#'         * Make `Survived` , `Pclass` and `Embark` factors, 
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
#' Saving parameters {#saveparams}
#' ==================
#' 
#' Optionnally, the results of the analysis may be saved
#' 
#+ savechunkparams

savefname <- "analysis_results.rda"
# save in the data dir
savedir <- datadir



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
tb_age <- summary(tdf$Age)
if (outresults) pander(tb_age)
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

tb_agestat <- local({
        # all values
        tb_agestat <-table(tdf$Agestat)
        # all known values
        tb_agestat_k <- table(tdf[tdf$Agestat != 'Ukn', "Agestat"])[1:3]
        tb_agestat_kr <- prop.table(tb_agestat_k) # relative frequency
        
        tb_agestat_k <- rbind(tb_agestat_k, tb_agestat_kr) # bind absolute and relative frequency
        row.names(tb_agestat_k) <- c( "Effectif", "Fréquence")
        # retour
        list(
                all = tb_agestat,
                known = tb_agestat_k
        )

})


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

if (outresults) local({
        xxx <- sapply(p_agestat_by, print)
        ""
} ) # a revoir



#' 
#' Family members on board {#fmly}
#' ---------------
#' 

#+ family2, fig.cap="Family members on board", warning=FALSE
p_famly <- list(
        sibsp = ggplot(tdf) + geom_bar(aes(SibSp)),
        parch = ggplot(tdf) + geom_bar(aes(Parch)),
        famly = ggplot(tdf) + 
                geom_bar(aes(Famly, y = ..prop..), width = .5) +
                labs(x = "Family members on Board")
)


if (outresults) p_famly$famly

                

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


#' ### Passengers demographics by class

#+ p_age_by, fig.cap = "Passengers deographics characteristics by class"
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
                agebox = ggplot(tdf) + 
                        geom_boxplot(aes(Pclass, Age, 
                                         fill = Pclass), color = "grey50") + 
                        scale_x_discrete(limits = rev(levels(tdf$Pclass))) +
                        coord_flip() + ggnolegend,
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

if (outresults){ 
        p_age_by 
}



#' Deck
#' ----
        
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

if (outresults){ 
        p_deck
}

#' Figure `r .ref("fig:", "Deck") shows a very partial distribution of decks:
#' the deck of nearly 80% of the passengers is unknown. However, the third graph
#' reveals the strong link between the passencger class and the deck.



#' 
#' Embarkation port
#' ------------------
#' 

# table
tdf$Embarked <- factor(tdf$Embarked, levels = c("S", "C", "Q"))

tb_embark <- local({
        temb <- table(tdf$Embarked)
        # proportion table
        ptemb <- prop.table(temb)
        df <- rbind(frequency = temb , Rel.freq. = ptemb)
        
        ft <- round(prop.table(table(tdf$Pclass, tdf$Embarked), margin = 1),2)
        ft2 <- round(prop.table(ftable(tdf$Pclass, tdf$Sex, tdf$Embarked), margin = 1),2)
        list(df = df, ft = ft, ft2 = ft2)
})

if (outresults) {
        kable(tb_embark$df, digits=2, caption = "Embarkation port")
        kable(tb_embark$ft, digits=2, row.names = TRUE, caption = "Embarkation port by class")
        pander(tb_embark$ft2, digits=2, caption = "Embarkation port by class and sex")
        }


# na_emb <- sum(is.na(tdf$Embarked)) #  2 passengers have unknown embarkation port
# tdfx <- tdf[complete.cases(tdf), ] 

p_embark <- local({
        list(
                all = ggplot(tdf) + 
                        geom_bar(aes(Embarked, y=..prop.., group=1)),
                by_sex = ggplot(tdf) + 
                        geom_bar(aes(Embarked, y=..prop.., group=Sex)) +
                        facet_grid(.~Sex) + ggnolegend,
                by_class = ggplot(tdf) + geom_bar(aes(Embarked, y=..prop.., fill = Pclass,  group=Pclass)) + 
                        facet_grid(Pclass~.) +
                        ggnolegend,
                by_sex_class = ggplot(tdf) + 
                        geom_bar(aes(Embarked, y=..prop.., 
                                     fill = Pclass, group=Pclass)) +
                        facet_grid(Pclass~Sex) + ggnolegend
        )
})

if (outresults){ 
        pander(tb_embark)
        p_embark
}



#' Figure `r .ref("fig:", "Embarked")` shows that Southampton was the major
#' embarkation point (`r  100 * tb_embark$df[2, 1]`% of the passengers). However,
#' this is not as true for the first-class passengers, particularly for the
#' women of the first-class : only `r 100 * tb_embark$ft2[1, 1]`% of them embarked at
#' Southampton, and `r 100 * tb_embark$ft2[1, 2]`% embarked at Cherbourg



#' 
#' The Fare
#' --------
#' 
        
#+ fare, w=w.12,a=a.11, fig.cap="Fare by class and Sex"

tb_fare <- local( {
        fare_1 <-  summary( tdf[ tdf$Pclass == 1, "Fare"] )
        fare_2  <-  summary( tdf[ tdf$Pclass == 2, "Fare"] )
        fare_3  <-  summary( tdf[ tdf$Pclass == 3, "Fare"] )
        tab <- rbind (t(fare_1), 
               t(fare_2),
               t(fare_3))
        row.names(tab) <- c(1, 2, 3)
        tab
})

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
                        facet_grid(Pclass ~.) + ggnolegend,
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
# t_fare[1,4]

if (outresults) {
        kable(tb_fare, row.names = TRUE)
        p_fare
}


#'  
#' survival
#' ============
#' 



#' ### overall

#+ survival, fig.cap="Overall survival"
tb_survived <- prop.table(table(tdf$Survived))

if (outresults) pander(tb_survived, digits=2, 
                       caption = "Suvival global distribution")

p_survived <- ggplot(tdf) + geom_bar(aes(Survived, y = ..prop.., group = 1))


if (outresults) p_survived



#' ### By category

#+ survival-by, fig.cap="Survival by sex or Passenger Class"
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
                class_sex_south = ggplot(tdf[tdf$Embarked == "S", ]) + 
                        geom_bar(aes(Survived, y = ..prop.., 
                                     group = Pclass, fill = Pclass))+ 
                        facet_grid(Pclass ~ Sex)+
                        labs(title = "Emb. Southampton") + ggnolegend,
                class_sex_cherbourg = ggplot(tdf[tdf$Embarked == "C", ]) + 
                        geom_bar(aes(Survived, y = ..prop.., 
                                     group = Pclass, fill = Pclass))+ 
                        facet_grid(Pclass ~ Sex)+
                        labs(title = "Emb. Cherbourg") + ggnolegend,
                class_sex_queenstown = ggplot(tdf[tdf$Embarked == "Q", ]) + 
                        geom_bar(aes(Survived, y = ..prop.., 
                                     group = Pclass, fill = Pclass))+ 
                        facet_grid(Pclass ~ Sex)+
                        labs(title = "Emb. Queenstown") + ggnolegend
                
                
        )
})

if (outresults) {p_survived_by}


#+ survival-by4, fig.cap="Age by Survival  and Class", warning=FALSE

# not interesting ??
p_age_survived <- local({
        nc <- nclass.FD(tdf$Age[!is.na(tdf$Age)])
 
        age_all <- ggplot(tdf) + 
                geom_histogram(aes(Age, y= ..density.., fill = Pclass), bins = nc) +
                facet_grid(Pclass ~ Sex) +
                labs(title= "All") + ggnolegend
        
        
        age_survivors <- ggplot(tdf[tdf$Survived == "Yes", ]) + 
                geom_histogram(aes(Age, y= ..density.., fill = Pclass), bins = nc) +
                facet_grid(Pclass ~ Sex) +
                labs(title= "Survivors") + ggnolegend
        
        age_non_survivors <- ggplot(tdf[tdf$Survived == "No", ]) + 
                geom_histogram(aes(Age, y= ..density.., fill = Pclass), bins = nc) +
                facet_grid(Pclass ~ Sex) +
                labs(title= "Non-Survivors") + ggnolegend
        # return
        list(age_all = age_all, 
             age_survivors = age_survivors,
             age_non_survivors = age_non_survivors
             )
        
})

if (outresults) {p_age_survived}



#' * Survival, Class and Agestat +  sex


# tb_survived_by_class_sex_age 

#+ panderthings, result = "asis"
survived_age_sex_class_f_p <-  local({
        # utility to reorganize these flat tables
        modifytable <- function(ft) {
                rtot <- rowSums(ft, 2) # compute row totals
                ft <- round(prop.table(ft, 1), 2) * 100 # compute %survived
                ft[,1] <- rtot
                attr(ft, which = "col.vars") <- list(c("Number", "% Survived")) #substitute columns
                ft
        }
        # flat table suvived class * sex * agestat
        agesex_class_f <- ftable(tdf$Pclass,tdf$Sex,tdf$Agestat, tdf$Survived)
        agesex_class_f_p <- modifytable(agesex_class_f)
        
        # flat table suvived class * agestat (no sex)
        age_class_f <- ftable(tdf$Pclass,tdf$Agestat, tdf$Survived)
        age_class_f_p <- modifytable(age_class_f)
        
        list(surv_class_sex_age = agesex_class_f_p,
             surv_class_age = age_class_f_p)
})

if (outresults) {pander(survived_age_sex_class_f_p, digits=2, 
                        caption = "Survival by  Class, Sex , Age ")
}


# plots survival-by

#+ plots_survival_by, w = w.11, fig.asp = 1.5,
pl_survived_by <-  local({
        list(
                class_age_sex_all = ggplot(tdf) +
                        geom_bar(aes(Survived, y = ..prop..,
                                     group = Pclass, fill = Pclass))+
                        facet_grid(Pclass ~ Agestat) +
                        labs(title = "Sex = All") + ggnolegend , 
                class_age_sex_female = ggplot(tdf[tdf$Sex == "female" ,]) +
                        geom_bar(aes(Survived, y = ..prop..,
                                     group = Pclass, fill = Pclass))+
                        facet_grid(Pclass ~ Agestat) +
                        labs(title = "Sex ='female'") + ggnolegend,
                class_age_sex_male = ggplot(tdf[tdf$Sex == "male" ,]) +
                        geom_bar(aes(Survived, y = ..prop..,
                                     group = Pclass, fill = Pclass))+
                        facet_grid(Pclass ~ Agestat) +
                        labs(title = "Sex = 'male'") + ggnolegend,
                Deck_age_class = ggplot(tdf) +
                        geom_bar(aes(Survived, y = ..prop..,
                                     group = Deck, fill = Pclass)) +
                        facet_grid(Deck ~ Agestat) +
                        labs(title = "by Deck") + ggnolegend,
                Deck_age_sex = ggplot(tdf) +
                        geom_bar(aes(Survived, y = ..prop..,
                                     group = Deck, fill = Sex)) +
                        facet_grid(Deck ~ Agestat)+
                        labs(Title = "by Deck") + ggnolegend
        )
})

if (outresults) {
        pl_survived_by
}

#' -------------------------------------------------------------------------------------------------------
#' 
#' Saving results {#saveresults}
#' ==================
#' 
#' Optionnally, the results of the analysis may be saved for the final report phase
#' 



#+ savechunk

timesave <- format(Sys.Date(), '%d %B %Y')

listsave = c(
        # time of save
        "timesave",
        # main data 
        "tdf", 
        # sex
        "tb_sex", "p_sex" ,
        #age, 
        #agestat
        "tb_age", "tb_agestat",
        "p_age", "p_age_sex","p_agestat_by",
        # famly
        "p_famly",
        # pclass
        "tb_pclass", "p_pclass", 
        "p_age_by", "p_deck",
        # embark
        "tb_embark", "p_embark",
        "tb_fare", "p_fare",
        ### survival
        "tb_survived", "p_survived",
        "tb_survived_by", "p_survived_by",
        "p_age_survived",
        "survived_age_sex_class_f_p",
        "pl_survived_by"
)

if (saveresults) save(list = listsave, 
                      file = file.path(savedir, savefname) )



