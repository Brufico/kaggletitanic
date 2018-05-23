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
bwtheme <- TRUE
specialpalette <- FALSE
showarning <- FALSE
datmis <- "Ukn"


#+libs_prep, echo =FALSE, warning=FALSE, message=FALSE, results='hide'
## Required packages (install before running): "caret", "ggplot2", pander
library(knitr)
library(pander)
library(caret)
library(ggplot2)
library(dplyr)


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
pander(head(tdf), caption = tabcap("A glimpse of the data"))



