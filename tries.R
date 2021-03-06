
# update.packages()

# install.packages(c( "pander", "caret", "tidyverse" , "ggplot2" ))
library(knitr)
library(pander)
# library(caret)
library(ggplot2)
library(dplyr)


tick <- tdf %>%
        group_by(Ticket) %>%
        mutate(Fare1 = mean(Fare), numtick = n()) %>%
        mutate(Pfare = Fare / numtick) %>% # Pfare = fare per person using numtick
        # mutate(Dfare = Fare - Pfare) %>% 
        mutate(Pfare2 = Fare / (Famly + 1)) %>% # Pfare = fare per person using Famly
        mutate(Pfare3 = ifelse(numtick >= Famly + 1, Pfare, Pfare2)) %>%
        arrange(numtick, Ticket)
# 
# tick <- tdf %>%
#         group_by(Ticket) %>%
#         mutate( numtick = n(), Fare1 = sum(Fare),) %>%
#         mutate(Dfare = Fare - Fare1)
with(data = tick[tick$Pclass != 1,], 
     table(Famly,numtick))

as.data.frame(tick[tick$Famly ==0 & tick$numtick == 3, c("Name", "Ticket", "Sex.Pclass", "Fare") ])

as.data.frame(tick[tick$Ticket == 248727 , c("Name", "Ticket", "Famly", "Sex.Pclass", "Fare") ])


# Fare, Pfare, Pfare2
nc <- nclass.FD(tick$Pfare3)
ggplot(tick, aes(x = Fare)) + geom_histogram(bins = nc) + facet_grid(Pclass ~ .)
ggplot(tick, aes(x = Pfare)) + geom_histogram(bins = nc) + facet_grid(Pclass ~ .)
ggplot(tick, aes(x = Pfare2)) + geom_histogram(bins = nc) + facet_grid(Pclass ~ .)
ggplot(tick, aes(x = Pfare3)) + geom_histogram(bins = nc)+ facet_grid(Pclass ~ .)

ggplot(tick, aes(x = numtick)) + geom_bar()
ggplot(tick, aes(x = numtick, y = Famly)) + 
        geom_jitter(height = 0, width = 0.5, alpha = .2 ) + 
        geom_smooth() + geom_smooth(method = "lm")

ggplot(tick, aes(x = numtick, y = Fare)) + 
        geom_jitter(height = 0, width = 0.5, alpha = .2 ) + 
        geom_smooth() + geom_smooth(method = "lm")

ggplot(tick, aes(x = numtick, y = Pfare)) + 
        geom_jitter(height = 0, width = 0.5, alpha = .2 ) + 
        geom_smooth() + geom_smooth(method = "lm")

ggplot(tick, aes(x = numtick, y = Pfare2)) + 
        geom_jitter(height = 0, alpha = .2 ) + 
        geom_smooth() + geom_smooth(method = "lm")

ggplot(tick, aes(x = numtick, y = Pfare3)) + 
        geom_jitter(height = 0, alpha = .2 ) + 
        geom_smooth() + geom_smooth(method = "lm")

with(tick, cor(x = numtick, Famly))

ggplot(tick, aes(x = Pclass, y = Pfare)) + geom_boxplot()
ggplot(tick, aes(x = Pclass, y = Pfare2)) + geom_boxplot()
ggplot(tick, aes(x = Pclass, y = Pfare3)) + geom_boxplot()

ggplot(tick, aes(x = Embarked, y = Pfare)) + geom_boxplot() + facet_grid(. ~ Pclass, margins = TRUE )
ggplot(tick, aes(x = Embarked, y = Pfare2)) + geom_boxplot() + facet_grid(. ~ Pclass, margins = TRUE )
ggplot(tick, aes(x = Embarked, y = Pfare3)) + geom_boxplot() + facet_grid(. ~ Pclass, margins = TRUE )

ggplot(tick, aes(x = Deck, y = Pfare2)) + geom_boxplot() + facet_grid(. ~ Pclass, margins = TRUE )
ggplot(tick, aes(x = Deck, y = Pfare3)) + geom_boxplot() + facet_grid(. ~ Pclass, margins = TRUE )

ggplot(tick, aes(x = numtick, y = Pfare3)) + geom_boxplot(aes(group = numtick )) # + facet_grid(. ~ Pclass, margins = TRUE )

ggplot(data = tick, aes(x=Survived)) + 
        geom_bar(aes( y = ..prop.., 
                      group = numtick)) + facet_grid(. ~ numtick)

ggplot(data = tick, aes(x=Survived)) + 
        geom_bar(aes( y = ..prop.., 
                      group = Famly)) + 
        facet_grid(. ~ Famly)
#  family size==class
ggplot(data = tick, aes(x=Pclass)) + 
        geom_bar(aes( y = ..prop.., 
                      group = Famly)) + facet_grid(. ~ Famly, margins = TRUE) +
        labs(title = " Family Size and Class: proportion of each class")

ggplot(data = tick, aes(x=Pclass)) + 
        geom_bar(aes( #y = ..prop.., 
                      group = Famly)) + facet_grid(. ~ Famly, margins = TRUE) +
        labs(title = " Family Size and Class: counts of classes")


ggplot(data = tick, aes(x=Famly)) +
        geom_bar(aes( y = ..prop.., 
                group = Pclass)) + facet_grid(. ~ Pclass, margins = TRUE)

tick <- tick %>%
        mutate(Famly2 = ifelse(Famly + 1 >= numtick, Famly + 1, numtick))

tick <- tick %>%
        mutate(Famsize = factor (ifelse( Famly2 == 1, 
                                        "1single", 
                                        ifelse(Famly2 <= 4, "2Smallfamily", "3Largefamily"))))

# Family size by class
ggplot(data = tick, aes(x=Famsize)) +
        geom_bar(aes( y = ..prop.., 
                      group = Pclass)) + facet_grid(Sex ~ Pclass, margins = TRUE)

# Family size by sex and class
ggplot(data = tick, aes(x=Famsize)) +
        geom_bar(aes( y = ..prop.., 
                      group = Pclass)) + facet_grid(Sex ~ Pclass, margins = TRUE)


ggplot(data = tick, aes(x=Pclass)) + 
        geom_bar(aes( y = ..prop.., 
                      group = numtick)) + facet_grid(. ~ numtick, margins = TRUE)    




#--------------------------------------------------------------------------------


moklist <- function(...) { # arguments may be gg_graphs or lists of graphs
        test <- function(obj) {"gg" %in% class(obj)}
        lisobj <- list(...)
        
        unlist(
                lapply(lisobj, 
                       FUN = function(x){
                               if (test(x)){
                                       list(x)
                               }else{x}
                       }
                ),
                recursive = FALSE
        )
}



rpl <- list(p_age, p_age_sex, p_agestat_by)
length(rpl)
sapply(rpl, function(obj) {"gg" %in% class(obj)})

pl <- moklist(p_age, p_age_sex, p_agestat_by)
length(pl)
sapply(pl, function(obj) {"gg" %in% class(obj)})






