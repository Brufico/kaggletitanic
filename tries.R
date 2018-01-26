
update.packages()

install.packages(c( "pander", "caret", "tidyverse" , "ggplot2" ))
library(knitr)
library(pander)
library(caret)
library(ggplot2)
library(dplyr)


tick <- tdf %>%
        group_by(Ticket) %>%
        mutate(Fare1 = mean(Fare), numtick = n()) %>%
        mutate(Dfare = Fare - Fare1) %>%
        arrange(numtick, Ticket)
# 
# tick <- tdf %>%
#         group_by(Ticket) %>%
#         mutate( numtick = n(), Fare1 = sum(Fare),) %>%
#         mutate(Dfare = Fare - Fare1)


ggplot(tick, aes(x = Dfare)) + geom_bar()

ggplot(tick, aes(x = numtick)) + geom_bar()
ggplot(tick, aes(x = numtick, y = Famly)) + 
        geom_jitter(height = 0, alpha = .2 ) + 
        geom_smooth() + geom_smooth(method = "lm")

ggplot(tick, aes(x = numtick, y = Fare)) + 
        geom_jitter(height = 0, alpha = .2 ) + 
        geom_smooth() + geom_smooth(method = "lm")

ggplot(tick, aes(x = numtick, y = Fare1)) + 
        geom_jitter(height = 0, alpha = .2 ) + 
        geom_smooth() + geom_smooth(method = "lm")

with(tick, cor(x = numtick, Famly))

ggplot(tick, aes(x = Embarked, y = Dfare)) + geom_boxplot() + facet_grid(. ~ Pclass, margins = TRUE )




ggplot(tick, aes(x = numtick, y = Fare1)) + geom_boxplot(aes(group = numtick )) # + facet_grid(. ~ Pclass, margins = TRUE )

ggplot(data = tick, aes(x=Survived)) + 
        geom_bar(aes( y = ..prop.., 
                      group = numtick)) + facet_grid(. ~ numtick)

ggplot(data = tick, aes(x=Survived)) + 
        geom_bar(aes( y = ..prop.., 
                      group = Famly)) + 
        facet_grid(. ~ Famly)

ggplot(data = tick, aes(x=Pclass)) + 
        geom_bar(aes( y = ..prop.., 
                      group = numtick)) + facet_grid(. ~ Famly, margins = TRUE)

ggplot(data = tick, aes(x=Pclass)) + 
        geom_bar(aes( y = ..prop.., 
                      group = numtick)) + facet_grid(. ~ numtick, margins = TRUE)    
