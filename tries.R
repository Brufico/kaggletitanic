
update.packages()

# install.packages(c( "pander", "caret", "tidyverse" , "ggplot2" ))
library(knitr)
library(pander)
library(caret)
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
table(Famly,numtick)
)


# Fare, Pfare, Pfare2
ggplot(tick, aes(x = Fare)) + geom_histogram()
ggplot(tick, aes(x = Pfare)) + geom_histogram()
ggplot(tick, aes(x = Pfare2)) + geom_histogram()

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

with(tick, cor(x = numtick, Famly))

ggplot(tick, aes(x = Pclass, y = Pfare)) + geom_boxplot()
ggplot(tick, aes(x = Pclass, y = Pfare2)) + geom_boxplot()
ggplot(tick, aes(x = Pclass, y = Pfare3)) + geom_boxplot()

ggplot(tick, aes(x = Embarked, y = Pfare)) + geom_boxplot() + facet_grid(. ~ Pclass, margins = TRUE )
ggplot(tick, aes(x = Embarked, y = Pfare2)) + geom_boxplot() + facet_grid(. ~ Pclass, margins = TRUE )
ggplot(tick, aes(x = Embarked, y = Pfare3)) + geom_boxplot() + facet_grid(. ~ Pclass, margins = TRUE )

ggplot(tick, aes(x = Deck, y = Pfare2)) + geom_boxplot() + facet_grid(. ~ Pclass, margins = TRUE )
ggplot(tick, aes(x = Deck, y = Pfare3)) + geom_boxplot() + facet_grid(. ~ Pclass, margins = TRUE )

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
