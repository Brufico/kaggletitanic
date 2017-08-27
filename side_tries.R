
unique(tdf$Embarked)
# 
# naem <- which(is.na(tdf$Embarked))
# tdf$Embarked[naem] <- "Unknown"
# 
# # 
tdf$Embarked[which(tdf$Embarked == "")] <- "Unknown"
# tdf$Embarked[which(is.na(tdf$Embarked))] <- "Unknown"

tdf$Embarked <-  factor(tdf$Embarked)
levels(tdf$Embarked)


pp1 <- preProcess(tdf,
           k=5,
           knnSummary = mean)

# =====================================================================================

xdf <-  data.frame( one = 1:3, two = 5 * (3:1), three = 10 * 0:2)
xdf

lco <- list( one_two = xdf$one * xdf$two,
      one_three = xdf$one * xdf$three)

ldf <- as.data.frame(lco)
ldf


# ===================== again

# data
xdf <-  data.frame( one = 1:3, two = 5 * (3:1), three = 10 * 0:2)
xdf

# manipulate to make dataframe of products
lco <- list( xdf$one * xdf$two,
             xdf$one * xdf$three)

lnam <- c("one_two", "one_three")

ldf <- as.data.frame(lco)
ldf

colnames(ldf) <- lnam

ldf


# function to do the same job
makeprods <- function(basevar, # the name (string) of the variable to multiply
                      prodstodo, # vector of names of the columns by which to multiply  
                      dframe) { # the dataframe containing the vars
        listp <- lapply(prodstodo,
        1                FUN = function(factname){
                                dframe[[basevar]] * dframe[[factname]]
                        } )
        vnames <- sapply(prodstodo,
                         FUN=function(factname){
                                 paste(basevar, factname, sep = "_")
                         } )
        dftp <- as.data.frame(listp)
        colnames(dftp) <- vnames
        dftp # return the dataframe of products
}

P <- makeprods("one", c("two", "three"), dframe=xdf )



A <- data.frame(dummy = numeric(3))
B <- cbind(A, P)
B[-1]


allprods <- function(vbv, vprods, dframe){
        # init dummy column
        A <- data.frame(dummy = numeric(nrow(dframe)))
        P <- lapply(1:length(vbv),
                    FUN = function(idx){
                            makeprods(vbv[idx], 
                                      vprods[(idx+1):length(vprods)],
                                      dframe = dframe)
                    } )
        # combine all columns
        A <- Reduce(f = function(dfx, dfy){cbind(dfx, dfy)},
               x = P,
               init = A)
        A[-1] # jettison dummy col and return
}

# test
allprods(colnames(xdf)[-3], colnames(xdf), xdf)






### Dummy vars new try ========================================================

# Sex
tdf$Sex <- factor(tdf$Sex)

# Representing the factors with dummies
dummies <- dummyVars(Survived ~ ., 
                     data = tdf[c("Survived", "Pclass", "Sex", "Embarked", "Deck" )],
                     fullRank = TRUE)

dumvars <- predict(dummies, newdata = tdf[c("Survived", "Pclass", "Sex", "Embarked", "Deck" )])


# dumvars <- dumvars[, c(1, 2, 4, 6, 7, 8, )]
dumvars <- as.data.frame(dumvars)
colnames(dumvars)
str(dumvars)

# add dummies to the data
tdf <- cbind(tdf, dumvars)
# names(tdf)


### missing ========================================================

var(tdf$Embarked.Ukn)
var(trainingset$Embarked.Ukn)


