
library(softImpute)
library(randomForest)
library(ranger)
library(dplyr)
library(tidyverse)
library(reshape2)


OSR2 <- function(predictions, train, test) {
  SSE <- sum((test - predictions)^2)
  SST <- sum((test - mean(train))^2)
  r2 <- 1 - SSE/SST
  return(r2)
  
}


music <- read.csv("MusicRatings.csv")
user <- read.csv("Users.csv")
song <- read.csv("Songs.csv")

#how many songs are in this dataset
length(unique(song$songID))
#how many users
length(unique(user$userID))
#the range of values
range(music$rating)

music <- left_join(music, user, by = "userID")
music <- left_join(music, song, by = "songID")
music$year <- as.factor(music$year)
song$year <- as.factor(song$year)


set.seed(345)
# split training into real training and validation set
split.index <- sample(nrow(music), 0.92*nrow(music))
train <- music[split.index,]
test <- music[-split.index,]

valA.split <- sample(nrow(train), (4/92)*nrow(train))
valA <- train[valA.split,]
train <- train[-valA.split,]

valB.split <- sample(nrow(train), (4/88)*nrow(train))
valB <- train[valB.split,]
train <- train[-valB.split,]
# incomplete training set ratings matrix.
incomplete.train <- Incomplete(train$userID, train$songID, train$rating)



### B
#	how many total parameters are included in model (1)? 
length(unique(train$userID))+length(unique(train$songID))

#	how many observations do we have to train the model with? 
length(train$rating)

#	fit the form of model (1)
set.seed(345)
incomplete.train.centered <- biScale(incomplete.train, maxit = 1000, row.scale = FALSE, col.scale = FALSE)

#	summary(incomplete.train.centered)
#	alpha.i denotes user affinity for rating songs and beta.j denotes popularity of the songs 
alpha <- attr(incomplete.train.centered, "biScale:row")$center
beta <- attr(incomplete.train.centered, "biScale:column")$center

#	(ii)
song <- mutate(song, popularity = beta)
song.ranking <- arrange(song, desc(song$popularity))
head(song.ranking)


# (iii)
user <- mutate(user, usertendency = alpha)
user.ranking <- arrange(user, desc(user$usertendency))
head(user.ranking)

# (iv)

test.temp <- left_join(test,user, by = "userID")
test.temp <- left_join(test.temp,song, by = c("songID" = "songID", "songName" = "songName", "year" = "year",
                                              "artist" = "artist", "genre" = "genre"))
test.temp	<-	mutate(test.temp,	predict.mod1	=	test.temp$usertendency	+
                      test.temp$popularity)
head(test.temp)

#MAE
mean(abs(test.temp$predict.mod1 - test.temp$rating))/(max(music$rating)-min(music$rating))
#RMSE
sqrt(mean((test.temp$predict.mod1 - test.temp$rating)^2))/(max(music$rating)-min(music$rating))
#OSR^2
OSR2(test.temp$predict.mod1, train$rating, test$rating)


### C
#	how many total parameters are included in model?
(length(unique(train$userID))+length(unique(train$songID))) # including alpha and beta

#	How many observations do we have to train the model with?
length(train$rating)

# (ii)
mae <- rep(NA,10)

for (i in 1:10) {
  set.seed(345)
  print(str_c("now is rank = ", i))
  mod.temp <- softImpute(incomplete.train.centered, rank.max = i, lambda = 0, 
                         maxit = 1000)
  pred.temp <- impute(mod.temp, valA$userID, valA$songID)
  mae[i] <- mean(abs(pred.temp - valA$rating))
  
}
mae.valA.df <- data.frame(ranks = seq(1,10), averageMAE = mae)


# Use a plot to justify and explain the value of k

ggplot(mae.valA.df, aes(x = ranks, y = averageMAE)) + geom_point() + ylab("Validation MAE") + xlab("Number of Archetype (k)") +
  theme_bw() + theme(axis.title=element_text(size=18), axis.text=element_text(size=18))


#	(iii) for k = 4 
set.seed(345)

mod.CF <- softImpute(incomplete.train.centered, rank.max = 4, lambda = 0, maxit = 10000) 
pred.CF <- impute(mod.CF, test$userID, test$songID)
mean(abs(pred.CF - test$rating))/(max(music$rating)-min(music$rating))
sqrt(mean((pred.CF - test$rating)^2))/(max(music$rating)-min(music$rating))
OSR2(pred.CF, train$rating, test$rating)


###	D
# (i)
# linear regression
mod.line <- lm(rating ~ genre + year, data = train) 
summary(mod.line)
pred.line <- predict(mod.line, newdata = test)
               
mean(abs(pred.line - test$rating))/(max(music$rating)-min(music$rating))
sqrt(mean((pred.line - test$rating)^2))/(max(music$rating)-min(music$rating))
OSR2(pred.line, train$rating, test$rating)
summary(mod.line)

#	random forest 
set.seed(345)
mod.rf <- ranger(rating ~ genre + year, 
                 data = train,
                 mtry = 1,
                 num.trees = 1000,
                 verbose = TRUE)
 
pred.rf <- predict(mod.rf, data = test)
pred.rf <- pred.rf$predictions
mean(abs(pred.rf - test$rating))/(max(music$rating)-min(music$rating))
sqrt(mean((pred.rf - test$rating)^2))/(max(music$rating)-min(music$rating))
OSR2(pred.rf, train$rating, test$rating)
            
#	(ii)
#	blend the models
              
valB.pred.CF <- impute(mod.CF, valB$userID, valB$songID) 
valB.pred.line <- predict(mod.line, newdata = valB) 
valB.pred.rf <- predict(mod.rf, data = valB) 
valB.pred.rf <- valB.pred.rf$predictions

valB.pseudo.feature <- data.frame(rating = valB$rating, 
                                  CF_predict = valB.pred.CF, 
                                  linear_predict = valB.pred.line,          
                                  RF_predict = valB.pred.rf)
mod.blend.final <- lm(rating ~ . -1, data = valB.pseudo.feature)
summary(mod.blend.final)
               
# test set assessment : MAE, RMSE, OSR2
test.pred.CF <- impute(mod.CF, test$userID, test$songID) 
test.pred.line <- predict(mod.line, newdata = test) 
test.pred.rf <- predict(mod.rf, data = test) 
test.pred.rf <- test.pred.rf$predictions

test.pseudo.feature <- data.frame(rating = test$rating, CF_predict = test.pred.CF, linear_predict = test.pred.line,
                                  RF_predict = test.pred.rf)

test.pred.blend <- predict(mod.blend.final, newdata = test.pseudo.feature)

mean(abs(test.pred.blend - test$rating))/(max(music$rating)-min(music$rating))
sqrt(mean((test.pred.blend - test$rating)^2))/(max(music$rating)-min(music$rating))
OSR2(test.pred.blend, train$rating, test$rating)

