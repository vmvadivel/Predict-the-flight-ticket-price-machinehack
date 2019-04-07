
#install packages which isn't already there!
#if (!require("pacman")) install.packages("pacman")
#pacman::p_load(knitr, ggplot2, plyr, dplyr, corrplot, caret, gridExtra, scales, Rmisc, ggrepel,
#randomForest, psych, xgboost)

library(knitr)
library(ggplot2)
library(plyr)
library(dplyr)
library(corrplot)
library(caret)
library(gridExtra)
library(scales)
library(Rmisc)
library(ggrepel)
library(randomForest)
library(psych)
library(xgboost)

#set the working directory
#setwd('E:/MachineHack/Flight_Ticket_Participant_Datasets')
#getwd()

train <- read.csv("Data_Train.csv", stringsAsFactors = F)

row.has.na <- apply(train, 1, function(x){any(is.na(x))})
na.omit(train)

# Feature Engineering

train$Date_of_Journey <- as.Date(train$Date_of_Journey,'%d/%m/%Y')
train$Month_of_Journey <- format(as.Date(train$Date_of_Journey), "%m") 
train$Day_of_Journey <- format(as.Date(train$Date_of_Journey), "%d") 
train$Day_of_week <- weekdays(train$Date_of_Journey)
train$WeekDayNumber <- as.numeric(format(train$Date_of_Journey,"%w"))
train$Weekend <- as.numeric(grepl("S.+",weekdays(train$Date_of_Journey)))
train$DaysLeftUntilDeparture <- as.numeric(as.Date(train$Date_of_Journey,"%d/%m/%Y") - as.Date(Sys.Date(),"%d/%m/%Y"))

time2 <- format(as.POSIXct(train$Dep_Time, tz = "" , format = "%H: %M"), "%H")
df <- data.frame(train$Dep_Time, time2 = as.numeric(time2))

train$Time_of_departure <- ifelse(df$time2 >= 05 & df$time2 <= 11, "Morning",
            ifelse(df$time2 > 11 & df$time2 <= 16, "Afternoon",
            ifelse(df$time2 > 16 & df$time2 <= 19, "Evening", "Night")))

# Get numbers next to hours and minutes
hour_minute <- sub("(\\d+)h (\\d+)m", "\\1-\\2", train$Duration)

train[c("hour", "minutes")] <- t(sapply(strsplit(hour_minute, "-"), 
function(x) {
  if (length(x) == 2) x 
  else if (endsWith(x, "h")) c(sub("h", "", x), 0)
  else c(0, sub("m", "", x))
}))

train$hour <- as.numeric(train$hour)
train$minutes <- as.numeric(train$minutes)
train$duration_in_minutes <- (60 * train$hour) + train$minutes

df2 <- within(train ,{
  posb <- as.POSIXlt(Dep_Time,format="%H:%M")
  hours <- posb$hour
  mins <- posb$min
  dates <- format(posb, "%x")
  posb <- NULL  # cleanup
})

train$dep_time_hours <- as.numeric(df2$hours)
train$dep_time_mins <- as.numeric(df2$mins)

df2 <- within(train ,{
  posb <- as.POSIXlt(Arrival_Time,format="%H:%M")
  hours <- posb$hour
  mins <- posb$min
  dates <- format(posb, "%x")
  posb <- NULL  # cleanup
})

train$arrival_time_hours <- as.numeric(df2$hours)
train$arrival_time_mins <- as.numeric(df2$mins)

str(train)

#Test data
test <- read.csv("Test_set.csv", stringsAsFactors = F)
test$Price <- NA

test$Date_of_Journey <- as.Date(test$Date_of_Journey,'%d/%m/%Y')
test$Month_of_Journey <- format(as.Date(test$Date_of_Journey), "%m") 
test$Day_of_Journey <- format(as.Date(test$Date_of_Journey), "%d") 
test$Day_of_week <- weekdays(test$Date_of_Journey)
test$WeekDayNumber <- as.numeric(format(test$Date_of_Journey,"%w"))
test$Weekend <- as.numeric(grepl("S.+",weekdays(test$Date_of_Journey)))

time2 <- format(as.POSIXct(test$Dep_Time, tz = "" , format = "%H: %M"), "%H")
df <- data.frame(test$Dep_Time, time2 = as.numeric(time2))

test$Time_of_departure <- ifelse(df$time2 >= 05 & df$time2 <= 11, "Morning",
            ifelse(df$time2 > 11 & df$time2 <= 16, "Afternoon",
            ifelse(df$time2 > 16 & df$time2 <= 19, "Evening", "Night")))

test$DaysLeftUntilDeparture <- as.numeric(test$Date_of_Journey - as.Date(Sys.Date(),"%d/%m/%Y"))

# Get numbers next to hours and minutes
hour_minute <- sub("(\\d+)h (\\d+)m", "\\1-\\2", test$Duration)

test[c("hour", "minutes")] <- t(sapply(strsplit(hour_minute, "-"), 
function(x) {
  if (length(x) == 2) x 
  else if (endsWith(x, "h")) c(sub("h", "", x), 0)
  else c(0, sub("m", "", x))
}))

test$hour <- as.numeric(test$hour)
test$minutes <- as.numeric(test$minutes)
test$duration_in_minutes <- (60 * test$hour) + test$minutes

df2 <- within(test ,{
  posb <- as.POSIXlt(Dep_Time,format="%H:%M")
  hours <- posb$hour
  mins <- posb$min
  dates <- format(posb, "%x")
  posb <- NULL  # cleanup
})

test$dep_time_hours <- as.numeric(df2$hours)
test$dep_time_mins <- as.numeric(df2$mins)

df2 <- within(test ,{
  posb <- as.POSIXlt(Arrival_Time,format="%H:%M")
  hours <- posb$hour
  mins <- posb$min
  dates <- format(posb, "%x")
  posb <- NULL  # cleanup
})

test$arrival_time_hours <- as.numeric(df2$hours)
test$arrival_time_mins <- as.numeric(df2$mins)

# Drop unwanted columns
train <- subset(train, select = -c(Arrival_Time, Dep_Time, Duration, Date_of_Journey))
test <- subset(test, select = -c(Arrival_Time, Dep_Time, Duration, Date_of_Journey))

str(train)
str(test)
head(test)

all <- rbind(train, test)
#dim(all)
#summary(all$Price)

ggplot(data=all[!is.na(all$Price),], aes(x=Price)) +
        geom_histogram(fill="blue", binwidth = 10000) +
        scale_x_continuous(breaks= seq(0, 8000, by=20000), labels = comma)

numericVars <- which(sapply(all, is.numeric)) #index vector numeric variables
numericVarNames <- names(numericVars) #saving names vector for use later on
cat('There are', length(numericVars), 'numeric variables')

all_numVar <- all[, numericVars]
cor_numVar <- cor(all_numVar, use="pairwise.complete.obs") #correlations of all numeric variables

NAcol <- which(colSums(is.na(all)) > 0)
sort(colSums(sapply(all[NAcol], is.na)), decreasing = TRUE)

cat('There are', length(NAcol), 'columns with missing values')

Charcol <- names(all[,sapply(all, is.character)])
Charcol

cat('There are', length(Charcol), 'remaining columns with character values')

all$Airline <- as.factor(all$Airline)
table(all$Airline)
sum(table(all$Airline))

all$Day_of_Journey <- as.factor(all$Day_of_Journey)
table(all$Day_of_Journey)
#sum(table(all$Day_of_Journey))

all$Month_of_Journey <- as.factor(all$Month_of_Journey)
table(all$Month_of_Journey)
#sum(table(all$Month_of_Journey))

all$Source <- as.factor(all$Source)
table(all$Source)
sum(table(all$Source))

all$Destination <- as.factor(all$Destination)
table(all$Destination)
sum(table(all$Destination))

all$Route <- as.factor(all$Route)
table(all$Route)
sum(table(all$Route))

all$Total_Stops <- as.factor(all$Total_Stops)
table(all$Total_Stops)
sum(table(all$Total_Stops))

all$Additional_Info <- as.factor(all$Additional_Info)
table(all$Additional_Info)
sum(table(all$Additional_Info))

all$Time_of_departure <- as.factor(all$Time_of_departure)
table(all$Time_of_departure)
sum(table(all$Time_of_departure))

all$Day_of_week <- as.factor(all$Day_of_week)
table(all$Day_of_week)
sum(table(all$Day_of_week))

numericVars <- which(sapply(all, is.numeric)) #index vector numeric variables
factorVars <- which(sapply(all, is.factor)) #index vector factor variables
cat('There are', length(numericVars), 'numeric variables, and', length(factorVars), 'categoric variables')

all_numVar <- all[, numericVars]
cor_numVar <- cor(all_numVar, use="pairwise.complete.obs")

#sort on decreasing correlations with Price
cor_sorted <- as.matrix(sort(cor_numVar[,'Price'], decreasing = TRUE))

#select only high corelations
CorHigh <- names(which(apply(cor_sorted, 1, function(x) abs(x)>0.5)))
cor_numVar <- cor_numVar[CorHigh, CorHigh]

corrplot.mixed(cor_numVar, tl.col="black", tl.pos = "lt", tl.cex = 0.7,cl.cex = .7, number.cex=.7)
str(all)

set.seed(2018)

numericVarNames <- numericVarNames[!(numericVarNames %in% c('Price'))] #numericVarNames was created before having done anything
numericVarNames <- append(numericVarNames, c('dep_time_hours', 
'dep_time_mins', 'arrival_time_hours', 'arrival_time_mins', 'Weekend', 'WeekDayNumber', 'DaysLeftUntilDeparture', 'duration_in_minutes'))

DFnumeric <- all[, names(all) %in% numericVarNames]

DFfactors <- all[, !(names(all) %in% numericVarNames)]
DFfactors <- DFfactors[, names(DFfactors) != 'Price']

cat('There are', length(DFnumeric), 'numeric variables, and', length(DFfactors), 'factor variables')

for(i in 1:ncol(DFnumeric)){
        if (abs(skew(DFnumeric[,i]))>0.8){
                DFnumeric[,i] <- log(DFnumeric[,i] +1)
        }
}

PreNum <- preProcess(DFnumeric, method=c("center", "scale"))
print(PreNum)

DFnorm <- predict(PreNum, DFnumeric)
dim(DFnorm)

DFdummies <- as.data.frame(model.matrix(~.-1, DFfactors))
dim(DFdummies)

#check if some values are absent in the test set
ZerocolTest <- which(colSums(DFdummies[(nrow(all[!is.na(all$Price),])+1):nrow(all),])==0)
colnames(DFdummies[ZerocolTest])

DFdummies <- DFdummies[,-ZerocolTest]

ZerocolTrain <- which(colSums(DFdummies[1:nrow(all[!is.na(all$Price),]),])==0)
colnames(DFdummies[ZerocolTrain])
DFdummies <- DFdummies[,-ZerocolTrain] 

fewOnes <- which(colSums(DFdummies[1:nrow(all[!is.na(all$Price),]),])<10)
colnames(DFdummies[fewOnes])

DFdummies <- DFdummies[,-fewOnes] #removing predictors
dim(DFdummies)

combined <- cbind(DFnorm, DFdummies)

skew(all$Price)

qqnorm(all$Price)
qqline(all$Price)

all$Price <- log(all$Price) #default is the natural logarithm, "+1" is not necessary as there are no 0's
skew(all$Price)

qqnorm(all$Price)
qqline(all$Price)

train1 <- combined[!is.na(all$Price),]
test1 <- combined[is.na(all$Price),]

##########XG_BOOST
my_control <-trainControl(method="cv", number=5)

xgb_grid = expand.grid(
nrounds = 1000,
eta = c(0.1, 0.05, 0.01, 0.02, 0.03, 0.04),
max_depth = c(2, 3, 4, 5, 6),
gamma = 0,
colsample_bytree=1,
min_child_weight=c(1, 2, 3, 4 ,5),
subsample=1
)

#xgb_caret <- train(x=train1, y=all$Price[!is.na(all$Price)], method='xgbTree', trControl= my_control, tuneGrid=xgb_grid) 
#xgb_caret$bestTune

label_train <- all$Price[!is.na(all$Price)]

# put our testing & training data into two seperates Dmatrixs objects
dtrain <- xgb.DMatrix(data = as.matrix(train1), label= label_train)
dtest <- xgb.DMatrix(data = as.matrix(test1))

default_param<-list(
        objective = "reg:linear",
        booster = "gbtree",
        eta=0.1, #0.01
        gamma=0,
        max_depth=6,
        min_child_weight=2, #3
        subsample=1,
        colsample_bytree=1
)

xgbcv <- xgb.cv( params = default_param, data = dtrain, nrounds = 35000, nfold = 10, showsd = T, stratified = T, print_every_n = 40, early_stopping_rounds = 10, maximize = F)
xgb_mod <- xgb.train(data = dtrain, params=default_param, nrounds = 679)
XGBpred <- predict(xgb_mod, dtest)
predictions_XGB <- exp(XGBpred) 

sub_avg <- data.frame(Price = (predictions_XGB))
write.csv(sub_avg, file = 'myprediction.csv', row.names = F) 
