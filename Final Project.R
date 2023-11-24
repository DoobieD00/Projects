## K353 Final Group Project
## MW 1:10pm Team 6
## Luke Finnell, Ethan Zastawny, Rishabh Tandon, Edbert Liu, Daniel Popowics

## ============================================================================
## ============================================================================
## OBJECTIVE 1

## Here we are importing the two datasets into R
knownroutes_data <- read.csv(file = 'KnownRoutes.csv', header = T, as.is = T)
newroutes_data <- read.csv(file = 'NewRoutes.csv', header = T, as.is = T)

## Here we are understanding the data and performing any necessary data preparations
knownroutes_data <- knownroutes_data[,-(1:4) ] 
summary(knownroutes_data)

index_vac_yes <- which(knownroutes_data$VACATION == 'Yes')
knownroutes_data$VACATION[index_vac_yes] <- 1
index_vac_no <- which(knownroutes_data$VACATION == 'No')
knownroutes_data$VACATION[index_vac_no] <- 0
knownroutes_data$VACATION <- as.integer(as.character(knownroutes_data$VACATION))
index_gate_free <- which(knownroutes_data$GATE == 'Free')
knownroutes_data$GATE[index_gate_free] <- 0
index_gate_con <- which(knownroutes_data$GATE == 'Constrained')
knownroutes_data$GATE[index_gate_con] <- 1
knownroutes_data$GATE <- as.integer(as.character(knownroutes_data$GATE))

index_vacn_yes <- which(newroutes_data$VACATION == 'Yes')
newroutes_data$VACATION[index_vacn_yes] <- 1
index_vacn_no <- which(newroutes_data$VACATION == 'No')
newroutes_data$VACATION[index_vacn_no] <- 0
newroutes_data$VACATION <- as.integer(as.character(newroutes_data$VACATION))
index_gaten_free <- which(newroutes_data$GATE == 'Free')
newroutes_data$GATE[index_gaten_free] <- 0
index_gaten_con <- which(newroutes_data$GATE == 'Constrained')
newroutes_data$GATE[index_gaten_con] <- 1
newroutes_data$GATE <- as.integer(as.character(newroutes_data$GATE))

index_1<-which(knownroutes_data$SW == 1)
index_0<-which(knownroutes_data$SW == 0)
index_slot_1 <-which(knownroutes_data$SLOT == 1)
index_slot_0 <-which(knownroutes_data$SLOT == 0)

## Here we are determining which predictors would be best to use in our models.
## We are exploring the data and understanding correlations between variables.
cor(knownroutes_data[ ,c(1,5,6,7,8,9,12,13,14)])
cor(knownroutes_data[ ,c(2,3,4,10,11)])
boxplot(knownroutes_data$DISTANCE ~ knownroutes_data$VACATION)
boxplot(knownroutes_data$E_INCOME ~ knownroutes_data$GATE)

## ============================================================================
## ============================================================================
## OBJECTIVE 2

## Here we are partitioning the dataset into training set and validation set.
## We are dividing the dataset as 50% training and 50% validation.
train_size <- 0.5 * nrow(knownroutes_data)
set.seed(500)
train_index <- sample(x = 1:nrow(knownroutes_data), size = train_size)
train_set <- knownroutes_data[train_index, ]
valid_set<- knownroutes_data[-train_index, ]
trainset_size <- nrow(train_set)
validset_size <- nrow(valid_set)

## Here is the first linear regression model (model1)
model1<- lm(formula = FARE ~ DISTANCE + S_INCOME + E_INCOME + VACATION, data = train_set)
model_coef <- coef(model1)
summary(model1)

## Here is the second linear regression model (model2)
model2<- lm(formula = FARE ~ DISTANCE + E_INCOME + HI + GATE + SLOT, data = train_set)
model_coef2 <- coef(model2)
summary(model2)

## Here we are testing model1 with the validation set.
actual_valid <- valid_set$FARE
pred_m1_valid <- predict(object = model1, newdata = valid_set)
errors_m1_valid <- actual_valid - pred_m1_valid
ME_m1_valid <- mean(errors_m1_valid)
RMSE_m1_valid <- sqrt(mean((errors_m1_valid)^2))
RMSE_m1_valid

## Here we are evaluating the predictive performance and overfitting behavior.
## We are comparing the performance of model1 validation and training set.
boxplot(errors_m1_valid, main= "Box plot of Prediction Errors")
hist(errors_m1_valid, main= "Histogram of Prediction Errors")

pred_m1_train <- predict(object = model1, newdata = train_set)
actual_train <- train_set$FARE
errors_m1_train <- actual_train - pred_m1_train
RMSE_m1_train <- sqrt(mean((errors_m1_train)^2))
c(RMSE_m1_train, RMSE_m1_valid)

## Here we are testing model2 with the validation set
pred_m2_valid <- predict(object = model2, newdata = valid_set)
errors_m2_valid <- actual_valid - pred_m2_valid
ME_m2_valid <- mean(errors_m1_valid)
RMSE_m2_valid <- sqrt(mean((errors_m2_valid)^2))
RMSE_m2_valid

## Here we are evaluating the predictive performance and overfitting behavior.
## We are comparing the performance of model2 validation and training set.
boxplot(errors_m2_valid, main= "Box plot of Prediction Errors")
hist(errors_m2_valid, main= "Histogram of Prediction Errors")

pred_m2_train <- predict(object = model2, newdata = train_set)
actual_train <- train_set$FARE
errors_m2_train <- actual_train - pred_m2_train
RMSE_m2_train <- sqrt(mean((errors_m2_train)^2))
c(RMSE_m2_train, RMSE_m2_valid)

## We will be using model2 over model1 because of a lower RMSE value.

## ============================================================================
## ============================================================================
## OBJECTIVE 3

## The target variable for objective 3 is SW. We are predicting whether Southwest
## will serve on the new unknown routes.

## Here we are choosing the predictors to include in the model by exploring the
## datasets.
plot(knownroutes_data$DISTANCE, knownroutes_data$HI, 
     col = ifelse(knownroutes_data$SW == 1, yes = "red", no = "blue"),
     pch  = ifelse(knownroutes_data$SW == 1, yes = '+', no = '*'))
plot(knownroutes_data$E_INCOME, knownroutes_data$DISTANCE, 
     col = ifelse(knownroutes_data$SW == 1, yes = "red", no = "blue"),
     pch  = ifelse(knownroutes_data$SW == 1, yes = '+', no = '*'))
cor(knownroutes_data[ ,c(1,5,6,7,8,9,12,13,14)])
table(knownroutes_data$E_INCOME, knownroutes_data$S_INCOME)
table(knownroutes_data$GATE, knownroutes_data$SLOT)
table(knownroutes_data$SW, knownroutes_data$VACATION)
table(knownroutes_data$SW, knownroutes_data$SLOT)
table(knownroutes_data$SW, knownroutes_data$GATE)
boxplot(knownroutes_data$E_INCOME ~ knownroutes_data$GATE)
boxplot(knownroutes_data$DISTANCE ~ knownroutes_data$VACATION)

## Here we are partitioning the dataset into training and validation sets.
## We are dividing the dataset as 50% training and 50% validation.
train_size_log <- 0.5 * nrow(knownroutes_data)
set.seed(500)
train_index_log <- sample(x = 1:nrow(knownroutes_data), size = train_size_log)
train_set_log <- knownroutes_data[train_index_log, ]
valid_set_log<- knownroutes_data[-train_index_log, ]
trainset_size_log <- nrow(train_set_log)
validset_size_log <- nrow(valid_set_log)

## Here we are fitting the coefficient values using training set.
model3 <- glm(formula = SW ~ S_INCOME + DISTANCE + VACATION + SLOT + S_POP, 
              data = train_set_log, family = "binomial")
summary(model3)
coef(model3)

model4 <- glm(formula = SW ~ E_INCOME + DISTANCE + HI, 
              data = train_set_log, family = "binomial")
summary(model4)
coef(model4)

## Here we are classifying the observations in the validation set using 
## a cut off of 0.5.
pred_sw_yes3 <- predict(object = model3, newdata = valid_set_log, 
                          type = "response")
cutoff <- 0.5
sw_yes3 <- ifelse(pred_sw_yes3>cutoff, yes = 1, no = 0)

## Here we are evaluating the classification performance of model3 
## with the validation set assuming that detecting whether SW will serve on 
## the new route (SW = 1) is more important.
library(caret)
actual_valid_log <- valid_set_log$SW
confusionMatrix(as.factor(sw_yes3), 
                as.factor(actual_valid_log), positive = "1")

## Here we are classifying the observations in the validation set using 
## a cut off of 0.5.
pred_sw_yes4 <- predict(object = model4, newdata = valid_set_log, 
                       type = "response")
cutoff <- 0.5
sw_yes4 <- ifelse(pred_sw_yes4>cutoff, yes = 1, no = 0)

## Here we are evaluating the classification performance of the model 
## with the validation set assuming that detecting whether SW will serve on 
## the new route (SW = 1) is more important.
library(caret)
actual_valid_log <- valid_set_log$SW
confusionMatrix(as.factor(sw_yes4), 
                as.factor(actual_valid_log), positive = "1")

## We will be using model3 over model 4 because of higher accuracy and
## sensitivity values.

#==============================================================================
#==============================================================================
## OBJECTIVE 4

## Here we are applying our models to predict FARE and SW in the newroutes_data.
newroutes_data$FARE <- predict(object = model2, newdata = newroutes_data)

pred_prob <- predict(object = model3, newdata = newroutes_data, 
                       type = "response")
cutoff <- 0.5
newroutes_data$SW <- ifelse(pred_prob>cutoff, yes = 1, no = 0)

#==============================================================================
#==============================================================================
## OBJECTIVES 5 & 6

## Here we are running a simulation for fare, SW, and demand of route 1.
## Route 1 is between St Louis, MO and Baltimore/Wash. Intl, MD.
## The first step is to simulate the fare (fare_1) by using the mean calculated
## from model2 and the RSME of model2. We assume that fare follows a normal dist.
fare_1 <- c()
nsim <- 10000
set.seed(500)
for(i in 1:nsim){
  fare_estimated_1 <- rnorm(n = 1, mean = newroutes_data$FARE[1], sd = RMSE_m2_valid)
  if(fare_estimated_1 >= 0){
    fare_1[i] <- fare_estimated_1
  }else{
    fare_1[i] <- 0
  }
}
## Here we are simulating SW based on the calculated probabilities from model3.
## If SW is serving the route, we use 0.9 to signify this because of our pricing
## strategy outlined before. 1.1 will signify is SW is not serving the route.
SW_1 <- c()
nsim <- 10000
set.seed(500)
for(i in 1:nsim){
  SW_estimated_1 <- rbinom(n = 1, size = 1, prob = pred_prob[1])
  if(SW_estimated_1 >= 0.5){
    SW_1[i] <- 0.9
  }else{
    SW_1[i] <- 1.1
  }
}
## Here we are simulating for demand of route 1 using the standard deviations
## given by AA.
demand_1 <- c()
nsim <- 10000
set.seed(500)
for(i in 1:nsim){
  estimated_1 <- rnorm(n=1, mean=10264, sd=4000)
  if(estimated_1 >= 0){
    demand_1[i] <- estimated_1
  }else{
    demand_1[i] <- 0
  }
}
## Here we calculate revenue and take the mean of all revenue simulations.
## The final result is 95% CI of the estimated revenue for route 1.
revenue_1 <- fare_1*SW_1*demand_1
sample_mean_1 <- mean(revenue_1)
newroutes_data$sample_mean[1] <- sample_mean_1
sample_sd_1 <- sd(revenue_1)
mse_1 <- sample_sd_1/sqrt(nsim)
newroutes_data$lower_95ci[1] <- sample_mean_1 - 1.96*mse_1
newroutes_data$upper_95ci[1] <- sample_mean_1 + 1.96*mse_1

## Here we are running a simulation for fare, SW, and demand of route 2.
## Route 2 is between between St Louis, MO and Tampa, FL.
## The first step is to simulate the fare (fare_2) by using the mean calculated
## from model2 and the RSME of model2. We assume that fare follows a normal dist.
fare_2 <- c()
nsim <- 10000
set.seed(500)
for(i in 1:nsim){
  fare_estimated_2 <- rnorm(n = 1, mean = newroutes_data$FARE[2], sd = RMSE_m2_valid)
  if(fare_estimated_2 >= 0){
    fare_2[i] <- fare_estimated_2
  }else{
    fare_2[i] <- 0
  }
}
## Here we are simulating SW based on the calculated probabilities from model3.
## If SW is serving the route, we use 0.9 to signify this because of our pricing
## strategy outlined before. 1.1 will signify is SW is not serving the route.
SW_2 <- c()
nsim <- 10000
set.seed(500)
for(i in 1:nsim){
  SW_estimated_2 <- rbinom(n = 1, size = 1, prob = pred_prob[2])
  if(SW_estimated_2 >= 0.5){
    SW_2[i] <- 0.9
  }else{
    SW_2[i] <- 1.1
  }
}
## Here we are simulating for demand of route 2 using the standard deviations
## given by AA.
demand_2 <- c()
nsim <- 10000
set.seed(500)
for(i in 1:nsim){
  estimated_2 <- rnorm(n=1, mean=4749, sd=2000)
  if(estimated_2 >= 0){
    demand_2[i] <- estimated_2
  }else{
    demand_2[i] <- 0
  }
}
## Here we calculate revenue and take the mean of all revenue simulations.
## The final result is 95% CI of the estimated revenue for route 2.
revenue_2 <- fare_2*SW_2*demand_2
sample_mean_2 <- mean(revenue_2)
newroutes_data$sample_mean[2] <- sample_mean_2
sample_sd_2 <- sd(revenue_2)
mse_2 <- sample_sd_2/sqrt(nsim)
newroutes_data$lower_95ci[2] <- sample_mean_2 - 1.96*mse_2
newroutes_data$upper_95ci[2] <- sample_mean_2 + 1.96*mse_2

## Here we are running a simulation for fare, SW, and demand of route 3.
## Route 3 is between St Louis, MO and Washington DC (IAD).
## The first step is to simulate the fare (fare_3) by using the mean calculated
## from model2 and the RSME of model2. We assume that fare follows a normal dist.
fare_3 <- c()
nsim <- 10000
set.seed(500)
for(i in 1:nsim){
  fare_estimated_3 <- rnorm(n = 1, mean = newroutes_data$FARE[3], sd = RMSE_m2_valid)
  if(fare_estimated_3 >= 0){
    fare_3[i] <- fare_estimated_3
  }else{
    fare_3[i] <- 0
  }
}
## Here we are simulating SW based on the calculated probabilities from model3.
## If SW is serving the route, we use 0.9 to signify this because of our pricing
## strategy outlined before. 1.1 will signify is SW is not serving the route.
SW_3 <- c()
nsim <- 10000
set.seed(500)
for(i in 1:nsim){
  SW_estimated_3 <- rbinom(n = 1, size = 1, prob = pred_prob[3])
  if(SW_estimated_2 >= 0.5){
    SW_3[i] <- 0.9
  }else{
    SW_3[i] <- 1.1
  }
}
## Here we are simulating for demand of route 3 using the standard deviations
## given by AA.
demand_3 <- c()
nsim <- 10000
set.seed(500)
for(i in 1:nsim){
  estimated_3 <- rnorm(n=1, mean=4957, sd=2000)
  if(estimated_3 >= 0){
    demand_3[i] <- estimated_3
  }else{
    demand_3[i] <- 0
  }
}
## Here we calculate revenue and take the mean of all revenue simulations.
## The final result is 95% CI of the estimated revenue for route 3.
revenue_3 <- fare_3*SW_3*demand_3
sample_mean_3 <- mean(revenue_3)
newroutes_data$sample_mean[3] <- sample_mean_3
sample_sd_3 <- sd(revenue_3)
mse_3 <- sample_sd_3/sqrt(nsim)
newroutes_data$lower_95ci[3] <- sample_mean_3 - 1.96*mse_3
newroutes_data$upper_95ci[3] <- sample_mean_3 + 1.96*mse_3

## Here we are running a simulation for fare, SW, and demand of route 4.
## Route 4 is between St Louis, MO and Washington DC (DCA).
## The first step is to simulate the fare (fare_4) by using the mean calculated
## from model2 and the RSME of model2. We assume that fare follows a normal dist.
fare_4 <- c()
nsim <- 10000
set.seed(500)
for(i in 1:nsim){
  fare_estimated_4 <- rnorm(n = 1, mean = newroutes_data$FARE[4], sd = RMSE_m2_valid)
  if(fare_estimated_4 >= 0){
    fare_4[i] <- fare_estimated_4
  }else{
    fare_4[i] <- 0
  }
}
## Here we are simulating SW based on the calculated probabilities from model3.
## If SW is serving the route, we use 0.9 to signify this because of our pricing
## strategy outlined before. 1.1 will signify is SW is not serving the route.
SW_4 <- c()
nsim <- 10000
set.seed(500)
for(i in 1:nsim){
  SW_estimated_4 <- rbinom(n = 1, size = 1, prob = pred_prob[4])
  if(SW_estimated_4 >= 0.5){
    SW_4[i] <- 0.9
  }else{
    SW_4[i] <- 1.1
  }
}
## Here we are simulating for demand of route 4 using the standard deviations
## given by AA.
demand_4 <- c()
nsim <- 10000
set.seed(500)
for(i in 1:nsim){
  estimated_4 <- rnorm(n=1, mean=4957, sd=100)
  if(estimated_4 >= 0){
    demand_4[i] <- estimated_4
  }else{
    demand_4[i] <- 0
  }
}
## Here we calculate revenue and take the mean of all revenue simulations.
## The final result is 95% CI of the estimated revenue for route 3.
revenue_4 <- fare_4*SW_4*demand_4
sample_mean_4 <- mean(revenue_4)
newroutes_data$sample_mean[4] <- sample_mean_4
sample_sd_4 <- sd(revenue_4)
mse_4 <- sample_sd_4/sqrt(nsim)
newroutes_data$lower_95ci[4] <- sample_mean_4 - 1.96*mse_4
newroutes_data$upper_95ci[4] <- sample_mean_4 + 1.96*mse_4

