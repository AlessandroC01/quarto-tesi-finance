########## TRADITIONAL COVARIATES ###########
## Basic GLM,
## GLM family, including elastic-net,
## XGBoost.

# Packages 

library(tidyverse)
library(vtable)
library(rpart)
library(repr)
library(rpart.plot)
# library(rfCountData)
library(gam)
library(knitr)
library(kableExtra)
library(janitor)
library(glmnet)
library(scoringRules)
library(sjPlot)



# Data

dataS <- read.csv('Synthetic_data.csv')

# Modifications 
dataS <- dataS %>%
  mutate(Territory = as.factor(Territory)) %>%
  select(-c('Annual.pct.driven', 'Annual.miles.drive'))
data.select <- dataS

# Train-test 
set.seed(123)
train <- data.select %>% sample_frac(0.8, replace = FALSE)
test <- data.select %>% anti_join(train)

# Modif data
train2 <- train %>%
  mutate(Miles.per.day = Total.miles.driven/Duration,
         max.day = pmax(Pct.drive.mon, Pct.drive.tue, Pct.drive.wed, Pct.drive.thr, Pct.drive.fri, Pct.drive.sat, Pct.drive.sun),
         min.day = pmin(Pct.drive.mon, Pct.drive.tue, Pct.drive.wed, Pct.drive.thr, Pct.drive.fri, Pct.drive.sat, Pct.drive.sun),
         max.min = max.day - min.day,
         Dayformax = 'Monday', 
         Dayformax = ifelse(max.day == Pct.drive.tue, 'Tuesday', Dayformax),
         Dayformax = ifelse(max.day == Pct.drive.wed, 'Wednesday', Dayformax),
         Dayformax = ifelse(max.day == Pct.drive.thr, 'Thursday', Dayformax),
         Dayformax = ifelse(max.day == Pct.drive.fri, 'Friday', Dayformax),
         Dayformax = ifelse(max.day == Pct.drive.sat, 'Saturday', Dayformax),
         Dayformax = ifelse(max.day == Pct.drive.sun, 'Sunday', Dayformax),
         Dayformin = 'Monday', 
         Dayformin = ifelse(min.day == Pct.drive.tue, 'Tuesday', Dayformin),
         Dayformin = ifelse(min.day == Pct.drive.wed, 'Wednesday', Dayformin),
         Dayformin = ifelse(min.day == Pct.drive.thr, 'Thursday', Dayformin),
         Dayformin = ifelse(min.day == Pct.drive.fri, 'Friday', Dayformin),
         Dayformin = ifelse(min.day == Pct.drive.sat, 'Saturday', Dayformin),
         Dayformin = ifelse(min.day == Pct.drive.sun, 'Sunday', Dayformin),
         expo = Duration/365.25)

transform.fct <- function(var){
  df <- train2 %>% mutate(var_ = get(var)*Total.miles.driven/(1000*Duration))
  q99 <- quantile(df$var_, 0.99)
  df <- df %>% mutate(var_ = ifelse(var_ > q99, q99, var_))
  #colnames(df)[ncol(df)] <- paste0(var, '_')
  return(df)
}

train2 <- transform.fct("Brake.06miles")
train2 <- transform.fct("Brake.08miles")
train2 <- transform.fct("Brake.09miles")
train2 <- transform.fct("Brake.11miles")
train2 <- transform.fct("Brake.14miles")
train2 <- transform.fct("Accel.06miles")
train2 <- transform.fct("Accel.08miles")
train2 <- transform.fct("Accel.09miles")
train2 <- transform.fct("Accel.11miles")
train2 <- transform.fct("Accel.12miles")
train2 <- transform.fct("Accel.14miles")
train2 <- transform.fct("Left.turn.intensity08")
train2 <- transform.fct("Left.turn.intensity09")
train2 <- transform.fct("Left.turn.intensity10")
train2 <- transform.fct("Left.turn.intensity11")
train2 <- transform.fct("Left.turn.intensity12")
train2 <- transform.fct("Right.turn.intensity08")
train2 <- transform.fct("Right.turn.intensity09")
train2 <- transform.fct("Right.turn.intensity10")
train2 <- transform.fct("Right.turn.intensity11")
train2 <- transform.fct("Right.turn.intensity12")

# Create folds
nb.fold <- 5
fold <- sample(1:nb.fold, nrow(train2), replace = TRUE)
train2$fold <- fold

## 

test2 <- test %>%
  mutate(Miles.per.day = Total.miles.driven/Duration,
         max.day = pmax(Pct.drive.mon, Pct.drive.tue, Pct.drive.wed, Pct.drive.thr, Pct.drive.fri, Pct.drive.sat, Pct.drive.sun),
         min.day = pmin(Pct.drive.mon, Pct.drive.tue, Pct.drive.wed, Pct.drive.thr, Pct.drive.fri, Pct.drive.sat, Pct.drive.sun),
         max.min = max.day - min.day,
         Dayformax = 'Monday', 
         Dayformax = ifelse(max.day == Pct.drive.tue, 'Tuesday', Dayformax),
         Dayformax = ifelse(max.day == Pct.drive.wed, 'Wednesday', Dayformax),
         Dayformax = ifelse(max.day == Pct.drive.thr, 'Thursday', Dayformax),
         Dayformax = ifelse(max.day == Pct.drive.fri, 'Friday', Dayformax),
         Dayformax = ifelse(max.day == Pct.drive.sat, 'Saturday', Dayformax),
         Dayformax = ifelse(max.day == Pct.drive.sun, 'Sunday', Dayformax),
         Dayformin = 'Monday', 
         Dayformin = ifelse(min.day == Pct.drive.tue, 'Tuesday', Dayformin),
         Dayformin = ifelse(min.day == Pct.drive.wed, 'Wednesday', Dayformin),
         Dayformin = ifelse(min.day == Pct.drive.thr, 'Thursday', Dayformin),
         Dayformin = ifelse(min.day == Pct.drive.fri, 'Friday', Dayformin),
         Dayformin = ifelse(min.day == Pct.drive.sat, 'Saturday', Dayformin),
         Dayformin = ifelse(min.day == Pct.drive.sun, 'Sunday', Dayformin),
         expo = Duration/365.25)

transform.fct <- function(var){
  df <- test2 %>% mutate(var_ = get(var)*Total.miles.driven/(1000*Duration))
  q99 <- quantile(df$var_, 0.99)
  df <- df %>% mutate(var_ = ifelse(var_ > q99, q99, var_))
  #colnames(df)[ncol(df)] <- paste0(var, '_')
  return(df)
}

test2 <- transform.fct("Brake.06miles")
test2 <- transform.fct("Brake.08miles")
test2 <- transform.fct("Brake.09miles")
test2 <- transform.fct("Brake.11miles")
test2 <- transform.fct("Brake.14miles")
test2 <- transform.fct("Accel.06miles")
test2 <- transform.fct("Accel.08miles")
test2 <- transform.fct("Accel.09miles")
test2 <- transform.fct("Accel.11miles")
test2 <- transform.fct("Accel.12miles")
test2 <- transform.fct("Accel.14miles")
test2 <- transform.fct("Left.turn.intensity08")
test2 <- transform.fct("Left.turn.intensity09")
test2 <- transform.fct("Left.turn.intensity10")
test2 <- transform.fct("Left.turn.intensity11")
test2 <- transform.fct("Left.turn.intensity12")
test2 <- transform.fct("Right.turn.intensity08")
test2 <- transform.fct("Right.turn.intensity09")
test2 <- transform.fct("Right.turn.intensity10")
test2 <- transform.fct("Right.turn.intensity11")
test2 <- transform.fct("Right.turn.intensity12")


# prediction score

Score.pred <- function(mu, x) {
  Sc.log  <- -sum(dpois(x, mu, log=TRUE))
  Sc.MSE  <- sum((x - mu)^2)
  Sc.quad <- sum(-2*dpois(x,lambda=mu) + sapply(mu, function(x){ sum(dpois(0:10,lambda=x)^2) }))
  Sc.sph <- sum(- dpois(x,mu) / sqrt(sapply(mu, function(x){ sum(dpois(0:10,lambda=x)^2) })))
  Sc.DSS <- sum(dss_pois(x, mu))
  Sc.CRPS <- sum(crps_pois(x, mu))
  
  return(c(Sc.log, Sc.MSE, Sc.quad, Sc.sph, Sc.DSS, Sc.CRPS))
}

## Basic GLM MODELS (single intercept)

### Model on each fold
Result_  <- data.frame()
Result2_  <- data.frame()
for(i in 1:nb.fold) {
  learn <- train2[train2$fold != i,]
  valid <- train2[train2$fold == i,]
  
  mean <- sum(learn$NB_Claim)/sum(learn$expo) 
  learn$pred.base <- mean*learn$expo
  valid$pred.base <- mean*valid$expo
  
  Result_ <- rbind(Result_, c(i, Score.pred(valid$pred.base, valid$NB_Claim)/nrow(valid)))
  Result2_ <- rbind(Result2_, c(i, Score.pred(valid$pred.base, valid$NB_Claim)))
}

### Show results
colnames(Result_) <- c('Fold', "Sc.log", "Sc.MSE", "Sc.quad", "Sc.sph", "Sc.DSS", "Sc.CRPS")
colnames(Result2_) <- c('Fold', "Sc.log", "Sc.MSE", "Sc.quad", "Sc.sph", "Sc.DSS", "Sc.CRPS")
tot <- colSums(Result2_)/nrow(train2)
tot$Fold <- 'Total'
Result_ <- rbind(Result_ , tot)

Result.base <- Result_  
Base <- Result.base[nb.fold+1,]

knitr::kable(Result_, align = "ccccccc", digits = c(0, 5, 5, 5, 5, 5, 5), format.args = list(big.mark = ","))%>%   
  kable_styling(bootstrap_options = "striped", full_width = T)  

mean <- sum(train2$NB_Claim)/sum(train2$expo) 
test2$pred.base <- mean*test2$expo

Result_ <- data.frame(t(Score.pred(test2$pred.base, test2$NB_Claim)/nrow(test2)))
Result_ <- cbind('Base', Result_)
colnames(Result_) <- c("Model", "Sc.log", "Sc.MSE", "Sc.quad", "Sc.sph", "Sc.DSS", "Sc.CRPS")

Result_all <- Result_



## Model with categorical covariates 
score.base <- as.formula(NB_Claim ~ 1 + offset(log(expo)))
score.glm <- as.formula(NB_Claim ~ Insured.sex + Marital  +  Car.use + Region + offset(log(expo)))

## Model on each fold
Result_  <- data.frame()
Result2_  <- data.frame()
for(i in 1:nb.fold) {
  learn <- train2[train2$fold != i,]
  valid <- train2[train2$fold == i,]
  glm.fit <- glm(score.glm, family = poisson(), data = learn)
  
  learn$pred.base <- predict(glm.fit, newdata=learn, type='response')
  valid$pred.base <- predict(glm.fit, newdata=valid, type='response')
  
  Result_ <- rbind(Result_, c(i, Score.pred(valid$pred.base, valid$NB_Claim)/nrow(valid)))
  Result2_ <- rbind(Result2_, c(i, Score.pred(valid$pred.base, valid$NB_Claim)))
}

## Model on all data from train
glm.base <- glm(score.base, family = poisson(), data = train2)
glm.fit <- glm(score.glm, family = poisson(), data = train2)
train2$pred.glm1 <- predict(glm.fit, newdata=train2, type='response')
Result.glm1 <- Result_  

## Show results
colnames(Result_) <- c('Fold', "Sc.log", "Sc.MSE", "Sc.quad", "Sc.sph", "Sc.DSS", "Sc.CRPS")
colnames(Result2_) <- c('Fold', "Sc.log", "Sc.MSE", "Sc.quad", "Sc.sph", "Sc.DSS", "Sc.CRPS")
tot <- colSums(Result2_)/nrow(train2)
tot$Fold <- 'Total'
Result_ <- rbind(Result_ , tot)
Result_ <- rbind(Result_, Base)

Result_[nb.fold+2,1] <- 'Improvement'

for(i in 2:7){
  Result_[nb.fold+2,i] <-  Result_[nb.fold+1,i] - Result_[nb.fold+2,i]
}


rownames(Result_) <- NULL
knitr::kable(Result_, align = "ccccccc", digits = c(0, 5, 5, 5, 5, 5, 5), format.args = list(big.mark = ","))%>%   
  kable_styling(bootstrap_options = "striped", full_width = T)  


#  ESTIMATED PARAMETErS

## Model 
score.base <- as.formula(NB_Claim ~ 1 + offset(log(expo)))
score.glm <- as.formula(NB_Claim ~ Insured.sex + Marital  +  Car.use + Region + offset(log(expo)))

## Model on all data from train
glm.base <- glm(score.base, family = poisson(), data = train2)
glm.fit <- glm(score.glm, family = poisson(), data = train2)

tab_model(glm.base, glm.fit, transform = NULL)



# XGBOOST

library(xgboost)
library(Ckmeans.1d.dp)
library(SHAPforxgboost)

trad.vars <- c("Marital", "Car.use", "Region", "Insured.sex", "Credit.score", "Insured.age", "Car.age", "Years.noclaims", "Territory") 

dtrain <- xgb.DMatrix(data = data.matrix(train2[, paste(trad.vars)]), label = train2$NB_Claim)
setinfo(dtrain,"base_margin",log(train2$expo))
folds <-list(fold1 = which(train2$fold == 1),
             fold2 = which(train2$fold == 2),
             fold3 = which(train2$fold == 3),
             fold4 = which(train2$fold == 4),
             fold5 = which(train2$fold == 5))


## Prediction score for XGBOOST

param <- list(
  eta = 0.08846194,
  max_depth = 43,
  subsample = 0.8389601,
  min_child_weight = 1,
  booster = "gbtree",
  objective = "count:poisson",
  eval_metric = "poisson-nloglik")

set.seed(333)
xgbcv <- xgb.cv(params = param,
                nrounds = 96,
                data = dtrain,
                folds = folds,
                prediction = TRUE,
                early_stopping_rounds = 10,
                verbose = 0,
                maximize = F)

Sc.log <- sapply(xgbcv$folds, function(x){-dpois(train2$NB_Claim[x], unlist(xgbcv$pred[x]), log=TRUE)})
Sc.MSE <- sapply(xgbcv$folds, function(x){(train2$NB_Claim[x]-unlist(xgbcv$pred[x]))^2})
Sc.quad <- sapply(xgbcv$folds, function(x){
  nb <- train2$NB_Claim[x]
  mu <- unlist(xgbcv$pred[x])
  -2*dpois(nb,lambda=mu) + dpois(0,lambda=mu)^2 + dpois(1,lambda=mu)^2 + dpois(2,lambda=mu)^2+ dpois(3,lambda=mu)^2 + dpois(4,lambda=mu)^2 + dpois(5,lambda=mu)^2 
})  
Sc.sph <- sapply(xgbcv$folds, function(x){
  nb <- train2$NB_Claim[x]
  mu <- unlist(xgbcv$pred[x])
  -dpois(nb,lambda=mu) / sqrt(dpois(0,lambda=mu)^2 + dpois(1,lambda=mu)^2 + dpois(2,lambda=mu)^2+ dpois(3,lambda=mu)^2 + dpois(4,lambda=mu)^2 + dpois(5,lambda=mu)^2 ) 
})
Sc.DSS <- sapply(xgbcv$folds, function(x){dss_pois(train2$NB_Claim[x], unlist(xgbcv$pred[x]))})
Sc.CRPS <- sapply(xgbcv$folds, function(x){crps_pois(train2$NB_Claim[x], unlist(xgbcv$pred[x]))})

Result_  <- rbind(
  c(1,mean(Sc.log[1]$fold1), mean(Sc.MSE[1]$fold1), mean(Sc.quad[1]$fold1), mean(Sc.sph[1]$fold1), mean(Sc.DSS[1]$fold1), mean(Sc.CRPS[1]$fold1)),
  c(2,mean(Sc.log[2]$fold2), mean(Sc.MSE[2]$fold2), mean(Sc.quad[2]$fold2), mean(Sc.sph[2]$fold2), mean(Sc.DSS[2]$fold2), mean(Sc.CRPS[2]$fold2)),
  c(3,mean(Sc.log[3]$fold3), mean(Sc.MSE[3]$fold3), mean(Sc.quad[3]$fold3), mean(Sc.sph[3]$fold3), mean(Sc.DSS[3]$fold3), mean(Sc.CRPS[3]$fold3)),
  c(4,mean(Sc.log[4]$fold4), mean(Sc.MSE[4]$fold4), mean(Sc.quad[4]$fold4), mean(Sc.sph[4]$fold4), mean(Sc.DSS[4]$fold4), mean(Sc.CRPS[4]$fold4)),
  c(5,mean(Sc.log[5]$fold5), mean(Sc.MSE[5]$fold5), mean(Sc.quad[5]$fold5), mean(Sc.sph[5]$fold5), mean(Sc.DSS[5]$fold5), mean(Sc.CRPS[5]$fold5))
)

Res.sum  <- rbind(
  c(sum(Sc.log[1]$fold1), sum(Sc.MSE[1]$fold1), sum(Sc.quad[1]$fold1), sum(Sc.sph[1]$fold1), sum(Sc.DSS[1]$fold1), sum(Sc.CRPS[1]$fold1)),
  c(sum(Sc.log[2]$fold2), sum(Sc.MSE[2]$fold2), sum(Sc.quad[2]$fold2), sum(Sc.sph[2]$fold2), sum(Sc.DSS[2]$fold2), sum(Sc.CRPS[2]$fold2)),
  c(sum(Sc.log[3]$fold3), sum(Sc.MSE[3]$fold3), sum(Sc.quad[3]$fold3), sum(Sc.sph[3]$fold3), sum(Sc.DSS[3]$fold3), sum(Sc.CRPS[3]$fold3)),
  c(sum(Sc.log[4]$fold4), sum(Sc.MSE[4]$fold4), sum(Sc.quad[4]$fold4), sum(Sc.sph[4]$fold4), sum(Sc.DSS[4]$fold4), sum(Sc.CRPS[4]$fold4)),
  c(sum(Sc.log[5]$fold5), sum(Sc.MSE[5]$fold5), sum(Sc.quad[5]$fold5), sum(Sc.sph[5]$fold5), sum(Sc.DSS[5]$fold5), sum(Sc.CRPS[5]$fold5))
)
sum <- c('Total', colSums(Res.sum)/nrow(train2))

Result_  <- data.frame(rbind(Result_, sum)) 

## Show results
colnames(Result_) <- c('Fold', "Sc.log", "Sc.MSE", "Sc.quad", "Sc.sph", "Sc.DSS", "Sc.CRPS")
Result_ <- rbind(Result_, Base)

Result_[nb.fold+2,1] <- 'Improvement'

for(i in 2:7){
  Result_[,i] <- as.numeric(Result_[,i])  
  Result_[nb.fold+2,i] <-  Result_[nb.fold+1,i] - Result_[nb.fold+2,i]
}

rownames(Result_) <- NULL
knitr::kable(Result_, align = "ccccccc", digits = c(0, 5, 5, 5, 5, 5, 5), format.args = list(big.mark = ","))%>%   
  kable_styling(bootstrap_options = "striped", full_width = T) 


### Variable importance 

param <- list(
  eta = 0.08846194,
  max_depth = 43,
  subsample = 0.8389601,
  min_child_weight = 1,
  booster = "gbtree",
  objective = "count:poisson",
  eval_metric = "poisson-nloglik")

set.seed(333)
fit.xgb <- xgb.train(params = param,
                     nrounds = 96,
                     data = dtrain)

importance_matrix <- xgb.importance(dimnames(dtrain)[[2]], model = fit.xgb)
xgb.ggplot.importance(importance_matrix,top_n=10) + theme(text = element_text(size=15))
