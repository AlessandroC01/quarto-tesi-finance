##################### TELEMATICS COVARIATES ############################


# To compare models, we will employ the following functions that calculate predictive scores:

Score.pred <- function(mu, x) {
  Sc.log  <- -sum(dpois(x, mu, log=TRUE))
  Sc.MSE  <- sum((x - mu)^2)
  Sc.quad <- sum(-2*dpois(x,lambda=mu) + sapply(mu, function(x){ sum(dpois(0:10,lambda=x)^2) }))
  Sc.sph <- sum(- dpois(x,mu) / sqrt(sapply(mu, function(x){ sum(dpois(0:10,lambda=x)^2) })))
  Sc.DSS <- sum(dss_pois(x, mu))
  Sc.CRPS <- sum(crps_pois(x, mu))
  
  return(c(Sc.log, Sc.MSE, Sc.quad, Sc.sph, Sc.DSS, Sc.CRPS))
}

# library 

library(tidyverse)
library(vtable)
library(rpart)
library(repr)
library(rpart.plot)
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


# Transformation 

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

# Mean Encoding with White Noise pour les territoires
cardi <- length(unique(train$Territory))

enc.terr <- train2 %>%
  group_by(Territory) %>%
  summarize(freq = sum(NB_Claim)/sum(expo)) %>%
  arrange(freq) %>%
  mutate(terr.code= row_number()/(cardi+1)) %>%
  select(Territory, terr.code)

train2 <- train2 %>%
  group_by(Territory) %>%
  left_join(enc.terr, by='Territory') %>%
  ungroup()

test2 <- test2 %>%
  group_by(Territory) %>%
  left_join(enc.terr, by='Territory') %>%
  ungroup()


# Basic GLM

## Single intercept 

## Model on each fold

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

## Show results
colnames(Result_) <- c('Fold', "Sc.log", "Sc.MSE", "Sc.quad", "Sc.sph", "Sc.DSS", "Sc.CRPS")
colnames(Result2_) <- c('Fold', "Sc.log", "Sc.MSE", "Sc.quad", "Sc.sph", "Sc.DSS", "Sc.CRPS")
tot <- colSums(Result2_)/nrow(train2)
tot$Fold <- 'Total'
Result_ <- rbind(Result_ , tot)

Result.base <- Result_  
Base <- Result.base[nb.fold+1,]

knitr::kable(Result_, align = "ccccccc", digits = c(0, 5, 5, 5, 5, 5, 5), format.args = list(big.mark = ","))%>%   
  kable_styling(bootstrap_options = "striped", full_width = T)  


## model base 

mean <- sum(train2$NB_Claim)/sum(train2$expo) 
test2$pred.base <- mean*test2$expo

Result_ <- data.frame(t(Score.pred(test2$pred.base, test2$NB_Claim)/nrow(test2)))
Result_ <- cbind('Base', Result_)
colnames(Result_) <- c("Model", "Sc.log", "Sc.MSE", "Sc.quad", "Sc.sph", "Sc.DSS", "Sc.CRPS")

Result_all <- Result_

knitr::kable(Result_all, align = "ccccccc", digits = c(0, 5, 5, 5, 5, 5, 5), format.args = list(big.mark = ","))%>%   
  kable_styling(bootstrap_options = "striped", full_width = T)  


# Model with traditional Covariates 
# already used (without protected variable)

## Model 
score.base <- as.formula(NB_Claim ~ 1 + offset(log(expo)))
score.glm <- as.formula(NB_Claim ~ Car.use + Region + Car.age + I(Car.age^2) 
                        + Years.noclaims + I(Years.noclaims^2)  + I(Years.noclaims^3) 
                        + offset(log(expo)))
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


## Model base + glm(trad.)

score.glm <- as.formula(NB_Claim ~ Car.use + Region + Car.age + I(Car.age^2) 
                        + Years.noclaims + I(Years.noclaims^2)  + I(Years.noclaims^3) 
                        + offset(log(expo)))

glm.fit <- glm(score.glm, family = poisson(), data = train2)
test2$pred.base <- predict(glm.fit, newdata=test2, type='response')

Result_ <- data.frame(t(Score.pred(test2$pred.base, test2$NB_Claim)/nrow(test2)))
Result_ <- cbind('GLM (trad.)', Result_)
colnames(Result_) <- c("Model", "Sc.log", "Sc.MSE", "Sc.quad", "Sc.sph", "Sc.DSS", "Sc.CRPS")

Result_all <- rbind(Result_all, Result_)

knitr::kable(Result_all, align = "ccccccc", digits = c(0, 5, 5, 5, 5, 5, 5), format.args = list(big.mark = ","))%>%   
  kable_styling(bootstrap_options = "striped", full_width = T) 


## Estimated parameters for the GLM 
# model with traditional covariates (without protected)

## Model 
score.base <- as.formula(NB_Claim ~ 1 + offset(log(expo)))

score.glm <- as.formula(NB_Claim ~ Car.use + Region + Car.age + I(Car.age^2) 
                        + Years.noclaims + I(Years.noclaims^2)  + I(Years.noclaims^3) 
                        + offset(log(expo)))

## Model on all data from train
glm.base <- glm(score.base, family = poisson(), data = train2)
glm.fit <- glm(score.glm, family = poisson(), data = train2)

tab_model(glm.base, glm.fit, transform = NULL)




# GLM-NET (ADDING TELEMATICS VARIABLES)

## Parametric transformation of continuous covariates

# VEHICLE USAGE LEVEL

min_ <- min(train2$Miles.per.day) 
max_ <- max(train2$Miles.per.day) 
by_ <-  (max_ - min_)/(nrow(train2)-1) 
add <- data.frame(seq(min_, max_, by_)) 
colnames(add) <- 'Miles.per.day'

q99 <- quantile(train2$Miles.per.day, 0.99)

db <- train2 %>%
  select(-'Miles.per.day') %>%
  dplyr::slice(1) 
db <- bind_rows(replicate(nrow(train2), db, simplify = FALSE))
db <- cbind(db, add)

##

score.gam <- as.formula(NB_Claim ~ Car.use + Region + Car.age + I(Car.age^2) 
                        + Years.noclaims + I(Years.noclaims^2)  + I(Years.noclaims^3) + offset(log(expo))
                        + s(Miles.per.day))

score.glm <- as.formula(NB_Claim ~ Car.use + Region + Car.age + I(Car.age^2) 
                        + Years.noclaims + I(Years.noclaims^2)  + I(Years.noclaims^3) + offset(log(expo))
                        + Miles.per.day + log(Miles.per.day) )

gam.fit <- gam(score.gam, family = poisson(), data = train2)
glm.fit <- glm(score.glm, family = poisson(), data = train2)

db$pred.gam <- predict(gam.fit, newdata=db, type='response')
db$pred.glm <- predict(glm.fit, newdata=db, type='response')
base <- db %>%
  mutate(diff = abs(Miles.per.day - mean(train2$Miles.per.day))) %>%
  filter(diff == min(diff))
db$pred.gam <- db$pred.gam/base$pred.gam[1]
db$pred.glm <- db$pred.glm/base$pred.glm[1]

ggplot()+
  geom_line(aes(x=Miles.per.day, y=pred.gam, color='GAM'), data=db) + 
  geom_line(aes(x=Miles.per.day, y=pred.glm, color='Parametric GLM'), data=db) +
  guides(color = guide_legend(title = "")) +
  labs(x = 'Miles per day',
       y = 'Relativity') +
  xlim(0, q99) +
  theme_classic()+
  theme(legend.position = 'bottom', legend.direction = "horizontal")


### Avgdays.week

min_ <- min(train2$Avgdays.week) 
max_ <- max(train2$Avgdays.week) 
by_ <-  (max_ - min_)/(nrow(train2)-1) 
add <- data.frame(seq(min_, max_, by_)) 
colnames(add) <- 'Avgdays.week'

q99 <- quantile(train2$Avgdays.week, 0.99)

db <- train2 %>%
  select(-'Avgdays.week') %>%
  dplyr::slice(1) 
db <- bind_rows(replicate(nrow(train2), db, simplify = FALSE))
db <- cbind(db, add)

##

score.gam <- as.formula(NB_Claim ~ Car.use + Region + Car.age + I(Car.age^2) 
                        + Years.noclaims + I(Years.noclaims^2)  + I(Years.noclaims^3) + offset(log(expo))
                        + s(Avgdays.week))

score.glm <- as.formula(NB_Claim ~ Car.use + Region + Car.age + I(Car.age^2) 
                        + Years.noclaims + I(Years.noclaims^2)  + I(Years.noclaims^3) + offset(log(expo))
                        + Avgdays.week + I(Avgdays.week^2) )

gam.fit <- gam(score.gam, family = poisson(), data = train2)
glm.fit <- glm(score.glm, family = poisson(), data = train2)

db$pred.gam <- predict(gam.fit, newdata=db, type='response')
db$pred.glm <- predict(glm.fit, newdata=db, type='response')
base <- db %>%
  mutate(diff = abs(Avgdays.week - mean(train2$Avgdays.week))) %>%
  filter(diff == min(diff))
db$pred.gam <- db$pred.gam/base$pred.gam[1]
db$pred.glm <- db$pred.glm/base$pred.glm[1]

ggplot()+
  geom_line(aes(x=Avgdays.week, y=pred.gam, color='GAM'), data=db) + 
  geom_line(aes(x=Avgdays.week, y=pred.glm, color='Parametric GLM'), data=db) +
  guides(color = guide_legend(title = "")) +
  labs(x = 'Avgdays.week',
       y = 'Relativity') +
  xlim(0, q99) +
  theme_classic()+
  theme(legend.position = 'bottom', legend.direction = "horizontal")

# TYPE OF VEHICLE USE 

train2$Pct.drive <- train2$Pct.drive.sun

min_ <- min(train2$Pct.drive) 
max_ <- max(train2$Pct.drive) 
by_ <-  (max_ - min_)/(nrow(train2)-1) 
add <- data.frame(seq(min_, max_, by_)) 
colnames(add) <- 'Pct.drive'

q99 <- quantile(train2$Pct.drive, 0.99)

db <- train2 %>%
  select(-'Pct.drive') %>%
dplyr::slice(1) 
db <- bind_rows(replicate(nrow(train2), db, simplify = FALSE))
db <- cbind(db, add)

##

score.gam <- as.formula(NB_Claim ~ Car.use + Region + Car.age + I(Car.age^2) 
                        + Years.noclaims + I(Years.noclaims^2)  + I(Years.noclaims^3) + offset(log(expo))
                        + s(Pct.drive))

score.glm <- as.formula(NB_Claim ~ Car.use + Region + Car.age + I(Car.age^2) 
                        + Years.noclaims + I(Years.noclaims^2)  + I(Years.noclaims^3) + offset(log(expo))
                        + Pct.drive + I(Pct.drive^2) )

gam.fit <- gam(score.gam, family = poisson(), data = train2)
glm.fit <- glm(score.glm, family = poisson(), data = train2)

db$pred.gam <- predict(gam.fit, newdata=db, type='response')
db$pred.glm <- predict(glm.fit, newdata=db, type='response')
base <- db %>%
  mutate(diff = abs(Pct.drive - mean(train2$Pct.drive))) %>%
  filter(diff == min(diff))
db$pred.gam <- db$pred.gam/base$pred.gam[1]
db$pred.glm <- db$pred.glm/base$pred.glm[1]

ggplot()+
  geom_line(aes(x=Pct.drive, y=pred.gam, color='GAM'), data=db) + 
  geom_line(aes(x=Pct.drive, y=pred.glm, color='Parametric GLM'), data=db) +
  guides(color = guide_legend(title = "")) +
  labs(x = 'Pct.drive',
       y = 'Relativity') +
  xlim(0, q99) +
  theme_classic()+
  theme(legend.position = 'bottom', legend.direction = "horizontal")


### Pct.drive 


train2$use.day <- train2$Pct.drive.rush.am

min_ <- min(train2$use.day) 
max_ <- max(train2$use.day) 
by_ <-  (max_ - min_)/(nrow(train2)-1) 
add <- data.frame(seq(min_, max_, by_)) 
colnames(add) <- 'use.day'

q99 <- quantile(train2$use.day, 0.99)

db <- train2 %>%
  select(-'use.day') %>%
dplyr::slice(1) 
db <- bind_rows(replicate(nrow(train2), db, simplify = FALSE))
db <- cbind(db, add)

##

score.gam <- as.formula(NB_Claim ~ Car.use + Region + Car.age + I(Car.age^2) 
                        + Years.noclaims + I(Years.noclaims^2)  + I(Years.noclaims^3) + offset(log(expo))
                        + s(use.day))

score.glm <- as.formula(NB_Claim ~ Car.use + Region + Car.age + I(Car.age^2) 
                        + Years.noclaims + I(Years.noclaims^2)  + I(Years.noclaims^3) + offset(log(expo))
                        + use.day + I(use.day^0.5))

gam.fit <- gam(score.gam, family = poisson(), data = train2)
glm.fit <- glm(score.glm, family = poisson(), data = train2)

db$pred.gam <- predict(gam.fit, newdata=db, type='response')
db$pred.glm <- predict(glm.fit, newdata=db, type='response')
base <- db %>%
  mutate(diff = abs(use.day - mean(train2$use.day))) %>%
  filter(diff == min(diff))
db$pred.gam <- db$pred.gam/base$pred.gam[1]
db$pred.glm <- db$pred.glm/base$pred.glm[1]

ggplot()+
  geom_line(aes(x=use.day, y=pred.gam, color='GAM'), data=db) + 
  geom_line(aes(x=use.day, y=pred.glm, color='Parametric GLM'), data=db) +
  guides(color = guide_legend(title = "")) +
  labs(x = 'Use per day',
       y = 'Relativity') +
  xlim(0, q99) +
  theme_classic()+
  theme(legend.position = 'bottom', legend.direction = "horizontal")



### Max day

train2$use.day <- train2$max.day

min_ <- min(train2$use.day) 
max_ <- max(train2$use.day) 
by_ <-  (max_ - min_)/(nrow(train2)-1) 
add <- data.frame(seq(min_, max_, by_)) 
colnames(add) <- 'use.day'

q99 <- quantile(train2$use.day, 0.99)

db <- train2 %>%
  select(-'use.day') %>%
  dplyr::slice(1) 
db <- bind_rows(replicate(nrow(train2), db, simplify = FALSE))
db <- cbind(db, add)

##

score.gam <- as.formula(NB_Claim ~ Car.use + Region + Car.age + I(Car.age^2) 
                        + Years.noclaims + I(Years.noclaims^2)  + I(Years.noclaims^3) + offset(log(expo))
                        + s(use.day))

score.glm <- as.formula(NB_Claim ~ Car.use + Region + Car.age + I(Car.age^2) 
                        + Years.noclaims + I(Years.noclaims^2)  + I(Years.noclaims^3) + offset(log(expo))
                        + use.day + log(use.day) )

gam.fit <- gam(score.gam, family = poisson(), data = train2)
glm.fit <- glm(score.glm, family = poisson(), data = train2)

db$pred.gam <- predict(gam.fit, newdata=db, type='response')
db$pred.glm <- predict(glm.fit, newdata=db, type='response')
base <- db %>%
  mutate(diff = abs(use.day - mean(train2$use.day))) %>%
  filter(diff == min(diff))
db$pred.gam <- db$pred.gam/base$pred.gam[1]
db$pred.glm <- db$pred.glm/base$pred.glm[1]

ggplot()+
  geom_line(aes(x=use.day, y=pred.gam, color='GAM'), data=db) + 
  geom_line(aes(x=use.day, y=pred.glm, color='Parametric GLM'), data=db) +
  guides(color = guide_legend(title = "")) +
  labs(x = 'Use per day',
       y = 'Relativity') +
  xlim(0, q99) +
  theme_classic()+
  theme(legend.position = 'bottom', legend.direction = "horizontal")

### Min day


train2$use.day <- train2$min.day

min_ <- min(train2$use.day) 
max_ <- max(train2$use.day) 
by_ <-  (max_ - min_)/(nrow(train2)-1) 
add <- data.frame(seq(min_, max_, by_)) 
colnames(add) <- 'use.day'

q99 <- quantile(train2$use.day, 0.99)

db <- train2 %>%
  select(-'use.day') %>%
  dplyr::slice(1) 
db <- bind_rows(replicate(nrow(train2), db, simplify = FALSE))
db <- cbind(db, add)

##

score.gam <- as.formula(NB_Claim ~ Car.use + Region + Car.age + I(Car.age^2) 
                        + Years.noclaims + I(Years.noclaims^2)  + I(Years.noclaims^3) + offset(log(expo))
                        + s(use.day))

score.glm <- as.formula(NB_Claim ~ Car.use + Region + Car.age + I(Car.age^2) 
                        + Years.noclaims + I(Years.noclaims^2)  + I(Years.noclaims^3) + offset(log(expo))
                        + use.day + I(use.day^2) )

gam.fit <- gam(score.gam, family = poisson(), data = train2)
glm.fit <- glm(score.glm, family = poisson(), data = train2)

db$pred.gam <- predict(gam.fit, newdata=db, type='response')
db$pred.glm <- predict(glm.fit, newdata=db, type='response')
base <- db %>%
  mutate(diff = abs(use.day - mean(train2$use.day))) %>%
  filter(diff == min(diff))
db$pred.gam <- db$pred.gam/base$pred.gam[1]
db$pred.glm <- db$pred.glm/base$pred.glm[1]

ggplot()+
  geom_line(aes(x=use.day, y=pred.gam, color='GAM'), data=db) + 
  geom_line(aes(x=use.day, y=pred.glm, color='Parametric GLM'), data=db) +
  guides(color = guide_legend(title = "")) +
  labs(x = 'Use per day',
       y = 'Relativity') +
  xlim(0, q99) +
  theme_classic()+
  theme(legend.position = 'bottom', legend.direction = "horizontal")

### Max min

train2$use.day <- train2$max.min

min_ <- min(train2$use.day) 
max_ <- max(train2$use.day) 
by_ <-  (max_ - min_)/(nrow(train2)-1) 
add <- data.frame(seq(min_, max_, by_)) 
colnames(add) <- 'use.day'

q99 <- quantile(train2$use.day, 0.99)

db <- train2 %>%
  select(-'use.day') %>%
  dplyr::slice(1) 
db <- bind_rows(replicate(nrow(train2), db, simplify = FALSE))
db <- cbind(db, add)

##

score.gam <- as.formula(NB_Claim ~ Car.use + Region + Car.age + I(Car.age^2) 
                        + Years.noclaims + I(Years.noclaims^2)  + I(Years.noclaims^3) + offset(log(expo))
                        + s(use.day))

score.glm <- as.formula(NB_Claim ~ Car.use + Region + Car.age + I(Car.age^2) 
                        + Years.noclaims + I(Years.noclaims^2)  + I(Years.noclaims^3) + offset(log(expo))
                        + use.day + I(use.day^2) )

gam.fit <- gam(score.gam, family = poisson(), data = train2)
glm.fit <- glm(score.glm, family = poisson(), data = train2)

db$pred.gam <- predict(gam.fit, newdata=db, type='response')
db$pred.glm <- predict(glm.fit, newdata=db, type='response')
base <- db %>%
  mutate(diff = abs(use.day - mean(train2$use.day))) %>%
  filter(diff == min(diff))
db$pred.gam <- db$pred.gam/base$pred.gam[1]
db$pred.glm <- db$pred.glm/base$pred.glm[1]

ggplot()+
  geom_line(aes(x=use.day, y=pred.gam, color='GAM'), data=db) + 
  geom_line(aes(x=use.day, y=pred.glm, color='Parametric GLM'), data=db) +
  guides(color = guide_legend(title = "")) +
  labs(x = 'Use per day',
       y = 'Relativity') +
  xlim(0, q99) +
  theme_classic()+
  theme(legend.position = 'bottom', legend.direction = "horizontal")




## FITTING GLM-NET



glm.score <- as.formula(NB_Claim ~ Car.use + Region + Car.age + I(Car.age^2) 
                        + Years.noclaims + I(Years.noclaims^2)  + I(Years.noclaims^3)
                        + Dayformax + Dayformin +
                          + Miles.per.day + log(Miles.per.day)
                        + Avgdays.week + I(Avgdays.week^2)
                        + Pct.drive.mon + I(Pct.drive.mon^2)
                        + Pct.drive.tue + I(Pct.drive.tue^2)
                        + Pct.drive.wed + I(Pct.drive.wed^2)
                        + Pct.drive.thr + I(Pct.drive.thr^2)
                        + Pct.drive.fri + I(Pct.drive.fri^2)
                        + Pct.drive.sat + I(Pct.drive.sat^2)
                        + Pct.drive.sun + I(Pct.drive.sun^2)
                        + Pct.drive.wkend + I(Pct.drive.wkend^2)
                        + max.day + log(max.day) 
                        + min.day + I(min.day^2)
                        + max.min + I(max.min^2)
                        + Pct.drive.rush.am + sqrt(Pct.drive.rush.am) 
                        + Pct.drive.rush.pm + sqrt(Pct.drive.rush.pm)   
                        + Pct.drive.2hrs + sqrt(Pct.drive.2hrs) 
                        + Pct.drive.3hrs + sqrt(Pct.drive.3hrs) 
                        + Pct.drive.4hrs + sqrt(Pct.drive.4hrs) 
                        + Accel.06miles + I(Accel.06miles^2) + I(Accel.06miles^3)
                        + Accel.08miles + I(Accel.08miles^2) + I(Accel.08miles^3)
                        + Accel.09miles + I(Accel.09miles^2) + I(Accel.09miles^3)
                        + Accel.11miles + I(Accel.11miles^2) + I(Accel.11miles^3)
                        + Accel.12miles + I(Accel.12miles^2) + I(Accel.12miles^3)
                        + Accel.14miles + I(Accel.14miles^2) + I(Accel.14miles^3)
                        + Brake.06miles + I(Brake.06miles^2) + I(Brake.06miles^3)
                        + Brake.08miles + I(Brake.08miles^2) + I(Brake.08miles^3)
                        + Brake.09miles + I(Brake.09miles^2) + I(Brake.09miles^3)
                        + Brake.11miles + I(Brake.11miles^2) + I(Brake.11miles^3)
                        + Brake.12miles + I(Brake.12miles^2) + I(Brake.12miles^3)
                        + Brake.14miles + I(Brake.14miles^2) + I(Brake.14miles^3)
                        + Left.turn.intensity08 + log1p(Left.turn.intensity08)
                        + Left.turn.intensity09 + log1p(Left.turn.intensity09)
                        + Left.turn.intensity10 + log1p(Left.turn.intensity10)
                        + Left.turn.intensity11 + log1p(Left.turn.intensity11)
                        + Left.turn.intensity12 + log1p(Left.turn.intensity12)
                        + Right.turn.intensity08 + log1p(Right.turn.intensity08)
                        + Right.turn.intensity09 + log1p(Right.turn.intensity09)
                        + Right.turn.intensity10 + log1p(Right.turn.intensity10)
                        + Right.turn.intensity11 + log1p(Right.turn.intensity11)
                        + Right.turn.intensity12 + log1p(Right.turn.intensity12))

Result_  <- data.frame()
Result2_  <- data.frame()
for(i in 1:nb.fold) {
  learn <- train2[train2$fold != i,]
  valid <- train2[train2$fold == i,]
  
  matrix.x <- model.matrix(glm.score, data=learn)[,-1]
  y <- learn$NB_Claim
  offset <- log(learn$expo)
  
  lambda.min <- 3.981072e-05
  lambda.1se <- 0.001258925
  
  lambda.select <- lambda.min
  fit <- glmnet(matrix.x, y, family = "poisson", relax=FALSE, offset = offset, alpha = 1, lambda = lambda.select)
  #fit <- glmnet(matrix.x, y, family = "poisson", relax=TRUE, offset = offset, alpha = 1, lambda = lambda.select)
  
  matrix.x <- model.matrix(glm.score, data=valid)[,-1]
  y <- valid$NB_Claim
  offset <- log(valid$expo)
  
  valid$pred <- predict(fit, newx = matrix.x, type='response', newoffset=offset, lambda = lambda.select)
  
  Result_ <- rbind(Result_, c(i, Score.pred(valid$pred, valid$NB_Claim)/nrow(valid)))
  Result2_ <- rbind(Result2_, c(i, Score.pred(valid$pred, valid$NB_Claim)))
}


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


## Test 


glm.score <- as.formula(NB_Claim ~ Car.use + Region + Car.age + I(Car.age^2) 
                        + Years.noclaims + I(Years.noclaims^2)  + I(Years.noclaims^3)
                        + Dayformax + Dayformin +
                          + Miles.per.day + log(Miles.per.day)
                        + Avgdays.week + I(Avgdays.week^2)
                        + Pct.drive.mon + I(Pct.drive.mon^2)
                        + Pct.drive.tue + I(Pct.drive.tue^2)
                        + Pct.drive.wed + I(Pct.drive.wed^2)
                        + Pct.drive.thr + I(Pct.drive.thr^2)
                        + Pct.drive.fri + I(Pct.drive.fri^2)
                        + Pct.drive.sat + I(Pct.drive.sat^2)
                        + Pct.drive.sun + I(Pct.drive.sun^2)
                        + Pct.drive.wkend + I(Pct.drive.wkend^2)
                        + max.day + log(max.day) 
                        + min.day + I(min.day^2)
                        + max.min + I(max.min^2)
                        + Pct.drive.rush.am + sqrt(Pct.drive.rush.am) 
                        + Pct.drive.rush.pm + sqrt(Pct.drive.rush.pm)   
                        + Pct.drive.2hrs + sqrt(Pct.drive.2hrs) 
                        + Pct.drive.3hrs + sqrt(Pct.drive.3hrs) 
                        + Pct.drive.4hrs + sqrt(Pct.drive.4hrs) 
                        + Accel.06miles + I(Accel.06miles^2) + I(Accel.06miles^3)
                        + Accel.08miles + I(Accel.08miles^2) + I(Accel.08miles^3)
                        + Accel.09miles + I(Accel.09miles^2) + I(Accel.09miles^3)
                        + Accel.11miles + I(Accel.11miles^2) + I(Accel.11miles^3)
                        + Accel.12miles + I(Accel.12miles^2) + I(Accel.12miles^3)
                        + Accel.14miles + I(Accel.14miles^2) + I(Accel.14miles^3)
                        + Brake.06miles + I(Brake.06miles^2) + I(Brake.06miles^3)
                        + Brake.08miles + I(Brake.08miles^2) + I(Brake.08miles^3)
                        + Brake.09miles + I(Brake.09miles^2) + I(Brake.09miles^3)
                        + Brake.11miles + I(Brake.11miles^2) + I(Brake.11miles^3)
                        + Brake.12miles + I(Brake.12miles^2) + I(Brake.12miles^3)
                        + Brake.14miles + I(Brake.14miles^2) + I(Brake.14miles^3)
                        + Left.turn.intensity08 + log1p(Left.turn.intensity08)
                        + Left.turn.intensity09 + log1p(Left.turn.intensity09)
                        + Left.turn.intensity10 + log1p(Left.turn.intensity10)
                        + Left.turn.intensity11 + log1p(Left.turn.intensity11)
                        + Left.turn.intensity12 + log1p(Left.turn.intensity12)
                        + Right.turn.intensity08 + log1p(Right.turn.intensity08)
                        + Right.turn.intensity09 + log1p(Right.turn.intensity09)
                        + Right.turn.intensity10 + log1p(Right.turn.intensity10)
                        + Right.turn.intensity11 + log1p(Right.turn.intensity11)
                        + Right.turn.intensity12 + log1p(Right.turn.intensity12))


matrix.x <- model.matrix(glm.score, data=train2)[,-1]
y <- train2$NB_Claim
offset <- log(train2$expo)

lambda.min <- 3.981072e-05
lambda.1se <- 0.001258925

lambda.select <- lambda.min
fit <- glmnet(matrix.x, y, family = "poisson", relax=FALSE, offset = offset, alpha = 1, lambda = lambda.select)
#fit <- glmnet(matrix.x, y, family = "poisson", relax=TRUE, offset = offset, alpha = 1, lambda = lambda.select)

train2$pred.tele <- predict(fit, newx = matrix.x, type='response', newoffset=offset, lambda = lambda.select)

matrix.x <- model.matrix(glm.score, data=test2)[,-1]
y <- test2$NB_Claim
offset <- log(test2$expo)

test2$pred.base <- predict(fit, newx = matrix.x, type='response', newoffset=offset, lambda = lambda.select)

Result_ <- data.frame(t(Score.pred(test2$pred.base, test2$NB_Claim)/nrow(test2)))
Result_ <- cbind('LASSO (optimal)', Result_)
colnames(Result_) <- c("Model", "Sc.log", "Sc.MSE", "Sc.quad", "Sc.sph", "Sc.DSS", "Sc.CRPS")

Result_all <- rbind(Result_all, Result_)

knitr::kable(Result_all, align = "ccccccc", digits = c(0, 5, 5, 5, 5, 5, 5), format.args = list(big.mark = ","))%>%   
  kable_styling(bootstrap_options = "striped", full_width = T)  




# XGBOOST

#We can calculate the model’s prediction scores based on 
# all classical and telematics covariates. 
# The XGBoost approach is particularly effective in 
# capturing the effect of all available telematic covariates. 
# Indeed,the scores obtained are significantly improved compared to other tested approaches.
 

library(xgboost)
library(Ckmeans.1d.dp)
library(SHAPforxgboost)

## Bayesian Optimization (FREQUENCY)

## data praparation
all.vars2 <- c("Car.use", "Region", "Car.age", "Years.noclaims",
               "Miles.per.day", "Avgdays.week",
               "Pct.drive.mon", "Pct.drive.tue", "Pct.drive.wed", "Pct.drive.thr", "Pct.drive.fri", "Pct.drive.sat", "Pct.drive.sun",
               "max.day", "min.day", "max.min", "Dayformax", "Dayformin",
               "Pct.drive.rush.am", "Pct.drive.rush.pm",
               "Pct.drive.wkend",
               "Pct.drive.2hrs", "Pct.drive.3hrs", "Pct.drive.4hrs",
               "Accel.06miles", "Accel.08miles", "Accel.09miles", "Accel.11miles", "Accel.12miles", "Accel.14miles", 
               "Brake.06miles", "Brake.08miles", "Brake.09miles", "Brake.11miles", "Brake.12miles", "Brake.14miles", 
               "Left.turn.intensity08", "Left.turn.intensity09", "Left.turn.intensity10", "Left.turn.intensity11", "Left.turn.intensity12",
               "Right.turn.intensity08", "Right.turn.intensity09", "Right.turn.intensity10", "Right.turn.intensity11", "Right.turn.intensity12")


#  Bayesian Optimization on XGBoost 

# Automatically optimize XGBoost parameters 
# to model a Poisson model for claims frequency,
# using bayesian cross-validation and optimization,
# to obtain the best model with respect to negative log-likelihood (Poisson-NLL).

dtrain <- xgb.DMatrix(data = data.matrix(train2[, paste(all.vars2)]), label = train2$NB_Claim)
setinfo(dtrain,"base_margin",log(train2$expo))
folds <-list(fold1 = which(train2$fold == 1),
             fold2 = which(train2$fold == 2),
             fold3 = which(train2$fold == 3),
             fold4 = which(train2$fold == 4),
             fold5 = which(train2$fold == 5))

bounds <- list(eta = c(0.001, 0.5),
               max_depth = c(1L, 50L),
               subsample = c(0.1, 1),
               min_child_weight = c(1, 175))

obj_func <- function(eta, max_depth, subsample, min_child_weight) {
  param <- list(
    eta = eta,
    max_depth = max_depth,
    subsample = subsample,
    min_child_weight = min_child_weight,
    booster = "gbtree",
    objective = "count:poisson",
    eval_metric = "poisson-nloglik")
  
  set.seed(533)
  xgbcv <- xgb.cv(params = param,
                  nrounds = base.rounds,
                  data = dtrain,
                  folds = folds,
                  prediction = TRUE,
                  early_stopping_rounds = 10,
                  verbose = 0,
                  maximize = F)
  
  lst <- list(
    Score = -min(xgbcv$evaluation_log$test_poisson_nloglik_mean),
    nrounds = xgbcv$best_iteration
  )
  
  return(lst)
}



base.rounds <- 30
set.seed(533)
bayes_out <- bayesOpt(FUN = obj_func, bounds = bounds, initPoints = length(bounds) + 2, iters.n = 5)
comp <- bayes_out$scoreSummary[which(bayes_out$scoreSummary$Score== max(bayes_out$scoreSummary$Score))]
comp 



#####################################################################
### Manual optimization using the same parameters################## 
## to confirm the correctness of the optimization procedure #########
#####################################################################

 obj_func(eta=comp$eta, max_depth=comp$max_depth, subsample=comp$subsample, min_child_weight=comp$min_child_weight)

param <- list(
  eta = comp$eta,
  max_depth = as.integer(comp$max_depth),
  subsample = comp$subsample,
  min_child_weight = comp$min_child_weight,
  booster = "gbtree",
  objective = "count:poisson",
  eval_metric = "poisson-nloglik"
)

set.seed(533) 
xgbcv <- xgb.cv(params = param,
                nrounds = comp$nrounds,
                data = dtrain,
                folds = folds,
                prediction = TRUE,
                early_stopping_rounds = 10,
                verbose = 0,
                maximize = FALSE)

# score
Score <- -min(xgbcv$evaluation_log$test_poisson_nloglik_mean)
print(Score)



# # Prediction Scores

# We can calculate the model’s prediction 
# scores based on all classical and telematics covariates.
# The XGBoost approach is particularly effective in capturing
# the effect of all available telematic covariates


# Data preparation

all.vars2 <- c("Car.use", "Region", "Car.age", "Years.noclaims",
               "Miles.per.day", "Avgdays.week",
               "Pct.drive.mon", "Pct.drive.tue", "Pct.drive.wed", "Pct.drive.thr", "Pct.drive.fri", "Pct.drive.sat", "Pct.drive.sun",
               "max.day", "min.day", "max.min", "Dayformax", "Dayformin",
               "Pct.drive.rush.am", "Pct.drive.rush.pm",
               "Pct.drive.wkend",
               "Pct.drive.2hrs", "Pct.drive.3hrs", "Pct.drive.4hrs",
               "Accel.06miles", "Accel.08miles", "Accel.09miles", "Accel.11miles", "Accel.12miles", "Accel.14miles", 
               "Brake.06miles", "Brake.08miles", "Brake.09miles", "Brake.11miles", "Brake.12miles", "Brake.14miles", 
               "Left.turn.intensity08", "Left.turn.intensity09", "Left.turn.intensity10", "Left.turn.intensity11", "Left.turn.intensity12",
               "Right.turn.intensity08", "Right.turn.intensity09", "Right.turn.intensity10", "Right.turn.intensity11", "Right.turn.intensity12")

dtrain <- xgb.DMatrix(data = data.matrix(train2[, paste(all.vars2)]), label = train2$NB_Claim)
setinfo(dtrain,"base_margin",log(train2$expo))
folds <-list(fold1 = which(train2$fold == 1),
             fold2 = which(train2$fold == 2),
             fold3 = which(train2$fold == 3),
             fold4 = which(train2$fold == 4),
             fold5 = which(train2$fold == 5))



### Prediction scores for the XGBoost model with telematics


param <- list(
  eta = 0.02337437,
  max_depth = 26,
  subsample = 0.8097923,
  min_child_weight = 1,
  booster = "gbtree",
  objective = "count:poisson",
  eval_metric = "poisson-nloglik")

set.seed(133)
xgbcv <- xgb.cv(params = param,
                nrounds = 367,
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



## test set

dtrain <- xgb.DMatrix(data = data.matrix(train2[, paste(all.vars2)]), label = train2$NB_Claim)
setinfo(dtrain,"base_margin",log(train2$expo))
dtest <- xgb.DMatrix(data = data.matrix(test2[, paste(all.vars2)]), label = test2$NB_Claim)
setinfo(dtest,"base_margin",log(test2$expo))
folds <-list(fold1 = which(train2$fold == 1),
             fold2 = which(train2$fold == 2),
             fold3 = which(train2$fold == 3),
             fold4 = which(train2$fold == 4),
             fold5 = which(train2$fold == 5))

param <- list(
  eta = 0.02337437,
  max_depth = 26,
  subsample = 0.8097923,
  min_child_weight = 1,
  booster = "gbtree",
  objective = "count:poisson",
  eval_metric = "poisson-nloglik")

set.seed(133)
fit.xgb <- xgb.train(params = param,
                     nrounds = 367,
                     data = dtrain)

train2$pred.xgb <- predict(fit.xgb, dtrain, type='response')


test2$pred.xgb <- predict(fit.xgb, dtest, type='response')

test2$pred.base <- test2$pred.xgb

Result_ <- data.frame(t(Score.pred(test2$pred.base, test2$NB_Claim)/nrow(test2)))
Result_ <- cbind('XGBoost', Result_)
colnames(Result_) <- c("Model", "Sc.log", "Sc.MSE", "Sc.quad", "Sc.sph", "Sc.DSS", "Sc.CRPS")

Result_all <- rbind(Result_all, Result_)

knitr::kable(Result_all, align = "ccccccc", digits = c(0, 5, 5, 5, 5, 5, 5), format.args = list(big.mark = ","))%>%   
  kable_styling(bootstrap_options = "striped", full_width = T)  




## XGBOOST ON RESIDUALS


#We repeat the same exercise we did with the GLM-Net approach: 
# fitting an XGBoost model on the residuals of the first XGBoost model

library(xgboost)
library(Ckmeans.1d.dp)
library(SHAPforxgboost)
library(pacman)


var.sens <- c("Marital", "Insured.sex", "Credit.score", "Insured.age", "Territory")    

dtrain <- xgb.DMatrix(data = data.matrix(train2[, paste(var.sens)]), label = train2$NB_Claim)
setinfo(dtrain,"base_margin",log(train2$pred.xgb))
folds <-list(fold1 = which(train2$fold == 1),
             fold2 = which(train2$fold == 2),
             fold3 = which(train2$fold == 3),
             fold4 = which(train2$fold == 4),
             fold5 = which(train2$fold == 5))

bounds <- list(eta = c(0.001, 0.5),
               max_depth = c(1L, 50L),
               subsample = c(0.1, 1),
               min_child_weight = c(1, 175))

obj_func <- function(eta, max_depth, subsample, min_child_weight) {
  param <- list(
    eta = eta,
    max_depth = max_depth,
    subsample = subsample,
    min_child_weight = min_child_weight,
    booster = "gbtree",
    objective = "count:poisson",
    eval_metric = "poisson-nloglik")
  
  set.seed(533)
  xgbcv <- xgb.cv(params = param,
                  nrounds = base.rounds,
                  data = dtrain,
                  folds = folds,
                  prediction = TRUE,
                  early_stopping_rounds = 10,
                  verbose = 0,
                  maximize = F)
  
  lst <- list(
    Score = -min(xgbcv$evaluation_log$test_poisson_nloglik_mean),
    nrounds = xgbcv$best_iteration
  )
  
  return(lst)
}

base.rounds <- 30
set.seed(254)
bayes_out <- bayesOpt(FUN = obj_func, bounds = bounds, initPoints = length(bounds) + 2, iters.n = 5)
comp <- bayes_out$scoreSummary[which(bayes_out$scoreSummary$Score== max(bayes_out$scoreSummary$Score))]
comp 

#   Epoch Iteration       eta max_depth subsample min_child_weight gpUtility acqOptimum inBounds Elapsed       Score nrounds errorMessage
#1:    10        16 0.2215803        50 0.7623535                1 0.7185566       TRUE     TRUE    7.14 -0.06200109       5           NA



## Verif ###
############
obj_func(eta=comp$eta, max_depth=comp$max_depth, subsample=comp$subsample, min_child_weight=comp$min_child_weight)

param <- list(
  eta = 0.2215803,
  max_depth = 50,
  subsample = 0.7623535,
  min_child_weight = 1,
  booster = "gbtree",
  objective = "count:poisson",
  eval_metric = "poisson-nloglik")

set.seed(533)
xgbcv <- xgb.cv(params = param,
                nrounds = 5,
                data = dtrain,
                folds = folds,
                prediction = TRUE,
                early_stopping_rounds = 10,
                verbose = 0,
                maximize = F)

-min(xgbcv$evaluation_log$test_poisson_nloglik_mean)

##############

var.sens <- c("Marital", "Insured.sex", "Credit.score", "Insured.age", "Territory")    

dtrain <- xgb.DMatrix(data = data.matrix(train2[, paste(var.sens)]), label = train2$NB_Claim)
setinfo(dtrain,"base_margin",log(train2$pred.xgb))
folds <-list(fold1 = which(train2$fold == 1),
             fold2 = which(train2$fold == 2),
             fold3 = which(train2$fold == 3),
             fold4 = which(train2$fold == 4),
             fold5 = which(train2$fold == 5))

param <- list(
  eta = 0.2215803,
  max_depth = 50,
  subsample = 0.7623535,
  min_child_weight = 1,
  booster = "gbtree",
  objective = "count:poisson",
  eval_metric = "poisson-nloglik")

set.seed(533)
fit.xgb2 <- xgb.train(params = param,
                      nrounds = 5,
                      data = dtrain)

dtest <- xgb.DMatrix(data = data.matrix(test2[, paste(var.sens)]), label = test2$NB_Claim)
setinfo(dtest,"base_margin",log(test2$pred.xgb))





# Prediction scores for the XGBoost model with telematics

test2$pred.base <- predict(fit.xgb2, dtest, type='response')

Result_ <- data.frame(t(Score.pred(test2$pred.base, test2$NB_Claim)/nrow(test2)))
Result_ <- cbind('XGBoost*', Result_)
colnames(Result_) <- c("Model", "Sc.log", "Sc.MSE", "Sc.quad", "Sc.sph", "Sc.DSS", "Sc.CRPS")

Result_all <- rbind(Result_all, Result_)

# save(Result_all, file='Data/ResultsSynth.Rda')

knitr::kable(Result_all, align = "ccccccc", digits = c(0, 5, 5, 5, 5, 5, 5), format.args = list(big.mark = ","))%>%   
  kable_styling(bootstrap_options = "striped", full_width = T)  


# importance matrix 

importance_matrix <- xgb.importance(dimnames(dtrain)[[2]], model = fit.xgb2)
xgb.ggplot.importance(importance_matrix,top_n=15) + theme(text = element_text(size=15))

