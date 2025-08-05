################# Severity - TRADITIONAL COVARIATES #######################
###########################################################################


"As mentioned in the theory review chapter, to compare models 
and strike a balance between bias and variance while avoiding 
overfitting, an interesting approach is to assess the prediction 
quality of models when applied to new data.
The following R script presents a function for calculating various 
scores."

# Models 

# Basic GLM  
# GLM familiy- Net 
# XGBOOST 




#To analyze severities, we define two scores:
  
# 1)  the negative loglikelihood based on the Gamma 
# distribution with shape parameter = 1/phi and scale = (mu)(phi),
# where mu is the expected value and phi is the dispersion parameter.

# 2)  the mean squared error (MSE) divided by 1,000,000.

# scores pred.function

Score.pred.sev <- function(mu, x, phi) {
  Sc.log  <- -sum(dgamma(x, shape = 1/phi, scale = mu*phi, log=TRUE))
  Sc.MSE  <- sum((x - mu)^2)/1000000
  return(c(Sc.log, Sc.MSE))
}



# Packages

library(tidyverse)
library(vtable)
library(rpart)
library(repr)
library(rpart.plot)
#library(rfCountData)
library(gam)
library(sjPlot)
library(glmnet)


# Data 

dataS <- read.csv('Synthetic_data.csv')

data <- dataS[dataS$AMT_Claim > 0,]
data$M_Claim <- data$AMT_Claim/data$NB_Claim

# Modifications 
data <- data %>%
  mutate(Territory = as.factor(Territory)) %>%
  select(-c('Annual.pct.driven', 'Annual.miles.drive'))

data.select <- data

# Train-test et folds
set.seed(123)
train <- data.select %>% sample_frac(0.8, replace = FALSE)
test <- data.select %>% anti_join(train)
test <- test[-640,]

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



# Basic GLM model 
### Single intercept used as a point of comparison.

## Model on each fold
Result_  <- data.frame()
Result2_  <- data.frame()
for(i in 1:nb.fold) {
  learn <- train2[train2$fold != i,]
  valid <- train2[train2$fold == i,]
  
  mean <- sum(learn$AMT_Claim)/sum(learn$NB_Claim) 
  variance <- sd(learn$AMT_Claim)^2
  phi <- variance/mean(learn$AMT_Claim)^2
  learn$pred.base <- mean*learn$NB_Claim
  valid$pred.base <- mean*valid$NB_Claim
  
  Result_ <- rbind(Result_, c(i, Score.pred.sev(valid$pred.base, valid$AMT_Claim, phi)/nrow(valid)))
  Result2_ <- rbind(Result2_, c(i, Score.pred.sev(valid$pred.base, valid$AMT_Claim, phi)))
}

## Show results
colnames(Result_) <- c('Fold', "Sc.log", "Sc.MSE")
colnames(Result2_) <- c('Fold', "Sc.log", "Sc.MSE")
tot <- colSums(Result2_)/nrow(train2)
tot$Fold <- 'Total'
Result_ <- rbind(Result_ , tot)

Result.base <- Result_  
Base <- Result.base[nb.fold+1,]

knitr::kable(Result_, align = "ccc", digits = c(0, 5, 5), format.args = list(big.mark = ","))%>%   
  kable_styling(bootstrap_options = "striped", full_width = F)  

mean <- sum(train2$AMT_Claim)/sum(train2$NB_Claim) 
variance <- sd(train2$AMT_Claim)^2
phi <- variance/mean(train2$AMT_Claim)^2 

test2$pred.base <- mean*test2$NB_Claim

Result_ <- data.frame(t(Score.pred.sev(test2$pred.base, test2$AMT_Claim, phi)/nrow(test2)))
Result_ <- cbind('Base', Result_)
colnames(Result_) <- c("Model", "Sc.log", "Sc.MSE")

Result_all <- Result_

knitr::kable(Result_all, align = "ccc", digits = c(0, 5, 5), format.args = list(big.mark = ","))%>%   
  kable_styling(bootstrap_options = "striped", full_width = F)  

## A first regression approach is attempted using only the 
# traditional categorical variables

#   Gender,\
#   Marital status,\
#   Vehicle usage,\
#   Region.


## Model 
score.base <- as.formula(M_Claim ~ 1)
score.glm <- as.formula(M_Claim ~ Insured.sex + Marital  +  Car.use + Region )

## Model on each fold
Result_  <- data.frame()
Result2_  <- data.frame()
for(i in 1:nb.fold) {
  learn <- train2[train2$fold != i,]
  valid <- train2[train2$fold == i,]
  glm.fit <- glm(score.glm, family = Gamma(link = "log"), data = learn)
  
  learn$pred.base <- predict(glm.fit, newdata=learn, type='response')*learn$NB_Claim
  valid$pred.base <- predict(glm.fit, newdata=valid, type='response')*valid$NB_Claim
  phi <- summary(glm.fit)$dispersion
  
  Result_ <- rbind(Result_, c(i, Score.pred.sev(valid$pred.base, valid$AMT_Claim, phi)/nrow(valid)))
  Result2_ <- rbind(Result2_, c(i, Score.pred.sev(valid$pred.base, valid$AMT_Claim, phi)))
}

## Model on all data from train
glm.base <- glm(score.base, family = Gamma(link = "log"), data = train2)
glm.fit <- glm(score.glm, family = Gamma(link = "log"), data = train2)
train2$pred.glm1 <- predict(glm.fit, newdata=train2, type='response')*train2$NB_Claim
Result.glm1 <- Result_  

## Show results
colnames(Result_) <- c('Fold', "Sc.log", "Sc.MSE")
colnames(Result2_) <- c('Fold', "Sc.log", "Sc.MSE")
tot <- colSums(Result2_)/nrow(train2)
tot$Fold <- 'Total'
Result_ <- rbind(Result_ , tot)
Result_ <- rbind(Result_, Base)

Result_[nb.fold+2,1] <- 'Improvement'

for(i in 2:3){
  Result_[nb.fold+2,i] <-  Result_[nb.fold+1,i] - Result_[nb.fold+2,i]
}


rownames(Result_) <- NULL
knitr::kable(Result_, align = "ccc", digits = c(0, 5, 5), format.args = list(big.mark = ","))%>%   
  kable_styling(bootstrap_options = "striped", full_width = F)  
score.glm <- as.formula(M_Claim ~ Insured.sex + Marital  +  Car.use + Region )

glm.fit <- glm(score.glm, family = Gamma(link = "log"), data = train2)
test2$pred.base <- predict(glm.fit, newdata=test2, type='response')*test2$NB_Claim
phi <- summary(glm.fit)$dispersion

Result_ <- data.frame(t(Score.pred.sev(test2$pred.base, test2$AMT_Claim, phi)/nrow(test2)))
Result_ <- cbind('GLM (trad.)', Result_)
colnames(Result_) <- c("Model", "Sc.log", "Sc.MSE")

Result_all <- rbind(Result_all, Result_)

knitr::kable(Result_all, align = "ccc", digits = c(0, 5, 5), format.args = list(big.mark = ","))%>%   
  kable_styling(bootstrap_options = "striped", full_width = F) 


### Estimated Parameters
## Model 
score.base <- as.formula(M_Claim ~ 1)
score.glm <- as.formula(M_Claim ~ Insured.sex + Marital  +  Car.use + Region)

## Model on all data from train
glm.base <- glm(score.base, family = Gamma(link = "log"), data = train2)
glm.fit <- glm(score.glm, family = Gamma(link = "log"), data = train2)

tab_model(glm.base, glm.fit, transform = NULL)

## GLM-Net
### Parametric transformation of continuous covariates
### Credit Score
min_ <- min(train2$Credit.score) 
max_ <- max(train2$Credit.score) 
by_ <-  (max_ - min_)/(nrow(train2)-1) 
add <- data.frame(seq(min_, max_, by_)) 
colnames(add) <- 'Credit.score'

db <- train2 %>%
  select(-'Credit.score') %>%
  dplyr::slice(1) 
db <- bind_rows(replicate(nrow(train2), db, simplify = FALSE))
db <- cbind(db, add)

##

score.gam <- as.formula(M_Claim ~ Insured.sex + Marital  +  Car.use + Region
                        + s(Credit.score) )
score.glm <- as.formula(M_Claim ~ Insured.sex + Marital  +  Car.use + Region
                        + Credit.score +  I(Credit.score^2))

gam.fit <- gam(score.gam, family = Gamma(link = "log"), data = train2)
glm.fit <- glm(score.glm, family = Gamma(link = "log"), data = train2)

db$pred.gam <- predict(gam.fit, newdata=db, type='response')*db$NB_Claim
db$pred.glm <- predict(glm.fit, newdata=db, type='response')*db$NB_Claim
base <- db %>%
  mutate(diff = abs(Credit.score - mean(train2$Credit.score))) %>%
  filter(diff == min(diff))
db$pred.gam <- db$pred.gam/base$pred.gam[1]
db$pred.glm <- db$pred.glm/base$pred.glm[1]

ggplot()+
  geom_line(aes(x=Credit.score, y=pred.gam, color='GAM'), data=db) + 
  geom_line(aes(x=Credit.score, y=pred.glm, color='Parametric GLM'), data=db) +
  guides(color = guide_legend(title = "")) +
  labs(x = 'Credit Score',
       y = 'Relativity') +
  theme_classic()

### Age of the insured
min_ <- min(train2$Insured.age) 
max_ <- max(train2$Insured.age) 
by_ <-  (max_ - min_)/(nrow(train2)-1) 
add <- data.frame(seq(min_, max_, by_)) 
colnames(add) <- 'Insured.age'

db <- train2 %>%
  select(-'Insured.age') %>%
  dplyr::slice(1) 
db <- bind_rows(replicate(nrow(train2), db, simplify = FALSE))
db <- cbind(db, add)

##

score.gam <- as.formula(M_Claim ~ Insured.sex + Marital  +  Car.use + Region
                        + s(Insured.age) )
score.glm <- as.formula(M_Claim ~ Insured.sex + Marital  +  Car.use + Region
                        + Insured.age  + I(Insured.age^2)  )

gam.fit <- gam(score.gam, family = Gamma(link = "log"), data = train2)
glm.fit <- glm(score.glm, family = Gamma(link = "log"), data = train2)

db$pred.gam <- predict(gam.fit, newdata=db, type='response')*db$NB_Claim
db$pred.glm <- predict(glm.fit, newdata=db, type='response')*db$NB_Claim
base <- db %>%
  mutate(diff = abs(Insured.age - mean(train2$Insured.age))) %>%
  filter(diff == min(diff))
db$pred.gam <- db$pred.gam/base$pred.gam[1]
db$pred.glm <- db$pred.glm/base$pred.glm[1]

ggplot()+
  geom_line(aes(x=Insured.age, y=pred.gam, color='GAM'), data=db) + 
  geom_line(aes(x=Insured.age, y=pred.glm, color='Parametric GLM'), data=db) +
  guides(color = guide_legend(title = "")) +
  labs(x = 'Age of the insured',
       y = 'Relativity') +
  theme_classic()

### Age of the car
min_ <- min(train2$Car.age) 
max_ <- max(train2$Car.age) 
by_ <-  (max_ - min_)/(nrow(train2)-1) 
add <- data.frame(seq(min_, max_, by_)) 
colnames(add) <- 'Car.age'

db <- train2 %>%
  select(-'Car.age') %>%
  dplyr::slice(1) 
db <- bind_rows(replicate(nrow(train2), db, simplify = FALSE))
db <- cbind(db, add)

##

score.gam <- as.formula(M_Claim ~ Insured.sex + Marital  +  Car.use + Region
                        + s(Car.age) )
score.glm <- as.formula(M_Claim ~ Insured.sex + Marital  +  Car.use + Region
                        + Car.age + I(Car.age^2)+ I(Car.age^3) )

gam.fit <- gam(score.gam, family = Gamma(link = "log"), data = train2)
glm.fit <- glm(score.glm, family = Gamma(link = "log"), data = train2)

db$pred.gam <- predict(gam.fit, newdata=db, type='response')*db$NB_Claim
db$pred.glm <- predict(glm.fit, newdata=db, type='response')*db$NB_Claim
base <- db %>%
  mutate(diff = abs(Car.age - mean(train2$Car.age))) %>%
  filter(diff == min(diff))
db$pred.gam <- db$pred.gam/base$pred.gam[1]
db$pred.glm <- db$pred.glm/base$pred.glm[1]

ggplot()+
  geom_line(aes(x=Car.age, y=pred.gam, color='GAM'), data=db) + 
  geom_line(aes(x=Car.age, y=pred.glm, color='Parametric GLM'), data=db) +
  guides(color = guide_legend(title = "")) +
  labs(x = 'Age of the car',
       y = 'Relativity') +
  theme_classic()

### Years without claims
min_ <- min(train2$Years.noclaims) 
max_ <- max(train2$Years.noclaims) 
by_ <-  (max_ - min_)/(nrow(train2)-1) 
add <- data.frame(seq(min_, max_, by_)) 
colnames(add) <- 'Years.noclaims'

db <- train2 %>%
  select(-'Years.noclaims') %>%
  dplyr::slice(1) 
db <- bind_rows(replicate(nrow(train2), db, simplify = FALSE))
db <- cbind(db, add)

##

score.gam <- as.formula(M_Claim ~ Insured.sex + Marital  +  Car.use + Region 
                        + s(Years.noclaims) )
score.glm <- as.formula(M_Claim ~ Insured.sex + Marital  +  Car.use + Region
                        + Years.noclaims + I(Years.noclaims^2)  + I(Years.noclaims^3) )

gam.fit <- gam(score.gam, family = Gamma(link = "log"), data = train2)
glm.fit <- glm(score.glm, family = Gamma(link = "log"), data = train2)

db$pred.gam <- predict(gam.fit, newdata=db, type='response')*db$NB_Claim
db$pred.glm <- predict(glm.fit, newdata=db, type='response')*db$NB_Claim
base <- db %>%
  mutate(diff = abs(Years.noclaims - mean(train2$Years.noclaims))) %>%
  filter(diff == min(diff))
db$pred.gam <- db$pred.gam/base$pred.gam[1]
db$pred.glm <- db$pred.glm/base$pred.glm[1]

ggplot()+
  geom_line(aes(x=Years.noclaims, y=pred.gam, color='GAM'), data=db) + 
  geom_line(aes(x=Years.noclaims, y=pred.glm, color='Parametric GLM'), data=db) +
  guides(color = guide_legend(title = "")) +
  labs(x = 'Years.noclaims',
       y = 'Relativity') +
  theme_classic()

### Territory
# Mean Encoding with White Noise pour les territoires
cardi <- length(unique(train$Territory))

enc.terr <- train2 %>%
  group_by(Territory) %>%
  summarize(sev = sum(AMT_Claim)/sum(NB_Claim)) %>%
  arrange(sev) %>%
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

##  
min_ <- min(train2$terr.code) 
max_ <- max(train2$terr.code) 
by_ <-  (max_ - min_)/(nrow(train2)-1) 
add <- data.frame(seq(min_, max_, by_)) 
colnames(add) <- 'terr.code'

db <- train2 %>%
  select(-'terr.code') %>%
  dplyr::slice(1) 
db <- bind_rows(replicate(nrow(train2), db, simplify = FALSE))
db <- cbind(db, add)

##

score.gam <- as.formula(M_Claim ~ Insured.sex + Marital  +  Car.use + Region 
                        + s(terr.code) )
score.glm <- as.formula(M_Claim ~ Insured.sex + Marital  +  Car.use + Region
                        + terr.code + I(terr.code^2)  + I(terr.code^3) )

gam.fit <- gam(score.gam, family = Gamma(link = "log"), data = train2)
glm.fit <- glm(score.glm, family = Gamma(link = "log"), data = train2)

db$pred.gam <- predict(gam.fit, newdata=db, type='response')*db$NB_Claim
db$pred.glm <- predict(glm.fit, newdata=db, type='response')*db$NB_Claim
base <- db %>%
  mutate(diff = abs(terr.code - mean(train2$terr.code))) %>%
  filter(diff == min(diff))
db$pred.gam <- db$pred.gam/base$pred.gam[1]
db$pred.glm <- db$pred.glm/base$pred.glm[1]

ggplot()+
  geom_line(aes(x=terr.code, y=pred.gam, color='GAM'), data=db) + 
  geom_line(aes(x=terr.code, y=pred.glm, color='Parametric GLM'), data=db) +
  guides(color = guide_legend(title = "")) +
  labs(x = 'terr.code',
       y = 'Relativity') +
  theme_classic()

### Fitting the GLM-Net model
glm.score <- as.formula(M_Claim ~ Insured.sex + Marital  +  Car.use + Region 
                        + Credit.score +  I(Credit.score^2) 
                        + Insured.age  + I(Insured.age^2) 
                        + Car.age + I(Car.age^2) + I(Car.age^3)
                        + Years.noclaims + I(Years.noclaims^2)  + I(Years.noclaims^3) 
                        + terr.code + I(terr.code^2)  + I(terr.code^3) )

matrix.x <- model.matrix(glm.score, data=train2)[,-1]
y <- train2$M_Claim
fold.id <- train2$fold

lambda_seq <- c(10^seq(0, -8, by = -.1), 0)
cvfit0  <- cv.glmnet(matrix.x, y, relax=FALSE, family = Gamma(link = "log"), lambda = lambda_seq, foldid = fold, alpha = 0)
cvfit.2 <- cv.glmnet(matrix.x, y, relax=FALSE, family = Gamma(link = "log"), lambda = lambda_seq, foldid = fold, alpha = 0.2)
cvfit.4 <- cv.glmnet(matrix.x, y, relax=FALSE, family = Gamma(link = "log"), lambda = lambda_seq, foldid = fold, alpha = 0.4)
cvfit.6 <- cv.glmnet(matrix.x, y, relax=FALSE, family = Gamma(link = "log"), lambda = lambda_seq, foldid = fold, alpha = 0.6)
cvfit.8 <- cv.glmnet(matrix.x, y, relax=FALSE, family = Gamma(link = "log"), lambda = lambda_seq, foldid = fold, alpha = 0.8)
cvfit1  <- cv.glmnet(matrix.x, y, relax=FALSE, family = Gamma(link = "log"), lambda = lambda_seq, foldid = fold, alpha = 1)

c(cvfit0$lambda.min, cvfit.2$lambda.min, cvfit.4$lambda.min, cvfit.6$lambda.min, cvfit.8$lambda.min, cvfit1$lambda.min)

all.min <- data.frame(c(min(cvfit0$cvm), min(cvfit.2$cvm), min(cvfit.4$cvm), min(cvfit.6$cvm), min(cvfit.8$cvm), min(cvfit1$cvm))) %>%
  mutate(alpha = 2*(row_number()-1)/10)
colnames(all.min)[1] <- 'min' 
all.min %>% filter(min == min(min))

cvfit1$lambda.min
cvfit1$lambda.1se

### Optimal value
glm.score <- as.formula(M_Claim ~ Insured.sex + Marital  +  Car.use + Region
                        + Credit.score +  I(Credit.score^2) 
                        + Insured.age + I(Insured.age^2) 
                        + Car.age + I(Car.age^2) + I(Car.age^3)
                        + Years.noclaims + I(Years.noclaims^2)  + I(Years.noclaims^3) 
                        + terr.code + I(terr.code^2)  + I(terr.code^3) )

Result_  <- data.frame()
Result2_  <- data.frame()
for(i in 1:nb.fold) {
  learn <- train2[train2$fold != i,]
  valid <- train2[train2$fold == i,]
  
  matrix.x <- model.matrix(glm.score, data=learn)[,-1]
  y <- learn$M_Claim
  
  lambda.min <- 0.01995262
  lambda.1se <- 0.07943282
  
  lambda.select <- lambda.min
  fit <- glmnet(matrix.x, y, family = Gamma(link = "log"), relax=FALSE, alpha = 0.6, lambda = lambda.select)
  learn$pred <- predict(fit, newx = matrix.x, type='response', lambda = lambda.select)*learn$NB_Claim
  
  
  matrix.x <- model.matrix(glm.score, data=valid)[,-1]
  y <- valid$M_Claim
  
  valid$pred <- predict(fit, newx = matrix.x, type='response', lambda = lambda.select)*valid$NB_Claim
  variance <- (sum((learn$AMT_Claim - learn$pred)^2)/(nrow(learn) - length(fit$beta)))
  phi <- variance/mean(learn$AMT_Claim)^2
  
  Result_ <- rbind(Result_, c(i, Score.pred.sev(valid$pred, valid$AMT_Claim, phi)/nrow(valid)))
  Result2_ <- rbind(Result2_, c(i, Score.pred.sev(valid$pred, valid$AMT_Claim, phi)))
}


## Show results
colnames(Result_) <- c('Fold', "Sc.log", "Sc.MSE")
colnames(Result2_) <- c('Fold', "Sc.log", "Sc.MSE")
tot <- colSums(Result2_)/nrow(train2)
tot$Fold <- 'Total'
Result_ <- rbind(Result_ , tot)
Result_ <- rbind(Result_, Base)

Result_[nb.fold+2,1] <- 'Improvement'

for(i in 2:3){
  Result_[nb.fold+2,i] <-  Result_[nb.fold+1,i] - Result_[nb.fold+2,i]
}


rownames(Result_) <- NULL
knitr::kable(Result_, align = "ccc", digits = c(0, 5, 5), format.args = list(big.mark = ","))%>%   
  kable_styling(bootstrap_options = "striped", full_width = F)  

## 
glm.score <- as.formula(M_Claim ~ Insured.sex + Marital  +  Car.use + Region
                        + Credit.score +  I(Credit.score^2) 
                        + Insured.age + I(Insured.age^2) 
                        + Car.age + I(Car.age^2) + I(Car.age^3)
                        + Years.noclaims + I(Years.noclaims^2)  + I(Years.noclaims^3) 
                        + terr.code + I(terr.code^2)  + I(terr.code^3) )

matrix.x <- model.matrix(glm.score, data=train2)[,-1]
y <- train2$M_Claim

lambda.min <- 0.01995262
lambda.1se <- 0.07943282

lambda.select <- lambda.min
fit <- glmnet(matrix.x, y, family = Gamma(link = "log"), relax=FALSE, alpha = 0.6, lambda = lambda.select)
#fit <- glmnet(matrix.x, y, family = "poisson", relax=TRUE, offset = offset, alpha = 0.6, lambda = lambda.select)

train2$pred <- predict(fit, newx = matrix.x, type='response', lambda = lambda.select)*train2$NB_Claim
train2$pred.tele <- train2$pred

matrix.x <- model.matrix(glm.score, data=test2)[,-1]
y <- test2$M_Claim

test2$pred.base <- predict(fit, newx = matrix.x, type='response', lambda = lambda.select)*test2$NB_Claim
variance <- (sum((train2$AMT_Claim - train2$pred)^2)/(nrow(train2) - length(fit$beta)))
phi <- variance/mean(train2$AMT_Claim)^2

Result_ <- data.frame(t(Score.pred.sev(test2$pred.base, test2$AMT_Claim, phi)/nrow(test2)))
Result_ <- cbind('LASSO (optimal)', Result_)
colnames(Result_) <- c("Model", "Sc.log", "Sc.MSE")

Result_all <- rbind(Result_all, Result_)

knitr::kable(Result_all, align = "ccc", digits = c(0, 5, 5), format.args = list(big.mark = ","))%>%   
  kable_styling(bootstrap_options = "striped", full_width = F)  

### Parsimonious model
Result_  <- data.frame()
Result2_  <- data.frame()
for(i in 1:nb.fold) {
  learn <- train2[train2$fold != i,]
  valid <- train2[train2$fold == i,]
  
  matrix.x <- model.matrix(glm.score, data=learn)[,-1]
  y <- learn$M_Claim
  
  lambda.min <- 0.01995262
  lambda.1se <- 0.07943282
  
  lambda.select <- lambda.1se
  fit <- glmnet(matrix.x, y, family = Gamma(link = "log"), relax=TRUE, alpha = 0.6, lambda = lambda.select)
  learn$pred <- predict(fit, newx = matrix.x, type='response', lambda = lambda.select)*learn$NB_Claim
  
  matrix.x <- model.matrix(glm.score, data=valid)[,-1]
  y <- valid$M_Claim
  
  
  valid$pred <- predict(fit, newx = matrix.x, type='response', lambda = lambda.select)*valid$NB_Claim
  variance <- (sum((learn$AMT_Claim - learn$pred)^2)/(nrow(learn) - length(fit$beta)))
  phi <- variance/mean(learn$AMT_Claim)^2
  
  Result_ <- rbind(Result_, c(i, Score.pred.sev(valid$pred, valid$AMT_Claim, phi)/nrow(valid)))
  Result2_ <- rbind(Result2_, c(i, Score.pred.sev(valid$pred, valid$AMT_Claim, phi)))
}


## Show results
colnames(Result_) <- c('Fold', "Sc.log", "Sc.MSE")
colnames(Result2_) <- c('Fold', "Sc.log", "Sc.MSE")
tot <- colSums(Result2_)/nrow(train2)
tot$Fold <- 'Total'
Result_ <- rbind(Result_ , tot)
Result_ <- rbind(Result_, Base)

Result_[nb.fold+2,1] <- 'Improvement'

for(i in 2:3){
  Result_[nb.fold+2,i] <-  Result_[nb.fold+1,i] - Result_[nb.fold+2,i]
}


rownames(Result_) <- NULL
knitr::kable(Result_, align = "ccc", digits = c(0, 5, 5), format.args = list(big.mark = ","))%>%   
  kable_styling(bootstrap_options = "striped", full_width = F)  

##
glm.score <- as.formula(M_Claim ~ Insured.sex + Marital  +  Car.use + Region
                        + Credit.score +  I(Credit.score^2) 
                        + Insured.age + I(Insured.age^2) 
                        + Car.age + I(Car.age^2) + I(Car.age^3)
                        + Years.noclaims + I(Years.noclaims^2)  + I(Years.noclaims^3) 
                        + terr.code + I(terr.code^2)  + I(terr.code^3) )

matrix.x <- model.matrix(glm.score, data=train2)[,-1]
y <- train2$M_Claim

lambda.min <- 0.01995262
lambda.1se <- 0.07943282

lambda.select <- lambda.1se
#fit <- glmnet(matrix.x, y, family = Gamma(link = "log"), relax=FALSE, alpha = 0.6, lambda = lambda.select)
fit <- glmnet(matrix.x, y, family = Gamma(link = "log"), relax=TRUE, alpha = 0.6, lambda = lambda.select)

train2$pred <- predict(fit, newx = matrix.x, type='response', lambda = lambda.select)*train2$NB_Claim
train2$pred.tele <- train2$pred

matrix.x <- model.matrix(glm.score, data=test2)[,-1]
y <- test2$M_Claim

test2$pred.base <- predict(fit, newx = matrix.x, type='response', lambda = lambda.select)*test2$NB_Claim
variance <- (sum((train2$AMT_Claim - train2$pred)^2)/(nrow(train2) - length(fit$beta)))
phi <- variance/mean(train2$AMT_Claim)^2

Result_ <- data.frame(t(Score.pred.sev(test2$pred.base, test2$AMT_Claim, phi)/nrow(test2)))
Result_ <- cbind('LASSO (parsimonious)', Result_)
colnames(Result_) <- c("Model", "Sc.log", "Sc.MSE")

Result_all <- rbind(Result_all, Result_)

knitr::kable(Result_all, align = "ccc", digits = c(0, 5, 5), format.args = list(big.mark = ","))%>%   
  kable_styling(bootstrap_options = "striped", full_width = F)  


## XGBoost

library(xgboost)
library(ParBayesianOptimization)


trad.vars <- c("Marital", "Car.use", "Region", "Insured.sex", "Credit.score", "Insured.age", "Car.age", "Years.noclaims", "Territory") 

dtrain <- xgb.DMatrix(data = data.matrix(train2[, paste(trad.vars)]), label = train2$M_Claim)
# setinfo(dtrain,"base_margin",log(train2$expo))
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
    objective = "reg:gamma",
    eval_metric = "gamma-nloglik")
  
  set.seed(333)
  xgbcv <- xgb.cv(params = param,
                  nrounds = base.rounds,
                  data = dtrain,
                  folds = folds,
                  prediction = TRUE,
                  early_stopping_rounds = 10,
                  verbose = 0,
                  maximize = F)
  
  lst <- list(
    Score = -min(xgbcv$evaluation_log$test_gamma_nloglik_mean),
    nrounds = xgbcv$best_iteration
  )
  
  return(lst)
}

base.rounds <- 200
set.seed(1234)

bayes_out <- bayesOpt(FUN = obj_func, bounds = bounds, initPoints = length(bounds) + 2, iters.n = 3)
comp <- bayes_out$scoreSummary[which(bayes_out$scoreSummary$Score== max(bayes_out$scoreSummary$Score))]
comp 

######################
library(pacman)

# p_load automatically installs packages if needed
p_load(xgboost, ParBayesianOptimization, mlbench, dplyr, skimr, recipes, resample)

trad.vars <- c("Marital", "Car.use", "Region", "Insured.sex", "Credit.score", "Insured.age", "Car.age", "Years.noclaims", "Territory") 

dtrain <- xgb.DMatrix(data = data.matrix(train2[, paste(trad.vars)]), label = train2$M_Claim)
# setinfo(dtrain,"base_margin",log(train2$expo))
folds <-list(fold1 = which(train2$fold == 1),
             fold2 = which(train2$fold == 2),
             fold3 = which(train2$fold == 3),
             fold4 = which(train2$fold == 4),
             fold5 = which(train2$fold == 5))

bounds <- list(eta = c(0.01, 0.2),
               max_depth = c(1L, 10L),
               subsample = c(0.1, 0.5),
               min_child_weight = c(1, 175))

obj_func <- function(eta, max_depth, subsample, min_child_weight) {
  param <- list(
    eta = eta,
    max_depth = max_depth,
    subsample = subsample,
    min_child_weight = min_child_weight,
    booster = "gbtree",
    objective = "reg:gamma",
    eval_metric = "gamma-nloglik")
  
  set.seed(233)
  xgbcv <- xgb.cv(params = param,
                  nrounds = base.rounds,
                  data = dtrain,
                  folds = folds,
                  prediction = TRUE,
                  early_stopping_rounds = 10,
                  verbose = 0,
                  maximize = F)
  
  lst <- list(
    Score = -min(xgbcv$evaluation_log$test_gamma_nloglik_mean),
    nrounds = xgbcv$best_iteration
  )
  
  return(lst)
}

base.rounds <- 200
set.seed(1234)
bayes_out <- bayesOpt(FUN = obj_func, bounds = bounds, initPoints = length(bounds) + 2, iters.n = 5)
comp <- bayes_out$scoreSummary[which(bayes_out$scoreSummary$Score== max(bayes_out$scoreSummary$Score))]
comp 

# Best Parameters

xgb_params <- list(
  booster = "gbtree",
  objective = "reg:gamma",
  eval_metric = "gamma-nloglik",
  eta = comp$eta,
  max_depth = as.integer(comp$max_depth),
  subsample = comp$subsample,
  min_child_weight = comp$min_child_weight
)
xgb_params 

nrounds <- comp$nrounds

nrounds

### Prediction Scores

library(xgboost)
library(Ckmeans.1d.dp)
library(SHAPforxgboost)


trad.vars <- c("Marital", "Car.use", "Region", "Insured.sex", "Credit.score", "Insured.age", "Car.age", "Years.noclaims", "Territory") 

dtrain <- xgb.DMatrix(data = data.matrix(train2[, paste(trad.vars)]), label = train2$M_Claim)
# setinfo(dtrain,"base_margin",log(train2$expo))
folds <-list(fold1 = which(train2$fold == 1),
             fold2 = which(train2$fold == 2),
             fold3 = which(train2$fold == 3),
             fold4 = which(train2$fold == 4),
             fold5 = which(train2$fold == 5))

## Prediction scores for the XGBoost model (severity)

param <- list(
  eta = comp$eta ,
  max_depth = comp$max_depth,
  subsample = comp$subsample,
  min_child_weight = comp$min_child_weight,
  booster = "gbtree",
  objective = "reg:gamma",
  eval_metric = "gamma-nloglik")




set.seed(333)
xgbcv <- xgb.cv(params = param,
                nrounds = comp$nrounds,
                data = dtrain,
                folds = folds,
                prediction = TRUE,
                early_stopping_rounds = 10,
                verbose = 0,
                maximize = F)


variance <- sapply(xgbcv$folds, function(x){sum((train2$AMT_Claim[x]-unlist(xgbcv$pred[x])*train2$NB_Claim[x])^2)/length(train2$AMT_Claim[x])})  
mean <- sapply(xgbcv$folds, function(x){mean(train2$AMT_Claim[x])})
phi <- unlist(variance)/mean^2

Sc.log1 <- -dgamma(train2$AMT_Claim[xgbcv$folds$fold1], shape = 1/phi[1], scale = unlist(xgbcv$pred[xgbcv$folds$fold1])*train2$NB_Claim[xgbcv$folds$fold1]*phi[1], log=TRUE)
Sc.log2 <- -dgamma(train2$AMT_Claim[xgbcv$folds$fold2], shape = 1/phi[2], scale = unlist(xgbcv$pred[xgbcv$folds$fold2])*train2$NB_Claim[xgbcv$folds$fold2]*phi[2], log=TRUE)
Sc.log3 <- -dgamma(train2$AMT_Claim[xgbcv$folds$fold3], shape = 1/phi[3], scale = unlist(xgbcv$pred[xgbcv$folds$fold3])*train2$NB_Claim[xgbcv$folds$fold3]*phi[3], log=TRUE)
Sc.log4 <- -dgamma(train2$AMT_Claim[xgbcv$folds$fold4], shape = 1/phi[4], scale = unlist(xgbcv$pred[xgbcv$folds$fold4])*train2$NB_Claim[xgbcv$folds$fold4]*phi[4], log=TRUE)
Sc.log5 <- -dgamma(train2$AMT_Claim[xgbcv$folds$fold5], shape = 1/phi[5], scale = unlist(xgbcv$pred[xgbcv$folds$fold5])*train2$NB_Claim[xgbcv$folds$fold5]*phi[5], log=TRUE)

Sc.MSE <- sapply(xgbcv$folds, function(x){(train2$AMT_Claim[x]-unlist(xgbcv$pred[x])*train2$NB_Claim[x])^2/1000000})


Result_  <- rbind(
  c(1,mean(Sc.log1), mean(Sc.MSE[1]$fold1)),
  c(2,mean(Sc.log2), mean(Sc.MSE[2]$fold2)),
  c(3,mean(Sc.log3), mean(Sc.MSE[3]$fold3)),
  c(4,mean(Sc.log4), mean(Sc.MSE[4]$fold4)),
  c(5,mean(Sc.log5), mean(Sc.MSE[5]$fold5))
)

Res.sum  <- rbind(
  c(sum(Sc.log1), sum(Sc.MSE[1]$fold1)),
  c(sum(Sc.log2), sum(Sc.MSE[2]$fold2)),
  c(sum(Sc.log3), sum(Sc.MSE[3]$fold3)),
  c(sum(Sc.log4), sum(Sc.MSE[4]$fold4)),
  c(sum(Sc.log5), sum(Sc.MSE[5]$fold5))
)
sum <- c('Total', colSums(Res.sum)/nrow(train2))

Result_  <- data.frame(rbind(Result_, sum)) 

## Show results
colnames(Result_) <- c('Fold', "Sc.log", "Sc.MSE")
Result_ <- rbind(Result_, Base)

Result_[nb.fold+2,1] <- 'Improvement'


for(i in 2:3){
  Result_[,i] <- as.numeric(Result_[,i])  
  Result_[nb.fold+2,i] <-  Result_[nb.fold+1,i] - Result_[nb.fold+2,i]
}

rownames(Result_) <- NULL
knitr::kable(Result_, align = "ccc", digits = c(0, 5, 5), format.args = list(big.mark = ","))%>%   
  kable_styling(bootstrap_options = "striped", full_width = F)  


"The same model can be used to compute 
the scores on the *test* set."

#### Sore on test set

param <- list(
  eta = comp$eta,
  max_depth = comp$max_depth,
  subsample = comp$subsample,
  min_child_weight = comp$ min_child_weight,
  booster = "gbtree",
  objective = "reg:gamma",
  eval_metric = "gamma-nloglik")

set.seed(633)
fit.xgb <- xgb.train(params = param,
                     nrounds = comp$nrounds,
                     data = dtrain)

train2$pred.xgb <- predict(fit.xgb, dtrain, type='response')*train2$NB_Claim
train2$pred.xgb.off <- predict(fit.xgb, dtrain, type='response')

dtest <- xgb.DMatrix(data = data.matrix(test2[, paste(trad.vars)]), label = test2$M_Claim)
#setinfo(dtest,"base_margin",log(test2$expo))
test2$pred.xgb <- predict(fit.xgb, dtest, type='response')*test2$NB_Claim
test2$pred.xgb.off <- predict(fit.xgb, dtest, type='response')

test2$pred.base <- test2$pred.xgb

variance <- (sum((train2$pred.xgb - (train2$AMT_Claim))^2)/(length(train2$AMT_Claim)))
phi <- variance/mean(train2$AMT_Claim)^2

Result_ <- data.frame(t(Score.pred.sev(test2$pred.base, test2$AMT_Claim, phi)/nrow(test2)))
Result_ <- cbind('XGBoost', Result_)
colnames(Result_) <- c("Model", "Sc.log", "Sc.MSE")

Result_all <- rbind(Result_all, Result_)

knitr::kable(Result_all, align = "ccc", digits = c(0, 5, 5), format.args = list(big.mark = ","))%>%   
  kable_styling(bootstrap_options = "striped", full_width = F)  


### Variables Importance


param <- list(
  eta = 0.1442358,
  max_depth = 4,
  subsample = 0.4589511,
  min_child_weight = 121.1358,
  booster = "gbtree",
  objective = "reg:gamma",
  eval_metric = "gamma-nloglik")

set.seed(333)
fit.xgb <- xgb.train(params = param,
                     nrounds = 120,
                     data = dtrain,
                     #                     prediction = TRUE,
                     verbose = 0,
                     maximize = F)

importance_matrix <- xgb.importance(dimnames(dtrain)[[2]], model = fit.xgb)
xgb.ggplot.importance(importance_matrix,top_n=10) + theme(text = element_text(size=15))


