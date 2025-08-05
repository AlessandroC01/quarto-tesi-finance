######################## severity---TELEMATICS ########################
######################################################################

"Compared to the previous chapter, we are now adding telematics variables to the exercise while removing protected variables. Thus, the following five covariates are excluded, for the moment, from the analysis:  

   1) Credit.score,  
   2) Insured age,  
   3) Insured.sex,  
   4) Marital, and  
   5) Territory. "

"We use the same two main models as in the previous chapters, 
namely Generalized Linear Model (GLM) family, including elastic-net 
and XGBoost. For each model, the response variable is the average cost 
of a claim, given that at least one claim has occurred.  
To analyze severities, we still use the same two scores used previously"


# VARIABLES
trad.vars2 <- c("Car.use", "Region", "Car.age", "Years.noclaims")
tele.var <- c("Miles.per.day", "Avgdays.week",
              "Pct.drive.mon", "Pct.drive.tue", "Pct.drive.wed", "Pct.drive.thr", "Pct.drive.fri", "Pct.drive.sat", "Pct.drive.sun",
              "max.day", "min.day", "max.min", "Dayformax", "Dayformin",
              "Pct.drive.rush.am", "Pct.drive.rush.pm",
              "Pct.drive.wkend",
              "Pct.drive.2hrs", "Pct.drive.3hrs", "Pct.drive.4hrs",
              "Accel.06miles", "Accel.08miles", "Accel.09miles", "Accel.11miles", "Accel.12miles", "Accel.14miles", 
              "Brake.06miles", "Brake.08miles", "Brake.09miles", "Brake.11miles", "Brake.12miles", "Brake.14miles", 
              "Left.turn.intensity08", "Left.turn.intensity09", "Left.turn.intensity10", "Left.turn.intensity11", "Left.turn.intensity12",
              "Right.turn.intensity08", "Right.turn.intensity09", "Right.turn.intensity10", "Right.turn.intensity11", "Right.turn.intensity12")

# PRED. SCORE FUNCTION
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
library(gam)
library(knitr)
library(kableExtra)
library(janitor)
library(glmnet)
library(scoringRules)
library(sjPlot)

# Data
dataS <- read.csv('Synthetic_data.csv')

data <- dataS[dataS$AMT_Claim > 0,]
data$M_Claim <- data$AMT_Claim/data$NB_Claim

# Modifications 
data <- data %>%
  mutate(Territory = as.factor(Territory)) %>%
  select(-c('Annual.pct.driven', 'Annual.miles.drive'))

data.select <- data

# Train-test 
set.seed(123)
train <- data.select %>% sample_frac(0.8, replace = FALSE)
test <- data.select %>% anti_join(train)

test <- test[-640,]

# DATA TRASFORMATION
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


"The model is, therefore, estimated on the entire *train* database, 
and the predictions are made on the *test* database, which was not 
used during the calibration phase."

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



### Traditional covariates already used (without protected variables)

"We construct a first GLM with only the following covariates:  

  - Car.use,  
  - Region,  
  - Car.age, and
  - Years.noclaims.  "

## Model 

## Model 
score.base <- as.formula(M_Claim ~ 1)
score.glm <- as.formula(M_Claim ~ Car.use + Region + Car.age + I(Car.age^2) 
                        + Years.noclaims + I(Years.noclaims^2))
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
phi <- summary(glm.fit)$dispersion
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

# Prediction scores for the GLM model with traditional covariates (testing set) (severity)


score.glm <- as.formula(M_Claim ~ Car.use + Region + Car.age + I(Car.age^2) 
                        + Years.noclaims + I(Years.noclaims^2))

glm.fit <- glm(score.glm, family = Gamma(link = "log"), data = train2)
test2$pred.base <- predict(glm.fit, newdata=test2, type='response')*test2$NB_Claim
phi <- summary(glm.fit)$dispersion

Result_ <- data.frame(t(Score.pred.sev(test2$pred.base, test2$AMT_Claim, phi)/nrow(test2)))
Result_ <- cbind('GLM (trad.)', Result_)
colnames(Result_) <- c("Model", "Sc.log", "Sc.MSE")

Result_all <- rbind(Result_all, Result_)

knitr::kable(Result_all, align = "ccc", digits = c(0, 5, 5), format.args = list(big.mark = ","))%>%   
  kable_styling(bootstrap_options = "striped", full_width = F)  


### Estimated parameters

## Model 
score.base <- as.formula(M_Claim ~ 1)

score.glm <- as.formula(M_Claim ~ Car.use + Region + Car.age + I(Car.age^2) 
                        + Years.noclaims + I(Years.noclaims^2))

## Model on all data from train
glm.base <- glm(score.base, family = Gamma(link = "log"), data = train2)
glm.fit <- glm(score.glm, family = Gamma(link = "log"), data = train2)

tab_model(glm.base, glm.fit, transform = NULL)


## GLM-Net
"First, we consider the GLM-net model. To make the approach as 
effective as possible, we need to adjust the continuous 
segmentation variables."

### Parametric transformation of telematic covariates

### Vehicle Usage level


"The graphs below compare the fit of the parametric 
approach with that of the GAM model"

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

score.gam <- as.formula(M_Claim ~ Car.use + Region + Car.age + I(Car.age^2) 
                        + Years.noclaims + I(Years.noclaims^2)
                        + s(Miles.per.day))

score.glm <- as.formula(M_Claim ~ Car.use + Region + Car.age + I(Car.age^2) 
                        + Years.noclaims + I(Years.noclaims^2)
                        + Miles.per.day + log(Miles.per.day))

gam.fit <- gam(score.gam, family = Gamma(link = "log"), data = train2)
glm.fit <- glm(score.glm, family = Gamma(link = "log"), data = train2)

db$pred.gam <- predict(gam.fit, newdata=db, type='response')*db$NB_Claim
db$pred.glm <- predict(glm.fit, newdata=db, type='response')*db$NB_Claim
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

##

min_ <- quantile(train2$Avgdays.week, 0.01) 
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

score.gam <- as.formula(M_Claim ~ Car.use + Region + Car.age + I(Car.age^2) 
                        + Years.noclaims + I(Years.noclaims^2)
                        + s(Avgdays.week))

score.glm <- as.formula(M_Claim ~ Car.use + Region + Car.age + I(Car.age^2) 
                        + Years.noclaims + I(Years.noclaims^2)
                        + Avgdays.week + I(Avgdays.week^2) + I(Avgdays.week^3))

gam.fit <- gam(score.gam, family = Gamma(link = "log"), data = train2)
glm.fit <- glm(score.glm, family = Gamma(link = "log"), data = train2)

db$pred.gam <- predict(gam.fit, newdata=db, type='response')*db$NB_Claim
db$pred.glm <- predict(glm.fit, newdata=db, type='response')*db$NB_Claim
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


### Type of vehicle usage

train2$Pct.drive <- train2$Pct.drive.mon

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

score.gam <- as.formula(M_Claim ~ Car.use + Region + Car.age + I(Car.age^2) 
                        + Years.noclaims + I(Years.noclaims^2)
                        + s(Pct.drive))

score.glm <- as.formula(M_Claim ~ Car.use + Region + Car.age + I(Car.age^2) 
                        + Years.noclaims + I(Years.noclaims^2)
                        + Pct.drive + I(Pct.drive^2))

gam.fit <- gam(score.gam, family = Gamma(link = "log"), data = train2)
glm.fit <- glm(score.glm, family = Gamma(link = "log"), data = train2)

db$pred.gam <- predict(gam.fit, newdata=db, type='response')*db$NB_Claim
db$pred.glm <- predict(glm.fit, newdata=db, type='response')*db$NB_Claim
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

### Rush

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

score.gam <- as.formula(M_Claim ~ Car.use + Region + Car.age + I(Car.age^2) 
                        + Years.noclaims + I(Years.noclaims^2) 
                        + s(use.day))

score.glm <- as.formula(M_Claim ~ Car.use + Region + Car.age + I(Car.age^2) 
                        + Years.noclaims + I(Years.noclaims^2)  
                        + use.day + I(use.day^0.5))

gam.fit <- gam(score.gam, family = Gamma(link = "log"), data = train2)
glm.fit <- glm(score.glm, family = Gamma(link = "log"), data = train2)

db$pred.gam <- predict(gam.fit, newdata=db, type='response')*db$NB_Claim
db$pred.glm <- predict(glm.fit, newdata=db, type='response')*db$NB_Claim
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

### Max.day

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

score.gam <- as.formula(M_Claim ~ Car.use + Region + Car.age + I(Car.age^2) 
                        + Years.noclaims + I(Years.noclaims^2) 
                        + s(use.day))

score.glm <- as.formula(M_Claim ~ Car.use + Region + Car.age + I(Car.age^2) 
                        + Years.noclaims + I(Years.noclaims^2)  
                        + use.day + I(use.day^2) + I(use.day^3) )

gam.fit <- gam(score.gam, family = Gamma(link = "log"), data = train2)
glm.fit <- glm(score.glm, family = Gamma(link = "log"), data = train2)

db$pred.gam <- predict(gam.fit, newdata=db, type='response')*db$NB_Claim
db$pred.glm <- predict(glm.fit, newdata=db, type='response')*db$NB_Claim
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
  xlim(0, 1) +
  theme_classic()+
  theme(legend.position = 'bottom', legend.direction = "horizontal")

### Min.day

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

score.gam <- as.formula(M_Claim ~ Car.use + Region + Car.age + I(Car.age^2) 
                        + Years.noclaims + I(Years.noclaims^2)  
                        + s(use.day))

score.glm <- as.formula(M_Claim ~ Car.use + Region + Car.age + I(Car.age^2) 
                        + Years.noclaims + I(Years.noclaims^2)  
                        + use.day + I(use.day^2)+ I(use.day^3) )

gam.fit <- gam(score.gam, family = Gamma(link = "log"), data = train2)
glm.fit <- glm(score.glm, family = Gamma(link = "log"), data = train2)

db$pred.gam <- predict(gam.fit, newdata=db, type='response')*db$NB_Claim
db$pred.glm <- predict(glm.fit, newdata=db, type='response')*db$NB_Claim
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

### Max.min

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

score.gam <- as.formula(M_Claim ~ Car.use + Region + Car.age + I(Car.age^2) 
                        + Years.noclaims + I(Years.noclaims^2)  
                        + s(use.day))

score.glm <- as.formula(M_Claim ~ Car.use + Region + Car.age + I(Car.age^2) 
                        + Years.noclaims + I(Years.noclaims^2)  
                        + use.day + I(use.day^2) )

gam.fit <- gam(score.gam, family = Gamma(link = "log"), data = train2)
glm.fit <- glm(score.glm, family = Gamma(link = "log"), data = train2)

db$pred.gam <- predict(gam.fit, newdata=db, type='response')*db$NB_Claim
db$pred.glm <- predict(glm.fit, newdata=db, type='response')*db$NB_Claim
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

### Driving behavior
train2$use.day <- train2$Accel.06miles

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

score.gam <- as.formula(M_Claim ~ Car.use + Region + Car.age + I(Car.age^2) 
                        + Years.noclaims + I(Years.noclaims^2) 
                        + s(use.day))

score.glm <- as.formula(M_Claim ~ Car.use + Region + Car.age + I(Car.age^2) 
                        + Years.noclaims + I(Years.noclaims^2) 
                        + use.day + I(use.day^2) + I(use.day^3) )

gam.fit <- gam(score.gam, family = Gamma(link = "log"), data = train2)
glm.fit <- glm(score.glm, family = Gamma(link = "log"), data = train2)

db$pred.gam <- predict(gam.fit, newdata=db, type='response')*db$NB_Claim
db$pred.glm <- predict(glm.fit, newdata=db, type='response')*db$NB_Claim
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

train2$use.day <- train2$Right.turn.intensity08

q99 <- quantile(train2$use.day, 0.99)

min_ <- min(train2$use.day) 
max_ <- q99
by_ <-  (max_ - min_)/(nrow(train2)-1) 
add <- data.frame(seq(min_, max_, by_)) 
colnames(add) <- 'use.day'

db <- train2 %>%
  select(-'use.day') %>%
  dplyr::slice(1) 
db <- bind_rows(replicate(nrow(train2), db, simplify = FALSE))
db <- cbind(db, add)

##

temp <- train2 %>%
  mutate(use.day = pmin(q99, use.day))

score.gam <- as.formula(M_Claim ~ Car.use + Region + Car.age + I(Car.age^2) 
                        + Years.noclaims + I(Years.noclaims^2) 
                        + s(use.day))

score.glm <- as.formula(M_Claim ~ Car.use + Region + Car.age + I(Car.age^2) 
                        + Years.noclaims + I(Years.noclaims^2)
                        + use.day + log1p(use.day))

gam.fit <- gam(score.gam, family = Gamma(link = "log"), data = temp)
glm.fit <- glm(score.glm, family = Gamma(link = "log"), data = temp)

db$pred.gam <- predict(gam.fit, newdata=db, type='response')*db$NB_Claim
db$pred.glm <- predict(glm.fit, newdata=db, type='response')*db$NB_Claim
base <- db %>%
  mutate(diff = abs(use.day - mean(temp$use.day))) %>%
  filter(diff == min(diff))
db$pred.gam <- db$pred.gam/base$pred.gam[1]
db$pred.glm <- db$pred.glm/base$pred.glm[1]

ggplot()+
  geom_line(aes(x=use.day, y=pred.gam, color='GAM'), data=db) + 
  geom_line(aes(x=use.day, y=pred.glm, color='Parametric GLM'), data=db) +
  guides(color = guide_legend(title = "")) +
  labs(x = 'Use per day',
       y = 'Relativity') +
  # xlim(0, q99) +
  theme_classic()+
  theme(legend.position = 'bottom', legend.direction = "horizontal")

#### TELE VARiables

tele.var <- c("Miles.per.day", "Avgdays.week",
              "Pct.drive.mon", "Pct.drive.tue", 
              "Pct.drive.wed", "Pct.drive.thr", "Pct.drive.fri", "Pct.drive.sat", "Pct.drive.sun",
              "max.day", "min.day", "max.min", "Dayformax", "Dayformin",
              "Pct.drive.rush.am", "Pct.drive.rush.pm",
              "Pct.drive.wkend",
              "Pct.drive.2hrs", "Pct.drive.3hrs", "Pct.drive.4hrs",
              "Accel.06miles", "Accel.08miles", "Accel.09miles", "Accel.11miles", "Accel.12miles", "Accel.14miles", 
              "Brake.06miles", "Brake.08miles", "Brake.09miles", "Brake.11miles", "Brake.12miles", "Brake.14miles", 
              "Left.turn.intensity08", "Left.turn.intensity09", "Left.turn.intensity10", "Left.turn.intensity11", "Left.turn.intensity12",
              "Right.turn.intensity08", "Right.turn.intensity09", "Right.turn.intensity10", "Right.turn.intensity11", "Right.turn.intensity12")


### Fitting the GLM-Net model

"To solve some convergence issues, we remove from all 
GML-Net models *Accel.* and *Brake.* terms."

glm.score <- as.formula(M_Claim ~ 
                          Car.use + Region + Car.age + I(Car.age^2) + I(Car.age^3) 
                        + Years.noclaims + I(Years.noclaims^2)  + I(Years.noclaims^3) 
                        + Dayformax + Dayformin +
                          + Miles.per.day + log(Miles.per.day)
                        + Avgdays.week + I(Avgdays.week^2) + I(Avgdays.week^3)
                        + Pct.drive.mon + I(Pct.drive.mon^2)
                        + Pct.drive.tue + I(Pct.drive.tue^2)
                        + Pct.drive.wed + I(Pct.drive.wed^2)
                        + Pct.drive.thr + I(Pct.drive.thr^2)
                        + Pct.drive.fri + I(Pct.drive.fri^2)
                        + Pct.drive.sat + I(Pct.drive.sat^2)
                        + Pct.drive.sun + I(Pct.drive.sun^2)
                        + Pct.drive.wkend + I(Pct.drive.wkend^2)
                        + max.day + I(max.day^2) + I(max.day^3) 
                        + min.day + I(min.day^2) + I(min.day^3)
                        + max.min + I(max.min^2)
                        + Pct.drive.rush.am + sqrt(Pct.drive.rush.am) 
                        + Pct.drive.rush.pm + sqrt(Pct.drive.rush.pm)   
                        + Pct.drive.2hrs + sqrt(Pct.drive.2hrs) 
                        + Pct.drive.3hrs + sqrt(Pct.drive.3hrs) 
                        + Pct.drive.4hrs + sqrt(Pct.drive.4hrs)
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
y <- train2$M_Claim

fold.id <- train2$fold

lambda_seq <- c(10^seq(0, -8, by = -0.1))

cvfit0  <- cv.glmnet(matrix.x, y, relax=FALSE, family = Gamma(link = "log"), lambda = lambda_seq, foldid = fold, alpha = 0)
cvfit.2 <- cv.glmnet(matrix.x, y, relax=FALSE, family = Gamma(link = "log"), lambda = lambda_seq, foldid = fold, alpha = 0.2)
cvfit.4 <- cv.glmnet(matrix.x, y, relax=FALSE, family = Gamma(link = "log"), lambda = lambda_seq, foldid = fold, alpha = 0.4)
cvfit.6 <- cv.glmnet(matrix.x, y, relax=FALSE, family = Gamma(link = "log"), lambda = lambda_seq, foldid = fold, alpha = 0.6)
cvfit.8 <- cv.glmnet(matrix.x, y, relax=FALSE, family = Gamma(link = "log"), lambda = lambda_seq, foldid = fold, alpha = 0.8)
cvfit1  <- cv.glmnet(matrix.x, y, relax=FALSE, family = Gamma(link = "log"), lambda = lambda_seq, foldid = fold, alpha = 1)


c(cvfit0$lambda.min, cvfit.2$lambda.min, cvfit.4$lambda.min, cvfit.6$lambda.min, cvfit.8$lambda.min, cvfit1$lambda.min)

cvfits <- list(cvfit0, cvfit.2, cvfit.4, cvfit.6, cvfit.8, cvfit1)
alphas <- c(0, 0.2, 0.4, 0.6, 0.8, 1)
cv_errors <- sapply(cvfits, function(cv) min(cv$cvm))

plot(alphas, cv_errors, type = "b", pch = 19, col = "darkblue",
     xlab = "Alpha", ylab = "CV Error (min)", main = "Errori di CV per diversi alpha")




all.min <- data.frame(c(min(cvfit0$cvm), min(cvfit.2$cvm), min(cvfit.4$cvm), min(cvfit.6$cvm), min(cvfit.8$cvm), min(cvfit1$cvm))) %>%
  mutate(alpha = 2*(row_number()-1)/10)
colnames(all.min)[1] <- 'min' 
all.min %>% filter(min == min(min))

cvfit1$lambda.min
cvfit1$lambda.1se

### Optimal value

glm.score <- as.formula(M_Claim ~ 
                          Car.use + Region + Car.age + I(Car.age^2) + I(Car.age^3) 
                        + Years.noclaims + I(Years.noclaims^2)  + I(Years.noclaims^3) 
                        + Dayformax + Dayformin +
                          + Miles.per.day + log(Miles.per.day)
                        + Avgdays.week + I(Avgdays.week^2) + I(Avgdays.week^3)
                        + Pct.drive.mon + I(Pct.drive.mon^2)
                        + Pct.drive.tue + I(Pct.drive.tue^2)
                        + Pct.drive.wed + I(Pct.drive.wed^2)
                        + Pct.drive.thr + I(Pct.drive.thr^2)
                        + Pct.drive.fri + I(Pct.drive.fri^2)
                        + Pct.drive.sat + I(Pct.drive.sat^2)
                        + Pct.drive.sun + I(Pct.drive.sun^2)
                        + Pct.drive.wkend + I(Pct.drive.wkend^2)
                        + max.day + I(max.day^2) + I(max.day^3) 
                        + min.day + I(min.day^2) + I(min.day^3)
                        + max.min + I(max.min^2)
                        + Pct.drive.rush.am + sqrt(Pct.drive.rush.am) 
                        + Pct.drive.rush.pm + sqrt(Pct.drive.rush.pm)   
                        + Pct.drive.2hrs + sqrt(Pct.drive.2hrs) 
                        + Pct.drive.3hrs + sqrt(Pct.drive.3hrs) 
                        + Pct.drive.4hrs + sqrt(Pct.drive.4hrs) 
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
  y <- learn$M_Claim
  
  lambda.min <- 0.003162278
  lambda.1se <- 0.05011872
  
  lambda.select <- lambda.min
  fit <- glmnet(matrix.x, y, family = Gamma(link = "log"), relax=FALSE, alpha = 1, lambda = lambda.select)
  #fit <- glmnet(matrix.x, y, family = "poisson", relax=TRUE, offset = offset, alpha = 1, lambda = lambda.select)
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

# TEST SET
glm.score <- as.formula(M_Claim ~ 
                          Car.use + Region + Car.age + I(Car.age^2) + I(Car.age^3) 
                        + Years.noclaims + I(Years.noclaims^2)  + I(Years.noclaims^3) 
                        + Dayformax + Dayformin +
                          + Miles.per.day + log(Miles.per.day)
                        + Avgdays.week + I(Avgdays.week^2) + I(Avgdays.week^3)
                        + Pct.drive.mon + I(Pct.drive.mon^2)
                        + Pct.drive.tue + I(Pct.drive.tue^2)
                        + Pct.drive.wed + I(Pct.drive.wed^2)
                        + Pct.drive.thr + I(Pct.drive.thr^2)
                        + Pct.drive.fri + I(Pct.drive.fri^2)
                        + Pct.drive.sat + I(Pct.drive.sat^2)
                        + Pct.drive.sun + I(Pct.drive.sun^2)
                        + Pct.drive.wkend + I(Pct.drive.wkend^2)
                        + max.day + I(max.day^2) + I(max.day^3) 
                        + min.day + I(min.day^2) + I(min.day^3)
                        + max.min + I(max.min^2)
                        + Pct.drive.rush.am + sqrt(Pct.drive.rush.am) 
                        + Pct.drive.rush.pm + sqrt(Pct.drive.rush.pm)   
                        + Pct.drive.2hrs + sqrt(Pct.drive.2hrs) 
                        + Pct.drive.3hrs + sqrt(Pct.drive.3hrs) 
                        + Pct.drive.4hrs + sqrt(Pct.drive.4hrs) 
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
y <- train2$M_Claim

lambda.min <- 0.003162278
lambda.1se <- 0.05011872

lambda.select <- lambda.min
fit <- glmnet(matrix.x, y, family = Gamma(link = "log"), relax=FALSE, alpha = 1, lambda = lambda.select)
#fit <- glmnet(matrix.x, y, family = "poisson", relax=TRUE, offset = offset, alpha = 1, lambda = lambda.select)

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
  
  lambda.min <- 0.003162278
  lambda.1se <- 0.05011872
  
  lambda.select <- lambda.1se
  #fit <- glmnet(matrix.x, y, family = "poisson", relax=FALSE, offset = offset, alpha = 1, lambda = lambda.select)
  fit <- glmnet(matrix.x, y, family = Gamma(link = "log"), relax=TRUE, alpha = 1, lambda = lambda.select)
  learn$pred <- predict(fit, newx = matrix.x, type='response', lambda = lambda.select)*learn$NB_Claim
  
  
  matrix.x <- model.matrix(glm.score, data=valid)[,-1]
  y <- valid$M_Claim
  
  
  valid$pred <- predict(fit, newx = matrix.x, type='response', lambda = lambda.select)*valid$NB_Claim
  variance <- (sum((learn$AMT_Claim - learn$pred)^2)/(nrow(learn) - length(fit$beta)))
  phi <-  variance/mean(learn$AMT_Claim)^2
  
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






# TEST SET 

glm.score <- as.formula(M_Claim ~ 
                          Car.use + Region + Car.age + I(Car.age^2) + I(Car.age^3) 
                        + Years.noclaims + I(Years.noclaims^2)  + I(Years.noclaims^3) 
                        + Dayformax + Dayformin +
                          + Miles.per.day + log(Miles.per.day)
                        + Avgdays.week + I(Avgdays.week^2) + I(Avgdays.week^3)
                        + Pct.drive.mon + I(Pct.drive.mon^2)
                        + Pct.drive.tue + I(Pct.drive.tue^2)
                        + Pct.drive.wed + I(Pct.drive.wed^2)
                        + Pct.drive.thr + I(Pct.drive.thr^2)
                        + Pct.drive.fri + I(Pct.drive.fri^2)
                        + Pct.drive.sat + I(Pct.drive.sat^2)
                        + Pct.drive.sun + I(Pct.drive.sun^2)
                        + Pct.drive.wkend + I(Pct.drive.wkend^2)
                        + max.day + I(max.day^2) + I(max.day^3) 
                        + min.day + I(min.day^2) + I(min.day^3)
                        + max.min + I(max.min^2)
                        + Pct.drive.rush.am + sqrt(Pct.drive.rush.am) 
                        + Pct.drive.rush.pm + sqrt(Pct.drive.rush.pm)   
                        + Pct.drive.2hrs + sqrt(Pct.drive.2hrs) 
                        + Pct.drive.3hrs + sqrt(Pct.drive.3hrs) 
                        + Pct.drive.4hrs + sqrt(Pct.drive.4hrs) 
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
y <- train2$M_Claim

lambda.min <- 0.003162278
lambda.1se <- 0.05011872

lambda.select <- lambda.1se
#fit <- glmnet(matrix.x, y, family = "poisson", relax=FALSE, offset = offset, alpha = 1, lambda = lambda.select)
fit <- glmnet(matrix.x, y, family = Gamma(link = "log"), relax=TRUE, alpha = 1, lambda = lambda.select)

train2$pred <- predict(fit, newx = matrix.x, type='response', lambda = lambda.select)*train2$NB_Claim

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


## Residuals and protected variables
"Observed Relativity vs. Residuals Relativity"
## Credit Score

meansev.inv <- sum(train2$NB_Claim)/sum(train2$M_Claim)
meanpred.inv <- sum(train2$pred.tele)/sum(train2$M_Claim)

temp2 <- train2 %>%
  dplyr::mutate(Duration.y = Duration/365.25, 
                Group = ceiling(Credit.score/25) * 25) %>%
  group_by(Group) %>% 
  summarize(M_Claim=sum(M_Claim),
            pred=sum(pred.tele),
            nbclaim = n()) %>% 
  mutate(sev = meansev.inv*M_Claim/nbclaim,
         sev2 = meanpred.inv*M_Claim/pred)


ggplot() + 
  geom_smooth(aes(x=Group, y=sev, weight = nbclaim, color='Observed'),se=F, linewidth=1, data=temp2) + 
  geom_smooth(aes(x=Group, y=sev2, weight = nbclaim, color='Residuals'),se=F, linewidth=1, data=temp2) + 
  labs(x = 'Credit Score',
       y = 'Relativity') +
  geom_hline(yintercept = 1, linetype='dashed')+
  guides(color = guide_legend(title = "")) +
  theme_classic()+    theme(legend.position = 'bottom', legend.direction = "horizontal")

## Insured Age

temp2 <- train2 %>%
  dplyr::mutate(Duration.y = Duration/365.25, 
                Insured.age = pmin(Insured.age, 80),
                Group = ceiling(Insured.age/5) * 5) %>%
  group_by(Group) %>% 
  summarize(M_Claim=sum(M_Claim),
            pred=sum(pred.tele),
            nbclaim = n()) %>% 
  mutate(sev = meansev.inv*M_Claim/nbclaim,
         sev2 = meanpred.inv*M_Claim/pred)

ggplot() + 
  geom_smooth(aes(x=Group, y=sev, weight = nbclaim, color='Observed'),se=F, linewidth=1, data=temp2) + 
  geom_smooth(aes(x=Group, y=sev2, weight = nbclaim, color='Residuals'),se=F,linewidth=1, data=temp2) + 
  labs(x = 'Age of the insured',
       y = 'Relativity') +
  geom_hline(yintercept = 1, linetype='dashed')+
  guides(color = guide_legend(title = "")) +
  theme_classic()+    theme(legend.position = 'bottom', legend.direction = "horizontal")

## Sex of the insured

temp <- train2 %>%
  mutate(Var_ = Insured.sex) %>%
  group_by(Var_) %>%
  summarize(M_Claim=sum(M_Claim),
            pred=sum(pred.tele),
            nbclaim = n()) %>% 
  mutate(sev = meansev.inv*M_Claim/nbclaim,
         sev2 = meanpred.inv*M_Claim/pred)

temp$sev <- temp$sev/temp$sev[1]
temp$sev2 <- temp$sev2/temp$sev2[1]

ggplot() + #start plot by by plotting bars
  geom_point(data = temp, aes(x = Var_, y = (sev2), group = 1, color='Residuals'), size=3) +
  geom_line(data = temp, aes(x = Var_, y = (sev2), group = 1, color='Residuals'), size=0.7) +
  geom_point(data = temp, aes(x = Var_, y = (sev), group = 1, color='Observed'), size=3) +
  geom_line(data = temp, aes(x = Var_, y = (sev), group = 1, color='Observed'), size=0.7) +
  labs(x = 'Sex of the insured', y = 'Relativity') +
  geom_hline(yintercept = 1, linetype='dashed')+
  #ylim(max(temp$freq, temp$freq2)*0.95, max(temp$freq, temp$freq2)*1.05)+
  guides(color=guide_legend(title="")) +
  theme_classic()+    theme(legend.position = 'bottom', legend.direction = "horizontal")

## Marital

temp <- train2 %>%
  mutate(Var_ = Marital) %>%
  group_by(Var_) %>%
  summarize(M_Claim=sum(M_Claim),
            pred=sum(pred.tele),
            nbclaim = n()) %>% 
  mutate(sev = meansev.inv*M_Claim/nbclaim,
         sev2 = meanpred.inv*pred/nbclaim)

temp$sev <- temp$sev/temp$sev[1]
temp$sev2 <- temp$sev2/temp$sev2[1]

ggplot() + #start plot by by plotting bars
  geom_point(data = temp, aes(x = Var_, y = (sev2), group = 1, color='Residuals'), size=3) +
  geom_line(data = temp, aes(x = Var_, y = (sev2), group = 1, color='Residuals'), size=0.7) +
  geom_point(data = temp, aes(x = Var_, y = (sev), group = 1, color='Observed'), size=3) +
  geom_line(data = temp, aes(x = Var_, y = (sev), group = 1, color='Observed'), size=0.7) +
  labs(x = 'Marital status of the insured', y = 'Relativity') +
  geom_hline(yintercept = 1, linetype='dashed')+
  #ylim(max(temp$freq, temp$freq2)*0.95, max(temp$freq, temp$freq2)*1.05)+
  guides(color=guide_legend(title="")) +
  theme_classic()+    theme(legend.position = 'bottom', legend.direction = "horizontal")

## Territory

temp <- train2 %>%
  mutate(Var_ = Territory) %>%
  group_by(Var_) %>%
  summarize(M_Claim=sum(M_Claim),
            pred=sum(pred.tele),
            nbclaim = n()) %>% 
  mutate(sev = meansev.inv*M_Claim/nbclaim,
         sev2 = meanpred.inv*M_Claim/pred)

#temp$sev <- temp$sev/temp$sev[1]
#temp$sev2 <- temp$sev2/temp$sev2[1]

ggplot() + #start plot by by plotting bars
  #geom_point(data = temp, aes(x = Var_, y = (sev2), group = 1, color='Residuals'), size=3) +
  geom_line(data = temp, aes(x = Var_, y = (sev2), group = 1, color='Residuals'), size=0.7) +
  #geom_point(data = temp, aes(x = Var_, y = (sev), group = 1, color='Observed'), size=3) +
  geom_line(data = temp, aes(x = Var_, y = (sev), group = 1, color='Observed'), size=0.7) +
  labs(x = 'Territory', y = 'Relativity') +
  geom_hline(yintercept = 1, linetype='dashed')+
  scale_x_discrete(labels = NULL, breaks = NULL)+
  #ylim(max(temp$freq, temp$freq2)*0.95, max(temp$freq, temp$freq2)*1.05)+
  guides(color=guide_legend(title="")) +
  theme_classic()+    theme(legend.position = 'bottom', legend.direction = "horizontal")


### GLM-net on residuals
glm.score <- as.formula(M_Claim ~ 
                          Car.use + Region + Car.age + I(Car.age^2) + I(Car.age^3) 
                        + Years.noclaims + I(Years.noclaims^2)  + I(Years.noclaims^3) 
                        + Dayformax + Dayformin +
                          + Miles.per.day + log(Miles.per.day)
                        + Avgdays.week + I(Avgdays.week^2) + I(Avgdays.week^3)
                        + Pct.drive.mon + I(Pct.drive.mon^2)
                        + Pct.drive.tue + I(Pct.drive.tue^2)
                        + Pct.drive.wed + I(Pct.drive.wed^2)
                        + Pct.drive.thr + I(Pct.drive.thr^2)
                        + Pct.drive.fri + I(Pct.drive.fri^2)
                        + Pct.drive.sat + I(Pct.drive.sat^2)
                        + Pct.drive.sun + I(Pct.drive.sun^2)
                        + Pct.drive.wkend + I(Pct.drive.wkend^2)
                        + max.day + I(max.day^2) + I(max.day^3) 
                        + min.day + I(min.day^2) + I(min.day^3)
                        + max.min + I(max.min^2)
                        + Pct.drive.rush.am + sqrt(Pct.drive.rush.am) 
                        + Pct.drive.rush.pm + sqrt(Pct.drive.rush.pm)   
                        + Pct.drive.2hrs + sqrt(Pct.drive.2hrs) 
                        + Pct.drive.3hrs + sqrt(Pct.drive.3hrs) 
                        + Pct.drive.4hrs + sqrt(Pct.drive.4hrs) 
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
y <- train2$M_Claim


lambda.min <- 0.003162278
lasso.min <- glmnet(matrix.x, y, family = Gamma(link = "log"), relax=FALSE, alpha = 1, lambda = lambda.min)
train2$pred.tele <- predict(lasso.min, newx = matrix.x, type='response', lambda = lambda.min)

###

var.sens <- c("Marital", "Insured.sex", "Credit.score", "Insured.age", "Territory")     
glm.score <- as.formula(M_Claim ~ Insured.sex + Marital 
                        + Credit.score +  I(Credit.score^2) 
                        + Insured.age + I(Insured.age^2) 
                        + terr.code + I(terr.code^2)  + I(terr.code^3))

matrix.x <- model.matrix(glm.score, data=train2)[,-1]
y <- train2$M_Claim
offset <- log(train2$pred.tele)
fold.id <- train2$fold

lambda_seq <- c(10^seq(0, -8, by = -.1), 0)
cvfit0  <- cv.glmnet(matrix.x, y, relax=FALSE, family = Gamma(link = "log"), offset = offset, lambda = lambda_seq, foldid = fold, alpha = 0)
cvfit.2 <- cv.glmnet(matrix.x, y, relax=FALSE, family = Gamma(link = "log"), offset = offset, lambda = lambda_seq, foldid = fold, alpha = 0.2)
cvfit.4 <- cv.glmnet(matrix.x, y, relax=FALSE, family = Gamma(link = "log"), offset = offset, lambda = lambda_seq, foldid = fold, alpha = 0.4)
cvfit.6 <- cv.glmnet(matrix.x, y, relax=FALSE, family = Gamma(link = "log"), offset = offset, lambda = lambda_seq, foldid = fold, alpha = 0.6)
cvfit.8 <- cv.glmnet(matrix.x, y, relax=FALSE, family = Gamma(link = "log"), offset = offset, lambda = lambda_seq, foldid = fold, alpha = 0.8)
cvfit1  <- cv.glmnet(matrix.x, y, relax=FALSE, family = Gamma(link = "log"), offset = offset, lambda = lambda_seq, foldid = fold, alpha = 1)

c(cvfit0$lambda.min, cvfit.2$lambda.min, cvfit.4$lambda.min, cvfit.6$lambda.min, cvfit.8$lambda.min, cvfit1$lambda.min)

all.min <- data.frame(c(min(cvfit0$cvm), min(cvfit.2$cvm), min(cvfit.4$cvm), min(cvfit.6$cvm), min(cvfit.8$cvm), min(cvfit1$cvm))) %>%
  mutate(alpha = 2*(row_number()-1)/10)
colnames(all.min)[1] <- 'min' 
all.min %>% filter(min == min(min))

cvfit1$lambda.min
cvfit1$lambda.1se

##############################
glm.score <- as.formula(M_Claim ~ 
                          Car.use + Region + Car.age + I(Car.age^2) + I(Car.age^3) 
                        + Years.noclaims + I(Years.noclaims^2)  + I(Years.noclaims^3) 
                        + Dayformax + Dayformin +
                          + Miles.per.day + log(Miles.per.day)
                        + Avgdays.week + I(Avgdays.week^2) + I(Avgdays.week^3)
                        + Pct.drive.mon + I(Pct.drive.mon^2)
                        + Pct.drive.tue + I(Pct.drive.tue^2)
                        + Pct.drive.wed + I(Pct.drive.wed^2)
                        + Pct.drive.thr + I(Pct.drive.thr^2)
                        + Pct.drive.fri + I(Pct.drive.fri^2)
                        + Pct.drive.sat + I(Pct.drive.sat^2)
                        + Pct.drive.sun + I(Pct.drive.sun^2)
                        + Pct.drive.wkend + I(Pct.drive.wkend^2)
                        + max.day + I(max.day^2) + I(max.day^3) 
                        + min.day + I(min.day^2) + I(min.day^3)
                        + max.min + I(max.min^2)
                        + Pct.drive.rush.am + sqrt(Pct.drive.rush.am) 
                        + Pct.drive.rush.pm + sqrt(Pct.drive.rush.pm)   
                        + Pct.drive.2hrs + sqrt(Pct.drive.2hrs) 
                        + Pct.drive.3hrs + sqrt(Pct.drive.3hrs) 
                        + Pct.drive.4hrs + sqrt(Pct.drive.4hrs) 
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
y <- train2$M_Claim


lambda.min <- 0.003162278
lasso.min <- glmnet(matrix.x, y, family = Gamma(link = "log"), relax=FALSE, alpha = 1, lambda = lambda.min)
train2$pred.tele <- predict(lasso.min, newx = matrix.x, type='response', lambda = lambda.min)


matrix.x <- model.matrix(glm.score, data=test2)[,-1]
y <- test2$M_Claim

test2$pred.tele <- predict(lasso.min, newx = matrix.x, type='response', lambda = lambda.min)

glm.score <- as.formula(M_Claim ~ Insured.sex + Marital 
                        + Credit.score +  I(Credit.score^2) 
                        + Insured.age +  I(Insured.age^2) 
                        + terr.code + I(terr.code^2)  + I(terr.code^3) )

matrix.x <- model.matrix(glm.score, data=train2)[,-1]
y <- train2$M_Claim
offset <- log(train2$pred.tele)

lambda.min <- 0.003162278
lambda.1se <- 0.1258925

lambda.select <- lambda.min
#fit <- glmnet(matrix.x, y, family = "poisson", relax=FALSE, offset = offset, alpha = 1, lambda = lambda.select)
fit <- glmnet(matrix.x, y, family = Gamma(link = "log"), relax=TRUE, offset = offset, alpha = 1, lambda = lambda.select)


train2$pred <- predict(fit, newx = matrix.x, type='response', newoffset=offset, lambda = lambda.select)*train2$NB_Claim

matrix.x <- model.matrix(glm.score, data=test2)[,-1]
y <- test2$M_Claim
offset <- log(test2$pred.tele)

test2$pred.base <- predict(fit, newx = matrix.x, type='response', newoffset=offset, lambda = lambda.select)*test2$NB_Claim
variance <- (sum((train2$AMT_Claim - train2$pred)^2)/(nrow(train2) - length(fit$beta)))
phi <- variance/mean(train2$AMT_Claim)^2

Result_ <- data.frame(t(Score.pred.sev(test2$pred.base, test2$AMT_Claim, phi)/nrow(test2)))
Result_ <- cbind('LASSO* (optimal)', Result_)
colnames(Result_) <- c("Model", "Sc.log", "Sc.MSE")

Result_all <- rbind(Result_all, Result_)

###

glm.score <- as.formula(M_Claim ~ Insured.sex + Marital 
                        + Credit.score +  I(Credit.score^2) 
                        + Insured.age + I(Insured.age^2) 
                        + terr.code + I(terr.code^2)  + I(terr.code^3) )

matrix.x <- model.matrix(glm.score, data=train2)[,-1]
y <- train2$M_Claim
offset <- log(train2$pred.tele)

lambda.min <- 0.003162278
lambda.1se <- 0.1258925

lambda.select <- lambda.1se
fit <- glmnet(matrix.x, y, family = Gamma(link = "log"), relax=FALSE, offset = offset, alpha = 1, lambda = lambda.select)
#fit <- glmnet(matrix.x, y, family = "poisson", relax=TRUE, offset = offset, alpha = 1, lambda = lambda.select)

train2$pred <- predict(fit, newx = matrix.x, type='response', newoffset=offset, lambda = lambda.select)*train2$NB_Claim

matrix.x <- model.matrix(glm.score, data=test2)[,-1]
y <- test2$M_Claim
offset <- log(test2$pred.tele)

test2$pred.base <- predict(fit, newx = matrix.x, type='response', newoffset=offset, lambda = lambda.select)*test2$NB_Claim
variance <- (sum((train2$AMT_Claim - train2$pred)^2)/(nrow(train2) - length(fit$beta)))
phi <- variance/mean(train2$AMT_Claim)^2

Result_ <- data.frame(t(Score.pred.sev(test2$pred.base, test2$AMT_Claim, phi)/nrow(test2)))
Result_ <- cbind('LASSO* (parsimonious)', Result_)
colnames(Result_) <- c("Model", "Sc.log", "Sc.MSE")

Result_all <- rbind(Result_all, Result_)

knitr::kable(Result_all, align = "ccc", digits = c(0, 5, 5), format.args = list(big.mark = ","))%>%   
  kable_styling(bootstrap_options = "striped", full_width = F)  


########Prediction scores for the GLM-net model without Credit Score  (testing set) 

glm.score <- as.formula(M_Claim ~ 
                          Car.use + Region + Car.age + I(Car.age^2) + I(Car.age^3) 
                        + Years.noclaims + I(Years.noclaims^2)  + I(Years.noclaims^3) 
                        + Dayformax + Dayformin +
                          + Miles.per.day + log(Miles.per.day)
                        + Avgdays.week + I(Avgdays.week^2) + I(Avgdays.week^3)
                        + Pct.drive.mon + I(Pct.drive.mon^2)
                        + Pct.drive.tue + I(Pct.drive.tue^2)
                        + Pct.drive.wed + I(Pct.drive.wed^2)
                        + Pct.drive.thr + I(Pct.drive.thr^2)
                        + Pct.drive.fri + I(Pct.drive.fri^2)
                        + Pct.drive.sat + I(Pct.drive.sat^2)
                        + Pct.drive.sun + I(Pct.drive.sun^2)
                        + Pct.drive.wkend + I(Pct.drive.wkend^2)
                        + max.day + I(max.day^2) + I(max.day^3) 
                        + min.day + I(min.day^2) + I(min.day^3)
                        + max.min + I(max.min^2)
                        + Pct.drive.rush.am + sqrt(Pct.drive.rush.am) 
                        + Pct.drive.rush.pm + sqrt(Pct.drive.rush.pm)   
                        + Pct.drive.2hrs + sqrt(Pct.drive.2hrs) 
                        + Pct.drive.3hrs + sqrt(Pct.drive.3hrs) 
                        + Pct.drive.4hrs + sqrt(Pct.drive.4hrs) 
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
y <- train2$M_Claim


lambda.min <- 0.003162278
lasso.min <- glmnet(matrix.x, y, family = Gamma(link = "log"), relax=FALSE, alpha = 1, lambda = lambda.min)
train2$pred.tele <- predict(lasso.min, newx = matrix.x, type='response', lambda = lambda.min)


matrix.x <- model.matrix(glm.score, data=test2)[,-1]
y <- test2$M_Claim

test2$pred.tele <- predict(lasso.min, newx = matrix.x, type='response', lambda = lambda.min)

glm.score <- as.formula(M_Claim ~ Insured.sex + Marital 
                        + Insured.age +  I(Insured.age^2) 
                        + terr.code + I(terr.code^2)  + I(terr.code^3) )

matrix.x <- model.matrix(glm.score, data=train2)[,-1]
y <- train2$M_Claim
offset <- log(train2$pred.tele)

lambda.min <- 0.003162278
lambda.1se <- 0.1258925

lambda.select <- lambda.min
#fit <- glmnet(matrix.x, y, family = "poisson", relax=FALSE, offset = offset, alpha = 1, lambda = lambda.select)
fit <- glmnet(matrix.x, y, family = Gamma(link = "log"), relax=TRUE, offset = offset, alpha = 1, lambda = lambda.select)


train2$pred <- predict(fit, newx = matrix.x, type='response', newoffset=offset, lambda = lambda.select)*train2$NB_Claim

matrix.x <- model.matrix(glm.score, data=test2)[,-1]
y <- test2$M_Claim
offset <- log(test2$pred.tele)

test2$pred.base <- predict(fit, newx = matrix.x, type='response', newoffset=offset, lambda = lambda.select)*test2$NB_Claim
variance <- (sum((train2$AMT_Claim - train2$pred)^2)/(nrow(train2) - length(fit$beta)))
phi <- variance/mean(train2$AMT_Claim)^2

Result_ <- data.frame(t(Score.pred.sev(test2$pred.base, test2$AMT_Claim, phi)/nrow(test2)))
Result_ <- cbind('LASSO** (optimal)', Result_)
colnames(Result_) <- c("Model", "Sc.log", "Sc.MSE")

Result_all <- rbind(Result_all, Result_)

###

glm.score <- as.formula(M_Claim ~ Insured.sex + Marital 
                        + Insured.age + I(Insured.age^2) 
                        + terr.code + I(terr.code^2)  + I(terr.code^3) )

matrix.x <- model.matrix(glm.score, data=train2)[,-1]
y <- train2$M_Claim
offset <- log(train2$pred.tele)

lambda.min <- 0.003162278
lambda.1se <- 0.1258925

lambda.select <- lambda.1se
fit <- glmnet(matrix.x, y, family = Gamma(link = "log"), relax=FALSE, offset = offset, alpha = 1, lambda = lambda.select)
#fit <- glmnet(matrix.x, y, family = "poisson", relax=TRUE, offset = offset, alpha = 1, lambda = lambda.select)

train2$pred <- predict(fit, newx = matrix.x, type='response', newoffset=offset, lambda = lambda.select)*train2$NB_Claim

matrix.x <- model.matrix(glm.score, data=test2)[,-1]
y <- test2$M_Claim
offset <- log(test2$pred.tele)

test2$pred.base <- predict(fit, newx = matrix.x, type='response', newoffset=offset, lambda = lambda.select)*test2$NB_Claim
variance <- (sum((train2$AMT_Claim - train2$pred)^2)/(nrow(train2) - length(fit$beta)))
phi <- variance/mean(train2$AMT_Claim)^2

Result_ <- data.frame(t(Score.pred.sev(test2$pred.base, test2$AMT_Claim, phi)/nrow(test2)))
Result_ <- cbind('LASSO** (parsimonious)', Result_)
colnames(Result_) <- c("Model", "Sc.log", "Sc.MSE")

Result_all <- rbind(Result_all, Result_)

knitr::kable(Result_all, align = "ccc", digits = c(0, 5, 5), format.args = list(big.mark = ","))%>%   
  kable_styling(bootstrap_options = "striped", full_width = F)



## XGBOOST

library(xgboost)
library(Ckmeans.1d.dp)
library(SHAPforxgboost)
library(pacman)

# p_load automatically installs packages if needed
p_load(xgboost, ParBayesianOptimization, mlbench, dplyr, skimr, recipes, resample)

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

dtrain <- xgb.DMatrix(data = data.matrix(train2[, paste(all.vars2)]), label = train2$M_Claim)
#setinfo(dtrain,"base_margin",log(train2$expo))
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
  
  set.seed(133)
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

base.rounds <- 500
set.seed(254)
bayes_out <- bayesOpt(FUN = obj_func, bounds = bounds, initPoints = length(bounds) + 2, iters.n = 5)
comp <- bayes_out$scoreSummary[which(bayes_out$scoreSummary$Score== max(bayes_out$scoreSummary$Score))]
comp 

#   Epoch Iteration       eta max_depth subsample min_child_weight gpUtility acqOptimum inBounds Elapsed     Score nrounds errorMessage
#1:     0         6 0.1914908         5 0.8674295         100.0327        NA      FALSE     TRUE    2.96 -8.959176     135           NA

############
## Verif ###
############

#1- Par la fonction
 obj_func(eta=comp$eta, max_depth=comp$max_depth, subsample=comp$subsample, min_child_weight=comp$min_child_weight)

#2- Par le XGB.CV
param <- list(
  eta = 0.1914908,
  max_depth = 5,
  subsample = 0.8674295,
  min_child_weight = 100.0327,
  booster = "gbtree",
  objective = "reg:gamma",
  eval_metric = "gamma-nloglik")

set.seed(133)
xgbcv <- xgb.cv(params = param,
                nrounds = 135,
                data = dtrain,
                folds = folds,
                prediction = TRUE,
                early_stopping_rounds = 10,
                verbose = 0,
                maximize = F)

-min(xgbcv$evaluation_log$test_gamma_nloglik_mean)


### Prediction SCORES

"Using these values, we can calculate the model’s 
prediction scores based on all classical and telematics covariates.  
can see that the XGBoost approach is particularly effective in capturing 
the effect of all available telematic covariates. Indeed, the scores
obtained are significantly improved compared to other tested approaches.
"
library(xgboost)
library(Ckmeans.1d.dp)
library(SHAPforxgboost)
library(pacman)

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

dtrain <- xgb.DMatrix(data = data.matrix(train2[, paste(all.vars2)]), label = train2$M_Claim)
#setinfo(dtrain,"base_margin",log(train2$expo))
folds <-list(fold1 = which(train2$fold == 1),
             fold2 = which(train2$fold == 2),
             fold3 = which(train2$fold == 3),
             fold4 = which(train2$fold == 4),
             fold5 = which(train2$fold == 5))

param <- list(
  eta = 0.1914908,
  max_depth = 5,
  subsample = 0.8674295,
  min_child_weight = 100.0327,
  booster = "gbtree",
  objective = "reg:gamma",
  eval_metric = "gamma-nloglik")

set.seed(133)
xgbcv <- xgb.cv(params = param,
                nrounds = 135,
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

Sc.MSE <- sapply(xgbcv$folds,function(x){(train2$AMT_Claim[x]-unlist(xgbcv$pred[x])*train2$NB_Claim[x])^2/1000000})


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




"The same model can be used to compute the scores on the *test* set.  
We also observe that the XGBoost approach is the most effective.
"



dtrain <- xgb.DMatrix(data = data.matrix(train2[, paste(all.vars2)]), label = train2$M_Claim)
#setinfo(dtrain,"base_margin",log(train2$expo))
folds <-list(fold1 = which(train2$fold == 1),
             fold2 = which(train2$fold == 2),
             fold3 = which(train2$fold == 3),
             fold4 = which(train2$fold == 4),
             fold5 = which(train2$fold == 5))

param <- list(
  eta = 0.1914908,
  max_depth = 5,
  subsample = 0.8674295,
  min_child_weight = 100.0327,
  booster = "gbtree",
  objective = "reg:gamma",
  eval_metric = "gamma-nloglik")

set.seed(133)
fit.xgb <- xgb.train(params = param,
                     nrounds = 135,
                     data = dtrain)

train2$pred.xgb <- predict(fit.xgb, dtrain, type='response')*train2$NB_Claim
train2$pred.xgb.off <- predict(fit.xgb, dtrain, type='response')

dtest <- xgb.DMatrix(data = data.matrix(test2[, paste(all.vars2)]), label = test2$M_Claim)
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




# Variables Importance

importance_matrix <- xgb.importance(dimnames(dtrain)[[2]], model = fit.xgb)
xgb.ggplot.importance(importance_matrix,top_n=15) + theme(text = element_text(size=15))


# RESIDUALS AND PROTECTED VARIABLES

"As we did for the GLM-net model, we can check whether the 
protected variables we excluded from the analysis retain 
predictive capacity.
"
"he conclusions are consistent with those obtained with 
the GLM-net model: most of the effect seems to be captured 
by the traditional and telematic variables"


moy.xgb <- sum(train2$pred.xgb.off)/sum(train2$M_Claim)

meansev.inv <- sum(train2$NB_Claim)/sum(train2$M_Claim)
meanpred.inv <- sum(train2$pred.xgb.off)/sum(train2$M_Claim)


temp2 <- train2 %>%
  dplyr::mutate(Duration.y = Duration/365.25, 
                Insured.age = pmin(Insured.age, 80),
                Group = ceiling(Credit.score/25) * 25) %>%
  group_by(Group) %>% 
  summarize(M_Claim=sum(M_Claim),
            pred=sum(pred.xgb.off),
            nbclaim = n()) %>% 
  mutate(sev = meansev.inv*M_Claim/nbclaim,
         sev2 = meanpred.inv*M_Claim/pred)

Graph_resCS <- ggplot() + 
  geom_smooth(aes(x=Group, y=sev, weight = nbclaim, color='Observed'),se=F, size=1, data=temp2) + 
  geom_smooth(aes(x=Group, y=sev2, weight = nbclaim, color='Residuals'),se=F, size=1, data=temp2) + 
  labs(x = 'Credit Score',
       y = 'Relativity') +
  geom_hline(yintercept = 1, linetype='dashed')+
  guides(color = guide_legend(title = "")) +
  theme_classic()+    theme(legend.position = 'bottom', legend.direction = "horizontal")

print(Graph_resCS)
# save(Graph_resCS, file = "Data/Graph_resCS_sev.rdata")


### Age of the insured


meansev.inv <- sum(train2$NB_Claim)/sum(train2$M_Claim)
meanpred.inv <- sum(train2$pred.xgb.off)/sum(train2$M_Claim)


temp2 <- train2 %>%
  dplyr::mutate(Duration.y = Duration/365.25, 
                Insured.age = pmin(Insured.age, 80),
                Group = ceiling(Insured.age/5) * 5) %>%
  group_by(Group) %>% 
  summarize(M_Claim=sum(M_Claim),
            pred=sum(pred.xgb.off),
            nbclaim = n()) %>% 
  mutate(sev = meansev.inv*M_Claim/nbclaim,
         sev2 = meanpred.inv*M_Claim/pred)

Graph_resAge <- ggplot() + 
  geom_smooth(aes(x=Group, y=sev, weight = nbclaim, color='Observed'),se=F, size=1, data=temp2) + 
  geom_smooth(aes(x=Group, y=sev2, weight = nbclaim, color='Residuals'),se=F, size=1, data=temp2) + 
  labs(x = 'Insured.age',
       y = 'Relativity') +
  geom_hline(yintercept = 1, linetype='dashed')+
  guides(color = guide_legend(title = "")) +
  theme_classic()+    theme(legend.position = 'bottom', legend.direction = "horizontal")

print(Graph_resAge)
# save(Graph_resAge, file = "Data/Graph_resAge_sev.rdata")

###

temp <- train2 %>%
  mutate(Var_ = Insured.sex) %>%
  group_by(Var_) %>%
  summarize(M_Claim=sum(M_Claim),
            pred=sum(pred.xgb),
            nbclaim = n()) %>% 
  mutate(sev = meansev.inv*M_Claim/nbclaim,
         sev2 = meanpred.inv*M_Claim/pred)

temp$sev <- temp$sev/temp$sev[1]
temp$sev2 <- temp$sev2/temp$sev2[1]

ggplot() + #start plot by by plotting bars
  geom_point(data = temp, aes(x = Var_, y = (sev2), group = 1, color='Residuals'), size=3) +
  geom_line(data = temp, aes(x = Var_, y = (sev2), group = 1, color='Residuals'), size=0.7) +
  geom_point(data = temp, aes(x = Var_, y = (sev), group = 1, color='Observed'), size=3) +
  geom_line(data = temp, aes(x = Var_, y = (sev), group = 1, color='Observed'), size=0.7) +
  labs(x = 'Sex of the insured', y = 'Relativity') +
  geom_hline(yintercept = 1, linetype='dashed')+
  #ylim(max(temp$freq, temp$freq2)*0.95, max(temp$freq, temp$freq2)*1.05)+
  guides(color=guide_legend(title="")) +
  theme_classic()+    theme(legend.position = 'bottom', legend.direction = "horizontal")

###

temp <- train2 %>%
  mutate(Var_ = Marital) %>%
  group_by(Var_) %>%
  summarize(M_Claim=sum(M_Claim),
            pred=sum(pred.xgb),
            nbclaim = n()) %>% 
  mutate(sev = meansev.inv*M_Claim/nbclaim,
         sev2 = meanpred.inv*M_Claim/pred)

temp$sev <- temp$sev/temp$sev[1]
temp$sev2 <- temp$sev2/temp$sev2[1]

ggplot() + #start plot by by plotting bars
  geom_point(data = temp, aes(x = Var_, y = (sev2), group = 1, color='Residuals'), size=3) +
  geom_line(data = temp, aes(x = Var_, y = (sev2), group = 1, color='Residuals'), size=0.7) +
  geom_point(data = temp, aes(x = Var_, y = (sev), group = 1, color='Observed'), size=3) +
  geom_line(data = temp, aes(x = Var_, y = (sev), group = 1, color='Observed'), size=0.7) +
  labs(x = 'Marital status of the insured', y = 'Relativity') +
  geom_hline(yintercept = 1, linetype='dashed')+
  #ylim(max(temp$freq, temp$freq2)*0.95, max(temp$freq, temp$freq2)*1.05)+
  guides(color=guide_legend(title="")) +
  theme_classic()+    theme(legend.position = 'bottom', legend.direction = "horizontal")

###

temp <- train2 %>%
  mutate(Var_ = Territory) %>%
  group_by(Var_) %>%
  summarize(M_Claim=sum(M_Claim),
            pred=sum(pred.xgb.off),
            nbclaim = n()) %>% 
  mutate(sev = meansev.inv*M_Claim/nbclaim,
         sev2 = meanpred.inv*M_Claim/pred)

#temp$sev <- temp$sev/temp$sev[1]
#temp$sev2 <- temp$sev2/temp$sev2[1]

Graph_resTerr <- ggplot() + #start plot by by plotting bars
  #geom_point(data = temp, aes(x = Var_, y = (sev2), group = 1, color='Residuals'), size=3) +
  geom_line(data = temp, aes(x = Var_, y = (sev2), group = 1, color='Residuals'), size=0.7) +
  #geom_point(data = temp, aes(x = Var_, y = (sev), group = 1, color='Observed'), size=3) +
  geom_line(data = temp, aes(x = Var_, y = (sev), group = 1, color='Observed'), size=0.7) +
  labs(x = 'Territory', y = 'Relativity') +
  geom_hline(yintercept = 1, linetype='dashed')+
  scale_x_discrete(labels = NULL, breaks = NULL)+
  #ylim(max(temp$freq, temp$freq2)*0.95, max(temp$freq, temp$freq2)*1.05)+
  guides(color=guide_legend(title="")) +
  theme_classic()+    theme(legend.position = 'bottom', legend.direction = "horizontal")

print(Graph_resTerr)
# save(Graph_resTerr, file = "Data/Graph_resTerr_sev.rdata")




### XGBOOST ON RESIDUALS

var.sens <- c("Marital", "Insured.sex", "Credit.score", "Insured.age", "Territory")    

dtrain <- xgb.DMatrix(data = data.matrix(train2[, paste(var.sens)]), label = train2$M_Claim)
# setinfo(dtrain,"base_margin",log(train2$pred.xgb.off))
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
    Score = -min(xgbcv$evaluation_log$test_gamma_nloglik_mean),
    nrounds = xgbcv$best_iteration
  )
  
  return(lst)
}

base.rounds <- 500
set.seed(254)
bayes_out <- bayesOpt(FUN = obj_func, bounds = bounds, initPoints = length(bounds) + 2, iters.n = 5)
comp <- bayes_out$scoreSummary[which(bayes_out$scoreSummary$Score== max(bayes_out$scoreSummary$Score))]
comp 


var.sens <- c("Marital", "Insured.sex", "Credit.score", "Insured.age", "Territory")    

dtrain <- xgb.DMatrix(data = data.matrix(train2[, paste(var.sens)]), label = train2$M_Claim)
setinfo(dtrain,"base_margin",log(train2$pred.xgb.off))
folds <-list(fold1 = which(train2$fold == 1),
             fold2 = which(train2$fold == 2),
             fold3 = which(train2$fold == 3),
             fold4 = which(train2$fold == 4),
             fold5 = which(train2$fold == 5))

param <- list(
  eta = 0.02337437,
  max_depth = 26,
  subsample = 0.8097923,
  min_child_weight = 123.6033,
  booster = "gbtree",
  objective = "reg:gamma",
  eval_metric = "gamma-nloglik")

set.seed(533)
fit.xgb2 <- xgb.train(params = param,
                      nrounds = 103,
                      data = dtrain)

var.sens <- c("Marital", "Insured.sex", "Credit.score", "Insured.age", "Territory")    

train2$pred.xgb <- predict(fit.xgb2, dtrain, type='response')*train2$NB_Claim

dtest <- xgb.DMatrix(data = data.matrix(test2[, paste(var.sens)]), label = test2$M_Claim)
setinfo(dtest,"base_margin",log(test2$pred.xgb.off))

test2$pred.base <- predict(fit.xgb2, dtest, type='response')*test2$NB_Claim

variance <- (sum((train2$pred.xgb - (train2$AMT_Claim))^2)/(length(train2$AMT_Claim)))
phi <- variance/mean(train2$AMT_Claim)^2


Result_ <- data.frame(t(Score.pred.sev(test2$pred.base, test2$AMT_Claim, phi)/nrow(test2)))
Result_ <- cbind('XGBoost*', Result_)
colnames(Result_) <- c("Model", "Sc.log", "Sc.MSE")

Result_all <- rbind(Result_all, Result_)

#save(Result_all, file='Data/ResultsSynth_sev.Rda')

knitr::kable(Result_all, align = "ccc", digits = c(0, 5, 5), format.args = list(big.mark = ","))%>%   
  kable_styling(bootstrap_options = "striped", full_width = F)  



# SENS VARIABLES

var.sensWO <- c("Marital", "Insured.sex", "Insured.age", "Territory")    

dtrain <- xgb.DMatrix(data = data.matrix(train2[, paste(var.sensWO)]), label = train2$M_Claim)
setinfo(dtrain,"base_margin",log(train2$pred.xgb.off))
folds <-list(fold1 = which(train2$fold == 1),
             fold2 = which(train2$fold == 2),
             fold3 = which(train2$fold == 3),
             fold4 = which(train2$fold == 4),
             fold5 = which(train2$fold == 5))

param <- list(
  eta = 0.02337437,
  max_depth = 26,
  subsample = 0.8097923,
  min_child_weight = 123.6033,
  booster = "gbtree",
  objective = "reg:gamma",
  eval_metric = "gamma-nloglik")

set.seed(533)
fit.xgb3 <- xgb.train(params = param,
                      nrounds = 103,
                      data = dtrain)

var.sensWO <- c("Marital", "Insured.sex", "Insured.age", "Territory")    

train2$pred.xgb <- predict(fit.xgb3, dtrain, type='response')*train2$NB_Claim

dtest <- xgb.DMatrix(data = data.matrix(test2[, paste(var.sensWO)]), label = test2$M_Claim)
setinfo(dtest,"base_margin",log(test2$pred.xgb.off))

test2$pred.base <- predict(fit.xgb3, dtest, type='response')*test2$NB_Claim

variance <- (sum((train2$pred.xgb - (train2$AMT_Claim))^2)/(length(train2$AMT_Claim)))
phi <- variance/mean(train2$AMT_Claim)^2


Result_ <- data.frame(t(Score.pred.sev(test2$pred.base, test2$AMT_Claim, phi)/nrow(test2)))
Result_ <- cbind('XGBoost**', Result_)
colnames(Result_) <- c("Model", "Sc.log", "Sc.MSE")

Result_all <- rbind(Result_all, Result_)

knitr::kable(Result_all, align = "ccc", digits = c(0, 5, 5), format.args = list(big.mark = ","))%>%   
  kable_styling(bootstrap_options = "striped", full_width = F)  


## MOST IMPORTANT PROTECTED VARIABLES


# importance_matrix <- xgb.importance(dimnames(dtrain)[[2]], model = fit.xgb2)
importance_matrix <- xgb.importance(model = fit.xgb2)
xgb.ggplot.importance(importance_matrix,top_n=15) + theme(text = element_text(size=15))
