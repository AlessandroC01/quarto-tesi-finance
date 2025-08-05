
##################### CLAIM SEVERITY ###################################
########################################################################

# DATA SUMMARY<-----
# TRAD. VARIABLE 
# TELEMATICS VARIABLE 

# The objective of this chapter is the same as that of Chapter 2. 
# However, we will analyze severity.


## Packages 


library(tidyverse)
library(vtable)
library(rpart)
library(repr)
library(rpart.plot)
#library(rfCountData)
library(gam)

## Data 
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

#########################################################################

# TRADITIONAL COVIARIATES 

### Credit score 
train %>%
  dplyr::mutate(Duration.y = Duration/365.25, 
                Group = ceiling(Credit.score/25) * 25) %>%
  group_by(Group) %>% 
  summarize(M_Claim=mean(M_Claim),
            expo=sum(Duration.y)) %>% 
  mutate(sev = M_Claim/1) %>%
  ggplot(aes(x=Group, y=sev)) + 
  geom_point(aes(size=expo), color='black') + 
  geom_smooth(aes(weight = expo),se=T, color='red', size=1) + 
  labs(x = 'Credit score',
       y = 'Claim severity') +
  guides(size = guide_legend(title = "Number of claims")) +
  #  xlim(0,1)+ylim(0, 0.06)+
  theme_bw()

### Age of the insured
train %>%
  dplyr::mutate(Duration.y = Duration/365.25, 
                Group = ceiling(Insured.age/10) * 10) %>%
  group_by(Group) %>% 
  summarize(M_Claim=mean(M_Claim),
            expo=sum(Duration.y)) %>% 
  mutate(sev = M_Claim/1) %>%
  ggplot(aes(x=Group, y=sev)) + 
  geom_point(aes(size=expo), color='black') + scale_size_continuous(range = c(1,4)) + 
  geom_smooth(aes(weight = expo),se=T, color='red', size=1) + 
  labs(x = 'Age',
       y = 'Claim severity') +
  guides(size = guide_legend(title = "Number of claims")) +
  #  xlim(0,1)+ylim(0, 0.06)+
  theme_bw()

### Sex of the insured
temp <- train %>%
  mutate(Var_ = Insured.sex) %>%
  group_by(Var_) %>%
  summarize(Msev = mean(M_Claim),
            expo = sum(Duration/365.25)) %>%
  mutate(sev = Msev/1)

div <- min(temp$expo/temp$sev)/1.5

ggplot(data = temp, aes(x = Var_, y = expo, linetype = "Number of claims")) + #start plot by by plotting bars
  geom_bar(stat = "identity", alpha=0.5) + 
  geom_point(data = temp, aes(x = Var_, y = (sev)*div, group = 1), color='red', size=3,
             inherit.aes = FALSE) +
  geom_line(data = temp, aes(x = Var_, y = (sev)*div, group = 1, linetype = "Claim severity"), color='red', size=0.7,
            inherit.aes = FALSE) +
  labs(x = 'Sex',
       y = 'Number of claims') +
  scale_y_continuous(sec.axis = sec_axis(~./div, name = "Claim severity")) +
  guides(linetype=guide_legend(title="")) +
  theme_classic()+
  theme(legend.position = 'bottom', legend.direction = "horizontal")


### Marital Status
temp <- train %>%
  mutate(Var_ = Marital) %>%
  group_by(Var_) %>%
  summarize(mSev = mean(M_Claim),
            expo = sum(Duration/365.25)) %>%
  mutate(sev = mSev/1)

div <- min(temp$expo/temp$sev)/0.8

ggplot(data = temp, aes(x = Var_, y = expo, linetype = "Number of claims")) + #start plot by by plotting bars
  geom_bar(stat = "identity", alpha=0.5) + 
  geom_point(data = temp, aes(x = Var_, y = (sev)*div, group = 1), color='red', size=3,
             inherit.aes = FALSE) +
  geom_line(data = temp, aes(x = Var_, y = (sev)*div, group = 1, linetype = "Claim Severity"), color='red', size=0.7,
            inherit.aes = FALSE) +
  labs(x = 'Marital status',
       y = 'Number of claims') +
  scale_y_continuous(sec.axis = sec_axis(~./div, name = "Average claim Severity")) +
  guides(linetype=guide_legend(title="")) +
  theme_classic()+
  theme(legend.position = 'bottom', legend.direction = "horizontal")


### Insured's territory
temp <- train %>%
  mutate(Var_ = Territory) %>%
  group_by(Var_) %>%
  summarize(mSev = mean(M_Claim),
            expo = sum(Duration/365.25)) %>%
  mutate(sev = mSev/1)

div <- min(temp$expo/temp$sev)/0.4

ggplot(data = temp, aes(x = Var_, y = expo, linetype = "Number of claims")) + #start plot by by plotting bars
  geom_bar(stat = "identity", alpha=0.5) + 
  geom_point(data = temp, aes(x = Var_, y = (sev)*div, group = 1), color='red', size=1.5,
             inherit.aes = FALSE) +
  geom_line(data = temp, aes(x = Var_, y = (sev)*div, group = 1, linetype = "Average Claim Severity"), color='red', size=0.7,
            inherit.aes = FALSE) +
  labs(x = 'Territory',
       y = 'Number of claims') +
  scale_x_discrete(labels = NULL, breaks = NULL)+
  scale_y_continuous(sec.axis = sec_axis(~./div, name = "Average claim severity")) +
  guides(linetype=guide_legend(title="")) +
  theme_classic()+
  theme(legend.position = 'bottom', legend.direction = "horizontal")

### Correlation

trad.vars  <- c("Marital", "Insured.sex", "Credit.score", "Insured.age", "Territory")

data = data.matrix(train[, paste(trad.vars)])
correlation_matrix <- abs(cor(data))

plt <- ggplot(data = as.data.frame(as.table(correlation_matrix)), aes(x = Var1, y = Var2, fill = Freq)) +
  geom_tile() +
  scale_fill_gradientn(colors = colorRampPalette(c("gray", "blue", "red"))(50), name = "Correlation") +
  geom_text(aes(label = round(Freq, 2)), color = "white", size=4) +
  #scale_fill_continuous(na.value = 'white') +
  theme_minimal() +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_text(size=10, angle = 45, hjust = 1), 
        axis.text.y = element_text(size = 10),
        plot.title = element_text(size = 10),   # Adjust plot title font size
        legend.text = element_text(size = 10),
        legend.title = element_text(size=10))
print(plt)
# The correlation matrix indicates the level of dependence 
#between each of the sensitive variables. 

## other coviariates
### Car Age
train %>%
  dplyr::mutate(Duration.y = Duration/365.25, 
                Group = ceiling(Car.age/1) * 1) %>%
  group_by(Group) %>% 
  summarize(M_Claim=mean(M_Claim),
            expo=sum(Duration.y)) %>% 
  mutate(sev = M_Claim/1) %>%
  ggplot(aes(x=Group, y=sev)) + 
  geom_point(aes(size=expo), color='black') + scale_size_continuous(range = c(1,4)) +
  geom_smooth(aes(weight = expo),se=T, color='red', size=1) + 
  labs(x = 'Car age',
       y = 'Claim severity') +
  guides(size = guide_legend(title = "Number of claims")) +
  #  xlim(0,1)+ylim(0, 0.06)+
  theme_bw()

### Use of the car
temp <- train %>%
  mutate(Var_ = Car.use) %>%
  group_by(Var_) %>%
  summarize(mSev = mean(M_Claim),
            expo = sum(Duration/365.25)) %>%
  mutate(sev = mSev/1)

div <- min(temp$expo/temp$sev)/0.07

ggplot(data = temp, aes(x = Var_, y = expo, linetype='Number of claims')) + #start plot by by plotting bars
  geom_bar(stat = "identity", alpha=0.5) + 
  geom_point(data = temp, aes(x = Var_, y = (sev)*div, group = 1), color='red', size=3,
             inherit.aes = FALSE) +
  geom_line(data = temp, aes(x = Var_, y = (sev)*div, group = 1, linetype = 'Claim Severity'), color='red', size=0.7,
            inherit.aes = FALSE) +
  labs(x = 'Car Use',
       y = 'Number of claims') +
  #scale_x_discrete(labels = NULL, breaks = NULL)+
  scale_y_continuous(sec.axis = sec_axis(~./div, name = "Claim Severity"))  + 
  guides(linetype=guide_legend(title="")) +
  theme_classic()+
  theme(legend.position = 'bottom', legend.direction = "horizontal")


### Region (urban vs rural)
temp <- train %>%
  mutate(Var_ = Region) %>%
  group_by(Var_) %>%
  summarize(mSev = mean(M_Claim),
            expo = sum(Duration/365.25)) %>%
  mutate(sev = mSev/1)

div <- min(temp$expo/temp$sev)/0.5

ggplot(data = temp, aes(x = Var_, y = expo, linetype='Number of claims')) + #start plot by by plotting bars
  geom_bar(stat = "identity", alpha=0.5) + 
  geom_point(data = temp, aes(x = Var_, y = (sev)*div, group = 1), color='red', size=3,
             inherit.aes = FALSE) +
  geom_line(data = temp, aes(x = Var_, y = (sev)*div, group = 1, linetype='Claim Severity'), color='red', size=0.7,
            inherit.aes = FALSE) +
  labs(x = 'Region',
       y = 'Number of claims') +
  #scale_x_discrete(labels = NULL, breaks = NULL)+
  scale_y_continuous(sec.axis = sec_axis(~./div, name = "Claim Severity")) + 
  guides(linetype=guide_legend(title="")) +
  theme_classic()+
  theme(legend.position = 'bottom', legend.direction = "horizontal")


### Years without Claim
train %>%
  dplyr::mutate(Duration.y = Duration/365.25, 
                Group = pmin(Years.noclaims, 60)) %>%
  group_by(Group) %>% 
  summarize(M_Claim=mean(M_Claim),
            expo=sum(Duration.y)) %>% 
  mutate(sev = M_Claim/1) %>%
  ggplot(aes(x=Group, y=sev)) + 
  geom_point(aes(size=expo), color='black') + scale_size_continuous(range = c(1,4)) +
  geom_smooth(aes(weight = expo),se=T, color='red', size=1) + 
  labs(x = 'Years without claim',
       y = 'Claim severity') +
  guides(size = guide_legend(title = "Number of claims")) +
  #xlim(0,40000)+
  #ylim(0, 0.06)+
  theme_bw()

### Correlation
trad.vars  <- c("Marital", "Insured.sex", "Credit.score", "Insured.age", "Territory",
                "Car.use", "Region", "Car.age", "Years.noclaims")

data = data.matrix(train[, paste(trad.vars)])
correlation_matrix <- abs(cor(data))

corr1 <- correlation_matrix[6:9, 6:9]

ggplot(data = as.data.frame(as.table(corr1)), aes(x = Var1, y = Var2, fill = Freq)) +
  geom_tile() +
  scale_fill_gradientn(colors = colorRampPalette(c("gray", "blue", "red"))(50), name = "Correlation") +
  geom_text(aes(label = round(Freq, 2)), color = "white", size=6) +
  #scale_fill_continuous(na.value = 'white') +
  theme_minimal() +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_text(size=13, angle = 45, hjust = 1), 
        axis.text.y = element_text(size = 13),
        plot.title = element_text(size = 13),   # Adjust plot title font size
        legend.text = element_text(size = 13),
        legend.title = element_text(size=13))

corr1 <- correlation_matrix[1:5, 6:9]

ggplot(data = as.data.frame(as.table(corr1)), aes(x = Var1, y = Var2, fill = Freq)) +
  geom_tile() +
  scale_fill_gradientn(colors = colorRampPalette(c("gray", "blue", "red"))(50), name = "Correlation") +
  geom_text(aes(label = round(Freq, 2)), color = "white", size=6) +
  #scale_fill_continuous(na.value = 'white') +
  theme_minimal() +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_text(size=13, angle = 45, hjust = 1), 
        axis.text.y = element_text(size = 13),
        plot.title = element_text(size = 13),   # Adjust plot title font size
        legend.text = element_text(size = 13),
        legend.title = element_text(size=13))

#############################################################################

# TELEMATICS COVIARIATES 


### Vehicle usage level
### Annual Miles Driven
train %>%
  dplyr::mutate(Duration.y = Duration/365.25, 
                Group = ifelse(Total.miles.driven < 20000, ceiling(Total.miles.driven/1000) * 1000, ceiling(Total.miles.driven/5000) * 5000)) %>%
  group_by(Group) %>% 
  summarize(M_Claim=mean(M_Claim),
            expo=sum(Duration.y)) %>% 
  mutate(sev = M_Claim/1) %>%
  ggplot(aes(x=Group, y=sev)) + 
  geom_point(aes(size=expo), color='black') + scale_size_continuous(range = c(1,4)) +
  geom_smooth(aes(weight = expo),se=T, color='red', size=1) + 
  labs(x = 'Total miles driven',
       y = 'Claim severity') +
  guides(size = guide_legend(title = "Number of claims")) +
  xlim(0,25000)+
  ylim(0, 7500)+
  theme_bw()

# distance driven in a car can constitute a more precise measure 
# of risk exposure than simply the duration of the insurance contract

## Contract duration revisited
train %>%
  dplyr::mutate(Miles.per.day = Total.miles.driven/Duration, 
                Duration.y = Duration/365.25, 
                Group = ceiling(Miles.per.day/7.5) * 7.5) %>%
  group_by(Group) %>% 
  summarize(M_Claim=mean(M_Claim),
            expo=sum(Duration.y)) %>% 
  mutate(sev = M_Claim/1) %>%
  ggplot(aes(x=Group, y=sev)) + 
  geom_point(aes(size=expo), color='black') + scale_size_continuous(range = c(1,4)) +
  geom_smooth(aes(weight = expo),se=T, color='red', size=1) + 
  labs(x = 'Average miles driven per day',
       y = 'Claim severity') +
  guides(size = guide_legend(title = "Number of claims")) +
  xlim(0,127.5)+
  #ylim(0, 0.31)+
  theme_bw()

### Days per week
train %>%
  dplyr::mutate(Duration.y = Duration/365.25, 
                Group = ceiling(Avgdays.week/0.5) * 0.5) %>%
  group_by(Group) %>% 
  summarize(M_Claim=mean(M_Claim),
            expo=sum(Duration.y)) %>% 
  mutate(sev = M_Claim/1) %>%
  ggplot(aes(x=Group, y=sev)) + 
  geom_point(aes(size=expo), color='black') + scale_size_continuous(range = c(1,4)) +
  geom_smooth(aes(weight = expo),se=T, color='red', size=1) + 
  labs(x = 'Average number of days per week',
       y = 'Claim severity') +
  guides(size = guide_legend(title = "Number of claims")) +
  #xlim(0,40000)+
  #ylim(0, 0.31)+
  theme_bw()

### Correlation
trad.vars  <- c("Marital", "Insured.sex", "Credit.score", "Insured.age", "Territory",
                "Miles.per.day", "Avgdays.week")

train <- train %>%
  dplyr::mutate(Miles.per.day = Total.miles.driven/Duration)

data = data.matrix(train[, paste(trad.vars)])
correlation_matrix <- abs(cor(data))

corr1 <- correlation_matrix[6:7, 6:7]

ggplot(data = as.data.frame(as.table(corr1)), aes(x = Var1, y = Var2, fill = Freq)) +
  geom_tile() +
  scale_fill_gradientn(colors = colorRampPalette(c("gray", "blue", "red"))(50), name = "Correlation") +
  geom_text(aes(label = round(Freq, 2)), color = "white", size=6) +
  #scale_fill_continuous(na.value = 'white') +
  theme_minimal() +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_text(size=13, angle = 45, hjust = 1), 
        axis.text.y = element_text(size = 13),
        plot.title = element_text(size = 13),   # Adjust plot title font size
        legend.text = element_text(size = 13),
        legend.title = element_text(size=13))

corr1 <- correlation_matrix[1:5, 6:7]

ggplot(data = as.data.frame(as.table(corr1)), aes(x = Var1, y = Var2, fill = Freq)) +
  geom_tile() +
  scale_fill_gradientn(colors = colorRampPalette(c("gray", "blue", "red"))(50), name = "Correlation") +
  geom_text(aes(label = round(Freq, 2)), color = "white", size=6) +
  #scale_fill_continuous(na.value = 'white') +
  theme_minimal() +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_text(size=13, angle = 45, hjust = 1), 
        axis.text.y = element_text(size = 13),
        plot.title = element_text(size = 13),   # Adjust plot title font size
        legend.text = element_text(size = 13),
        legend.title = element_text(size=13))

# Once again, we can observe the dependency between 
# the covariates by looking at the results below. 

## Type of vehicle usage {#sec-tele2}

#Instead of using the telematics device solely to measure vehicle usage, 
#it is also possible to see if certain types of vehicle 
#usage are indicators of a higher risk of claims. In this section, 
#we will analyze certain telematics information that
#we classify as types of usage.


### Days
# Thus, vehicle usage for each day appears to signify something, 
# but the information provided by these covariates likely needs 
# transformation.
div <- 1/15 

var <- 'Pct.drive.mon'
train %>%
  dplyr::mutate(Duration.y = Duration/365.25, 
                Group = ceiling(get(var)/div) * div) %>%
  group_by(Group) %>% 
  summarize(M_Claim=mean(M_Claim),
            expo=sum(Duration.y)) %>% 
  mutate(sev = M_Claim/1) %>%
  ggplot(aes(x=Group, y=sev)) + 
  geom_point(aes(size=expo), color='black') + scale_size_continuous(range = c(1,4)) +
  geom_smooth(aes(weight = expo),se=F, color='red', size=1) + 
  labs(x = 'Usage percentage per day',
       y = 'Claims severity') +
  guides(size = guide_legend(title = "Number of claims")) +
  #xlim(0,40000)+
  ylim(0, 6000)+
  theme_bw()


var <- 'Pct.drive.tue'
train %>%
  dplyr::mutate(Duration.y = Duration/365.25, 
                Group = ceiling(get(var)/div) * div) %>%
  group_by(Group) %>% 
  summarize(M_Claim=mean(M_Claim),
            expo=sum(Duration.y)) %>% 
  mutate(sev = M_Claim/1) %>%
  ggplot(aes(x=Group, y=sev)) + 
  geom_point(aes(size=expo), color='black') + scale_size_continuous(range = c(1,4)) +
  geom_smooth(aes(weight = expo),se=F, color='red', size=1) + 
  labs(x = 'Usage percentage per day',
       y = 'Claim severity') +
  guides(size = guide_legend(title = "Number of claims")) +
  #xlim(0,40000)+
  ylim(0, 6000)+
  theme_bw()


var <- 'Pct.drive.wed'
train %>%
  dplyr::mutate(Duration.y = Duration/365.25, 
                Group = ceiling(get(var)/div) * div) %>%
  group_by(Group) %>% 
  summarize(M_Claim=mean(M_Claim),
            expo=sum(Duration.y)) %>% 
  mutate(sev = M_Claim/1) %>%
  ggplot(aes(x=Group, y=sev)) + 
  geom_point(aes(size=expo), color='black') + scale_size_continuous(range = c(1,4)) +
  geom_smooth(aes(weight = expo),se=F, color='red', size=1) + 
  labs(x = 'Usage percentage per day',
       y = 'Claim severity') +
  guides(size = guide_legend(title = "Number of claims")) +
  #xlim(0,40000)+
  ylim(0, 8000)+
  theme_bw()


var <- 'Pct.drive.thr'
train %>%
  dplyr::mutate(Duration.y = Duration/365.25, 
                Group = ceiling(get(var)/div) * div) %>%
  group_by(Group) %>% 
  summarize(M_Claim=mean(M_Claim),
            expo=sum(Duration.y)) %>% 
  mutate(sev = M_Claim/1) %>%
  ggplot(aes(x=Group, y=sev)) + 
  geom_point(aes(size=expo), color='black') + scale_size_continuous(range = c(1,4)) +
  geom_smooth(aes(weight = expo),se=F, color='red', size=1) + 
  labs(x = 'Usage percentage per day',
       y = 'Claim severity') +
  guides(size = guide_legend(title = "Number of claims")) +
  #xlim(0,40000)+
  ylim(0, 6000)+
  theme_bw()


var <- 'Pct.drive.fri'
train %>%
  dplyr::mutate(Duration.y = Duration/365.25, 
                Group = ceiling(get(var)/div) * div) %>%
  group_by(Group) %>% 
  summarize(M_Claim=mean(M_Claim),
            expo=sum(Duration.y)) %>% 
  mutate(sev = M_Claim/1) %>%
  ggplot(aes(x=Group, y=sev)) + 
  geom_point(aes(size=expo), color='black') + scale_size_continuous(range = c(1,4)) +
  geom_smooth(aes(weight = expo),se=F, color='red', size=1) + 
  labs(x = 'Usage percentage per day',
       y = 'Claim severity') +
  guides(size = guide_legend(title = "Number of claims")) +
  #xlim(0,40000)+
  ylim(0, 20000)+
  theme_bw()


var <- 'Pct.drive.sat'
train %>%
  dplyr::mutate(Duration.y = Duration/365.25, 
                Group = ceiling(get(var)/div) * div) %>%
  group_by(Group) %>% 
  summarize(M_Claim=mean(M_Claim),
            expo=sum(Duration.y)) %>% 
  mutate(sev = M_Claim/1) %>%
  ggplot(aes(x=Group, y=sev)) + 
  geom_point(aes(size=expo), color='black') + scale_size_continuous(range = c(1,4)) +
  geom_smooth(aes(weight = expo),se=F, color='red', size=1) + 
  labs(x = 'Usage percentage per day',
       y = 'Claim severity') +
  guides(size = guide_legend(title = "Number of claims")) +
  #xlim(0,40000)+
  ylim(0, 8000)+
  theme_bw()


var <- 'Pct.drive.sun'
train %>%
  dplyr::mutate(Duration.y = Duration/365.25, 
                Group = ceiling(get(var)/div) * div) %>%
  group_by(Group) %>% 
  summarize(M_Claim=mean(M_Claim),
            expo=sum(Duration.y)) %>% 
  mutate(sev = M_Claim/1) %>%
  ggplot(aes(x=Group, y=sev)) + 
  geom_point(aes(size=expo), color='black') + scale_size_continuous(range = c(1,4)) +
  geom_smooth(aes(weight = expo),se=F, color='red', size=1) + 
  labs(x = 'Usage percentage per day',
       y = 'Claim severity') +
  guides(size = guide_legend(title = "Number of claims")) +
  #xlim(0,40000)+
  ylim(0, 8000)+
  theme_bw()

### Days (2)
#create new variables that may better represent the risk. 
#We thus create the following variables:  
  
#1) A variable identifying the maximum value of vehicle usage 
#for each day;  

#) A variable identifying the minimum value of vehicle
#usage for each day;  

#) A variable measuring the difference between the maximum 
#and minimum values, which have just been calculated. This variable can thus identify insured individuals who use their vehicle more on specific days, or conversely, insured individuals who typically refrain from using their vehicle on certain days of the week.
df2 <- train %>%
  mutate(max.day = pmax(Pct.drive.mon, Pct.drive.tue, Pct.drive.wed, Pct.drive.thr, Pct.drive.fri, Pct.drive.sat, Pct.drive.sun),
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
         Dayformin = ifelse(min.day == Pct.drive.sun, 'Sunday', Dayformin))

div <- 1/25 
var <- 'max.day'
df2 %>%
  dplyr::mutate(Duration.y = Duration/365.25, 
                Group = ceiling(get(var)/div) * div) %>%
  group_by(Group) %>% 
  summarize(M_Claim=mean(M_Claim),
            expo=sum(Duration.y)) %>% 
  mutate(sev = M_Claim/1) %>%
  ggplot(aes(x=Group, y=sev)) + 
  geom_point(aes(size=expo), color='black') + scale_size_continuous(range = c(1,4)) +
  geom_smooth(aes(weight = expo),se=T, color='red', size=1) + 
  labs(x = 'Maximum usage percentage per day',
       y = 'Claim severity') +
  guides(size = guide_legend(title = "Number of claims")) +
  #xlim(0,40000)+
  #ylim(-0.01, 0.06)+
  theme_bw()

div <- 1/75 
var <- 'min.day'
df2 %>%
  dplyr::mutate(Duration.y = Duration/365.25, 
                Group = ceiling(get(var)/div) * div) %>%
  group_by(Group) %>% 
  summarize(M_Claim=mean(M_Claim),
            expo=sum(Duration.y)) %>% 
  mutate(sev = M_Claim/1) %>%
  ggplot(aes(x=Group, y=sev)) + 
  geom_point(aes(size=expo), color='black') + scale_size_continuous(range = c(1,4)) +
  geom_smooth(aes(weight = expo),se=T, color='red', size=1) + 
  labs(x = 'Minimum usage percentage per day',
       y = 'Claim severity') +
  guides(size = guide_legend(title = "Number of claims")) +
  #xlim(0,40000)+
  #ylim(-0.01, 0.06)+
  theme_bw()

div <- 1/25 
var <- 'max.min'
df2 %>%
  dplyr::mutate(Duration.y = Duration/365.25, 
                Group = ceiling(get(var)/div) * div) %>%
  group_by(Group) %>% 
  summarize(M_Claim=mean(M_Claim),
            expo=sum(Duration.y)) %>% 
  mutate(sev = M_Claim/1) %>%
  ggplot(aes(x=Group, y=sev)) + 
  geom_point(aes(size=expo), color='black') + scale_size_continuous(range = c(1,4)) +
  geom_smooth(aes(weight = expo),se=T, color='red', size=1) + 
  labs(x = 'Difference between percentages',
       y = 'Claim severity') +
  guides(size = guide_legend(title = "Number of claims")) +
  #xlim(0,40000)+
  #ylim(-0.01, 0.06)+
  theme_bw()

### Days (3)
# additional variables can also be explored: 

#1) A variable identifying the day of the week when 
#the vehicle is most used;

#2) A variable identifying the day of the week 
#when the vehicle is least used.

### Week-end
var <- 'Pct.drive.wkday'
div <- 1/25
train %>%
  dplyr::mutate(Duration.y = Duration/365.25, 
                Group = ceiling(get(var)/div) * div) %>%
  group_by(Group) %>% 
  summarize(M_Claim=mean(M_Claim),
            expo=sum(Duration.y)) %>% 
  mutate(sev = M_Claim/1) %>%
  ggplot(aes(x=Group, y=sev)) + 
  geom_point(aes(size=expo), color='black') + scale_size_continuous(range = c(1,4)) +
  geom_smooth(aes(weight = expo),se=T, color='red', size=1) + 
  labs(x = 'Usage percentage per day',
       y = 'Claim severity') +
  guides(size = guide_legend(title = "Number of claims")) +
  #xlim(0,40000)+
  #ylim(-0.01, 0.06)+
  theme_bw()

var <- 'Pct.drive.wkend'
train %>%
  dplyr::mutate(Duration.y = Duration/365.25, 
                Group = ceiling(get(var)/div) * div) %>%
  group_by(Group) %>% 
  summarize(M_Claim=mean(M_Claim),
            expo=sum(Duration.y)) %>% 
  mutate(sev = M_Claim/1) %>%
  ggplot(aes(x=Group, y=sev)) + 
  geom_point(aes(size=expo), color='black') + scale_size_continuous(range = c(1,4)) +
  geom_smooth(aes(weight = expo),se=T, color='red', size=1) + 
  labs(x = 'Usage percentage per day',
       y = 'Claim severity') +
  guides(size = guide_legend(title = "Number of claims")) +
  #xlim(0,40000)+
  #ylim(-0.01, 0.06)+
  theme_bw()

### Trip Duration
var <- 'Pct.drive.2hrs'

df2 <- train %>% mutate(VAR = get(var))
q99 <- quantile(df2$VAR, 0.99)
div <- q99/15
train %>%
  dplyr::mutate(Duration.y = Duration/365.25,
                VAR = ifelse(get(var) > q99, q99, get(var)),
                Group = ceiling(VAR/div) * div) %>%
  group_by(Group) %>% 
  summarize(M_Claim=mean(M_Claim),
            expo=sum(Duration.y)) %>% 
  mutate(sev = M_Claim/1) %>%
  ggplot(aes(x=Group, y=sev)) + 
  geom_point(aes(size=expo), color='black') + scale_size_continuous(range = c(1,4)) +
  geom_smooth(aes(weight = expo),se=T, color='red', size=1) + 
  labs(x = 'Percent of driving',
       y = 'Claim severity') +
  guides(size = guide_legend(title = "Number of claims")) +
  #xlim(0,40000)+
  #ylim(-0.01, 0.06)+
  theme_bw()

var <- 'Pct.drive.3hrs'

df2 <- train %>% mutate(VAR = get(var))
q99 <- quantile(df2$VAR, 0.99)
div <- q99/15
train %>%
  dplyr::mutate(Duration.y = Duration/365.25,
                VAR = ifelse(get(var) > q99, q99, get(var)),
                Group = ceiling(VAR/div) * div) %>%
  group_by(Group) %>% 
  summarize(M_Claim=mean(M_Claim),
            expo=sum(Duration.y)) %>% 
  mutate(sev = M_Claim/1) %>%
  ggplot(aes(x=Group, y=sev)) + 
  geom_point(aes(size=expo), color='black') + scale_size_continuous(range = c(1,4)) +
  geom_smooth(aes(weight = expo),se=T, color='red', size=1) + 
  labs(x = 'Percent of driving',
       y = 'Claim severity') +
  guides(size = guide_legend(title = "Number of claims")) +
  #xlim(0,40000)+
  #ylim(-0.01, 0.06)+
  theme_bw()

var <- 'Pct.drive.4hrs'

df2 <- train %>% mutate(VAR = get(var))
q99 <- quantile(df2$VAR, 0.99)
div <- q99/15
train %>%
  dplyr::mutate(Duration.y = Duration/365.25,
                VAR = ifelse(get(var) > q99, q99, get(var)),
                Group = ceiling(VAR/div) * div) %>%
  group_by(Group) %>% 
  summarize(M_Claim=mean(M_Claim),
            expo=sum(Duration.y)) %>% 
  mutate(sev = M_Claim/1) %>%
  ggplot(aes(x=Group, y=sev)) + 
  geom_point(aes(size=expo), color='black') + scale_size_continuous(range = c(1,4)) +
  geom_smooth(aes(weight = expo),se=T, color='red', size=1) + 
  labs(x = 'Percent of driving',
       y = 'Claim severity') +
  guides(size = guide_legend(title = "Number of claims")) +
  #xlim(0,40000)+
  #ylim(-0.01, 0.06)+
  theme_bw()

### Rush hours
var <- 'Pct.drive.rush.am'

df2 <- train %>% mutate(VAR = get(var))
q99 <- quantile(df2$VAR, 0.99)
div <- q99/15
train %>%
  dplyr::mutate(Duration.y = Duration/365.25,
                VAR = ifelse(get(var) > q99, q99, get(var)),
                Group = ceiling(VAR/div) * div) %>%
  group_by(Group) %>% 
  summarize(M_Claim=mean(M_Claim),
            expo=sum(Duration.y)) %>% 
  mutate(sev = M_Claim/1) %>%
  ggplot(aes(x=Group, y=sev)) + 
  geom_point(aes(size=expo), color='black') + scale_size_continuous(range = c(1,4)) +
  geom_smooth(aes(weight = expo),se=T, color='red', size=1) + 
  labs(x = 'Percent of driving',
       y = 'Claim severity') +
  guides(size = guide_legend(title = "Number of claims")) +
  #xlim(0,40000)+
  #ylim(-0.01, 0.06)+
  theme_bw()


var <- 'Pct.drive.rush.pm'

df2 <- train %>% mutate(VAR = get(var))
q99 <- quantile(df2$VAR, 0.99)
div <- q99/15
train %>%
  dplyr::mutate(Duration.y = Duration/365.25,
                VAR = ifelse(get(var) > q99, q99, get(var)),
                Group = ceiling(VAR/div) * div) %>%
  group_by(Group) %>% 
  summarize(M_Claim=mean(M_Claim),
            expo=sum(Duration.y)) %>% 
  mutate(sev = M_Claim/1) %>%
  ggplot(aes(x=Group, y=sev)) + 
  geom_point(aes(size=expo), color='black') + scale_size_continuous(range = c(1,4)) +
  geom_smooth(aes(weight = expo),se=T, color='red', size=1) + 
  labs(x = 'Percent of driving',
       y = 'Claims frequency') +
  guides(size = guide_legend(title = "Number of claims")) +
  #xlim(0,40000)+
  #ylim(-0.01, 0.06)+
  theme_bw()

### Correlation
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


trad.vars  <- c("Marital", "Insured.sex", "Credit.score", "Insured.age", "Territory",
                "Pct.drive.mon", "Pct.drive.tue", "Pct.drive.wed", "Pct.drive.thr", "Pct.drive.fri", "Pct.drive.sat", "Pct.drive.sun")

data = data.matrix(train2[, paste(trad.vars)])
correlation_matrix <- abs(cor(data))

corr1 <- correlation_matrix[6:12, 6:12]

ggplot(data = as.data.frame(as.table(corr1)), aes(x = Var1, y = Var2, fill = Freq)) +
  geom_tile() +
  scale_fill_gradientn(colors = colorRampPalette(c("gray", "blue", "red"))(50), name = "Correlation") +
  geom_text(aes(label = round(Freq, 2)), color = "white", size=4) +
  #scale_fill_continuous(na.value = 'white') +
  theme_minimal() +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_text(size=13, angle = 45, hjust = 1), 
        axis.text.y = element_text(size = 13),
        plot.title = element_text(size = 13),   # Adjust plot title font size
        legend.text = element_text(size = 13),
        legend.title = element_text(size=13))

corr1 <- correlation_matrix[1:5, 6:12]

ggplot(data = as.data.frame(as.table(corr1)), aes(x = Var1, y = Var2, fill = Freq)) +
  geom_tile() +
  scale_fill_gradientn(colors = colorRampPalette(c("gray", "blue", "red"))(50), name = "Correlation") +
  geom_text(aes(label = round(Freq, 2)), color = "white", size=6) +
  #scale_fill_continuous(na.value = 'white') +
  theme_minimal() +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_text(size=13, angle = 45, hjust = 1), 
        axis.text.y = element_text(size = 13),
        plot.title = element_text(size = 13),   # Adjust plot title font size
        legend.text = element_text(size = 13),
        legend.title = element_text(size=13))


trad.vars  <- c("Marital", "Insured.sex", "Credit.score", "Insured.age", "Territory",
                "max.day", "min.day", "Dayformax", "Dayformin", "Pct.drive.wkend", 
                "Pct.drive.2hrs", "Pct.drive.3hrs", "Pct.drive.4hrs",
                "Pct.drive.rush.am", "Pct.drive.rush.pm")

data = data.matrix(train2[, paste(trad.vars)])
correlation_matrix <- abs(cor(data))

corr1 <- correlation_matrix[6:15, 6:15]

ggplot(data = as.data.frame(as.table(corr1)), aes(x = Var1, y = Var2, fill = Freq)) +
  geom_tile() +
  scale_fill_gradientn(colors = colorRampPalette(c("gray", "blue", "red"))(50), name = "Correlation") +
  geom_text(aes(label = round(Freq, 2)), color = "white", size=4) +
  #scale_fill_continuous(na.value = 'white') +
  theme_minimal() +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_text(size=13, angle = 45, hjust = 1), 
        axis.text.y = element_text(size = 13),
        plot.title = element_text(size = 13),   # Adjust plot title font size
        legend.text = element_text(size = 13),
        legend.title = element_text(size=13))

corr1 <- correlation_matrix[1:5, 6:15]

ggplot(data = as.data.frame(as.table(corr1)), aes(x = Var1, y = Var2, fill = Freq)) +
  geom_tile() +
  scale_fill_gradientn(colors = colorRampPalette(c("gray", "blue", "red"))(50), name = "Correlation") +
  geom_text(aes(label = round(Freq, 2)), color = "white", size=6) +
  #scale_fill_continuous(na.value = 'white') +
  theme_minimal() +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_text(size=13, angle = 45, hjust = 1), 
        axis.text.y = element_text(size = 13),
        plot.title = element_text(size = 13),   # Adjust plot title font size
        legend.text = element_text(size = 13),
        legend.title = element_text(size=13))



## OTHER COVARIATES--> Driving behavior
### Brakes
var <- 'Brake.06miles'
df2 <- train %>% mutate(VAR = get(var))
q99 <- quantile(df2$VAR, 0.99)
div <- q99/15
train %>%
  dplyr::mutate(Duration.y = Duration/365.25,
                VAR = ifelse(get(var) > q99, q99, get(var)),
                Group = ceiling(VAR/div) * div) %>%
  group_by(Group) %>% 
  summarize(M_Claim=mean(M_Claim),
            expo=sum(Duration.y)) %>% 
  mutate(sev = M_Claim/1) %>%
  ggplot(aes(x=Group, y=sev)) + 
  geom_point(aes(size=expo), color='black') + scale_size_continuous(range = c(1,4)) +
  geom_smooth(aes(weight = expo),se=T, color='red', size=1) + 
  labs(x = 'Average number of brakes per day',
       y = 'Claim severity') +
  guides(size = guide_legend(title = "Number of claims")) +
  #xlim(0,40000)+
  #ylim(-0.01, 0.06)+
  theme_bw()

var <- 'Brake.08miles'
df2 <- train %>% mutate(VAR = get(var))
q99 <- quantile(df2$VAR, 0.99)
div <- q99/15
train %>%
  dplyr::mutate(Duration.y = Duration/365.25,
                VAR = ifelse(get(var) > q99, q99, get(var)),
                Group = ceiling(VAR/div) * div) %>%
  group_by(Group) %>% 
  summarize(M_Claim=mean(M_Claim),
            expo=sum(Duration.y)) %>% 
  mutate(sev = M_Claim/1) %>%
  ggplot(aes(x=Group, y=sev)) + 
  geom_point(aes(size=expo), color='black') + scale_size_continuous(range = c(1,4)) +
  geom_smooth(aes(weight = expo),se=T, color='red', size=1) + 
  labs(x = 'Average number of brakes per day',
       y = 'Claim severity') +
  guides(size = guide_legend(title = "Number of claims")) +
  #xlim(0,40000)+
  #ylim(-0.01, 0.06)+
  theme_bw()

var <- 'Brake.09miles'
df2 <- train %>% mutate(VAR = get(var))
q99 <- quantile(df2$VAR, 0.99)
div <- q99/15
train %>%
  dplyr::mutate(Duration.y = Duration/365.25,
                VAR = ifelse(get(var) > q99, q99, get(var)),
                Group = ceiling(VAR/div) * div) %>%
  group_by(Group) %>% 
  summarize(M_Claim=mean(M_Claim),
            expo=sum(Duration.y)) %>% 
  mutate(sev = M_Claim/1) %>%
  ggplot(aes(x=Group, y=sev)) + 
  geom_point(aes(size=expo), color='black') + scale_size_continuous(range = c(1,4)) +
  geom_smooth(aes(weight = expo),se=T, color='red', size=1) + 
  labs(x = 'Average number of brakes per day',
       y = 'Claim severity') +
  guides(size = guide_legend(title = "Number of claims")) +
  #xlim(0,40000)+
  #ylim(-0.01, 0.06)+
  theme_bw()


var <- 'Brake.11miles'
df2 <- train %>% mutate(VAR = get(var))
q99 <- quantile(df2$VAR, 0.99)
div <- q99/15
train %>%
  dplyr::mutate(Duration.y = Duration/365.25,
                VAR = ifelse(get(var) > q99, q99, get(var)),
                Group = ceiling(VAR/div) * div) %>%
  group_by(Group) %>% 
  summarize(M_Claim=mean(M_Claim),
            expo=sum(Duration.y)) %>% 
  mutate(sev = M_Claim/1) %>%
  ggplot(aes(x=Group, y=sev)) + 
  geom_point(aes(size=expo), color='black') + scale_size_continuous(range = c(1,4)) +
  geom_smooth(aes(weight = expo),se=T, color='red', size=1) + 
  labs(x = 'Average number of brakes per day',
       y = 'Claim severity') +
  guides(size = guide_legend(title = "Number of claims")) +
  #xlim(0,40000)+
  #ylim(-0.01, 0.06)+
  theme_bw()

var <- 'Brake.12miles'
df2 <- train %>% mutate(VAR = get(var))
q99 <- quantile(df2$VAR, 0.99)
div <- q99/15
train %>%
  dplyr::mutate(Duration.y = Duration/365.25,
                VAR = ifelse(get(var) > q99, q99, get(var)),
                Group = ceiling(VAR/div) * div) %>%
  group_by(Group) %>% 
  summarize(M_Claim=mean(M_Claim),
            expo=sum(Duration.y)) %>% 
  mutate(sev = M_Claim/1) %>%
  ggplot(aes(x=Group, y=sev)) + 
  geom_point(aes(size=expo), color='black') + scale_size_continuous(range = c(1,4)) +
  geom_smooth(aes(weight = expo),se=T, color='red', size=1) + 
  labs(x = 'Average number of brakes per day',
       y = 'Claim severity') +
  guides(size = guide_legend(title = "Number of claims")) +
  #xlim(0,40000)+
  #ylim(-0.01, 0.06)+
  theme_bw()

var <- 'Brake.14miles'
df2 <- train %>% mutate(VAR = get(var))
q99 <- quantile(df2$VAR, 0.99)
div <- q99/15
train %>%
  dplyr::mutate(Duration.y = Duration/365.25,
                VAR = ifelse(get(var) > q99, q99, get(var)),
                Group = ceiling(VAR/div) * div) %>%
  group_by(Group) %>% 
  summarize(M_Claim=mean(M_Claim),
            expo=sum(Duration.y)) %>% 
  mutate(sev = M_Claim/1) %>%
  ggplot(aes(x=Group, y=sev)) + 
  geom_point(aes(size=expo), color='black') + scale_size_continuous(range = c(1,4)) +
  geom_smooth(aes(weight = expo),se=T, color='red', size=1) + 
  labs(x = 'Average number of brakes per day',
       y = 'Claim severity') +
  guides(size = guide_legend(title = "Number of claims")) +
  #xlim(0,40000)+
  #ylim(-0.01, 0.06)+
  theme_bw()

### Accelerations
var <- 'Accel.06miles'
df2 <- train %>% mutate(VAR = get(var))
q99 <- quantile(df2$VAR, 0.99)
div <- q99/15
train %>%
  dplyr::mutate(Duration.y = Duration/365.25,
                VAR = ifelse(get(var) > q99, q99, get(var)),
                Group = ceiling(VAR/div) * div) %>%
  group_by(Group) %>% 
  summarize(M_Claim=mean(M_Claim),
            expo=sum(Duration.y)) %>% 
  mutate(sev = M_Claim/1) %>%
  ggplot(aes(x=Group, y=sev)) + 
  geom_point(aes(size=expo), color='black') + scale_size_continuous(range = c(1,4)) +
  geom_smooth(aes(weight = expo),se=T, color='red', size=1) + 
  labs(x = 'Average number of accelerations per day',
       y = 'Claim severity') +
  guides(size = guide_legend(title = "Number of claims")) +
  #xlim(0,40000)+
  #ylim(-0.01, 0.06)+
  theme_bw()

var <- 'Accel.08miles'
df2 <- train %>% mutate(VAR = get(var))
q99 <- quantile(df2$VAR, 0.99)
div <- q99/15
train %>%
  dplyr::mutate(Duration.y = Duration/365.25,
                VAR = ifelse(get(var) > q99, q99, get(var)),
                Group = ceiling(VAR/div) * div) %>%
  group_by(Group) %>% 
  summarize(M_Claim=mean(M_Claim),
            expo=sum(Duration.y)) %>% 
  mutate(sev = M_Claim/1) %>%
  ggplot(aes(x=Group, y=sev)) + 
  geom_point(aes(size=expo), color='black') + scale_size_continuous(range = c(1,4)) +
  geom_smooth(aes(weight = expo),se=T, color='red', size=1) + 
  labs(x = 'Average number of accelerations per day',
       y = 'Claim severity') +
  guides(size = guide_legend(title = "Number of claims")) +
  #xlim(0,40000)+
  #ylim(-0.01, 0.06)+
  theme_bw()

var <- 'Accel.09miles'
df2 <- train %>% mutate(VAR = get(var))
q99 <- quantile(df2$VAR, 0.99)
div <- q99/15
train %>%
  dplyr::mutate(Duration.y = Duration/365.25,
                VAR = ifelse(get(var) > q99, q99, get(var)),
                Group = ceiling(VAR/div) * div) %>%
  group_by(Group) %>% 
  summarize(M_Claim=mean(M_Claim),
            expo=sum(Duration.y)) %>% 
  mutate(sev = M_Claim/1) %>%
  ggplot(aes(x=Group, y=sev)) + 
  geom_point(aes(size=expo), color='black') + scale_size_continuous(range = c(1,4)) +
  geom_smooth(aes(weight = expo),se=T, color='red', size=1) + 
  labs(x = 'Average number of accelerations per day',
       y = 'Claim severity') +
  guides(size = guide_legend(title = "Number of claims")) +
  #xlim(0,40000)+
  #ylim(-0.01, 0.06)+
  theme_bw()

var <- 'Accel.11miles'
df2 <- train %>% mutate(VAR = get(var))
q99 <- quantile(df2$VAR, 0.99)
div <- q99/15
train %>%
  dplyr::mutate(Duration.y = Duration/365.25,
                VAR = ifelse(get(var) > q99, q99, get(var)),
                Group = ceiling(VAR/div) * div) %>%
  group_by(Group) %>% 
  summarize(M_Claim=mean(M_Claim),
            expo=sum(Duration.y)) %>% 
  mutate(sev = M_Claim/1) %>%
  ggplot(aes(x=Group, y=sev)) + 
  geom_point(aes(size=expo), color='black') + scale_size_continuous(range = c(1,4)) +
  geom_smooth(aes(weight = expo),se=T, color='red', size=1) + 
  labs(x = 'Average number of accelerations per day',
       y = 'Claim severity') +
  guides(size = guide_legend(title = "Number of claims")) +
  #xlim(0,40000)+
  #ylim(-0.01, 0.06)+
  theme_bw()

var <- 'Accel.12miles'
df2 <- train %>% mutate(VAR = get(var))
q99 <- quantile(df2$VAR, 0.99)
div <- q99/15
train %>%
  dplyr::mutate(Duration.y = Duration/365.25,
                VAR = ifelse(get(var) > q99, q99, get(var)),
                Group = ceiling(VAR/div) * div) %>%
  group_by(Group) %>% 
  summarize(M_Claim=mean(M_Claim),
            expo=sum(Duration.y)) %>% 
  mutate(sev = M_Claim/1) %>%
  ggplot(aes(x=Group, y=sev)) + 
  geom_point(aes(size=expo), color='black') + scale_size_continuous(range = c(1,4)) +
  geom_smooth(aes(weight = expo),se=T, color='red', size=1) + 
  labs(x = 'Average number of accelerations per day',
       y = 'Claim severity') +
  guides(size = guide_legend(title = "Number of claims")) +
  #xlim(0,40000)+
  #ylim(-0.01, 0.06)+
  theme_bw()

var <- 'Accel.14miles'
df2 <- train %>% mutate(VAR = get(var))
q99 <- quantile(df2$VAR, 0.99)
div <- q99/15
train %>%
  dplyr::mutate(Duration.y = Duration/365.25,
                VAR = ifelse(get(var) > q99, q99, get(var)),
                Group = ceiling(VAR/div) * div) %>%
  group_by(Group) %>% 
  summarize(M_Claim=mean(M_Claim),
            expo=sum(Duration.y)) %>% 
  mutate(sev = M_Claim/1) %>%
  ggplot(aes(x=Group, y=sev)) + 
  geom_point(aes(size=expo), color='black') + scale_size_continuous(range = c(1,4)) +
  geom_smooth(aes(weight = expo),se=T, color='red', size=1) + 
  labs(x = 'Average number of accelerations per day',
       y = 'Claim severity') +
  guides(size = guide_legend(title = "Number of claims")) +
  #xlim(0,40000)+
  #ylim(-0.01, 0.06)+
  theme_bw()

### Right turns
var <- 'Right.turn.intensity08'
df2 <- train %>% mutate(VAR = get(var))
q99 <- quantile(df2$VAR, 0.99)
div <- q99/15
train %>%
  dplyr::mutate(Duration.y = Duration/365.25,
                VAR = ifelse(get(var) > q99, q99, get(var)),
                Group = ceiling(VAR/div) * div) %>%
  group_by(Group) %>% 
  summarize(M_Claim=mean(M_Claim),
            expo=sum(Duration.y)) %>% 
  mutate(sev = M_Claim/1) %>%
  ggplot(aes(x=Group, y=sev)) + 
  geom_point(aes(size=expo), color='black') + scale_size_continuous(range = c(1,4)) +
  geom_smooth(aes(weight = expo),se=T, color='red', size=1) + 
  labs(x = 'Average number of right turns per day',
       y = 'Claim severity') +
  guides(size = guide_legend(title = "Number of claims")) +
  #xlim(0,40000)+
  #ylim(-0.01, 0.06)+
  theme_bw()

var <- 'Right.turn.intensity09'
df2 <- train %>% mutate(VAR = get(var))
q99 <- quantile(df2$VAR, 0.99)
div <- q99/15
train %>%
  dplyr::mutate(Duration.y = Duration/365.25,
                VAR = ifelse(get(var) > q99, q99, get(var)),
                Group = ceiling(VAR/div) * div) %>%
  group_by(Group) %>% 
  summarize(M_Claim=mean(M_Claim),
            expo=sum(Duration.y)) %>% 
  mutate(sev = M_Claim/1) %>%
  ggplot(aes(x=Group, y=sev)) + 
  geom_point(aes(size=expo), color='black') + scale_size_continuous(range = c(1,4)) +
  geom_smooth(aes(weight = expo),se=T, color='red', size=1) + 
  labs(x = 'Average number of right turns per day',
       y = 'Claim severity') +
  guides(size = guide_legend(title = "Number of claims")) +
  #xlim(0,40000)+
  #ylim(-0.01, 0.06)+
  theme_bw()

var <- 'Right.turn.intensity10'
df2 <- train %>% mutate(VAR = get(var))
q99 <- quantile(df2$VAR, 0.99)
div <- q99/15
train %>%
  dplyr::mutate(Duration.y = Duration/365.25,
                VAR = ifelse(get(var) > q99, q99, get(var)),
                Group = ceiling(VAR/div) * div) %>%
  group_by(Group) %>% 
  summarize(M_Claim=mean(M_Claim),
            expo=sum(Duration.y)) %>% 
  mutate(sev = M_Claim/1) %>%
  ggplot(aes(x=Group, y=sev)) + 
  geom_point(aes(size=expo), color='black') + scale_size_continuous(range = c(1,4)) +
  geom_smooth(aes(weight = expo),se=T, color='red', size=1) + 
  labs(x = 'Average number of right turns per day',
       y = 'Claims frequency') +
  guides(size = guide_legend(title = "Number of claims")) +
  #xlim(0,40000)+
  #ylim(-0.01, 0.06)+
  theme_bw()

var <- 'Right.turn.intensity11'
df2 <- train %>% mutate(VAR = get(var))
q99 <- quantile(df2$VAR, 0.99)
div <- q99/15
train %>%
  dplyr::mutate(Duration.y = Duration/365.25,
                VAR = ifelse(get(var) > q99, q99, get(var)),
                Group = ceiling(VAR/div) * div) %>%
  group_by(Group) %>% 
  summarize(M_Claim=mean(M_Claim),
            expo=sum(Duration.y)) %>% 
  mutate(sev = M_Claim/1) %>%
  ggplot(aes(x=Group, y=sev)) + 
  geom_point(aes(size=expo), color='black') + scale_size_continuous(range = c(1,4)) +
  geom_smooth(aes(weight = expo),se=T, color='red', size=1) + 
  labs(x = 'Average number of right turns per day',
       y = 'Claim severity') +
  guides(size = guide_legend(title = "Number of claims")) +
  #xlim(0,40000)+
  #ylim(-0.01, 0.06)+
  theme_bw()

var <- 'Right.turn.intensity12'
df2 <- train %>% mutate(VAR = get(var))
q99 <- quantile(df2$VAR, 0.99)
div <- q99/15
train %>%
  dplyr::mutate(Duration.y = Duration/365.25,
                VAR = ifelse(get(var) > q99, q99, get(var)),
                Group = ceiling(VAR/div) * div) %>%
  group_by(Group) %>% 
  summarize(M_Claim=mean(M_Claim),
            expo=sum(Duration.y)) %>% 
  mutate(sev = M_Claim/1) %>%
  ggplot(aes(x=Group, y=sev)) + 
  geom_point(aes(size=expo), color='black') + scale_size_continuous(range = c(1,4)) +
  geom_smooth(aes(weight = expo),se=T, color='red', size=1) + 
  labs(x = 'Average number of right turns per day',
       y = 'Claim severity') +
  guides(size = guide_legend(title = "Number of claims")) +
  #xlim(0,40000)+
  #ylim(-0.01, 0.06)+
  theme_bw()

### Left turns
var <- 'Left.turn.intensity08'
df2 <- train %>% mutate(VAR = get(var))
q99 <- quantile(df2$VAR, 0.99)
div <- q99/15
train %>%
  dplyr::mutate(Duration.y = Duration/365.25,
                VAR = ifelse(get(var) > q99, q99, get(var)),
                Group = ceiling(VAR/div) * div) %>%
  group_by(Group) %>% 
  summarize(M_Claim=mean(M_Claim),
            expo=sum(Duration.y)) %>% 
  mutate(sev = M_Claim/1) %>%
  ggplot(aes(x=Group, y=sev)) + 
  geom_point(aes(size=expo), color='black') + scale_size_continuous(range = c(1,4)) +
  geom_smooth(aes(weight = expo),se=T, color='red', size=1) + 
  labs(x = 'Average number of left turns per day',
       y = 'Claim severity') +
  guides(size = guide_legend(title = "Number of claims")) +
  #xlim(0,40000)+
  #ylim(-0.01, 0.06)+
  theme_bw()


var <- 'Left.turn.intensity09'
df2 <- train %>% mutate(VAR = get(var))
q99 <- quantile(df2$VAR, 0.99)
div <- q99/15
train %>%
  dplyr::mutate(Duration.y = Duration/365.25,
                VAR = ifelse(get(var) > q99, q99, get(var)),
                Group = ceiling(VAR/div) * div) %>%
  group_by(Group) %>% 
  summarize(M_Claim=mean(M_Claim),
            expo=sum(Duration.y)) %>% 
  mutate(sev = M_Claim/1) %>%
  ggplot(aes(x=Group, y=sev)) + 
  geom_point(aes(size=expo), color='black') + scale_size_continuous(range = c(1,4)) +
  geom_smooth(aes(weight = expo),se=T, color='red', size=1) + 
  labs(x = 'Average number of left turns per day',
       y = 'Claim severity') +
  guides(size = guide_legend(title = "Number of claims")) +
  #xlim(0,40000)+
  #ylim(-0.01, 0.06)+
  theme_bw()


var <- 'Left.turn.intensity10'
df2 <- train %>% mutate(VAR = get(var))
q99 <- quantile(df2$VAR, 0.99)
div <- q99/15
train %>%
  dplyr::mutate(Duration.y = Duration/365.25,
                VAR = ifelse(get(var) > q99, q99, get(var)),
                Group = ceiling(VAR/div) * div) %>%
  group_by(Group) %>% 
  summarize(M_Claim=mean(M_Claim),
            expo=sum(Duration.y)) %>% 
  mutate(sev = M_Claim/1) %>%
  ggplot(aes(x=Group, y=sev)) + 
  geom_point(aes(size=expo), color='black') + scale_size_continuous(range = c(1,4)) +
  geom_smooth(aes(weight = expo),se=T, color='red', size=1) + 
  labs(x = 'Average number of left turns per day',
       y = 'Claim severity') +
  guides(size = guide_legend(title = "Number of claims")) +
  #xlim(0,40000)+
  #ylim(-0.01, 0.06)+
  theme_bw()


var <- 'Left.turn.intensity11'
df2 <- train %>% mutate(VAR = get(var))
q99 <- quantile(df2$VAR, 0.99)
div <- q99/15
train %>%
  dplyr::mutate(Duration.y = Duration/365.25,
                VAR = ifelse(get(var) > q99, q99, get(var)),
                Group = ceiling(VAR/div) * div) %>%
  group_by(Group) %>% 
  summarize(M_Claim=mean(M_Claim),
            expo=sum(Duration.y)) %>% 
  mutate(sev = M_Claim/1) %>%
  ggplot(aes(x=Group, y=sev)) + 
  geom_point(aes(size=expo), color='black') + scale_size_continuous(range = c(1,4)) +
  geom_smooth(aes(weight = expo),se=T, color='red', size=1) + 
  labs(x = 'Average number of left turns per day',
       y = 'Claim severity') +
  guides(size = guide_legend(title = "Number of claims")) +
  #xlim(0,40000)+
  #ylim(-0.01, 0.06)+
  theme_bw()


var <- 'Left.turn.intensity12'
df2 <- train %>% mutate(VAR = get(var))
q99 <- quantile(df2$VAR, 0.99)
div <- q99/15
train %>%
  dplyr::mutate(Duration.y = Duration/365.25,
                VAR = ifelse(get(var) > q99, q99, get(var)),
                Group = ceiling(VAR/div) * div) %>%
  group_by(Group) %>% 
  summarize(M_Claim=mean(M_Claim),
            expo=sum(Duration.y)) %>% 
  mutate(sev = M_Claim/1) %>%
  ggplot(aes(x=Group, y=sev)) + 
  geom_point(aes(size=expo), color='black') + scale_size_continuous(range = c(1,4)) +
  geom_smooth(aes(weight = expo),se=T, color='red', size=1) + 
  labs(x = 'Average number of left turns per day',
       y = 'Claim severity') +
  guides(size = guide_legend(title = "Number of claims")) +
  #xlim(0,40000)+
  #ylim(-0.01, 0.06)+
  theme_bw()

### Correlation
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


trad.vars  <- c("Marital", "Insured.sex", "Credit.score", "Insured.age", "Territory",
                "Accel.06miles", "Accel.08miles", "Accel.09miles", "Accel.11miles", "Accel.12miles", "Accel.14miles", 
                "Brake.06miles", "Brake.08miles", "Brake.09miles", "Brake.11miles", "Brake.12miles", "Brake.14miles", 
                "Left.turn.intensity08", "Left.turn.intensity09", "Left.turn.intensity10", "Left.turn.intensity11", "Left.turn.intensity12",
                "Right.turn.intensity08", "Right.turn.intensity09", "Right.turn.intensity10", "Right.turn.intensity11", "Right.turn.intensity12")

data = data.matrix(train2[, paste(trad.vars)])
correlation_matrix <- abs(cor(data))

corr1 <- correlation_matrix[6:17, 6:17]

ggplot(data = as.data.frame(as.table(corr1)), aes(x = Var1, y = Var2, fill = Freq)) +
  geom_tile() +
  scale_fill_gradientn(colors = colorRampPalette(c("gray", "blue", "red"))(50), name = "Correlation") +
  geom_text(aes(label = round(Freq, 2)), color = "white", size=4) +
  #scale_fill_continuous(na.value = 'white') +
  theme_minimal() +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_text(size=13, angle = 45, hjust = 1), 
        axis.text.y = element_text(size = 13),
        plot.title = element_text(size = 13),   # Adjust plot title font size
        legend.text = element_text(size = 13),
        legend.title = element_text(size=13))

corr1 <- correlation_matrix[1:5, 6:17]

ggplot(data = as.data.frame(as.table(corr1)), aes(x = Var1, y = Var2, fill = Freq)) +
  geom_tile() +
  scale_fill_gradientn(colors = colorRampPalette(c("gray", "blue", "red"))(50), name = "Correlation") +
  geom_text(aes(label = round(Freq, 2)), color = "white", size=6) +
  #scale_fill_continuous(na.value = 'white') +
  theme_minimal() +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_text(size=13, angle = 45, hjust = 1), 
        axis.text.y = element_text(size = 13),
        plot.title = element_text(size = 13),   # Adjust plot title font size
        legend.text = element_text(size = 13),
        legend.title = element_text(size=13))


corr1 <- correlation_matrix[18:27, 18:27]

ggplot(data = as.data.frame(as.table(corr1)), aes(x = Var1, y = Var2, fill = Freq)) +
  geom_tile() +
  scale_fill_gradientn(colors = colorRampPalette(c("gray", "blue", "red"))(50), name = "Correlation") +
  geom_text(aes(label = round(Freq, 2)), color = "white", size=4) +
  #scale_fill_continuous(na.value = 'white') +
  theme_minimal() +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_text(size=13, angle = 45, hjust = 1), 
        axis.text.y = element_text(size = 13),
        plot.title = element_text(size = 13),   # Adjust plot title font size
        legend.text = element_text(size = 13),
        legend.title = element_text(size=13))

corr1 <- correlation_matrix[1:5, 18:27]

ggplot(data = as.data.frame(as.table(corr1)), aes(x = Var1, y = Var2, fill = Freq)) +
  geom_tile() +
  scale_fill_gradientn(colors = colorRampPalette(c("gray", "blue", "red"))(50), name = "Correlation") +
  geom_text(aes(label = round(Freq, 2)), color = "white", size=6) +
  #scale_fill_continuous(na.value = 'white') +
  theme_minimal() +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_text(size=13, angle = 45, hjust = 1), 
        axis.text.y = element_text(size = 13),
        plot.title = element_text(size = 13),   # Adjust plot title font size
        legend.text = element_text(size = 13),
        legend.title = element_text(size=13))

