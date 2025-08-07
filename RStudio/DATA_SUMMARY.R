
################ TELEMATIC USAGE-BASED AUTO INSURANCE ################### 
#########################################################################

# renv::deactivate()

# Packages

library(tidyverse)
library(vtable)
library(rpart)
library(repr)
library(rpart.plot)
# library(rfCountData)
library(gam)


dataS <- read.csv('Synthetic_data.csv')

#View(dataS)

# Modifications 
dataS <- dataS %>%
  mutate(Territory = as.factor(Territory)) %>%
  select(-c('Annual.pct.driven', 'Annual.miles.drive'))

data.select <- dataS

# Train-test et folds
set.seed(123)
train <- data.select %>% sample_frac(0.8, replace = FALSE)
test <- data.select %>% anti_join(train)


# TRADITIONAL COVARIATES

# Average number of claims vs. contract duration

train %>%
dplyr::mutate(Duration.y = Duration/366, 
              Duration.group = ceiling(Duration.y/0.05) * 0.05) %>%
  group_by(Duration.group) %>% 
  summarize(NB_Claim=sum(NB_Claim),
            nb=n()) %>% 
  mutate(freq = NB_Claim/nb) %>%
  ggplot(aes(x=Duration.group, y=freq)) + 
  geom_point(aes(size=nb), color='black') + 
  #geom_smooth(aes(weight = nb),se=F) + 
  labs(x = 'Contract Duration',
       y = 'Average Nnumber of Claims') +
  geom_segment(aes(x = 0, y = 0, xend = 1, yend = max(freq)), linetype='dashed')+
  guides(size = guide_legend(title = "Number of contracts")) +
  xlim(0,1)+ylim(0, 0.06)+
  theme_bw()

# Observed claims frequency vs. credit score

train %>%
  dplyr::mutate(Duration.y = Duration/365.25, 
                Group = ceiling(Credit.score/25) * 25) %>%
  group_by(Group) %>% 
  summarize(NB_Claim=sum(NB_Claim),
            expo=sum(Duration.y)) %>% 
  mutate(freq = NB_Claim/expo) %>%
  ggplot(aes(x=Group, y=freq)) + 
  geom_point(aes(size=expo), color='black') + 
  geom_smooth(aes(weight = expo),se=T, color='red', size=1) + 
  labs(x = 'Credit Score',
       y = 'Claims frequency') +
  guides(size = guide_legend(title = "Total exposures")) +
  #  xlim(0,1)+ylim(0, 0.06)+
  theme_bw()


# Observed claims frequency vs. age of the insured

train %>%
  dplyr::mutate(Duration.y = Duration/365.25, 
                Group = ceiling(Insured.age/3) * 3) %>%
  group_by(Group) %>% 
  summarize(NB_Claim=sum(NB_Claim),
            expo=sum(Duration.y)) %>% 
  mutate(freq = NB_Claim/expo) %>%
  ggplot(aes(x=Group, y=freq)) + 
  geom_point(aes(size=expo), color='black') + scale_size_continuous(range = c(1,4)) + 
  geom_smooth(aes(weight = expo),se=T, color='red', size=1) + 
  labs(x = 'Age of the insured',
       y = 'Claims frequency') +
  guides(size = guide_legend(title = "Total exposures")) +
  #  xlim(0,1)+ylim(0, 0.06)+
  theme_bw()


# Observed claims frequency vs. territory

temp <- train %>%
  mutate(Var_ = Territory) %>%
  group_by(Var_) %>%
  summarize(nbclaim = sum(NB_Claim),
            expo = sum(Duration/365.25)) %>%
  mutate(freq = nbclaim/expo)

div <- min(temp$expo/temp$freq)/0.4

ggplot(data = temp, aes(x = Var_, y = expo, linetype = "Total Exposures")) + #start plot by by plotting bars
  geom_bar(stat = "identity", alpha=0.5) + 
  geom_point(data = temp, aes(x = Var_, y = (freq)*div, group = 1), color='red', size=1.5,
             inherit.aes = FALSE) +
  geom_line(data = temp, aes(x = Var_, y = (freq)*div, group = 1, linetype = "Claims Frequency"), color='red', size=0.7,
            inherit.aes = FALSE) +
  labs(x = 'Territory',
       y = 'Total exposures') +
  scale_x_discrete(labels = NULL, breaks = NULL)+
  scale_y_continuous(sec.axis = sec_axis(~./div, name = "Claims frequency")) +
  guides(linetype=guide_legend(title="")) +
  theme_classic()+
  theme(legend.position = 'bottom', legend.direction = "horizontal")



# Correlation between sensible covariates

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

# Other coviariates 

# Age of the car
train %>%
  dplyr::mutate(Duration.y = Duration/365.25, 
                Group = ceiling(Car.age/1) * 1) %>%
  group_by(Group) %>% 
  summarize(NB_Claim=sum(NB_Claim),
            expo=sum(Duration.y)) %>% 
  mutate(freq = NB_Claim/expo) %>%
  ggplot(aes(x=Group, y=freq)) + 
  geom_point(aes(size=expo), color='black') + scale_size_continuous(range = c(1,4)) +
  geom_smooth(aes(weight = expo),se=T, color='red', size=1) + 
  labs(x = 'Age of the car',
       y = 'Claims frequency') +
  guides(size = guide_legend(title = "Total exposures")) +
  #  xlim(0,1)+ylim(0, 0.06)+
  theme_bw()



# Use of the car
temp <- train %>%
  mutate(Var_ = Car.use) %>%
  group_by(Var_) %>%
  summarize(nbclaim = sum(NB_Claim),
            expo = sum(Duration/365.25)) %>%
  mutate(freq = nbclaim/expo)

div <- min(temp$expo/temp$freq)/0.07

ggplot(data = temp, aes(x = Var_, y = expo, linetype='Total Exposures')) + #start plot by by plotting bars
  geom_bar(stat = "identity", alpha=0.5) + 
  geom_point(data = temp, aes(x = Var_, y = (freq)*div, group = 1), color='red', size=3,
             inherit.aes = FALSE) +
  geom_line(data = temp, aes(x = Var_, y = (freq)*div, group = 1, linetype = 'Claims Frequency'), color='red', size=0.7,
            inherit.aes = FALSE) +
  labs(x = 'Use of the car',
       y = 'Total exposures') +
  #scale_x_discrete(labels = NULL, breaks = NULL)+
  scale_y_continuous(sec.axis = sec_axis(~./div, name = "Claims frequency"))  + 
  guides(linetype=guide_legend(title="")) +
  theme_classic()+
  theme(legend.position = 'bottom', legend.direction = "horizontal")

# Region

temp <- train %>%
  mutate(Var_ = Region) %>%
  group_by(Var_) %>%
  summarize(nbclaim = sum(NB_Claim),
            expo = sum(Duration/365.25)) %>%
  mutate(freq = nbclaim/expo)

div <- min(temp$expo/temp$freq)/0.5

ggplot(data = temp, aes(x = Var_, y = expo, linetype='Total Exposures')) + #start plot by by plotting bars
  geom_bar(stat = "identity", alpha=0.5) + 
  geom_point(data = temp, aes(x = Var_, y = (freq)*div, group = 1), color='red', size=3,
             inherit.aes = FALSE) +
  geom_line(data = temp, aes(x = Var_, y = (freq)*div, group = 1, linetype='Claims Frequency'), color='red', size=0.7,
            inherit.aes = FALSE) +
  labs(x = 'Region',
       y = 'Total exposures') +
  #scale_x_discrete(labels = NULL, breaks = NULL)+
  scale_y_continuous(sec.axis = sec_axis(~./div, name = "Claims frequency")) + 
  guides(linetype=guide_legend(title="")) +
  theme_classic()+
  theme(legend.position = 'bottom', legend.direction = "horizontal")


# years without cliams

train %>%
  dplyr::mutate(Duration.y = Duration/365.25, 
                Group = pmin(Years.noclaims, 60)) %>%
  group_by(Group) %>% 
  summarize(NB_Claim=sum(NB_Claim),
            expo=sum(Duration.y)) %>% 
  mutate(freq = NB_Claim/expo) %>%
  ggplot(aes(x=Group, y=freq)) + 
  geom_point(aes(size=expo), color='black') + scale_size_continuous(range = c(1,4)) +
  geom_smooth(aes(weight = expo),se=T, color='red', size=1) + 
  labs(x = 'Years without claim',
       y = 'Claims frequency') +
  guides(size = guide_legend(title = "Total exposures")) +
  #xlim(0,40000)+
  #ylim(0, 0.06)+
  theme_bw()

# Correlation matrix

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

# TELEMATICS COVARIATES


#Claims Frequency vs. Total miles driven

train %>%
  dplyr::mutate(Duration.y = Duration/365.25, 
                Group = ifelse(Total.miles.driven < 20000, ceiling(Total.miles.driven/1000) * 1000, ceiling(Total.miles.driven/5000) * 5000)) %>%
  group_by(Group) %>% 
  summarize(NB_Claim=sum(NB_Claim),
            expo=sum(Duration.y)) %>% 
  mutate(freq = NB_Claim/expo) %>%
  ggplot(aes(x=Group, y=freq)) + 
  geom_point(aes(size=expo), color='black') + scale_size_continuous(range = c(1,4)) +
  geom_smooth(aes(weight = expo),se=T, color='red', size=1) + 
  labs(x = 'Total miles driven',
       y = 'Claims frequency') +
  guides(size = guide_legend(title = "Total exposures")) +
  xlim(0,40000)+
  ylim(0, 0.34)+
  theme_bw()

# Contract duration 

# Claims Frequency vs. average miles driven per day


train %>%
  dplyr::mutate(Miles.per.day = Total.miles.driven/Duration, 
                Duration.y = Duration/365.25, 
                Group = ceiling(Miles.per.day/7.5) * 7.5) %>%
  group_by(Group) %>% 
  summarize(NB_Claim=sum(NB_Claim),
            expo=sum(Duration.y)) %>% 
  mutate(freq = NB_Claim/expo) %>%
  ggplot(aes(x=Group, y=freq)) + 
  geom_point(aes(size=expo), color='black') + scale_size_continuous(range = c(1,4)) +
  geom_smooth(aes(weight = expo),se=T, color='red', size=1) + 
  labs(x = 'Average miles driven per day',
       y = 'Claims frequency') +
  guides(size = guide_legend(title = "Total exposures")) +
  xlim(0,127.5)+
  #ylim(0, 0.31)+
  theme_bw()

# "Claims Frequency vs. average number of days per week the car is used"

train %>%
  dplyr::mutate(Duration.y = Duration/365.25, 
                Group = ceiling(Avgdays.week/0.5) * 0.5) %>%
  group_by(Group) %>% 
  summarize(NB_Claim=sum(NB_Claim),
            expo=sum(Duration.y)) %>% 
  mutate(freq = NB_Claim/expo) %>%
  ggplot(aes(x=Group, y=freq)) + 
  geom_point(aes(size=expo), color='black') + scale_size_continuous(range = c(1,4)) +
  geom_smooth(aes(weight = expo),se=T, color='red', size=1) + 
  labs(x = 'Average number of days per week',
       y = 'Claims frequency') +
  guides(size = guide_legend(title = "Total exposures")) +
  #xlim(0,40000)+
  #ylim(0, 0.31)+
  theme_bw()



# "Claims Frequency vs. percentage of use for each day"


div <- 1/15 

var <- 'Pct.drive.mon'
train %>%
  dplyr::mutate(Duration.y = Duration/365.25, 
                Group = ceiling(get(var)/div) * div) %>%
  group_by(Group) %>% 
  summarize(NB_Claim=sum(NB_Claim),
            expo=sum(Duration.y)) %>% 
  mutate(freq = NB_Claim/expo) %>%
  ggplot(aes(x=Group, y=freq)) + 
  geom_point(aes(size=expo), color='black') + scale_size_continuous(range = c(1,4)) +
  geom_smooth(aes(weight = expo),se=F, color='red', size=1) + 
  labs(x = 'Usage percentage per day',
       y = 'Claims frequency') +
  guides(size = guide_legend(title = "Total exposures")) +
  #xlim(0,40000)+
  ylim(-0.01, 0.06)+
  theme_bw()


var <- 'Pct.drive.tue'
train %>%
  dplyr::mutate(Duration.y = Duration/365.25, 
                Group = ceiling(get(var)/div) * div) %>%
  group_by(Group) %>% 
  summarize(NB_Claim=sum(NB_Claim),
            expo=sum(Duration.y)) %>% 
  mutate(freq = NB_Claim/expo) %>%
  ggplot(aes(x=Group, y=freq)) + 
  geom_point(aes(size=expo), color='black') + scale_size_continuous(range = c(1,4)) +
  geom_smooth(aes(weight = expo),se=F, color='red', size=1) + 
  labs(x = 'Usage percentage per day',
       y = 'Claims frequency') +
  guides(size = guide_legend(title = "Total exposures")) +
  #xlim(0,40000)+
  ylim(-0.01, 0.06)+
  theme_bw()


var <- 'Pct.drive.wed'
train %>%
  dplyr::mutate(Duration.y = Duration/365.25, 
                Group = ceiling(get(var)/div) * div) %>%
  group_by(Group) %>% 
  summarize(NB_Claim=sum(NB_Claim),
            expo=sum(Duration.y)) %>% 
  mutate(freq = NB_Claim/expo) %>%
  ggplot(aes(x=Group, y=freq)) + 
  geom_point(aes(size=expo), color='black') + scale_size_continuous(range = c(1,4)) +
  geom_smooth(aes(weight = expo),se=F, color='red', size=1) + 
  labs(x = 'Usage percentage per day',
       y = 'Claims frequency') +
  guides(size = guide_legend(title = "Total exposures")) +
  #xlim(0,40000)+
  ylim(-0.01, 0.06)+
  theme_bw()


var <- 'Pct.drive.thr'
train %>%
  dplyr::mutate(Duration.y = Duration/365.25, 
                Group = ceiling(get(var)/div) * div) %>%
  group_by(Group) %>% 
  summarize(NB_Claim=sum(NB_Claim),
            expo=sum(Duration.y)) %>% 
  mutate(freq = NB_Claim/expo) %>%
  ggplot(aes(x=Group, y=freq)) + 
  geom_point(aes(size=expo), color='black') + scale_size_continuous(range = c(1,4)) +
  geom_smooth(aes(weight = expo),se=F, color='red', size=1) + 
  labs(x = 'Usage percentage per day',
       y = 'Claims frequency') +
  guides(size = guide_legend(title = "Total exposures")) +
  #xlim(0,40000)+
  ylim(-0.01, 0.06)+
  theme_bw()


var <- 'Pct.drive.fri'
train %>%
  dplyr::mutate(Duration.y = Duration/365.25, 
                Group = ceiling(get(var)/div) * div) %>%
  group_by(Group) %>% 
  summarize(NB_Claim=sum(NB_Claim),
            expo=sum(Duration.y)) %>% 
  mutate(freq = NB_Claim/expo) %>%
  ggplot(aes(x=Group, y=freq)) + 
  geom_point(aes(size=expo), color='black') + scale_size_continuous(range = c(1,4)) +
  geom_smooth(aes(weight = expo),se=F, color='red', size=1) + 
  labs(x = 'Usage percentage per day',
       y = 'Claims frequency') +
  guides(size = guide_legend(title = "Total exposures")) +
  #xlim(0,40000)+
  ylim(-0.01, 0.06)+
  theme_bw()


var <- 'Pct.drive.sat'
train %>%
  dplyr::mutate(Duration.y = Duration/365.25, 
                Group = ceiling(get(var)/div) * div) %>%
  group_by(Group) %>% 
  summarize(NB_Claim=sum(NB_Claim),
            expo=sum(Duration.y)) %>% 
  mutate(freq = NB_Claim/expo) %>%
  ggplot(aes(x=Group, y=freq)) + 
  geom_point(aes(size=expo), color='black') + scale_size_continuous(range = c(1,4)) +
  geom_smooth(aes(weight = expo),se=F, color='red', size=1) + 
  labs(x = 'Usage percentage per day',
       y = 'Claims frequency') +
  guides(size = guide_legend(title = "Total exposures")) +
  #xlim(0,40000)+
  ylim(-0.01, 0.06)+
  theme_bw()


var <- 'Pct.drive.sun'
train %>%
  dplyr::mutate(Duration.y = Duration/365.25, 
                Group = ceiling(get(var)/div) * div) %>%
  group_by(Group) %>% 
  summarize(NB_Claim=sum(NB_Claim),
            expo=sum(Duration.y)) %>% 
  mutate(freq = NB_Claim/expo) %>%
  ggplot(aes(x=Group, y=freq)) + 
  geom_point(aes(size=expo), color='black') + scale_size_continuous(range = c(1,4)) +
  geom_smooth(aes(weight = expo),se=F, color='red', size=1) + 
  labs(x = 'Usage percentage per day',
       y = 'Claims frequency') +
  guides(size = guide_legend(title = "Total exposures")) +
  #xlim(0,40000)+
  ylim(-0.01, 0.06)+
  theme_bw()

# "Claims Frequency vs. percentage of use week day and week-end day"

var <- 'Pct.drive.wkday'
div <- 1/25
train %>%
  dplyr::mutate(Duration.y = Duration/365.25, 
                Group = ceiling(get(var)/div) * div) %>%
  group_by(Group) %>% 
  summarize(NB_Claim=sum(NB_Claim),
            expo=sum(Duration.y)) %>% 
  mutate(freq = NB_Claim/expo) %>%
  ggplot(aes(x=Group, y=freq)) + 
  geom_point(aes(size=expo), color='black') + scale_size_continuous(range = c(1,4)) +
  geom_smooth(aes(weight = expo),se=T, color='red', size=1) + 
  labs(x = 'Usage percentage per day',
       y = 'Claims frequency') +
  guides(size = guide_legend(title = "Total exposures")) +
  #xlim(0,40000)+
  #ylim(-0.01, 0.06)+
  theme_bw()

var <- 'Pct.drive.wkend'
train %>%
  dplyr::mutate(Duration.y = Duration/365.25, 
                Group = ceiling(get(var)/div) * div) %>%
  group_by(Group) %>% 
  summarize(NB_Claim=sum(NB_Claim),
            expo=sum(Duration.y)) %>% 
  mutate(freq = NB_Claim/expo) %>%
  ggplot(aes(x=Group, y=freq)) + 
  geom_point(aes(size=expo), color='black') + scale_size_continuous(range = c(1,4)) +
  geom_smooth(aes(weight = expo),se=T, color='red', size=1) + 
  labs(x = 'Usage percentage per day',
       y = 'Claims frequency') +
  guides(size = guide_legend(title = "Total exposures")) +
  #xlim(0,40000)+
  #ylim(-0.01, 0.06)+
  theme_bw()

#Claims Frequency vs. Percentage of vehicule driven during rush hours"

var <- 'Pct.drive.rush.am'

df2 <- train %>% mutate(VAR = get(var))
q99 <- quantile(df2$VAR, 0.99)
div <- q99/15
train %>%
  dplyr::mutate(Duration.y = Duration/365.25,
                VAR = ifelse(get(var) > q99, q99, get(var)),
                Group = ceiling(VAR/div) * div) %>%
  group_by(Group) %>% 
  summarize(NB_Claim=sum(NB_Claim),
            expo=sum(Duration.y)) %>% 
  mutate(freq = NB_Claim/expo) %>%
  ggplot(aes(x=Group, y=freq)) + 
  geom_point(aes(size=expo), color='black') + scale_size_continuous(range = c(1,4)) +
  geom_smooth(aes(weight = expo),se=T, color='red', size=1) + 
  labs(x = 'Percent of driving',
       y = 'Claims frequency') +
  guides(size = guide_legend(title = "Total exposures")) +
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
  summarize(NB_Claim=sum(NB_Claim),
            expo=sum(Duration.y)) %>% 
  mutate(freq = NB_Claim/expo) %>%
  ggplot(aes(x=Group, y=freq)) + 
  geom_point(aes(size=expo), color='black') + scale_size_continuous(range = c(1,4)) +
  geom_smooth(aes(weight = expo),se=T, color='red', size=1) + 
  labs(x = 'Percent of driving',
       y = 'Claims frequency') +
  guides(size = guide_legend(title = "Total exposures")) +
  #xlim(0,40000)+
  #ylim(-0.01, 0.06)+
  theme_bw()


## Correlation between covariates

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

# Driving behavior

# Brakes

# Claims Frequency vs. Average number of brakes

var <- 'Brake.06miles'
df2 <- train %>% mutate(VAR = get(var))
q99 <- quantile(df2$VAR, 0.99)
div <- q99/15
train %>%
  dplyr::mutate(Duration.y = Duration/365.25,
                VAR = ifelse(get(var) > q99, q99, get(var)),
                Group = ceiling(VAR/div) * div) %>%
  group_by(Group) %>% 
  summarize(NB_Claim=sum(NB_Claim),
            expo=sum(Duration.y)) %>% 
  mutate(freq = NB_Claim/expo) %>%
  ggplot(aes(x=Group, y=freq)) + 
  geom_point(aes(size=expo), color='black') + scale_size_continuous(range = c(1,4)) +
  geom_smooth(aes(weight = expo),se=T, color='red', size=1) + 
  labs(x = 'Average number of brakes per day',
       y = 'Claims frequency') +
  guides(size = guide_legend(title = "Total exposures")) +
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
  summarize(NB_Claim=sum(NB_Claim),
            expo=sum(Duration.y)) %>% 
  mutate(freq = NB_Claim/expo) %>%
  ggplot(aes(x=Group, y=freq)) + 
  geom_point(aes(size=expo), color='black') + scale_size_continuous(range = c(1,4)) +
  geom_smooth(aes(weight = expo),se=T, color='red', size=1) + 
  labs(x = 'Average number of brakes per day',
       y = 'Claims frequency') +
  guides(size = guide_legend(title = "Total exposures")) +
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
  summarize(NB_Claim=sum(NB_Claim),
            expo=sum(Duration.y)) %>% 
  mutate(freq = NB_Claim/expo) %>%
  ggplot(aes(x=Group, y=freq)) + 
  geom_point(aes(size=expo), color='black') + scale_size_continuous(range = c(1,4)) +
  geom_smooth(aes(weight = expo),se=T, color='red', size=1) + 
  labs(x = 'Average number of brakes per day',
       y = 'Claims frequency') +
  guides(size = guide_legend(title = "Total exposures")) +
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
  summarize(NB_Claim=sum(NB_Claim),
            expo=sum(Duration.y)) %>% 
  mutate(freq = NB_Claim/expo) %>%
  ggplot(aes(x=Group, y=freq)) + 
  geom_point(aes(size=expo), color='black') + scale_size_continuous(range = c(1,4)) +
  geom_smooth(aes(weight = expo),se=T, color='red', size=1) + 
  labs(x = 'Average number of brakes per day',
       y = 'Claims frequency') +
  guides(size = guide_legend(title = "Total exposures")) +
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
  summarize(NB_Claim=sum(NB_Claim),
            expo=sum(Duration.y)) %>% 
  mutate(freq = NB_Claim/expo) %>%
  ggplot(aes(x=Group, y=freq)) + 
  geom_point(aes(size=expo), color='black') + scale_size_continuous(range = c(1,4)) +
  geom_smooth(aes(weight = expo),se=T, color='red', size=1) + 
  labs(x = 'Average number of brakes per day',
       y = 'Claims frequency') +
  guides(size = guide_legend(title = "Total exposures")) +
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
  summarize(NB_Claim=sum(NB_Claim),
            expo=sum(Duration.y)) %>% 
  mutate(freq = NB_Claim/expo) %>%
  ggplot(aes(x=Group, y=freq)) + 
  geom_point(aes(size=expo), color='black') + scale_size_continuous(range = c(1,4)) +
  geom_smooth(aes(weight = expo),se=T, color='red', size=1) + 
  labs(x = 'Average number of brakes per day',
       y = 'Claims frequency') +
  guides(size = guide_legend(title = "Total exposures")) +
  #xlim(0,40000)+
  #ylim(-0.01, 0.06)+
  theme_bw()

# Accelleration

# Claims Frequency vs. Average number of accelerations

var <- 'Accel.06miles'
df2 <- train %>% mutate(VAR = get(var))
q99 <- quantile(df2$VAR, 0.99)
div <- q99/15
train %>%
  dplyr::mutate(Duration.y = Duration/365.25,
                VAR = ifelse(get(var) > q99, q99, get(var)),
                Group = ceiling(VAR/div) * div) %>%
  group_by(Group) %>% 
  summarize(NB_Claim=sum(NB_Claim),
            expo=sum(Duration.y)) %>% 
  mutate(freq = NB_Claim/expo) %>%
  ggplot(aes(x=Group, y=freq)) + 
  geom_point(aes(size=expo), color='black') + scale_size_continuous(range = c(1,4)) +
  geom_smooth(aes(weight = expo),se=T, color='red', size=1) + 
  labs(x = 'Average number of accelerations per day',
       y = 'Claims frequency') +
  guides(size = guide_legend(title = "Total exposures")) +
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
  summarize(NB_Claim=sum(NB_Claim),
            expo=sum(Duration.y)) %>% 
  mutate(freq = NB_Claim/expo) %>%
  ggplot(aes(x=Group, y=freq)) + 
  geom_point(aes(size=expo), color='black') + scale_size_continuous(range = c(1,4)) +
  geom_smooth(aes(weight = expo),se=T, color='red', size=1) + 
  labs(x = 'Average number of accelerations per day',
       y = 'Claims frequency') +
  guides(size = guide_legend(title = "Total exposures")) +
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
  summarize(NB_Claim=sum(NB_Claim),
            expo=sum(Duration.y)) %>% 
  mutate(freq = NB_Claim/expo) %>%
  ggplot(aes(x=Group, y=freq)) + 
  geom_point(aes(size=expo), color='black') + scale_size_continuous(range = c(1,4)) +
  geom_smooth(aes(weight = expo),se=T, color='red', size=1) + 
  labs(x = 'Average number of accelerations per day',
       y = 'Claims frequency') +
  guides(size = guide_legend(title = "Total exposures")) +
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
  summarize(NB_Claim=sum(NB_Claim),
            expo=sum(Duration.y)) %>% 
  mutate(freq = NB_Claim/expo) %>%
  ggplot(aes(x=Group, y=freq)) + 
  geom_point(aes(size=expo), color='black') + scale_size_continuous(range = c(1,4)) +
  geom_smooth(aes(weight = expo),se=T, color='red', size=1) + 
  labs(x = 'Average number of accelerations per day',
       y = 'Claims frequency') +
  guides(size = guide_legend(title = "Total exposures")) +
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
  summarize(NB_Claim=sum(NB_Claim),
            expo=sum(Duration.y)) %>% 
  mutate(freq = NB_Claim/expo) %>%
  ggplot(aes(x=Group, y=freq)) + 
  geom_point(aes(size=expo), color='black') + scale_size_continuous(range = c(1,4)) +
  geom_smooth(aes(weight = expo),se=T, color='red', size=1) + 
  labs(x = 'Average number of accelerations per day',
       y = 'Claims frequency') +
  guides(size = guide_legend(title = "Total exposures")) +
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
  summarize(NB_Claim=sum(NB_Claim),
            expo=sum(Duration.y)) %>% 
  mutate(freq = NB_Claim/expo) %>%
  ggplot(aes(x=Group, y=freq)) + 
  geom_point(aes(size=expo), color='black') + scale_size_continuous(range = c(1,4)) +
  geom_smooth(aes(weight = expo),se=T, color='red', size=1) + 
  labs(x = 'Average number of accelerations per day',
       y = 'Claims frequency') +
  guides(size = guide_legend(title = "Total exposures")) +
  #xlim(0,40000)+
  #ylim(-0.01, 0.06)+
  theme_bw()


#Claims Frequency vs. Average number of right turns

var <- 'Right.turn.intensity08'
df2 <- train %>% mutate(VAR = get(var))
q99 <- quantile(df2$VAR, 0.99)
div <- q99/15
train %>%
  dplyr::mutate(Duration.y = Duration/365.25,
                VAR = ifelse(get(var) > q99, q99, get(var)),
                Group = ceiling(VAR/div) * div) %>%
  group_by(Group) %>% 
  summarize(NB_Claim=sum(NB_Claim),
            expo=sum(Duration.y)) %>% 
  mutate(freq = NB_Claim/expo) %>%
  ggplot(aes(x=Group, y=freq)) + 
  geom_point(aes(size=expo), color='black') + scale_size_continuous(range = c(1,4)) +
  geom_smooth(aes(weight = expo),se=T, color='red', size=1) + 
  labs(x = 'Average number of right turns per day',
       y = 'Claims frequency') +
  guides(size = guide_legend(title = "Total exposures")) +
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
  summarize(NB_Claim=sum(NB_Claim),
            expo=sum(Duration.y)) %>% 
  mutate(freq = NB_Claim/expo) %>%
  ggplot(aes(x=Group, y=freq)) + 
  geom_point(aes(size=expo), color='black') + scale_size_continuous(range = c(1,4)) +
  geom_smooth(aes(weight = expo),se=T, color='red', size=1) + 
  labs(x = 'Average number of right turns per day',
       y = 'Claims frequency') +
  guides(size = guide_legend(title = "Total exposures")) +
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
  summarize(NB_Claim=sum(NB_Claim),
            expo=sum(Duration.y)) %>% 
  mutate(freq = NB_Claim/expo) %>%
  ggplot(aes(x=Group, y=freq)) + 
  geom_point(aes(size=expo), color='black') + scale_size_continuous(range = c(1,4)) +
  geom_smooth(aes(weight = expo),se=T, color='red', size=1) + 
  labs(x = 'Average number of right turns per day',
       y = 'Claims frequency') +
  guides(size = guide_legend(title = "Total exposures")) +
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
  summarize(NB_Claim=sum(NB_Claim),
            expo=sum(Duration.y)) %>% 
  mutate(freq = NB_Claim/expo) %>%
  ggplot(aes(x=Group, y=freq)) + 
  geom_point(aes(size=expo), color='black') + scale_size_continuous(range = c(1,4)) +
  geom_smooth(aes(weight = expo),se=T, color='red', size=1) + 
  labs(x = 'Average number of right turns per day',
       y = 'Claims frequency') +
  guides(size = guide_legend(title = "Total exposures")) +
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
  summarize(NB_Claim=sum(NB_Claim),
            expo=sum(Duration.y)) %>% 
  mutate(freq = NB_Claim/expo) %>%
  ggplot(aes(x=Group, y=freq)) + 
  geom_point(aes(size=expo), color='black') + scale_size_continuous(range = c(1,4)) +
  geom_smooth(aes(weight = expo),se=T, color='red', size=1) + 
  labs(x = 'Average number of right turns per day',
       y = 'Claims frequency') +
  guides(size = guide_legend(title = "Total exposures")) +
  #xlim(0,40000)+
  #ylim(-0.01, 0.06)+
  theme_bw()


# Claims Frequency vs. Average number of left turns

var <- 'Left.turn.intensity08'
df2 <- train %>% mutate(VAR = get(var))
q99 <- quantile(df2$VAR, 0.99)
div <- q99/15
train %>%
  dplyr::mutate(Duration.y = Duration/365.25,
                VAR = ifelse(get(var) > q99, q99, get(var)),
                Group = ceiling(VAR/div) * div) %>%
  group_by(Group) %>% 
  summarize(NB_Claim=sum(NB_Claim),
            expo=sum(Duration.y)) %>% 
  mutate(freq = NB_Claim/expo) %>%
  ggplot(aes(x=Group, y=freq)) + 
  geom_point(aes(size=expo), color='black') + scale_size_continuous(range = c(1,4)) +
  geom_smooth(aes(weight = expo),se=T, color='red', size=1) + 
  labs(x = 'Average number of left turns per day',
       y = 'Claims frequency') +
  guides(size = guide_legend(title = "Total exposures")) +
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
  summarize(NB_Claim=sum(NB_Claim),
            expo=sum(Duration.y)) %>% 
  mutate(freq = NB_Claim/expo) %>%
  ggplot(aes(x=Group, y=freq)) + 
  geom_point(aes(size=expo), color='black') + scale_size_continuous(range = c(1,4)) +
  geom_smooth(aes(weight = expo),se=T, color='red', size=1) + 
  labs(x = 'Average number of left turns per day',
       y = 'Claims frequency') +
  guides(size = guide_legend(title = "Total exposures")) +
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
  summarize(NB_Claim=sum(NB_Claim),
            expo=sum(Duration.y)) %>% 
  mutate(freq = NB_Claim/expo) %>%
  ggplot(aes(x=Group, y=freq)) + 
  geom_point(aes(size=expo), color='black') + scale_size_continuous(range = c(1,4)) +
  geom_smooth(aes(weight = expo),se=T, color='red', size=1) + 
  labs(x = 'Average number of left turns per day',
       y = 'Claims frequency') +
  guides(size = guide_legend(title = "Total exposures")) +
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
  summarize(NB_Claim=sum(NB_Claim),
            expo=sum(Duration.y)) %>% 
  mutate(freq = NB_Claim/expo) %>%
  ggplot(aes(x=Group, y=freq)) + 
  geom_point(aes(size=expo), color='black') + scale_size_continuous(range = c(1,4)) +
  geom_smooth(aes(weight = expo),se=T, color='red', size=1) + 
  labs(x = 'Average number of left turns per day',
       y = 'Claims frequency') +
  guides(size = guide_legend(title = "Total exposures")) +
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
  summarize(NB_Claim=sum(NB_Claim),
            expo=sum(Duration.y)) %>% 
  mutate(freq = NB_Claim/expo) %>%
  ggplot(aes(x=Group, y=freq)) + 
  geom_point(aes(size=expo), color='black') + scale_size_continuous(range = c(1,4)) +
  geom_smooth(aes(weight = expo),se=T, color='red', size=1) + 
  labs(x = 'Average number of left turns per day',
       y = 'Claims frequency') +
  guides(size = guide_legend(title = "Total exposures")) +
  #xlim(0,40000)+
  #ylim(-0.01, 0.06)+
  theme_bw()

# # Correlation

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


