### INTERPRETATIONS ARE GIVEN ALONG WITH THE CODE AS AND WHEN IT IS RUN

#Setting up directory
#setwd("C:/Users/SRIRAM/Desktop/VCU SCMA/RStudio/SCMA NSSO")
 #Importing file

nsso<-read.csv('NSSO68 data set.csv')

#Exploring
names(nsso)
head(nsso)
tail(nsso)
View(nsso)
dim(nsso)
summary(nsso)

#Subsetting the dataset for only himachal pradesh, it has the code HP under state_1
hp<-nsso[nsso$state_1=='HP',]

class(hp)
View(hp)

#Checking no. of districts and giving them names from wikipedia
unique(hp$District)

 #Checking for null values to clean
any(is.na(hp))

#There are null values
#so we need to replacce them with zeroes
hp[is.na(hp)]<-0
any(is.na(hp))

str(hp)

#creating a new column using mutate

library(dplyr)
hp<-hp%>%
  mutate(Distname=recode(District,"1"="Chamba",
                         "2"="Kangra",
                         "3"="Una",
                         "4"="Bilaspur",
                         "5"="Hamirpur",
                         "6"="Kullu",
                         "7"="Lahaul and Spiti",
                         "8"="Mandi",
                         "9"="Kinnaur",
                         "10"="Shimla",
                         "11"="Sirmaur",
                         "12"="Solan",
                         .default="yes"))

View(hp)
names(hp)
#next question asked is to perform descriptive analytics
hp2 = (hp[,c("Age","Sex","Marital_Status","Education","Distname","sugartotal_v","Milktotal_v","fishprawn_q","peas_q","apple_q","maida_v","besan_v","oilseeds_v")])

View(hp2)
names(hp2)

summary(hp2)

# Interpretations:
# Minimum age is 15 and max is 91, range is 76. Mean age is 49.6 with the median being 48.
# This means the most common age of people is 48.
# Mean sex is 1.19. As 2 is female and 1 is male, we can say there are more women than men in Himachal.
# The median education is 7. It is closer to the third quadrant.

#Another way of seeing descriptive stats

library(pastecs)
stat.desc(hp2)

library(psych)
describe(hp2)

#to find standard deviation of the relevant variables lying between columns 6 and 12
lapply(hp2[,6:13], sd)

#Lowest SD is in fishprawn, which means if people consume fishprawn, they consume of the same amount.
# From here, we can tell that most people have a uniform amount of peas and apples in Himachal pradesh.
# Apples and peas also have a relatively low standard deviation which means people consume evenly when compared to
# other resources like pulses Milktotal, besan and maida which have a higher standard deviation

# this can mean that some people do not consume fishprawn or oilseeds in himachal.

# outlier detection, we use boxplots

boxplot(hp2$cerealsub_q)
boxplot.stats(hp2$cerealsub_q)$out
#No outliers
boxplot(hp2$Age)
boxplot.stats(hp2$Age)$out
#1 outlier, being 91
boxplot(hp2$Education)
boxplot.stats(hp2$Education)$out
#64 outliers or value 13
boxplot(hp2$sugartotal_v)
boxplot.stats(hp2$sugartotal_v)$out
#123 outliers
boxplot(hp2$Milktotal_v)
boxplot.stats(hp2$Milktotal_v)$out
#97 outliers
boxplot(hp2$fishprawn_q)
boxplot.stats(hp2$fishprawn_q)$out
#9 outliers
boxplot(hp2$peas_q)
boxplot.stats(hp2$peas_q)$out
# 134 outliers
boxplot(hp2$apple_q)
boxplot.stats(hp2$apple_q)$out

boxplot(hp2$maida_v)
boxplot.stats(hp2$maida_v)$out

boxplot(hp2$besan_v)
boxplot.stats(hp2$besan_v)$out
boxplot(hp2$oilseeds_v)
boxplot.stats(hp2$oilseeds_v)$out
#only 4 outliers

#Visualisations using histogram

hist(hp2$sugartotal_v)
hist(hp2$Milktotal_v)
hist(hp2$fishprawn_q)
hist(hp2$peas_q)
hist(hp2$apple_q)
hist(hp2$maida_v)
hist(hp2$besan_v)
hist(hp2$oilseeds_v)



#Assignment 1Ab


#TO READ A CSV FILE INTO R


df.score = read.csv('IPL Ball-by-Ball 2008-2020.csv', header=TRUE)
names(df.score)
View(df.score)
dim(df.score)

library(dplyr)
runs  = df.score %>%
  group_by(batsman, id)%>%
  summarize(score = sum(batsman_runs))
View(runs)
unique(runs$batsman)
dim(runs)

runs$round = substr(runs$id, start = 1, stop = 3)
View(runs)
str(runs$round)
unique(runs$round)
#SUBSETTING

rahul = runs[runs$batsman=='KL Rahul',]
hist(rahul$score)

chopra = runs[runs$batsman=='A Chopra',]
hist(chopra$score)

mishra = runs[runs$batsman=='A Mishra',]
hist(mishra$score)

plot(rahul$score, type='l', col='blue')


rahul$time = 1:length(rahul$score)

#PROBABILITY AND FREQUENCY
hist(rahul$score,20)
plot(density(rahul$score))



# Bowlers

wickets  = df.score %>%
  group_by(bowler, id)%>%
  summarize(score = sum(is_wicket))
View(wickets)

calynn = wickets[wickets$bowler=='CA Lynn',]
hist(calynn$score)  

steyn = wickets[wickets$bowler=='DW Steyn',]
hist(steyn$score)

plot(calynn$score, type='l', col='blue')
plot(steyn$score, type='l', col='blue')

calynn$time = 1:length(calynn$score)
steyn$time = 1:length(steyn$score)


##continuous distribution for run
install.packages("fitdistrplus")

##discrete distribution for wickets
install.packages("vcd")
library(vcd)

library(MASS)

library(fitdistrplus)

descdist(rahul$score)
#Interpretations
#From the cullen and frey graph, we can see the observation at a skewness of almost 1 with a kurtosis of 3.
#It is close to the gamma line, so it is a weibull distribution.


#CA Lynn has not taken any wickets
# this seems to be because he is a batsman, not a bowler
# There are no visualizations I can make as he has not debuted in a match as a bowler yet.


########
#Assignment 2

names(hp)
required_data <- c("state_1","District",
                   "cerealstt_q",
                   "pulsestt_q",
                   "emftt_q",
                   "foodtotal_q",
                   "moong_q")
hp_data<- hp[required_data]
names(hp_data)
View(hp_data)

descdist(hp_data$cerealstt_q,discrete = FALSE) 
descdist(hp_data$pulsestt_q,discrete = FALSE)  
descdist(hp_data$emftt_q,discrete = FALSE) 
descdist(hp_data$foodtotal_q,discrete = FALSE)
descdist(hp_data$moong_q,discrete = FALSE)

cerealstt_q_logis = fitdistr(hp_data$cerealstt_q+0.001,'Logistic',lower=0.001)
cerealstt_q_logis$estimate
cereal_logis  = rlogis(1000,location = 12.030010,scale = 1.566138)
plot(density(cereal_logis))

pulsestt_q_logis = fitdistr(hp_data$pulsestt_q+0.001,'Logistic',lower=0.001)
pulsestt_q_logis$estimate
pulse_logis  = rlogis(1000,location = 1.3845726,scale = 0.3731095)
plot(density(pulse_logis))

emftt_q_weibull = fitdistr(hp_data$emftt_q+0.00001,'weibull',lower=0.001)
emftt_q_weibull$estimate
m_weibull  = rweibull(1000,shape = 0.21454557,scale = 0.01632833)
plot(density(m_weibull))

food_logis = fitdistr(hp_data$foodtotal_q+0.001,'Logistic',lower=0.001)
food_logis$estimate
food_logis  = rlogis(1000,location = 30.538941,scale = 6.621054)
plot(density(food_logis))

moong_weibull = fitdistr(hp_data$moong_q+0.00001,'weibull',lower=0.001)
moong_weibull$estimate
moon_weibull  = rweibull(1000,shape = 0.26152951,scale = 0.02769517)
plot(density(moon_weibull))

#confidence interval

cereal.cons = aggregate(hp_data$cerealstt_q, by=list(hp_data$District), FUN=mean)
View(cereal.cons)

pulses.cons = aggregate(hp_data$pulsestt_q, by=list(hp_data$District), FUN=mean)
View(pulses.cons)

emf.cons = aggregate(hp_data$emftt_q, by=list(hp_data$District), FUN=mean)
View(emf.cons)

food.cons = aggregate(hp_data$foodtotal_q, by=list(hp_data$District), FUN=mean)
View(food.cons)

moong.cons = aggregate(hp_data$moong_q, by=list(hp_data$District), FUN=mean)
View(moong.cons)


#Testing hypotheses

#Testing of hypothesis
#H1 = The cereal consumption is same across all districts in HP
#H0 = The cereal consumption is not the same across all districts in HP
cereals_test = aov(cerealstt_q~District, data = hp_data)
summary(cereals_test)

#H1 = The pulses consumption is same across all districts in HP
#H0 = The pulses consumption is not the same across all districts in HP
pulses_test = aov(pulsestt_q~District, data = hp_data)
summary(pulses_test)

#H1 = The emftt consumption is same across all districts in HP
#H0 = The emftt consumption is not the same across all districts in HP
meat_test = aov(emftt_q~District, data = hp_data)
summary(meat_test)

#H1 = The total food consumption is same across all districts in HP
#H0 = The total food consumption is not the same across all districts in HP
food_test = aov(foodtotal_q~District, data = hp_data)
summary(food_test)

#H1 = The moong consumption is same across all districts in HP
#H0 = The moong consumption is not the same across all districts in HP
moong_test = aov(moong_q~District, data = hp_data)
summary(moong_test)



