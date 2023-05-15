# install packages
#install.packages("ggplot2")
#install.packages("stargazer")
#install.packages("rms")
#install.packages(c('tibble', 'dplyr'))
#install.packages("stargazer")
#install.packages("rms")
#install.packages("randomForest")
#install.packages("tree")
#install.packages("rpart.plot")
#install.packages("ggthemes")

library(tree)
library(rpart)				        
library(rpart.plot)	
library(ggplot2)
library(stargazer)
require(methods)
require(psych)
library("car")
require(caTools)
library(MASS)
require(rms)
library(dplyr)
library(randomForest)
library(ggthemes)

#II) Data description
#import data
Olympics = read.csv("/Users/liu/Documents/FALL 2022/MGSC 401 -Stat founds of data analytics/final project/Olympic events data.csv")
attach(Olympics)

#remove columns ID, city, Team, Games, Season, and Event
Olympics = Olympics[,-c(1,7,9,12,14)]
attach(Olympics)

#remove NA observations that are in the variables: Age, height, weight, NOC, Year, Sport 

Olympics1 <- Olympics[!is.na(Olympics$Height), ]
Olympics1 <- Olympics1[!is.na(Olympics1$Weight), ]
Olympics1<- Olympics1[!is.na(Olympics1$Age), ]
Olympics1 <- Olympics1[!is.na(Olympics1$NOC), ]
Olympics1 <- Olympics1[!is.na(Olympics1$Year), ]
Olympics1 <- Olympics1[!is.na(Olympics1$Sport), ]

#create column name to see if an athlete is a Champion (won Gold, silver, or bronze) and remove medal column
library(tibble)
library(dplyr)
Olympics1 <- Olympics1 %>%
  add_column(Champion = if_else(is.na(Olympics1$Medal) , "0", "1"))
Olympics1 <- Olympics1[,-c(10)]

#remove all observations before the games of 1996 (leaving room for 20 years of data) and winter season
Olympics1 <- subset(Olympics1, Year >=1996)
Olympics1 <- subset(Olympics1, Season =="Summer")
Olympics1 <- Olympics1[,-c(8)]

#use groupby function to analyze the variables in terms of the number of Medals won 
NOC_groupby <- Olympics1 %>%
  group_by(NOC) %>%
  dplyr::summarise(Total_medals_won = sum(as.numeric(Champion)))

## Numerical variables (description using GGplot)
#Age
ggplot(Olympics1, aes(x=Age))+geom_histogram(aes(y=..density..), position="identity", binwidth = 3,color="white",fill="royalblue3") +xlab("Age")+ylab("Density")+ggtitle("Distribution of Variable Age")+
  theme_minimal()+theme(plot.title = element_text(hjust=0.5))+geom_density(size=0.7)+geom_vline(aes(xintercept=mean(Age)),color="violetred2",linetype="dashed", size=1)

ggplot(Olympics1, aes(y=Age))+geom_boxplot(color="black",fill="royalblue3",outlier.colour = "violetred2")+ylab("Age")+ggtitle("Boxplot of Age")+theme_minimal()+theme(plot.title = element_text(hjust=0.5)) 

#Height
ggplot(Olympics1, aes(x=Height))+geom_histogram(aes(y=..density..), position="identity", binwidth = 3,color="white",fill="royalblue3") +geom_density(size=0.7)+
  xlab("Height")+ylab("Density")+theme_minimal()+ ggtitle("Distribution of Variable Height")+theme(plot.title = element_text(hjust=0.5))+geom_vline(aes(xintercept=mean(Height)),color="violetred2",linetype="dashed", size=1)
ggplot(Olympics1, aes(y=Height))+geom_boxplot(color="black",fill="royalblue3",outlier.colour = "violetred2")+ylab("Height")+ggtitle("Boxplot of Height")+theme_minimal()+theme(plot.title = element_text(hjust=0.5)) 

#Weight
ggplot(Olympics1, aes(x=Weight))+geom_histogram(aes(y=..density..), position="identity", binwidth = 3,color="white",fill="royalblue3") +geom_density(size=0.7)+
  xlab("Weight")+ylab("Density")+theme_minimal()+ ggtitle("Distribution of Variable Weight")+theme(plot.title = element_text(hjust=0.5))+geom_vline(aes(xintercept=mean(Weight)),color="violetred2",linetype="dashed", size=1)
ggplot(Olympics1, aes(y=Weight))+geom_boxplot(color="black",fill="royalblue3",outlier.colour = "violetred2")+ylab("Weight")+ggtitle("Boxplot of Weight")+theme_minimal()+theme(plot.title = element_text(hjust=0.5)) 
mean(Olympics1$Weight)

#Year
ggplot(Olympics, aes(x=Year))+geom_histogram(binwidth = 4,color="white",fill="royalblue3") +xlab("Year")+ylab("Number of Participants")+ggtitle("Number of Participants throughout time")+theme_minimal()+theme(plot.title = element_text(hjust=0.5))
ggplot(Olympics1, aes(y=Year))+geom_boxplot(color="black",fill="royalblue3",outlier.colour = "violetred2")+ylab("Year")+ggtitle("Boxplot of Year")+theme_minimal()+theme(plot.title = element_text(hjust=0.5)) 

## Categorical variables (description using GGplot)
#NOC
ggplot(Olympics1, aes(x=NOC))+geom_bar(color="black",fill="lightskyblue2")+
  xlab("countries")+ylab("Number of Occurences")+ggtitle("Distribution of the countries")+theme_minimal()+
  theme(plot.title = element_text(hjust=0.5))

ggplot(NOC_groupby, aes(NOC, Total_medals_won))+geom_col()
NTOP_NOC <- subset(NOC_groupby, Total_medals_won>100)

Olympics1=Olympics1[grepl("USA|UKR|SWE|RUS|ROU|POL|NOR|NED|KOR|JPN|JAM|ITA|HUN|GER|GBR|FRA|ESP|DEN|CUB|CRO|CHN|CAN|BRA|BLR|AUS|ARG",Olympics1$NOC),]
attach(Olympics1)

#Graph to show the top countries (NTOP_NOC)
ggplot(NTOP_NOC,aes(x=reorder(NOC,-Total_medals_won),y=Total_medals_won, fill=Total_medals_won))+geom_bar(width=1,color="white",stat='identity')+
  xlab("Countries")+ylab("Total Medals won from 1996")+scale_fill_gradient(low="royalblue3",high="violetred2")+ggtitle("Top Countries in Medals")+theme_minimal()+theme(plot.title = element_text(hjust=0.5))

#Sex
ggplot(Olympics1, aes(x=Sex),fill=Sex)+geom_bar(color="white")+
  xlab("Sex")+ylab("Number of Occurences")+ggtitle("Distribution of Athletes' Sex")+theme_minimal()+scale_fill_manual(values=c("violetred2","royalblue3"))+
  theme(plot.title = element_text(hjust=0.5))

#Sport
ggplot(Olympics1, aes(x=Sport))+geom_bar(binwidth = 1,color="black",fill="grey")+
  xlab("Sport")+ylab("Number of Occurences")+
  ggtitle("Distribution of Sport")+theme_minimal()+theme(plot.title = element_text(hjust=0.5))

#Create Dummies for our categorical variables
Olympics1$Champion=as.factor(Olympics1$Champion)
Olympics1$Sex=as.factor(Olympics1$Sex)
Olympics1$NOC=as.factor(Olympics1$NOC)
Olympics1$Sport=as.factor(Olympics1$Sport)
attach(Olympics1)

#remove test data from training data
Olympics2 = Olympics1[!((Olympics1$Age==16|Olympics1$Age==17|Olympics1$Age==18|Olympics1$Age==19)&Olympics1$Year==2016),]

#Correlation matrix for numerical variables on stargazer
numerical_games =Olympics1[,unlist(lapply(Olympics1,is.numeric))]
corr_matrix=cor(numerical_games)
round(corr_matrix,2)
stargazer(corr_matrix,title=c('Correlation Matrix'),type="html")

attach(Olympics2)
vif_test=glm(Champion~Sex+Age+Height+Weight+NOC+Year+Sport, family="binomial")
vif(vif_test)


### III)  Model Building
## 3.1) Logistic regression
attach(Olympics2)
mlogit=glm(Champion~Sex+Age+Weight+NOC+Year+Sport, family="binomial")
summary(mlogit)

stargazer(mlogit,title=c('Final Logistic Regression Model'),covariate.labels=c("Sex","Age","Weight","Australia","Belarus","Brazil","Canada","China","Croatia",
                                                                               "Cuba", "Denmark", "Spain", "France", "United Kingdom", "Germany", "Hungary",
                                                                               "Italia", "Jamaica", "Japan", "Korea", "Netherlands", "Norway", "Poland", "Roumania",
                                                                               "Russia", "Sweden", "Ukraine", "USA","Year","Athletics", "Badminton", "Baseball", "Basketball", "Beach Volleyball", "Boxing", "Canoeing", "Cycling", "Diving",
                                                                               "Equestrianism", "Fencing", "Football", "Golf", "Gymnastics", "Handball", "Hockey", "Judo", "Modern Pentathlon", "Rhythmic Gymnastics",
                                                                               "Rowing", "Rugby Sevens", "Sailing", "Shooting", "Softball", "Swimming", "Synchronized Swimming", "Table Tennis", "Taekwondo", "Tennis","Trampolining",
                                                                               "Triathlon", "Volleyball", "Water Polo", "Weightlifting", "Wrestling",
                                                                         "Intercept"),no.space=T,type="html")

                                                                         
#plotting logistic function

plot1=ggplot(Olympics2, aes(y=as.numeric(Champion)-1, x=Height))+ylab("Probability of Winning")+xlab("Height")+
  ggtitle("Probability plot of variable Height")+theme_minimal()+theme(plot.title = element_text(hjust=0.5))
scatter=geom_point(color="royalblue3")
line=geom_smooth(method="glm", formula=y~x, method.args=list(family=binomial),colour="violetred2")
plot1+scatter+line

plot2=ggplot(Olympics2, aes(y=as.numeric(Champion)-1, x=Weight))+ylab("Probability of Winning")+xlab("Height")+
  ggtitle("Probability plot of variable Weight")+theme_minimal()+theme(plot.title = element_text(hjust=0.5))
scatter=geom_point(color="royalblue3")
line=geom_smooth(method="glm", formula=y~x, method.args=list(family=binomial),colour="violetred2")
plot2+scatter+line

plot3=ggplot(Olympics2, aes(y=as.numeric(Champion)-1, x=Age))+ylab("Probability of Winning")+xlab("Height")+
  ggtitle("Probability plot of variable Age")+theme_minimal()+theme(plot.title = element_text(hjust=0.5))
scatter=geom_point(color="royalblue3")
line=geom_smooth(method="glm", formula=y~x, method.args=list(family=binomial),colour="violetred2")
plot3+scatter+line


# R-squared in logistic models
require(rms)
mlogit2 =lrm(Champion~Sex+Age+Weight+NOC+Sport,data = Olympics2)
mlogit2


## 3.2) Random forest (with best cp and sqrt(7) parameters)
# find optimal cp
attach(Olympics2)
mytree=rpart(Champion~Sex+Age+Height+Weight+NOC+Sport,control=rpart.control(cp=0.001))
rpart.plot(mytree)
summary(mytree)

printcp(mytree)
plotcp(mytree)

opt_cp=mytree$cptable[which.min(mytree$cptable[,"xerror"]),"CP"]
opt_cp

#Use random forest method
myforest=randomForest(Champion~Sex+Age+Height+Weight+NOC+Sport, data=Olympics2, cp=0.001, importance=TRUE,  na.action = na.omit)
myforest

importance(myforest)
varImpPlot(myforest)

### IV) Predictions and results
#test data on unique players
TestData=Olympics1[Olympics1$Age==16|Olympics1$Age==17|Olympics1$Age==18|Olympics1$Age==19,]
TestData=TestData[TestData$Year==2016,]

#predicting using logistic function
TestData$Logistic_Probabilities<-predict(mlogit, TestData, type="response")

TestData$Logistic_predictions = ""
for (i in 1:length(TestData$Name)){
  TestData$Logistic_predictions[i]=ifelse(TestData$Logistic_Probabilities[i]>0.4,1,0)
}

#predicting using Random forest
TestData$RandomForest_Prediction <- predict(myforest, TestData, type="response")

# error rate compared to the players that actually won in 2016 
#logistic
attach(TestData)
k=0
for (j in 1:579){
  k=ifelse(TestData$Logistic_predictions[j]==TestData$Champion[j],k+1,k+0)
}
Logistic_error_rate=1-k/579
Logistic_error_rate # = 0.1606

#random forest
r=0
for (j in 1:579){
  r=ifelse(TestData$RandomForest_Prediction[j]==TestData$Champion[j],r+1,r+0)
}
RandomForest_error_rate=1-r/579
RandomForest_error_rate # = 0.1295

# similarity rate between the two models 
d=0
for (j in 1:579){
  d=ifelse(TestData$Logistic_predictions[j]==TestData$RandomForest_Prediction[j],d+1,d+0)
}
Models_similarity_rate=d/579
Models_similarity_rate # = 0.9309

# List of players using random Forest 
New_Recruits <- subset(TestData, RandomForest_Prediction==1)
New_Recruits <- New_Recruits[,-c(9,10,11,12)]
New_Recruits <- New_Recruits[!duplicated(New_Recruits), ] #23 recruits 

# total number of athletes aged 16 to 19 in 2016 
potential_athletes = TestData[,-c(9,10,11,12)]
potential_athletes <- potential_athletes[!duplicated(potential_athletes), ]

# Acceptance rate of 0.072289 
Acceptance_rate = nrow(New_Recruits)/nrow(potential_athletes)

# Graphs to analyze what are the characteristics shared by the new recruits
#women and men recruits
NR_Sex <- New_Recruits %>% 
  group_by(Sex) %>%
  dplyr::summarise(Count = length(as.numeric(Sex)))

ggplot(NR_Sex,aes(x=reorder(Sex,-Count),y=Count, fill=Sex))+geom_bar(stat='identity')+
  xlab("Sex")+ylab("Count")+scale_fill_manual(values = c('violetred2',"royalblue3"))+ggtitle("Distribution of Men and Women in the new recruits")+theme_minimal()+theme(plot.title = element_text(hjust=0.5))

#Country recruits
NR_Country <- New_Recruits %>% 
  group_by(NOC) %>%
  dplyr::summarise(Count = length(as.numeric(NOC)))

ggplot(NR_Country,aes(x=reorder(NOC,-Count),y=Count, fill=Count))+geom_bar(stat='identity')+
  xlab("Country")+ylab("Count")+ggtitle("Distribution of Nationalities in the new recruits")+scale_fill_gradient(low="royalblue3",high="violetred2")+theme_minimal()+theme(plot.title = element_text(hjust=0.5))

#Sports recruits
NR_Sports <- New_Recruits %>% 
  group_by(Sport) %>%
  dplyr::summarise(Count = length(as.numeric(Sport)))

ggplot(NR_Sports,aes(x=reorder(Sport,-Count),y=Count, fill=Count))+geom_bar(stat='identity')+
  xlab("Sport")+ylab("Count")+ggtitle("Distribution of Sport played by the new recruits")+scale_fill_gradient(low="royalblue3",high="violetred2")+theme_minimal()+theme(plot.title = element_text(hjust=0.5))

#Height distribution
ggplot(New_Recruits, aes(x=Height))+geom_histogram(binwidth = 2,color="white",fill="royalblue3") +
  xlab("Height")+ylab("Number of Occurences") +
  ggtitle("Distribution of Variable Height") + theme_minimal()+theme(plot.title = element_text(hjust=0.5))

#Weight distribution
ggplot(New_Recruits, aes(x=Weight))+geom_histogram(binwidth = 2,color="white",fill="royalblue3") +
  xlab("Weight")+ylab("Number of Occurences") +
  ggtitle("Distribution of Variable Weight") + theme_minimal()+theme(plot.title = element_text(hjust=0.5))


