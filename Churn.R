library(C50)
library(ggplot2)
library(dplyr)
library(mapproj)
library(maps)
library(reshape)
library(ggplot2)
library(pscl)
library(ROCR)
library(caret)
library(randomForest)
library(gbm)
library(mlbench)


data(churn)
str(churnTrain)
table(churnTrain$Class)

master <- rbind(churnTrain,churnTest)

master$international_cat <- ifelse(master$international_plan == "yes", 1, 0)
master$voice_cat <- ifelse(master$voice_mail_plan == "yes", 1, 0)
master$churn_cat <- factor(ifelse(master$churn == "yes", 1, 0))
master$area_415 <- ifelse(master$area_code == "area_code_415", 1, 0)
master$area_408 <- ifelse(master$area_code == "area_code_408", 1, 0)
master$allarea <- ifelse(master$area_code == "area_code_408", 1, ifelse(master$area_code == "area_code_415", 2, 3))  

#Churn
ggplot(data=master, aes(churn))+geom_bar(stat = "count",fill='lightgreen')
churn_rate <- sum(master$churn=='yes')/nrow(master)*100
print(churn_rate)

#Interesting insights
#Spending during the day
day <- master$total_day_charge
eve <- master$total_eve_charge
night <- master$total_night_charge
z <- as.data.frame(cbind(day,eve,night))
x <- melt(z)

ggplot(x, aes(x=value, fill=variable)) +
  geom_area(stat = "bin")+
  xlab("Charge")+
  ylab("Count")+
  facet_grid(variable~.)

#Percentage of revenue
day <- sum(master$total_day_charge)
eve <- sum(master$total_eve_charge)
night <- sum(master$total_night_charge)
sum <- day+eve+night
day <- day/sum
eve <- eve/sum
night <- night/sum
z <- as.data.frame(cbind(day,eve,night))
x <- melt(z)

ggplot(x, aes(x="", y=value, fill=variable))+
  geom_bar(width = 1, stat = "identity")+
  coord_polar("y", start=0)+
  scale_x_discrete("")

#Total Number of Calls
day <- master$total_day_calls
eve <- master$total_eve_calls
night <- master$total_night_calls
z <- as.data.frame(cbind(day,eve,night))
x <- melt(z)

ggplot(x, aes(x=value, fill=variable)) +
  geom_area(stat = "bin")+
  xlab("Number of Calls")+
  ylab("Count")+
  facet_grid(variable~.)

#Average Charge during the day
day_avgcharge <- master$total_day_charge/master$total_day_minutes
eve_avgcharge <- master$total_eve_charge/master$total_eve_minutes
night_avgcharge <- master$total_night_charge/master$total_night_minutes
z <- as.data.frame(cbind(day_avgcharge,eve_avgcharge,night_avgcharge))
x <- melt(z)

ggplot(x, aes(x=value, fill=variable)) +
  geom_freqpoly()+
  xlab("Average Charge for all the callers during the Day")+
  ylab("Count")+
  facet_grid(variable~.)

print(mean(master$total_day_charge)/mean(master$total_day_minutes))
print(mean(master$total_eve_charge)/mean(master$total_eve_minutes))
print(mean(master$total_night_charge)/mean(master$total_night_minutes))


#Map for total charge and Churn

us <- map_data("state")
stateFromLower <-function(x) {
  #read 52 state codes into local variable [includes DC (Washington D.C. and PR (Puerto Rico)]
  st.codes<-data.frame(
    state=as.factor(c("AK", "AL", "AR", "AZ", "CA", "CO", "CT", "DC", "DE", "FL", "GA",
                      "HI", "IA", "ID", "IL", "IN", "KS", "KY", "LA", "MA", "MD", "ME",
                      "MI", "MN", "MO", "MS",  "MT", "NC", "ND", "NE", "NH", "NJ", "NM",
                      "NV", "NY", "OH", "OK", "OR", "PA", "PR", "RI", "SC", "SD", "TN",
                      "TX", "UT", "VA", "VT", "WA", "WI", "WV", "WY")),
    full=as.factor(c("alaska","alabama","arkansas","arizona","california","colorado",
                     "connecticut","district of columbia","delaware","florida","georgia",
                     "hawaii","iowa","idaho","illinois","indiana","kansas","kentucky",
                     "louisiana","massachusetts","maryland","maine","michigan","minnesota",
                     "missouri","mississippi","montana","north carolina","north dakota",
                     "nebraska","new hampshire","new jersey","new mexico","nevada",
                     "new york","ohio","oklahoma","oregon","pennsylvania","puerto rico",
                     "rhode island","south carolina","south dakota","tennessee","texas",
                     "utah","virginia","vermont","washington","wisconsin",
                     "west virginia","wyoming"))
  )
  #create an nx1 data.frame of state codes from source column
  st.x<-data.frame(state=x)
  #match source codes with codes from 'st.codes' local variable and use to return the full state name
  refac.x<-st.codes$full[match(st.x$state,st.codes$state)]
  #return the full state names in the same order in which they appeared in the original source
  return(refac.x)
  
}
master$statefull <- stateFromLower(master$state)

master$totalcharge <- master$total_day_charge+master$total_eve_charge+master$total_night_charge
totalcharge<- aggregate(master$totalcharge, by=list(Category=master$statefull), FUN=sum)


ggplot()+ geom_map(data=us, map=us,aes(x=long, y=lat, map_id=region),
                    fill="#ffffff", color="#ffffff", size=0.15)+
                    geom_map(data=totalcharge, map=us,
                    aes(fill=x, map_id=Category),
                    color="#ffffff", size=0.15)+
                    scale_fill_continuous(low='thistle2', high='darkred', 
                                 guide='colorbar')+
                    labs(x=NULL, y=NULL)+
                    coord_map("albers", lat0 = 39, lat1 = 45)+ 
                    theme(panel.border = element_blank())+
                    theme(panel.background = element_blank())+
                    theme(axis.ticks = element_blank())+
                    theme(axis.text = element_blank())+
                    xlab("Total Charge Statewise")


totalchurn<- aggregate(as.numeric(master$churn_cat==1), by=list(Category=master$statefull), FUN=sum)

ggplot()+ geom_map(data=us, map=us,aes(x=long, y=lat, map_id=region),
                    fill="#ffffff", color="#ffffff", size=0.15)+
  geom_map(data=totalchurn, map=us,aes(fill=x, map_id=Category),
                    color="#ffffff", size=0.15)+
  scale_fill_continuous(low='lightblue', high='DarkRed',guide='colorbar')+
  labs(x=NULL, y=NULL)+
  coord_map("albers", lat0 = 39, lat1 = 45)+
  theme(panel.border = element_blank())+
  theme(panel.background = element_blank())+
  theme(axis.ticks = element_blank())+
  theme(axis.text = element_blank())+xlab("Total Churn Statewise")

###Running Logit to see the significant Variables
#Converting Categorical into Dummy
churnTrain$international_cat <- factor(ifelse(churnTrain$international_plan == "yes", 1, 0))
churnTrain$voice_cat <- factor(ifelse(churnTrain$voice_mail_plan == "yes", 1, 0))
churnTrain$churn_cat <- factor(ifelse(churnTrain$churn == "yes", 1, 0))
churnTrain$area_415 <- factor(ifelse(churnTrain$area_code == "area_code_415", 1, 0))
churnTrain$area_408 <- factor(ifelse(churnTrain$area_code == "area_code_408", 1, 0))
churnTrain$allarea <- factor(ifelse(churnTrain$area_code == "area_code_408", 1, ifelse(churnTrain$area_code == "area_code_415", 2, 3)))  
churnTrain$avg <- (churnTrain$total_day_charge+churnTrain$total_eve_charge+churnTrain$total_night_charge)/churnTrain$account_length

churnTest$international_cat <- factor(ifelse(churnTest$international_plan == "yes", 1, 0))
churnTest$voice_cat <- factor(ifelse(churnTest$voice_mail_plan == "yes", 1, 0))
churnTest$churn_cat <- factor(ifelse(churnTest$churn == "yes", 1, 0))
churnTest$area_415 <- factor(ifelse(churnTest$area_code == "area_code_415", 1, 0))
churnTest$area_408 <- factor(ifelse(churnTest$area_code == "area_code_408", 1, 0))
churnTest$allarea <- factor(ifelse(churnTest$area_code == "area_code_408", 1, ifelse(churnTest$area_code == "area_code_415", 2, 3)))
churnTest$avg <- (churnTest$total_day_charge+churnTest$total_eve_charge+churnTest$total_night_charge)/churnTest$account_length

#Correlation Plot
x <- subset(churnTrain, , -c(state,allarea, area_code, international_plan, voice_mail_plan,churn))
m <- x[,-c(17,18,19,20,21)]
#Checking for correlation
t <- cor(m)
corrplot(t,method = "circle")
#Removing COrrelated Variables
x <- x[,-c(3,6,9,12)]
#Running Logit Model
model <- glm(churn_cat ~.,family=binomial(link='logit'),data=x)
summary(model)
anova(model, test="Chisq")
pR2(model)
#Preparing test set
y <- subset(churnTest, , -c(state,allarea, area_code, international_plan, voice_mail_plan,churn))
#Predicting Model on test set
pred <- predict(model,newdata=y,type='response')
pred <- ifelse(pred > 0.5,1,0)
#Performance Metric for the model
confusionMatrix(pred,y$churn_cat)
roc(as.numeric(pred),as.numeric(y$churn_cat))
plot(roc(as.numeric(pred),as.numeric(y$churn_cat)))
varImp(model)

#Building Random Forest to see the results and compare with Logit

#Tuning Parameters for Random Forest
mtry <- tuneRF(x[,-14], x$churn_cat, ntreeTry=1000,stepFactor=3, plot=TRUE)
metric <- "Accuracy"
control <- trainControl(method="repeatedcv", number=5, repeats=3, search="grid")
tunegrid <- expand.grid(.mtry=c(1:15))
#Building Model based on Train set
rf_gridsearch <- train(churn_cat~., data=x, method="rf", metric=metric, tuneGrid=tunegrid, trControl=control)
print(rf_gridsearch)
plot(rf_gridsearch)
#Predicting on Test Set
Pred<-predict(rf_gridsearch,newdata=y)
#Finding Important Variables
plot(varImp(rf_gridsearch))
#Seeing performance metrics of the model
plot(roc(as.numeric(Pred),as.numeric(y$churn_cat)))
confusionMatrix(Pred,y$churn_cat)

