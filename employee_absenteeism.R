rm(list=ls())
setwd('C:/chann/ds/edwisor/employee absenteeism')
#library(xlsx)
data=read.xlsx('Absenteeism_at_work_Project.xls',sheetIndex = 1)
str(data)
summary(data)
# Imputing 0 to 26 (Unjustified absence)
data$Reason.for.absence[which(data$Reason.for.absence %in% 0)]=26
# Changing the variable type
data$Reason.for.absence=as.factor(data$Reason.for.absence)
data$Month.of.absence=as.factor(data$Month.of.absence)
data$Day.of.the.week=as.factor(data$Day.of.the.week)
data$Seasons=as.factor(data$Seasons)
data$Disciplinary.failure=as.factor(data$Disciplinary.failure)
data$Education=as.factor(data$Education)
data$Son=as.factor(data$Son)
data$Social.drinker=as.factor(data$Social.drinker)
data$Social.smoker=as.factor(data$Social.smoker)
data$Pet=as.factor(data$Pet)
summary(data)

#######################Missing Value Analysis##############################

miss_values=data.frame(apply(data,2,function(x){sum(is.na(x))}))
miss_values$variable=row.names(miss_values)
row.names(miss_values)=NULL
miss_values=miss_values[,c(2,1)]
names(miss_values)=c("variable","Missing_percentage")
miss_values$Missing_percentage=miss_values$Missing_percentage/nrow(data)*100
miss_values=miss_values[order(-miss_values$Missing_percentage),]
miss_values$Missing_percentage=round(miss_values$Missing_percentage,2)
View(miss_values)
sum(is.na(data))
data$ID=as.factor(data$ID)
cat_index=which(sapply(data,is.factor))
cat_index
#table(as.factor(data$ID))
#sort(data$ID[is.na(data$Absenteeism.time.in.hours)])
for(i in 1:36){
  mode1=unique(data$Social.drinker[data$ID==i])
  data$Social.drinker[is.na(data$Social.drinker) & data$ID==i]=mode1
}
for(i in 1:36){
  mode1=unique(data$Social.smoker[data$ID==i])
  data$Social.smoker[is.na(data$Social.smoker)& data$ID==i]=mode1
}
for(i in 1:36){
  mode1=unique(data$Education[data$ID==i])
  data$Education[is.na(data$Education)& data$ID==i]=mode1  #There are 3 NA's left
}
#summary(data$Education)

#library(DMwR)

#set.seed(7)
#t=sample(740,10,replace=F)
#sam=data$Body.mass.index[t]
#data$Body.mass.index[t]=NA
#data->k
#data=k
#data$Body.mass.index[is.na(data$Body.mass.index)]=mean(data$Body.mass.index,na.rm = T)
#data$Body.mass.index[is.na(data$Body.mass.index)]=median(data$Body.mass.index,na.rm = T)
data=knnImputation(data,k=5)
#library(Metrics)
#mape(sam,data$Body.mass.index[t])  ############## mape is less for knn so we knn for imputation
#table(data$Reason.for.absence)
# as.factor(names(table(data$Reason.for.absence))[table(data$Reason.for.absence)==max(table(data$Reason.for.absence))])
#sam_Cat=data$Education[t]
#as.factor(names(table(data$Education))[table(data$Education)==max(table(data$Education))])
#table(data$Education)
#data$Education[t]=NA   ########## knn is performing better even for categorical variables so, we fix knn for imputing missing values 
#data$Education[t]
data=data[!data$Absenteeism.time.in.hours==0,]
#write.csv(data,"d1.csv",row.names = F)

sum(is.na(data))
###########3OUTLIER  ANALYSIS
t=0
for(i in 2:21){
  if(! i %in% cat_index){
   val=data[,i][data[,i] %in% boxplot.stats(data[,i])$out]
   t=t+length(val)
  }
}
# It's not a good idea to remove outliers as there are 229 in number.So, we replace outliers 
# in the data with NA and impute them using knn_imputation( ) function 

for (i in 2:21 ){
  if(! i %in% cat_index){
    data[,i][data[,i] %in% boxplot.stats(data[,i])$out]=NA
  }
}
sum(is.na(data))   # shows 211 NA values
data=knnImputation(data,k=5)
sum(is.na(data))   # 0 NA values
colnames(data)

num_index=sapply(data,is.numeric)

for(i in which(num_index)){
  data[,i]=as.integer(data[,i])
}
#write.csv(data,'preprocessed_Data.csv',row.names = F)
#PROBLEM1: What changes company should bring to reduce number of absenteeism ?
#Let's visualize 
#library(ggplot2)
#library(scales)

ggplot(data,aes_string(x=data$Seasons,y=data$Absenteeism.time.in.hours)) +
  geom_point(aes_string(color=data$Education))+ggtitle("absenteesism.time.in.hours Vs Seasons")+xlab("Season")+
  ylab("absenteeism time in hours")+theme_bw()
table(data$Seasons)
#Seasons (summer (1), autumn (2), winter (3), spring (4))

#sum(i)
seasons=aggregate(data$Absenteeism.time.in.hours,by=list(data$Seasons),sum)
seasons$x=seasons$x/sum(seasons$x)*100
ggplot(seasons,aes_string(x=seasons$Group.1,y=seasons$x))+geom_boxplot()+theme_bw()+
  xlab("Seasons")+ylab("Percentage of absenteesim time in hours")+scale_y_continuous(breaks = pretty_breaks(n=10))
# Employees tend to take leave in winter season more than 25 % of time

table(data$Service.time)
data$Service.time[data$Service.time>20]=median(data$Service.time)
ggplot(data,aes_string(x=data$Service.time,y=data$Absenteeism.time.in.hours)) +
  geom_point(aes_string(color=as.factor(data$Hit.target)))+ggtitle("absenteesism.time.in.hours Vs Service time")+xlab("Service time")+
  ylab("absenteeism time in hours")+theme_bw()+scale_colour_discrete(name="Hit target")

# Service_time ofemployee is one of the factor responsible for absenteeism time
#From the plot we can observe that employees whose service time is more than 12 hours are more and are
# responsible for absenteeism time in hours

ggplot(data,aes_string(x=data$Education,y=data$Absenteeism.time.in.hours)) +geom_point()+
  ggtitle("absenteesism.time.in.hours Vs Education")+xlab("Education")+ylab("absenteeism time in hours")+
  theme_bw()
educat=aggregate(data$Absenteeism.time.in.hours,by=list(data$Education),sum)
educat$x=educat$x/sum(educat$x)*100

ggplot(educat,aes_string(x=educat$Group.1,y=educat$x))+geom_boxplot()+theme_bw()+
  xlab("education")+ylab("Percentage of absenteesim time in hours")
# Education (high school (1), graduate (2), postgraduate (3), master and doctor (4)
#  employees whose education background is high school take 80 percent of absenteeism time in hours 
#################Disciplinary.failure    Reason.for.absence   ID
cat_index

reas=aggregate(data$Absenteeism.time.in.hours,by=list(data$Reason.for.absence),sum)
reas$x=reas$x/sum(reas$x)*100

ggplot(reas,aes_string(x=reas$Group.1,y=reas$x))+geom_boxplot()+theme_bw()+
  xlab("Reason for absence")+ylab("Percentage of absenteesim time in hours")
# 23 =>  medical consultation   (Employees with this reason are absent for more than 12.5% of total time)
# 13  =>  Diseases of the musculoskeletal system and connective tissue   (Employees with this reason are absent for 10% of total time)
# 28 => dental consultation (28) (Employees with this reason are absent for around 9% of total time)
# 19 =>Injury, poisoning and certain other consequences of external causes 
# 22  =>  7 categories without (CID) patient follow-up  (Employees with this reason are absent for around 8% of total time)
# 26 => unjustified absence (26) (Employees with this reason are absent for around 8% of total time)
# 27,10,11,18,1,14,25 => between 2.5 to 5 percent

table(data$Month.of.absence)
mon=aggregate(data$Absenteeism.time.in.hours,by=list(data$Month.of.absence),sum)
mon$x=mon$x/sum(mon$x)*100
ggplot(mon,aes_string(x=mon$Group.1,y=mon$x))+geom_boxplot()+theme_bw()+xlab("Month of absence")+
  ylab("Percent of absenteeism time ")
# 3 => march => employee in month of march tend to absent of about 13% of time
# 7=> july => employee in month of march tend to absent of about 11% of time

#PROBLEM1: What changes company should bring to reduce number of absenteeism ?

#1.Company should provide hygienic working conditions (Providing safety and health measures).
#2.Service time should be around  12 hours or less than that. (High workload leads to high absenteeism time)
#3. Providing leave facility based on employee's needs and organizational requirement
#4.Events can be organised in month of march and july .So there is a good chance to reduce absenteeism
#5.To reduce absenteeism it's better not to hire people who have diseases of musculoskelatal system and connective tissue complaints




####################################### Time Series Modeling###############
emp=aggregate(data$Absenteeism.time.in.hours,by=list(data$Month.of.absence),sum)
emp$Group.1=NULL
names(emp)=c("avg_loss") 
  # data is recorded from july 2007 to july 2010  
emp$avg_loss=emp$avg_loss/3
# to get loss per month ..july should be divided by 4.
emp$avg_loss[7]=emp$avg_loss[7]*3/4
timser=ts(emp$avg_loss)
plot(timser)
#plot(lag(timser))
###### checking if the time series is stationary series
#library(urca)
  
boxplot(timser~c(1:12))   
timser
summary(ur.df(timser,selectlags = "AIC"))
# p>0.05 that means data is not stationary
#plot(log(timser))
#plot(diff(log(timser)))
summary(ur.df(diff(log(timser)),selectlags = "AIC",lags =0))
# p< 0.05 that means we can reject null hypothesis .
#Therefore the diff(log(timser)) data is stationary
# Now , let's compute p and q values (Parameters used to build arima model)
acf(diff(log(timser)))
# q=0
pacf(diff(log(timser)))  # there we can see 4th value is below blue line and just before inverted line
#p=4
# diff is done onetime  so d=1
arima(log(timser),order = c(4,1,0))->model
# AIC value is 8.91
model

# Now, let's predict the losses in 2011
ts_pred=predict(model,n.ahead = 12)
ts_pred=exp(ts_pred$pred)
#plot(timser)
#2. How much losses every month can we project in 2011 if same trend of absenteeism continues?
plot(ts_pred)
title(main = " Expected Losses in 2011")


################################
# Model development for predicting employee absenteeism time in hours
#library(usdm)
#vifcor(data[,num_index])

###################feature selection
# Anova to select categorical features
summary(aov(formula = Absenteeism.time.in.hours~., data[,c(cat_index,21)])) 
# Reason.for.absence ,disciplinary.failure,Son are significant.So, we remove other categorical features
x1=data[,c(colnames(data[,num_index]))]
x2=data[,c(1,2,12)]
y=c(colnames(x2))
data=data.frame(x2,x1)
#library(corrgram)  
## Correlation Plot 
corrgram(data[,colnames(x1)], order = F,
         upper.panel=panel.pie, text.panel=panel.txt, main = "Correlation Plot")
# Correlation plot shows us that no independent variable better strength of association with absenteeism time in hours

sum(is.na(data)) # Checking for any missing values
cor(data$Weight,data$Body.mass.index)
data$Weight=NULL   # Removing weight column as it has high correlation with body.mass,index
cat_index
#library(dummies) 
#converting cat data to numeric
new_data=dummy.data.frame(data,names =y) #colnames(data[,c(cat_index)])

#library(caret)
set.seed(7688)
createDataPartition(new_data$Absenteeism.time.in.hours,list = F,p=0.8)->ind
train=new_data[ind,]
test=new_data[-ind,]

#library(rpart)
dt_model=rpart(Absenteeism.time.in.hours~.,data=train)
dt_pred=predict(dt_model,test)
#library(Metrics)
rmse(test$Absenteeism.time.in.hours,dt_pred) # 2.731049
library(rpart.plot)
rpart.plot(dt_model)
########Linear Regression
lr_model=lm(Absenteeism.time.in.hours~.,train)
summary(lr_model)
lr_pred=predict(lr_model,test)
rmse(test$Absenteeism.time.in.hours,lr_pred)  #2.774819

#library(randomForest)
rf_model=randomForest(Absenteeism.time.in.hours~.,train)
rf_pred=predict(rf_model,test)
rmse(test$Absenteeism.time.in.hours,rf_pred)  #2.916354

###########
#Apllying PCA

y_train=train$Absenteeism.time.in.hours
y_test=test$Absenteeism.time.in.hours
train$Absenteeism.time.in.hours=NULL
prin_comp <- prcomp(train)
names(prin_comp)
sd=prin_comp$sdev
var=sd**2
prop_var=var/sum(var)

#scree plot
plot(prop_var, xlab = "Principal Component",
     ylab = "Proportion of Variance Explained",
     type = "b")
#cumulative scree plot
plot(cumsum(prop_var), xlab = "Principal Component",
     ylab = "Cumulative Proportion of Variance Explained",
     type = "b")
train1=data.frame(Absenteeism.time.in.hours=y_train,prin_comp$x)
train1=train1[,1:41]
test$Absenteeism.time.in.hours=NULL
test1=predict(prin_comp,newdata =test)
test1=data.frame(test1)
test1=test1[,1:40]

######Decision Tree
dt_model1=rpart(Absenteeism.time.in.hours~.,train1,method = "anova")
dt_pred1=predict(dt_model1,test1)
rmse(y_test,dt_pred1)  # 2.997098

###LR
lr_model1=lm(Absenteeism.time.in.hours~.,train1)
summary(lr_model1)
lr_pred1=predict(lr_model1,test1)
rmse(y_test,lr_pred1)  #2.719461
# Random Forest

rf_model1=randomForest(Absenteeism.time.in.hours~.,train1)
rf_pred1=predict(rf_model1,test1)
rmse(y_test,rf_pred1)  #2.930581

##########As rmse value for lrmodel1 is less,we fix lr_model1 for predicting Absenteeism time in hours





