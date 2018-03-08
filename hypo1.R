#install.packages('gtools')
#load library
library(gtools)
#get all permutations
x=c("l","m","h")
p=permutations(n=3,r=3,v=x,repeats.allowed=T)
a=data.frame(p)
a=a[-c(1,14,27),]
x <- paste(a$X1,a$X2, a$X3)

sample=sample(x, 500, replace=TRUE)
head(sample)
library(stringr)
b=str_split_fixed(sample, " ", 3)
df=data.frame(b)
colnames(df) <- c("morning", "noon" , "night")
df

##############pattern recognition
library(data.table)
setDT(df)[,pattern:=rleid(morning,noon,night)]
library(dplyr)
group_indices(df, morning, noon, night)
group_indices_(df, .dots = names(df))
df$pattern <- group_indices(df, morning, noon, night)
df %>% mutate(pattern = group_indices_(df, .dots = c('morning', 'noon', 'night')))

table(df$pattern)

##########################################################################################

#data generation
l=sample(30:70,40,replace=F)
m=sample(80:200,100,replace=F)
n=sample(220:400,120,replace=F)

morning=sample(c(l,m,n),50000,replace=T)
noon=sample(c(l,m,n),50000,replace=T)
night=sample(c(l,m,n),50000,replace=T)

df=data.frame(morning,noon,night)
head(df, 20)

df$morning=as.numeric(df$morning)
df$noon=as.numeric(df$noon)
df$night=as.numeric(df$night)
dim(df)

a1 <- which(df$morning>=220 & df$noon>=220 & df$night>=220)
df=df[-a1,]
a2 <- which(df$morning<=70 & df$noon<=70 & df$night<=70)
df=df[-a2,]
a3 <- which(df$morning>=80 & df$morning<=200 & df$noon>=80 & df$noon<=200 & df$night>=80 & df$night<=200)
df=df[-a3,]
dim(df)

head(df,25)

############################

#generate data
date=rep(seq(as.POSIXct("2017-01-01"), as.POSIXct("2018-01-01"), by="day"),1)
length(date)
df1=df[1:366,]
data=data.frame(date,df1)


#transform data (columns into rows)
library(reshape)
data1= melt(data, id=(c("date")))
library(lubridate)
data1$date=as.POSIXct(paste(data1$date), format="%Y-%m-%d")
colnames(data1) <- c("date", "Time_of_day","Glucose")


#sort data by date
data2 <- data1[(order(as.Date(data1$date))),]

# Add other columns to the data
data2$month=month(as.POSIXlt(data2$date, format="%Y-%m-%d"))
data2$week <- week(as.Date(data2$date))
data2$weekdays <- weekdays(as.Date(data2$date))

head(data2)

#reordered the data with consecutive index numbers
rownames(data2) <- NULL

#Response variable is next 24 glucose readings
data3=data2[4:1098,]
Glucose_24hr=data3$Glucose
data4=data2[1:1095,]
data4$Glucose_24hr=Glucose_24hr


# response variable is event hypo, hyper, normal
# <70 hypo
#> 220 hyper
#other than above is normal
data4$event=ifelse(data4$Glucose_24hr<70,"hypo", ifelse(data4$Glucose_24hr>220,"hyper","normal"))  
head(data4)

# response variable is hypo event 
data4$hypo_event=ifelse(data4$Glucose_24hr<70,1,0)
head(data4)

prop.table(table(data4$hypo_event))

# extract data for analysis
new=data4[,-c(1,7)]
head(new)

#variables has to be converted to the factors
datafactor <-c(1,3:6)
new[,datafactor]<-lapply(new[,datafactor],factor)


#Data Parition
n = nrow(new)
s = sample(n, n*0.7, replace = F)
train = new[s,]
test = new[-s,]
train1 = train[,-6]
test1 = test[,-6]

#........1 Random Forest
library(randomForest)
library(caret)
model <- randomForest(hypo_event~., data = train)
pred<-predict(model,test1)
confusionMatrix(test$hypo_event,pred,mode = "everything")


#.. Rf with cross validation
# define training control
train_control <- trainControl(method="cv", number=5)
model1 <- train(hypo_event~., data=new, trControl=train_control, method="rf")
# summarize results
print(model1)


#.............. svm
library(e1071)
model2<-svm(event~., data<-train, kernel="linear", cost=2)
pred <-predict(model2, test1, type="prob")
#acc
t=table(test$event, pred)
acc<-sum(diag(t))/sum(t)
acc

#.. svm with cross validation
# define training control
train_control <- trainControl(method="cv", number=5)
model2 <- train(event~., data=new, trControl=train_control, method="svmLinearWeights")
# summarize results
print(model2)


##............more data
# medications (oral , insulin) and dosage (0.5, 1, 1.5, 2)
new1=new
new1$insulin = sample(1:10, 1095, replace=T)
new1$oral = sample(c(500,750,1000,2000), 1095, replace=T)
new1$dosage = sample(c(0.5,1.0,1.5,2.0), 1095, replace=T)

#Data Parition
new2=new1[sample(nrow(new1)), ]
new2=new2[sample(nrow(new2)), ]
dim(new2)
head(new2)

n = nrow(new2)
s = sample(n, n*0.7, replace = F)
train = new2[s,]
test = new2[-s,]
train1 = train[,-6]
test1 = test[,-6]

#........1 Random Forest
library(randomForest)
library(caret)
model <- randomForest(hypo_event~., data = train)
pred<-predict(model,test1)
confusionMatrix(test$hypo_event,pred,mode = "everything")

#.. Rf with cross validation
# define training control
train_control <- trainControl(method="cv", number=5)
model1 <- train(hypo_event~., data=new, trControl=train_control, method="rf")
# summarize results
print(model1)

#.............. svm
library(e1071)
model2<-svm(event~., data<-train, kernel="linear", cost=2)
pred <-predict(model2, test1, type="prob")
#acc
t=table(test$event, pred)
acc<-sum(diag(t))/sum(t)
acc


##....unbalaned problem
library(ROSE)
prop.table(table(train$hypo_event))
head(train)

########### Random over sampling
data_balanced_over <- ovun.sample(hypo_event ~ ., data = train, method = "over",N = 1230)$data
table(data_balanced_over$hypo_event)

tree.over <- randomForest(hypo_event~ ., data = data_balanced_over)
pred.tree.over <- predict(tree.over, newdata = test1)
confusionMatrix(test$hypo_event,pred.tree.over ,mode = "everything")





