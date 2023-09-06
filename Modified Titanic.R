data=read.csv("F:/Data Science/Titanic.csv")
print(data)



str(data)

names(data)

head(data)

summary(data$age)

sapply(data,function(x) which(is.na(x)))

data$age[is.na(data$age)]<-mean(data$age,na.rm= TRUE)
print(data)

data1<-data
for(i in 1:ncol(data)){
  data1[,i][is.na(data1[ ,i])]<-mean(data1[ ,i],na.rm= TRUE)
}
data1

boxplot(data$age)
boxplot(data$gender)
boxplot(data$sibsp)
boxplot(data$parch)
boxplot(data$fare)
boxplot(data$survived)

install.packages("zoo")
library("zoo")

data2<-na.aggregate(data)
data2

data2$age=as.numeric(format(round(data2$age,0)))
data2$fare=as.numeric(format(round(data2$fare,0)))
print(data2)
data2$gender=as.numeric(format(round(data2$gender,0)))
print(data2)
