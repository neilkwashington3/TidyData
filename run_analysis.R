setwd("C://Users/Neil/Documents/Data Science Specialization/getdata-projectfiles/UCI HAR Dataset")

X_train<-read.table("./train/X_train.txt")
X_test<-read.table("./test/X_test.txt")
subject_train<-read.table("./train/subject_train.txt")
subject_test<-read.table("./test/subject_test.txt")
y_train<-read.table("./train/y_train.txt")
y_test<-read.table("./test/y_test.txt")
total<-rbind(X_test,X_train)
##total is data frame with 10299 rows and the X_test set of 2947 rows first then the
##X_train set of 7352 rows and 561 columns corresponding to the 561 features


features<-read.csv("features.txt",header=F,sep="")
##features is 561 row data frame with V2 being the type of measurement that was
##collected

means<-grep("mean()",features$V2, fixed=T)
std<-grep("std()",features$V2, fixed=T)
meansStdsLocation<-c(means,std)
meansStdsLocation<-sort(meansStdsLocation)
##meansStds is a vector of Integers with each number corresponding to a spot where a
##mean of standard deviation measurement is located

total<-total[,meansStdsLocation]
##now total has 66 rows corresponding to where means and stds are located

meansStdsNames<-features[meansStdsLocation,2]
meansStdsNames<-gsub("^f","Frequency",meansStdsNames)
meansStdsNames<-gsub("^t","Time",meansStdsNames)
meansStdsNames<-gsub("mean()","mean",meansStdsNames,fixed=T)
meansStdsNames<-gsub("std()","std",meansStdsNames,fixed=T)
meansStdsNames<-gsub("-","_",meansStdsNames,fixed=T)
colnames(total)<-meansStdsNames

totalSubject<-rbind(subject_test,subject_train)
colnames(totalSubject)<-"Subject"
totalAct<-rbind(y_test,y_train)
colnames(totalAct)<-"Activity"
total<-cbind(totalAct,totalSubject,total)
##total is data frame with 10299 rows and 68 columns with the first column being
##the Activity, the Second being the Subject, and the next 66 being means and stds

for(i in 1:nrow(total)){ 
if(total[i,1]==1){total[i,1]<-"Walking"}
if(total[i,1]==2){total[i,1]<-"Walking Upstairs"}
if(total[i,1]==3){total[i,1]<-"Walking Downstairs"}
if(total[i,1]==4){total[i,1]<-"Sitting"}
if(total[i,1]==5){total[i,1]<-"Standing"}
if(total[i,1]==6){total[i,1]<-"Laying"}
}

library(dplyr)
totalGrouped<-group_by(total,Subject,Activity)
totalGrouped<-summarise_each(totalGrouped,funs(mean))

write.table(totalGrouped,row.names=FALSE)
