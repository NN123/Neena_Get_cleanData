# 1.  Merges the training and the test sets to create one data set.
# 2.	Extracts only the measurements on the mean and standard deviation for each measurement. 
# 3.	Uses descriptive activity names to name the activities in the data set
# 4.	Appropriately labels the data set with descriptive variable names. 
# 5.	From the data set in step 4, creates a second, independent tidy data set with the average 
#   of each variable for each activity and each subject.

rm(list=ls())
setwd("C://2_Neena/3 Clean Data/Assign/Data");

training_data <- read.table("./UCI HAR Dataset/train/X_train.txt", header = FALSE)
test_data <- read.table("./UCI HAR Dataset/test/X_test.txt", header = FALSE)
training_label <- read.table("./UCI HAR Dataset/train/y_train.txt", header = FALSE)
test_label <- read.table("./UCI HAR Dataset/test/y_test.txt", header = FALSE)
feature_label <- read.table("./UCI HAR Dataset/features.txt", header = FALSE)
activity_label <- read.table("./UCI HAR Dataset/activity_labels.txt", header = FALSE)

subject_train <- read.table("./UCI HAR Dataset/train/subject_train.txt", header = FALSE)
subject_test <- read.table("./UCI HAR Dataset/test/subject_test.txt", header = FALSE)

library(dplyr)
sapply(training_label, class)


## 1. Merges the training and the test sets to create one data set:
labels<- rbind(training_label,test_label)
names(labels)<- "Activity_Num"

subject<-rbind(subject_train,subject_test)
names(subject)<-"subject"

str(feature_label)
head(labels,3)
head(subject,3)

data_set<-rbind(training_data,test_data)
names(feature_label)<-c("fNum","fName")
names(data_set)<-feature_label$fName
names(activity_label)<-c("Activity_Num","Activity_Name")

head(feature_label)
head(data_set)

sub_activity<-cbind(subject,labels)
str(sub_activity)
tail(sub_activity,3)

comb_data<-cbind(sub_activity,data_set)
str(comb_data)
tail(comb_data,2)


# 2.  Extracts only the measurements on the mean and standard deviation for each measurement. 
mean_std<-feature_label  %>% filter(grepl("mean\\(\\)|std\\(\\)", fName,ignore.case=T)) %>% select(-fNum)
mean_std<-as.character(mean_std[,1])
 
str(mean_std)
head(mean_std)


req_flds<-c("subject","Activity_Num",as.character(mean_std))

comb_data2<-comb_data[req_flds]

# 3.  Uses descriptive activity names to name the activities in the data set
head(activity_label)
final_data<-merge(activity_label, comb_data2 , by="Activity_Num", all.x=TRUE)
head(final_data,2)

# 4.  Appropriately labels the data set with descriptive variable names. 


names(final_data)<-gsub("^t", "time", names(final_data))
names(final_data)<-gsub("^f", "frequency", names(final_data))
names(final_data)<-gsub("Acc", "Accelerometer", names(final_data))
names(final_data)<-gsub("Gyro", "Gyroscope", names(final_data))
names(final_data)<-gsub("Mag", "Magnitude", names(final_data))
names(final_data)<-gsub("BodyBody", "Body", names(final_data))

names(final_data)

# 5.	From the data set in step 4, creates a second, independent tidy data set with the average 
#   of each variable for each activity and each subject.
?aggregate
tidy_data<-aggregate(.~subject +Activity_Name, final_data,mean)
tidy_data<-tidy_data%>% arrange(subject,Activity_Num)
head(tidy_data,2)

write.table(tidy_data, file = "tidydata.txt",row.name=FALSE)
