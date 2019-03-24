
## Reading dataset:

activity_lables <- read.table("./UCI HAR Dataset/activity_labels.txt")
features_name <- read.table("./UCI HAR Dataset/features.txt")

## Reading test files
testAddress <- paste("./UCI HAR Dataset/test")
body_acc_x_test <- read.table(paste(testAddress,"/Inertial Signals/body_acc_x_test.txt",sep=""))
body_acc_y_test <- read.table(paste(testAddress,"/Inertial Signals/body_acc_y_test.txt",sep=""))
body_acc_z_test <- read.table(paste(testAddress,"/Inertial Signals/body_acc_z_test.txt",sep=""))
body_gyro_x_test <- read.table(paste(testAddress,"/Inertial Signals/body_gyro_x_test.txt",sep=""))
body_gyro_y_test <- read.table(paste(testAddress,"/Inertial Signals/body_gyro_y_test.txt",sep=""))
body_gyro_z_test <- read.table(paste(testAddress,"/Inertial Signals/body_gyro_z_test.txt",sep=""))
total_acc_x_test <- read.table(paste(testAddress,"/Inertial Signals/total_acc_x_test.txt",sep=""))
total_acc_y_test <- read.table(paste(testAddress,"/Inertial Signals/total_acc_y_test.txt",sep=""))
total_acc_z_test <- read.table(paste(testAddress,"/Inertial Signals/total_acc_z_test.txt",sep=""))
subject_test <- read.table(paste(testAddress,"/subject_test.txt",sep=""))
test_set <- read.table(paste(testAddress,"/X_test.txt",sep=""))
test_labels <- read.table(paste(testAddress,"/y_test.txt",sep=""))                     
# Reading train files
trainAddress <- paste("./UCI HAR Dataset/train")
body_acc_x_train <- read.table(paste(trainAddress,"/Inertial Signals/body_acc_x_train.txt",sep=""))
body_acc_y_train <- read.table(paste(trainAddress,"/Inertial Signals/body_acc_y_train.txt",sep=""))
body_acc_z_train <- read.table(paste(trainAddress,"/Inertial Signals/body_acc_z_train.txt",sep=""))
body_gyro_x_train <- read.table(paste(trainAddress,"/Inertial Signals/body_gyro_x_train.txt",sep=""))
body_gyro_y_train <- read.table(paste(trainAddress,"/Inertial Signals/body_gyro_y_train.txt",sep=""))
body_gyro_z_train <- read.table(paste(trainAddress,"/Inertial Signals/body_gyro_z_train.txt",sep=""))
total_acc_x_train <- read.table(paste(trainAddress,"/Inertial Signals/total_acc_x_train.txt",sep=""))
total_acc_y_train <- read.table(paste(trainAddress,"/Inertial Signals/total_acc_y_train.txt",sep=""))
total_acc_z_train <- read.table(paste(trainAddress,"/Inertial Signals/total_acc_z_train.txt",sep=""))
subject_train <- read.table(paste(trainAddress,"/subject_train.txt",sep=""))
train_set <- read.table(paste(trainAddress,"/X_train.txt",sep=""))
train_labels <- read.table(paste(trainAddress,"/y_train.txt",sep=""))

# Adding subject to train and test sets
test_set<-cbind(subject_test,test_set)
names(test_set)[1]<-"subject"

train_set<-cbind(subject_train,train_set)
names(train_set)[1]<-"subject"

# Adding labels to train and test sets
test_set<-cbind(test_labels,test_set)
names(test_set)[1]<-"activity"

train_set<-cbind(train_labels,train_set)
names(train_set)[1]<-"activity"


# Giving names to each features
names(test_set)[3:563]<-as.character(features_name[,2])
names(train_set)[3:563]<-as.character(features_name[,2])


# Step 1: Merging test and train sets:
test_and_train_set <-rbind(test_set,train_set)

# Step 2: Extracting mean and standard deviation
test_and_train_set_modified <- test_and_train_set[grep("[mM]ean|std|activity|subject",names(test_and_train_set))]
# Step 3 :labeling the acitivity labels descriptively
test_and_train_set_modified$activity <-factor (test_and_train_set$activity,levels= as.character(c(1,2,3,4,5,6)),
                                             labels=c("walking","walkingupstairs","walkingdownstairs","sitting",
                                                      "standing","laying"))

# Step 4: Adjusting feature names
names(test_and_train_set_modified)<-tolower(names(test_and_train_set_modified))
names(test_and_train_set_modified)<- gsub("[()]","",names(test_and_train_set_modified))
names(test_and_train_set_modified)<- gsub("[-]","_",names(test_and_train_set_modified))

#Step 5: creating a table with the average of each variable for each activity and each subject.
test_and_train_set_modified<- data.table(test_and_train_set_modified)
# first method
independent_table_mean <- test_and_train_set_modified[,lapply(.SD,mean),by=c("activity","subject"),.SDcols=-c("activity","subject")]
names(independent_table_mean)[-c(1:2)] <- paste("mean of",names(test_and_train_set_modified)[-c(1:2)])
independent_table_mean<- independent_table_mean[order(independent_table_mean$subject,independent_table_mean$activity),]
                
#second method
Data1<- melt(test_and_train_set_modified,id.vars = c("activity","subject"))
independent_table_mean1<- dcast(Data1, activity + subject ~ variable,mean)
names(independent_table_mean1)[-c(1:2)] <- paste("mean of",names(test_and_train_set_modified)[-c(1:2)])
independent_table_mean1<- independent_table_mean1[order(independent_table_mean1$subject,independent_table_mean1$activity),]                
        
write.table(independent_table_mean,file="tidyTable.txt",row.names=FALSE)


