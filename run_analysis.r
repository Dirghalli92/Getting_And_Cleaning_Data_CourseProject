#Loads Packages
library(data.table)
library(plyr)

#Download & Extract Data Sets
filename <- "Week_4_Project.zip"
wd <- getwd()

if (!file.exists(filename)){
        fileURL <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip "
        download.file(fileURL, filename, mode = "wb")
        unzip(filename) 
        setwd(paste(wd,"UCI HAR Dataset", sep = "/"))
}  

if (file.exists(filename)) {
        unzip(filename) 
        setwd(paste(wd,"UCI HAR Dataset", sep = "/"))
}  

## Reads appropriate files into R

# Descriptive Labels
feature_names <- read.table(file = "features.txt")
act_labels <- read.table(file = "activity_labels.txt",header = FALSE)

# Trainign Data Sets
train_subject <- read.table(file = "train/subject_train.txt",header = FALSE)
train_labels <- read.table(file = "train/y_train.txt",header = FALSE)
train_sets <- read.table(file = "train/X_train.txt",header = FALSE)

# Testing Data Sets
test_subject <- read.table(file = "test/subject_test.txt",header = FALSE)
test_labels <- read.table(file = "test/y_test.txt",header = FALSE)
test_sets <- read.table(file = "test/X_test.txt",header = FALSE)


# Combine Train/Test Data Sets 
subject <- rbind(train_subject,test_subject)
activity <- rbind(train_labels,test_labels)
features <- rbind(train_sets,test_sets)

# Clean Workspace for Clarity
rm(train_subject,train_labels,train_sets,test_subject,test_labels,test_sets)

#Assign names to Columns 
colnames(subject) <- "Subject"
colnames(activity) <- "Activity"
colnames(features) <- t(feature_names[2])

#Final Data Merge
All_Data <- cbind(features,subject,activity)

# Pull requested Means/SD
colMeanStd <- grep(".*Mean.*|.*Std.*",x = names(All_Data), ignore.case=TRUE)
filteredCols <- c(colMeanStd,562:563)
filteredData <- All_Data[,filteredCols]

## Assign Descriptive Names to the Activity field
filteredData$Activity <- as.character(filteredData$Activity)
for(i in 1:6) {
        filteredData$Activity[filteredData$Activity == i] <- as.character(act_labels[,2])
}
filteredData$Activity <- as.factor(filteredData$Activity)


# Remove parentheses & Restructures names for clarity
names(filteredData) <- gsub('\\(|\\)',"",names(filteredData), perl = TRUE)
names(filteredData) <- make.names(names(filteredData))
names(filteredData) <- gsub('Acc',"Acceleration",names(filteredData))
names(filteredData) <- gsub('GyroJerk',"AngularAcceleration",names(filteredData))
names(filteredData) <- gsub('Gyro',"AngularSpeed",names(filteredData))
names(filteredData) <- gsub('Mag',"Magnitude",names(filteredData))
names(filteredData) <- gsub('^t',"TimeDomain.",names(filteredData))
names(filteredData) <- gsub('^f',"FrequencyDomain.",names(filteredData))
names(filteredData) <- gsub('\\.mean',".Mean",names(filteredData))
names(filteredData) <- gsub('\\.std',".StandardDeviation",names(filteredData))
names(filteredData) <- gsub('Freq\\.',"Frequency.",names(filteredData))
names(filteredData) <- gsub('Freq$',"Frequency",names(filteredData))

## Extract Tidy Data Set
filteredData$Subject <- as.factor(filteredData$Subject)
filteredData <- data.table(filteredData)

tidydata <- aggregate(. ~Subject+Activity,filteredData,mean) 
tidydata <- tidydata[order(tidydata$Subject,tidydata$Activity),]
write.table(x = tidydata,file = "tidy_data.txt",row.names = FALSE)

#Clean Workspace for Clarity 
rm(activity,features,subject)