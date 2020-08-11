#run_analysis.R
#create one R script called run_analysis.R
#that does the following

#1. merges the training and test sets to create
#one data set
#first read the data and assign column names for joining
features <- read.table("./UCI HAR Dataset/features.txt",
                       col.names = c("n", "functions"))
activities <- read.table("./UCI HAR Dataset/activity_labels.txt",
                         col.names = c("label", "activities"))

testX <- read.table("./UCI HAR Dataset/test/X_test.txt",
                    col.names = features$functions)
testY <- read.table("./UCI HAR Dataset/test/Y_test.txt",
                    col.names = "label")

testSubject <- read.table("./UCI HAR Dataset/test/subject_test.txt", 
                          col.names = "subject")

trainX <- read.table("./UCI HAR Dataset/train/X_train.txt",
                     col.names = features$functions)
trainY <- read.table("./UCI HAR Dataset/train/Y_train.txt",
                     col.names = "label")
trainSubject <- read.table("./UCI HAR Dataset/train/subject_train.txt",
                           col.names = "subject")

#next combine the training dataset and the
#test dataset
x <- rbind(testX, trainX)
y <- rbind(testY, trainY)
subject <- rbind(testSubject, trainSubject)
mergedDF <- cbind(subject, y, x)

#2. Extracts only the measurements on the mean
#and standard deviation for each measurement

#select function has a contains() option
#that will identify a literal string
library(dplyr)
extractedDF <- select(mergedDF, subject, label, contains("mean."), contains("std"))
extractedDF <- select(extractedDF, -contains("angle"))

#3 Uses descriptive activity names to name the
#activities in the data set

extractedDF$label <- activities[extractedDF$label, 2]

#4 Appropriately labels the data set with
#descriptive variable names

descriptiveNames <- c("subject", "activity", 
                      "MeanTimeDomainSignalForBodyAccelerationOnAxisX",
                      "MeanTimeDomainSignalForBodyAccelerationOnAxisY",
                      "MeanTimeDomainSignalForBodyAccelerationOnAxisZ",
                      "MeanTimeDomainSignalForGravityAccelerationOnAxisX",
                      "MeanTimeDomainSignalForGravityAccelerationOnAxisY",
                      "MeanTimeDomainSignalForGravityAccelerationOnAxisZ",
                      "MeanTimeDomainSignalForBodyAccelerationJerkOnAxisX",
                      "MeanTimeDomainSignalForBodyAccelerationJerkOnAxisY",
                      "MeanTimeDomainSignalForBodyAccelerationJerkOnAxisZ",
                      "MeanTimeDomainSignalForBodyGyroscopeOnAxisX",
                      "MeanTimeDomainSignalForBodyGyroscopeOnAxisY",
                      "MeanTimeDomainSignalForBodyGyroscopeOnAxisZ",
                      "MeanTimeDomainSignalForBodyGyroscopeJerkOnAxisX",
                      "MeanTimeDomainSignalForBodyGyroscopeJerkOnAxisY",
                      "MeanTimeDomainSignalForBodyGyroscopeJerkOnAxisZ",
                      "MeanTimeDomainSignalForBodyAccelerationMagnitude",
                      "MeanTimeDomainSignalForGravityAccelerationMagnitude",
                      "MeanTimeDomainSignalForBodyAcceralationJerkMagnitude",
                      "MeanTimeDomainSignalForBodyGyroscopeMagnitude",
                      "MeanTimeDomainSignalForBodyGyroscopeJerkMagnitude",
                      "MeanFrequencyDomainSignalForBodyAccelerationOnAxisX",
                      "MeanFrequencyDomainSignalForBodyAccelerationOnAxisY",
                      "MeanFrequencyDomainSignalForBodyAccelerationOnAxisZ",
                      "MeanFrequencyDomainSignalForBodyAccelerationJerkOnAxisX",
                      "MeanFrequencyDomainSignalForBodyAccelerationJerkOnAxisY",
                      "MeanFrequencyDomainSignalForBodyAccelerationJerkOnAxisZ",
                      "MeanFrequencyDomainSignalForBodyGyroscopeOnAxisX",
                      "MeanFrequencyDomainSignalForBodyGyroscopeOnAxisY",
                      "MeanFrequencyDomainSignalForBodyGyroscopeOnAxisZ",
                      "MeanFrequencyDomainSignalForBodyAccelerationMagnititude",
                      "MeanFrequencyDomainSignalForBodyAccelarationJerkMagnitude",
                      "MeanFrequencyDomainSignalForBodyGyroscopeMagnitude",
                      "MeanFrequencyDomainSignalForBodyGyroscopeJerkMagnitude",
                      "StandardDeviationTimeDomainSignalForBodyAccelerationOnAxisX",
                      "StandardDeviationTimeDomainSignalForBodyAccelerationOnAxisY",
                      "StandardDeviationTimeDomainSignalForBodyAccelerationOnAxisZ",
                      "StandardDeviationTimeDomainSignalForGravityAccelerationOnAxisX",
                      "StandardDeviationTimeDomainSignalForGravityAccelerationOnAxisY",
                      "StandardDeviationTimeDomainSignalForGravityAccelerationOnAxisZ",
                      "StandardDeviationTimeDomainSignalForBodyAccelerationJerkOnAxisX",
                      "StandardDeviationTimeDomainSignalForBodyAccelerationJerkOnAxisY",
                      "StandardDeviationTimeDomainSignalForBodyAccelerationJerkOnAxisZ",
                      "StandardDeviationTimeDomainSignalForBodyGyroscopeOnAxisX",
                      "StandardDeviationTimeDomainSignalForBodyGyroscopeOnAxisY",
                      "StandardDeviationTimeDomainSignalForBodyGyroscopeOnAxisZ",
                      "StandardDeviationTimeDomainSignalForBodyGyroscopeJerkOnAxisX",
                      "StandardDeviationTimeDomainSignalForBodyGyroscopeJerkOnAxisY",
                      "StandardDeviationTimeDomainSignalForBodyGyroscopeJerkOnAxisZ",
                      "StandardDeviationTimeDomainSignalForBodyAccelerationMagnitude",
                      "StandardDeviationTimeDomainSignalForGravityAccelerationMagnitude",
                      "StandardDeviationTimeDomainSignalForBodyAccelerationJerkMagnitude",
                      "StandardDeviationTimeDomainSignalForBodyGyroscopMagnitude",
                      "StandardDeviationTimeDomainSignalForBodyGyroscopeJerkMagnitude",
                      "StandardDeviationFrequencyDomainSignalForBodyAccelerationOnAxisX",
                      "StandardDeviationFrequencyDomainSignalForBodyAccelerationOnAxisY",
                      "StandardDeviationFrequencyDomainSignalForBodyAccelerationOnAxisZ",
                      "StandardDeviationFrequencyDomainSignalForBodyAccelerationJerkOnAxisX",
                      "StandardDeviationFrequencyDomainSignalForBodyAccelerationJerkOnAxisY",
                      "StandardDeviationFrequencyDomainSignalForBodyAccelerationJerkOnAxisZ",
                      "StandardDeviationFrequencyDomainSignalForBodyGyroscopeOnAxisX",
                      "StandardDeviationFrequencyDomainSignalForBodyGyroscopeOnAxisY",
                      "StandardDeviationFrequencyDomainSignalForBodyGyroscopeOnAxisZ",
                      "StandardDeviationFrequencyDomainSignalForBodyAccelerationMagnitude",
                      "StandardDeviationFrequencyDomainSignalForBodyAccelerationJerkMagnitude",
                      "StandardDeviationFrequencyDomainSignalForBodyGyroscopeMagnitude",
                      "StandardDeviationFrequencyDomainSignalForBodyGyroscopeJerkMagnitude")
names(extractedDF) <- descriptiveNames
#and assigned the descriptive names to
#the dataframe

#5. From the dataset in step 4, creates a second,
#independent tidy data set with the average of each
#variable, for each activity, and each subject

tidyData <- extractedDF %>%
  group_by(subject, activity) %>%
  summarize_all(mean)

write.table(tidyData, "tidyData.txt", row.name=FALSE)
