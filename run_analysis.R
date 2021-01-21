library(dplyr)
library(data.table)

# train database

data_train <- read.delim("./train/X_train.txt", head= FALSE, sep = "")
label_train <- read.delim("./train/y_train.txt", head = FALSE)
subject_train <- read.table("./train/subject_train.txt")
#train <- cbind(label_train, data_train)

# test database

data_test <- read.delim("./test/X_test.txt", head= FALSE, sep = "")
label_test <- read.delim("./test/y_test.txt", head = FALSE)
subject_test <- read.table("./test/subject_test.txt")
#test <- cbind(label_test, data_test)

# features

features <- read.table("./features.txt", col.names= c("featureid", "featurename"))

# Setting columns names to train and test
colnames(data_train) <- features$featurename
colnames(data_test) <- features$featurename
colnames(subject_train) <- c("subjectid")
colnames(label_train) <- "labelid"
colnames(label_test) <- "labelid"
colnames(subject_test) <- c("subjectid")

# Merging data and label

train <- cbind(label_train, subject_train, data_train)
test <- cbind(label_test, subject_test, data_test)

# Merging train and test

dbmerged <- rbind(train, test)

# Extracts only the measurements on the mean and standard deviation for each measurement. 

mean_db <- grepl("mean", colnames(dbmerged))
std_db <- grepl("std", colnames(dbmerged))
labelid_db <- grepl("labelid", colnames(dbmerged))

mean_std <- dbmerged[,mean_db | std_db | labelid_db]

# Read activity_labels
activity_labels <- read.table("./activity_labels.txt")
colnames(activity_labels) <- c("activityid", "activityname")

# Uses descriptive activity names to name the activities in the data set

dbmerged_activity <- merge(activity_labels, dbmerged, by.y = "labelid", by.x = "activityid") 

# Appropriately labels the data set with descriptive variable names.
# previously done ^^ 

# From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.

act_subj_mean <- aggregate(dbmerged_activity[, 4:564],list(activityid=dbmerged_activity$activityid, subjectid=dbmerged_activity$subjectid), mean)
act_subj_mean <- act_subj_mean[order(act_subj_mean$activityid, act_subj_mean$subjectid),]

write.table(act_subj_mean, "act_subj_mean.txt", row.name=FALSE)