# 1.Merges the training and the test sets to create one data set.
# 2.Extracts only the measurements on the mean and standard deviation for each measurement. 
# 3.Uses descriptive activity names to name the activities in the data set
# 4.Appropriately labels the data set with descriptive variable names. 
# 5.From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.

packages <- c("data.table", "reshape2", "dplyr")
sapply(packages, require, character.only=TRUE, quietly=TRUE)

# load the data
features <- read.table("./UCI HAR Dataset/features.txt")
activities <- read.table("./UCI HAR Dataset/activity_labels.txt")
test_subjects <- read.table("./UCI HAR Dataset/test/subject_test.txt")
test_activity <- read.table("./UCI HAR Dataset/test/y_test.txt")
test_values <- read.table("./UCI HAR Dataset/test/X_test.txt")
train_subjects <- read.table("./UCI HAR Dataset/train/subject_train.txt")
train_activity <- read.table("./UCI HAR Dataset/train/y_train.txt")
train_values <- read.table("./UCI HAR Dataset/train/X_train.txt")

# set the column names for the activities
colnames(activities) <- c("id", "activity")

# set the column names for the measurements
colnames(features) <- c("id", "value")

names(test_activity) <- c("id")
names(train_activity) <- c("id")

# look for any feature name with "std" or "mean"
required_columns <- grep("std\\(|mean\\(", features$value)

# filter these from the test_values
required_test_values <- test_values[,required_columns]

# filter these from the train values
required_train_values <- train_values[,required_columns]

# get the required value labels
required_value_labels <- as.vector(features[required_columns, 2])

# add in the subject and activity
test_values <- cbind(test_subjects, "a" = test_activity$id, required_test_values)
train_values <- cbind(train_subjects, "a" = train_activity$id, required_train_values)

# combine the data sets
combined_values <- rbind(test_values, train_values)

# set the names on the subject and activity columns on combined
colnames(combined_values) <- c("subject", "activity_id", required_value_labels)

tidy <- combined_values %>%
  # convert to a long format to allow us to summarize
  melt(id.vars = c('subject', 'activity_id'), value.name = 'value') %>%
  
  # convert back to wide format with aggregation
  dcast(subject + activity_id ~ variable, fun.aggregate = mean) %>%
  
  # convert the activity id to the label (factor)
  merge(activities, by.x = 'activity_id', by.y = 'id', sort = FALSE)
  
# put the columns in the right order
tidy <- tidy[,c(2, 69, 3:68)]

write.table(tidy, file = "./tidy.txt", row.names = FALSE)
