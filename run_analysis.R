# The script for Getting and Cleaning Data course.

# Task 1. 
# Merge the training and the test sets 
# to create one data set.

train_DF <- read.table("train\\X_train.txt")
test_DF <- read.table("test\\X_test.txt")
X_merged_DF <- rbind(train_DF, test_DF)

train_DF <- read.table("train\\y_train.txt")
test_DF <- read.table("test\\y_test.txt")
Y_merged_DF <- rbind(train_DF, test_DF)

train_DF <- read.table("train\\subject_train.txt")
test_DF <- read.table("test\\subject_test.txt")
S_merged_DF <- rbind(train_DF, test_DF)

# Task 2.
# Extract only the measurements on the mean 
# and standard deviation for each measurement. 

features_DF <- read.table("features.txt")
std_and_mean_features <- grep("-std\\(\\)|-mean\\(\\)", features_DF[, 2])
names(X_merged_DF) <- gsub("\\(|\\)", "", names(X_merged_DF))
X_merged_DF <- X_merged_DF[, std_and_mean_features]

# Task 3. 
# Use descriptive activity names to name 
# the activities in the data set.

activity_labels <- read.table("activity_labels.txt")
activity_labels[, 2] = gsub("_", "", tolower(as.character(activity_labels[, 2])))
Y_merged_DF[,1] = activity_labels[Y_merged_DF[,1], 2]

# Task 4. 
# Appropriately label the data set with 
# descriptive variable names.

names(X_merged_DF) <- features_DF[mean_and_std_features, 2]
names(X_merged_DF) <- tolower(names(X_merged_DF))
names(Y_merged_DF) <- "activity"
names(S_merged_DF) <- "subject"
final_DF <- cbind(S_merged_DF, Y_merged_DF, X_merged_DF)
write.table(final_DF, "final_data_set.txt",row.name=F)

# 5. From the data set in step 4, creates a second, 
# independent tidy data set with the average of each
# variable for each activity and each subject.

final_means_DF <- data.frame()
subject_list = unique(S_merged_DF)[,1]
# Split final data frame into list of data frames by subject as factor
split_by_subj <- split(final_DF,as.factor(final_DF$subject))
for (i in subject_list) { # for each subject
  split_tmp <- split_by_subj[i]
  # Extract data frame from the list
  split_DF_tmp <- split_tmp[[1]]
  # Split data frame per subject into list of data frames by activity as factor
  split_by_act <- split(split_DF_tmp,as.factor(split_DF_tmp$activity))
  for (j in 1:6) { # for each activity
    # Extract data frame from the list
    split2_DF_tmp <- split_by_act[[j]]
    # Calculate means actoss all variables per subject per activity 
    list_of_means_tmp <- lapply(split2_DF_tmp,mean)
    # Convert list of means to the data frame
    means_DF_tmp <- as.data.frame(list_of_means_tmp)
    # Restore broken (NA) activity labels
    means_DF_tmp$activity <- split2_DF_tmp$activity[1]
    # Add to the resulting data frame
    final_means_DF <- rbind(final_means_DF,means_DF_tmp)
  }
}
write.table(final_means_DF, "final_data_set_of_means.txt",row.name=F)

