## Extract and read "test" and "train" data from working directory
test <- read.table("X_test.txt", header = FALSE, dec = ".")
train <- read.table("X_train.txt", header = FALSE, dec = ".")

## Combine test and train into one large data set
alldat <- rbind(test, train)

## Extract and read activity label and feature data
activities <- read.table("activity_labels.txt", header = FALSE)
features <- read.delim("features.txt", header = FALSE)

## Use feature data to create column names for combined dataframe
colnames(alldat) <- features[,1]

## Identify which columns in dataframe include "mean" or "std" in their name
mean_std_col <- grep("mean()|std()",names(alldat))

## Use previous step to create new, smaller dataframe with only "mean" and "std" columns
mean_std_dat <- alldat[,mean_std_col]

## Extract and read y data for test and train, which represents activity type numerically
ytest <- read.table("y_test.txt", header = FALSE, dec = ".")
ytrain <- read.table("y_train.txt", header = FALSE, dec = ".")

## Combine test and train activity type data into one label column
all_labels <- rbind(ytest, ytrain)

## Bind label column to mean & std dataframe created above
label_dat <- cbind(mean_std_dat, all_labels)

## Rename label column to be descriptive as "activity.label"
label_dat <- rename(label_dat, c("activity.label" = "V1"))

## Create new column in dataframe to assign descriptive names to numeric activity labels
activity_full <- label_dat %>%
  mutate(activity = case_when(
    activity.label == 1 ~ "walking",
    activity.label == 2 ~ "walking upstairs",
    activity.label == 3 ~ "walking downstairs",
    activity.label == 4 ~ "sitting",
    activity.label == 5 ~ "standing",
    activity.label == 6 ~ "laying"))

## Change column headers so they fit descriptive naming convention by eliminating "()"    
names(activity_full) <- gsub("()","",names(activity_full), fixed = TRUE)   

## Extract and read subject data for both test and train data sets
test_subject <- read.table("subject_test.txt", header = FALSE, dec = ".")
train_subject <- read.table("subject_train.txt", header = FALSE, dec = ".")

## Cobmine test and train subject data into one column
all_subject <- rbind(test_subject, train_subject)


## Add subject column to mean & std dataframe
all_dat <- cbind(activity_full, all_subject)

## Change name of subject column to be descriptive
all_dat <- rename(all_dat, c("subject" = "V1"))

## Create tidy data set that aggregates data by activty and subject, then finds mean for every other column
tidy_dat <- aggregate(.~activity+subject, all_dat, mean)

## Write txt file of final tidy data set
write.table(tidy_dat, "tidy_dat.txt", row.name = FALSE)