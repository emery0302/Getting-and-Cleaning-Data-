library(dplyr)

# download data

filename <- "Coursera_DS3_Final.zip"

if (!file.exists(filename)){
  fileURL <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
  download.file(fileURL, filename, method="curl")
}  

if (!file.exists("UCI HAR Dataset")) { 
  unzip(filename) 
}

# read data
features <- read.table("UCI HAR Dataset/features.txt", col.names = c("n","functions"))
activities <- read.table("UCI HAR Dataset/activity_labels.txt", col.names = c("code", "activity"))
subject_test <- read.table("UCI HAR Dataset/test/subject_test.txt", col.names = "subject")
x_test <- read.table("UCI HAR Dataset/test/X_test.txt", col.names = features$functions)
y_test <- read.table("UCI HAR Dataset/test/y_test.txt", col.names = "code")
subject_train <- read.table("UCI HAR Dataset/train/subject_train.txt", col.names = "subject")
x_train <- read.table("UCI HAR Dataset/train/X_train.txt", col.names = features$functions)
y_train <- read.table("UCI HAR Dataset/train/y_train.txt", col.names = "code")

# view data
head(features)
head(activities)
head(subject_test)
head(x_test)
head(y_test)

# Merges the training and the test sets to create one data set.
df_x <- rbind(x_test, x_train)
df_y <- rbind(y_test, y_train)
df_sub <- rbind(subject_train, subject_test)
df_merged <- cbind(df_sub, df_y, df_x)
head(df_merged)
# Extracts only the measurements on the mean and standard deviation for each measurement. 
mean_col <- grep("[Mm]ean", colnames(df_merged))
std_col <- grep("std", colnames(df_merged))
length(c(mean_col, std_col))

df_select <- df_merged[, c(1, 2, mean_col, std_col)] 
ncol(df_select) # 88 columns

# Uses descriptive activity names to name the activities in the data set

df_select$code <- activities[df_select$code, 2]

df_select$code

# Appropriately labels the data set with descriptive variable names.
names(df_select)[2] = "activity"
names(df_select)<-gsub("Acc", "Accelerometer", names(df_select))
names(df_select)<-gsub("Gyro", "Gyroscope", names(df_select))
names(df_select)<-gsub("BodyBody", "Body", names(df_select))
names(df_select)<-gsub("Mag", "Magnitude", names(df_select))
names(df_select)<-gsub("^t", "Time", names(df_select))
names(df_select)<-gsub("^f", "Frequency", names(df_select))
names(df_select)<-gsub("tBody", "TimeBody", names(df_select))
names(df_select)<-gsub("-mean()", "Mean", names(df_select), ignore.case = TRUE)
names(df_select)<-gsub("-std()", "STD", names(df_select), ignore.case = TRUE)
names(df_select)<-gsub("-freq()", "Frequency", names(df_select), ignore.case = TRUE)
names(df_select)<-gsub("angle", "Angle", names(df_select))
names(df_select)<-gsub("gravity", "Gravity", names(df_select))

head(df_select)

# From the data set in step 4, creates a second, independent tidy data set 
# with the average of each variable for each activity and each subject.

df_2 <- df_select |> 
  group_by(subject, activity) |> 
  summarise_all(mean)

write.table(df_2, "Result.txt", row.name=FALSE)

str(df_2)
View(df_2)


### Review criteria

# 1. The submitted data set is tidy. 
# 2. The Github repo contains the required scripts.
# 3. GitHub contains a code book that modifies and updates the available codebooks with the data to indicate all the variables and summaries calculated, along with units, and any other relevant information.
# 4. The README that explains the analysis files is clear and understandable.
# 5. The work submitted for this project is the work of the student who submitted it.








