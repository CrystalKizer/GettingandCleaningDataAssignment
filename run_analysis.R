# downloading data for final assignment
library(dplyr)

dir.create("./data")
fileUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"

download.file(fileUrl,destfile="./data/Dataset.zip") #Download file
unzip(zipfile="./data/Dataset.zip",exdir="./data")  #unzip file

# Reading training tables and assign column names
  x_train <- read.table("./data/UCI HAR Dataset/train/X_train.txt", col.names = features$functions)  
  y_train <- read.table("./data/UCI HAR Dataset/train/y_train.txt",col.names = "ActivityNumber")
  subject_train <- read.table("./data/UCI HAR Dataset/train/subject_train.txt", col.names = "Subject")

# Reading testing tables and assign column names
  x_test <- read.table("./data/UCI HAR Dataset/test/X_test.txt", col.names = features$functions)
  y_test <- read.table("./data/UCI HAR Dataset/test/y_test.txt", col.names = "ActivityNumber")
  subject_test <- read.table("./data/UCI HAR Dataset/test/subject_test.txt", col.names = "Subject")

# Reading features and assign column names
  features <- read.table("./data/UCI HAR Dataset/features.txt", col.names = c("n","functions"))

# Reading activity labels and assign column names
  activityLabels = read.table("./data/UCI HAR Dataset/activity_labels.txt", col.names = c("ActivityNumber", "ActivityType"))



# Step 1: Merges the training and the test sets to create one data set.
  x_data<-rbind(x_test, x_train)  # combines x data
  y_data<-rbind(y_test, y_train)   # combines y data
  subject_data<-rbind(subject_train, subject_test) # combines subject data
 
   # merges all three training and test sets into one final data set 
  Merge_all_data<-cbind(subject_data, y_data, x_data)  

#Step2: 2.Extracts only the measurements on the mean and standard deviation for each measurement. 
  # selects data from combined data set with only mean and std devcolumns
  mean_and_std  <- Merge_all_data %>% select(Subject, ActivityNumber, contains("mean"), contains("std"))  


# Step 3: 3.Uses descriptive activity names to name the activities in the data set
  # replaces activity number with the activity name from acitivitylabels to make more descriptive name
  mean_and_std$ActivityNumber <-activityLabels[mean_and_std$ActivityNumber, 2] 




#step4: 4.Appropriately labels the data set with descriptive variable names. 
  names(mean_and_std)<-gsub("Acc", "Accelerometer", names(mean_and_std))  # takes columns with Acc in and renames to Acceleromater
  names(mean_and_std)<-gsub("Gyro", "Gyroscope", names(mean_and_std))   # takes columns with gyrp in and renames to Gyroscope
  names(mean_and_std)<-gsub("BodyBody", "Body", names(mean_and_std))   # takes columns with body repeated and cleans it to body
  names(mean_and_std)<-gsub("Mag", "Magnitude", names(mean_and_std))    # takes columns with mag in and renames to Magnitude
  names(mean_and_std)<-gsub("^t", "Time", names(mean_and_std))     # takes columns with ^t in and renames to Time
  names(mean_and_std)<-gsub("^f", "Frequency", names(mean_and_std))   # takes columns with ^f in and renames to Frequency
  names(mean_and_std)<-gsub("mean", "Mean", names(mean_and_std), ignore.case = TRUE) # renames Mean
  names(mean_and_std)<-gsub("std", "STD_DEVIATION", names(mean_and_std), ignore.case = TRUE) # names std to STD_DEVIATION
  names(mean_and_std)<-gsub("-freq()", "Frequency", names(mean_and_std), ignore.case = TRUE) # renames freq to Frequency
  names(mean_and_std)<-gsub("angle", "Angle", names(mean_and_std)) #renames/capitalizes angle
  names(mean_and_std)<-gsub("gravity", "Gravity", names(mean_and_std))   #renames/capitalizes gravity

#5.From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.
# creates the final independent tidy data with the average for each subject and activity
  TidyData<-mean_and_std %>%
    group_by(Subject, ActivityNumber) %>%
    summarize_all(list(mean))

# creates Txt file called Tidydata
write.table(TidyData, "TidyData.txt", row.names = FALSE)
