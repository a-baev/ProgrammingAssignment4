#This file run_analysis.R  does the following: 
#1. Merges the training and the test sets to create one data set.
#2. Extracts only the measurements on the mean and standard deviation for each measurement. 
#3. Uses descriptive activity names to name the activities in the data set
#4. Appropriately labels the data set with descriptive variable names. 
#5. From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.


##### 1* MErging the training and the test data-sets to create one data-set.
#1.1. loading Train data-set:
train_x_df <- read.table('UCI HAR Dataset\\train\\X_train.txt', sep = "")
train_y_df <- read.table('UCI HAR Dataset\\train\\y_train.txt', sep = "")
train_subj_df <- read.table('UCI HAR Dataset\\train\\subject_train.txt', sep = "")
dim(train_x_df) # 7352  561
dim(train_y_df) # 7352    1
dim(train_subj_df) # 7352    1 
#1.2. loading Test data-sets:
test_x_df <-  read.table('UCI HAR Dataset\\test\\X_test.txt', sep = "")
test_y_df <-  read.table('UCI HAR Dataset\\test\\y_test.txt', sep = "")
test_subj_df <- read.table('UCI HAR Dataset\\test\\subject_test.txt', sep = "")
dim(test_x_df) # 2947  561
dim(test_y_df) # 2947    1
dim(test_subj_df) # 2947    1
#1.3. Merging (rbind-ing) Train and Test data-sets:
x_df <- rbind(train_x_df, test_x_df) # <---- x_df contains result of merging Train and Test X-datasets 
y_df <- rbind(train_y_df, test_y_df) # <---- y_df contains result of merging Train and Test y-datasets
subj_df <- rbind(train_subj_df, test_subj_df) # <---- subj_df contains result of merging Train and Test y-datasets
dim(x_df) # 10299   561
dim(y_df) # 10299     1
dim(subj_df) # 10299     1
#1.4 Loading  names of the measurements:
df_features <- read.table("UCI HAR Dataset\\features.txt")
df_features <- df_features$V2
length(df_features) #561
names(x_df) <- df_features
names(y_df) <- "activityID"
names(subj_df) <- "subjectID"
#1.5 merging data into one dataset:
df <- cbind(x_df, y_df, subj_df) 
dim(df) #10299   563 


##### 2* Extracting only the measurements on the mean and standard deviation for each measurement
#2.1. locating columns, associated with mean and std only:
names_all <- names(df)
names_all
loc_m <- grepl(x = names_all, pattern = '-mean\\(\\)'); sum(grepl(x = names_all, pattern = '-mean\\(\\)')) ## 33
loc_s <- grepl(x = names_all, pattern = '-std\\(\\)'); sum(grepl(x = names_all, pattern = '-std\\(\\)')) ## 33
loc_ms <- as.logical(loc_m + loc_s); sum(loc_ms) #66
#2.2. including columns activityID and subjectID in column location:
loc_as <- rep(FALSE, ncol(df)); loc_as[ncol(df)] <- loc_as[ncol(df)-1] <- TRUE
loc_msas <- as.logical(loc_ms + loc_as)
sum(loc_msas) #68
#2.3. locating  columns, associated with mean and std only:
df_mean_std_only <- df[, loc_msas] #<--- Required subset
dim(df_mean_std_only) # 10299    68 


##### 3* Using descriptive activity names to name the activities in the data set
#Getting names activity names:
activity_names <- read.table("UCI HAR Dataset\\activity_labels.txt")
activity_names <- as.vector(activity_names$V2)
#Updating data frame so that activityID column contains descriptive activity names, not a digits.
dfz <- mutate(df_mean_std_only, activityID = activity_names[activityID]) #<--- New data set has been created 


##### 4*  To label the data set with descriptive variable names. 
list1 <- names(dfz)
replacement = function(x){
        x <- gsub(pattern = '^t'     , replacement = 'time'              , x)
        print(x)
        x <- gsub(pattern ='^f'      , replacement = 'frequency'         , x)
        print(x)
        x <- gsub(pattern ='Gyro'    , replacement = 'Gyroscope'         , x)
        x <- gsub(pattern ='Acc'     , replacement = 'Accelerometer'     , x)
        x <- gsub(pattern ='Mag'     , replacement = 'Magnitude'         , x)
        x <- gsub(pattern ='std'     , replacement = 'StandardDeviation' , x)
        x <- gsub(pattern ='BodyBody', replacement = 'Body'              , x)
        x <- gsub(pattern ='gravity' , replacement = 'Gravity'           , x)
        return(x)
}
list2 <- sapply(X = list1, FUN = replacement, USE.NAMES = F) 
names(dfz) <- list2 # <--- assinging descriptive variable names


##### 5* From the data set in step 4,  create a second, 
#independent tidy data set with an average of each variable for each activity 
#and each subject.
library(reshape2)
dim(dfz) # 10299    68
# Melt data:
melt_dfz <- melt(data = dfz, id = c('subjectID', 'activityID'))
dim(melt_dfz)
# Dcast data with mean-function:
tidy_data <- dcast(melt_dfz, activityID + subjectID ~ variable, mean)
tidy_data[,1:3]