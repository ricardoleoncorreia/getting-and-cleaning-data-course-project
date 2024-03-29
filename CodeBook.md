# Johns Hopkins University, Getting and Cleaning Data course project

## Ricardo Leon

## www.ricardoleoncorreia.com


Expected outputs
================

* Train and test sets merged as one tidy dataset (named tidy_dataset.txt)
* Summary tidy dataset with averages grouped by subject and activity (named summary_dataset.txt)


Feature Selection (modified version of original features_info.txt)
==================================================================

The features selected for this database come from the accelerometer and gyroscope 3-axial raw signals TimeAcc-XYZ and TimeGyro-XYZ. These time domain signals were captured at a constant rate of 50 Hz. Then they were filtered using a median filter and a 3rd order low pass Butterworth filter with a corner frequency of 20 Hz to remove noise. Similarly, the acceleration signal was then separated into body and gravity acceleration signals (TimeBodyAcc-XYZ and TimeGravityAcc-XYZ) using another low pass Butterworth filter with a corner frequency of 0.3 Hz. 

Subsequently, the body linear acceleration and angular velocity were derived in time to obtain Jerk signals (tBodyAccJerk-XYZ and tBodyGyroJerk-XYZ). Also the magnitude of these three-dimensional signals were calculated using the Euclidean norm (tBodyAccMag, tGravityAccMag, tBodyAccJerkMag, tBodyGyroMag, tBodyGyroJerkMag). 

Finally a Fast Fourier Transform (FFT) was applied to some of these signals producing FrequencyBodyAcc-XYZ, FrequencyBodyAccJerk-XYZ, FrequencyBodyGyro-XYZ, FrequencyBodyAccJerkMag, FrequencyBodyGyroMag, FrequencyBodyGyroJerkMag.

These signals were used to estimate variables of the feature vector for each pattern:  
'-XYZ' is used to denote 3-axial signals in the X, Y and Z directions.

The set of variables that were estimated from these signals are: 

mean: Mean value
std: Standard deviation

The complete list of variables of each feature vector is available in 'features.txt'


Data Source
===========

* Original source: http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones
* Project source: https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip


Data process
============

There are 5 main steps that were folloed during the processing:

1. Merge the training and the test sets to create one data set
2. Extract only the measurements on the mean and standard deviation for each measurement
3. Uses descriptive activity names to name the activities in the data set
4. Appropriately labels the data set with descriptive variable names
5. From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject

The detail for each step is described below

### Merge the training and the test sets to create one data set

1. Download all the files
2. Read X_train data
3. Read y_train data and set "y" as column name
4. Read subject_train data and set "subject" as column name
5. Merge previous three training datasets
6. Read X_test data
7. Read y_test data and set "y" as column name
8. Read subject_test data and set "subject" as column name
9. Merge previous three test datasets
10. Append test set below the training set to create only one dataset

### Extract only the measurements on the mean and standard deviation for each measurement

1. Load features from features.txt
2. Look for column names that has "mean" or "std" in it
3. Save resulting names in new_features.txt file (in root folder)
4. Select from the dataset all columns of interest

### Use descriptive activity names to name the activities in the data set

1. Load activities from activity_labels.txt
2. Merge activities with dataset by "y" column
3. Remove "y" column from resulting dataset

### Appropriately labels the data set with descriptive variable names

1. Get a vector with requested features and add "activity" and "subject" labels
2. Name columns
3. Reorder columns
4. Save data in tidy_dataset.txt

### From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject

1. Group data by "activity" and "subject"
2. Get all mean values by groups
3. Save data in summary_dataset.txt


Helper Functions (extract.R)
============================

check_downloaded_data <- if data doesn't exist in root folder, execute download

download_data <- download data from url


Helper Functions (transform.R)
==============================

get_mean_and_std_columns <- return boolean vector with coincidences for 'mean' and 'std'

save_in_file <- save data in txt file

extract_std_and_mean <- return dataset only with mean and std columns and save selected names in txt file

name_activities <- substitute activities names in main dataset

makeVariablesNamesUserFriendly <- make variables user friendly

set_variable_names <- set column names


Helper Functions (load.R)
=========================

get_data_from_path <- load data from specified path

get_sub_dataset <- load training/test set

get_full_dataset <- merge all training/test data

get_features <- load features from file

get_activities <- load activities from file
