# Merges the training and the test sets to create one data set.
init = read.table("./UCI HAR Dataset/train/X_train.txt", nrows = 10)
classes = sapply(init, class)
xtrain = read.table("./UCI HAR Dataset/train/X_train.txt", stringsAsFactors=F, colClasses=classes)
ytrain = read.table("./UCI HAR Dataset/train/y_train.txt", stringsAsFactors=F)
subjectTrain = read.table("./UCI HAR Dataset/train/subject_train.txt", stringsAsFactors=F)

xtest = read.table("./UCI HAR Dataset/test/X_test.txt", stringsAsFactors=F, colClasses=classes)
ytest = read.table("./UCI HAR Dataset/test/y_test.txt", stringsAsFactors=F)
subjectTest = read.table("./UCI HAR Dataset/test/subject_test.txt", stringsAsFactors=F)

X = rbind(xtrain, xtest)
y = rbind(ytrain, ytest)
subjects = rbind(subjectTrain, subjectTest)

# Extracts only the measurements on the mean and standard deviation for each measurement.
features = read.table("./UCI HAR Dataset/features.txt", stringsAsFactors=F)
features = features[,2]
meanAndStdCol = grep("mean\\(\\)|std\\(\\)", features)
X = X[,meanAndStdCol]

# process feature names
features = features[meanAndStdCol]
features = gsub("BodyBody", "Body", features)
features = gsub("\\(\\)", "", features)
features = gsub("-","",features)
# Put in underscores to separate different variables
features = gsub(
    "^(t|f)(Body|Gravity)(Acc|Gyro)(Jerk|JerkMag|Mag)?(mean|std)([:alpha:]*)",
    "\\1_\\2_\\3_\\5_\\4\\6",features)

# label variables
names(X) = features

# Uses descriptive activity names to name the activities in the data set
activityLabels = read.table("./UCI HAR Dataset/activity_labels.txt", stringsAsFactors=F)
activityLabels = tolower(activityLabels[,2])
activityLabels = sub("_", " ", activityLabels)
temp = as.factor(y[,1])
levels(temp) = activityLabels # give names to variables
y[,1] = temp
names(y) = "Activity"
names(subjects) = "SubjectID"
X = cbind(subjects,y,X) # join everything together

require(dplyr)
require(tidyr)
X = tbl_df(X) # convert to data table
X = X %>% gather(M,Value,-SubjectID,-Activity) %>%
    separate(M,
             c("Domain","OriginOfEffect","Source","Statistic","Type"),
             sep="_",
             remove=T) %>%
    mutate(Domain = ifelse(Domain=="t","Time","Frequency")) %>%
    mutate(Source = ifelse(Source=="Acc","Accelerometer","Gyroscope"))
# X is now tidy

# From the data set in step 4, creates a second, independent tidy data set 
# with the average of each variable for each activity and each subject.
X2 = X %>%
    group_by(SubjectID,Activity,Domain,OriginOfEffect,Source,Statistic,Type) %>%
    summarise(Average = mean(Value))

write.table(X2,"TidyData.txt",row.names=F)