activities_file <- read.table("./UCI HAR Dataset/activity_labels.txt")                              ## step 1
activities <- as.character(activities_file$V2)
features_file <- read.table("./UCI HAR Dataset/features.txt")
features <- as.character(features_file$V2)
xtrain <- read.table("./UCI HAR Dataset/train/X_train.txt")
colnames(xtrain) <- features
ytrain <- read.table("./UCI HAR Dataset/train/y_train.txt")
xtest <- read.table("./UCI HAR Dataset/test/X_test.txt")
colnames(xtest) <- features
ytest <- read.table("./UCI HAR Dataset/test/y_test.txt")
x <- rbind(xtrain,xtest)
activitynames_file <- rbind(ytrain, ytest)                                 
activitynames <- activitynames_file$V1
subjecttrain <- read.table("./UCI HAR Dataset/train/subject_train.txt")
subjecttest <- read.table("./UCI HAR Dataset/test/subject_test.txt")
subjectnames_file <- rbind(subjecttrain, subjecttest)
subjects <- subjectnames_file$V1                                               
mean <- sapply(x[,1:561], mean)                                                                     ## step 2
sd <- sapply(x[,1:561], sd)
meanAndsd <- rbind(mean, sd)   ## extract the mean and sd of all measurements
for(i in 1:6){
    for(j in 1:length(activitynames)){
        if(activitynames[j] == i)
            activitynames[j] <- activities[i]
    }
}  
newx <- cbind(activitynames, x)                                                                     ## step 3
newx2 <- cbind(subjects, x)                                                                         ## step 4
newx3 <- cbind(activitynames, newx2)    ## this Uses descriptive activity names to name the activities in the data set and appropriately labels the data set with descriptive variable names.                                                           ## step 5
newx4 <- data.frame()
for(i in 1:6){                          ## using a for loop to find all the measurements of a specific activity and subject and then calculate the average(mean) of those measurements 
    for(j in 1:30){
        a <- t(sapply(newx3[newx3$subjects == j & newx3$activitynames == activities[i],3:563],mean))
        b <- cbind(activities[i], j, a)
        newx4 <- rbind(newx4, b)
    }
}                                    
colnames(newx4)[1] <- "activities"
colnames(newx4)[2] <- "subjects"
tidyset <- newx4                       ## in this data frame, the value of each measurement is the average of it of the specific activity and subject 
write.table(tidyset, file = "./tidy_data.txt")