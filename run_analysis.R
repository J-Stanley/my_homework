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
meanAndsd <- rbind(mean, sd)
for(i in 1:6){
    for(j in 1:length(activitynames)){
        if(activitynames[j] == i)
            activitynames[j] <- activities[i]
    }
}  
newx <- cbind(activitynames, x)                                                                     ## step 3
newx2 <- cbind(subjects, x)                                                                         ## step 4
newx3 <- cbind(activitynames, newx2)                                                                ## step 5
newx4 <- data.frame()
for(i in 1:6){
    for(j in 1:30){
        a <- t(sapply(newx3[newx3$subjects == j & newx3$activitynames == activities[i],3:563],mean))
        b <- cbind(activities[i], j, "mean", a)
        c <- t(sapply(newx3[newx3$subjects == j & newx3$activitynames == activities[i],3:563],sd))
        d <- cbind(activities[i], j, "sd", c)
        newx4 <- rbind(newx4, b)
        newx4 <- rbind(newx4, d)
    }
}
colnames(newx4)[1] <- "activities"
colnames(newx4)[2] <- "subjects"
colnames(newx4)[3] <- "measurements"
tidyset <- newx4
