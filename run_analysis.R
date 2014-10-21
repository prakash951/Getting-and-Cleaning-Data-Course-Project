rdir <- "./UCI HAR Dataset"

activities <- read.table(paste(rdir,"activity_labels.txt",sep="/"), stringsAsFactors=FALSE, col.names=c("ID","ACTIVITY NAME"))
features <- read.table(paste(rdir,"features.txt",sep="/"), stringsAsFactors=FALSE, col.names=c("Feature ID", "Feature Name"))


test_x <- read.table(paste(rdir,"test", "X_test.txt",sep="/"),stringsAsFactors=FALSE,header=FALSE)
test_y <- read.table(paste(rdir,"test", "y_test.txt",sep="/"),stringsAsFactors=FALSE,col.names=c("ACTIVITY NAME"),header=FALSE)

training_x <- read.table(paste(rdir,"train", "X_train.txt",sep="/"),stringsAsFactors=FALSE,header=FALSE)
training_y <- read.table(paste(rdir,"train", "y_train.txt",sep="/"),stringsAsFactors=FALSE,col.names=c("ACTIVITY NAME"),header=FALSE)

subject_test <- read.table(paste(rdir,"test", "subject_test.txt",sep="/"),stringsAsFactors=FALSE)
subject_train <- read.table(paste(rdir,"train", "subject_train.txt",sep="/"),stringsAsFactors=FALSE) 

#Filter mean and standard deviation
features <- rbind(features[grep("mean()", fixed=T, features[[2]]),], features[grep("std()", fixed=T, features[[2]]),])
features <- features[order(as.numeric(features[,1])),]

#Merges the training and the test sets to create one data set.
data_x <- rbind(test_x,training_x)
data_y <- rbind(test_y,training_y)
data_subject <- rbind(subject_test,subject_train)


idToActivityMapper <- function(raw_acty,activity_labels)
{
	res <- c()
	for(i in raw_acty)
	{
		res <- c(res, activity_labels[i,2])
	}
	res
}

#Extracts only the measurements on the mean and standard deviation for each measurement. 
data_x <- data_x[,as.numeric(features[,1])]

#Uses descriptive activity names to name the activities in the data set
features$Feature.Name <- gsub("-","",gsub("std\\(\\)","StandardDeviation",gsub("mean\\(\\)","Mean",gsub("fB","FrequencyB",gsub("tG","TimeG",gsub("tB","TimeB",features[,2]))))))

#Merge entire data set into a singe data set
tidy_data <- cbind(data_subject,data_y,data_x)
tidy_data$ACTIVITY.NAME <- idToActivityMapper(as.numeric(tidy_data$ACTIVITY.NAME),activities)


#Appropriately labels the data set with descriptive variable names. 
names(tidy_data) <- c("Subject","ActivityName",features[,2])


#Function to implement group by 
grouper <- function(data,activity)
{
	options(digits=20)
    subject <- sort(unique(data$Subject))
	result_frame <- data.frame()
    for(sub1 in subject)
    {
		for(act in activity)
		{
			subset_data <- data[data$Subject==sub1 & data$ActivityName==act,]
			result <- c()
			for(i in 3:ncol(subset_data))
			{
				result <- c(result,mean(subset_data[[i]]))
			}
			print(result)
			print(mean(result))
			row_index <- nrow(result_frame)
			result_frame[row_index+1,1]=sub1
			result_frame[row_index+1,2]=act
			result_frame[row_index+1,3]=mean(result)
        }
    }
	names(result_frame) = c("Subject", "ActivityName", "TotalSampleMean")
	result_frame
}

#From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.
tidy_mean_data <- grouper(tidy_data,activities[[2]])


write.csv(file="complete_tidy_data.csv",tidy_mean_data,row.names=F)
