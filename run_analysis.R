#Will use data.table
library(data.table)

#This function takes the directory name and returns a table 
readData <- function(dataDirectory, datasetName) {
  #Read the feature names
  featureNames<-read.table(file.path(dataDirectory,"features.txt"), header=FALSE, col.names=c("featureIdx","feature"), stringsAsFactors=F)
  
  #Read the activity names
  activityNames <- read.table(file.path(dataDirectory,"activity_labels.txt"),header=FALSE,col.names=c("activityIdx","activity"), stringsAsFactors=F)
  
  
  #Read the subjects file                           
  filename <- file.path(dataDirectory,datasetName,paste("subject_",datasetName,".txt",sep=""))
  if(!file.exists(filename)) {
    print(paste("Couldn't find file:",filename))
    return(NULL)
  }    
  subjects <- read.table(filename, header=F, col.names="subject")
  
  
  #Read the activity file
  filename <- file.path(dataDirectory,datasetName,paste("X_",datasetName,".txt",sep=""))
  
    
  if(!file.exists(filename)) {
        print(paste("Couldn't find file:",filename))
      return(NULL)
    }    
  
  #Should switch to use the big table class
  #features<-read.table(filename,header=F, col.names=featureNames$feature)
  features<-fread(filename, header=F, col.names=featureNames$feature) 
   
  #Read the activity file
  filename <- file.path(dataDirectory,datasetName,paste("y_",datasetName,".txt",sep=""))
  
  
  if(!file.exists(filename)) {
    print(paste("Couldn't find file:",filename))
    return(NULL)
  }    
  activities<-read.table(filename,header=F,col.names="activity")
  
  #Activities are currently represented as integers. Convert them to factors with descriptive names
  activities<-factor(activities$activity,levels=1:length(activityNames$activity), labels=activityNames$activity)
 

  #Create the full data frame
  data<- cbind(activity=activities,subjects,features) 
  data
}

##First, read in features)
dataDir <- "./"
training <- readData(dataDir, "train")
test <- readData(dataDir, "test")

#Merge the data
data <- rbind(training, test)

#Extract only data set, subject, activity, means, and stdevs
logicalVector <- grepl("subject",colNames) | grepl("activity",colNames) | (grepl("mean",colNames) & !grepl("meanFreq",colNames)) | grepl("std",colNames)
desiredColumns <- colNames[logicalVector]
data <- subset(data,,desiredColumns)


#Clean up column names
colNames <- colnames(data)
colNames <- gsub("\\(\\)", "", colNames) #remove parens
colNames <- gsub("-std", "-stdev", colNames) #rename std to stdev
colNames <- gsub("Acc","Acceleration", colNames) #Acc is short for Acceleration
colNames <- gsub("Mag", "Magnitude", colNames) #Mag is short for Magnitude
colNames <- gsub("Gyro", "Gyroscope", colNames) #Gyro is short for Gyroscop
colNames <- gsub("^t","time",colNames) #t at the beginning is short for time
colNames <- gsub("^f", "frequency", colNames) #f at the beginning is short for frequency
colNames <- gsub("BodyBody", "Body", colNames) #some column names have Body twice

colnames(data) <- colNames

#Now need to find the mean by each combination of subject and activity
#tidyData <- aggregate(data[,!(names(data) %in% c("subject","activity"))], 
              #        by=list(subject=data$subject, activity=data$activity), mean)

#Write out the result
#write.table(format(tidyData, scientific=T), "tidyData.tsv", row.names=F, col.names=T)