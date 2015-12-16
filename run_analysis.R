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
 
  #Create vector with the name of this dataset
  datasetNames<-data.frame(dataset=rep(datasetName, dim(features)[1]))
  
  #Create the full data frame
  data<- cbind(dataset=datasetNames, activity=activities,subjects,features) 
  data
}

##First, read in features)
dataDir <- "UCI HAR Dataset"
training <- readData(dataDir, "train")
test <- readData(dataDir, "test")

#Merge the data
data <- rbind(training, test)
