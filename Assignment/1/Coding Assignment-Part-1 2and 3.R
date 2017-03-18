directory <- ("./C:/Users/117053/Downloads/Mooc_Coursera/R_Coursera/week-2/Assign-2_Data/specdata/")
#Part-1 for week-2 assigment 
pollutantmean <- function(directory, pollutant = "sulfate", id = 1:332) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'pollutant' is a character vector of length 1 indicating
  ## the name of the pollutant for which we will calculate the
  ## mean; either "sulfate" or "nitrate".
  
  ## 'id' is an integer vector indicating the monitor ID numbers
  ## to be used
  
  ## Return the mean of the pollutant across all monitors list
  ## in the 'id' vector (ignoring NA values)
  
  # set working directory
  if(grep("specdata", directory) == 1) {
    directory <- ("./specdata/")
  }
  # initialize a vector to hold the pollutant data
  mean_vector <- c()
  # find all files in the specdata folder
  all_files <- as.character( list.files(directory) )
  file_paths <- paste(directory, all_files, sep="")
  for(i in id) {
    current_file <- read.csv(file_paths[i], header=T, sep=",")
    head(current_file)
    pollutant
    na_removed <- current_file[!is.na(current_file[, pollutant]), pollutant]
    mean_vector <- c(mean_vector, na_removed)
  }
  result <- mean(mean_vector)
  return(round(result, 3)) 
}


#OR 
pollutantmean<-function(directory,pollutant,id=1:332){
  #create a list of files
  filesD<-list.files(directory,full.names = TRUE)
  #create an empty data frame
  dat <- data.frame()
  
  #loop through the list of files until id is found
  for(i in id){
    #read in the file
    temp<- read.csv(filesD[i],header=TRUE)
    #add files to the main data frame
    dat<-rbind(dat,temp)
  }
  #find the mean of the pollutant, make sure you remove NA values
  return(mean(dat[pollutant],na.rm = TRUE))
  
}














#Part-2
complete <- function(directory, id = 1:332) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'id' is an integer vector indicating the monitor ID numbers
  ## to be used
  
  ## Return a data frame of the form:
  ## id nobs
  ## 1  117
  ## 2  1041
  ## ...
  ## where 'id' is the monitor ID number and 'nobs' is the
  ## number of complete cases
  
  # set working directory
  if(grep("specdata", directory) == 1) {
    directory <- ("./specdata/")
  }
  # get the length of id vector
  id_len <- length(id)
  complete_data <- rep(0, id_len)
  # find all files in the specdata folder
  all_files <- as.character( list.files(directory) )
  file_paths <- paste(directory, all_files, sep="")
  j <- 1 
  for (i in id) {
    current_file <- read.csv(file_paths[i], header=T, sep=",")
    complete_data[j] <- sum(complete.cases(current_file))
    j <- j + 1
  }
  result <- data.frame(id = id, nobs = complete_data)
  return(result)
} 

#OR
complete <- function(directory,id=1:332){
  
  #create a list of files
  filesD<-list.files(directory,full.names = TRUE)
  #create an empty data frame
  dat <- data.frame()
  
  for(i in id){
    #read in the file
    temp<- read.csv(filesD[i],header=TRUE)
    #delete rows that do not have complete cases
    temp<-na.omit(temp)
    
    #count all of the rows with complete cases
    tNobs<-nrow(temp)
    
    #enumerate the complete cases by index
    dat<-rbind(dat,data.frame(i,tNobs))
    
  }
  return(dat)
}
























#Part-3
corr <- function(directory, threshold = 0) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'threshold' is a numeric vector of length 1 indicating the
  ## number of completely observed observations (on all
  ## variables) required to compute the correlation between
  ## nitrate and sulfate; the default is 0
  
  ## Return a numeric vector of correlations
  
  if(grep("specdata", directory) == 1) {
    directory <- ("./specdata/")
  }
  # get the complete table
  complete_table <- complete("specdata", 1:332)
  nobs <- complete_table$nobs
  # find the valid ids
  ids <- complete_table$id[nobs > threshold]
  # get the length of ids vector
  id_len <- length(ids)
  corr_vector <- rep(0, id_len)
  # find all files in the specdata folder
  all_files <- as.character( list.files(directory) )
  file_paths <- paste(directory, all_files, sep="")
  j <- 1
  for(i in ids) {
    current_file <- read.csv(file_paths[i], header=T, sep=",")
    corr_vector[j] <- cor(current_file$sulfate, current_file$nitrate, use="complete.obs")
    j <- j + 1
  }
  result <- corr_vector
  return(result)   
}

#OR

corr<-function(directory,threshold=0){
  #create list of file names
  filesD<-list.files(directory,full.names = TRUE)
  
  #create empty vector
  dat <- vector(mode = "numeric", length = 0)
  
  for(i in 1:length(filesD)){
    #read in file
    temp<- read.csv(filesD[i],header=TRUE)
    #delete NAs
    temp<-temp[complete.cases(temp),]
    #count the number of observations
    csum<-nrow(temp)
    #if the number of rows is greater than the threshold
    if(csum>threshold){
      #for that file you find the correlation between nitrate and sulfate
      #combine each correlation for each file in vector format using the concatenate function 
      #since this is not a data frame we cannot use rbind or cbind
      dat<-c(dat,cor(temp$nitrate,temp$sulfate))
    }
    
  }
  
  return(dat)
}
