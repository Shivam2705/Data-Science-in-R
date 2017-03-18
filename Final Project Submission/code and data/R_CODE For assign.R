#Part-1: BEST
  
  
  best <- function(state, outcome) {
  ## Read outcome data
  ## Check that state and outcome are valid
  ## Return hospital name in that state with lowest 30-day death
  ## rate
  
  valid_outcomes <- c("heart attack","heart failure","pneumonia")
  
  
  # Check if the outcome is a valid one
  if(outcome %in% valid_outcomes)
  {
    file <- read.csv(file="outcome-of-care-measures.csv",colClasses = "character")
    filterData <- file[c(2, 7, 11, 17, 23)]
    # 2  -> Hospital Name
    # 7  -> State abv.
    # 11 -> outcome = Heart Attack
    # 17 -> outcome = Heart Failure
    # 23 -> outcome = Pneumonia
    # set readable col names
    names(filterData) <- c("name","state","heart attack","heart failure","pneumonia")
    
    #Check if valid state
    if (state %in% filterData[,"state"]) 
    {       # get where the valid rows with the information about the given state       
      validData <- filterData[,"state"] == state   &   filterData[,outcome] != "Not Available"              
      minPos <- which.min(filterData[,outcome][validData])   # give the position of the lower outcome rate
      name <- filterData[,"name"][validData][minPos]         # get hospital name
    } else stop("invalid state")       
  }
  else stop("invalid outcome")
  }
  
  
  
  
  
  
  
  
  #PART:2 : RANKHOSPITAL
  
  rankhospital <- function(state, outcome, num = "best"){
    
    ## Read outcome data
    ## Check that state and outcome are valid
    ## Return hospital name in that state with the given rank
    ## 30-day death rate
    
    valid_outcomes <- c("heart attack","heart failure","pneumonia")
    
    
    # Check if the outcome is a valid one
    if(outcome %in% valid_outcomes)
    {
      file <- read.csv(file="outcome-of-care-measures.csv",colClasses = "character")
      filterData <- file[c(2, 7, 11, 17, 23)]
      # 2  -> Hospital Name
      # 7  -> State abv.
      # 11 -> outcome = Heart Attack
      # 17 -> outcome = Heart Failure
      # 23 -> outcome = Pneumonia
      # set readable col names
      #http://www.dummies.com/how-to/content/how-to-sort-data-frames-in-r.html
      names(filterData) <- c("name","state","heart attack","heart failure","pneumonia")
      
      #Check if valid state
      if (state %in% filterData[,"state"]) 
      {       # get where the valid rows with the information about the given state       
        dFrameFilter <- filterData[filterData$state==state & filterData[outcome] != "Not Available", ]
        
        if(is.numeric(num) & nrow(dFrameFilter) <= num) 
        {
          NA
        }
        else
        {       #getting correct order first by rate. In case of ties the name is use.
          index <- order(dFrameFilter$name) # order by name
          dFrameFilter <- dFrameFilter[index,]
          index <- order(as.numeric(dFrameFilter[,outcome])) # order by outcome
          
          if(is.numeric(num))     dFrameFilter[index,][,"name"][num]
          else if(num == "best")  dFrameFilter[index,][,"name"][1]
          else                    tail(dFrameFilter[index,], n=1)[,"name"] # worst
        }
      }
      else stop("invalid state")   
    }
    else stop("invalid outcome")   
  }
  
  
  
  
  
  
  
  
  #Part-3: RANKALL
  
  
  
  rankall <- function(outcome, num = "best"){
    
    ## Read outcome data
    ## Check that state and outcome are valid
    ## For each state, find the hospital of the given rank
    ## Return a data frame with the hospital names and the
    ## (abbreviated) state name
    
    
    valid_outcomes <- c("heart attack","heart failure","pneumonia")
    
    
    # Check if the outcome is a valid one
    if(outcome %in% valid_outcomes)
    {
      file              <- read.csv(file="outcome-of-care-measures.csv",colClasses = "character")
      filterData        <- file[c(2, 7, 11, 17, 23)]
      names(filterData) <- c("name","state","heart attack","heart failure","pneumonia")
      dFrameFilter      <- filterData[filterData[outcome] != "Not Available", ] # get where the valid rows with the information 
      
      #getting correct order first by rate. In case of ties the name is use.
      index        <- order(dFrameFilter$name)                   # order by name
      dFrameFilter <- dFrameFilter[index,]
      index        <- order(as.numeric(dFrameFilter[,outcome])) # order by outcome
      orderData    <- dFrameFilter[index,]                      # The dataFrame ordered by outcome and name
      allStates    <- unique(dFrameFilter[,"state"])            # a vector with all unique states
      allStates    <- allStates[order(allStates)]               # order the vector of unique states
      
      output <- data.frame()
      for(state in allStates) # For all state in the unique list of states
      {
        getStateData <- orderData[orderData$state == state, c(1,2)] #get State data (name,state)
        if(is.numeric(num) & nrow(getStateData) <= num) 
        {
          name <- NA
        }
        else
        {       
          if(is.numeric(num))      name   <- getStateData[,"name"][num]
          else if(num == "best")   name   <- getStateData[,"name"][1]
          else                     name   <- tail(getStateData, n=1)[,"name"] #worst
        }
        newRow <- data.frame(name,state)       #create row to be added
        names(newRow) <- names(output)
        output <- rbind(output,newRow)
      } # for(state in allStates)
      
      names(output) <- c("hospital","state")
      output
    }
    else stop("invalid outcome")   
  }