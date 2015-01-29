## Function to get the specified rank of hospital based on the lowest 30 day depth rate fo specifed outcome and state
rankhospital <- function(state, outcome, num = "best") {
      ## Read outcome data
      Data = read.csv("outcome-of-care-measures.csv", colClasses = "character")
      
      ## Check that state and outcome are valid
      if (!(state %in% unique(Data$State))) stop('invalid state')
      if (length(grep(gsub(' ','.',outcome),colnames(Data),ignore.case = T))==0) stop('invalid outcome')
      
      ## Return hospital name in that state with the given rank
      ## 30-day death rate
      Data=Data[Data$State==state,c(2,grep(gsub(' ','.',outcome),colnames(Data),ignore.case = T)[1])]
      Data=Data[which(Data[,2]!='Not Available'),]
      Data[,2]=as.numeric(Data[,2])
      Order = order(Data[,2],Data[,1])      
      
      if (num=='best') num=1
      if (num=='worst') num=Order[length(Order)]
      return(Data[Order[num],1])
}
