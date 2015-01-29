## Function to get the best hospital based on lowest 30-day death rate of specified outcome and state
best <- function(state, outcome) {
      ## Read outcome data
      Data = read.csv("outcome-of-care-measures.csv", colClasses = "character")
      
      ## Check that state and outcome are valid
      if (!(state %in% unique(Data$State))) stop('invalid state')
      if (length(grep(gsub(' ','.',outcome),colnames(Data),ignore.case = T))==0) stop('invalid outcome')
      
      ## Return hospital name in that state with lowest 30-day death
      ## rate
      Data=Data[Data$State==state,c(2,grep(gsub(' ','.',outcome),colnames(Data),ignore.case = T)[1])]
      Data=Data[which(Data[,2]!='Not Available'),]
      Data[,2]=as.numeric(Data[,2])
      Order = order(Data[,2],Data[,1])      
      return(Data[Order[1],1])
}
