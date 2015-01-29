## Function to get the specified rank of hospital based on the lowest 
## 30 day depth rate for specified outcome for ALL state
rankall <- function(outcome, num = "best") {
      ## Read outcome data
      Data = read.csv("outcome-of-care-measures.csv", colClasses = "character")
      
      ## Check that outcome is valid
      if (length(grep(gsub(' ','.',outcome),colnames(Data),ignore.case = T))==0) stop('invalid outcome')
      
      ## For each state, find the hospital of the given rank
      ## Return a data frame with the hospital names and the
      ## (abbreviated) state name
      
      
      Data=Data[,c(2,7,grep(gsub(' ','.',outcome),colnames(Data),ignore.case = T)[1])]
      Data=Data[which(Data[,3]!='Not Available'),]
      Data[,3]=as.numeric(Data[,3])
      Data_ST=split(Data,Data$State)
      
      rnkMinMort<- function(x,num) {
            Order=order(x[,3],x[,1])
            if (num=='best') num=1
            if (num=='worst') num=Order[length(Order)]
            x[Order[num],1]
      }
      
      HospRnk=data.frame(hospital=sapply(Data_ST,rnkMinMort,num))
      
      return(data.frame(HospRnk,State=rownames(HospRnk)))
      
      
}
