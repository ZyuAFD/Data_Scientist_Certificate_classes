# Function to calculate the correlation between sulfate and nitrate 
# for specified monitor files


corr <- function(directory, threshold = 0) {
   
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  # directory='E:\\Classes\\Data Scientist Certificate\\R Programming\\Week 2\\Assignment\\specdata'
      id=1:332
      readsinglefile <- function(id,directory) {
            id=sprintf("%03d", id)
            filepath=file.path(directory,paste(id,'.csv',sep=''))
            MonData=read.csv(filepath,sep=',',header=T)
            orbs=length(which(complete.cases(MonData)==T))
            if (orbs>threshold) {
                  return(cor(MonData$nitrate,MonData$sulfate,use= "complete.obs"))
            }
      }
  
  
  ## 'threshold' is a numeric vector of length 1 indicating the
  ## number of completely observed observations (on all
  ## variables) required to compute the correlation between
  ## nitrate and sulfate; the default is 0
  
  ## Return a numeric vector of correlations
  corD=unlist(mapply(readsinglefile,id,directory))
  return(as.numeric(corD))
}
