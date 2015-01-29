# Function to calculate the mean of specified pollutant in specified monitor files

pollutantmean <- function(directory, pollutant, id = 1:332) {
  ## Test Parameters
  #directory='E:\\Classes\\Data Scientist Certificate\\R Programming\\Week 2\\Assignment\\specdata'
  #id=1:10
  #pollutant='sulfate'
  
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  
  readsinglefile <- function(directory, pollutant, id) {
        id=sprintf("%03d", id)
        filepath=file.path(directory,paste(id,'.csv',sep=''))
        MonData=read.csv(filepath,sep=',',header=T)
        MonData=MonData[,pollutant]
        MonData[!is.na(MonData)]
  }
  
  
  ## 'pollutant' is a character vector of length 1 indicating
  ## the name of the pollutant for which we will calculate the
  ## mean; either "sulfate" or "nitrate".
  
  
  ## 'id' is an integer vector indicating the monitor ID numbers
  ## to be used
  
  ## Return the mean of the pollutant across all monitors list
  ## in the 'id' vector (ignoring NA values)
  PollData=unlist(mapply(readsinglefile,directory,pollutant,id))
  return(mean(PollData)) 
}
