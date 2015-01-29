## Function check the number of complete cases in specified monitor files
complete <- function(directory, id ) {
  ## Test Parameters
  #directory='E:\\Classes\\Data Scientist Certificate\\R Programming\\Week 2\\Assignment\\specdata'
  #id=1:10
  
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
      readsinglefile <- function(id,directory) {
            id=sprintf("%03d", id)
            filepath=file.path(directory,paste(id,'.csv',sep=''))
            MonData=read.csv(filepath,sep=',',header=T)
            return(length(which(complete.cases(MonData)==T)))
      }
 
  ## 'id' is an integer vector indicating the monitor ID numbers
  ## to be used
  
  ## Return a data frame of the form:
  ## id nobs
  ## 1  117
  ## 2  1041
  ## ...
  ## where 'id' is the monitor ID number and 'nobs' is the
  ## number of complete cases
             
  
  results=data.frame(id=id,nobs=mapply(readsinglefile,id,directory))

  return(results)
}
