read.data <- function(type) {
      
      # read the column names from features.txt file
      # get the column index contains 'mean()' or 'std()'
      colnm       =read.table('./UCI HAR Dataset/features.txt',stringsAsFactors=F)
      colind=grep('mean\\(\\)|std\\(\\)',tolower(colnm[,2]))
      
      # read x data
      filenm=paste('x_',type,'.txt',sep='')
      filepath=file.path('./UCI HAR Dataset',type,filenm)
      dat   =read.table(filepath)[,colind]
      names(dat)  =colnm[colind,2]
      
      # read subject data
      filenm=paste('subject_',type,'.txt',sep='')
      filepath=file.path('./UCI HAR Dataset',type,filenm)
      subject.temp =read.table(filepath)
      dat=cbind(dat,subject=subject.temp[,1])
      
      # read y data
      filenm=paste('y_',type,'.txt',sep='')
      filepath=file.path('./UCI HAR Dataset',type,filenm)
      y.temp =read.table(filepath)
      dat=cbind(dat,actID=y.temp[,1])
      
      # name activity
      activity.Desp    =read.table('./UCI HAR Dataset/activity_labels.txt')
      names(activity.Desp)=c('actID','activity')
      dat=merge(dat,activity.Desp,by='actID',all.x=T)
      dat=dat[,-grep('actID',names(dat))]
      
      return(dat)
}

# combine test and train data and calculate colMeans
library(data.table)

dat=data.table(rbind(read.data('train'),read.data('test')))

results=dat[,lapply(.SD,mean),by=list(activity,subject)]
write.csv(results,'tidydata.csv',row.names=F,quote=F)
