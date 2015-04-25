libs=c('data.table','caret')
lapply(libs,library,character.only=T)

test=fread('https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv')
train=fread('https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv')

# convert all measurements into numeric informtion
for (i in grep('belt|arm|forearm|dumbbell',names(train))) {train[,i]=as.numeric(train[,i])}
# Combine name and class and convert into factor
train[,UserClass:=paste(user_name,classe)]
train$UserClass=as.factor(train$UserClass)

c(grep('belt|arm|forearm|dumbbell',names(train)),161)
