libs=c('data.table','caret','dplyr','corrplot')
lapply(libs,library,character.only=T)


test=fread('https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv',na.strings=c('','#DIV/0!','NA'))
train=fread('https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv',na.strings=c('','#DIV/0!','NA'))

# Combine name and class and convert into factor
train[,UserClass:=as.factor(paste(user_name,classe))]
# columns that are has NA
NaCol=train[,sapply(.SD,function(x) any(is.na(x)))]

Train_clean=train %>%   # Combine name and class and convert into factor
      names %>%
      grep('belt|arm|forearm|dumbbell',.) %>%         #Get all name indexes of measurement variables 
      intersect(.,as.numeric(which(NaCol==F)))  %>%   #Remove the NA variables  
      train[,.,with=F] %>%                
      sapply(.,as.numeric) %>%                        #Convert all measurement variables into numeric
      data.frame() 

Train_clean$UserClass=train$UserClass  #Include the result column

Test_clean=train[,UserClass:=as.factor(paste(user_name,classe))] %>%   # Combine name and class and convert into factor
      names %>%
      grep('belt|arm|forearm|dumbbell',.) %>%         #Get all name indexes of measurement variables 
      intersect(.,as.numeric(which(NaCol==F)))  %>%   #Remove the NA variables  
      test[,.,with=F] %>%
      sapply(.,as.numeric) %>%                        #Convert all measurement variables into numeric
      data.frame() 


# Training data split
set.seed(3721)

inTrain = createDataPartition(Train_clean$UserClass, p = 3/4, list=FALSE)
training = Train_clean[inTrain,]
crossValidation = Train_clean[-inTrain,]




#Detect highly correlated variables
highCor=training[,-grep('UserClass',names(training))] %>%
      scale(.,center=T,scale=T) %>%                   #Standardize values
      cor(.,use= "pairwise.complete.obs") %>%  #Calculate the correlation matrix
      findCorrelation(.,0.75)   

#Correlation plot
training[,-c(highCor,grep('UserClass',names(training)))] %>%                #Remove the highly correlated variables
      scale(.,center=T,scale=T) %>%                   #Standardize values
      cor(.,use= "pairwise.complete.obs")  %>%
      corrplot(.,method='square',mar=c(0,0,0,0))


Train_clean_sim=training[,-highCor]
crossValidation_sim=crossValidation[,-highCor]
Test_clean_sim=Test_clean[,-highCor]



trans = preProcess(Train_clean_sim[,-grep('UserClass',names(Train_clean_sim))], 
                   method=c("BoxCox", "center", 
                            "scale", "pca"))

train_PC = predict(trans, Train_clean_sim[,-grep('UserClass',names(crossValidation_sim))])
train_PC$UserClass=Train_clean_sim$UserClass

crossValidation_PC = predict(trans, crossValidation_sim[,-grep('UserClass',names(crossValidation_sim))])
crossValidation_PC$UserClass=as.character(crossValidation_sim$UserClass)



Model=train(UserClass~.,data=train_PC,method = "rf",prox=T)
Pred=predict(Model,crossValidation_PC[,-grep('UserClass',names(crossValidation_sim))])
crossValidation_PC$Pred=as.character(Pred)

crossValidation_PC=data.table(crossValidation_PC)
crossValidation_PC[,ClassPred:=substr(Pred,nchar(Pred),nchar(Pred))]
crossValidation_PC[,UserPred:=substr(Pred,1,nchar(Pred)-2)]

crossValidation_PC[,Classe:=substr(UserClass,nchar(UserClass),nchar(UserClass))]
crossValidation_PC[,User:=substr(UserClass,1,nchar(UserClass)-2)]







test
Pred=predict(Model,crossValidation_PC[,-grep('UserClass',names(crossValidation_sim))])





