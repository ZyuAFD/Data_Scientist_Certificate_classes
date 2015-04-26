---
title: "Practical Machine Learning Project"

output: html_document
---

### Background
This is the report of the project for practical machine learning course from John Hopkins University. The purpose of this project is to address the following question.

In the aforementioned study, six participants participated in a dumbell lifting exercise five different ways. The five ways, as described in the study, were “exactly according to the specification (Class A), throwing the elbows to the front (Class B), lifting the dumbbell only halfway (Class C), lowering the dumbbell only halfway (Class D) and throwing the hips to the front (Class E). Class A corresponds to the specified execution of the exercise, while the other 4 classes correspond to common mistakes.”

By processing data gathered from accelerometers on the belt, forearm, arm, and dumbell of the participants in a machine learning algorithm, the question is can the appropriate activity quality (class A-E) be predicted?

Libraries used in this project are first loaded.
```{r Environment,message=FALSE}
libs=c('data.table','caret','dplyr','corrplot','rattle','rpart.plot')
lapply(libs,library,character.only=T)
```

### Data Load and Processing
Data is first imported from web portal with proper NA value specified. As the positions of belt, arm, forearm, dumbbell of different people could be different even doing the same activity, the classification should not only have the information of activity class but also users. 
```{r Load Data and combine class and user information,message=FALSE}
test=fread('https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv',na.strings=c('','#DIV/0!','NA'))
train=fread('https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv',na.strings=c('','#DIV/0!','NA'))
# Combine name and class and convert into factor
train[,UserClass:=as.factor(paste(user_name,classe))]
```

#### Clean and Transform Data
Variables with NA values will be eliminated from the model and all predictors are transformed into numeric data type for further analysis. The training data is split into train set, **75%**, and crossvalidation set, **25%**.
```{r}
# columns that are has NA
NaCol=train[,sapply(.SD,function(x) any(is.na(x)))]

## Train data
Train_clean=train %>%   # Combine name and class and convert into factor
  names %>%
  grep('belt|arm|forearm|dumbbell',.) %>%         #Get all name indexes of measurement variables 
  intersect(.,as.numeric(which(NaCol==F)))  %>%   #Remove the NA variables  
  train[,.,with=F] %>%                
  sapply(.,as.numeric) %>%                        #Convert all measurement variables into numeric
  data.frame() 

Train_clean$UserClass=train$UserClass  #Include the result column

## Test data
Test_clean=test %>%   # Combine name and class and convert into factor
  names %>%
  grep('belt|arm|forearm|dumbbell',.) %>%         #Get all name indexes of measurement variables 
  intersect(.,as.numeric(which(NaCol==F)))  %>%   #Remove the NA variables  
  test[,.,with=F] %>%
  sapply(.,as.numeric) %>%                        #Convert all measurement variables into numeric
  data.frame() 

Test_clean$User=test$user_name
Test_clean$Problem_id=test$problem_id

# Training data split
set.seed(3721)

inTrain = createDataPartition(Train_clean$UserClass, p = 3/4, list=FALSE)
training = Train_clean[inTrain,]
crossValidation = Train_clean[-inTrain,]

ncol(Train_clean)
```
After cleaning the NA variables, there remain 52 variables and a user class column.

### Feature Selection
To build a model for predict the class from many variables, variables with redundent information should be removed from the model. The correlation across all remaining variables is calcuated below after scaling. The variables with correlation above 75% are considered redundent and removed. The correlation plot is displayed after redundence removal.
```{r Variable selection}
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


training_sim=training[,-highCor]
crossValidation_sim=crossValidation[,-highCor]
Test_clean_sim=Test_clean[,-highCor]

length(highCor)
```
After the analysis of variable correlations, another 21 variables are moved from modeling.

### Modeling
A pre-processing is conducted before building the predict model. BoxCox, scale and principle component analysis is used to transform and reduce the variables using training data. This process is then applied to training, crossvalidation and test data sets.
```{r Preprocessing}
trans = preProcess(training_sim[,-grep('UserClass',names(training_sim))], 
                   method=c("BoxCox", "center", 
                            "scale", "pca"))

# Get the principle component for training, crossvalidation and test set
train_PC = predict(trans, training_sim[,-grep('UserClass',names(training_sim))])
train_PC$UserClass=training_sim$UserClass

crossValidation_PC = predict(trans, crossValidation_sim[,-grep('UserClass',names(crossValidation_sim))])
crossValidation_PC$UserClass=as.character(crossValidation_sim$UserClass)

test_PC = predict(trans, Test_clean_sim[,1:31])
test_PC$Problem_id=Test_clean_sim$Problem_id
test_PC$User=Test_clean_sim$User

```

#### Decision Tree
The decision tree model only returns 44.4% accuracy.
```{r Decision tree}
Dtree <- rpart(UserClass ~ ., 
               data=train_PC, 
               method="class")


predDtree <- predict(Dtree, 
                     crossValidation_PC[,-grep('UserClass',names(crossValidation_PC))], 
                     type="class")

confusionMatrix(predDtree, crossValidation_PC$UserClass)$overall
```
#### Random Forest
```{r Random Forest,eval=FALSE}

Model=train(UserClass~.,
            data=train_PC,
            method = "rf",
            prox=T,
            ntree=200)

Pred=predict(Model,
             crossValidation_PC[,-grep('UserClass',names(crossValidation_sim))])
confusionMatrix(Pred, crossValidation_PC$UserClass)$overall

```
Random Forests yielded better Results, as expected!

### Predict and Generate Files for submission

Finally, using the provided Test Set out-of-sample error.

For Random Forests we use the following formula, which yielded a much better prediction in in-sample:
```{r Submission File}
predresult <- predict(Model, test_PC, type = "class")
test_PC$Pred=predresult
test_PC=data.table(test_PC)
test_PC[,UserPred:=substr(Pred,1,nchar(Pred)-2)]
confusionMatrix(test_PC$UserPred, test_PC$User)$overall

pml_write_files = function(x){
  n = length(x)
  for(i in 1:n){
    filename = paste0("problem_id_",i,".txt")
    write.table(x[i],file=filename,quote=FALSE,row.names=FALSE,col.names=FALSE)
  }
}

pml_write_files(predictionsB2)
```

