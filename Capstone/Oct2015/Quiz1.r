setwd('C:\\Users\\Ziwen.Yu\\Documents\\Class\\Capstone\\Data\\Oct 2015\\yelp_dataset_challenge_academic_dataset\\')

#Q1: After untaring the the dataset, how many files are there (including the documentation pdfs)?
list.files(path = ".") # 7

#Q2: The data files are in what format?
#JSON

#Q3: How many lines of text are there in the reviews file (in orders of magnitude)?
library(rjson)
library(dplyr)
review=readLines('yelp_academic_dataset_review.json') 
length(review)  #1569264

#Q4: Consider line 100 of the reviews file. “I’ve been going to the Grab n Eat for almost XXX years”
review[100] # 20 years

#Q5: What percentage of the reviews are five star reviews (rounded to the nearest percentage point)?
x=lapply(review,fromJSON)

y=sapply(x,function(x) x$star==5)
sum(y)/length(y) #0.369

#Q6: How many lines are there in the businesses file?        
business=readLines('yelp_academic_dataset_business.json') 

#Q7: Conditional on having an response for the attribute "Wi-Fi", how many businesses 
# are reported for having free wi-fi (rounded to the nearest percentage point)?
business_Json=lapply(business,fromJSON)
x=sapply(business_Json,function(x) 'Wi-Fi' %in% names(x$attributes))
business_wifi=business_Json[x]
unique(sapply(business_wifi,function(x) x$attributes$`Wi-Fi`))
wifi=sapply(business_wifi,function(x) x$attributes$`Wi-Fi`=='free')
sum(wifi)/length(wifi)  # 0.409

#Q8: How many lines are in the tip file?
tip=readLines('yelp_academic_dataset_tip.json')
length(tip) #495107

#Q9: In the tips file on the 1,000th line, fill in the blank: "Consistently terrible ______"
tip[1000] #Service

#Q10: What is the name of the user with over 10,000 compliment votes of type "funny"?
User_Json=lapply(readLines('yelp_academic_dataset_user.json'),fromJSON)
x=sapply(User_Json,function(x) 'funny' %in% names(x$compliments))
y=sapply(User_Json[x],function(x) x$compliments$funny>10000)
User_Json[x][y][[1]]$name #Brian



#Q11: 
df=data.frame(funny=sapply(User_Json,function(x) 
                                    if ('funny' %in% names(x$compliments)) {
                                          x$compliments$funny>1
                                    } else
                                    {FALSE}),
              fans=sapply(User_Json,function(x) 
                                    #'fans' %in% names(x) &
                                    x$fans>1)
)

forfish=matrix(
      c(
      #fans & funny
      df %>%
            filter(funny==TRUE,fans==TRUE) %>%
            nrow 
      ,
      #fans & no funny
      df %>%
            filter(funny==FALSE,fans==TRUE) %>%
            nrow
      ,
      #no fans & funny
      df %>%
            filter(funny==TRUE,fans==FALSE) %>%
            nrow
      ,
      #no fans & no funny
      df %>%
            filter(funny==FALSE,fans==FALSE) %>%
            nrow
      )
      ,nrow=2
)

fisher.test(table(df))
