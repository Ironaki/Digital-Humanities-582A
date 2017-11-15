rm(list=ls())
library(stringr)
Sys.setlocale("LC_ALL", "English")
Sys.setlocale("LC_ALL", "Japanese")
iconv(x, 'cp932', 'utf-8')

## reads file with training set of proof-read text
Training.df <- read.delim("C:/Users/klijia/Desktop/HIST582A/W4/Tr.txt", header=FALSE, stringsAsFactors=FALSE, encoding = "ANSI")

## finds cases of  二 or ニ and get three characters before and after
samples <- str_extract_all(Training.df$V1, ".{3}[二|ニ].{3}", simplify = FALSE)


samples <- unlist(samples)
##creates variables based on characters
pre_3 <- substr(samples,1,1)
pre_2<- substr(samples,2,2)
pre_1 <- substr(samples,3,3)
answer <- substr(samples,4,4)
post_1 <- substr(samples,5,5)
post_2 <- substr(samples,6,6)
post_3 <- substr(samples,7,7)


## defines a machine learning training set using the preceeding and following characters as attributes
Training.data.df <- data.frame(answer, pre_3, pre_2, pre_1, post_1, post_2,post_3)
summary(Training.data.df$answer)
library(RWeka)
library(stringr)

#  WPM("refresh-cache")
##creates a series of classifiers
m1 <- AdaBoostM1(answer ~ ., data = Training.data.df, control = Weka_control(W = "DecisionStump"))
m2 <- Bagging(answer ~ ., data = Training.data.df)
m3 <- LBR(answer ~ ., data = Training.data.df)
m4 <- LogitBoost(answer ~ ., data = Training.data.df)
m5 <- Logistic(answer ~ ., data = Training.data.df)

##generates predictions on the training set
table(predict(m5), Training.data.df$answer)
summary(m5) # uses evaluate_Weka_classifier()
evaluate_Weka_classifier(m1, newdata = NULL, cost = NULL, numFolds = 5, complexity = FALSE,class = FALSE, seed = NULL)



###validate
Validation <- read.table("http://history.emory.edu/RAVINA/Machine_learning/OCR_correction/Validation_ni.txt", quote="\"", comment.char="", stringsAsFactors=FALSE)
# Validate_data <- str_extract_all(Validation$V1, ".{1}[二｜ニ].{1}", simplify = FALSE)
Validate_data <- str_extract_all(Validation$V1, ".{3}[二|ニ].{3}", simplify = FALSE)
Validate_data <- unlist(Validate_data)
# ##pre_5 <- substr(Validate_data,1,1)
# ##pre_4<- substr(Validate_data,2,2)
# pre_3 <- substr(Validate_data,1,1)
# pre_2 <- substr(Validate_data,2,2)
# pre_1 <- substr(Validate_data,3,3)
# post_1 <- substr(Validate_data,5,5)
# post_2 <- substr(Validate_data,6,6)
# post_3 <- substr(Validate_data,7,7)
# ##post_4 <- substr(Validate_data,10,10)
# ##post_5 <- substr(Validate_data,11,11)
# answer <- substr(Validate_data,4,4)
pre_3 <- substr(Validate_data,1,1)
pre_2<- substr(Validate_data,2,2)
pre_1 <- substr(Validate_data,3,3)
answer <- substr(Validate_data,4,4)
post_1 <- substr(Validate_data,5,5)
post_2 <- substr(Validate_data,6,6)
post_3 <- substr(Validate_data,7,7)

Validate.df <- data.frame(pre_3, pre_2, pre_1, answer, post_1, post_2,post_3)
# Validate <- data.frame(pre, answer, post)
summary(Validate.df$answer)
evaluate_Weka_classifier(m1, newdata = Validate.df, cost = NULL, complexity = FALSE,class = FALSE, seed = NULL)
evaluate_Weka_classifier(m2, newdata = Validate.df, cost = NULL, complexity = FALSE,class = FALSE, seed = NULL)
evaluate_Weka_classifier(m3, newdata = Validate.df, cost = NULL, complexity = FALSE,class = FALSE, seed = NULL)
evaluate_Weka_classifier(m4, newdata = Validate.df, cost = NULL, complexity = FALSE,class = FALSE, seed = NULL)
evaluate_Weka_classifier(m5, newdata = Validate.df, cost = NULL, complexity = FALSE,class = FALSE, seed = NULL)

