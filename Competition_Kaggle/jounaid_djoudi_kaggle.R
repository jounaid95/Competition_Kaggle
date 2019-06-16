rm(list=ls())


if(!require(gbm)){
  install.packages("gbm")
  library(gbm)
}

if(!require(SnowbTrainC)){
  install.packages("SnowbTrainC")
  library(SnowbTrainC)
}

if(!require(caTools)){
  install.packages("caTools")
  library(caTools)
}

if(!require(tm)){
  install.packages("tm")
  library(tm)
}

if(!require(SnowballC)){
  install.packages("SnowballC")
  library(SnowballC)
}

if(!require(wordcloud)){
  install.packages("wordcloud")
  library(wordcloud)
}

if(!require(randomForest)){
  install.packages("randomForest")
  library(randomForest)
}

if(!require(caret)){
  install.packages("caret")
  library(caret)
}

if(!require(pROC)){
  install.packages("pROC")
  library(pROC)
}
if(!require(ROCR)){
  install.packages("ROCR")
  library(ROCR)
}

if(!require(data.table)){
  install.packages("data.table")
  library(data.table)
}

if(!require(caretEnsemble)){
  install.packages("caretEnsemble")
  library(caretEnsemble)
}

if(!require(e1071)){
  install.packages("e1071")
  library(e1071)
}

if(!require(xgboost)){
  install.packages("xgboost")
  library(xgboost)
}

Train = read.csv("Train.csv", stringsAsFactors=FALSE)
Test = read.csv("Test.csv", stringsAsFactors=FALSE)

AllDatas <- rbind(Train[,-c(9,10,12)],Test)

AllDatas$NewsDesk[AllDatas$SectionName=="Opinion"] <- "OpEd"
AllDatas$NewsDesk[AllDatas$SectionName=="Style"] <- "Styles"
AllDatas$NewsDesk[AllDatas$SectionName=="Business Day"] <- "Business"
AllDatas$NewsDesk[AllDatas$SectionName=="Arts"] <- "Culture"
AllDatas$NewsDesk[AllDatas$SectionName=="Health"] <- "Science"
AllDatas$NewsDesk[AllDatas$SectionName=="Magazine"] <- "Magazine"
AllDatas$NewsDesk[AllDatas$SectionName=="N.Y. / Region"] <- "Metro"
AllDatas$NewsDesk[AllDatas$SectionName=="T:Style"] <- "TStyle"
AllDatas$NewsDesk[AllDatas$SectionName=="World"] <- "Foreign"
AllDatas$NewsDesk[AllDatas$SectionName=="Sports"] <- "Sports"
AllDatas$NewsDesk[AllDatas$SectionName=="Technology"] <- "Business"
AllDatas$NewsDesk[AllDatas$SectionName=="Crosswords/Games"] <- "Business"
AllDatas$NewsDesk[AllDatas$SectionName=="Travel"] <- "Travel"
# AllDatas$NewsDesk[AllDatas$SectionName=="false" | AllDatas$SubsectionName=="false"]<- "UnknownNewsDesk"


AllDatas$SubsectionName[AllDatas$SectionName=="Education"] <- "Education"
AllDatas$SubsectionName[AllDatas$SectionName=="Multimedia"] <- "Multimedia"
AllDatas$SubsectionName[AllDatas$SectionName=="Multimedia/Photos"] <- "Multimedia/Photos"
# AllDatas$SubsectionName[AllDatas$SubsectionName=="false"] <- "UnknownSubsectionName"

AllDatas$SectionName[AllDatas$SectionName=="Crosswords & Games"]  <- "Crosswords/Games"
AllDatas$SectionName[AllDatas$NewsDesk=="Foreign"] <- "World"
AllDatas$SectionName[AllDatas$NewsDesk=="OpEd"] <- "Opinion"
# AllDatas$SectionName[AllDatas$SectionName=="false"] <- "UnknownSectionName"

# AllDatas$NewsDesk[AllDatas$SubsectionName=="" & AllDatas$SectionName==""] <- "UnknownNewsDesk"
# AllDatas$SectionName[AllDatas$SubsectionName=="" & AllDatas$NewsDesk=="UnknownNewsDesk"] <- "UnknownSectionName"
# AllDatas$SubsectionName[AllDatas$NewsDesk=="UnknownNewsDesk" & AllDatas$SectionName=="UnknownSectionName"] <- "UnknownSubsectionName"



Train_bis <- head(AllDatas,nrow(Train))
Test <- tail(AllDatas, nrow(Test))
Train <- cbind(Train_bis,Train[,12])
colnames(Train)[11]="popular"

Train$Snippet <- NULL
Test$Snippet <- NULL
# Train$Recommendations <- NULL
# Test$Recommandations <- NULL
# Train$Comments <- NULL
# Test$Comments <- NULL

Train$PubDate = strptime(Train$PubDate, "%Y-%m-%d %H:%M:%S")
Test$PubDate = strptime(Test$PubDate, "%Y-%m-%d %H:%M:%S")

#Train$popular <- as.factor(Train$popular)
Train$Weekday <- Train$PubDate$wday
Train$hour <- Train$PubDate$hour
Train$monthday <- Train$PubDate$mday
Train$yearPubDate <- Train$PubDate$year
Train$month <- substr(Train$PubDate,6,7)
Test$Weekday <- Test$PubDate$wday
Test$hour <- Test$PubDate$hour
Test$monthday <- Test$PubDate$mday
Test$yearPubDate <- Test$PubDate$year
Test$month <- substr(Test$PubDate,6,7)




Train$NewsDesk <- as.factor(Train$NewsDesk)
Train$SectionName <- as.factor(Train$SectionName)
Train$SubsectionName <- as.factor(Train$SubsectionName)
Train$Weekday <- as.factor(Train$Weekday)
Train$hour <- as.factor(Train$hour)
Train$monthday <- as.factor(Train$monthday)
Train$yearPubDate <- as.factor(Train$year)
Train$logWordCount <- log(Train$WordCount+1)
Train$month <- as.factor(Train$month)


Test$SubsectionName[Test$SubsectionName == "Pro Football"] <- ""
Test$NewsDesk <- as.factor(Test$NewsDesk)
Test$SectionName <- as.factor(Test$SectionName)
Test$SubsectionName <- as.factor(Test$SubsectionName)
Test$Weekday <- as.factor(Test$Weekday)
Test$hour <- as.factor(Test$hour)
Test$monthday <- as.factor(Test$monthday)
Test$yearPubDate <- as.factor(Test$year)
Test$logWordCount <- log(Test$WordCount+1)
Test$month <- as.factor(Test$month)

Train$PubDate <- NULL
Test$PubDate <- NULL

levels(Test$SectionName) <- levels(Train$SectionName)

#################################### Text Mining ##################################

AllCorpus <- rbind(Train[,4:5],Test[,4:5])

# count the 'c' character
count.c = function(col, c) {
  sapply(sapply(gregexpr(paste0("[",c,"]"), col),function(x) { as.integer(x>=0) * length(x) } ), max)
}

AllCorpus$nchar.head <- nchar(AllCorpus$Headline)
AllCorpus$nwords.head <- sapply(strsplit(AllCorpus$Headline, ' '), length)
AllCorpus$nmaj.head <- count.c(AllCorpus$Headline, "A-Z")
AllCorpus$number.head <- count.c(AllCorpus$Headline, "0-9")
AllCorpus$dollar.head <- count.c(AllCorpus$Headline, "$")
AllCorpus$exclam.head <- count.c(AllCorpus$Headline, "!")
AllCorpus$quest.head <- count.c(AllCorpus$Headline, "?")
AllCorpus$quote.head <- count.c(AllCorpus$Headline, "'")
AllCorpus$deux_points.head <- count.c(AllCorpus$Headline, ":")
AllCorpus$lognchar.head <- log(AllCorpus$nchar.head+1)

AllCorpus$nchar.abstract <- nchar(AllCorpus$Abstract)
AllCorpus$nwords.abstract <- sapply(strsplit(AllCorpus$Abstract, ' '), length)
AllCorpus$nmaj.abstract <- count.c(AllCorpus$Abstract, "A-Z")
AllCorpus$number.abstract <- count.c(AllCorpus$Abstract, "0-9")
AllCorpus$dollar.abstract <- count.c(AllCorpus$Abstract, "$")
AllCorpus$exclam.abstract <- count.c(AllCorpus$Abstract, "!")
AllCorpus$quest.abstract <- count.c(AllCorpus$Abstract, "?")
AllCorpus$quote.abstract <- count.c(AllCorpus$Abstract, "'")
AllCorpus$deux_points.abstract <- count.c(AllCorpus$Abstract, ":")
AllCorpus$lognchar.abstract <- log(AllCorpus$nchar.abstract+1)




AllCorpus$Headline <- gsub("New York|New York City", "NewYork", AllCorpus$Headline, ignore.case = TRUE)
AllCorpus$nNewYork.head <- count.c(AllCorpus$Headline, "NewYork")
AllCorpus$Abstract <- gsub("New York|New York City", "NewYork", AllCorpus$Abstract, ignore.case = TRUE)
AllCorpus$nNewYork.abstract <- count.c(AllCorpus$Abstract, "NewYork")

AllCorpus$Abstract <- gsub(pattern="\\W",replace = " ",AllCorpus$Abstract)
AllCorpus$Abstract <- gsub(pattern="\\â",replace = "",AllCorpus$Abstract)
AllCorpus$Abstract <- gsub(pattern = "\\b[A-z]\\b{1}",replace=" ",AllCorpus$Abstract)
AllCorpus$Abstract <- gsub(pattern="\\d",replace = " ",AllCorpus$Abstract)
AllCorpus$Headline <- gsub(pattern="\\â",replace = "",AllCorpus$Headline)
AllCorpus$Headline <- gsub(pattern = "\\b[A-z]\\b{1}",replace=" ",AllCorpus$Headline)
AllCorpus$Headline <- gsub(pattern="\\d",replace = " ",AllCorpus$Headline)
AllCorpus$Headline <- gsub(pattern="\\W",replace = " ",AllCorpus$Headline)


countTrain <- head(AllCorpus, nrow(Train))
countTest <- tail(AllCorpus, nrow(Test))

countTrain$Headline <- NULL
countTrain$Abstract <- NULL
countTest$Abstract <- NULL
countTest$Headline <- NULL

Train <- cbind(Train,countTrain)
Test <- cbind(Test,countTest)

#################################### Corpus Headline #####################################

CorpusHeadline = Corpus(VectorSource(AllCorpus$Headline))

# You can go through all of the standard pre-processing steps like we did in Unit 5:

CorpusHeadline = tm_map(CorpusHeadline, tolower)

CorpusHeadline = tm_map(CorpusHeadline, removePunctuation)

CorpusHeadline = tm_map(CorpusHeadline, removeWords, stopwords("english"))

CorpusHeadline = tm_map(CorpusHeadline, stemDocument)


dtm = DocumentTermMatrix(CorpusHeadline)

sparse = removeSparseTerms(dtm, 0.99)

HeadlineWords = as.data.frame(as.matrix(sparse))

colnames(HeadlineWords) <- paste(colnames(HeadlineWords),"head",sep = ".")

colnames(HeadlineWords) = make.names(colnames(HeadlineWords))

HeadlineWordsTrain = head(HeadlineWords, nrow(Train))
HeadlineWordsTest = tail(HeadlineWords, nrow(Test))
TrainB = cbind(Train,HeadlineWordsTrain)
TestB = cbind(Test,HeadlineWordsTest)

TrainB$Headline <- NULL
TestB$Headline <- NULL

######################################### Corpus Abstract ################################

CorpusAbstract = Corpus(VectorSource(AllCorpus$Abstract))

# You can go through all of the standard pre-processing steps like we did in Unit 5:

CorpusAbstract = tm_map(CorpusAbstract, tolower)

CorpusAbstract = tm_map(CorpusAbstract, removePunctuation)

CorpusAbstract = tm_map(CorpusAbstract, removeWords, stopwords("english"))

CorpusAbstract = tm_map(CorpusAbstract, stemDocument)


dtm = DocumentTermMatrix(CorpusAbstract)

sparse = removeSparseTerms(dtm, 0.99)

AbstractWords = as.data.frame(as.matrix(sparse))

colnames(AbstractWords) <- paste(colnames(AbstractWords),"abstract",sep = ".")

colnames(AbstractWords) = make.names(colnames(AbstractWords))

AbstractWordsTrain = head(AbstractWords, nrow(Train))
AbstractWordsTest = tail(AbstractWords, nrow(Test))
# TrainB = cbind(TrainB,AbstractWordsTrain)
# TestB = cbind(TestB,AbstractWordsTest)

TrainB$Abstract <- NULL
TestB$Abstract <- NULL

######################################### random forest #########################################

set.seed(112)
forest <- randomForest(popular ~.-UniqueID-WordCount,data = TrainB, 
                       nodesize = 2,na.action=na.exclude,ntree = 1000,importance = TRUE)

print(forest)
importance(forest)
varImpPlot(forest)

pred_forest_test <- predict(forest, TestB,type="response")

MySubmission = data.frame(UniqueID = TestB$UniqueID, Probability1 = pred_forest_test)

write.csv(MySubmission, "Forest_1000_arbres_sans_abstractwords.csv", row.names=FALSE)

trainPredforest <- predict(forest, type = "response")
table(TrainB$popular, trainPredforest > 0.5)
as.numeric(performance(prediction(trainPredforest, TrainB$popular), "auc")@y.values)

forest$importance[order(forest$importance[, 1], decreasing = TRUE), ]

######################################### Gradient boosting #####################################
# gboost <- gbm(formula =popular ~.-UniqueID-WordCount,data = TrainB, distribution = "bernoulli", n.trees = 8000, interaction.depth = 4)
# 
# print(gboost)
# 
# pred_gboost_test <- predict(gboost, TestB,n.trees = 8000,type="response")
# 
# MySubmission2 = data.frame(UniqueID = TestB$UniqueID, Probability1 = pred_gboost_test)
# 
# write.csv(MySubmission2, "gboost.csv", row.names=FALSE)
# 
# 
# trainPredgboost <- predict(gboost, n.trees = 8000, type = "response")
# table(TrainB$popular, trainPredgboost > 0.5)
# as.numeric(performance(prediction(trainPredgboost, TrainB$popular), "auc")@y.values)

########################################### Ensemble ###########################################

# Ensemble <- data.frame(UniqueID = TestB$UniqueID, Probability1 = 0.5*pred_forest_test + 0.5*pred_gboost_test)
# write.csv(Ensemble, "Ensemble.csv", row.names = FALSE)
# 
# pred_ensemble <- 0.5*trainPredforest + 0.5*trainPredgboost
# table(TrainB$popular, pred_ensemble > 0.5)
# as.numeric(performance(prediction(pred_ensemble, TrainB$popular), "auc")@y.values)

############################################XGBoost################################

# # Combine data  
# AllDatas <- data.frame(rbind(TrainB[,-c(6)], TestB))
# 
# # Get categorical features names
# OneHot_vars <- names(AllDatas)[which(sapply(AllDatas, is.factor))]
# 
# # Convert them
# dummies <- dummyVars(~., data = AllDatas)
# AllDatas_OneHot <- as.data.frame(predict(dummies, newdata = AllDatas))
# 
# # Replace factor variables in data with OHE
# AllDatas <- cbind(AllDatas[, -c(which(colnames(AllDatas) %in% OneHot_vars))], AllDatas_OneHot)
# 
# 
# Train_OneHot <- head(AllDatas, nrow(TrainB))
# Test_OneHot <- tail(AllDatas, nrow(TestB))
# Train_OneHot <- cbind(Train_OneHot,TrainB[,c(6)])
# colnames(Train_OneHot)[279]="popular"
# 
# XGBoost <- xgboost(data = data.matrix(Train_OneHot), label = Train_OneHot$popular,
#                    booster = "gbtree", objective = "reg:linear",
#                    colsample_bytree = 0.2, gamma = 0.0,
#                    learning_rate = 0.05, max_depth = 6,
#                    min_child_weight = 1.5,
#                    reg_alpha = 0.9, reg_lambda = 0.5,
#                    subsample = 0.2, seed = 110,
#                    silent = 1, nrounds = 35)
# 
# pred_XGBoost_test <- predict(XGBoost, data.matrix(Test_OneHot))
# 
# MySubmission3 = data.frame(UniqueID = TestB$UniqueID, Probability1 = pred_XGBoost_test)
# 
# write.csv(MySubmission3, "XGBoost.csv", row.names=FALSE)

