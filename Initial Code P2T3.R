pacman:: p_load(readr, caret, mlbench, e1071, rpart, rpart.plot, corrplot, plotly)
#if (!require("processx")) install.packages("processx")
library(processx)
setwd("C:/Users/louis/OneDrive/Desktop/Ubiqum/Program 2/task2-3-profitability-louislancksweert")
existprod <- read.csv('existingproductattributes2017.2.csv')
newprod <- read.csv('newproductattributes2017.2.csv')

----#Anova Decision tree before dummying to look at potential impact of product type#----
DTvol <- rpart(Volume~.,data=existprod, method='anova')
rpart.plot(DTvol)
existprod$x5StarReviews <- NULL
newprod$x5StarReviews <- NULL

#----PreProcessing----
#Dummifying the data
newDataframe <- dummyVars('~ .', data = newprod)
newpro_dummified <- data.frame(predict(newDataframe, newdata = newprod))

newDataframe <- dummyVars('~ .', data=existprod)
existprod_dummified <- data.frame(predict(newDataframe, newdata = existprod))

#Getting rid of BSR (maybe we can do soemthing different with this)
existprod_dum_NAs <- existprod_dummified
existprod_dum_NAs$BestSellersRank <- NULL

newprod_dum_NAs <- newpro_dummified
newprod_dum_NAs$BestSellersRank <- NULL

#Getting rid of illogical or irrelevant variables: Product# & Profit margin
existprod_dum_NAs$ProductNum <- NULL
#existprod_dum_NAs$ProfitMargin <- NULL

newprod_dum_NAs$ProductNum <- NULL
#newprod_dum_NAs$ProfitMargin <-NULL

#Correlation of variables
Correxist <- cor(existprod_dum_NAs)
corrplot(Correxist)

#Correlation of Ratings, Service Reviews and Volume
Volume <- existprod_dum_NAs[,25]
exist_CustRev_ServRev_vol <- cbind(existprod_dum_NAs[,14:20],Volume)
Cor_CustRev_ServRev_vol <- cor(exist_CustRev_ServRev_vol)
corrplot(Cor_CustRev_ServRev_vol)
Cor_CustRev_ServRev_vol

#Getting rid of highly correlated features
existprod_dum_NAs_xcor <- existprod_dum_NAs
existprod_dum_NAs_xcor$x3StarReviews <- NULL
existprod_dum_NAs_xcor$x1StarReviews <- NULL
existprod_dum_NAs_xcor$NegativeServiceReview <- NULL

newprod_dum_NAs_xcor <- newprod_dum_NAs
newprod_dum_NAs_xcor$x3StarReviews <- NULL
newprod_dum_NAs_xcor$x1StarReviews <- NULL
newprod_dum_NAs_xcor$NegativeServiceReview <- NULL

#Feature engineering width, length and depth into one variable
Product_size <- existprod_dum_NAs_xcor$ProductDepth*existprod_dum_NAs_xcor$ProductWidth*existprod_dum_NAs_xcor$ProductHeight
existprod_dum_NAs_xcor_size <- existprod_dum_NAs_xcor
existprod_dum_NAs_xcor_size$ProdcutSize <- Product_size

Product_size <- newprod_dum_NAs_xcor$ProductDepth * newprod_dum_NAs_xcor$ProductWidth * newprod_dum_NAs_xcor$ProductHeight
newprod_dum_NAs_xcor_size <- newprod_dum_NAs_xcor
newprod_dum_NAs_xcor_size$ProductSize <- Product_size

# Delete Width/Height/Depth
existprod_dum_NAs_xcor_size[,19:21] <- NULL
newprod_dum_NAs_xcor_size [,19:21] <- NULL

#Decision tree to have a quick look at ProductType impact
DTclass <- rpart(Volume~., data=existprod_dum_NAs_xcor_size,method='anova', 
                 control=rpart.control(minsplit=3))
rpart.plot(DTclass)


----# Series of plots for initial look #----
# A loop for scatterplots of existing data
for (i in 13:ncol(existprod_dum_NAs_xcor_size)){
  (plot(existprod_dum_NAs_xcor_size[,i],existprod_dum_NAs_xcor_size$Volume,main=paste('Scatterplot of Volume and',colnames(existprod_dum_NAs_xcor[i]))))}

#Looking at outliers of existing data
for (i in 13:ncol(existprod_dum_NAs_xcor_size)){
  (boxplot(existprod_dum_NAs_xcor[,i],main=paste('Boxplot of',colnames(existprod_dum_NAs_xcor[i]))))}

#Looking at outliers the new_prod data
for (i in 13:ncol(newpro_dummified)){
  (boxplot(newpro_dummified[,i],main=paste('Boxplot of',colnames(newpro_dummified[i]))))}

#for loop to graph box plot of each feature for existing and new products
for (i in 13:ncol(existprod_dum_NAs_xcor_size)){
    plot_ly(y=existprod_dum_NAs_xcor_size[,i], type="box", name='Existing products') %>% 
    add_boxplot(y=newprod_dum_NAs_xcor_size[,i], type="box", name='New products') %>%
    layout(title= paste('Boxplot of',colnames(existprod_dum_NAs_xcor_size[i]),'for Existing and New Products'))
}
# A simple example
plot_ly(y=existprod_dum_NAs_xcor_size[,20], type="box", name='Existing products') %>% 
  add_boxplot(y=newprod_dum_NAs_xcor_size[,20], type="box", name='New products') %>%
  layout(title= paste('Boxplot of',colnames(existprod_dum_NAs_xcor_size[20]),'for Existing and New Products'))


#----Deletion of Outliers----
#Delete Warranties
existprod_dum_NAs_xcor_size_warr <- existprod_dum_NAs_xcor_size[!existprod_dum_NAs_xcor_size$ProductType.ExtendedWarranty == 1,] #getting rid of warranty rows
rownames(existprod_dum_NAs_xcor_size_warr) <- 1:nrow(existprod_dum_NAs_xcor_size_warr) # reindexing the dataframe

newprod_dum_NAs_xcor_size_warr <- newprod_dum_NAs_xcor_size[!newprod_dum_NAs_xcor_size$ProductType.ExtendedWarranty ==1,] #gettint rid of warranty rows
rownames(newprod_dum_NAs_xcor_size_warr) <- 1:nrow(newprod_dum_NAs_xcor_size_warr) # reindexing the dataframe

existprod_dum_NAs_xcor_size_warr$ProductType.ExtendedWarranty <- NULL
newprod_dum_NAs_xcor_size_warr$ProductType.ExtendedWarranty <- NULL

#Deletion of specific outliers
existprod_dum_NAs_xcor_size_warr <- existprod_dum_NAs_xcor_size_warr[!existprod_dum_NAs_xcor_size_warr$PositiveServiceReview > 250,]
existprod_dum_NAs_xcor_size_warr <- existprod_dum_NAs_xcor_size_warr[!existprod_dum_NAs_xcor_size_warr$x2StarReviews > 200,]
existprod_dum_NAs_xcor_size_warr <- existprod_dum_NAs_xcor_size_warr[!existprod_dum_NAs_xcor_size_warr$Volume > 3000,]

#----Look at predicting Best Seller Rank----

BSRdataset <- existprod
BSRdataset$ProductNum <- NULL

#creating a vector to then partition the data
vec <- vector() 
for (i in 1:nrow(BSRdataset)){
  if (is.na(BSRdataset[i,11])){
    vec <- c(vec,i)
  }
}

BSRpredict <- BSRdataset[vec,] #Using vector to partition data
BSRmodel <- BSRdataset[-vec,]

rownames(BSRpredict) <- 1:nrow(BSRpredict) #reindexing the subsequent dataframes
rownames(BSRmodel) <- 1:nrow(BSRmodel)

#creating quick DT to look at possible predictions
DT_BSR <- rpart(BestSellersRank~.,data=BSRmodel,method='anova', 
                control=rpart.control(minsplit=2))
rpart.plot(DT_BSR)

#Dummyfying Product type
newDataframe <- dummyVars('~ .', data = BSRmodel)
BSRmodel_dumm <- data.frame(predict(newDataframe, newdata = BSRmodel))

newDataframe <- dummyVars('~ .', data = BSRpredict)
BSRmpredict_dumm <- data.frame(predict(newDataframe, newdata = BSRpredict))

#Creating correlation matrix
CORR <- cor(BSRmodel_dumm)
corrplot(CORR)


#----Create Models----

#look at correlation to look at features to start predicting with
corrplot(cor((existprod_dum_NAs_xcor_size_warr)))

set.seed(107)
intrain <- createDataPartition(
  y = existprod_dum_NAs_xcor_size_warr$Volume,
  p=.75,
  list = FALSE
)

exist_train <- existprod_dum_NAs_xcor_size_warr[intrain,]
exist_test <- existprod_dum_NAs_xcor_size_warr[-intrain,]
nrow(exist_train)
nrow(exist_test)

set.seed(107)
ctrl <- trainControl(method = "repeatedcv", number=10, repeats = 3, search='random')
SVMmod <- train(
  Volume ~ PositiveServiceReview,
  data=exist_train,
  method='svmRadial',
  preProcess= c('center','scale'),
  trControl = ctrl,
  metric = 'RMSE',
  tune.length = 100
)

SVMmod2 <- train(
  Volume ~ PositiveServiceReview + x4StarReviews,
  data=exist_train,
  method='svmRadial',
  preProcess= c('center','scale'),
  trControl = ctrl,
  metric = 'RMSE',
  tune.length = 100
)

SVMmod3 <-  train(
  Volume ~ PositiveServiceReview + x4StarReviews + x2StarReviews,
  data=exist_train,
  method='svmRadial',
  preProcess= c('center','scale'),
  trControl = ctrl,
  metric = 'RMSE',
  tune.length = 100
)

SVMmod4 <-  train(
  Volume ~ PositiveServiceReview + x4StarReviews + x2StarReviews + ProductType.GameConsole,
  data=exist_train,
  method='svmRadial',
  preProcess= c('center','scale'),
  trControl = ctrl,
  metric = 'RMSE',
  tune.length = 100
)

SVMmod5 <-  train(
  Volume ~ PositiveServiceReview + x4StarReviews + ProductType.GameConsole,
  data=exist_train,
  method='svmRadial',
  preProcess= c('center','scale'),
  trControl = ctrl,
  metric = 'RMSE',
  tune.length = 100
)

SVMmod6 <- train(
  Volume ~ PositiveServiceReview + ProductType.GameConsole,
  data=exist_train,
  method='svmRadial',
  preProcess= c('center','scale'),
  trControl = ctrl,
  tune.length = 100
)

SVMmod7 <- train(
  Volume ~ PositiveServiceReview + ProductType.GameConsole + x2StarReviews,
  data=exist_train,
  method='svmRadial',
  preProcess= c('center','scale'),
  trControl = ctrl,
  tune.length = 100
)

SVMmod8 <- train(
  Volume ~ PositiveServiceReview + ProductType.GameConsole + x2StarReviews,
  data=exist_train,
  method='svmRadial',
  preProcess= c('center','scale'),
  trControl = ctrl,
  tune.length = 100
)
#-----For loop to check results----
results <-c()
SVM_prediction_results <- c()
SVMmodels = list(SVMmod,SVMmod2,SVMmod3,SVMmod4,SVMmod5,SVMmod6, SVMmod7)
for (i in 1:7){
  if (i==1){
    results <- c()
  }
  placeholder <- predict(SVMmodels[i], newdata = exist_test)
  placeholder <- unlist(placeholder, use.names=FALSE)
  perf <- postResample(placeholder,exist_test$Volume)
  results <- cbind(results,perf)
  colnames(results)[i] <- paste('SVM',i)
}
