pacman:: p_load(readr, caret, mlbench, e1071, rpart, rpart.plot, corrplot, plotly)
setwd("C:/Users/louis/OneDrive/Desktop/Ubiqum/Program 2/task2-3-profitability-louislancksweert")
existprod <- read.csv('existingproductattributes2017.2.csv')
newprod <- read.csv('newproductattributes2017.2.csv')

#Anova Decision tree before dummying to look at potential impact of product type
existprod$x5StarReviews <- NULL
DTvol <- rpart(Volume~.,data=existprod, method='anova')
rpart.plot(DTvol)

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
existprod_dum_NAs$ProfitMargin <- NULL

newprod_dum_NAs$ProductNum <- NULL
newprod_dum_NAs$ProfitMargin <-NULL

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
existprod_dum_NAs_xcor$x5StarReviews <- NULL
existprod_dum_NAs_xcor$x3StarReviews <- NULL
existprod_dum_NAs_xcor$x1StarReviews <- NULL
existprod_dum_NAs_xcor$NegativeServiceReview <- NULL

newprod_dum_NAs_xcor <- newprod_dum_NAs
newprod_dum_NAs_xcor$x5StarReviews <- NULL
newprod_dum_NAs_xcor$x3StarReviews <- NULL
newprod_dum_NAs_xcor$x1StarReviews <- NULL
newprod_dum_NAs_xcor$NegativeServiceReview <- NULL

#Feature engineering width, length and depth into one variable
Product_size <- existprod_dum_NAs_xcor$ProductDepth*existprod_dum_NAs_xcor$ProductWidth*existprod_dum_NAs_xcor$ProductHeight
existprod_dum_NAs_xcor_size <- existprod_dum_NAs_xcor
existprod_dum_NAs_xcor$ProdcutSize <- Product_size


Product_size <- newprod_dum_NAs_xcor$ProductDepth * newprod_dum_NAs_xcor$ProductWidth * newprod_dum_NAs_xcor$ProductHeight
newprod_dum_NAs_xcor_size <- newprod_dum_NAs_xcor
newprod_dum_NAs_xcor$ProductSize <- Product_size

# Delete Width/Height/Depth
#for (i in 18:20){
 # existprod_dum_NAs_xcor_size[,i] <- NULL
#}
existprod_dum_NAs_xcor_size[,18:20] <- NULL
###This is causing problems for some reason

#Decision tree to have a quick look at ProductType impact
DTclass <- rpart(Volume~., data=existprod_dum_NAs_xcor,method='anova', 
                 control=rpart.control(minsplit=3))
rpart.plot(DTclass)

# Maybe a for loop for scatterplots --> but we already know what the results of that will be
for (i in 13:ncol(existprod_dum_NAs_xcor_size)){
  (plot(existprod_dum_NAs_xcor_size[,i],existprod_dum_NAs_xcor_size$Volume,main=paste('Scatterplot of Volume and',colnames(existprod_dum_NAs_xcor[i]))))}

#Looking at outliers
for (i in 13:ncol(existprod_dum_NAs_xcor_size)){
  (boxplot(existprod_dum_NAs_xcor[,i],main=paste('Boxplot of',colnames(existprod_dum_NAs_xcor[i]))))}

#Looking at outliers the new_prod data
for (i in 13:ncol(newpro_dummified)){
  (boxplot(newpro_dummified[,i],main=paste('Boxplot of',colnames(newpro_dummified[i]))))}

#----Trying to merge the 2 datasets and boxplot relevant variables together----

#for loop to graph box plot of each feature for existing and new products
for (i in 13:ncol(existprod_dum_NAs_xcor_size)){
 plot_ly(y=existprod_dum_NAs_xcor_size[,i], type="box", name='Existing products') #%>% 
    add_boxplot(y=newprod_dum_NAs_xcor_size[,i], type="box", name='New products') #%>%
    layout(title= paste('Boxplot of',colnames(existprod_dum_NAs_xcor_size[i]),'for Existing and New Products'))
 
}
# A simple example
plot_ly(y=existprod_dum_NAs_xcor_size[,14], type="box", name='Existing products') %>% 
  add_boxplot(y=newprod_dum_NAs_xcor_size[,14], type="box", name='New products') %>%
  layout(title= paste('Boxplot of',colnames(existprod_dum_NAs_xcor_size[14]),'for Existing and New Products'))


#----Impact of Warranties----
#Delete Warranties
existprod_dum_NAs_xcor_size_warr <- existprod_dum_NAs_xcor_size[!existprod_dum_NAs_xcor_size$ProductType.ExtendedWarranty == 1,] #getting rid of warranty rows
rownames(existprod_dum_NAs_xcor_size_warr) <- 1:nrow(existprod_dum_NAs_xcor_size_warr) # reindexing the dataframe

newprod_dum_NAs_xcor_size_warr <- newprod_dum_NAs_xcor_size![newprod_dum_NAs_xcor_size$ProductType.ExtendedWarranty ==1,] #gettint rid of warranty rows
rownames(newprod_dum_NAs_xcor_size_warr) <- 1:nrow(newprod_dum_NAs_xcor_size_warr) # reindexing the dataframe

#existprod_dum_NAs_xcor_size_warr <- filter_(existprod_dum_NAs_xcor_size,existprod_dum_NAs_xcor_size$ProductType.ExtendedWarranty == 1)

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

