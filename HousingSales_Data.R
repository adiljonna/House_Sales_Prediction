setwd("D:/Miage/1st_Semester/DA/code")
housingSales_data <- read.table("train.csv", header=TRUE, sep=",")
testData <- read.table("test.csv", header=TRUE, sep=",")
#=======================================

# Data Summary
total_rows <- length(rownames(housingSales_data))
total_column <- length(colnames(housingSales_data))
dim(housingSales_data)
clnames <- colnames(housingSales_data) 
summary(housingSales_data)
View(clnames)

#=============================================
# filtering the numericData

housingSales_data_numeric <- unlist(lapply(housingSales_data,is.numeric))
housingSales_data_numericN <- housingSales_data[, housingSales_data_numeric]
dim(housingSales_data_numericN)

#housingSales_data_numericNClean <- na.omit(housingSales_data_numericN) 

#featuresMedian<-apply(housingSales_data_numericN,2,median )

# to run "for" loop start running from here


# num_features <- ncol(housingSales_data_numericNClean)
# sig_feature_names <- c()
# feature_names <- colnames(housingSales_data_numericNClean)
# features_labels <- c()
#   for (f in 2:num_features){
#   feature <- housingSales_data_numericNClean[, c(f)]
#   
#   if (median(feature)>0 )
#   {
#     print (paste(feature_names[f],median(feature)))
#     feature_name <- feature_names[f]
#     sig_feature_names <- c(sig_feature_names, feature_name)
#     
#   }
#   }
# print (sig_feature_names)
# features_labels <- housingSales_data_numericNClean[,c(sig_feature_names)]
#alternate option to above for loop given below (manually)

features_labels <- housingSales_data_numericN[, -c(1,9,11,15,16,18,19,21,29,31,32,33,34,35)]
dim(features_labels)

#=====================================================

#Exploring all statistically significant features

# with confidence interval (99.9999%) and corelation >=0.2

X <- features_labels[, c(1:23)]
num_featuresNew <- ncol(X)
y <- features_labels$SalePrice
N <- length(y)
alpha <- 0.0001 #99.9999% confidence interval
feature_namesNew <- colnames(X)
sig_feature_namesNew <- c()
for (f in 1:num_featuresNew){
  featureNew <- X[, c(f)]
  r <- cor(featureNew, y, method = "pearson", use = "pairwise.complete.obs")
  t_value <- r * sqrt((N-2)/(1 - r*r))
  p_value <- 2*pt(-abs(t_value),df=N-1)
 
  if (abs(r)>=0.20){
    print (paste(feature_namesNew[f], r, t_value, p_value))
  if (p_value < alpha){
    feature_nameNew <- feature_namesNew[f]
    sig_feature_namesNew <- c(sig_feature_namesNew, feature_nameNew)
  }}
}
print (sig_feature_namesNew)
XY <- features_labels[, c("SalePrice",sig_feature_namesNew)]
dim(XY)
saveCor<-cor(XY[,2:18])
install.packages('corrplot')
library(corrplot)
corrplot(saveCor, method="circle")
head(XY)

# strongly corelated pairs (with corelation = 0.8)
#YearBuilt ,GarageYrBlt, keep  YearBuilt
#TotalBsmtSF,X1stFlrSF , keep TotalBsmtSF
#GrLivArea ,TotRmsAbvGrd , keep TotRmsAbvGrd 
#GarageCars ,GarageArea.. keep Garage Area

XYNew <- XY[, -c(15,10,11,17)]
dim(XYNew)
head(XYNew)
#=======Linear Regression Model

model <- lm(SalePrice ~., data= XYNew)
summary(model)
model <- lm(log(SalePrice) ~., data= XY)
summary(model)
plot(model, 2)
plot(model, 1)
d <- density(model[['residuals']])
plot(d, main='Residual KDE Plot', xlab='Residual value')
head(testData)
dim(testData)
Y2predict <- predict(model,testData )
length(Y2predict)
View(Y2predict)
df <- data.frame(Id = testData$Id ,SalePrice = Y2predict)

#==============================
write.csv(df,"SalesPrice_prediction.csv",row.names = FALSE)
