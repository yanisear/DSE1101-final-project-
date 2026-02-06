
#libraries
library(leaps)
library(dplyr)
library(boot)
library(kknn)
library(tree)
library(pls)
Econ <- read.csv("econ_data.csv", header = TRUE, stringsAsFactors = FALSE)
summary(Econ) #139 observations and 71 variables

#however some of the countries do not have proper values for some variables, must filter the data
Econ[Econ == "."] <- NA #replacing full stops '.' with NA for filtering purposes
na_counts <- sapply(Econ, function(x) sum(is.na(x))) #calculating the sum of 'NA' values in each column
columns_above_threshold <- names(na_counts[na_counts > 20]) #setting the threshold of 'NA' values as 20
filtered_var <- Econ[, !(names(Econ) %in% columns_above_threshold)] #removing variables that have more than 20 'NA' values
# we are left with 60 variables
# With the remaining variables, I will remove countries that have any 'NA' values for any variables 
filtered_countries = na.omit(filtered_var)
df = filtered_countries #this is the data after filtering. We are left with 95 countries and 60 variables
df[,4:ncol(df)] <- lapply(df[, 4:ncol(df)], as.numeric)
df$GR6096 <- df$GR6096 * 100

#splitting the data into training and test sets
set.seed(100)
ntrain = 76
train_obs = sample(1:nrow(df), ntrain)
train = df[train_obs, ]
test = df[-train_obs, ]


#Using the 'leaps' package to conduct backward selection for the construction of a linear regression model
lm_fit2 <- regsubsets(GR6096~ . - OBS - CODE - COUNTRY, data = train, method = "backward", nvmax = 60)
summary(lm_fit2)$adjr2
which.max(summary(lm_fit2)$adjr2) #this tells us that the best model (the one with the highest adjusted r-squared) has 22 coefficients
coef(lm_fit2, 22)

lm2 <- lm(GR6096 ~ ABSLATIT+AVELF+CIV72+COLONY+EAST+EUROPE
          +FERTLDC1+GDPCH60L+GOVNOM1+IPRICE1+LAAM+LIFE060+OTHFRAC+PRIGHTS+POP1560+POP6560
          +SIZE60+YRSOPEN, data = train)
summary(lm2)
lm_pred2 <- predict(lm2, newdata = test) #using the linear model to make predictions on the test data
sqrt(mean((test$GR6096-lm_pred2)^2))

plot(x = test$GR6096, y = lm_pred2, xlab = "Actual growth rates (%)", ylab = "Predicted growth rate (%)", cex.lab = 1.6)
text(x = test$GR6096, y = lm_pred2, labels = test$CODE, pos = 4, cex = 1.6)  
abline(a = 0, b = 1)


##KNN REGRESSION##
set.seed(100)
ntrain = 76
train_obs = sample(1:nrow(df), ntrain)
train = df[train_obs, ]
test = df[-train_obs, ]

growthcv=train.kknn(GR6096 ~ . - OBS - CODE - COUNTRY,data=train,kmax=73, kernel = "rectangular")
windows()
plot((1:73),growthcv$MEAN.SQU, type="l", col = "blue", main="LOOCV MSE", xlab="Number of neighbors", ylab="MSE")
kbest=growthcv$best.parameters$k
knnreg = kknn(GR6096~ . - OBS - CODE - COUNTRY,train,test,k=kbest,kernel = "rectangular")
knnrmse=sqrt(mean((test$GR6096-knnreg$fitted.values)^2))
windows()
plot(x = test$GR6096, y = knnreg$fitted.values, xlab = "Actual growth rates (%)", ylab = "Predicted growth rate (%)", cex.lab = 1.8)
text(x = test$GR6096, y = knnreg$fitted.values, labels = test$CODE, pos = 4, cex = 1.6)  
abline(a = 0, b = 1)

##Regression trees##
#tree package

set.seed(100)
ntrain = 76
train_obs = sample(1:nrow(df), ntrain)
train = df[train_obs, ]
test = df[-train_obs, ]

temp = tree(GR6096~ . - OBS - CODE - COUNTRY,data=train,mindev=0.00001) #building a full tree
length(unique(temp$where)) #tree has 13 leaves

#using cross-validation with tree package:
set.seed(100)
cv.df = cv.tree(temp,, prune.tree) #10-fold cross-validation
best.cp = cv.df$size[max(which(cv.df$dev == min(cv.df$dev)))] #calculating the best complexity parameter to help us determine the size of tree
df.tree = prune.tree(temp, best = best.cp) #pruning the tree according to the best cp obtained
length(unique(df.tree$where)) #pruned tree has 4 leaves
#let's plot the pruned tree
windows()
plot(df.tree,type="uniform")
text(df.tree,col="blue",label=c("yval"),cex=2.0)
title(main = 'pruned regression tree')

#passing the tree models (both unpruned and pruned tree) onto the test data and comparing RMSEs
tree.pred = predict(df.tree, newdata = test, type = 'vector')
temp.pred = predict(temp, newdata = test, type = 'vector')
sqrt(mean((test$GR6096 - tree.pred)^2))
sqrt(mean((test$GR6096 - temp.pred)^2))

#pcr regression
set.seed(100)
train2 = train[, -c(1,2,3)] #removing non-numerical values from the training data
pcr.fit=pcr(GR6096 ~ .,data=train2, scale=TRUE, validation="LOO")
validationplot(pcr.fit, val.type="MSEP", main="LOOCV",legendpos = "topright")
MSEPtable=MSEP(pcr.fit,estimate = "adjCV",ncomp=1:56)
which.min(MSEPtable$val) - 1
pcr.pred=predict(pcr.fit, newdata=test, ncomp=17)
sqrt(mean((test$GR6096-pcr.pred)^2))
