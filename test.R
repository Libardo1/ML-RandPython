library(dplyr)






trainTestSplit <- function(df,trainPercent,seed1){
    ## Sample size percent
    samp_size <- floor(trainPercent/100 * nrow(df))
    ## set the seed 
    set.seed(seed1)
    idx <- sample(seq_len(nrow(df)), size = samp_size)
    idx

}

# For simple linear regression
df1 <- crimesDF %>% select(medIncome,ViolentCrimesPerPop)
train_idx <- trainTestSplit(df1,trainPercent=75,seed=5)
train <- df1[train_idx, ]
fit <- lm(ViolentCrimesPerPop~medIncome,data=df1)
summary(fit)
names(fit)
plot(df1$medIncome,df1$ViolentCrimesPerPop)

df=read.csv("Boston.csv",stringsAsFactors = FALSE) # Data from MASS - SL
train_idx <- trainTestSplit(df,trainPercent=75,seed=5)
train <- df[train_idx, ]
test <- df[-train_idx, ]


fit=lm(medv~lstat,data=df)
fit
summary(fit)
names(fit)
coef(fit)
confint(fit)

predict(fit,data.frame(lstat=(c(5,10,15))), interval="confidence")
predict(fit,data.frame(lstat=(c(5,10,15))), interval="prediction")
plot(df$lstat,df$medv)
abline(fit)
abline(fit,lwd=3)
abline(fit,lwd=3,col="red")

summary(fit)

pred <- predict(fit,newdata=test)
actual <- test$medv
RSS <- sum((actual - pred)^2)
TSS <- sum((actual - mean(actual))^2)
rsquared <-1 - (RSS/TSS)

Rsquared <- function(lmfit,newdf,y){
  yhat <- predict(lmfit,newdata=newdf)
  RSS <- sum((y - yhat)^2)
  TSS <- sum((y - mean(y))^2)
  rsquared <-1 - (RSS/TSS)
  rsquared
}


#########################################
#All data

#crimesDF <- read.csv("communities_data.csv")
b <- read.table("names.txt",header=FALSE,sep=" ")
b1 <-as.character(b[,2])

names(crimesDF) <- b1
write.csv(crimesDF,file="crimes.csv")

##################
crimesDF <- read.csv("crimes.csv",stringsAsFactors = FALSE)
names(crimesDF)
#crimesDF1 <- sapply(crimesDF,as.numeric)
crimesDF1 <- crimesDF[,7:length(crimesDF)]

# Conert all to numeric
crimesDF2 <- sapply(crimesDF1,as.numeric)

# Check for NAs
a <- is.na(crimesDF2)
# Set to 0 as an imputation
crimesDF2[a] <-0
crimesDF2 <- as.data.frame(crimesDF2)
train_idx <- trainTestSplit(crimesDF2,trainPercent=75,seed=5)
train <- crimesDF2[train_idx, ]
test <- crimesDF2[-train_idx, ]

fit <- lm(ViolentCrimesPerPop~poly(names(train)[1:122],2),data=train)
fit <- lm(ViolentCrimesPerPop~poly(as.matrix(train[1:120],2)),data=train)
fit.2 <- lm(ViolentCrimesPerPop~.,data=train)
summary(fit)
          
Rsquared(fit,test,test$ViolentCrimesPerPop)


####################################################


x=as.matrix(crimesDF2[1:122])
poly(x,2)
fit <- lm(ViolentCrimesPerPop~poly(as.matrix(train[1:10]),2),data=train)


df=read.csv("Boston.csv",stringsAsFactors = FALSE) # Data from MASS - SL
train_idx <- trainTestSplit(df,trainPercent=75,seed=5)
train <- df[train_idx, ]
test <- df[-train_idx, ]


fit=lm(medv~.,data=df)
fit

x=as.matrix(df[1:length(df)-1])
X_poly=poly(x,2,raw=TRUE)
fit <- lm(medv~ poly(x,2,raw=TRUE),data=df)



#####################################
df=read.csv("Boston.csv",stringsAsFactors = FALSE) # Data from MASS - SL
df1 = as.matrix(df[1:14])
train_idx <- trainTestSplit(df,trainPercent=75,seed=5)
train <- df[train_idx, ]
test <- df[-train_idx, ]


x_train=as.matrix(train[1:length(train)-1])
X_train_poly=poly(x_train,2,raw=TRUE)
fit <- lm(medv~ X_train_poly,data=train)
summary(fit)


x_test=as.matrix(test[1:length(test)-1])
X_test_poly=poly(x_test,2,raw=TRUE)
Rsquared(fit,X_test_poly,test$medv)

yhat <- predict(fit,newdata=test)
RSS <- sum((y - yhat)^2)
TSS <- sum((y - mean(y))^2)
rsquared <-1 - (RSS/TSS)
rsquared


#**************************************
df=read.csv("Boston.csv",stringsAsFactors = FALSE) # Data from MASS - SL
x = as.matrix(df[1:14])
# Make poly of total Data frame before split
df1=as.data.frame(poly(x,2,raw=TRUE))
df2 <- cbind(df1,df[15])
train_idx <- trainTestSplit(df2,trainPercent=75,seed=5)
train <- df2[train_idx, ]
test <- df2[-train_idx, ]
# Fir the 
fit <- lm(medv~as.matrix(train[1:135]) ,data=train)
summary(fit)


x_test=as.matrix(test[1:length(test)-1])
X_test_poly=poly(x_test,2,raw=TRUE)
# Since predict requires same name assign as follows
train=test
Rsquared(fit,train,test$medv)

yhat <- predict(fit,newdata=train)
RSS <- sum((y - yhat)^2)

#####################################################################
#**************************************
df=read.csv("Boston.csv",stringsAsFactors = FALSE) # Data from MASS - SL

train_idx <- trainTestSplit(df,trainPercent=75,seed=5)
train <- df[train_idx, ]
test <- df[-train_idx, ]
# Fir the 
fit <- lm(medv~. ,data=train)
summary(fit)



train=test
Rsquared(fit,train,test$medv)


######################################################
#Poly-1
df=read.csv("auto_mpg.csv",stringsAsFactors = FALSE) # Data from UCI
df1 <- as.data.frame(sapply(df,as.numeric))

df2 <- df1 %>% select(cylinder,displacement, horsepower,weight, acceleration, year,mpg)
df3 <- df2[complete.cases(df2),]
train_idx <- trainTestSplit(df3,trainPercent=75,seed=5)
train <- df3[train_idx, ]
test <- df3[-train_idx, ]
# Fit the 
fit <- lm(mpg~. ,data=train)
summary(fit)

train=test
Rsquared(fit,test,test$mpg)

################
#poly 2
x = as.matrix(df3[1:6])
# Make poly of total Data frame before split
df4=as.data.frame(poly(x,2,raw=TRUE))
df5 <- cbind(df4,df3[7])
train_idx <- trainTestSplit(df5,trainPercent=75,seed=5)
train <- df5[train_idx, ]
test <- df5[-train_idx, ]
# Fir the 
fit <- lm(mpg~. ,data=train)
summary(fit)


# Since predict requires same name assign as follows
train=test
Rsquared(fit,train,test$mpg)

#####
#poly 3
x = as.matrix(df3[1:6])
# Make poly of total Data frame before split
df4=as.data.frame(poly(x,3,raw=TRUE))
df5 <- cbind(df4,df3[7])
train_idx <- trainTestSplit(df5,trainPercent=75,seed=5)
train <- df5[train_idx, ]
test <- df5[-train_idx, ]
# Fir the 
fit <- lm(mpg~. ,data=train)
summary(fit)
Rsquared(fit,test,test$mpg)


# Since predict requires same name assign as follows
train=test
Rsquared(fit,train,test$mpg)


######################
#####


train_idx <- trainTestSplit(df3,trainPercent=75,seed=5)
train <- df3[train_idx, ]
test <- df3[-train_idx, ]
# Fir the 
train.X=train[,1:6]
train.Y=train[,7]
test.X=test[,1:6]
test.Y=test[,7]
for(i in 1:4){
    knn=knn.reg(train.X,test.X,train.Y,k=i)
    a=knnRSquared(knn$pred,test.Y)
    print(a)
}

knn=knn.reg(train.X,test.X,train.Y,k=4)

#RSS <- sum((y - yhat)^2)
#TSS <- sum((y - mean(y))^2)
#rsquared <-1 - (RSS/TSS)
#rsquared

knnRSquared <- function(yhat=knn$pred,y){
    RSS <- sum((test.Y - yhat)^2)
    TSS <- sum((test.Y - mean(test.Y))^2)
    rsquared <-1 - (RSS/TSS)
    rsquared
}

knnRSquared(knn$pred,test.Y)

fit <- lm(mpg~. ,data=train)
summary(fit)
Rsquared(fit,test,test$mpg)


# Since predict requires same name assign as follows
train=test
Rsquared(fit,train,test$mpg)





MinMaxScaler <- function(df){
    minx=sapply(df,min)
    maxx=sapply(df,max)
    d=(t(df) -minx)/(maxx-minx)
    e=t(d)
    e
}

train.X.scaled=MinMaxScaler(train.X)
test.X.scaled=MinMaxScaler(test.X)

knn=knn.reg(train.X.scaled,test.X.scaled,train.Y,k=11)
knnRSquared(knn$pred,test.Y)
for(i in c(1,2,4,6,8,10,12,15,20,25,30)){
    knn=knn.reg(train.X.scaled,test.X.scaled,train.Y,k=i)
    a=knnRSquared(knn$pred,test.Y)
    print(a)
}
