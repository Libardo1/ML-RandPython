library(dplyr)






trainTestSplit <- function(df,trainPercent,seed1){
    ## Sample size percent
    samp_size <- floor(trainPercent/100 * nrow(df))
    
    ## set the seed to make your partition reproductible
    ## set the seed to make your partition reproductible
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

crimesDF <- read.csv("communities_data.csv")
b <- read.table("names.txt",header=FALSE,sep=" ")
b1 <-as.character(b[,2])

names(crimesDF) <- b1
write.csv(crimesDF,file="crimes.csv")
crimesDF <- read.csv("crimes.csv",stringsAsFactors = FALSE)
names(crimesDF)
crimesDF1 <- crimesDF[,7:length(crimesDF)]

train_idx <- trainTestSplit(crimesDF1,trainPercent=75,seed=5)
train <- crimesDF1[train_idx, ]
test <- crimesDF1[-train_idx, ]

fit <- lm(ViolentCrimesPerPop~.,data=crimesDF1)
Rsquared(fit,test,test$ViolentCrimesPerPop)
