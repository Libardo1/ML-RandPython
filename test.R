library(dplyr)
crimesDF <- read.csv("communities_data.csv")
b <- read.table("names.txt",header=FALSE,sep=" ")
b1 <-as.character(b[,2])

names(crimesDF) <- b1
write.csv(crimesDF,file="crimes.csv")
crimesDF <- read.csv("crimes.csv",stringsAsFactors = FALSE)
names(crimesDF)






trainTestSplit <- function(df,trainPercent,seed1){
    ## Sample size percent
    samp_size <- floor(trainPercent/100 * nrow(df))
    
    ## set the seed to make your partition reproductible
    ## set the seed to make your partition reproductible
    set.seed(seed1)
    idx <- sample(seq_len(nrow(df)), size = samp_size)
    idx

}
train_idx <- trainTestSplit(crimesDF,trainPercent=75,seed=5)
train <- crimesDF[train_idx, ]
test <- crimesDF[-train_idx, ]

# For simple linear regression
df1 <- crimesDF %>% select(medIncome,ViolentCrimesPerPop)
train_idx <- trainTestSplit(df1,trainPercent=75,seed=5)
train <- df1[train_idx, ]
test <- df1[-train_idx, ]
