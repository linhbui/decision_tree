rm(list=ls())
require("rpart")

# PART 1
# Playing with ages for a set of trees
ages <- c(19, 23, 30, 30, 45, 25, 24, 20)
# Standard deviation, mean and median
sd(ages)
mean(ages)
median(ages)

# Compute SD without using the sd function
sqrt(sum((ages - mean(ages))^2) / (length(ages) - 1))

# SD when you add 10 to all values
sd(ages + 10)

# SD when you multiply 100 to all values
sd(ages * 100)

# add 70 to ages
ages <- c(ages, 70)
mean(ages)
median(ages)

# PART 2
# Compute the classification error on the test set when training on the training set for a tree of
# depth 5. The 61st column is the response and the other 60 columns are the predictors.

rm(list=ls())
train<-read.csv("sonar_train.csv",header=FALSE)
dim(train)

yin <- as.factor(train[,61])
xin <- train[,1:60]
fit <- rpart(yin~.,xin,control=rpart.control(maxdepth=5, minsplit=2))
trainErr <- (1-sum(yin==predict(fit,xin,type="class"))/length(yin))

plot(fit)
text(fit)
print(fit)
post(fit,file="")


#  How did this do on the Test Set?
test <- read.csv("sonar_test.csv",header=FALSE)
testansw <- as.factor(test[,61])
testobv <- test[,1:60]
dum <- predict(fit,testobv)
yhat <- rep(0.0,nrow(dum))
for(i in 1:nrow(dum)){
	yhat[i] <- 2*(which.max(dum[i,]) - 1) - 1			
}
testError <- (1-sum(testansw==yhat)/length(testansw))
trainErr
testError

# PART 3
# Wine quality data set. User rpart to create trees of range of different tree depths.
# Use cross validation to generate training error and test error for each tree depth
# Determine the best tree depth

rm(list=ls())
data <- read.csv('winequality-red.csv', header=TRUE, sep = ';')
train <- data

x <- data[,1:11]
#put quality score from wine taster into Y
y <- data[,12]

require(rpart)
# 10-fold cross validation
nxval <- 10
ndepth <- 20
trainOutput <- matrix(0.0,nrow = ndepth, ncol = 2)
testOutput <- matrix(0.0,nrow =ndepth, ncol = 2)
I <- seq(from = 1, to = nrow(train))

for(idepth in 1:ndepth){
	trainErr <- 0.0
	testErr <- 0.0
	for(ixval in seq(from =  1, to = nxval)){
		Iout <- which(I%%nxval == ixval%%nxval)
		trainIn <- train[-Iout,]
		trainOut <- train[Iout,]
		yin <- trainIn[,12]
		yout <- trainOut[,12]
		xin <- trainIn[,1:11]
		xout <- trainOut[,1:11]		
		fit <- rpart(yin~.,xin,control=rpart.control(maxdepth=idepth, minsplit=2,cp=-1))
    # here the predict function returns a number - the prediction
		yhat <- predict(fit,xin)
		dY <- yin - yhat
		# here the error is the Euclidean distance between
		# the actual and predicted values
		trainErr <- trainErr + sqrt(sum(dY*dY))/(length(yin))	
		yhat <- predict(fit,xout)
		dY <- yout - yhat
		testErr <- testErr + sqrt(sum(dY*dY))/(length(yout))	
		}
	trainOutput[idepth,1] <- idepth
	trainOutput[idepth,2] <- trainErr/nxval
	testOutput[idepth,1] <- idepth
	testOutput[idepth,2] <- testErr/nxval
}

maxval = max(testOutput[,2])

# Create the Model Complexity Graph
plot(trainOutput, ylim=c(0,maxval),
		main="Model Complexity",
		xlab="Model Complexity = Tree Depth",
		ylab="Prediction Error"
)
text(12, 1, paste("Min Error = ",signif(testOutput[which.min(testOutput[,2]),2],3),cat("\n"),"Best Depth = ",testOutput[which.min(testOutput[,2]),1]), cex = 0.8) 
legend("topright", c("test", "train"), col = c(2,1), pch=1)#,cex=0.6)
points(testOutput, col = 2)


index <- which.min(testOutput[,2])
testOutput[index,2] 
index
bestDepth <- testOutput[index,1]
bestDepth
resultError <-testOutput[index,2]
resultError

# model for the bestDepth
bestFit <- rpart(y~.,x,control=rpart.control(maxdepth=bestDepth, minsplit=2,cp=-1))
plot(bestFit)
text(bestFit)
print(bestFit)
post(bestFit,file="")

# The attribute at the root node is alcohol
# Scatter plot of Wine Quality Score vs Alcohol
plot(data[,11], data[,12], main='Wine Quality Score vs Alcohol', xlab='Alcohol', ylab='Quality Score')

# Correlation between each of the eleven attributes and wine quality
cor(data[,1:11], data$quality)
