library("data.table")
library("glmnet")

library("glmnet")
library("caret")
library("pROC")
library("ROCR") 
spam <- read.table("spambase/spambase.data",sep=",")

spam.names <- c(read.table("spambase/spambase.names",sep=":",skip=33,nrows=53,as.is=TRUE)[,1],
                "char_freq_#",
                read.table("spambase/spambase.names",sep=":",skip=87,nrows=3,as.is=TRUE)[,1],
                "spam.01")

names(spam) <- spam.names 

n<-dim(spam)[1]
p<-dim(spam)[2]-1

spam.01 <- spam[,p+1]
spam.vars <- as.matrix(spam[,1:p])

cat(paste("n = ",n,', p = ',p,sep=""))
cat(paste("Proportion of spam e-mails =",round(mean(spam.01),2),sep=""))

glm.spam <- glm(spam.01 ~ spam.vars,family=binomial)
summary(glm.spam)

totalNumObs = nrow(spam)

mail.spam = spam[which(spam$spam.01 == 1), ]
mail.non.spam = spam[which(spam$spam.01 == 0), ]


## Proportions of Spam
train.prop.spam = 2*nrow(mail.spam)/3

spam.training = mail.spam[1:train.prop.spam,]
spam.val = mail.spam[train.prop.spam:nrow(mail.spam),]


## Proportions non-spam
train.prop.no.spam = 2*nrow(mail.non.spam)/3

non.spam.training = mail.non.spam[1:train.prop.no.spam,]
non.spam.val = mail.non.spam[train.prop.no.spam:nrow(mail.non.spam),]


## Merging

train.set = rbind(spam.training, non.spam.training)
val.set = rbind(spam.val, non.spam.val)
train.set$spam.01 <-  as.factor(train.set$spam.01)
val.set$spam.01 <-  as.factor(val.set$spam.01)

## Scaling and centering

x.train <- as.matrix(train.set[, 1:57])
y.train <- as.factor(train.set$spam.01)

# Scale validation set
x.val <- as.matrix(val.set[, 1:57])
y.val <- as.factor(val.set$spam.01)

table(y.val)

m1 <- glm(spam.01 ~ .,data=train.set, family=binomial)
## you don't need to worry about this warning.  
## It says that some covariates are nearly perfect predictors.

plot(m1$fit~train.set$spam.01, 
     xlab="", ylab=c("fitted probability of spam"), 
     col=c("navy","red"))

y.pred <- predict(m1, as.data.frame(x.val), type="response")
summary(y.pred)

y_pred_num <- ifelse(y.pred < 0.5, 0, 1)
table(y_pred_num)
plot(1:length(y.val), y.val, col="red")
points(1:length(y.val), y_pred_num, col="blue")

cm = confusionMatrix(data = as.factor(y_pred_num), as.factor(val.set$spam.01))
cm$table
cm$byClass
cm$overall

m2 <- cv.glmnet(x.train, y.train, alpha=1, family = "binomial", standardize=FALSE, nfolds=10)
plot(m2)
#m2 <- glmnet(x.train, y.train, alpha=1, family = "binomial", standardize=FALSE)
y.pred <- predict(m2, x.val, type="response")
y_pred_num <- ifelse(y.pred < 0.5, 0, 1)

cm = confusionMatrix(data = as.factor(y_pred_num), as.factor(val.set$spam.01))
cm$table
cm$byClass
cm$overall
