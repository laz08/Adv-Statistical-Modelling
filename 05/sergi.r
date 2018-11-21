library("data.table")
library("glmnet")

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


## Scaling and centering
sc.train.set <- scale(train.set[, 1:57])
x.train <- as.matrix(sc.train.set)
y.train <- as.matrix(train.set[, 58])


mat.train.set <- as.matrix(train.set)
mean.train.set <- mean(mat.train.set)
sd.train.set <- sd(mat.train.set)

# Scale validation set
x.val <- (val.set[, 1:57] - mean.train.set)/sd.train.set
y.val <- as.matrix(val.set$spam.01)

log.reg.fit <- glm(spam.01 ~ ., data=train.set)
y.pred <- predict(log.reg.fit, x.val)
