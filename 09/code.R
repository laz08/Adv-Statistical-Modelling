library(mgcv)

hirs <- read.table("hirsutism.dat",header=T, sep="\t",fill=TRUE)

hirs$Treatment <- as.factor(hirs$Treatment)

summary(hirs)
head(hirs)
attach(hirs)

boxplot(hirs[,2:5])

par(mfrow=c(2,2))
boxplot(hirs[,2]~Treatment,ylim=c(0,30))
boxplot(hirs[,3]~Treatment,ylim=c(0,30))
boxplot(hirs[,4]~Treatment,ylim=c(0,30))
boxplot(hirs[,5]~Treatment,ylim=c(0,30))
par(mfrow=c(1,1))

par(mfrow=c(2,2))
boxplot(hirs[Treatment==0,2:5],ylim=c(0,30))
boxplot(hirs[Treatment==1,2:5],ylim=c(0,30))
boxplot(hirs[Treatment==2,2:5],ylim=c(0,30))
boxplot(hirs[Treatment==3,2:5],ylim=c(0,30))
par(mfrow=c(1,1))

# Lets start with a simple linear model
am1.0 <- gam(FGm12 ~ weight + height + DiaPres + SysPres + FGm0 + Treatment, data=hirs)
am1.0
summary(am1.0)
# It seems that the weight, heigh, diaPres and SysPres are not relevant, lets try it without them to achieve a simpler model.
am1.1 <- gam(FGm12 ~FGm0 + Treatment, data=hirs)
am1.1
summary(am1.1)

# The model does indeed look better.

am1.2 <- gam(FGm12 ~ s(FGm0, by=Treatment) + Treatment, data=hirs)
am1.2
summary(am1.2)
plot.gam(am1.2, page=1, residuals=TRUE, shade=TRUE) 
vis.gam(am1.2)
# This is quite intersting, teratment 0 is the only one that does not seem to be linear.


am1.3 <- gam(FGm12 ~ s(FGm0, by=Treatment) + te(weight, height), data=hirs)
am1.3
summary(am1.3)
plot.gam(am1.3, page=1, residuals=TRUE, shade=TRUE) 
# Once again it appears that the weight and height are not relevant.

am1.4 <- gam(FGm12 ~ s(FGm0, by=Treatment) + s(weight) + s(height), data=hirs)
am1.4
summary(am1.4)
plot.gam(am1.4, page=1, residuals=TRUE, shade=TRUE) 

#### Begin ANOVA ####

anova(am1.1,am1.2,test="F") # Used to compare the different models, null hypotesis that model 1 is correct
# Model 1.2 is better.



