requiredPackages <- c("ggplot2", "car", "MASS")
for (pac in requiredPackages) {
    if(!require(pac,  character.only=TRUE)){
        install.packages(pac, repos="http://cran.rstudio.com")
        library(pac,  character.only=TRUE)
    } 
}
rm(pac)
rm(requiredPackages)

# Set WD and load data
wd = getwd()
if(grepl("nora", wd)) {
    setwd("~/Documents/18-19/ASM/HW/02")
} 
rm(wd)

load("ais.Rdata")
View(ais)
head(ais)

plot(ais)
summary(ais)

attach(ais)
table(sport,sex)

ggplot(ais, aes(x=sport, y=pcBfat, fill=sex)) +
    geom_boxplot()

# linear model considering pcBfat as response variable and sex, sport and ssf 
# as explanatory variables.
model1<-lm(pcBfat~ssf+sex+sport)
summary(model1)

model2<-lm(pcBfat~ssf+sex)
summary(model2)


# 1./ Deduce which is the change in the pcBfat by changing one unit the ssf?
# 
# pcBFat(x)  = 0.15x (ssf) - 2.98x (sexm) + 4.115
# By changing one unit the ssf we pcBFat changes 0.15 each time.

# 2./ Which is the pcBfat predicted for a male with a ssf of 54?
predict(model2, newdata = data.frame(sex = "m", ssf=54))

# 3) Which is the pcBfat predicted for a female with a sff of 48?
predict(model2, newdata = data.frame(sex = "f", ssf=48))

# 4) Which has to be the ssf of a male in order to have a pcBfat of 14.7? and for a female?
model3<-lm(ssf~pcBfat+sex)
summary(model3)

predict(model3, newdata = data.frame(sex = "m", pcBfat=14.7))
predict(model3, newdata = data.frame(sex = "f", pcBfat=14.7))

# 5) Interpret the standard error value.