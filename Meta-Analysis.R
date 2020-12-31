rm(list=ls())

library(metfor)
library(ape)
#library(here)

############################

# Correlation between the size of offspring and the number of offspring

dat1<-read.csv("O-O Unadjusted.csv")
tree1<-read.tree("O-O Unadjusted.tre")
tree1 <- compute.brlen(tree1)
A <- vcv(tree1, cor = T)


dat1 <- escalc(measure = "ZCOR", ri = dat1$r, ni = dat1$N, data = dat1)
dat1$phy <- dat1$animal
dat1$obs <- 1:dim(dat1)[1]

mod1 <- rma.mv(yi, vi, random = list( ~ 1 | phy,
                                      ~ 1 | animal, 
                                      ~ 1 | Article,
                                      ~ 1 | obs), 
               R=list(phy = A), data=dat1)
summary(mod1)


mod2 <- rma.mv(yi, vi, random = list( ~ 1 | phy,
                                      ~ 1 | Article,
                                      ~ 1 | obs), 
               R=list(phy = A), data=dat1)
summary(mod2)

#####################################


dat2<-read.csv("O-O Adjusted.csv")
tree2<-read.tree("O-O Adjusted.tre")
dat1$Article<-as.factor(dat1$Article)

tree2 <- compute.brlen(tree2)
B <- vcv(tree2, cor = T)


dat2 <- escalc(measure = "ZCOR", ri = dat2$r, ni = dat2$N, data = dat2)
dat2$phy <- dat2$animal
dat2$obs <- 1:dim(dat2)[1]

mod3 <- rma.mv(yi, vi, random = list( ~ 1 | phy,
                                      ~ 1 | animal, 
                                      ~ 1 | Article,
                                      ~ 1 | obs), 
               R=list(phy = B), data=dat2)
summary(mod3)


mod4 <- rma.mv(yi, vi, random = list( ~ 1 | phy,
                                      ~ 1 | Article,
                                      ~ 1 | obs), 
               R=list(phy = B), data=dat2)
summary(mod4)

mod5 <- rma.mv(yi, vi, random = list( ~ 1 | Article,
                                      ~ 1 | obs), 
               R=list(phy = B), data=dat2)
summary(mod5)
