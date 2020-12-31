# test

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