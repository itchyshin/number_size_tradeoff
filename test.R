# test

rm(list=ls())

library(metafor)
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

# simple model
mod0 <- rma.mv(yi, vi, mod = ~ factor(Data.Extraction), 
               random = list(  ~ 1 | Article, ~1 | obs), 
               data=dat1)
summary(mod0)

AIC(mod0)

# model heteroscadecity
mod1 <- rma.mv(yi, vi, mod = ~ factor(Data.Extraction) -1, 
               random = list(  ~ 1 | Article, ~ factor(Data.Extraction) | obs), 
               struct="HCS",
               rho = 0, data=dat1)
summary(mod1)

mod2 <- rma.mv(yi, vi, mod = ~ factor(Data.Extraction) - 1, 
               random = list(  ~ 1 | Article, ~ factor(Data.Extraction) | obs), 
               struct="HCS", data=dat1)
summary(mod2)


mod3 <- rma.mv(yi, vi, mod = ~ factor(Data.Extraction) - 1, 
               random = list(  ~ 1 | Article, ~ factor(Data.Extraction) | obs), 
               struct="DIAG", data=dat1)
summary(mod2)
