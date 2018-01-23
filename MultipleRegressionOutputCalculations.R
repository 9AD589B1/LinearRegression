rm(list=ls(all=TRUE)) 

cognitive <- read.csv("http://bit.ly/dasi_cognitive")
cog_full <- lm(kid_score ~ mom_hs + mom_iq + mom_work + mom_age, data = cognitive)
summary(cog_full)

#Verify the T score and the p-value for the slope of mom_hs

slope <- 5.09482
stderror <- 2.3145
n <-  434 #sample size
degfreedom <- n - 4 - 1 #degrees of freedom is 434 minus the number of predictors minus 1
t <- (slope - 0) / stderror
pvalue <- 2 * pt(t, df = degfreedom, lower.tail = FALSE)

#Calculate the 95% confidence interval for the slope of mom_work

slope <- 2.53718
stderror <- 2.35067
tscore <- abs ( qt(0.025, df = degfreedom) )
CI <- c(slope - (tscore * stderror), slope + (tscore * stderror))
CI

regsub <-
  regsubsets(
    kid_score ~ mom_hs + mom_iq + mom_work + mom_age,
    data = cognitive,
    nbest=1,
    nvmax = 3,
    method = "backward"
  )
summary(regsub)

library(olsrr)
ols_best_subset(cog_full)

MultiRegression1 <- lm(audience_score ~ mpaa_rating + critics_score + imdb_rating + runtime, data = movies)
summary(MultiRegression1)

ols_best_subset(MultiRegression1)