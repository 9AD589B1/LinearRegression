#1. Given the regression output below for predicting y from x where n = 100, 
#   confirm the T score and the p-value, determine whether x is a significant predictor of y, 
#   and interpret the p-value in context.

#             Estimate	Std. Error	t-value	 Pr(>|t|)
# (Intercept) 19.8581	  6.1379	    3.24	   0.0017
# x	          0.2557	  0.1055	    2.42	   0.0172

rm(list=ls(all=TRUE)) 

slope <- 0.2557
stderror <- 0.1055
n <-  100 #sample size
degfreedom <- n - 2 #degrees of freedom is 100 - 2
t <- (slope - 0) / stderror #calculate t-statistic
#pvalue <- 2 * pt(-abs(t), degfreedom) #calculate p-value
pvalue <- 2 * pt(t, df = degfreedom, lower.tail = FALSE)
pvalue

#2. Calculate a 95% confidence interval for the slope given above.

tscore <- abs ( qt(0.025, df = degfreedom) )
CI <- c(slope - (tscore * stderror), slope + (tscore * stderror))
CI
