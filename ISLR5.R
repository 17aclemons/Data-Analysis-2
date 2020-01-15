####1####
#Question 1) What is the probability that a bootstrap sample contains a given observation? Explain in precise detail, with math, in your own words.
# there is a 1/n (with n being the number of observations) that a given observation is in a sample. Similar to rolling a dice,
# getting a specific number is 1/6. 

####2####
#Question 2) What are the tradeoffs of using k-fold cross validation approach vs the validation set?

#Validation estimate of the test error rate can be highly varable, depending on which observations are in the 
#testing and training sets. The validation set error rate may overestimate the test error rate because it is training
#on fewer observations. The validation set is simple and easy to implement. K fold can give more accurate estimates
#for the test error rate due to only averaging a few models than averaging many. K fold is computationaly more expensive
#depending on which value you select for k. 

####3####
#Question 3) What are the tradeoffs of using k-fold cross validation approach vs leave one out cross validation?

#K fold is computationaly cheaper than LOOCV because it is running k models not n models. K fold tends to have
#a lower variance that LOOCV due to averaging a few different models than many similar ones. 

####4####
#Question 4) Now that you're familiar with bootstrapping, 
#walk through the process (in words) on how you would calculate the variance of a model prediction?

#
####5####
#Question 5) Create a model to estimate balance based on the variables from the ISLR::Default data set. Use the variables of your choosing.
#A) Compute the standard error of the median balance.
#B) Compute the 95% confidence interval of the standard error of your prediction



