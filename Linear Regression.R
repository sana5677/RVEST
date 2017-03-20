rm(list=ls())

##--> installing the required packages

if (!'rstudioapi' %in% installed.packages()){
  install.packages('rstudioapi')
}

if (!'MASS' %in% installed.packages()){
  install.packages('MASS')
}

if (!'leaps' %in% installed.packages()){
  install.packages('leaps')
}

if (!'glmnet' %in% installed.packages()){
  install.packages('glmnet')
}

library(rstudioapi)
library(MASS)
library(leaps)
library(glmnet)

##--> picking the files from the same path as the .R file
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

##--> loading the scrapped data
denver <- read.csv("denver additional.csv")

denver$zipcode <- as.factor(denver$zipcode)

row.names(denver) <- denver$link

## --> Creating the traing and Test Samples
set.seed(200)
restaurant_test <- sample(denver$restaurant_id,nrow(denver)*0.20)

denver_test <- denver[(denver$restaurant_id %in% restaurant_test),c("price_range",
                                                                    "rating","reviews","zipcode",
                                                                    "dayselapsed","latitude",
                                                                    "longitude","final_category",
                                                                    "subcat_num","branch",
                                                                    "median_income","avg_income",
                                                                    "per_capita","population",
                                                                    "num_restaurants","ratio_ex_restaurant",
                                                                    "ratio_inex_restaurant","review_rate",
                                                                    "avg_dayselapsed")]
##--> creating denver1 and denver2 training samples
denver1 <- denver[!(denver$restaurant_id %in% restaurant_test),c("price_range",
                                                                 "rating","reviews","zipcode",
                                                                 "dayselapsed","latitude",
                                                                 "longitude","final_category")]


denver2 <- denver[!(denver$restaurant_id %in% restaurant_test),c("price_range",
                                                                 "rating","reviews","zipcode",
                                                                 "dayselapsed","latitude",
                                                                 "longitude","final_category",
                                                                 "subcat_num","branch",
                                                                 "median_income","avg_income",
                                                                 "per_capita","population",
                                                                 "num_restaurants","ratio_ex_restaurant",
                                                                 "ratio_inex_restaurant","review_rate",
                                                                 "avg_dayselapsed")]

##--> correlation between the variables in the training set
cor(denver1[,-c(4,8)])
cor(denver2[,-c(4,8)])



##--> Basic multiple linear regressions with interactions and transformations
n <- lm(rating~.,data=denver1)
summary(n)

## Model 1 ---> lm_all_variables
m <- lm(rating~.,data=denver2)
summary(m)
form_lm_all_variables = as.formula(m$call[[2]])
mat_lm_all_variables = model.matrix(form_lm_all_variables, denver_test)
coefi_lm_all_variables = ifelse(is.na(coef(m)),0,coef(m))
pred_lm_all_variables <-  mat_lm_all_variables[ ,names(coefi_lm_all_variables)] %*% coefi_lm_all_variables
mse_lm_all_variables <- sqrt(mean((pred_lm_all_variables-denver_test$rating)^2))


o <- lm(rating~.+final_category:zipcode,data=denver2)
summary(o)

p <- lm(rating~.+final_category:median_income,data=denver2)
summary(p)

q <- lm(rating~.+final_category:avg_income, data=denver2)
summary(q)

r <- lm(rating~.+final_category:population, data=denver2)
summary(r)

s <- lm(rating~.+price_range:reviews, data=denver2)
summary(s)

t <- lm(rating~.+dayselapsed:reviews, data=denver2)
summary(t)

u <- lm(rating~.+dayselapsed:avg_dayselapsed, data=denver2)
summary(u)

v <- lm(rating~.+final_category:avg_dayselapsed, data=denver2)
summary(v)

w <- lm(rating~.+final_category:per_capita, data=denver2)
summary(w)

z <- lm(rating~.+final_category:branch, data=denver2)
summary(z)

a <- lm(rating~.+zipcode:branch, data=denver2)
summary(a)

b <- lm(rating~.+price_range:branch, data=denver2)
summary(b)

c <- lm(rating~.+review_rate:final_category, data=denver2)
summary(c)

## Model 2 ---> lm_all_varaiables with transformation & interactions
d <- lm(I(rating^2)~.+review_rate:zipcode+final_category:zipcode, data=denver2)
summary(d)
## Model 2 --> MSE
form_lm_int_trans = as.formula(d$call[[2]])
mat_lm_int_trans = model.matrix(form_lm_int_trans, denver_test)
coefi_lm_int_trans = ifelse(is.na(coef(d)),0,coef(d))
pred_lm_int_trans <-  mat_lm_int_trans[ ,names(coefi_lm_int_trans)] %*% coefi_lm_int_trans
mse_lm_int_trans <- sqrt(mean((pred_lm_int_trans-(denver_test$rating)^2)^2))



j <- lm(rating~.+review_rate:zipcode+final_category:zipcode, data=denver2)
summary(j)

i <- lm(I(rating^2)~.+review_rate:zipcode+final_category:zipcode+final_category:avg_income, data=denver2)
summary(i)

e <- lm(rating~.+final_category:branch, data=denver2)
summary(e)

f <- lm(rating~.+branch:ratio_inex_restaurant, data=denver2)
summary(f)

g <- lm(rating~.+branch:zipcode, data=denver2)
summary(g)

h <- lm(rating~.+branch:median_income, data=denver2)
summary(h)

l <- lm(I(rating^2)~., data=denver2)
summary(l)


## --> Box Cox Transformation

boxcox(rating~.,lambda=seq(-7,7,1/10),data=denver2)

## Model 3 --> Forward Selection
regsubfit.fwd_denver2 <- regsubsets(I(rating^2)~.+review_rate:zipcode+final_category:zipcode, 
                                    data = denver2, nvmax = 10000,
                                    method = "forward")
regsubfit.fwd.summary_denver2 <- summary(regsubfit.fwd_denver2)
names(regsubfit.fwd.summary_denver2)

plot(regsubfit.fwd.summary_denver2$adjr2, xlab="# of variables", ylab="adjr2")
abline(v=99,col="red")
##--> Max AdjR2
which.max(regsubfit.fwd.summary_denver2$adjr2)
coef(regsubfit.fwd_denver2, 99)
regsubfit.fwd.summary_denver2$adjr2[99]

##--> negative mallow's cp
plot(regsubfit.fwd.summary_denver2$cp, xlab="# of variables", ylab="Cp")
which.min(regsubfit.fwd.summary_denver2$cp)

## Model 3 --> MSE

form_regsubfit.fwd = as.formula(regsubfit.fwd_denver2$call[[2]])
mat_regsubfit.fwd = model.matrix(form_regsubfit.fwd, denver_test)
coefi_regsubfit.fwd = ifelse(is.na(coef(regsubfit.fwd_denver2,99)),0,coef(regsubfit.fwd_denver2,99))
pred_regsubfit.fwd <-  mat_regsubfit.fwd[ ,names(coefi_regsubfit.fwd)] %*% coefi_regsubfit.fwd
mse_regsubfit.fwd <- sqrt(
  mean(
    (pred_regsubfit.fwd-(denver_test$rating)^2)
    ^2))

## Model 4 --> Forward_selection_lm
lm.fit.fwd <- lm(I(rating^2)~branch+dayselapsed+longitude+price_range+
                   reviews+subcat_num+zipcode+final_category+final_category:zipcode+
                   zipcode:review_rate,data=denver2)
summary(lm.fit.fwd)

## Model 4 --> MSE
form_lm.fit.fwd = as.formula(lm.fit.fwd$call[[2]])
mat_lm.fit.fwd = model.matrix(form_lm.fit.fwd, denver_test)
coefi_lm.fit.fwd = ifelse(is.na(coef(lm.fit.fwd)),0,coef(lm.fit.fwd))
pred_lm.fit.fwd <-  mat_lm.fit.fwd[ ,names(coefi_lm.fit.fwd)] %*% coefi_lm.fit.fwd
mse_lm.fit.fwd <- sqrt(mean((pred_lm.fit.fwd-(denver_test$rating)^2)^2))

## Model 5 --> Backward Selection
regsubfit.bwd_denver2 <- regsubsets(I(rating^2)~.+review_rate:zipcode+final_category:zipcode, 
                                    data = denver2, nvmax = 100000,
                                    method = "backward")
regsubfit.bwd.summary_denver2 <- summary(regsubfit.bwd_denver2)
names(regsubfit.bwd.summary_denver2)
plot(regsubfit.bwd.summary_denver2$adjr2, xlab="# of variables", ylab="adjr2")
abline(v=138,col="red")
## --> Max AdjR2
which.max(regsubfit.bwd.summary_denver2$adjr2)
names(coef(regsubfit.bwd_denver2, 138))
regsubfit.bwd.summary_denver2$adjr2[138]
## --> Negative Mallow's cp
plot(regsubfit.bwd.summary_denver2$cp, xlab="# of variables", ylab="Cp")
which.min(regsubfit.bwd.summary_denver2$cp)


## Model 5 --> MSE
form_regsubfit.bwd = as.formula(regsubfit.bwd_denver2$call[[2]])
mat_regsubfit.bwd = model.matrix(form_regsubfit.bwd, denver_test)
coefi_regsubfit.bwd = ifelse(is.na(coef(regsubfit.bwd_denver2,138)),0,coef(regsubfit.bwd_denver2,138))
pred_regsubfit.bwd <-  mat_regsubfit.bwd[ ,names(coefi_regsubfit.bwd)] %*% coefi_regsubfit.bwd
mse_regsubfit.bwd <- sqrt(
  mean(
    (pred_regsubfit.bwd-(denver_test$rating)^2)
    ^2))


## Model 6 --> Backward selection_lm
lm.fit.bwd <- lm(I(rating^2)~branch+dayselapsed+longitude+reviews+zipcode+final_category+
                   zipcode:final_category+zipcode:review_rate,data=denver2)
summary(lm.fit.bwd)

## Model 6 --> MSE
form_lm.fit.bwd = as.formula(lm.fit.bwd$call[[2]])
mat_lm.fit.bwd = model.matrix(form_lm.fit.bwd, denver_test)
coefi_lm.fit.bwd = ifelse(is.na(coef(lm.fit.bwd)),0,coef(lm.fit.bwd))
pred_lm.fit.bwd <-  mat_lm.fit.bwd[ ,names(coefi_lm.fit.bwd)] %*% coefi_lm.fit.bwd
mse_lm.fit.bwd <- sqrt(mean((pred_lm.fit.bwd-(denver_test$rating)^2)^2))


## Model 7 --> Ridge Regression

x_var <- model.matrix(I(rating^2)~.+review_rate:zipcode+final_category:zipcode, -1, data = denver2)
y_var <- (denver2$rating)^2

fit.ridge <- glmnet(x_var, y_var, alpha=0)
plot(fit.ridge, xvar="lambda", label=TRUE)

## --> Cross validation to find the best ridge regression model for the best lambda
cv.ridge <- cv.glmnet(x_var, y_var, alpha=0)
plot(cv.ridge)
coef(cv.ridge)


## Model 7--> MSE
form_ridge = as.formula("I(rating^2)~.+review_rate:zipcode+final_category:zipcode")
mat_ridge = model.matrix(form_ridge, denver_test)
coefi_ridge = coef(cv.ridge)[,1]
pred_ridge <-  mat_ridge[ ,names(coefi_ridge)] %*% coefi_ridge
mse_ridge <- sqrt(
  mean(
    (pred_ridge-(denver_test$rating)^2)
    ^2))

## Model 8 --> Ridge regression_lm
lm.fit.ridge <- lm(I(rating^2)~num_restaurants+population+ratio_inex_restaurant+
                     review_rate+subcat_num+zipcode+final_category+
                     zipcode:final_category+
                     zipcode:review_rate, data=denver2)
summary(lm.fit.ridge)

## Model 8 --> MSE
form_lm.fit.ridge = as.formula(lm.fit.ridge$call[[2]])
mat_lm.fit.ridge = model.matrix(form_lm.fit.ridge, denver_test)
coefi_lm.fit.ridge = ifelse(is.na(coef(lm.fit.ridge)),0,coef(lm.fit.ridge))
pred_lm.fit.ridge <-  mat_lm.fit.ridge[ ,names(coefi_lm.fit.ridge)] %*% coefi_lm.fit.ridge
mse_lm.fit.ridge <- sqrt(mean((pred_lm.fit.ridge-(denver_test$rating)^2)^2))


## Model 9--> Lasso regression
fit.lasso <- glmnet(x_var, y_var,alpha=1)
plot(fit.lasso, xvar="lambda", label=TRUE)

cv.lasso <- cv.glmnet(x_var, y_var,alpha=1)
plot(cv.lasso)

coef(cv.lasso)



## Model 9 --> MSE
form_lasso = as.formula("I(rating^2)~.+review_rate:zipcode+final_category:zipcode")
mat_lasso = model.matrix(form_lasso, denver_test)
coefi_lasso = coef(cv.lasso)[,1]
pred_lasso <-  mat_lasso[ ,names(coefi_lasso)] %*% coefi_lasso
mse_lasso <- sqrt(
  mean(
    (pred_lasso-(denver_test$rating)^2)
    ^2))

## Model 10 --> Lasso regression_lm
lm.fit.lasso <- lm(I(rating^2)~final_category+final_category:zipcode, data=denver2)
summary(lm.fit.lasso)

## Model 10 --> MSE
form_lm.fit.lasso = as.formula(lm.fit.lasso$call[[2]])
mat_lm.fit.lasso = model.matrix(form_lm.fit.lasso, denver_test)
coefi_lm.fit.lasso = ifelse(is.na(coef(lm.fit.lasso)),0,coef(lm.fit.lasso))
pred_lm.fit.lasso <-  mat_lm.fit.lasso[ ,names(coefi_lm.fit.lasso)] %*% coefi_lm.fit.lasso
mse_lm.fit.lasso <- sqrt(mean((pred_lm.fit.lasso-(denver_test$rating)^2)^2))


## Model 11 --> Forward selection_noTrans_lm
regsubfit.fwd_denver2_noTrans <- regsubsets(rating~., 
                                    data = denver2, nvmax = 10000,
                                    method = "forward")
regsubfit.fwd.summary_denver2_noTrans <- summary(regsubfit.fwd_denver2_noTrans)
names(regsubfit.fwd.summary_denver2_noTrans)

plot(regsubfit.fwd.summary_denver2_noTrans$adjr2, xlab="# of variables", ylab="adjr2")
abline(v=34,col="red")
##--> Max AdjR2
which.max(regsubfit.fwd.summary_denver2_noTrans$adjr2)
coef(regsubfit.fwd_denver2_noTrans, 34)
regsubfit.fwd.summary_denver2_noTrans$adjr2[34]

##Variables from the best Fwd selection model
lm.fit.fwd.noTrans <- lm(rating~price_range+reviews+dayselapsed+longitude+branch+avg_income+
                           population+ratio_inex_restaurant+final_category+zipcode,data=denver2)
summary(lm.fit.fwd.noTrans)

## Model 11 --> MSE

form_lm.fit.fwd.noTrans = as.formula(lm.fit.fwd.noTrans$call[[2]])
mat_lm.fit.fwd.noTrans = model.matrix(form_lm.fit.fwd.noTrans, denver_test)
coefi_lm.fit.fwd.noTrans = ifelse(is.na(coef(lm.fit.fwd.noTrans)),0,coef(lm.fit.fwd.noTrans))
pred_lm.fit.fwd.noTrans <-  mat_lm.fit.fwd.noTrans[ ,names(coefi_lm.fit.fwd.noTrans)] %*% coefi_lm.fit.fwd.noTrans
mse_lm.fit.fwd.noTrans <- sqrt(
  mean(
    (pred_lm.fit.fwd.noTrans-denver_test$rating)
    ^2))


## Model 12 --> Backward selection_noTrans_lm
regsubfit.bwd_denver2_noTrans <- regsubsets(rating~., 
                                            data = denver2, nvmax = 10000,
                                            method = "backward")
regsubfit.bwd.summary_denver2_noTrans <- summary(regsubfit.bwd_denver2_noTrans)
names(regsubfit.bwd.summary_denver2_noTrans)

plot(regsubfit.bwd.summary_denver2_noTrans$adjr2, xlab="# of variables", ylab="adjr2")
abline(v=32,col="red")
##--> Max AdjR2
which.max(regsubfit.bwd.summary_denver2_noTrans$adjr2)
coef(regsubfit.bwd_denver2_noTrans, 32)
regsubfit.bwd.summary_denver2_noTrans$adjr2[32]


##Variables from the best bwd selection model
lm.fit.bwd.noTrans <- lm(rating~price_range+reviews+dayselapsed+longitude+branch+
                           final_category+zipcode,data=denver2)
summary(lm.fit.bwd.noTrans)

## Model 12 --> MSE

form_lm.fit.bwd.noTrans = as.formula(lm.fit.bwd.noTrans$call[[2]])
mat_lm.fit.bwd.noTrans = model.matrix(form_lm.fit.bwd.noTrans, denver_test)
coefi_lm.fit.bwd.noTrans = ifelse(is.na(coef(lm.fit.bwd.noTrans)),0,coef(lm.fit.bwd.noTrans))
pred_lm.fit.bwd.noTrans <-  mat_lm.fit.bwd.noTrans[ ,names(coefi_lm.fit.bwd.noTrans)] %*% coefi_lm.fit.bwd.noTrans
mse_lm.fit.bwd.noTrans <- sqrt(
  mean(
    (pred_lm.fit.bwd.noTrans-denver_test$rating)
    ^2))

## Model 13 --> Ridge regression_lm_noTrans
x_var_noTrans <- model.matrix(rating~., -1, data = denver2)
y_var_noTrans <- denver2$rating
fit.ridge_noTrans <- glmnet(x_var_noTrans, y_var_noTrans,alpha = 0)
plot(fit.ridge_noTrans, xvar="lambda", label=TRUE)

cv.ridge_noTrans <- cv.glmnet(x_var_noTrans, y_var_noTrans,alpha=0)
plot(cv.ridge_noTrans)

coef(cv.ridge_noTrans)





## Model 13 --> MSE
form_fit.ridge.noTrans = as.formula("rating~.")
mat_fit.ridge.noTrans = model.matrix(form_fit.ridge.noTrans, denver_test)
coefi_fit.ridge.noTrans = ifelse(is.na(coef(cv.ridge_noTrans)[,1]),0,coef(cv.ridge_noTrans)[,1])
pred_fit.ridge.noTrans <-  mat_fit.lasso.noTrans[ ,names(coefi_fit.ridge.noTrans)] %*% coefi_fit.ridge.noTrans
mse_fit.ridge.noTrans <- sqrt(mean((pred_fit.ridge.noTrans-denver_test$rating)^2))


## Model 14 --> Lasso regression_lm_noTrans
fit.lasso_noTrans <- glmnet(x_var_noTrans, y_var_noTrans,alpha=1)
plot(fit.lasso_noTrans, xvar="lambda", label=TRUE)

cv.lasso_noTrans <- cv.glmnet(x_var_noTrans, y_var_noTrans,alpha=1)
plot(cv.lasso_noTrans)

coef(cv.lasso_noTrans)



## Model 14 --> MSE
form_fit.lasso.noTrans = as.formula("rating~.")
mat_fit.lasso.noTrans = model.matrix(form_fit.lasso.noTrans, denver_test)
coefi_fit.lasso.noTrans = ifelse(is.na(coef(cv.lasso_noTrans)[,1]),0,coef(cv.lasso_noTrans)[,1])
pred_fit.lasso.noTrans <-  mat_fit.lasso.noTrans[ ,names(coefi_fit.lasso.noTrans)] %*% coefi_fit.lasso.noTrans
mse_fit.lasso.noTrans <- sqrt(mean((pred_fit.lasso.noTrans-denver_test$rating)^2))

