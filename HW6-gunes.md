# Homework 6
Gunes Alkan  
November 18, 2015  



```r
#First load required packages
library(class)
library(gmodels)
```

Now, let's read in the data, and see characteristics of variables:



```r
esteem <- read.csv("E:/ex1223.csv", header=TRUE)
str(esteem)
```

```
## 'data.frame':	2584 obs. of  32 variables:
##  $ Subject       : int  2 6 7 8 9 13 16 17 18 20 ...
##  $ Imagazine     : int  1 0 1 1 1 1 1 1 1 1 ...
##  $ Inewspaper    : int  1 1 1 1 1 1 1 1 1 1 ...
##  $ Ilibrary      : int  1 1 1 1 1 1 1 1 1 1 ...
##  $ MotherEd      : int  5 12 12 9 12 12 12 12 12 12 ...
##  $ FatherEd      : int  8 12 12 6 10 16 12 15 16 18 ...
##  $ FamilyIncome78: int  20000 35000 8502 7227 17000 20000 48000 15000 4510 50000 ...
##  $ Race          : int  3 3 3 3 3 3 3 3 3 3 ...
##  $ Gender        : Factor w/ 2 levels "female","male": 1 2 2 1 2 2 1 2 2 1 ...
##  $ Educ          : int  12 16 12 14 14 16 13 13 13 17 ...
##  $ Science       : int  6 23 14 18 17 16 13 19 22 21 ...
##  $ Arith         : int  8 30 14 13 21 30 17 29 30 17 ...
##  $ Word          : int  15 35 27 35 28 29 30 33 35 28 ...
##  $ Parag         : int  6 15 8 12 10 13 12 13 14 14 ...
##  $ Numer         : int  29 45 32 24 40 36 49 35 48 39 ...
##  $ Coding        : int  52 68 35 48 46 30 58 58 61 54 ...
##  $ Auto          : int  9 21 13 11 13 21 11 18 21 18 ...
##  $ Math          : int  6 23 11 4 13 24 17 21 23 20 ...
##  $ Mechanic      : int  10 21 9 12 13 19 11 19 16 20 ...
##  $ Elec          : int  5 19 11 12 15 16 10 16 17 13 ...
##  $ AFQT          : num  6.84 99.39 47.41 44.02 59.68 ...
##  $ Income2005    : int  5500 65000 19000 36000 65000 8000 71000 43000 120000 64000 ...
##  $ Esteem1       : int  1 2 2 1 1 1 2 2 2 1 ...
##  $ Esteem2       : int  1 1 1 1 1 1 2 2 2 1 ...
##  $ Esteem3       : int  4 4 3 3 4 4 3 3 3 3 ...
##  $ Esteem4       : int  1 2 2 2 1 1 2 2 2 1 ...
##  $ Esteem5       : int  3 4 3 3 1 4 3 3 3 3 ...
##  $ Esteem6       : int  3 2 2 2 1 1 2 2 2 2 ...
##  $ Esteem7       : int  1 2 2 3 1 1 3 2 2 1 ...
##  $ Esteem8       : int  3 4 2 3 4 4 3 3 3 3 ...
##  $ Esteem9       : int  3 3 3 3 4 4 3 3 3 3 ...
##  $ Esteem10      : int  3 4 3 3 4 4 3 3 3 3 ...
```

```r
esteem$Esteem1 <- as.factor(esteem$Esteem1)
head(esteem)
```

```
##   Subject Imagazine Inewspaper Ilibrary MotherEd FatherEd FamilyIncome78
## 1       2         1          1        1        5        8          20000
## 2       6         0          1        1       12       12          35000
## 3       7         1          1        1       12       12           8502
## 4       8         1          1        1        9        6           7227
## 5       9         1          1        1       12       10          17000
## 6      13         1          1        1       12       16          20000
##   Race Gender Educ Science Arith Word Parag Numer Coding Auto Math
## 1    3 female   12       6     8   15     6    29     52    9    6
## 2    3   male   16      23    30   35    15    45     68   21   23
## 3    3   male   12      14    14   27     8    32     35   13   11
## 4    3 female   14      18    13   35    12    24     48   11    4
## 5    3   male   14      17    21   28    10    40     46   13   13
## 6    3   male   16      16    30   29    13    36     30   21   24
##   Mechanic Elec   AFQT Income2005 Esteem1 Esteem2 Esteem3 Esteem4 Esteem5
## 1       10    5  6.841       5500       1       1       4       1       3
## 2       21   19 99.393      65000       2       1       4       2       4
## 3        9   11 47.412      19000       2       1       3       2       3
## 4       12   12 44.022      36000       1       1       3       2       3
## 5       13   15 59.683      65000       1       1       4       1       1
## 6       19   16 72.313       8000       1       1       4       1       4
##   Esteem6 Esteem7 Esteem8 Esteem9 Esteem10
## 1       3       1       3       3        3
## 2       2       2       4       3        4
## 3       2       2       2       3        3
## 4       2       3       3       3        3
## 5       1       1       4       4        4
## 6       1       1       4       4        4
```

Let's construct a new binary variable from this variable, which takes the value 1 for strong agreement and 0 for agreement, disagreement, or strong disagreement. (combine 4 levels into 2), also transform Income2005 variable:


```r
## create a new var
esteem$estnew <- ifelse((esteem$Esteem1 == 1), 1, 0)
esteem$estnew <- as.factor(esteem$estnew)
## log annual income
esteem$Income2005 <- log(esteem$Income2005)
```

We'll now explore the dependence of the probability that a person has positive self-esteem on log annual income (Income2005), intelligence (AFQT), years of education (educ), and gender.

Let's fit rogistic model first:


```r
## fit logistic model
attach(esteem)
myfit <- glm(estnew ~ Income2005 + AFQT + Educ + Gender, data = esteem, family = "binomial")
summary(myfit)
```

```
## 
## Call:
## glm(formula = estnew ~ Income2005 + AFQT + Educ + Gender, family = "binomial", 
##     data = esteem)
## 
## Deviance Residuals: 
##     Min       1Q   Median       3Q      Max  
## -1.7891  -1.2240   0.8174   1.0410   1.6339  
## 
## Coefficients:
##              Estimate Std. Error z value Pr(>|z|)    
## (Intercept) -2.773457   0.485833  -5.709 1.14e-08 ***
## Income2005   0.165970   0.047114   3.523 0.000427 ***
## AFQT         0.007603   0.001842   4.128 3.65e-05 ***
## Educ         0.076135   0.021045   3.618 0.000297 ***
## Gendermale  -0.146214   0.087097  -1.679 0.093201 .  
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## (Dispersion parameter for binomial family taken to be 1)
## 
##     Null deviance: 3508.3  on 2583  degrees of freedom
## Residual deviance: 3395.5  on 2579  degrees of freedom
## AIC: 3405.5
## 
## Number of Fisher Scoring iterations: 4
```

```r
confint(myfit)  ## adds confidence intervals
```

```
## Waiting for profiling to be done...
```

```
##                    2.5 %      97.5 %
## (Intercept) -3.730815455 -1.82512175
## Income2005   0.073787914  0.25869833
## AFQT         0.003998658  0.01121973
## Educ         0.035034318  0.11756275
## Gendermale  -0.317172867  0.02431818
```

Looking at the output of logistic procedure, the data provides evidence that at the alpha=0.05 level of significance gender is not a significant estimator (or classifier) of the self-esteem (p-value=0.0932).
The quantitative effects of best classifiers as follows:

It's estimated that each 1 unit increase in log of Income2005 is associated with 18% increase in the odds of the highest self esteem response (95% CI: (7.38, 25.9))

Each one percentile increase in AFQT score is associated with an estimated 0.76% increase in the odds of highest self esteem response. (95% Confidence Interval: (0.40%, 1.12%)).

Associated with each one additional year of education was estimated 7.6% increase in the odds of highest self esteem response( Confidence Interval: 3.5%, 11.7%).

(ALthough gender is not a good estimator, let's make its interpretation too. After accounting for all other variables, the odds of highest self esteem response were estimated to be 14.6% lower for males than for females.)

(In summary, not all variables are needed to obtain the best classification for this dataset.)

Now let's continue with dividing dataset into two group: training and testing set for our best classifiers:


```r
## divide esteem into two groups:test and train
ind <- sample(2, nrow(esteem), replace=TRUE, prob=c(0.67, 0.33))
esteem.train <- esteem[ind==1, c("Income2005" , "AFQT" , "Educ" )]
head(esteem.train)
```

```
##   Income2005   AFQT Educ
## 1   8.612503  6.841   12
## 2  11.082143 99.393   16
## 3   9.852194 47.412   12
## 5  11.082143 59.683   14
## 6   8.987197 72.313   16
## 7  11.170435 50.283   13
```

```r
esteem.test <- esteem[ind==2,c("Income2005" , "AFQT" , "Educ" ) ]
head(esteem.test)
```

```
##    Income2005   AFQT Educ
## 4    10.49127 44.022   14
## 9    11.69525 95.977   13
## 10   11.06664 67.021   17
## 14   11.92127  6.069   13
## 21   10.64542 78.095   19
## 22   11.10208 75.473   12
```

Lastly, let's obtain corresponding table :


```r
## create labels for the table
esteem.trainLabels <- esteem[ind==1, "estnew"]
esteem.testLabels <- esteem[ind==2, "estnew"]
esteem_pred <- knn(train = esteem.train, test = esteem.test, cl = esteem.trainLabels , k=2)
## plot the table
CrossTable(x = esteem.testLabels, y = esteem_pred, prop.chisq=FALSE)
```

```
## 
##  
##    Cell Contents
## |-------------------------|
## |                       N |
## |           N / Row Total |
## |           N / Col Total |
## |         N / Table Total |
## |-------------------------|
## 
##  
## Total Observations in Table:  867 
## 
##  
##                   | esteem_pred 
## esteem.testLabels |         0 |         1 | Row Total | 
## ------------------|-----------|-----------|-----------|
##                 0 |       171 |       195 |       366 | 
##                   |     0.467 |     0.533 |     0.422 | 
##                   |     0.449 |     0.401 |           | 
##                   |     0.197 |     0.225 |           | 
## ------------------|-----------|-----------|-----------|
##                 1 |       210 |       291 |       501 | 
##                   |     0.419 |     0.581 |     0.578 | 
##                   |     0.551 |     0.599 |           | 
##                   |     0.242 |     0.336 |           | 
## ------------------|-----------|-----------|-----------|
##      Column Total |       381 |       486 |       867 | 
##                   |     0.439 |     0.561 |           | 
## ------------------|-----------|-----------|-----------|
## 
## 
```






