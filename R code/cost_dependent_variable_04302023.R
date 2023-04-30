################################################################################
## Title:       Cost as a Dependent Variable
## Programmer:  Mark Bounthavong
## Date:        29 April 2023
## Program:     R
## Version:     4.2.2.
## Updated:     30 April 2023
## Updated by:  Mark Bounthavong
################################################################################


#### Some helpful links:
# url: https://devinincerti.com/2015/09/11/twopart.html
# url: https://rdrr.io/cran/twopartm/f/README.md


#### Clear environment
rm(list = ls())

#### Load libraries
library("twopartm")           ## Package to perform two-part models
library("haven")              ## Package to allow import of Stata *.dta file
library("ResourceSelection")  ## Package to perform the Hosmer-Lemeshow GOF test


#### Import data
data1 <- read_dta("https://github.com/mbounthavong/Cost-as-a-dependent-variable/blob/main/Data/limited_data.dta?raw=true")
str(data1)


#### Restrict data to adults age > = 18 years old
data1 <- data1[(data1$age17x >= "18"), ]
sort(data1$age17x)


#### Average cost RAW
summary(data1$totexp17)
hist(data1$totexp17)


#### Two-part model
tpm.model1 <- tpm(totexp17 ~ age17x + sex + racev2x + hispanx + marry17x + povcat17, data = data1, link_part1 = "logit", family_part2 = Gamma(link = "log"))
summary(tpm.model1)
AME(tpm.model1)
margin(tpm.model1)

#### Generated the estimated total costs for each subject and summarize for the sample
predict1 <- predict(tpm.model1, se.fit = TRUE)
summary(predict1$fit)
summary(predict1$se.fit)

psych::describe(predict1$fit, type = 2)


predict1$ci <- 1.96*predict1$se.fit
predict1$ll <- predict1$fit - predict1$ci
predict1$ul <- predict1$fit + predict1$ci
summary(predict1$ll)
summary(predict1$ul)


#### GOF tests
res <- data1$totexp17 - predict1$fit    ## Estimate the residuals
summary(res)

### Pearson correlation
pwcorr <- cor.test(predict1$fit, res)
pwcorr

### Pregibon's link test
xb2 <- predict1$fit * predict1$fit  ## Square the predicted values
linear.model <- glm(data1$totexp17 ~ predict1$fit + xb2, family = gaussian(link = "identity"))
summary(linear.model)

### Modified Hosmer-Lemeshow test with 10 deciles
hoslem.test(data1$totexp17, predict1$fit, g = 10)


