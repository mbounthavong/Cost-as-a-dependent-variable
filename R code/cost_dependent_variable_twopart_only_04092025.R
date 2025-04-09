################################################################################
## Title:       Cost as a Dependent Variable - Two-part model
## Programmer:  Mark Bounthavong
## Date:        29 April 2023
## Program:     R
## Version:     4.2.3.
## Updated:     09 April 2025
## Updated by:  Mark Bounthavong
################################################################################


#### Some helpful links:
# url: https://devinincerti.com/2015/09/11/twopart.html
# url: https://rdrr.io/cran/twopartm/f/README.md


#### Clear environment
rm(list = ls())

#### Load libraries
library("twopartm")

#### Import data
data1 <- read.csv("https://raw.githubusercontent.com/mbounthavong/Cost-as-a-dependent-variable/main/Data/limited_data.csv")
str(data1)


#### Average cost RAW
summary(data1$totexp17)
hist(data1$totexp17)

#### Convert cost to numeric
data1$totexp17 <- as.numeric(data1$totexp17)


#### Two-part model:
tpm.model1 <- tpm(totexp17 ~ age17x + sex + racev2x + hispanx + marry17x + povcat17, data = data1, link_part1 = "logit", family_part2 = Gamma(link = "log"))
summary(tpm.model1)

predict1 <- predict(tpm.model1, se.fit = TRUE)
summary(predict1$fit)

