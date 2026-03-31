################################################################################
## TITLE:       TWO-PART MODEL W/ BOOTSTRAP 95% CI
## PROGRAMMER   Mark Bounthavong
## DATE:        29 March 2026
## UPDATED:     31 March 2026
## UPDATED BY:  Mark Bounthavong
################################################################################

### CLEAR ENVIRONMENT
rm(list = ls())


### LOAD Libraries
library("MEPS")
library("tidyverse")
library("survey")
library("gmodels")
library("boot")


### LOAD DATA
hc2021 = read_MEPS(file = "h233")
names(hc2021) <- tolower(names(hc2021))   ## Convert column headers to lower case

### We'll parse the data to a few key variables
hc2021p = hc2021 %>%
  rename(
    age = age21x,
    totexp = totexp21,
    ertexp = ertexp21,
    perwt21f = perwt21f,
    varstr = varstr,
    varpsu = varpsu) %>%
  select(
    dupersid,
    age,
    sex,
    totexp,
    ertexp,
    perwt21f,
    varstr,
    varpsu)
hc2021p$year <- 2021

hc2021p <- filter(hc2021p, perwt21f > 0) # Only select individuals with a positive weight
nrow(hc2021p) # Check number of rows


### CHECK DISTRIBUTION of TOTEXP
hist_male <- hc2021p %>%
              filter(sex == 1)
hist(hist_male$totexp, breaks = 400, xlim = c(0, 100000))

### CLEAN DATA
#### Generate male variable
hc2021p$male[hc2021p$sex == 2] = 0
hc2021p$male[hc2021p$sex == 1] = 1
table(hc2021p$male)
CrossTable(hc2021p$male, hc2021p$sex)

#### Create a binary nonzero cost variable
hc2021p$nonzero[hc2021p$totexp == 0] = 0
hc2021p$nonzero[hc2021p$totexp >  0] = 1
table(hc2021p$nonzero)

#### Create 5-levels of age categories
hc2021p$agecat[hc2021p$age>=0  & hc2021p$age<30] = 0
hc2021p$agecat[hc2021p$age>=30 & hc2021p$age<40] = 1
hc2021p$agecat[hc2021p$age>=40 & hc2021p$age<50] = 2
hc2021p$agecat[hc2021p$age>=50 & hc2021p$age<60] = 3
hc2021p$agecat[hc2021p$age>=60 & hc2021p$age<99] = 4
hc2021p$agecat <- as.factor(hc2021p$agecat)
table(hc2021p$agecat)


### APPLY survey weight
options(survey.lonely.psu = 'adjust')

mepsdsgn = svydesign(
  id = ~varpsu,
  strata = ~varstr,
  weights = ~perwt21f,
  data = hc2021p,
  nest = TRUE)

################################################################################
### MOTIVATING EXAMPLE:
### In this example, we will estimate the difference in total expenditures 
### between males and non-males. 
###
### We also will estimate the difference in total expenditures at various 
### levels of age categories:
### agecat <  30
### agecat >= 30 & < 40
### agecat >= 40 & < 50
### agecat >= 50 & < 60
### agecat >= 60
################################################################################


#############################
### Two-part model
#############################

### PART 1 - Logit model
#### Run the logit model and predict the probabilities (include interaction term)
part1 <- svyglm(nonzero ~ male + agecat + male:agecat,
                design = mepsdsgn,
                family = quasibinomial(link = "logit"))

### PART 2 - Gamma model
#### Subset data with nonzero
mepsdsgn_nonzero <- subset(mepsdsgn, totexp > 0)

#### Run the gamma model and predict the probabilities (include interaction term)
part2 <- svyglm(totexp ~ male + agecat + male:agecat,
                design = mepsdsgn_nonzero,
                family = Gamma(link = "log"))

#### STORE OUTPUTS for each agecat levels by male/female
group0 <- data.frame(agecat = factor("0", 
                                     levels = levels(hc2021p$agecat)), male = 0)
group1 <- data.frame(agecat = factor("0", 
                                     levels = levels(hc2021p$agecat)), male = 1) 

#### PREDICTIONS
pred_part1_0 <- predict(part1, group0, type = "response")
pred_part1_1 <- predict(part1, group1, type = "response")

pred_part2_0 <- predict(part2, group0, type = "response")
pred_part2_1 <- predict(part2, group1, type = "response")

totexp0 <- pred_part1_0 * pred_part2_0
totexp1 <- pred_part1_1 * pred_part2_1

#### DIFFERENCE in costs between male and female
diff_groups <- totexp1 - totexp0
print(diff_groups)

#### On average, males spend less than non-males by -$228.


#################################
#### Bootstrap code
#################################

#### Need to use this to estimate the 95% CIs
twopm <- function(data, indices) {  
  hc2021p <- data[indices, ]
  
  ### SVY DESIGN
  ### APPLY survey weight
  options(survey.lonely.psu = 'adjust')
  mepsdsgn = svydesign(
    id = ~varpsu,
    strata = ~varstr,
    weights = ~perwt21f,
    data = hc2021p,
    nest = TRUE)
  
  ### Two-part model
  
  ### Part 1 - Logit model
  #### Run the logit model and predict the probabilities (include interaction term)
  part1 <- svyglm(nonzero ~ male + agecat + male:agecat,
                  design = mepsdsgn,
                  family = quasibinomial(link = "logit"))
  
  ### Part 2 - Gamma model
  #### Subset data with nonzero
  mepsdsgn_nonzero <- subset(mepsdsgn, totexp > 0)
  
  #### Run the gamma model and predict the probabilities (include interaction term)
  part2 <- svyglm(totexp ~ male + agecat + male:agecat,
                  design = mepsdsgn_nonzero,
                  family = Gamma(link = "log"))
  
  #### STORE OUTPUTS
  group0 <- data.frame(agecat = factor("0", 
                                       levels = levels(hc2021p$agecat)), male = 0)
  group1 <- data.frame(agecat = factor("0", 
                                       levels = levels(hc2021p$agecat)), male = 1) 
  
  #### PREDICTIONS
  pred_part1_0 <- predict(part1, group0, type = "response")
  pred_part1_1 <- predict(part1, group1, type = "response")
  
  pred_part2_0 <- predict(part2, group0, type = "response")
  pred_part2_1 <- predict(part2, group1, type = "response")
  
  totexp0 <- pred_part1_0 * pred_part2_0
  totexp1 <- pred_part1_1 * pred_part2_1
  
  #### Summarize
  return(c(totexp0 = totexp0, 
           totexp1 = totexp1,
           diff = totexp1 - totexp0))
}


### BOOTSTRAP 
set.seed(12321) # Set seed to reproduce findings
bootstrap_results <- boot(data = hc2021p,
                          statistic = twopm,
                          R = 1000)

#### Generate the summary of the average total expenditures and difference
bootstrap_results$t0

#### Generate the 95% CI of the mean differences
boot.ci(bootstrap_results, index = 1, type = "perc") # Males
boot.ci(bootstrap_results, index = 2, type = "perc") # Females
boot.ci(bootstrap_results, index = 3, type = "perc") # Diff

#### Plot the distribution of the mean differences
plot(bootstrap_results, index = 1)
plot(bootstrap_results, index = 2)
plot(bootstrap_results, index = 3)



### Estimate the differences in slopes by year
#### Bootstrap code
slopes <- function(data, indices) {  
  hc2021p <- data[indices, ]
  
  ### SVY DESIGN
  ### APPLY survey weight
  options(survey.lonely.psu = 'adjust')
  mepsdsgn = svydesign(
    id = ~varpsu,
    strata = ~varstr,
    weights = ~perwt21f,
    data = hc2021p,
    nest = TRUE)
  
  ### Two-part model
  
  ### Part 1 - Logit model
  #### Inspect the total expenditure (totexp)
  summary(hc2021p$totexp)
  
  #### Run the logit model and predict the probabilities (include interaction term)
  part1 <- svyglm(nonzero ~ male + agecat + male:agecat, 
                  design = mepsdsgn,
                  family = quasibinomial(link = "logit"))
  
  ### Part 2 - Gamma model
  #### Subset data with nonzero
  mepsdsgn_nonzero <- subset(mepsdsgn, totexp > 0)
  
  #### Run the gamma model and predict the probabilities (include interaction term)
  part2 <- svyglm(totexp ~ male + agecat + male:agecat,
                  design = mepsdsgn_nonzero,
                  family = Gamma(link = "log"))
  
  #### STORE OUTPUTS
  years <- levels(hc2021p$agecat)
  
  results <- sapply(years, function(yr) {
    group0 <- data.frame(agecat = factor(yr, levels = years), male = 0)
    group1 <- data.frame(agecat = factor(yr, levels = years), male = 1) 
    
    #### PREDICTIONS
    pred_part1_0 <- predict(part1, group0, type = "response")
    pred_part1_1 <- predict(part1, group1, type = "response")
    
    pred_part2_0 <- predict(part2, group0, type = "response")
    pred_part2_1 <- predict(part2, group1, type = "response")
    
    totexp0 <- pred_part1_0 * pred_part2_0
    totexp1 <- pred_part1_1 * pred_part2_1
    
    #### Summarize
    return(totexp1 - totexp0)
  })
  
  return(results)
}


boot_fun_slopes <- function(data, indices) {
  d <- data[indices, ]
  slopes(d)
}

boot_slopes <- boot(hc2021p, 
                    boot_fun_slopes, 
                    R = 1000)


#### Mean diff in slopes between male and non-male at each years (use t0)
print(boot_slopes)

#### Confidence interval at each agecat
boot.ci(boot_slopes, index = 1, type = "perc")  # agecat = 1
boot.ci(boot_slopes, index = 2, type = "perc")  # agecat = 2
boot.ci(boot_slopes, index = 3, type = "perc")  # agecat = 3
boot.ci(boot_slopes, index = 4, type = "perc")  # agecat = 4
boot.ci(boot_slopes, index = 5, type = "perc")  # agecat = 5


#### OPTIONAL: Differences between slopes (agecat = 5 versus agecat = 1)
boot_fun_diff <- function(data, indices) {
  d <- data[indices, ]
  s <- slopes(d)
  # Example: Year5 - Year1
  return(s[5] - s[1])
}

#### Perform bootstrap
boot_diff <- boot(hc2021p, 
                  boot_fun_diff, 
                  R = 1000)

#### Difference in the slopes between agecat = 5 versus agecat = 1
mean(boot_diff$t0)

#### 95% CI of the difference in slopes
boot.ci(boot_diff, type = "perc")


