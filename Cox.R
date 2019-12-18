# Load required packages
library(survival)
library(survminer)
library(dplyr)

# Import the ovarian cancer dataset and have a look at it
data(ovarian)
glimpse(ovarian)
help(ovarian)

# Dichotomize age and change data labels
ovarian$rx <- factor(ovarian$rx, 
                     levels = c("1", "2"), 
                     labels = c("A", "B"))
ovarian$resid.ds <- factor(ovarian$resid.ds, 
                           levels = c("1", "2"), 
                           labels = c("no", "yes"))
ovarian$ecog.ps <- factor(ovarian$ecog.ps, 
                          levels = c("1", "2"), 
                          labels = c("good", "bad"))

# Data seems to be bimodal
hist(ovarian$age) 

ovarian <- ovarian %>% mutate(age_group = ifelse(age >=50, "old", "young"))
ovarian$age_group <- factor(ovarian$age_group)

# Fit survival data using the Kaplan-Meier method
surv_object <- Surv(time = ovarian$futime, event = ovarian$fustat)
surv_object 


fit1 <- survfit(surv_object ~ rx, data = ovarian)
summary(fit1)

ggsurvplot(fit1, data = ovarian, pval = TRUE)

# Examine prdictive value of residual disease status
fit2 <- survfit(surv_object ~ resid.ds, data = ovarian)
ggsurvplot(fit2, data = ovarian, pval = TRUE)

# Fit a Cox proportional hazards model
fit.coxph <- coxph(surv_object ~ rx + resid.ds + age_group + ecog.ps, 
                   data = ovarian)
ggforest(fit.coxph, data = ovarian)

####################################################

# Load required packages

library(survival)
library(survminer)
library(dplyr)

# Examine the structure of the dataset 
# Dataset Ovarian 
# This dataset comprises a cohort of ovarian cancer patients and respective clinical information, 
# including the time patients were tracked until they either died or were lost to follow-up (futime), 
# whether patients were censored or not (fustat), patient age, treatment group assignment, 
# presence of residual disease and performance status.

# survival times futime
# fustat tells you if the indivudal survived 

data(ovarian)
glimpse(ovarian)

# dichotomize continuous to binary values and change data labels 

ovarian$rx <- factor(ovarian$rx, 
                     levels = c("1", "2"), 
                     labels = c("A", "B"))
ovarian$resid.ds <- factor(ovarian$resid.ds, 
                           levels = c("1", "2"), 
                           labels = c("no", "yes"))
ovarian$ecog.ps <- factor(ovarian$ecog.ps, 
                          levels = c("1", "2"), 
                          labels = c("good", "bad"))

# Check the distribution of the continuous variable age 
hist(ovarian$age)

ovarian <- ovarian %>% mutate(age_group = ifelse(age >=50, "old", "young"))
# Convert to factors for future covariates
ovarian$age_group <- factor(ovarian$age_group)

##########################################
## Kaplan Meier Curve - Surv / survfit  ##
##########################################

# Create the survival object / with the event and time
km_ovarian <- Surv(time = ovarian$futime, event = ovarian$fustat)
km_ovarian

# + behind survival times indicates censored data points.

# Next step is to fir the Kaplan Meier Curve
km_fit <- survfit(km_ovarian ~ rx, data = ovarian) # stratify by treatment regimen (rx) #
summary(km_fit)

ggsurvplot(km_fit, data = ovarian, pval = TRUE)
# # he log-rank p-value of 0.3 indicates a non-significant result if you consider p < 0.05 to indicate statistical significance. 
# In this study, none of the treatments examined were significantly superior, 
# although patients receiving treatment B are doing better in the first month of follow-up.

# Stritify by other variables 
# Examine prdictive value of residual disease status
km_fit2 <- survfit(km_ovarian ~ resid.ds, data = ovarian)
ggsurvplot(km_fit2, data = ovarian, pval = TRUE)

##The Kaplan-Meier plots stratified according to residual disease status look a bit different: 
# The curves diverge early and the log-rank test is almost significant./ log rank test of 0.057

## A more systematic way to look at different covariates before deciding 
## Cox Proportioanl Hazard allows you to look at different covariates

#####################################
## Cox Proportional hazard - coxph ##
#####################################

# Fit a Cox Proportional Hazards model 
fit.coxph <- coxph(km_ovarian ~ rx + resid.ds + age_group + ecog.ps, 
                   data = ovarian)

ggforest(fit.coxph, data = ovarian)