## LIBRARIES & DIRECTORY ----
# libraries
library(MASS)
library(carData)
library(car)
#install.packages("stargazer")
#library(stargazer)
# set directory to the location of the R file (only works in RStudio)
install.packages("rstudioapi")
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))


## DATA ----
# make sure the R file and the data file are in the same folder to import data (or set directory manually)
data <- read.table("expenditure.txt")

# dissect the dummy variables into several binary dummy variables and remove the original dummy variable
temp.data <- data.frame(data)

# general health
temp.data$gen_excellent <- ifelse(temp.data$general == 1, 1, 0)
temp.data$gen_vgood <- ifelse(temp.data$general == 2, 1, 0)
temp.data$gen_good <- ifelse(temp.data$general == 3, 1, 0)
temp.data$gen_fair <- ifelse(temp.data$general == 4, 1, 0)
# temp.data$gen_bad <- ifelse(temp.data$general == 5, 1, 0)      # reference
temp.data$general <- NULL

# mental health
temp.data$men_excellent <- ifelse(temp.data$mental == 1, 1, 0)
temp.data$men_vgood <- ifelse(temp.data$mental == 2, 1, 0)
temp.data$men_good <- ifelse(temp.data$mental == 3, 1, 0)
temp.data$men_fair <- ifelse(temp.data$mental == 4, 1, 0)
# temp.data$men_bad <- ifelse(temp.data$mental == 5, 1, 0)       # reference
temp.data$mental <- NULL

# ethnicity
temp.data$eth_white <- ifelse(temp.data$ethnicity == 1, 1, 0)
temp.data$eth_black <- ifelse(temp.data$ethnicity == 2, 1, 0)
temp.data$eth_na <- ifelse(temp.data$ethnicity == 3, 1, 0)
# temp.data$eth_other <- ifelse(temp.data$ethnicity == 4, 1, 0)  # reference
temp.data$ethnicity <- NULL

# region
temp.data$reg_ne <- ifelse(temp.data$region == 1, 1, 0)
temp.data$reg_mw <- ifelse(temp.data$region == 2, 1, 0)
temp.data$reg_s <- ifelse(temp.data$region == 3, 1, 0)
# temp.data$reg_w <- ifelse(temp.data$region == 4, 1, 0)         # reference
temp.data$region <- NULL

# commit changes to the data
data <- temp.data


## TOTAL MEDICAL DATA ----
par(mfrow = c(2,1))

# mdexpend showing combined expenses on medical visits
data$mdvexpend <- data$dvexpend + data$ndvexpend
hist(data$mdvexpend,
     xlim = c(0, 60000), 
     ylim = c(0, 5000), 
     breaks = 1000)
#TODO ensure to remove the column before running the regression!
data$mdvexpend <- NULL

# mdvisit showing combined number of medical visits
data$mdvisit <- data$dvisit + data$ndvisit
hist(data$mdvisit, 
     xlim = c(0, 60), 
     ylim = c(0, 5000), 
     breaks = 80)
#TODO ensure to remove the column before running the regression!
data$mdvisit <- NULL


## INCOME ----
# histogram of the response
par(mfrow = c(1, 1))
hist(data$income,
     ylim = c(0, 700),
     breaks = 100)

# fitting the MLR model and removing colinear variables
lm.fit <- lm(income ~. 
             -men_excellent  # VIF = 21.2
             -gen_vgood      # VIF = 10.1
             ,data)
summary(lm.fit)
vif(lm.fit)

# running a mixed selection to only include significant variables
lm.fit.select <- stepAIC(lm.fit, ~.,
                         direction = "both",  # ensure mixed is used
                         trace = FALSE        # less console output
                         )
summary(lm.fit.select)

# Manually removing more variables based on p-values above 1% & 5%
lm.fit.manual <- update(lm.fit.select, ~.
                        -gen_good  # -0.0002, p-value > 5%
                        -ndvisit   # -0.0003, p-value > 1%
                        -eth_na    # -0.0004, p-value > 1%
                        )          # minus value shows the reduction of Adj.R^2 by removing that variable
summary(lm.fit.manual)  # Adj.R^2 = 18.75% --> 18.66%, for removing 3 predictors

# Parsimonious model
lm.fit.parsim <- update(lm.fit.manual, ~.
                        -dvexpend        # -0.0012, approx. 45% zero observations
                        -hypertension    # -0.0004, p-value > 1%
                        -hyperlipidemia  # -0.0005, greatest p-value
                        -reg_mw          # -0.0009, greatest p-value
                        -eth_white       # -0.0018, greatest p-value
                        -men_good        # -0.0021, greatest p-value
                        -men_fair        # -0.0024, greatest p-value
                        -reg_s           # -0.0030, greatest p-value
                        -gender          # -0.0031, greatest p-value
                        )                # minus value shows the reduction of Adj.R^2 by removing that variable
summary(lm.fit.parsim)
#stargazer(lm.fit.manual,lm.fit.parsim, type="text")# Adj.R^2 = 18.66% --> 17.12%, for removing 9 predictors

# comparing two models, manual & parsim, using ANOVA
anova(lm.fit.manual, lm.fit.parsim)

# checking for linearity/ normality of residuals/ outliers/ high leverage points
par(mfrow = c(2, 2))
plot(lm.fit.manual)

# plotting standardized residuals against each predictor for 'manual model' (possible to plot other models)
mod <- lm.fit.manual  # model used for plotting (select / manual / parsim)
par(mfrow = c(4, 4))
fit.names <- names(mod$model)[-1]
lm.fit.resid <- rstandard(mod)
for (i in 1:length(fit.names)) {
  plot(data[[fit.names[i]]], lm.fit.resid,
       xlab = fit.names[i],
       ylab = "Residuals")
  abline(lm(lm.fit.resid ~ data[[fit.names[i]]]),
         col = "red",
         lwd = 1.5)
}

# predicting response and seeing its confidence & predicting intervals
confint(lm.fit.manual)
fit.predict.values <- data.frame(bmi = 20,
                                 age = 30,
                                 gender = 1,
                                 education = 12,
                                 hypertension = 1,
                                 hyperlipidemia = 1,
                                 dvexpend = 300,
                                 gen_excellent = 0, gen_fair = 1,
                                 men_good = 1, men_fair = 0,
                                 eth_white = 1, eth_black = 0,
                                 reg_mw = 0, reg_s = 1
                                 )
predict(lm.fit.manual, fit.predict.values, se.fit = TRUE, interval = c("confidence"))
predict(lm.fit.manual, fit.predict.values, se.fit = TRUE, interval = c("prediction"))


## sqrt(INCOME) (attempt to fix for non-normal distribution of residuals) ----
# new data set with square for income
sqrt_data <- data.frame(data)
sqrt_data$sqrt_income <- sqrt(sqrt_data$income)
sqrt_data$income <- NULL

# histogram of the response
par(mfrow = c(1, 1))
hist(sqrt_data$sqrt_income,
     ylim = c(0, 350),
     breaks = 100)

# fitting the MLR model and removing colinear variables
sqrt.lm.fit <- lm(sqrt_income ~. 
                  -men_excellent  # VIF = 21.2
                  -gen_vgood      # VIF = 10.1
                  ,sqrt_data)
summary(sqrt.lm.fit)
vif(sqrt.lm.fit)

# running a mixed selection to only include significant variables
sqrt.lm.fit.select <- stepAIC(sqrt.lm.fit, ~., 
                              direction = "both",  # ensure mixed is used
                              trace = FALSE        # less console output
                              )
summary(sqrt.lm.fit.select)

# manually removing more variables based on p-values above 1% & 5%
sqrt.lm.fit.manual <- update(sqrt.lm.fit.select, ~.
                             -reg_ne   # -0.0001, p-value > 5%
                             -reg_mw   # -0.0001, P-value > 5%
                             -eth_na   # -0.0004, p-value > 1%
                             -dvisit   # -0.0004, p-value > 1%
                             -ndvisit  # -0.0003, p-value > 1%
                             )
summary(sqrt.lm.fit.manual) # Adj.R^2 = 20.16% --> 20.03%, for removing 5 predictors

# parsimonious model
sqrt.lm.fit.parsim <- update(sqrt.lm.fit.manual, ~.
                             -dvexpend        # -0.0010, approx. 45% zero observations
                             -hypertension    # -0.0006, greatest p-value
                             -hyperlipidemia  # -0.0003, greatest p-value
                             -bmi             # -0.0015, greatest p-value
                             -eth_white       # -0.0021, greatest p-value
                             -gen_excellent   # -0.0024, greatest p-value
                             -reg_s           # -0.0026, greatest p-value
                             )
summary(sqrt.lm.fit.parsim)  # Adj.R^2 = 20.03% --> 18.98%, for removing 7 predictors

#stargazer(sqrt.lm.fit.manual,sqrt.lm.fit.parsim)# Adj.R^2 = 18.66% --> 17.12%, for removing 9 predictors
# comparing two models, manual & parsim, using ANOVA
anova(sqrt.lm.fit.manual, sqrt.lm.fit.parsim)

# checking for linearity/ normality of residuals/ outliers/ high leverage points
par(mfrow = c(2, 2))
plot(sqrt.lm.fit.manual)

# plotting standardized residuals against each predictor for 'manual model' (possible to plot other models)
sqrt.mod <- sqrt.lm.fit.manual  # model used for plotting (select / manual / parsim)
par(mfrow = c(4, 4))
sqrt.fit.names <- names(sqrt.mod$model)[-1]
sqrt.lm.fit.resid <- rstandard(sqrt.mod)
for (i in 1:length(sqrt.fit.names)) {
  plot(data[[sqrt.fit.names[i]]], sqrt.lm.fit.resid,
       xlab = sqrt.fit.names[i],
       ylab = "Residuals")
  abline(lm(sqrt.lm.fit.resid ~ data[[sqrt.fit.names[i]]]),
         col = "red",
         lwd = 1.5)
}

# predicting response and seeing its confidence & predicting intervals for manual model
confint(sqrt.lm.fit.manual)
sqrt.fit.predict.values <- data.frame(bmi = 20,
                                      age = 30,
                                      gender = 1,
                                      education = 12,
                                      hypertension = 1,
                                      hyperlipidemia = 1,
                                      dvexpend = 300,
                                      gen_excellent = 0, gen_fair = 1,
                                      men_good = 1, men_fair = 0,
                                      eth_white = 0, eth_black = 1,
                                      reg_s = 1
                                      )
predict(sqrt.lm.fit.manual, sqrt.fit.predict.values, se.fit = TRUE, interval = c("confidence"))
predict(sqrt.lm.fit.manual, sqrt.fit.predict.values, se.fit = TRUE, interval = c("prediction"))
