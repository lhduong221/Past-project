# Libraries ---- 
library(glmnet)
library(AER)
library(car)
library(mgcv)
library(MASS)
library(ROCR)

# Data ----
data <- read.table("hospital.txt")
str(data)

# Check for collineratity ----
lm.fit.died <- lm(died ~., data)
vif(lm.fit.died)
lm.fit.los <- lm(los ~., data)
vif

# Model 1: Mortality ----
hist(data$died)
## Model fitting
died_logit <- glm(died ~ los + sp02 + risk , 
                  family = binomial(link = "logit"),
                  data)
summary(died_logit)
## Confusion matrix
table(true = data$died, pred = round(fitted(died_logit)))
## ROC curve
pred <- prediction(fitted(died_logit), data$died)
plot(performance(pred, "tpr", "fpr"))
abline(0, 1, lty = 2)


# Model 2: Length of Stay ----
## Check for Overdispersion
mean(data$los)
var(data$los)
disp_ratio = var(data$los)/mean(data$los)
hist(data$los)
## Negative binomial model
los_nb <- glm.nb(los ~ died + gender +severity, data)
summary(los_nb)
par(mfrow=c(2,2))
plot(los_nb)

## Gamma (for comparison)
los_gam <- glm(los ~ died + gender + severity,
               family = Gamma(link = "log"),
               data)
summary(los_gam)
par(mfrow = c(2,2))
plot(los_gam)


