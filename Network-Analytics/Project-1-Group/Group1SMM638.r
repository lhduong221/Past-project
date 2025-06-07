#### STEP 6: Run statistical analysis
## Libraries ----
# libraries
library(readr)
library(ggplot2)
library(gridExtra)
library(MASS)
library(carData)
library(car)
library(Matrix)
library(glmnet)


## Data ----
getwd()
data = read.csv("silicon_stat_df.csv")
rownames(data) <- data$team_id  # make sure the row indeces are 'team_id's
data$team_id <- NULL            # remove team_id

# seeing how project novelty and duration are related
data$project_tech_success <- factor(data$project_tech_success)  # Convert project_tech_success to a factor for plotting
ggplot(data, aes(x = project_novelty, y = project_duration)) +
  geom_point(aes(color = project_tech_success, size = team_size)) +
  scale_size_continuous(range = c(2, 11), breaks = 2:11) +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  labs(x = "Project Novelty", y = "Project Duration", shape = "Tech Success") +
  theme_minimal()


## Project Novelty: Is project_tech_success significant for novelty? ----
data$project_tech_success <- as.numeric(as.character(data$project_tech_success))  # make project success into numeric
summary(lm(project_novelty ~ project_tech_success, data))                         # NO, p-value = 0.335
data_nov <- data[data$project_tech_success != 0, ]                                # data for novelty regression only for successful projects
data_nov$project_tech_success <- NULL

## Project Novelty: Visual representation of relationships with other variables ----
plot.nov.names <- names(data_nov)[-c(7, 8)]  # variables to use
plot.nov.list <- list()  # temporary list for grid.arrange
for (i in plot.nov.names){
  p <- ggplot(data_nov, aes(x = .data[[i]], y = project_novelty)) +
    geom_point(size = 3) +
    geom_smooth(method = "lm", se = FALSE, color = "blue") +
    labs(x = i, y = "Project Novelty") +
    theme_minimal()
  plot.nov.list[[i]] <- p  # add created plot to the list
}
grid.arrange(grobs = plot.nov.list)  # show all the plots


## Project Novelty: Regression ----
# histogram of project_duration
par(mfrow = c(1, 1))
hist(data_nov$project_novelty)

# Linear regression of project_novelty onto entire data set
# clustering/ Burt/ nbc are collinear, however, useful to be looked at individually
lm.nov <- lm(project_novelty ~.
             -clust_coef   # VIF = 58.07
             #-burt_constr  # VIF = 66.81
             #-nbc          # VIF = 10.66
             ,data_nov)
summary(lm.nov)
vif(lm.nov)

# Clustering Coefficient
lm.nov.cc <- lm(project_novelty ~.
                -burt_constr
                -nbc
                ,data_nov)
summary(lm.nov.cc)

# Burt's Constraint
lm.nov.bc <- lm(project_novelty ~.
                -nbc
                -clust_coef
                ,data_nov)
summary(lm.nov.bc)

# Node Betweenness Centrality
lm.nov.nbc <- lm(project_novelty ~.
                 -burt_constr
                 -clust_coef
                 ,data_nov)
summary(lm.nov.nbc)

lm.nov.models <- list(lm.nov.cc, lm.nov.bc, lm.nov.nbc)
lm.nov.select.models <- list()

# running a mixed selection to only include significant variables on all models
for( i in seq_along(lm.nov.models)){
  lm.nov.select <- stepAIC(lm.nov.models[[i]], ~.,
                           direction = "both",  # ensure mixed is used
                           trace = FALSE        # less console output
  )
  lm.nov.select.models <- append(lm.nov.select.models, list(lm.nov.select)) 
  print(summary(lm.nov.select))
  
  # checking for linearity/ normality of residuals/ outliers/ high leverage points
  par(mfrow = c(2, 2))
  plot(lm.nov.select)
  
}
# Node Betweenness Centrality
lm.nov.nbc <- lm(project_novelty ~ node_degree
                 ,data_nov)
summary(lm.nov.nbc)

## Project Novelty: Plotting residuals against predictors ----
nov.predict <- list(data_nov$clust_coef, data_nov$burt_constr, data_nov$nbc)  # predictors for the 3 models
nov.predict.names <- list("Clustering Coefficient", "Burt's Constraint", "Node Betwenness Centrality")
nov.model.names <- list("CC", "BC", "NBC")
par(mfrow = c(3, 3))

for (i in 1:length(lm.nov.select.models)) {
  lm.nov.resid <- rstandard(lm.nov.select.models[[i]])  # residual of the model
  
  for (j in 1:length(nov.predict)){
    plot(nov.predict[[j]], lm.nov.resid,
         main = nov.model.names[i],
         xlab = nov.predict.names[j],
         ylab = "Residuals")
    
    abline(lm(lm.nov.resid ~ nov.predict[[j]]),
           col = "red",
           lwd = 1.5) 
  }
}


## Project Duration: Is project_tech_success significant for duration? ----
data$project_tech_success <- as.numeric(as.character(data$project_tech_success))  # make project success into numeric
summary(lm(project_duration ~ project_tech_success, data))                        # YES, p-value = 0.00127
data_dur <- data[data$project_tech_success != 0, ]                                # data for novelty regression only for successful projects
data_dur$project_tech_success <- NULL


## Project Duration: Visual representation of relationships with other variables ----
plot.dur.names <- names(data_dur)[-c(7, 8)]  # variables to use
plot.dur.list <- list()  # temporary list for grid.arrange
for (i in plot.dur.names){
  p <- ggplot(data_dur, aes(x = .data[[i]], y = project_duration)) +
    geom_point(size = 3) +
    geom_smooth(method = "lm", se = FALSE, color = "blue") +
    labs(x = i, y = "Project Duration") +
    theme_minimal()
  plot.dur.list[[i]] <- p  # add created plot to the list
}
grid.arrange(grobs = plot.dur.list)  # show all the plots


## Project Duration: Regression ----
# histogram of project_duration
par(mfrow = c(1, 1))
hist(data$project_duration)

# Linear regression of project_duration onto entire data set
# clustering/ Burt/ nbc are collinear, however, useful to be looked at individially
lm.dur <- lm(project_duration ~.
             #-clust_coef   # VIF = 56.58
             #-burt_constr  # VIF = 63.61
             #-nbc          # VIF = 10.10
             ,data_dur)
summary(lm.dur)
vif(lm.dur)

# Clustering Coefficient
lm.dur.cc <- lm(project_duration ~.
                -burt_constr
                -nbc
             ,data_dur)
summary(lm.dur.cc)

# Burt's Constraint
lm.dur.bc <- lm(project_duration ~.
                -nbc
                -clust_coef
                ,data_dur)
summary(lm.dur.bc)

# Node Betweenness Centrality
lm.dur.nbc <- lm(project_duration ~.
                -burt_constr  # VIF = 63.61
                -clust_coef   # VIF = 56.58
                ,data_dur)
summary(lm.dur.nbc)

lm.dur.models <- list(lm.dur.cc, lm.dur.bc, lm.dur.nbc)
lm.dur.select.models <- list()

# running a mixed selection to only include significant variables on all models
for( i in seq_along(lm.dur.models)){
  lm.dur.select <- stepAIC(lm.dur.models[[i]], ~.,
                       direction = "both",  # ensure mixed is used
                       trace = FALSE        # less console output
  )
  lm.dur.select.models <- append(lm.dur.select.models, list(lm.dur.select)) 
  print(summary(lm.dur.select))
  
  # checking for linearity/ normality of residuals/ outliers/ high leverage points
  par(mfrow = c(2, 2))
  plot(lm.dur.select)
  
}

## Project Duration: Plotting residuals against predictors ----
dur.predict <- list(data_dur$node_degree, data_dur$clust_coef, data_dur$burt_constr, data_dur$nbc)  # predictors for the 3 models
dur.predict.names <- list("Node Degree", "Clustering Coefficient", "Burt's Constraint", "Node Betwenness Centrality")
dur.model.names <- list("CC", "BC", "NBC")
par(mfrow = c(3, 4))

for (i in 1:length(lm.dur.select.models)) {
  lm.dur.resid <- rstandard(lm.dur.select.models[[i]])  # residual of the model
  
  for (j in 1:length(dur.predict)){
    plot(dur.predict[[j]], lm.dur.resid,
         main = dur.model.names[i],
         xlab = dur.predict.names[j],
         ylab = "Residuals")
    
    abline(lm(lm.dur.resid ~ dur.predict[[j]]),
           col = "red",
           lwd = 1.5) 
  }
}


## Predicting Project Success and Failure ----
logit_model <- glm(project_tech_success ~ clust_coef, data = data, family = binomial)
summary(logit_model)
vif(logit_model_nbc)

#nbc
logit_model <- glm(project_tech_success ~ nbc, data = data, family = binomial)
summary(logit_model)

# Prepare a range of nbc values for prediction
nbc_range <- data.frame(nbc = seq(min(data$nbc), max(data$nbc), length.out = 100))

# Generate predictions with confidence intervals
predictions <- predict(logit_model, newdata = nbc_range, type = "link", se.fit = TRUE)
nbc_range$predicted_success <- plogis(predictions$fit) # Convert to probability
nbc_range$lower_ci <- plogis(predictions$fit - 1.96 * predictions$se.fit) # Lower 95% CI
nbc_range$upper_ci <- plogis(predictions$fit + 1.96 * predictions$se.fit) # Upper 95% CI

ggplot() +
  geom_point(data = data, aes(x = nbc, y = project_tech_success), 
             alpha = 0.5, position = position_jitter(width = 0, height = 0.05)) +  # Scatter plot for actual project successes
  geom_line(data = nbc_range, aes(x = nbc, y = predicted_success), color = "blue") +  # Line for predicted probabilities
  geom_ribbon(data = nbc_range, aes(x = nbc, ymin = lower_ci, ymax = upper_ci), alpha = 0.2) +  # Ribbon for confidence intervals
  labs(x = "Node Betweenness Centrality (nbc)", y = "Probability of Technical Success", 
       title = "Logistic Regression of Technical Success on Node Betweenness Centrality") +
  theme_minimal()

#Clustering Coefficient 
logit_model_clust <- glm(project_tech_success ~ clust_coef, data = data, family = binomial)
summary(logit_model_clust)

# Prepare a range of clust_coef values for prediction
clust_coef_range <- data.frame(clust_coef = seq(min(data$clust_coef), max(data$clust_coef), length.out = 100))

# Generate predictions with confidence intervals
predictions_clust <- predict(logit_model_clust, newdata = clust_coef_range, type = "link", se.fit = TRUE)
clust_coef_range$predicted_success <- plogis(predictions_clust$fit)  # Convert to probability
clust_coef_range$lower_ci <- plogis(predictions_clust$fit - 1.96 * predictions_clust$se.fit)  # Lower 95% CI
clust_coef_range$upper_ci <- plogis(predictions_clust$fit + 1.96 * predictions_clust$se.fit)  # Upper 95% CI

ggplot() +
  geom_point(data = data, aes(x = clust_coef, y = project_tech_success),
             alpha = 0.5, position = position_jitter(width = 0, height = 0.05)) +  # Scatter plot for actual project successes
  geom_line(data = clust_coef_range, aes(x = clust_coef, y = predicted_success), color = "blue") +  # Line for predicted probabilities
  geom_ribbon(data = clust_coef_range, aes(x = clust_coef, ymin = lower_ci, ymax = upper_ci), alpha = 0.2) +  # Ribbon for confidence intervals
  labs(x = "Clustering Coefficient (clust_coef)", y = "Probability of Technical Success", 
       title = "Logistic Regression of Technical Success on Clustering Coefficient") +
  theme_minimal()