# note this file contains experiments in fitting data with multiple linear regression
# it is only for playing around, and probably results in overfitting

source('baztools.R')
library(caTools)
library(ggplot2)
# con <- start_sql()
fd <- get_data(con, 1, "'2017-10-01 00:00:00'", "'2017-10-10 00:00:00'")
fd$hour_of_day <- factor(floor(fd$min_of_day/60), levels=1:24, labels=1:24)
try(detach(fd))
attach(fd)
dep_var <- hour_of_day

dataset <- data.frame(
    current,
    real_power,
    reac_power,
    # min_of_day,
    current_thd,
    disp_power_factor,
    frequency,
    voltage,
    dep_var)

split = sample.split(dataset$dep_var, SplitRatio = 0.8)
training_set = subset(dataset, split == TRUE)
test_set = subset(dataset, split == FALSE)
regressor <- lm(formula = dep_var ~ .,
                data= training_set)
summary(regressor)
y_pred <- predict(regressor, newdata=test_set)
percentage_error <- 100 * (test_set$dep_var - y_pred)/test_set$dep_var
qplot(test_set$dep_var, y_pred) + 
  # coord_fixed() + 
  geom_point(aes(col=y_pred)) +
  geom_smooth() +
  theme(legend.position = "none")
