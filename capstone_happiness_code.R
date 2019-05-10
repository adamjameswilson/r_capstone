# load necessary libraries

library(tidyverse)
library(rvest)
library(ggplot2)
library(gridExtra)
library(caret)
library(randomForest)

# retrieve wiki artickle

url <- paste0("https://en.wikipedia.org/wiki/World_Happiness_Report")
h <- read_html(url)

# retrieve necessary table and change it into a dataframe

tab <- h %>% html_nodes("table")
tab <- tab[[5]] %>% html_table

head(tab,10)

# change dataframe for analysis

ranks <- tab
names(ranks) <- c('rank','country','score','gdp_per_capita','social_support','healthy_life_expectancy',
                  'life_choices_freedom','generosity','corruption_perceptions')
str(ranks)

# make sure columns are numeric, and remove any cases with NA

ranks$corruption_perceptions <- as.numeric(ranks$corruption_perceptions)
ranks <- ranks[complete.cases(ranks),]

# density plot for score

ranks %>%
  ggplot(aes(score)) +
  geom_density(fill = 'gold') +
  xlab('Score') + ylab('Density') + ggtitle('Density of Scores')

# density plots for the 6 independent variables

den1 <- ranks %>%
  ggplot(aes(gdp_per_capita)) + geom_density(fill='green') +
  xlab('GDP Per Capita') + ylab('Density')
den2 <- ranks %>%
  ggplot(aes(social_support)) + geom_density(fill='deeppink') +
  xlab('Social Support') + ylab('Density')
den3 <- ranks %>%
  ggplot(aes(healthy_life_expectancy)) + geom_density(fill='blue') +
  xlab('Healthy Life Expectancy') + ylab('Density')
den4 <- ranks %>%
  ggplot(aes(life_choices_freedom)) + geom_density(fill='orange') +
  xlab('Freedom of Life Choices') + ylab('Density')
den5 <- ranks %>%
  ggplot(aes(generosity)) + geom_density(fill='purple') +
  xlab('Generosity') + ylab('Density')
den6 <- ranks %>%
  ggplot(aes(corruption_perceptions)) + geom_density(fill='red') +
  xlab('Perceptions of Corruption') + ylab('Density')
grid.arrange(den1,den2,den3,den4,den5,den6, ncol=3)

# plot score against variables

plot1 <- ranks %>%
  ggplot(aes(score, gdp_per_capita)) + geom_point(color='green') +
  geom_abline(slope = (sum(ranks$gdp_per_capita)/sum(ranks$score)), lwd=1.5, color='gold') +
  xlab('Score') + ylab('GDP Per Capita')
plot2 <- ranks %>%
  ggplot(aes(score,social_support)) + geom_point(color='deeppink') +
  geom_abline(slope = (sum(ranks$social_support)/sum(ranks$score)), lwd=1.5, color='gold') +
  xlab('Score') + ylab('Social Support')
plot3 <- ranks %>%
  ggplot(aes(score, healthy_life_expectancy)) + geom_point(color='blue') +
  geom_abline(slope = (sum(ranks$healthy_life_expectancy)/sum(ranks$score)), lwd=1.5, color='gold') +
  xlab('Score') + ylab('Healthy Life Expectancy')
plot4 <- ranks %>%
  ggplot(aes(score, life_choices_freedom)) + geom_point(color='orange') +
  geom_abline(slope = (sum(ranks$life_choices_freedom)/sum(ranks$score)), lwd=1.5, color='gold') +
  xlab('Score') + ylab('Freedom of Life Choices')
plot5 <- ranks %>%
  ggplot(aes(score, generosity)) + geom_point(color='purple') +
  geom_abline(slope = (sum(ranks$generosity)/sum(ranks$score)), lwd=1.5, color='gold') +
  xlab('Score') + ylab('Generosity')
plot6 <- ranks %>%
  ggplot(aes(score,corruption_perceptions)) + geom_point(color='red') +
  geom_abline(slope = (sum(ranks$corruption_perceptions)/sum(ranks$score)), lwd=1.5, color='gold') +
  xlab('Score') + ylab('Perceptions of Corruption')
grid.arrange(plot1,plot2,plot3,plot4,plot5,plot6, ncol=3)

# correlation slopes

ranks %>%
  ggplot(aes(scale(score), scale(gdp_per_capita))) +
  geom_point(alpha=0) +
  geom_abline(intercept=0, slope = cor(ranks$score, ranks$gdp_per_capita), col='green', lwd=1.5) +
  geom_abline(intercept=0, slope = cor(ranks$score, ranks$social_support), col='deeppink', lwd=2) +
  geom_abline(intercept=0, slope = cor(ranks$score, ranks$healthy_life_expectancy), col='blue', lwd=1.5) +
  geom_abline(intercept=0, slope = cor(ranks$score, ranks$life_choices_freedom), col='orange', lwd=1.5) +
  geom_abline(intercept=0, slope = cor(ranks$score, ranks$generosity), col='purple', lwd=1.5) +
  geom_abline(intercept=0, slope = cor(ranks$score, ranks$corruption_perceptions), col='red', lwd=1.5) +
  ggtitle('Correlation Coefficients of Score against the 6 independent variables') +
  xlab('Score (Standardized)') + ylab('Independent Variables (Standardized)') +
  geom_text(x=-1.5, y=1, label='GDP Per Capita: 0.81', color='green') +
  geom_text(x=-1.5, y=0.5, label='Social Support: 0.77', color='deeppink') +
  geom_text(x=-1.5, y=0.75, label='Healthy Life Expectancy: 0.78', color='blue') +
  geom_text(x=-1.5, y=0.25, label='Freedom of Life Choices: 0.56', color='orange') +
  geom_text(x=1, y=-0.75, label='Generosity: 0.13', color='purple') +
  geom_text(x=1,y=-0.5, label='Perceptions of Corruption: 0.41', color='red')

# create train and test sets

set.seed(2019)

y <- ranks$score
test_index <- createDataPartition(y, times=1, p=0.2, list = FALSE)
train_set <- ranks %>% slice(-test_index)
test_set <- ranks %>% slice(test_index)

# just guess the score as mean score

m <- mean(train_set$score)

avg_rmse <- sqrt(mean((m - test_set$score)^2))
avg_rmse

avg_acc <- mean(abs(test_set$score - m) <= 0.5)
mean(avg_acc)

# linear regression

set.seed(2019)
y <- ranks$score
test_index <- createDataPartition(y, times=1, p=0.2, list = FALSE)
train_set <- ranks %>% slice(-test_index)
test_set <- ranks %>% slice(test_index)

fit <- lm(score ~ gdp_per_capita + social_support + healthy_life_expectancy + life_choices_freedom + generosity + corruption_perceptions,
          data = train_set)
fit$coef

y_hat <- fit$coef[1] +
  fit$coef[2]*test_set$gdp_per_capita +
  fit$coef[3]*test_set$social_support +
  fit$coef[4]*test_set$healthy_life_expectancy +
  fit$coef[5]*test_set$life_choices_freedom +
  fit$coef[6]*test_set$generosity +
  fit$coef[7]*test_set$corruption_perceptions

lm_rmse <- sqrt(mean((y_hat - test_set$score)^2))
lm_rmse

lm_acc <- mean(abs(y_hat - test_set$score) <= 0.5)
mean(lm_acc)

# knn

set.seed(2019)
test_index <- createDataPartition(y, times=1, p=0.2, list = FALSE)
train_set <- ranks %>% slice(-test_index)
test_set <- ranks %>% slice(test_index)

train_knn <- train(score ~ gdp_per_capita + social_support + healthy_life_expectancy + life_choices_freedom + generosity + corruption_perceptions,
                   method = 'knn',
                   data = train_set,
                   tuneGrid = data.frame(k = seq(2,100,2)))
train_knn$bestTune

knn_predict <- predict(train_knn, test_set)

knn_rmse <- sqrt(mean((knn_predict - test_set$score)^2))
knn_rmse

knn_acc <- mean(abs(knn_predict - test_set$score) <= 0.5)
mean(knn_acc)

# random forests

set.seed(2019)
test_index <- createDataPartition(y, times=1, p=0.2, list = FALSE)
train_set <- ranks %>% slice(-test_index)
test_set <- ranks %>% slice(test_index)

train_rf <- randomForest(score ~ gdp_per_capita + social_support + healthy_life_expectancy +
                           life_choices_freedom + generosity + corruption_perceptions,
                         data = train_set)

rf_predict <- predict(train_rf, test_set)

rf_rmse <- sqrt(mean((rf_predict - test_set$score)^2))
rf_rmse

rf_acc <- mean(abs(rf_predict - test_set$score) <= 0.5)
mean(rf_acc)

# summarize into dataframes

ml_scores <- round(data.frame(test_set$score, m - test_set$score, y_hat - test_set$score,
                              knn_predict - test_set$score, rf_predict - test_set$score),2)
names(ml_scores) <- c('True Score','Average Score Difference','Linear Regression Difference',
                      'KNN Difference','Random Forests Difference')
ml_scores

Method = c('Just the Average','Linear Regression','KNN','Random Forest')
RMSE <- round(c(avg_rmse, lm_rmse, knn_rmse, rf_rmse),2)
Accuracy <- round(c(mean(avg_acc), mean(lm_acc), mean(knn_acc), mean(rf_acc)),2)
ml_df <- data.frame(Method, RMSE, Accuracy)
ml_df

# score density vs normal dist

ranks %>%
  ggplot(aes(score)) +
  geom_density(fill = 'gold') +
  xlab('Score') + ylab('Density') + ggtitle('Density of Scores vs Normal Distribution') +
  stat_function(fun = dnorm, n = 155, args = list(mean=mean(ranks$score), sd = sd(ranks$score)), lwd=2)