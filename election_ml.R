#regression modeling libraries
library(caret)
library(glmnet)

#read CSV file
txpoll <- read.csv("pollresult.csv", sep=",", stringsAsFactors=FALSE)

#create binary variable for prediction (binary since only 2 outcomes)
txpoll$cruz_winning <- ifelse(txpoll$cruz > txpoll$allred, 1, 0)

#more recent polls have more influence
txpoll$recency <- 1 / txpoll$days

set.seed(42)

#80% training, 20% testing
train_index <- createDataPartition(txpoll$cruz_winning, p = 0.8, list = FALSE)
train_set <- txpoll[train_index, ]
test_set <- txpoll[-train_index, ]

#logistic regression
model <- glm(cruz_winning ~ recency + samplesize + pollsterrating + votertype, 
             data = train_set, 
             family = binomial)

#summarize model
summary(model)

#predict probabilities on test set
test_set$predicted_prob <- predict(model, newdata = test_set, type = "response")

#turn probabilities into predicted winner (Cruz = 1, Allred = 0)
test_set$predicted_winner <- ifelse(test_set$predicted_prob > 0.5, 1, 0)

#prediction accuracy
accuracy <- mean(test_set$predicted_winner == test_set$cruz_winning)
print(paste("Prediction Accuracy:", round(accuracy * 100, 2), "%"))

#running 1000 simulations
set.seed(42)
num_simulations <- 1000000
sim_results <- numeric(num_simulations)

for (i in 1:num_simulations) {
  #add (+/-)5% margin of error
  margin_cruz <- pmin(pmax(test_set$cruz + rnorm(nrow(test_set), mean = 0, sd = 5), 0), 100)
  margin_allred <- pmin(pmax(test_set$allred + rnorm(nrow(test_set), mean = 0, sd = 5), 0), 100)
  
  #determine winner of election in each simulation
  sim_results[i] <- mean(margin_cruz > margin_allred)
}

#calculate percentage of simulations where Cruz wins
cruz_win_percentage <- mean(sim_results) * 100

#print results
print(paste("Cruz wins in", round(cruz_win_percentage, 2), "% of simulations"))
print(paste("Allred wins in", round(100 - cruz_win_percentage, 2), "% of simulations"))

#rest of code is now plotting graphs/charts

#------------------------------------------------
#library to help plot distributions
library(ggplot2)

#convert simulation results (which are proportions) to percentages
sim_df <- data.frame(cruz_win_rate = sim_results * 100)

#histogram of Cruz’s simulated win rates 
ggplot(sim_df, aes(x = cruz_win_rate)) +
  geom_histogram(binwidth = 2, fill = "lightblue", color = "black") +
  geom_vline(aes(xintercept = mean(cruz_win_rate)),
             color = "red", linetype = "dashed", size = 1) +
  #text label for the mean
  annotate("text", 
           x = mean(sim_df$cruz_win_rate) + 3, 
           y = 30, 
           label = paste("Mean =", round(mean(sim_df$cruz_win_rate), 2), "%"), 
           color = "red") +
  labs(
    title = "Distribution of Cruz’s Simulated Win Rate",
    x = "Cruz Win Rate (%)",
    y = "Number of Simulations"
  ) +
  theme_minimal()

#------------------------------------------------
#creates data frame for plotting
df <- data.frame(margin = margins)
df$winner <- ifelse(df$margin > 0, "Cruz", "Allred")

#calculate mean margin
mean_margin <- mean(df$margin)

#plot histogram with annotated mean margin of victory
ggplot(df, aes(x = margin, fill = winner)) +
  geom_histogram(binwidth = 2, color = "black") +
  scale_fill_manual(values = c("Cruz" = "red", "Allred" = "blue")) +
  #vertical dashed line at tie (0 margin)
  geom_vline(xintercept = 0, linetype = "dashed", size = 1) +
  #vertical dashed line for the mean margin
  geom_vline(xintercept = mean_margin, linetype = "dashed", size = 1, color = "black") +
  #annotate the mean margin
  annotate("text",
           x = mean_margin + 1,   #place label slightly to the right of mean
           y = 30,                #manually set y-value (adjust based on your plot)
           label = paste("Mean margin =", round(mean_margin, 2), "%"),
           color = "black",
           angle = 90,
           vjust = -0.5) +
  labs(
    title = "Distribution of Simulated Margin (Cruz - Allred)",
    x = "Margin of Victory (%)",
    y = "Number of Simulations"
  ) +
  theme_minimal()