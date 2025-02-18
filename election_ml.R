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
num_simulations <- 1000
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

print(paste("Cruz wins in", round(cruz_win_percentage, 2), "% of simulations"))
print(paste("Allred wins in", round(100 - cruz_win_percentage, 2), "% of simulations"))