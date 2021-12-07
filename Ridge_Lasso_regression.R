library(glmnet)
library(tidyverse)
library(caret)

## Data manip again
bball <- read_csv("bbtraindata.csv") %>% 
  select(-c(Season, TeamIdA, TeamIdB, SeedA, SeedB, ScoreA, ScoreB, Round, A.team, B.team, A.conference, B.conference, A.record, B.record, Score_diff)) %>% 
  mutate(WinA = as.factor(WinA))

num_basketball <- nrow(bball)
basketball_train_ids <- sample(1:num_basketball, round(num_basketball * .80))
basketball_test_ids <- (1:num_basketball)[!(1:num_basketball) %in% basketball_train_ids]
basketball_train <- bball %>% slice(basketball_train_ids)
basketball_test <- bball %>% slice(basketball_test_ids)

# Hold out data
Ytrain = basketball_train$WinA
Xtrain = model.matrix(WinA ~ ., basketball_train)[,-1]
Ytest = basketball_test$WinA
Xtest = model.matrix(WinA ~ ., basketball_test)[,-1]


# Ridge regression
ridge_cv = cv.glmnet(y = Ytrain, 
                     x = Xtrain,
                     family = "binomial", 
                     alpha = 0)
plot(ridge_cv)
mod1 = glmnet(Xtrain, Ytrain, family = "binomial", alpha = 0, lambda = ridge_cv$lambda.min)
preds1 = predict(mod1, newx = Xtest, type = "response")
outcome1 = tibble(prob = preds1, Truth = as.factor(Ytest)) %>% 
  mutate(Pred = as.factor(mapply(rbinom, 1, 1, preds1[,1], SIMPLIFY = TRUE)))

caret::confusionMatrix(reference = outcome1$Truth, data = outcome1$Pred)


# LASSO regression
lasso_cv = cv.glmnet(y = Ytrain, 
                     x = Xtrain,
                     family = "binomial", 
                     alpha = 1)
plot(lasso_cv)
mod2 = glmnet(Xtrain, Ytrain, family = "binomial", alpha = 0, lambda = lasso_cv$lambda.min)
preds2 = predict(mod2, newx = Xtest, type = "response")
outcome2 = tibble(prob = preds2, Truth = as.factor(Ytest)) %>% 
  mutate(Pred = as.factor(mapply(rbinom, 1, 1, preds2[,1], SIMPLIFY = TRUE)))

caret::confusionMatrix(reference = outcome2$Truth, data = outcome2$Pred)
print(mod2)
