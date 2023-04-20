library(glmnet)
library(splines)
library(randomForest)
library(gbm)

shoes <- read.csv("StockX-Data-Contest-2019-3.csv", stringsAsFactors = TRUE)

shoes <- shoes[,-c(1,2,5)]


shoes <- na.omit(shoes)

head(shoes)


hist(shoes$Sale.Price)

model1 <- lm(log(Sale.Price) ~., data = shoes)
summary(model1)
par(mfrow=c(2,2))
plot(model1) # not great 

x = model.matrix(Sale.Price~.,data = shoes)[,-1]
y = shoes$Sale.Price

set.seed(1)
trainIndex <- sample(1:nrow(x), nrow(x)*.80)


grid = 10^seq(-2, 4,length=200)

model2 <- glmnet(x[trainIndex,], y[trainIndex], alpha=0.5, lambda=grid)

cv_class <- cv.glmnet(x[trainIndex,], y[trainIndex], alpha=0.5,lambda=grid, nfolds=12)
best_lam <- cv_class$lambda.min
best_lam

best_mod2 <- glmnet(x[trainIndex,], y[trainIndex], alpha=0.5, lambda=best_lam)
coef(best_mod2)

mod2_pred <- predict(model2, s = best_lam, newx = x[-trainIndex,])

mod2_rmse <- sqrt(mean( (mod2_pred - y[-trainIndex])^2) )
mod2_rmse

rf_model <- randomForest(Sale.Price ~ . ,
                        data = shoes,
                        subset = trainIndex, # row indices
                        ntree = 100, #specify the number of trees if you want
                        importance = TRUE)


#Create Predictions
rf_pred <- predict(rf_model, newdata = shoes[-trainIndex,])

rf_rmse <- sqrt(mean((rf_pred - shoes$Sale.Price[-trainIndex])^2))
rf_rmse 

#run a gbm model now

gbm_model <- gbm(Sale.Price ~ .,
                data = shoes[trainIndex,],
                distribution = "gaussian",#distribution is Gaussian for Regression
                n.trees = 500, #How many trees do you want
                interaction.depth = 3, #How many layers does each tree have
                shrinkage = 0.2, #The learning rate (Lambda) for each tree
                verbose = F) #Does it output something for each tree

#Making Predictions
boost_pred <- predict(gbm_model,
                     newdata = shoes[-trainIndex,],
                     n.trees = 500)

boost_rmse <- sqrt(mean((boost_pred - shoes$Sale.Price[-trainIndex])^2))
boost_rmse

rmse_list = rep(0,18)
count <- 0
for (i in 1:3) {
  for (j in 1:3) {
    for (k in 1:2) {
      count <- count + 1
      gbm_model <- gbm(Sale.Price ~ .,
                       data = shoes[trainIndex,],
                       distribution = "gaussian",#distribution is Gaussian for Regression
                       n.trees = 500*i, #How many trees do you want
                       interaction.depth = j, #How many layers does each tree have
                       shrinkage = 0.1 * k, #The learning rate (Lambda) for each tree
                       verbose = F) #Does it output something for each tree
      
      #Making Predictions
      boost_pred <- predict(gbm_model,
                            newdata = shoes[-trainIndex,],
                            n.trees = 500*i)
      
      rmse_list[count] <- sqrt(mean((boost_pred - shoes$Sale.Price[-trainIndex])^2))
    }
  }
}
index <- which.min(rmse_list);index

rmse_list[index]

# The best model is with 1000 trees, interaction depth of 3, and shrinkage of 0.2




