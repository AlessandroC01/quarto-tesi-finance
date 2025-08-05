
### To confirm the correctness of 
### the optimization procedure, we 
##  manually reconstructed the XGBoost 
### model using the optimal parameters
### found via Bayesian optimization


param <- list(
  eta = comp$eta,
  max_depth = as.integer(comp$max_depth),
  subsample = comp$subsample,
  min_child_weight = comp$min_child_weight,
  booster = "gbtree",
  objective = "count:poisson",
  eval_metric = "poisson-nloglik"
)

set.seed(124)  # 
xgbcv <- xgb.cv(params = param,
                nrounds = comp$nrounds,
                data = dtrain,
                folds = folds,
                prediction = TRUE,
                early_stopping_rounds = 10,
                verbose = 0,
                maximize = FALSE)

# Valutazione
   Score <- -min(xgbcv$evaluation_log$test_poisson_nloglik_mean)
print(Score)
