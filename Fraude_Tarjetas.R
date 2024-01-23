  # Loading Libraries -------------------------------------------------------
  
  library(tidyverse)
  library(ggplot2)
  library(xgboost)
  library(ROCR)
  library(caret)
  library(pROC)
  
  # Loading Data ------------------------------------------------------------
  
  df <- read_csv("dataset_cards.csv")
  df %>% View()
  
  head(df)
  summary(df)
  
  # Exploration of the data -------------------------------------------------
  
  # Fraud exploration
  
  class_counts <- table(df$isFraud)
  class_counts %>% View()
  
  # Data high imbalanced
  
  class_proportions <- prop.table(class_counts)
  class_proportions %>% View()
  
  # Distribution of fraud by transaction type
  
  ggplot(df, aes(x = type, fill = factor(isFraud))) +
    geom_bar(position = "stack") +
    labs(title = "Distribution of Fraud by Transaction Type", 
         x = "Transaction Type", 
         y = "Count") +
    scale_fill_manual(values = c("blue", "red"), name = "Fraud") +
    theme_minimal()
  
  df_isFraud <- subset(df, isFraud == 1)
  df_isFraud %>% View()
  
  sum(df_isFraud$isFraud == df_isFraud$isFlaggedFraud) # only 16 are equal
  
  df <- df[, !(names(df) %in% c('isFlaggedFraud'))] # we delete the column isFlaggedFraud because we won´t use it at all
  
  table(df_isFraud$type) # there's only fraud in 'CASH_OUT' and 'TRANSFER'
  
  # Splitting Data ----------------------------------------------------------
  
  df$type <- as.factor(df$type)
  df$encoded_type <- as.integer(df$type)
  
  df_model <- subset(df, encoded_type == 2 | encoded_type == 5)
  df_model %>% View()
  
  fraudes <- df_model %>% filter(isFraud == 1)
  no_fraudes <- df_model %>% filter(isFraud == 0) %>% slice_sample(n = nrow(fraudes), replace = FALSE)
  
  datos_balanceados <- bind_rows(fraudes, no_fraudes)
  
  set.seed(123)
  index <- createDataPartition(datos_balanceados$isFraud, p = 0.8, list = FALSE)
  train_data <- datos_balanceados[index, ]
  test_data <- datos_balanceados[-index, ]
  
  X_training <- train_data[, !(names(train_data) %in% c('isFraud', 'nameOrig', 'nameDest', 'type'))]
  y_training <- train_data$isFraud
  
  X_test <- test_data[, !(names(test_data) %in% c('isFraud', 'nameOrig', 'nameDest', 'type'))]
  y_test <- test_data$isFraud
  
  # XGBoost matrices
  # Utilizamos XGBoost dado que al estar los datos muy desvalanceados, un modelo de Árboles Aleatorios es una gran opción 
  
  dtrain <- xgb.DMatrix(data = as.matrix(X_training), label = y_training)
  dtest <- xgb.DMatrix(data = as.matrix(X_test), label = y_test)
  
  # Setting Params ----------------------------------------------------------
  
  params <- list(
    objective = "binary:logistic",
    eval_metric = "logloss",
    booster = "gbtree",
    eta = 0.1,
    max_depth = 4,
    subsample = 0.8,
    colsample_bytree = 0.4
  )
  
  # Cross Validation --------------------------------------------------------
  
  cv_results <- xgb.cv(
    params = params,
    data = dtrain,
    nrounds = 50,
    early_stopping_rounds = 10,
    folds = createFolds(y_training, k = 5, list = TRUE, returnTrain = TRUE),
    stratified = TRUE,
    print_every_n = 10,
    verbose = 0
  )
  
  # Get the best number of iterations
  
  best_rounds <- cv_results$best_iteration
  
  # Get the best parameters
  
  best_params <- params
  
  # Training Model ----------------------------------------------------------
  
  xgb_model <- xgb.train(params = best_params, data = dtrain, nrounds = best_rounds)
  
  # Visualize feature importance
  
  importance_matrix <- xgb.importance(feature_names = colnames(X_training), model = xgb_model)
  xgb.plot.importance(importance_matrix)
  
  # Model Evaluation --------------------------------------------------------
  
  predictions <- predict(xgb_model, as.matrix(X_test))
  predicted_labels <- ifelse(predictions > 0.5, 1, 0)
  
  confusionMatrix(table(predicted_labels, y_test))
  
  accuracy <- sum(predicted_labels == y_test) / length(y_test)
  cat("Accuracy:", round(accuracy, digits = 4), "\n")
  
