#He elegido ligthgbm ya que se utiliza para la clasificacion de datos muy grandes y desbalanceados 
#Cargamos las librerías
library(lightgbm)
library(tidyverse)
library(pROC)
library(caret)
library(readr)
library(caTools)


# cargamos el dataset 
data <- read_csv("dataset_cards.csv")
head(data)
summary(data)
colnames(data)

#Exploramos la distribución de las clases de la columna isFraud. 
#Vemos que esta desbalanceado por lo que luego utilizaremos un parametro para que estén más balanceados
cantidad_isFraud <- table(data$isFraud) %>% View()

# Codificamos la columna 'type' con LabelEncoder
data$type <- as.numeric(factor(data$type))

# Filtrar las transacciones fraudulentas
fraud <- subset(data, isFraud == 1, select = c('step', 'type', 'amount', 'oldbalanceOrg', 'newbalanceOrig', 'oldbalanceDest', 'newbalanceDest', 'isFraud'))

#Exploramos la columna 'type' y vemos que solo tiene los valores 2 y 5 que corresponden a TRANSFER y a CASH_OUT
print(unique(fraud$type))

#Vemos las filas con no fraude
fraud_no <- subset(data, isFraud == 0)

# Seleccionamos las columnas y las filas relevantes
data1 <- subset(data, type %in% c(2, 5), select = c('step', 'type', 'amount', 'oldbalanceOrg', 'newbalanceOrig', 'oldbalanceDest', 'newbalanceDest', 'isFraud'))

# Dividir el conjunto de datos en entrenamiento y prueba
set.seed(100)
split <- sample.split(data1$isFraud, SplitRatio = 0.8)
train_data <- subset(data1, split == TRUE)
test_data <- subset(data1, split == FALSE)

# Configuramos X e Y para el train
X_train <- train_data[, !(names(train_data) %in% c('isFraud', 'type'))]
Y_train <- train_data$isFraud

# Configuramos X e Y para el test
X_test <- test_data[, !(names(test_data) %in% c('isFraud', 'type'))]
Y_test <- test_data$isFraud

# Convertimos data a un formato de dataset LightGBM 
train_data_lgb <- lgb.Dataset(data = as.matrix(X_train), label = as.numeric(Y_train))
test_data_lgb <- lgb.Dataset(data = as.matrix(X_test), label = as.numeric(Y_test), reference = train_data_lgb)

# Configurar los parametros de LightGBM
params <- list(
  objective = "binary",
  metric = "binary_error",
  boosting_type = "gbdt",
  max_depth = 4,
  num_leaves = 8,
  learning_rate = 0.05,
  feature_fraction = 0.8,
  is_unbalance = TRUE
)

#Realizamos validacion cruzada
cross_validation <- lgb.cv(
  params = params,
  data = train_data_lgb,
  nrounds = 100,
  early_stopping_rounds = 40,
  folds = createFolds(Y_train, k = 5, list = TRUE, returnTrain = TRUE),
  stratified = TRUE, 
  verbose = 10
)

# Obtenemos el mejor número de iteraciones y los mejores parámetros
best_rounds <- cross_validation$best_iter
best_params <- params
best_params$num_rounds <- best_rounds

# Entrenamos el modelo
model <- lgb.train(params=best_params, data = train_data_lgb)

# Realizamos las predicciones
predictions <- predict(model, as.matrix(X_test))

# Convertimos las probabilidades predichas a clases binarias (0 o 1)
predicted_classes <- ifelse(predictions > 0.5, 1, 0)

# Creamos matriz de confusion
conf_matrix <- confusionMatrix(factor(predicted_classes), factor(Y_test))
print(conf_matrix)

# Evaluamos el modelo utilizando la curva ROC y AUC
curva_roc <- roc(Y_test, predictions)
auc_value <- auc(curva_roc)
print(paste("AUC:", auc_value))

#Dibujamos la curva ROC
plot.roc(curva_roc, col = "blue", main = "Curva ROC", col.main = "darkblue", lwd = 2, legacy.axes = TRUE, print.auc =TRUE)

