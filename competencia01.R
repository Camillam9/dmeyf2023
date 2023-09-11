# cargo las librerias que necesito
require("data.table")
require("rpart")
require("rpart.plot")

# Aqui se debe poner la carpeta de la materia de SU computadora local
setwd("/Users/camillamassardi/Desktop/DMEyF/") # Establezco el Working Directory

# cargo el dataset
dataset <- fread("./datasets/competencia_01.csv")

library(data.table)

# Calculate the correlation matrix for numeric columns
numeric_cols <- names(dataset)[sapply(dataset, is.numeric)]
cor_matrix <- cor(dataset[, ..numeric_cols])

# Set a threshold for correlation (e.g., 0.7)
cor_threshold <- 0.7

# Find highly correlated pairs
highly_correlated_pairs <- which(upper.tri(cor_matrix, diag = FALSE) & abs(cor_matrix) > cor_threshold, arr.ind = TRUE)

# Identify columns to drop
columns_to_drop <- character(0)
for (i in 1:nrow(highly_correlated_pairs)) {
  col1 <- rownames(cor_matrix)[highly_correlated_pairs[i, 1]]
  col2 <- rownames(cor_matrix)[highly_correlated_pairs[i, 2]]
  
  # Keep one of the columns in each pair (you can choose which one to keep)
  # In this example, we keep the first column in each pair
  columns_to_drop <- c(columns_to_drop, col2)
}

# Remove highly correlated columns
dataset <- dataset[, !columns_to_drop, with = FALSE]

dtrain <- dataset[foto_mes == 202103] # defino donde voy a entrenar
dapply <- dataset[foto_mes == 202105] # defino donde voy a aplicar el modelo

# genero el modelo,  aqui se construye el arbol
# quiero predecir clase_ternaria a partir de el resto de las variables
modelo <- rpart(
  formula = "clase_ternaria ~ .",
  data = dtrain, # los datos donde voy a entrenar
  xval = 0,
  cp = -0.5, # esto significa no limitar la complejidad de los splits
  minsplit = 600, # minima cantidad de registros para que se haga el split
  minbucket = 2, # ta6maño minimo de una hoja
  maxdepth = 12
) # profundidad maxima del arbol



# grafico el arbol
prp(modelo,
    extra = 101, digits = -5,
    branch = 1, type = 4, varlen = 0, faclen = 0
)


# aplico el modelo a los datos nuevos
prediccion <- predict(
  object = modelo,
  newdata = dapply,
  type = "prob"
)

# prediccion es una matriz con TRES columnas,
# llamadas "BAJA+1", "BAJA+2"  y "CONTINUA"
# cada columna es el vector de probabilidades

# agrego a dapply una columna nueva que es la probabilidad de BAJA+2
dapply[, prob_baja2 := prediccion[, "BAJA+2"]]

# solo le envio estimulo a los registros
#  con probabilidad de BAJA+2 mayor  a  1/40
dapply[, Predicted := as.numeric(prob_baja2 > 1 / 40)]

# genero el archivo para Kaggle
# primero creo la carpeta donde va el experimento
dir.create("./exp/")
dir.create("./exp/KA2001")

# solo los campos para Kaggle
fwrite(dapply[, list(numero_de_cliente, Predicted)],
       file = "./exp/KA2001/K101_001corr.csv",
       sep = ","
)
