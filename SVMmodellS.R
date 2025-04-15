## in diesem Script wird ein Support Vector Machines Modell 
## zur klassifizierung des S2-Rasters erstellt.

rm(list=ls())
library(terra)
library(sf)
library(mapview)
library(caret)

# Arbeitsverzeichnis
setwd("/Users/philippmundinger/Desktop/Bachelorarbeit _final/Sentinel/")
sentinel <- rast("sentinel_predictor.tif") # Raster laden
trainDat <- readRDS("Trainingsdaten/Trainingsdaten.RDS") # Trainingsdaten laden 


# Prädiktoren definieren
predictors <- c("B02","B03","B04","B08","B05","B06","B07","B11",
                "B12","B8A","NDVI","NDVI_3x3_sd","NDVI_5x5_sd")


trainDat$Label <- factor(trainDat$Label)  # Sicherstellen, dass Labels als Faktor vorliegen

# 70% der Trainingsdaten für das Training nutzen
trainIDs <- createDataPartition(trainDat$ID, p = 0.7, list = FALSE)
trainData <- trainDat[trainIDs, ]
trainData <- trainData[complete.cases(trainData[, predictors]), ] 


### Modelltraining mit räumlicher Kreuzvalidierung
trainids <- CreateSpacetimeFolds(trainData, spacevar = "ID", class = "Label", k = 3)

ctrl <- trainControl(method = "cv",
                     index = trainids$index,
                     indexOut = trainids$indexOut,
                     savePredictions = "final")

# abgestimmtes Grid für die optimale Parameterauswahl
C_values <- c(2, 3, 4, 5, 6, 8, 10, 12, 14, 16)
sigma_values <- c(0.02, 0.025, 0.03, 0.035, 0.04, 0.05, 0.06)
svm_grid <- expand.grid(sigma = sigma_values, C = C_values)

# mit manuellem Grid trainieren
svm_model_grid_se <- train(trainData[, predictors],
                           trainData$Label,
                           method = "svmRadial",
                           trControl = ctrl,
                           tuneGrid = svm_grid,
                           metric = "Accuracy")

# Ergebnis 
svm_model_grid_se
best <- svm_model_grid_se$bestTune
best

# Ergebnis-Matrix als Heatmap plotten
ggplot(svm_model_grid_se$results, aes(x = as.factor(C), y = as.factor(sigma), fill = Accuracy)) +
  geom_tile(color = "white") +
  scale_fill_gradient2(low = "red", mid = "yellow", high = "springgreen4",
                       midpoint = median(svm_model_grid_se$results$Accuracy)) +
  labs(
    title = "SVM Hyperparameter-Tuning Sentinel",
    x = "C-Wert",
    y = "Sigma-Wert",
    fill = "Accuracy"
  ) +
  theme_minimal(base_size = 14)

# Konfusionsmatrix erstellen
preds <- svm_model_grid_se$pred[svm_model_grid_se$pred$C == svm_model_grid_se$bestTune$C & 
                                     svm_model_grid_se$pred$sigma == svm_model_grid_se$bestTune$sigma,]
table(Predicted = preds$pred, Actual = preds$obs)

# F1-Scores 
conf_matrix <- confusionMatrix(preds$pred, preds$obs)
f1_scores <- conf_matrix$byClass[, "F1"]
f1_scores

# Raster klassifizieren und darstellen
lvls <- list(levels(svm_model_grid_se$trainingData$.outcome))
prediction <- predict(as(sentinel,"Raster"),svm_model_grid_se)
prediction_terra <- as(prediction,"SpatRaster")

# Darstellung der Klassen definieren
cols <- c( "#2c1c14","lightgreen", "orange","forestgreen",
           "darkgreen","red","turquoise","blue","#B6FF00")

plot(prediction_terra,col=cols)

