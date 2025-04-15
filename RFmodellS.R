## in diesem Script wird ein RF-Modell zur Klassifikation der S2-Daten erstellt
## das Training und die Validierung läuft über eine 3-Fold räumliche Crossvalidation

rm(list=ls())
library(terra)
library(raster)
library(sf)
library(caret)
library(CAST)

# Arbeitsverzeichnis
setwd("/Users/philippmundinger/Desktop/Bachelorarbeit _final/Sentinel/")
sentinel <- rast("sentinel_predictor.tif") # Raster laden
trainDat <- readRDS("Trainingsdaten/Trainingsdaten.RDS") # Trainingsdaten laden

# Prädiktoren definieren
predictors <- c("B02","B03","B04","B08","B05","B06","B07","B11",
                "B12","B8A","NDVI","NDVI_3x3_sd","NDVI_5x5_sd")

# 70% der Trainingsdaten für das Training nutzen
# Verhälnis der Daten aus jedem Trainingsgebiet beibehalten
trainIDs <- createDataPartition(trainDat$ID,p=0.7,list = FALSE)
trainDat <- trainDat[trainIDs,]
trainDat <- trainDat[complete.cases(trainDat[,predictors]),]


### Modelltraining mit räumlicher Kreuzvalidierung
trainids <- CreateSpacetimeFolds(trainDat,spacevar="ID",class="Label",k=3)

ctrl <- trainControl(method="cv",
                     index=trainids$index,
                     indexOut = trainids$indexOut,
                     savePredictions=TRUE)

model <- train(trainDat[,predictors],
               trainDat$Label,
               method="rf",
               importance=TRUE,
               ntree=200,          # ntree = 200 als optimal ausgewählt
               trControl=ctrl)
model
save(model,file="RFmodelS.RData") # Modell speichern

# Konfusionsmatrix berechnen
preds <- model$pred[model$pred$mtry==model$bestTune$mtry,]
table(preds$pred,preds$obs)

# Raster klassifizieren und darstellen
lvls <- list(levels(model$trainingData$.outcome))
prediction <- predict(as(sentinel,"Raster"),model)
prediction_terra <- as(prediction,"SpatRaster")

# Darstellung der Klassen definieren
cols <- c( "#2c1c14","lightgreen", "orange","forestgreen",
           "darkgreen","red","turquoise","blue","#B6FF00")

plot(prediction_terra,col=cols)

# F1-Score berechnen 
conf_matrix <- confusionMatrix(preds$pred, preds$obs) # Berechne die Confusion Matrix
conf_matrix$byClass[, "F1"] # Zeigt die F1-Scores für jede Klasse

# Variablen gewichtung anszeigen 
importance <- varImp(model)
plot(importance)




# Trennbarkeit der Klassen untersuchen 
library(ggplot2)
library(tidyr)
library(dplyr)

extr <- trainDat

# 1. Deine 13 Features definieren
feature_vars <- c("B02","B03","B04","B08","B05","B06","B07","B11",
                  "B12","B8A","NDVI","NDVI_3x3_sd","NDVI_5x5_sd")

# 2. Nur die gewünschten Klassen
extr_subset_filtered <- extr[extr$Label %in% c("Laubwald", "Nadelwald"), ]

long_df <- dplyr::select(extr_subset_filtered, all_of(feature_vars), Label) %>%
  tidyr::pivot_longer(cols = all_of(feature_vars), names_to = "Band", values_to = "Wert")

# 4. Plotten als Boxplot
ggplot(long_df, aes(x = Band, y = Wert, fill = Label)) +
  geom_boxplot(position = "dodge", outlier.alpha = 0.2) +
  scale_fill_manual(values = c("Laubwald" = "darkgreen", "Nadelwald" = "deepskyblue")) +
  theme_minimal() +
  labs(title = "Spektrale Verteilung pro Band",
       x = "Spektralband", y = "Reflexion", fill = "Klasse") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))




