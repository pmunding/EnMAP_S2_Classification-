# In diesem Script wird eine Fast Feature Selection auf den EnMAP-Daten durchgeführt 

rm(list=ls())
library(terra)
library(raster)
library(sf)
library(caret)
library(CAST)

# Arbeitsverzeichnis
setwd("/Users/philippmundinger/Desktop/Bachelorarbeit _final/EnMAP/FFS")
trainDat <- readRDS("Trainingsdaten/Trainingsdaten.RDS")
enmap <- rast("enmap219.tif")

### Das Modell mit einer Forward Feature selection trainieren 
imp <- varImp(model)$importance
top_vars <- rownames(imp)[order(rowMeans(imp), decreasing = TRUE)][1:50]
top_vars
# Erstelle räumliche Folds 3-Fold 
trainids <- CreateSpacetimeFolds(trainDat, spacevar = "ID", class = "Label", k = 3)
# Kontrollstruktur für train()
ctrl <- trainControl(method = "cv",
                     index = trainids$index,
                     indexOut = trainids$indexOut,
                     savePredictions = TRUE)

ffs_model <- ffs(trainDat[, top_vars],
                 trainDat$Label,
                 method = "rf",
                 metric = "Kappa",
                 trControl = ctrl,
                 tuneGrid = data.frame(mtry = 7))  # siehst du gleich unten

# eventuell Finales Modell speichern
save(ffs_model,file="ffsmodel.RData") 
saveRDS(ffs_model,file="ffsmodel.RDS") 

# Konfusionsmatrix 
preds <- ffs_model$pred[ffs_model$pred$mtry==ffs_model$bestTune$mtry,]
table(preds$pred,preds$obs)

# F1-Score berechnen 
conf_matrix <- confusionMatrix(preds$pred, preds$obs) # Berechne die Confusion Matrix
conf_matrix$byClass[, "F1"] # Zeigt die F1-Scores für jede Klasse

plot(ffs_model)
plot(ffs_model,plotType="selected")

importance <- varImp(ffs_model)
plot(importance)

# Raster klassifiziert und darstellen
lvls <- list(levels(ffs_model$trainingData$.outcome))

prediction <- predict(as(enmap,"Raster"),ffs_model)
prediction_terra <- as(prediction,"SpatRaster")

# Darstellung der Klassen definieren
cols <- c( "#2c1c14","lightgreen", "orange","forestgreen",
           "darkgreen","red","turquoise","blue","#B6FF00")

plot(prediction_terra,col=cols)



## Modell laden 
ffs_model <- readRDS("ffsmodel.RDS")

# Layer der benötigten Bänder erstellen
enmap_combined <- subset(enmap, c(29, 122, 8, 21, 9, 11, 22, 28, 14, 20, 
                                  127, 12, 17))
# namen zuweisen
names(enmap_combined) <- c("B29", "B122", "B8", "B21", "B9", "B11", "B22", "B28", "B14", "B20", 
                           "B127", "B12", "B17")

writeRaster(enmap_combined, "enmapFFS.tif", datatype = "FLT4S", overwrite=TRUE)



### Anschließend wurde eine Klassifikation nur mit den Ausgewählten Bändern durchgeführt

### Trainingsdaten für den Stack erstellen 
# Trainingsdaten laden
trainingsites <- st_read("Trainingsdaten/TrainingsdatenEnMAP.shp") 
# Trainingsdaten das passende Koordinatensystem zuweisen 
trainingsites <- st_transform(trainingsites, crs = crs(enmap_combined))

# Trainingsdaten die Eigenschaften der predictor.tif Daten geben
extr <- extract(enmap_combined,trainingsites)
trainingsites$PolyID <- 1:nrow(trainingsites) 
extr <- merge(extr,trainingsites,by.x="ID",by.y="PolyID")
# Anzahl der Polygone und bildpunkte pro Klasse
library(dplyr)
head(extr)

pixel_pro_klasse <- extr %>%
  group_by(Label) %>%
  summarise(Pixelanzahl = n())
pixel_pro_klasse #Ausgeben 

# Speichern
saveRDS(extr,file="Trainingsdaten/Trainingsdaten.RDS") 

### Modell training mit RF
setwd("/Users/philippmundinger/Desktop/Bachelorarbeit _final/EnMAP/FFS")
trainDat <- readRDS("Trainingsdaten/Trainingsdaten.RDS")

predictors <- c("B29", "B122", "B8", "B21", "B9", "B11", "B22", "B28", "B14", "B20", 
                 "B127", "B12", "B17")

# 70% der Trainingsdaten für das trainieren des Modells verwenden 
trainIDs <- createDataPartition(trainDat$ID, p=1.0, list=FALSE)
trainData <- trainDat[trainIDs,]
trainDat <- trainDat[complete.cases(trainDat[,predictors]),] 

#### räumliche Kreuzvalidierung erstellen
trainids <- CreateSpacetimeFolds(trainDat,spacevar="ID",class="Label",k=3)

ctrl <- trainControl(method="cv",
                     index=trainids$index,
                     indexOut = trainids$indexOut,
                     savePredictions=TRUE)

model <- train(trainDat[,predictors],
               trainDat$Label,
               method="rf",
               importance=TRUE,
               ntree=500,
               trControl=ctrl)
model


# nur 20%  Daten (ausreichend und reduziert den Aufwand)
extr <- trainDat
extr_subset <- extr[createDataPartition(extr$ID,p=0.2)$Resample1,]

# Featureplot Darstellung anpassen 
myColors<- c("#000000", "#7fff00", "red", "#458b00", "darkgreen","orange",
             "#008b8b", "#0000ff")
my_settings <- list(superpose.symbol=list(col=myColors,fill= myColors))
# Plot
feature_vars <- c("B29", "B122", "B8", "B21", "B11", "B22", "B28", "B14", 
                  "B20", "B127", "B12", "B17")

featurePlot(extr_subset[, feature_vars],
            factor(extr_subset$Label),
            plot = "pairs",
            auto.key = list(columns = 2),
            par.settings = my_settings)

## weitere Klassen untersuchen 
# Nur die Klassen "Laubwald" und "Wasser" verwenden
extr_subset_filtered <- extr_subset[extr_subset$Label %in% c("Laubwald", "Nadelwald"), ]
newColors<- c("red", "blue")
new_settings <- list(superpose.symbol=list(col=newColors,fill= newColors))
# Featureplot Visualisierung anpassen und nur "Wiese" und "Acker_bepflant" anzeigen
featurePlot(extr_subset[, feature_vars],
            factor(extr_subset_filtered$Label), plot = "pairs",
            auto.key = list(columns = 2),
            par.settings = new_settings)



library(ggplot2)
library(tidyr)
library(dplyr)

# 13 Features definieren
feature_vars <- c("B29", "B122", "B8", "B21", "B9", "B11", "B22", "B28", "B14", "B20", 
                  "B127", "B12", "B17")

# Nur die gewünschten Klassen
extr_subset_filtered <- extr[extr$Label %in% c("Solarpark", "Industrie"), ]

long_df <- dplyr::select(extr_subset_filtered, all_of(feature_vars), Label) %>%
  tidyr::pivot_longer(cols = all_of(feature_vars), names_to = "Band", values_to = "Wert")

# Plotten als Boxplot
ggplot(long_df, aes(x = Band, y = Wert, fill = Label)) +
  geom_boxplot(position = "dodge", outlier.alpha = 0.2) +
  scale_fill_manual(values = c("Solarpark" = "darkgreen", "Industrie" = "deepskyblue")) +
  theme_minimal() +
  labs(title = "Spektrale Verteilung pro Band",
       x = "Spektralband", y = "Reflexion", fill = "Klasse") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

