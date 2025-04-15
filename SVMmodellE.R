## in diesem Script wird ein Support Vector Machines Modell 
## zur klassifizierung des EnMAP-Rasters erstellt

rm(list = ls())
library(terra)
library(sf)
library(caret)
library(CAST)


# Arbeitsverzeichnis
setwd("/Users/philippmundinger/Desktop/Bachelorarbeit _final/EnMAP/")
enmap <- rast("enmap219.tif") # Tif laden
trainDat <- readRDS("Trainingsdaten/Trainingsdaten.RDS") # Trainingsdaten laden 


# Prädiktoren
predictors <- c(
  "B1", "B2", "B3", "B4", "B5", "B6", "B7", "B8", "B9", "B10", 
  "B11", "B12", "B13", "B14", "B15", "B16", "B17", "B18", "B19", "B20", 
  "B21", "B22", "B23", "B24", "B25", "B26", "B27", "B28", "B29", "B30", 
  "B31", "B32", "B33", "B34", "B35", "B36", "B37", "B38", "B39", "B40", 
  "B41", "B42", "B43", "B44", "B45", "B46", "B47", "B48", "B49", "B50", 
  "B51", "B52", "B53", "B54", "B55", "B56", "B57", "B58", "B59", "B60", 
  "B61", "B62", "B63", "B64", "B65", "B66", "B67", "B68", "B69", "B70", 
  "B71", "B72", "B73", "B74", "B75", "B76", "B77", "B78", "B79", "B80", 
  "B81", "B82", "B83", "B84", "B85", "B86", "B87", "B88", "B89", "B90", 
  "B91", "B92", "B93", "B94", "B95", "B96", "B97", "B98", "B99", "B100",
  "B101", "B102", "B103", "B104", "B105", "B106", "B107", "B108", "B109", "B110", 
  "B111", "B112", "B113", "B114", "B115", "B116", "B117", "B118", "B119", "B120",
  "B121", "B122", "B123", "B124", "B125", "B126", "B127", "B128", "B129", "B136", 
  "B137", "B138", "B139", "B140", "B141", "B142", "B143", "B144", "B145", 
  "B146", "B147", "B148", "B149", "B150", "B151", "B152", "B153", "B154", 
  "B155", "B156", "B157", "B158", "B159", "B160", "B161", "B162", "B163", 
  "B164", "B165", "B166", "B167", "B168", "B169", "B170", "B171", "B172", 
  "B173", "B174", "B175", "B176", "B177", "B178", "B179", "B180", "B181", 
  "B182", "B183", "B184", "B185", "B186", "B187", "B188", "B189", "B190", 
  "B191", "B192", "B193", "B194", "B195", "B196", "B197", "B198", "B199", 
  "B200", "B201", "B202", "B203", "B204", "B205", "B206", "B207", "B208", 
  "B209", "B210", "B211", "B212", "B213", "B214", "B215", "B216", "B217", 
  "B218", "B219", "B220", "B221", "B222", "B223", "B224"
)

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
svm_model_grid_enmap <- train(trainData[, predictors],
                              trainData$Label,
                              method = "svmRadial",
                              trControl = ctrl,
                              tuneGrid = svm_grid,
                              metric = "Accuracy")
 
# Ergebnis 
svm_model_grid_enmap
<- <- best <- svm_model_grid_enmap$bestTune
cat("\nBeste Kombination:\n", best)


# Ergebnis Grid plotten
ggplot(svm_model_grid_enmap$results, aes(x = as.factor(C), y = as.factor(sigma), fill = Accuracy)) +
  geom_tile(color = "white") +
  scale_fill_gradient2(low = "red", mid = "yellow", high = "springgreen4",
                       midpoint = median(svm_model_grid_enmap$results$Accuracy)) +
  labs(
    title = "SVM Hyperparameter-Tuning",
    x = "C-Wert",
    y = "Sigma-Wert",
    fill = "Accuracy"
  ) +
  theme_minimal(base_size = 14)

plot(svm_model_grid)  # Einfluss von C & sigma auf Accuracy

# Konfusionsmatrix erstellen
preds <- svm_model_grid_enmap$pred[svm_model_grid_enmap$pred$C == svm_model_grid_enmap$bestTune$C & 
                               svm_model_grid_enmap$pred$sigma == svm_model_grid_enmap$bestTune$sigma,]
table(Predicted = preds$pred, Actual = preds$obs)

# F1-Scores 
conf_matrix <- confusionMatrix(preds$pred, preds$obs)
f1_scores <- conf_matrix$byClass[, "F1"]
f1_scores

 # Konfusionsmatrix 
print(conf_matrix)

# Raster klassifiziert und darstellen
lvls <- list(levels(svm_model_grid_enmap$trainingData$.outcome))

prediction <- predict(as(enmap,"Raster"),svm_model_grid_enmap)
prediction_terra <- as(prediction,"SpatRaster")

# Darstellung der Klassen definieren
cols <- c( "#2c1c14","lightgreen", "orange","forestgreen",
           "darkgreen","red","turquoise","blue","#B6FF00")

plot(prediction_terra,col=cols)

