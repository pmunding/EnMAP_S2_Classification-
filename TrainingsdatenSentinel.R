## In diesem Script werden Trainingsdaten und Rasterdaten der Sentinel-2-Szene zusammengeführt.
## Anschließend wird in einem Scatterplot die Trennbarkeit der Klassen dargestellt, um einen 
## ersten Überblick über den Einfluss verschiedener Bänder zu haben

rm(list=ls())
library(terra)
library(sf)
library(mapview)
library(caret)

# Arbeitsverzeichnis
setwd("/Users/philippmundinger/Desktop/Bachelorarbeit _final/Sentinel/")
trainingsites <- st_read("Trainingsdaten/TrainingsdatenSentinel.shp")
sentinel <- rast("sentinel_predictor.tif")

plotRGB(sentinel,r=3,g=2,b=1,stretch="lin")
plot(trainingsites,add=TRUE) 
# CRS anpassen
trainingsites <- st_transform(trainingsites,crs(sentinel))

### Trainingsdaten und Rasterdaten kombinieren 
extr <- extract(sentinel,trainingsites)
trainingsites$PolyID <- 1:nrow(trainingsites) 
extr <- merge(extr,trainingsites,by.x="ID",by.y="PolyID")
extr$Label <- trainingsites$Label[extr$ID]
head(extr)
saveRDS(extr,file="Trainingsdaten/Trainingsdaten.RDS") 


# Visualisierung Trennbarkeit der Klassen ###########################################
# Boxplot
boxplot(extr$NDVI~extr$Label,las=2)
boxplot(extr$B08~extr$Label,las=2)


# Feature plot
library(caret)

# nur 20% der Daten 
extr_subset <- extr[createDataPartition(extr$ID,p=0.2)$Resample1,]

featurePlot(extr_subset[,c("B02","B03","B08","NDVI_3x3_sd")],
            factor(extr_subset$Label),plot="pairs",
            auto.key = list(columns = 2))

### Visualisierung anpassen
myColors<- c("#000000", "#7fff00", "#8b0000", "#9932cc", "#ff7f00",
             "#458b00", "#008b8b", "#0000ff", "#ffff00","darkred","darkblue")
my_settings <- list(superpose.symbol=list(col=myColors,
                                          fill= myColors))

### Feature Plot:
featurePlot(extr_subset[,c("B02","B03","B08","NDVI_3x3_sd")],
            factor(extr_subset$Label),plot="pairs",
            auto.key = list(columns = 2),
            par.settings=my_settings)







