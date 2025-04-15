## In diesem Script werden Trainingsdaten und Rasterdaten der EnMAP-Szene zusammengeführt.
## Anschließend wird in einem Scatterplot die Trennbarkeit der Klassen dargestellt, um einen 
## ersten Überblick über den Einfluss verschiedener Bänder zu haben

rm(list=ls())
library(terra)
library(raster)
library(sf)
library(caret)
library(CAST)
library(dplyr)


# Arbeitsverzeichnis
setwd("/Users/philippmundinger/Desktop/Bachelorarbeit _final/EnMAP/")
#list.files()

# Trainingsdaten laden und das passende Koordinatensystem zuweisen 
trainingsites <- st_read("Trainingsdaten/TrainingsdatenEnMAP.shp") 
enmap <- rast("enmap219.tif") # Tif laden
trainingsites <- st_transform(trainingsites, crs = crs(enmap))

# als RGB darstellen, Trainingsdaten drüber legen 
plotRGB(enmap,r=48,g=30,b=16,stretch="lin")
plot(st_geometry(trainingsites), add = TRUE, col = "red", border = "red", lwd = 2)


# Trainingsdaten die Eigenschaften der predictor.tif Daten geben
extr <- extract(enmap,trainingsites)
trainingsites$PolyID <- 1:nrow(trainingsites) 
extr <- merge(extr,trainingsites,by.x="ID",by.y="PolyID")

# Anzahl der Polygone und bildpunkte pro Klasse
head(extr)
pixel_pro_klasse <- extr %>%
  group_by(Label) %>%
  summarise(Pixelanzahl = n())

# Speichern
saveRDS(extr,file="Trainingsdaten/Trainingsdaten.RDS") 


### Trennbarkeit der Klassen in einem Featureplot darstellen 
# nur 20%  Daten 
extr_subset <- extr[createDataPartition(extr$ID,p=0.2)$Resample1,]

# Featureplot Darstellung anpassen 
myColors<- c("#000000", "#7fff00", "red", "#458b00", "darkgreen","orange",
             "#008b8b", "#0000ff")
my_settings <- list(superpose.symbol=list(col=myColors,fill= myColors))

# Plot
featurePlot(extr_subset[,c(48,31,16,64,120,219)],
            factor(extr_subset$Label),
            plot="pairs",
            auto.key = list(columns = 2),
            par.settings=my_settings)

## weitere Klassen untersuchen 
# Nur die Klassen "Laubwald" und "Wasser" verwenden
extr_subset_filtered <- extr_subset[extr_subset$Label %in% c("Laubwald", "Nadelwald"), ]
newColors<- c("red", "blue")
new_settings <- list(superpose.symbol=list(col=newColors,fill= newColors))
# Featureplot Visualisierung anpassen und nur "Wiese" und "Acker_bepflant" anzeigen
featurePlot(extr_subset[,c(48,64, 120, 219)],
            factor(extr_subset_filtered$Label), plot = "pairs",
            auto.key = list(columns = 2),
            par.settings = new_settings)










