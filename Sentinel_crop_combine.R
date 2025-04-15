## In diesem Script wird ein Sentinel-2 Stack mit allen benötigten Bändern und Prädiktoren erstellt. 
## Anschließend wird das Raster auf die Größe des Untersuchungsgebiets zugeschnitten.

rm(list=ls()) 
library(terra) 
library(mapview)
library(raster)


# Arbeitsverzeichnis 
setwd("/Users/philippmundinger/Documents/Bachelorarbeit/Sentinel/Daten/A_Sentinel/Sentinel_260923/GRANULE/L2A_T32UMB_A043148_20230926T103133/IMG_DATA/")

# 10m und 20m Kanäle einladen
senstack_10 <- rast(c("R10m/T32UMB_20230926T102801_B02_10m.jp2",  # Blau 490 Znm
                      "R10m/T32UMB_20230926T102801_B03_10m.jp2",  # Grün 560 Znm
                      "R10m/T32UMB_20230926T102801_B04_10m.jp2",  # Rot 665 Znm
                      "R10m/T32UMB_20230926T102801_B08_10m.jp2")) # NIR 842 Znm

# Weil B02,03,04 fehlerhaft als "Grayscale" benannt werden 
names(senstack_10) <- c("T32UMB_20230926T102801_B02_10m", "T32UMB_20230926T102801_B03_10m", "T32UMB_20230926T102801_B04_10m", "T32UMB_20230926T102801_B08_10m")


senstack_20 <- rast(c("R20m/T32UMB_20230926T102801_B05_20m.jp2",  # Red Edge 1 (Vegetation) 705 Znm 
                      "R20m/T32UMB_20230926T102801_B06_20m.jp2",  # Red Edge 2 (Vegetation) 740 Znm
                      "R20m/T32UMB_20230926T102801_B07_20m.jp2",  # Red Edge 3 (Vegetation) 783 Znm
                      "R20m/T32UMB_20230926T102801_B11_20m.jp2",  # SWIR 1 1610 Znm
                      "R20m/T32UMB_20230926T102801_B12_20m.jp2",  # SWIR 2 2190 Znm
                      "R20m/T32UMB_20230926T102801_B8A_20m.jp2"))  # Narrow NIR 865 Znm

# Shapfile als Maske nutzen
library(sf)
setwd("/Users/philippmundinger/Desktop/Bachelorarbeit _final/Datengrundlage")
maske <- st_read("maskeShape.shp")  # Shapefile laden 
maske <- st_transform(maske, crs(senstack_10)) # Shapefile crs auf das von sentinel anpasse
maske_vect <- vect(maske) # Shapefile in ein SpatVector umwandeln

senstack_10 <- mask(senstack_10, maske_vect) # Raster mit der Shapefile maskieren
senstack_20 <- mask(senstack_20, maske_vect) 

# in einem 20m Stack zusammenführen 
senstack_20_res <- resample(senstack_20,senstack_10)
senstack_all <- rast(list(senstack_10,senstack_20_res))

# Stack Größe anpassen 
senstack <- crop(senstack_all, maske)

# Namen anpassen
names(senstack) 
names(senstack) <- substr(names(senstack_all),
                              nchar(names(senstack_all))-6, # von der 6-letzten Stelle...
                              nchar(names(senstack_all))-4) #bis zur 4-letzten

names(senstack)
plot(senstack)

plot(senstack$B08)

# kombinierten Datensatz rausschreiben:
writeRaster(senstack,"/Users/philippmundinger/Desktop/Bachelorarbeit _final/Datengrundlage/Sentinel_combined.tif",overwrite=T)

### Prädiktoren hinzufügen 
setwd("/Users/philippmundinger/Desktop/Bachelorarbeit _final/Datengrundlage/")
rasterstack <- rast("Sentinel_combined.tif")

### NDVI berechnen
# NIR(B08) - rot(B04) / NIR + rot
rasterstack$NDVI <- (rasterstack$B08-rasterstack$B04)/(rasterstack$B08+rasterstack$B04)

# NDVI Texturen berechnen und hinzufügen 
rasterstack$NDVI_3x3_sd <- focal(rasterstack$NDVI,w=matrix(1,3,3), fun=sd)
rasterstack$NDVI_5x5_sd <- focal(rasterstack$NDVI,w=matrix(1,5,5), fun=sd)
plot(rast(list(rasterstack$NDVI_3x3_sd,rasterstack$NDVI_5x5_sd)))

### Koordinaten hinzufügen 
template <- rasterstack$B02 # Kopie erstellen
template_ll <- project(template,"+proj=longlat +datum=WGS84 +no_defs") #in Latlong umprojizieren
coords <- crds(template_ll,na.rm=FALSE) # Koordinaten für jedes Pixel als tabelle bekommen
lat <- template_ll # Kopie erstellen
lon <- template_ll# Kopie erstellen
values(lat) <- coords[,2] # Koordinaten auf das Raster schreiben
values(lon) <- coords[,1] # Koordinaten auf das Raster schreiben
coords <- rast(list(lat,lon)) # neuen Steck erstellen
names(coords) <- c("lat","lon") #...und benennen
coords <- project(coords,crs(rasterstack)) # wieder in Projektion des Combiend stacks bringen
coords <- resample(coords,rasterstack) # und auf die gleiche Geometrie
rasterstack <- rast(list(rasterstack,coords)) # neuen Stack dem rasterstack hinzufügen 

# ausschreiben als prdictor.tif
writeRaster(rasterstack, "sentinel_predictor.tif", datatype = "FLT4S", overwrite = TRUE)
