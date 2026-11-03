library(terra)

All <- rast("Z:/Christine/NWT Fire Refugia Project Z/Prediction/V6/FRUs/Full NWT FRUs 120m.tif")
Shield <- rast("Z:/Christine/NWT Fire Refugia Project Z/Prediction/V6/FRUs/Individual/Singles before grouping/FRU_26_shield.tif")

Out <- merge(Shield, All, first=T)
plot(Out)

writeRaster(Out, filename = "Z:/Christine/NWT Fire Refugia Project Z/Manuscript/Data/FRUs.tif")
