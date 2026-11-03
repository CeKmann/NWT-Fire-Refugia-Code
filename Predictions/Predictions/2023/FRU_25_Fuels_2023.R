library(raster)
library(gbm)
library(dismo)

# Load model
Model <- readRDS("Z:/Christine/NWT Fire Refugia Project Z/BRT Models/Models/Final/Refugia/V9/FRU_25_Final_V9.rds")

# Load stack as SpatRaster
EM_Stack <- stack("Z:/Christine/NWT Fire Refugia Project Z/Prediction/V6/Stacks/2023/FRU_25_FUELS_2023_Stack_Not_Capped_V6.tif")
gc()
names(EM_Stack)

# Predict
EM_Prediction <- raster::predict(EM_Stack,Model, n.trees = Model$gbm.call$n.trees, type = "response")
gc()


EM_Prediction
plot(EM_Prediction)

#Check the proj etc of the prediction, then run the following if it doesn't match the stack's
crs(EM_Prediction) <- crs(EM_Stack)
crs(EM_Prediction)
compareCRS(EM_Stack, EM_Prediction)

writeRaster(EM_Prediction, filename = "Z:/Christine/NWT Fire Refugia Project Z/Prediction/V6/Predictions/2023/FRU_25_FUELS_2023_Not_Capped_V6.tif",
            overwrite = TRUE)