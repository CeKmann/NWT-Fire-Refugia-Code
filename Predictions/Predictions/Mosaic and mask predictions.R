library(terra)


#####Mosaic the predictions together into one
FRUs <- list.files("Z:/Christine/NWT Fire Refugia Project Z/Prediction/V6/Predictions/For CIF Conference/Pers",
                   pattern=".tif$")
FRUs <- FRUs[c(1:14)]
FRUs_Short <- gsub("\\.[^.]*$", "", FRUs)


for(i in 1:length(FRUs_Short)){
  Pred <- rast(paste("Z:/Christine/NWT Fire Refugia Project Z/Prediction/V6/Predictions/For CIF Conference/Pers/", FRUs_Short[i], ".tif", sep=''))
  
  if(i==1){
    Done = Pred
  } else{
    Done = mosaic(Done, Pred)
  }
}
writeRaster(Done, filename = "Z:/Christine/NWT Fire Refugia Project Z/Prediction/V6/Predictions/Mosaics/Pers/Unmasked_Pers_CIF_Mean_Mosaic_V6.tif",
            overwrite=T)
gc()


#####Mask the predictions
Mask <- rast("Z:/Christine/NWT Fire Refugia Project Z/Data/Final Data/Sampling Mask/Pred_Mask_120m.tif")
Masked <- mask(Done, Mask, maskvalue=1, updatevalue=NA)
gc()

writeRaster(Masked, filename = "Z:/Christine/NWT Fire Refugia Project Z/Prediction/V6/Predictions/Mosaics/Pers/Masked_Pers_CIF_Mean_Mosaic_V6.tif",
            overwrite=T)

