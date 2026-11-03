library(terra)

#Read in the data being worked on + the template raster
LC <- rast("W:/EDM/CCVegMod4/Christine Fire Refugia Projects/AB Fire Refugia Project/Data/Original Data/Canada Landcover/landcover-2020-classification.tif")
template <- rast("W:/EDM/CCVegMod4/Christine Fire Refugia Projects/NWT Fire Refugia Project/Data/Final Data/Study Area/NWT_Base_Lam_30m.tif")

#Crop the national map to match the template (don't need to project, as the landcover layer is the projection we're using throughout)
LC_crop <- crop(LC, template)

#Mask the projected version to match the template
LC_mask <- mask(LC_crop, template)

#Reclassify the prepped version so that landcover classes we want to mask out are 1 and all the ones to keep are 0
#This keeps the numbering the same as how we made the harvesting mask layers
LC_bin <- ifel(LC_mask %in% c(13, 15,16,17,18,19), 1, 0)

#To check and make sure the final output layers perfectly match the template, use this code:
compareGeom(template, LC_bin, stopOnError = FALSE)
#If you get a value of TRUE, proj, ext, res, rows/columns, and origin/alignment all match perfectly.
#If you get FALSE, one of these parameters are off. 

#If that all looks good, export and check it out in Arc
writeRaster(LC_bin, filename = "W:/EDM/CCVegMod4/Christine Fire Refugia Projects/NWT Fire Refugia Project/Data/Final Data/Sampling Mask/NALCMS_2020_Sampling_Mask.tif")
