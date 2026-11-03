library(terra)

#Compare the sampling and and input layers to the template (random ones are chosen from annuals)
template <- rast("W:/EDM/CCVegMod4/Christine Fire Refugia Projects/NWT Fire Refugia Project/Data/Final Data/Study Area/NWT_Base_Lam_30m.tif")
Mask <- rast("W:/EDM/CCVegMod4/Christine Fire Refugia Projects/NWT Fire Refugia Project/Data/Final Data/Sampling Mask/NALCMS_2020_Sampling_Mask.tif")
Fires <- rast("W:/EDM/CCVegMod4/Christine Fire Refugia Projects/NWT Fire Refugia Project/Data/Final Data/Fires/Rasters/Final_BSC_2008_NWT_Base_Lam_v20250527.tif")
FRU <- rast("W:/EDM/CCVegMod4/Christine Fire Refugia Projects/NWT Fire Refugia Project/Data/Final Data/FRU/NWT_FRU_30m.tif")


compareGeom(template, Mask, Fires, FRU, stopOnError = FALSE) #Shows that all the rasters match

#Can't check raster to vector, so just do it manually
NBAC <- vect("W:/EDM/CCVegMod4/Christine Fire Refugia Projects/NWT Fire Refugia Project/Data/Final Data/Fires/Polygons/Final_NBAC.shp")
crs(template) == crs(NBAC) #True
plot(template, main="Raster + Vector Overlay") #Can also check it out visually with this code
plot(NBAC, add=TRUE, border="red", lwd=2) #Seems ok here

#Base layer polygon
Base <- vect("W:/EDM/CCVegMod4/Christine Fire Refugia Projects/NWT Fire Refugia Project/Data/Final Data/Study Area/NWT_Base_Lam.shp")
crs(template) == crs(Base) #True
ext(NBAC) == ext(Base) #False but see note below
ext(template) == ext(Base) #False but see note below


############ALL OF THE ABOVE LAYERS HAVE BEEN CHECKED AND ARE AS THEY SHOULD BE. CHATGPT WAS ASKED ABOUT THE VECTOR EXTENTS AND THEY ARE ALSO APPROPRIATE AND READY TO USE######
############LAYERS ARE READY TO USE FOR GENERATING INITIAL TRAINING SAMPLE##########