library(terra)
#I have repurposed the AB code for this

template <- rast("W:/EDM/CCVegMod4/Christine Fire Refugia Projects/AB Fire Refugia Project/Data/Original Data/Canada Landcover/landcover-2020-classification.tif")
#I'm going to redo things with this projection. Xavier has suggested this may help with some of the focal GEE exports


##########################PREP AB FRU LAYER##########################
#Select only the FRUs that are in AB. I tried to do this all via coding but it didn't work very well, so instead I just looked at them in Arc and wrote them down
#I've excluded 39 since it's alpine and won't have many fires/extends too far/we did it for BC already
#Also left out 26, 31, 32 since they were already done in the NWT
FRU <- vect("W:/EDM/CCVegMod4/Christine Fire Refugia Projects/NWT Fire Refugia Project/Data/Original Data/FRU/CanSZ_20180228_dissolve.shp")
selected_FRUs <- c(24, 25, 26, 31, 32, 33, 34, 35, 37, 38, 40, 41, 42, 43, 45, 46)

NWT_FRU <- FRU[FRU$GRIDCODE %in% selected_FRUs,]
NWT_FRU_proj <- project(NWT_FRU, crs(template))

#Change to a raster. 
prov_extent <- ext(NWT_FRU_proj)  # provincial extent in template CRS
res_val <- res(template)[1]      # use template resolution (assume square cells)

FRU_rast_template <- rast(
  extent = prov_extent,
  resolution = res_val,
  crs = crs(template)
)
FRU_rast <- rasterize(NWT_FRU_proj, FRU_rast_template, field = "GRIDCODE")

#Align exactly to template origin via resample (otherwise masking steps in later codes will fail)
FRU_rast_aligned <- resample(FRU_rast, template, method = "near")
FRU_Masked <- crop(FRU_rast_aligned, FRU_rast)

#Check the proj, res, and extent of both. 
crs(template) == crs(FRU_Masked) #True, match
origin(template) == origin(FRU_Masked)    # should be (0,0) and match template origin (does)
ext(FRU_rast) == ext(FRU_Masked)       # should match provincial extent (doesn't, but ChatGPT says this is to be expected because of the change in origin?)
res(FRU_Masked)       # should be 30


#Export
writeRaster(FRU_Masked, filename = "W:/EDM/CCVegMod4/Christine Fire Refugia Projects/NWT Fire Refugia Project/Data/Final Data/FRU/NWT_FRU_30m.tif", overwrite=T)


##########################MAKE BASE LAYERS##########################
#Raster (Lam)
Out <- ifel(!is.na(FRU_Masked), 1, NA)

writeRaster(Out, filename = "W:/EDM/CCVegMod4/Christine Fire Refugia Projects/NWT Fire Refugia Project/Data/Final Data/Study Area/NWT_Base_Lam_30m.tif")

Scaled_Up <- terra::aggregate(Out, fact = 4, fun = max, na.rm = TRUE) #ChatGPT say this is the proper way to scale up from 30m if valid data is simply 1 and all else is NA
writeRaster(Scaled_Up, filename = "W:/EDM/CCVegMod4/Christine Fire Refugia Projects/NWT Fire Refugia Project/Data/Final Data/Study Area/NWT_Base_Lam_120m.tif")


#Vector (Lam)
#I ended up needing to merge all these in Arc afterwards since there wasn't a good way to do it here and leaving the FRU boundaries in messed with the NBAC aggregated steps later on
writeVector(NWT_FRU_proj, filename = "W:/EDM/CCVegMod4/Christine Fire Refugia Projects/NWT Fire Refugia Project/Data/Final Data/Study Area/NWT_Base_Lam.shp")


#Vector (WGS)
library(sf) #Doing the WGS version like you did above will give warnings. ChatGPT says this is a better way to handle it.
NWT_FRU_sf <- st_as_sf(NWT_FRU) #I reran this one with the corrected NWT_FRU layer in the proper projection
ROI_wgs_sf <- st_transform(NWT_FRU_sf, 4326)
ROI_WGS <- vect(ROI_wgs_sf)

#Also just merged these in Arc
writeVector(ROI_WGS, filename = "W:/EDM/CCVegMod4/Christine Fire Refugia Projects/NWT Fire Refugia Project/Data/Final Data/Study Area/NWT_Base_WGS.shp", overwrite=T)
