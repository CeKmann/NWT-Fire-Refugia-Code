library(terra)

Lakes <- vect("W:/EDM/CCVegMod4/Christine Fire Refugia Projects/NWT Fire Refugia Project/Data/Original Data/Large Lakes/HydroLAKES_polys_v10_shp/HydroLAKES_polys_v10_shp/HydroLAKES_polys_v10.shp")
ROI <- rast("W:/EDM/CCVegMod4/Christine Fire Refugia Projects/NWT Fire Refugia Project/Data/Final Data/Study Area/NWT_Base_Lam_120m.tif")
#I went with 120m instead of 30m, because otherwise they'd be massive to store and GEE wouldn't accept them 

#Get into proper projection
crs(Lakes) == crs(ROI)
Lakes_proj <- terra::project(Lakes, crs(ROI))

#Change km2 to ha and filter to only retain those >=5000ha 
#Do this after projecting, since the original proj is WGS and this may affect how calculations are done
Lakes_proj$Size_ha <- Lakes_proj$Lake_area * 100
Large <- Lakes_proj[Lakes_proj$Size_ha >= 5000,]
crs(Large) == crs(ROI) #TRUE
min(Large$Size_ha) #5001
gc()
writeVector(Large, filename = "W:/EDM/CCVegMod4/Christine Fire Refugia Projects/NWT Fire Refugia Project/Data/Processing Data/Lakes/Lakes_Filtered.shp")


#Rasterize to match ROI
Lakes_Rast <- rasterize(Large, ROI, field=1, background=NA) #Setting the field to 1 will give a binary raster where lakes =1 and all else is NA
gc()
compareGeom(ROI, Lakes_Rast, stopOnError = FALSE) #Match

#Turn into a distance raster
Lake_Dist <- terra::distance(Lakes_Rast)
gc()
Lakes_log10 <- log10(Lake_Dist + 1)
gc()


#Export
writeRaster(Lakes_log10, filename = "W:/EDM/CCVegMod4/Christine Fire Refugia Projects/NWT Fire Refugia Project/Data/Final Data/Large Lakes/Dist_to_Lakes_120m.tif")




#Now do direction, using the NON-log10 version
# Calculate the aspect (this gives the direction of steepest slope)
aspect <- terrain(Lake_Dist, v = "aspect", unit = "radians", neighbors = 8)
gc()

# Compute east-west direction based on aspect: 0 degrees = north, 90 degrees = east, etc.
east_west_direction <- cos(aspect - pi / 2)  # This gives east-west component (cosine of the aspect)
gc()

writeRaster(east_west_direction, filename = "W:/EDM/CCVegMod4/Christine Fire Refugia Projects/NWT Fire Refugia Project/Data/Final Data/Large Lakes/Dir_to_Lakes_120m.tif")
