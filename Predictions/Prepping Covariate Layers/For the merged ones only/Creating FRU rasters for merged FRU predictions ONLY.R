library(terra)

#For 37/38/42:
FRU <- rast("W:/EDM/CCVegMod4/Christine Fire Refugia Projects/NWT Fire Refugia Project/Prediction/V6/FRUs/Full NWT FRUs 120m.tif")

Out <- ifel(FRU == 42, FRU, NA)
plot(Out)

writeRaster(Out, filename = "Z:/Christine/NWT Fire Refugia Project Z/Prediction/V6/FRUs/Individual/Singles before grouping/FRU_42.tif")


#For 24/26
FRU <- rast("W:/EDM/CCVegMod4/Christine Fire Refugia Projects/NWT Fire Refugia Project/Prediction/V6/FRUs/Full NWT FRUs 120m.tif")
EZ <- vect("W:/EDM/CCVegMod4/Christine Fire Refugia Projects/NWT Fire Refugia Project/Data/Original Data/Ecozones/ecozones.shp")

#Project and rasterize ecozone
Ecozone <- project(EZ, FRU)
Ecozone_120m <- rasterize(Ecozone, FRU, field = "ECOZONE")
compareGeom(FRU, Ecozone_120m)


fru_groups <- list(
  FRU_24 = 24
)

fru_rules <- list(
  FRU_26_shield = function(fru, eco) {
    (fru == 26 & eco %in% c(5, 6))
  })


out_dir <- "W:/EDM/CCVegMod4/Christine Fire Refugia Projects/NWT Fire Refugia Project/Prediction/V6/FRUs/Individual"

for (grp_name in names(fru_groups)) {
  
  vals <- fru_groups[[grp_name]]
  
  fru_mask <- ifel(FRU %in% vals, 1, NA)
  
  writeRaster(
    fru_mask,
    filename = file.path(out_dir, paste0(grp_name, ".tif")),
    overwrite = TRUE
  )
}
gc()

for (name in names(fru_rules)) {
  
  condition <- fru_rules[[name]](FRU, Ecozone_120m)
  
  fru_mask <- ifel(condition, 1, NA)
  
  writeRaster(
    fru_mask,
    filename = file.path(out_dir, paste0(name, ".tif")),
    overwrite = TRUE
  )
}
gc()


#Do they look like they're supposed to?
FRU_24 <- rast("W:/EDM/CCVegMod4/Christine Fire Refugia Projects/NWT Fire Refugia Project/Prediction/V6/FRUs/Individual/FRU_26_shield.tif")
plot(FRU_24)
