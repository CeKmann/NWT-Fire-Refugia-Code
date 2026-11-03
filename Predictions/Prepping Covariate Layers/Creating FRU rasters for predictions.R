library(terra)

# ---------------------------
# LOAD DATA
# ---------------------------

ROI <- rast("W:/EDM/CCVegMod4/Christine Fire Refugia Projects/NWT Fire Refugia Project/Data/Final Data/Study Area/NWT_Base_Lam_120m.tif")

FRU_30m <- rast("W:/EDM/CCVegMod4/Christine Fire Refugia Projects/NWT Fire Refugia Project/Data/Final Data/FRU/NWT_FRU_30m.tif")

# Bring up to 120m
FRU_120m <- resample(FRU_30m, ROI, method = "near")
compareGeom(FRU_120m, ROI)

# Export full FRU raster
writeRaster(
  FRU_120m,
  filename = "W:/EDM/CCVegMod4/Christine Fire Refugia Projects/NWT Fire Refugia Project/Prediction/V6/FRUs/Full NWT FRUs 120m.tif",
  overwrite = TRUE
)

# ---------------------------
# ECOZONE DATA
# ---------------------------

EZ <- vect("W:/EDM/CCVegMod4/Christine Fire Refugia Projects/NWT Fire Refugia Project/Data/Original Data/Ecozones/ecozones.shp")

Ecozone <- project(EZ, ROI)

Ecozone_120m <- rasterize(Ecozone, ROI, field = "ECOZONE")

compareGeom(FRU_120m, Ecozone_120m)

# ---------------------------
# FRU GROUP DEFINITIONS
# ---------------------------

fru_groups <- list(
  FRU_25 = 25,
  FRU_31 = 31,
  FRU_32 = 32,
  FRU_33 = 33,
  FRU_34 = 34,
  FRU_35 = 35,
  FRU_37_38_42 = c(37, 38, 42),
  FRU_40 = 40,
  FRU_41 = 41,
  FRU_43 = 43,
  FRU_45 = 45,
  FRU_46 = 46
)

# ---------------------------
# ECOZONE-DEPENDENT RULES
# ---------------------------

fru_rules <- list(
  FRU_24_26_EZ5_6 = function(fru, eco) {
    (fru == 24) | (fru == 26 & eco %in% c(5, 6))
  },
  
  FRU_26_EZ4_9 = function(fru, eco) {
    (fru == 26 & eco %in% c(4, 9))
  }
)

# ---------------------------
# OUTPUT DIRECTORY
# ---------------------------

out_dir <- "W:/EDM/CCVegMod4/Christine Fire Refugia Projects/NWT Fire Refugia Project/Prediction/V6/FRUs/Individual"

# ---------------------------
# 1. EXPORT GROUPED FRUs (FIXED MAIN EXPORT LOGIC)
# ---------------------------

for (grp_name in names(fru_groups)) {
  
  vals <- fru_groups[[grp_name]]
  
  fru_mask <- ifel(FRU_120m %in% vals, 1, NA)
  
  writeRaster(
    fru_mask,
    filename = file.path(out_dir, paste0(grp_name, ".tif")),
    overwrite = TRUE
  )
}

# ---------------------------
# 2. EXPORT ECOZONE-CONDITIONAL FRUs
# ---------------------------

for (name in names(fru_rules)) {
  
  condition <- fru_rules[[name]](FRU_120m, Ecozone_120m)
  
  fru_mask <- ifel(condition, 1, NA)
  
  writeRaster(
    fru_mask,
    filename = file.path(out_dir, paste0(name, ".tif")),
    overwrite = TRUE
  )
}
