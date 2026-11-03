library(terra)


############Climate Normals:
Normals_Files <- list.files(
  "Z:/Christine/NWT Fire Refugia Project Z/Prediction/V6/Processed GEE Layers/Climate Normals V6",
  pattern = "\\.tif$",
  full.names = TRUE
)

Normal_Stack <- rast(Normals_Files)
gc()


############Climate Anoms:
Anoms_Files <- list.files(
  "Z:/Christine/NWT Fire Refugia Project Z/Prediction/V6/Processed GEE Layers/Climate Anoms V6",
  pattern = "2015.*\\.tif$",
  full.names = TRUE
)

Anoms_Stack <- rast(Anoms_Files)
gc()
names(Anoms_Stack)


############Pontone:
Pontone_Files <- list.files(
  "Z:/Christine/NWT Fire Refugia Project Z/Prediction/V6/Processed GEE Layers/Pontone V6",
  pattern = "\\.tif$",
  full.names = TRUE
)

Pontone_Stack <- rast(Pontone_Files)
old_names <- names(Pontone_Stack)
keep <- old_names %in% c("Pont_Urban_300m", "Pont_Urban_1200m")
old_names[!keep & grepl("^Pont", old_names)] <-
  gsub("^Pont", "Pontone", old_names[!keep & grepl("^Pont", old_names)])
names(Pontone_Stack) <- old_names


############Soils:
Soils_Files <- list.files(
  "Z:/Christine/NWT Fire Refugia Project Z/Prediction/V6/Processed GEE Layers/Soils V6",
  pattern = "\\.tif$",
  full.names = TRUE
)

Soils_Stack <- rast(Soils_Files)
names(Soils_Stack)[names(Soils_Stack) == "b1"] <- "Permafrost"
gc()


############Terrain
Terrain_Files <- list.files(
  "Z:/Christine/NWT Fire Refugia Project Z/Prediction/V6/Processed GEE Layers/Terrain V6",
  pattern = "\\.tif$",
  full.names = TRUE
)

Terrain_Stack <- rast(Terrain_Files)
gc()

############Open areas and extras:
Open_Areas_Extras_Files <- list.files(
  "Z:/Christine/NWT Fire Refugia Project Z/Prediction/V6/Processed GEE Layers/Open and Extra V6/Most current layers available",
  pattern = "\\.tif$",
  full.names = TRUE
)
Open_Areas_Extras_Stack <- rast(Open_Areas_Extras_Files)
names(Open_Areas_Extras_Stack) <- gsub(
  "Shrub_High",
  "Shrub_Tall",
  names(Open_Areas_Extras_Stack))
gc()
names(Open_Areas_Extras_Stack)
Open_Areas_Extras_Stack <- Open_Areas_Extras_Stack[[ 
  !names(Open_Areas_Extras_Stack) %in% c("SCANFI_Herbs_1200m", "SCANFI_Herbs_300m", "SCANFI_Lichen_1200m", "SCANFI_Lichen_300m", "SCANFI_Rock_1200m", "SCANFI_Rock_300m", "SCANFI_Shrub_Tall_1200m",  
                                         "SCANFI_Shrub_Tall_300m", "SCANFI_Shrub_Low_1200m", "SCANFI_Shrub_Low_300m", "SCANFI_Soil_1200m", "SCANFI_Soil_300m")]]



############DOB
DOB <- rast("Z:/Christine/NWT Fire Refugia Project Z/Prediction/V6/Processed GEE Layers/DOB V6/DOB.tif")
names(DOB) <- "DOB"


############Create the big stack that can be applied to all FRUs
Full_Stack <- c(Normal_Stack, Anoms_Stack, Pontone_Stack, Soils_Stack, Terrain_Stack, Open_Areas_Extras_Stack, DOB)
gc()


##################################################################################
############Check that all the names in the stack match vars in models############
##################################################################################
# All layer names in your master stack
stack_names <- names(Full_Stack)

#The variable names used in your models
model_vars <- c("Anom_CMI", "Anom_MAT", "Anom_MFST", "Anom_VPD",
                "Normal_CMI", "Normal_MAT", "Normal_MFST", "Normal_VPD",
                "DOB", "bui", "isi", "rh", "vpd",
                "CTI", "Clay", "Coarse", "Eastness", "Elevation", "Lake_Dir", "Lake_Dist", "Northness", "PCurve", "Permafrost",
                "Pont_Urban_1200m", "Pont_Urban_300m", "Pontone_Agri_1200m", "Pontone_Agri_300m", "Pontone_Bogs_1200m", "Pontone_Bogs_300m",
                "Pontone_Min_Wetlands_1200m", "Pontone_Min_Wetlands_300m", "Pontone_Peat_Perma_1200m", "Pontone_Peat_Perma_300m",
                "Pontone_Poor_Fen_1200m", "Pontone_Poor_Fen_300m", "Pontone_Rich_Fen_1200m", "Pontone_Rich_Fen_300m",
                "Pontone_Uplands_1200m", "Pontone_Uplands_300m",       
                "Sand", "Silt", "Slope", "Soil_Carbon_Histel", "Soil_Carbon_Histo", "TCurve","TPI_1200m", "TPI_300m",  "TRI",                        
                "Water_Oc_1200m", "Water_Oc_300m", "Water_Occurrence",
                "Cutblock_0_10_1200m", "Cutblock_0_10_300m", "Cutblock_10_20_1200m", "Cutblock_10_20_300m",
                "Fire_10_20_1200m", "Fire_10_20_300m", "Fire_1_10_1200m", "Fire_1_10_300m", "Fire_20_30_1200m", "Fire_20_30_300m",
                "Insect_0_10_1200m", "Insect_0_10_300m", "Insect_10_20_1200m", "Insect_10_20_300m",
                "SCANFI_Biomass_1200m", "SCANFI_Biomass_300m", "SCANFI_Closure_1200m", "SCANFI_Closure_300m", "SCANFI_Decid_1200m",        
                "SCANFI_Decid_300m", "SCANFI_Height_1200m", "SCANFI_Height_300m", "SCANFI_Other_Con_1200m", "SCANFI_Other_Con_300m",
                "SCANFI_Pine_1200m", "SCANFI_Pine_300m", "Herm_Barren_1200m", "Herm_Barren_300m", "Herm_Rock_1200m", "Herm_Rock_300m",
                "Herm_Snow_Ice_1200m", "Herm_Snow_Ice_300m", "Herm_Treed_Wetland_1200m", "Herm_Treed_Wetland_300m", "Herm_Wetland_1200m",        
                "Herm_Wetland_300m", "NALCMS_Crop_1200m", "NALCMS_Crop_300m", "NALCMS_Urban_1200m", "NALCMS_Urban_300m",
                "SCANFI_Herbs_1200m", "SCANFI_Herbs_300m", "SCANFI_Lichen_1200m", "SCANFI_Lichen_300m", "SCANFI_Rock_1200m",
                "SCANFI_Rock_300m", "SCANFI_Shrub_Low_1200m", "SCANFI_Shrub_Low_300m", "SCANFI_Shrub_Tall_1200m", "SCANFI_Shrub_Tall_300m",
                "SCANFI_Soil_1200m", "SCANFI_Soil_300m")

#Variables you know are missing and want to ignore
ignore_missing <- c("vpd", "rh", "bui", "isi",
                    "Cutblock_0_10_1200m", "Cutblock_0_10_300m", "Cutblock_10_20_1200m", "Cutblock_10_20_300m", 
                    "Fire_10_20_1200m", "Fire_10_20_300m", "Fire_1_10_1200m",  "Fire_1_10_300m", "Fire_20_30_1200m", "Fire_20_30_300m", 
                    "Insect_0_10_1200m", "Insect_0_10_300m", "Insect_10_20_1200m", "Insect_10_20_300m",
                    "SCANFI_Biomass_1200m", "SCANFI_Biomass_300m", "SCANFI_Closure_1200m", "SCANFI_Closure_300m", 
                    "SCANFI_Decid_1200m", "SCANFI_Decid_300m", "SCANFI_Height_1200m", "SCANFI_Height_300m",     
                    "SCANFI_Other_Con_1200m", "SCANFI_Other_Con_300m", "SCANFI_Pine_1200m", "SCANFI_Pine_300m",        
                    "SCANFI_Herbs_1200m", "SCANFI_Herbs_300m", "SCANFI_Lichen_1200m","SCANFI_Lichen_300m", "SCANFI_Rock_1200m", "SCANFI_Rock_300m",
                    "SCANFI_Shrub_Tall_1200m", "SCANFI_Shrub_Tall_300m", "SCANFI_Shrub_Low_1200m", "SCANFI_Shrub_Low_300m",
                    "SCANFI_Soil_1200m", "SCANFI_Soil_300m")

# Check which model variables are present in your stack
vars_in_stack <- model_vars[model_vars %in% stack_names]

# Check which model variables are missing, ignoring the ones you expect not to be there
vars_missing <- setdiff(model_vars, c(stack_names, ignore_missing))

# Print results
if(length(vars_missing) == 0) {
  cat("No missing variables (excluding the ignored ones).\n")
} else {
  cat("Missing variables (excluding the ignored ones):\n")
  cat(paste(vars_missing, collapse = "\n"), "\n")
} #Flags TRI, but that one was removed in every model so it doesn't matter
gc()



##################################################################################
####################################Create stacks#################################
##################################################################################
# -----------------------------
# Paths
# -----------------------------
fru_path <- "Z:/Christine/NWT Fire Refugia Project Z/Prediction/V6/FRUs/Individual"
fire_weather_base <- "Z:/Christine/NWT Fire Refugia Project Z/Prediction/V6/Processed GEE Layers/Fire Weather V6/Hot/Mountains"
fuels_path <- "Z:/Christine/NWT Fire Refugia Project Z/Prediction/V6/Processed GEE Layers/Layers for Topo Preds V6"
export_path <- "Z:/Christine/NWT Fire Refugia Project Z/Prediction/V6/Stacks/Pers/Hot/Mountains"


# -----------------------------
# Helper functions
# -----------------------------
# Load all fire weather layers for all FRUs
load_all_fire_weather <- function(fru_list, base_path, type = "median", month = "Hot_Mountains") {
  
  vars <- c("bui", "isi", "rh", "vpd")
  all_rasters <- list()
  
  for (fru_short in fru_list) {
    
    # Convert FRU_24_26 -> FRU 24 26
    fru_folder <- file.path(base_path, gsub("_", " ", fru_short))
    
    rasters <- lapply(vars, function(v) {
      
      fname <- file.path(
        fru_folder,
        paste0(v, "_", type, "_", month, ".tif")
      )
      
      if (!file.exists(fname)) {
        stop(paste("Missing file:", fname))
      }
      
      r <- rast(fname)
      names(r) <- v
      return(r)
    })
    
    all_rasters[[fru_short]] <- do.call(c, rasters)
  }
  
  return(all_rasters)
}


# Load all fuel and disturbance layers for all FRUs
load_all_fuels <- function(fru_list, base_path) {
  
  vars <- c("Cutblock_0_10_1200m", "Cutblock_0_10_300m", "Cutblock_10_20_1200m", "Cutblock_10_20_300m", 
            "Fire_10_20_1200m", "Fire_10_20_300m", "Fire_1_10_1200m",  "Fire_1_10_300m", "Fire_20_30_1200m", "Fire_20_30_300m", 
            "Insect_0_10_1200m", "Insect_0_10_300m", "Insect_10_20_1200m", "Insect_10_20_300m",           
            "SCANFI_Biomass_1200m", "SCANFI_Biomass_300m", "SCANFI_Closure_1200m", "SCANFI_Closure_300m", 
            "SCANFI_Decid_1200m", "SCANFI_Decid_300m", "SCANFI_Height_1200m", "SCANFI_Height_300m",     
            "SCANFI_Other_Con_1200m", "SCANFI_Other_Con_300m", "SCANFI_Pine_1200m", "SCANFI_Pine_300m",        
            "SCANFI_Herbs_1200m", "SCANFI_Herbs_300m", "SCANFI_Lichen_1200m","SCANFI_Lichen_300m", "SCANFI_Rock_1200m", "SCANFI_Rock_300m",
            "SCANFI_Shrub_Tall_1200m", "SCANFI_Shrub_Tall_300m", "SCANFI_Shrub_Low_1200m", "SCANFI_Shrub_Low_300m",
            "SCANFI_Soil_1200m", "SCANFI_Soil_300m")
  
  all_rasters <- list()
  
  for(fru_short in fru_list) {
    
    rasters <- lapply(vars, function(v) {
      
      pattern <- paste0("^", fru_short, "_", v, "\\.tif$")
      fname <- list.files(base_path, pattern = pattern, full.names = TRUE)
      
      if(length(fname) == 0) {
        stop(paste("No file found for", fru_short, v))
      }
      
      if(length(fname) > 1) {
        warning(paste("Multiple files found for", fru_short, v, "- using first match"))
      }
      
      r <- rast(fname[1])
      names(r) <- v
      return(r)
    })
    
    all_rasters[[fru_short]] <- do.call(c, rasters)
  }
  
  return(all_rasters)
}


# -----------------------------
# FRU list
# -----------------------------
# -----------------------------
fru_list <- c("FRU_24_26", "FRU_25", "FRU_26", "FRU_31",
              "FRU_32", "FRU_33", "FRU_34", "FRU_35",
              "FRU_37_38_42", "FRU_40", "FRU_41",
              "FRU_43", "FRU_45", "FRU_46")

# Only process FRUs 8–14
fru_list <- fru_list[1:7]

# Find FRU mask files
FRU_files <- list.files(fru_path, pattern = "\\.tif$", full.names = TRUE)

# Keep only the FRU files that are in fru_list
FRU_files <- FRU_files[
  tools::file_path_sans_ext(basename(FRU_files)) %in% fru_list
]

#To just do a single one: 
# fru_list <- c("FRU_24_26")
# FRU_files <- list.files(fru_path, pattern = "\\.tif$", full.names = TRUE)
# FRU_files <- FRU_files[grepl("FRU_24_26", basename(FRU_files))]


# -----------------------------
# Load all FRU-specific rasters first
# -----------------------------
fire_weather_rasters <- load_all_fire_weather(fru_list, fire_weather_base, type = "median")
fire_weather_rasters
fuel_rasters <- load_all_fuels(fru_list, fuels_path)
fuel_rasters
gc()



# -----------------------------
# Loop through FRUs
# -----------------------------
for (m in seq_along(FRU_files)) {
  FRU_Raster <- terra::rast(FRU_files[m])
  
  fru_key <- tools::file_path_sans_ext(basename(FRU_files[m]))
  
  #Load fire weather layers (already a SpatRaster stack)
  fire_layers_stack <- fire_weather_rasters[[fru_key]]
  
  #Load climate anomaly layers (already a SpatRaster stack)
  fuel_layers_stack <- fuel_rasters[[fru_key]]
  
  #Combine everything with Full_Stack (ensure Full_Stack is a SpatRaster)
  FRU_Stack <- c(Full_Stack, fire_layers_stack, fuel_layers_stack)
  gc()
  
  #Mask using FRU raster
  Stack_Masked <- mask(FRU_Stack, FRU_Raster)
  gc()
  
  #Export
  out_file <- file.path(export_path, paste0(fru_key, "_PERS_Hot_Mountains_Stack_V6.tif"))
  writeRaster(Stack_Masked, filename = out_file, overwrite = TRUE)
  gc()
  
  #Clean up memory
  rm(FRU_Raster, fire_layers_stack, FRU_Stack, Stack_Masked)
  gc()
  
  #Progress message
  cat("Processed FRU:", fru_key, "\n")
}
