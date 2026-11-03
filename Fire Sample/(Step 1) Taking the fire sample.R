library(terra)
library(dplyr)

# -------------------------
# Paths
# -------------------------
base_fire_rasters <- "W:/EDM/CCVegMod4/Christine Fire Refugia Projects/NWT Fire Refugia Project/Data/Final Data/Fires/Rasters"
Mask        <- rast("W:/EDM/CCVegMod4/Christine Fire Refugia Projects/NWT Fire Refugia Project/Data/Final Data/Sampling Mask/NALCMS_2020_Sampling_Mask.tif")
out_dir           <- "W:/EDM/CCVegMod4/Christine Fire Refugia Projects/NWT Fire Refugia Project/Dataset CSVs/(Step 1) Initial R Sample/V6"

# -------------------------
# Read data
# -------------------------
NBAC <- vect(
  "W:/EDM/CCVegMod4/Christine Fire Refugia Projects/NWT Fire Refugia Project/Data/Final Data/Fires/Polygons/Final_NBAC.shp")

fire.dat <- as.data.frame(NBAC) |>
  select(
    GID, agg_ADJ_HA, YEAR,
    HS_SDATE, HS_EDATE,
    AG_SDATE, AG_EDATE,
    FIRECAUS, NATPARK,
    PRESCRIBED, month_num
  )

# -------------------------
# Containers for problem logs
# -------------------------
problems <- list()  # fires that fail
severity_problems_log <- data.frame()  # fires with 10 / NoData

# -------------------------
# Group fires by year for automatic progress tracking
# -------------------------
years_present <- sort(unique(fire.dat$YEAR))

for (yr in years_present) {
  
  message("=== Processing year ", yr, " ===")
  
  fires_in_year <- fire.dat |> filter(YEAR == yr)
  n_fires <- nrow(fires_in_year)
  
  # Reset results_list per year (start fresh each year)
  results_list <- list()
  
  for (i in seq_len(n_fires)) {
    
    Fire_Poly <- fires_in_year[i, ]
    fire_id   <- Fire_Poly$GID
    
    # Set seed here, per fire (more robust than putting it at start of code, as dropped fires will break the seed)
    seed_val <- as.integer(gsub("_", "", fire_id))
    
    message("Processing fire ", i, " of ", n_fires, " (GID ", fire_id, ")")
    
    fire_raster_path <- file.path(
      base_fire_rasters,
      paste0("Final_BSC_", yr, "_NWT_Base_Lam_v20250527.tif")
    )
    
    
    # Safety check
    if (!file.exists(fire_raster_path)) {
      message("  Missing raster. Skipping fire ", fire_id)
      problems[[length(problems) + 1]] <- Fire_Poly
      next
    }
    
    Fire_Raster <- rast(fire_raster_path)
    Current_Polygon <- NBAC[NBAC$GID == fire_id, ]
    
    # Crop & mask
    Sampled_Fire <- Fire_Raster |>
      crop(Current_Polygon) |>
      mask(Current_Polygon)
    
    # Rasterize polygon
    NBAC_Rast <- rasterize(Current_Polygon, Sampled_Fire, field = "YEAR")
    #terra::compareGeom(NBAC_Rast, Sampled_Fire) #TRUE
    
    # Apply mask
    Mask_Crop <- crop(Mask, NBAC_Rast)
    #terra::compareGeom(NBAC_Rast, Mask_Crop) #TRUE
    NBAC_Rast <- mask(NBAC_Rast, Mask_Crop, maskvalue = 1, updatevalue = NA)
    
    # Determine sample size (1% of valid pixels, min 1)
    n_pixels <- sum(!is.na(values(NBAC_Rast)))
    if (n_pixels == 0) {
      message("  Fire ", fire_id, " has no valid pixels. Skipping.")
      problems[[length(problems) + 1]] <- Fire_Poly
      # Clear memory before next fire
      rm(Fire_Raster, Mask, Sampled_Fire, NBAC_Rast); gc()
      next
    }
    n_sample <- max(1, round(n_pixels * 0.01))
    
    # Sample points
    fire.out <- spatSample(
      NBAC_Rast,
      size   = n_sample,
      method = "random",
      na.rm  = TRUE,
      xy     = TRUE,
      as.df  = TRUE
    )
    
    # Extract severity
    fire.out$Severity <- extract(
      Sampled_Fire,
      fire.out[, c("x", "y")],
      ID = FALSE
    )[, 1]
    
    # This step records any fires that have values of 10 or NoData so you can verify them in Arc later
    problem_fires <- fire.out %>%
      filter(Severity %in% c(10, "NoData"))
    
    if(nrow(problem_fires) > 0){
      log_entry <- data.frame(
        GID    = Fire_Poly$GID,
        YEAR   = Fire_Poly$YEAR,
        n_rows = nrow(problem_fires),
        values = paste(unique(problem_fires$Severity), collapse = ", ")
      )
      severity_problems_log <- rbind(severity_problems_log, log_entry)
    }
    
    # Add refugia column and change severity values such that NA and Unchanged are 1,
    # whereas Low, Moderate, and High are all 0
    # It also removes any row in Severity that's either 10 or NoData (both mean the same thing), but retains NAs 
    fire.out <- fire.out %>%
      mutate(
        Refugia = case_when(
          is.na(Severity) | Severity == "Unchanged to very low" ~ 1,
          Severity %in% c("Low", "Moderate", "High")        ~ 0
        )
      ) %>%
      filter(!(Severity %in% c(10, "NoData")))
    
    # Ensure at least one sample per fire
    if (nrow(fire.out) == 0) {
      coords <- which(!is.na(values(Sampled_Fire)), arr.ind = TRUE)[1, ]
      xy <- xyFromCell(Sampled_Fire, cellFromRowCol(Sampled_Fire, coords[1], coords[2]))
      fire.out <- data.frame(
        x = xy[1], y = xy[2],
        Severity = values(Sampled_Fire)[cellFromRowCol(Sampled_Fire, coords[1], coords[2])]
      )
    }
    
    # Attach attributes
    fire.out <- fire.out |>
      mutate(
        GID         = Fire_Poly$GID,
        YEAR        = Fire_Poly$YEAR,
        ADJ_HA      = Fire_Poly$agg_ADJ_HA,
        HS_SDATE    = Fire_Poly$HS_SDATE,
        HS_EDATE    = Fire_Poly$HS_EDATE,
        AG_SDATE    = Fire_Poly$AG_SDATE,
        AG_EDATE    = Fire_Poly$AG_EDATE,
        FIRECAUS    = Fire_Poly$FIRECAUS,
        NATPARK     = Fire_Poly$NATPARK,
        PRESCRIBED  = Fire_Poly$PRESCRIBED,
        month_num   = Fire_Poly$month_num
      )
    
    results_list[[length(results_list) + 1]] <- fire.out
    
    # Clear memory after each fire to save RAM
    rm(Fire_Raster, Mask_Crop, Sampled_Fire, NBAC_Rast, fire.out); gc()
  }
  
  # Write CSV for this year automatically (bind_rows only includes this year's fires)
  if (length(results_list) > 0) {
    year_samples <- bind_rows(results_list)
    out_file <- file.path(out_dir, paste0("Initial_Sample_", yr, "_V6.csv"))
    write.csv(year_samples, out_file, row.names = FALSE)
    message("  Written CSV for year ", yr, " (", nrow(year_samples), " samples)")
  } else {
    message("  No valid samples for year ", yr)
  }
  
  # Clear per-year list to free memory
  rm(results_list); gc()
}

# Write problem fires log automatically. This records any fires that are missing their severity raster (usually little edge pieces that didn't have severity values generated for them)
if (length(problems) > 0) {
  write.csv(bind_rows(problems),
            file.path(out_dir, "Problem_Fires.csv"),
            row.names = FALSE)
  message("Some fires had problems. See 'Problem_Fires.csv'")
}

# Write severity problems log automatically. This records any samples that had 10 or NoData as their severity
if (nrow(severity_problems_log) > 0) {
  write.csv(severity_problems_log,
            file.path(out_dir, "Severity_Problems_Log.csv"),
            row.names = FALSE)
  message("Severity problems saved: Severity_Problems_Log.csv")
}
