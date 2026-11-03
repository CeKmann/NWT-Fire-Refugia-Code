library(terra)
library(dplyr)
library(data.table)

# Input files
df <- fread("W:/EDM/CCVegMod4/Christine Fire Refugia Projects/NWT Fire Refugia Project/Dataset CSVs/(Step 3) Prepping sample for GEE/V6/B_Names_Matched_V6.csv")
Quinn_dir <- "W:/EDM/CCVegMod4/Christine Fire Refugia Projects/AB Fire Refugia Project/Data/Original Data/Fire Weather"

# Output file and drop log
out_file      <- "W:/EDM/CCVegMod4/Christine Fire Refugia Projects/NWT Fire Refugia Project/Dataset CSVs/(Step 4) Prepping sample for GEE/V6/C_Fire_Weather_Extracted_V6.csv"
drop_log_file <- "W:/EDM/CCVegMod4/Christine Fire Refugia Projects/NWT Fire Refugia Project/Dataset CSVs/(Step 4) Prepping sample for GEE/V6/Drop Log.csv"

if (file.exists(drop_log_file)) file.remove(drop_log_file)


#Will overwrite existing output file if there is one
if(file.exists(out_file)) file.remove(out_file)

years <- sort(unique(df$YEAR))

#Columns from Quinn to keep and check for NAs
quinn_vars <- c("fwi","isi","ffmc","dmc","dc","bui","prec","vpd","tmax","ws","rh")

#yr = 2020
for(yr in years) {
  message("Processing year: ", yr)
  
  # Subset sample for the year
  year_df <- df[YEAR == yr]
  Fire_List <- unique(year_df$GID)
  
  # Read Quinn file for that year
  Quinn_file <- file.path(Quinn_dir, paste0("Firegrowth_pts_v1_1_", yr, ".csv"))
  Quinn_df <- fread(Quinn_file) #Chat says this does the same thing as read.csv but is faster for very big files

  
  for(fire in Fire_List) {
    # Subset sample points for this fire
    fire_df <- year_df[GID == fire]
    message("Processing fire: ", fire)
    
    # Convert to SpatVector
    fire_vect <- vect(fire_df, geom = c("longitude", "latitude"), crs = "+proj=longlat +datum=WGS84") |>
      project("EPSG:3979")
    
    # Subset Quinn points for this fire
    Quinn_fire_raw <- Quinn_df[ID == fire]
    
    if (nrow(Quinn_fire_raw) == 0) {
      stop("Fire ", fire, " not found in Quinn data for year ", yr)
    }
    
    
    # Aggregate Quinn points for one fire a time, otherwise it takes way too long to do the whole file
    Quinn_fire <- Quinn_fire_raw %>%
      group_by(ID, lat, lon, year, ecozone) %>%
      summarise(
        across(any_of(quinn_vars), mean, na.rm = TRUE),
        DOB = first(DOB),       # keep DOB for export (double checked with Diana and it's fine to keep this as first while the rest are averaged)
        .groups = "drop"
      ) %>%
      mutate(Quinn_UID = paste0(ID, "_", row_number()))
    
    # Convert Quinn points to SpatVector
    Quinn_vect <- vect(Quinn_fire, geom = c("lon", "lat"), crs = "+proj=longlat +datum=WGS84") |>
      project("EPSG:3979")
    
    # Find nearest neighbors
    nearest_idx <- as.data.frame(nearest(fire_vect, Quinn_vect))
    
    # Subset fire and Quinn points by nearest indices
    fire_matched <- fire_df[nearest_idx$from_id, , drop = FALSE]
    Quinn_matched <- Quinn_fire[nearest_idx$to_id, , drop = FALSE]
    
    # Combine into a single dataframe with distance
    final <- cbind(
      fire_matched,
      Quinn_matched %>% select(all_of(quinn_vars), lat, lon, ecozone, year, DOB, Quinn_UID),
      Dist_to_FW = nearest_idx$distance
    )
    
    # Rename Quinn coordinates
    names(final)[names(final) == "lat"] <- "Quinn_lat"
    names(final)[names(final) == "lon"] <- "Quinn_lon"
    
    # Identify rows with NA in Quinn variables
    na_mask <- !complete.cases(as.data.frame(final[, ..quinn_vars]))
  
    
    # ---- LOG DROPPED POINTS ----
    if (any(na_mask)) {
      
      dropped <- final[na_mask, ]
      
      # Identify which Quinn variables caused the drop
      na_vars <- apply(
        is.na(dropped[, ..quinn_vars]),
        1,
        function(x) paste(quinn_vars[x], collapse = ";")
      )
      
      
      drop_log <- dropped %>%
        transmute(
          YEAR = YEAR,
          GID = GID,
          longitude = longitude,
          latitude = latitude,
          Quinn_UID = Quinn_UID,
          Dist_to_FW = Dist_to_FW,
          Drop_Reason = "NA in Quinn variables",
          NA_Variables = na_vars
        )
      
      fwrite(
        drop_log,
        drop_log_file,
        append = file.exists(drop_log_file)
      )
    }
    
    # ---- KEEP ONLY COMPLETE CASES ----
    final_clean <- final[!na_mask, ]
    
    # If nothing remains, skip fire
    if (nrow(final_clean) == 0) {
      message("    → Dropped fire ", fire, ": all points had NA Quinn variables")
      rm(fire_df, fire_vect, Quinn_fire, Quinn_vect,
         fire_matched, Quinn_matched, final, final_clean, nearest_idx)
      gc()
      next
    }
    
    
    # Write to disk (append mode)
    fwrite(final_clean, out_file, append = file.exists(out_file))
    
    # Clean memory
    rm(fire_df, fire_vect, Quinn_fire, Quinn_vect, fire_matched, Quinn_matched, final, final_clean, nearest_idx)
    gc()
    gc()
  }}
  

message("All years processed. Output written to: ", out_file)

#Note: see R file "Checks for step 4 part 3" to go over checks that were done to ensure aggregate worked properly and that nothing was scrambled in cbind step
#I also checked via ArcPro to make sure that the points identified as nearest was properly recorded as the nearest point and it seems like they were 
#Also looks like all the necessary info from those nearest points carried through properly too