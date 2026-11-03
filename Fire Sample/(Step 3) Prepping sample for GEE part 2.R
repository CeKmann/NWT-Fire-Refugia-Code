library(terra)
library(data.table)

df <- fread("W:/EDM/CCVegMod4/Christine Fire Refugia Projects/NWT Fire Refugia Project/Dataset CSVs/(Step 2) Prepping sample for GEE/A_NWT_FRU_WGS_50k_V1.csv")
      #Are any samples NULL or blank when they should be recorded as NA?
      sapply(df, function(x) sum(x == "NULL", na.rm = TRUE))
      sapply(df, function(x) sum(x == "", na.rm = TRUE))
      #Nope, we're all good
      colSums(is.na(df)) #NAs still present where they're supposed to be, though, and haven't been removed?
      #Correct
#Get rid of that v1 column that's not supposed to be there
df <- df[, -1]


Quinn_dir <- "W:/EDM/CCVegMod4/Christine Fire Refugia Projects/AB Fire Refugia Project/Data/Original Data/Fire Weather"
#Quinn's data was downloaded from here in early Jan 2026: https://osf.io/f48ry/files/osfstorage

years <- sort(unique(df$YEAR))

# Containers
Matched_Fires <- list()
Missing_Fires <- list()

#yr = 2020
for (yr in years) {
  
  message("Processing year: ", yr)
  
  # --- Your data for this year ---
  Sample_year <- df[df$YEAR == yr, ]
  
  if (nrow(Sample_year) == 0) next
  
  Sample_names <- unique(Sample_year$GID)
  
  # --- Quinn file for this year ---
  Quinn_file <- file.path(
    Quinn_dir,
    paste0("Firegrowth_pts_v1_1_", yr, ".csv")
  )
  
  if (!file.exists(Quinn_file)) {
    warning("Missing Quinn file for year ", yr)
    
    # All fires from this year are missing
    Missing_Fires[[as.character(yr)]] <-
      data.frame(
        YEAR = yr,
        GID  = Sample_names,
        reason = "Quinn file missing",
        stringsAsFactors = FALSE
      )
    next
  }
  
  Quinn_df <- fread(Quinn_file)
  
  # --- Fire name column in Quinn's data ---
  if (!"ID" %in% names(Quinn_df)) {
    warning("Column 'ID' missing in ", Quinn_file)
    
    Missing_Fires[[as.character(yr)]] <-
      data.frame(
        YEAR = yr,
        GID  = Sample_names,
        reason = "ID column missing in Quinn file",
        stringsAsFactors = FALSE
      )
    next
  }
  
  Quinn_names <- unique(Quinn_df$ID)
  
  # --- Compare ---
  Common_fires  <- intersect(Sample_names, Quinn_names)
  Missing_names <- setdiff(Sample_names, Quinn_names)
  
  # --- Store matched fires ---
  if (length(Common_fires) > 0) {
    Matched_Fires[[as.character(yr)]] <-
      Sample_year[Sample_year$GID %in% Common_fires, ]
  }
  
  # --- Store missing fires ---
  if (length(Missing_names) > 0) {
    Missing_Fires[[as.character(yr)]] <-
      data.frame(
        YEAR = yr,
        GID  = Missing_names,
        reason = "Not present in Quinn data",
        stringsAsFactors = FALSE
      )
  }
}


# Combine results
#NOTE! If you used fread to read in the data, you should export with fwrite instead of write.csv. write.csv will automatically add an extra column for row numbers, and if this is already included it will be duplicated and cause issues when extracting fire weather
Fires_Done   <- do.call(rbind, Matched_Fires) #1817169 points to start, 1810970 points left after filtering by name
  fwrite(Fires_Done, file = "W:/EDM/CCVegMod4/Christine Fire Refugia Projects/NWT Fire Refugia Project/Dataset CSVs/(Step 3) Prepping sample for GEE/V6/B_Names_Matched_V6.csv")
Fires_Missing <- do.call(rbind, Missing_Fires)
  fwrite(Fires_Missing, file = "W:/EDM/CCVegMod4/Christine Fire Refugia Projects/NWT Fire Refugia Project/Dataset CSVs/(Step 3) Prepping sample for GEE/V6/Names not in Quinns/Fires not in Quinns.csv")

  
orig <- unique(df$GID) #2516 fires before matching to Quinn's
new <- unique(Fires_Done$GID) #2352 fires after matching
#164 fewer fires now, same number as what went into the Fires_Missing object
  
  
#To double check and make sure those missing fires really aren't in his data at all, run the following (takes a while):
  # Make a vector to collect any potential "false missing" fires
  false_missing <- character(0)
  
  for (yr in years) {
    
    # Skip years with no missing fires
    if (!as.character(yr) %in% names(Missing_Fires)) next
    
    missing_year <- Missing_Fires[[as.character(yr)]]
    missing_names <- unique(missing_year$GID)
    
    Quinn_file <- file.path(
      Quinn_dir,
      paste0("Firegrowth_pts_v1_1_", yr, ".csv")
    )
    
    # Skip if file doesn't exist
    if (!file.exists(Quinn_file)) next
    
    Quinn_df <- read.csv(Quinn_file, stringsAsFactors = FALSE)
    
    if (!"ID" %in% names(Quinn_df)) next
    
    Quinn_names <- unique(Quinn_df$ID)
    
    # Check if any "missing" fires are actually in Quinn's data
    found_in_quinn <- intersect(missing_names, Quinn_names)
    
    if (length(found_in_quinn) > 0) {
      false_missing <- c(false_missing, paste0(yr, "_", found_in_quinn))
    }
    
    rm(Quinn_df); gc()
  }
  
  if (length(false_missing) == 0) {
    message("✅ All fires in Missing_Fires are truly absent from Quinn's data.")
  } else {
    warning("⚠ Some 'missing' fires were found in Quinn's data: ", paste(false_missing, collapse=", "))
  }
  