library(terra)
library(data.table)

#Load sample
df <- fread("W:/EDM/CCVegMod4/Christine Fire Refugia Projects/NWT Fire Refugia Project/Dataset CSVs/(Step 7) Final/V6/Training_Sample_V6.csv")
df <- as.data.frame(df)

#Load NBAC and an ROI
NBAC <- vect("W:/EDM/CCVegMod4/Christine Fire Refugia Projects/AB Fire Refugia Project/Data/Original Data/Fires/Polygons/NBAC/NBAC_1972to2024_20250506.shp")
ROI <- vect("W:/EDM/CCVegMod4/Christine Fire Refugia Projects/NWT Fire Refugia Project/Data/Final Data/Study Area/NWT_Base_Lam.shp")

NBAC_proj <- project(NBAC, ROI)
NBAC_proj <- crop(NBAC_proj, ROI)
crs(NBAC_proj) == crs(ROI)
gc()


df$Reburns <- 0  #Make a new column to record any identified reburns

year <- sort(unique(df$YEAR))
#year <- 2015 #For checking one at a time
for (year in year) {
  
  cat("Processing year:", year, "\n")
  
  rows <- which(df$YEAR == year)
  if (length(rows) == 0) next
  
  df_sub <- df[rows, ]
  
  # Convert to spatial points
  pts <- vect(df_sub, geom = c("Lam_x", "Lam_y"), crs = crs(ROI))
  
  # Subset polygons for 1–20 years prior
  polys <- NBAC_proj[
    NBAC_proj$YEAR >= (year - 20) &
      NBAC_proj$YEAR <= (year - 1)
  ]
  
  # Skip safely if no polygons
  if (length(polys) == 0) {
    df$Reburns[rows] <- 0
    next
  }
  
  # --- FIXED SPATIAL LOGIC ---
  mat <- terra::relate(pts, polys, relation = "intersects")
  
  # Collapse to per-point result (THIS is the key fix)
  out <- rowSums(mat, na.rm = TRUE) > 0
  
  # Safety check (prevents silent recycling)
  if (length(out) != length(rows)) {
    stop("Mismatch: spatial result not equal to input points")
  }
  
  df$Reburns[rows] <- as.integer(out)
}
