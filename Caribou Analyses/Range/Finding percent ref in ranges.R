library(terra)

# Load data
Preds <- rast("Z:/Christine/NWT Fire Refugia Project Z/Prediction/V6/Predictions/Mosaics/RSF/Masked_Fuels_RSF_Median_Mosaic_V6.tif")
Range <- vect("Z:/Christine/NWT Fire Refugia Project Z/Data/Original Data/Caribou Ranges/From Zihaohan/BorealCaribou RangePlanRegions.shp")

Range_proj <- project(Range, crs(Preds))

# thresholds
low_thr  <- 0.1552978
high_thr <- 0.5

# -----------------------------
# choose ONE management unit
# -----------------------------
poly <- Range_proj[Range_proj$OBJECTID == 6]
plot(poly)

# -----------------------------
# STEP 1: crop + mask (still raster, but minimal)
# -----------------------------
r_clip <- mask(crop(Preds, poly), poly)
plot(r_clip)

# -----------------------------
# STEP 2: convert ONLY clipped area to dataframe
# (this is the key improvement)
# -----------------------------
df <- as.data.frame(r_clip, xy = TRUE, na.rm = FALSE)
names(df)[3] <- "refugia_raw"

# -----------------------------
# STEP 4: classification logic
# -----------------------------
df$refugia_class <- with(df,
                         ifelse(refugia_raw < low_thr, "Low",
                                ifelse(refugia_raw <= high_thr, "Moderate", "High"))
)

# -----------------------------
# STEP 5: % refugia
# -----------------------------
df <- na.omit(df)
pct_refugia <- data.frame(
  Class = names(prop.table(table(df$refugia_class))),
  Percent = as.numeric(prop.table(table(df$refugia_class)) * 100)
)

pct_refugia
