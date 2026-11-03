library(terra)
library(dplyr)
library(data.table)

df <- fread("W:/EDM/CCVegMod4/Christine Fire Refugia Projects/NWT Fire Refugia Project/Dataset CSVs/(Step 7) Final/V6/Training_Sample_V6.csv")
df <- as.data.frame(df)

#Load the range shapefile and project
template <- rast("W:/EDM/CCVegMod4/Christine Fire Refugia Projects/NWT Fire Refugia Project/Data/Final Data/Study Area/NWT_Base_Lam_120m.tif")
range <- vect("W:/EDM/CCVegMod4/Christine Fire Refugia Projects/NWT Fire Refugia Project/Data/Original Data/Caribou Ranges/From Zihaohan/BorealCaribou RangePlanRegions.shp")

range_proj <- project(range, template)

#Turn the sample points into spatial ones
points_vect <- project(vect(df, geom = c("Lam_x", "Lam_y"), crs = crs(template)), crs(range_proj))

#Break off one range at a time
range_polys <- range_proj[range_proj$OBJECTID == 6, ]

# Extract only points inside the polygon
pts_in_poly <- points_vect[range_polys, ]

#Double check to make sure all points are inside the polygon
plot(range_polys)
points(pts_in_poly, pch = 20, col = "red", cex = 1.5)

#Convert to dataframe, find the thresholds
df_in_poly <- as.data.frame(pts_in_poly)
mean(df_in_poly$Refugia)
