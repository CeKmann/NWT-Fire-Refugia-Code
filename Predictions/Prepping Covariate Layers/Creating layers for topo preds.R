library(terra)
library(dplyr)
library(tibble)
library(data.table)

#Load dataframe
df <- fread("W:/EDM/CCVegMod4/Christine Fire Refugia Projects/NWT Fire Refugia Project/Dataset CSVs/(Step 7) Final/V6/Training_Sample_V6.csv")
df <- as.data.frame(df)

df$FRU <- as.factor(df$FRU)

FRU_45 <- df %>%
  filter(FRU == 45) %>%
  droplevels()

#Isolate each FRU and pull out the variables which need to be averaged (SCANFI, fires, open areas, cutblocks, Hermo non-fuels)
Needed <- FRU_45 %>%
  select(
    #Disturbance
    "Cutblock_0_10_1200m", "Cutblock_0_10_300m", "Cutblock_10_20_1200m", "Cutblock_10_20_300m", 
    "Fire_10_20_1200m", "Fire_10_20_300m", "Fire_1_10_1200m",  "Fire_1_10_300m", "Fire_20_30_1200m", "Fire_20_30_300m", 
    "Insect_0_10_1200m", "Insect_0_10_300m", "Insect_10_20_1200m", "Insect_10_20_300m",       
    #SCANFI/Open areas fuels (these versions of soil, etc, are taken as 'fuels' and averaged). Herm barren, rock, snow/ice are taken as-is like terrain            
    "SCANFI_Biomass_1200m", "SCANFI_Biomass_300m", "SCANFI_Closure_1200m", "SCANFI_Closure_300m", 
    "SCANFI_Decid_1200m", "SCANFI_Decid_300m", "SCANFI_Height_1200m", "SCANFI_Height_300m",     
    "SCANFI_Other_Con_1200m", "SCANFI_Other_Con_300m", "SCANFI_Pine_1200m", "SCANFI_Pine_300m",        
    "SCANFI_Herbs_1200m", "SCANFI_Herbs_300m", "SCANFI_Lichen_1200m","SCANFI_Lichen_300m", "SCANFI_Rock_1200m", "SCANFI_Rock_300m",
    "SCANFI_Shrub_Tall_1200m", "SCANFI_Shrub_Tall_300m", "SCANFI_Shrub_Low_1200m", "SCANFI_Shrub_Low_300m",
    "SCANFI_Soil_1200m", "SCANFI_Soil_300m")
#Any NAs? (Shouldn't be for these)
if (anyNA(Needed)) {
  print(colSums(is.na(Needed)))
}

#For every column, find the average within the full sample
Means <- colMeans(Needed, na.rm = FALSE) #Already know there are no NAs, but this will catch them if any do happen to occur and aren't noticed earlier
head(Means)
tail(Means)


#Load base layer for predictions and turn all values to 0
ROI <- rast("W:/EDM/CCVegMod4/Christine Fire Refugia Projects/NWT Fire Refugia Project/Data/Final Data/Study Area/NWT_Base_Lam_120m.tif")
Blank <- ROI*0

#Export
for (var in names(Means)) {
  Out <- Blank + Means[var]
  
  writeRaster(
    Out,
    filename = paste0("W:/EDM/CCVegMod4/Christine Fire Refugia Projects/NWT Fire Refugia Project/Prediction/V6/Processed GEE Layers/Layers for Topo Preds V6/FRU_45_", var, ".tif"),
    overwrite = TRUE
  )
}
