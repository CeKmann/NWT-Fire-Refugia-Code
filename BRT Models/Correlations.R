library(dplyr)
library(corrplot)
library(stats)
library(data.table)

df <- fread("W:/EDM/CCVegMod4/Christine Fire Refugia Projects/NWT Fire Refugia Project/Dataset CSVs/(Step 7) Final/V6/Training_Sample_V6.csv")
df <- as.data.frame(df)
df$FRU <- as.factor(df$FRU)
df$ecozone <- as.factor(df$ecozone)

#Singles
# df_subset <- df %>%
#   filter(FRU == 46) %>%
#   droplevels()

#Groups
# df_subset <- df %>%
#   filter(FRU == 37 | FRU == 38 | FRU == 42) %>%
#   droplevels()
# df_subset <- df %>%
#   filter(FRU == 26, ecozone == 4 | ecozone == 9) %>%
#   droplevels()
df_subset <- df %>%
  filter(FRU == 24 | (FRU == 26 & (ecozone == 5 | ecozone == 6))) %>%
  droplevels()


###########################Check correlations for all variables (for ones with Pontone data)
Vars <- df_subset %>%
  dplyr::select("Anom_CMI", "Anom_MAT", "Anom_MFST", "Anom_VPD",
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

# Check for constant variables (ignoring NA)
constant_cols <- names(Vars)[sapply(Vars, function(x) length(unique(x[!is.na(x)])) == 1)]
constant_cols

# Remove constant variables
Vars_nonconst <- Vars %>% 
  select(where(~length(unique(.[!is.na(.)])) > 1))

# Generate all pairwise combinations
corrs_df <- as.data.frame(t(combn(names(Vars_nonconst), 2)))

# Explicit Pearson correlation with NA handling
corrs_df$cor <- apply(
  corrs_df, 1,
  function(x) cor(
    Vars_nonconst[[x[1]]],
    Vars_nonconst[[x[2]]],
    method = "pearson",
    use = "pairwise.complete.obs"
  )
)

threshold1 <- 0.90
highs <- corrs_df[corrs_df$cor >= threshold1, ] 

threshold2 <- -0.90
lows <- corrs_df[corrs_df$cor <= threshold2, ]



