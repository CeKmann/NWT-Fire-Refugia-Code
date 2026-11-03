library(ggplot2)
library(gbm)
library(dismo)
library(pdp)
library(dplyr)
library(data.table)
library(ggpubr)
library(grid)
library(gridExtra)
library(data.table)
library(vip)

##Set up a seperate dataframe that contains only the full list of names. Will be used later to populate any missing variables with a value of 0 so final totals all sum to 0
Name_List <- tibble(
  Variable = c("Anom_CMI", "Anom_MAT", "Anom_MFST", "Anom_VPD",
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
               "SCANFI_Soil_1200m", "SCANFI_Soil_300m"),
  Full_Name = c("Climate moisture index anomalies", "Mean annual temperature anomalies", "Mean fire season temperature anomalies", "Vapor pressure deficit anomalies",
                "Climate moisture index normals", "Mean annual temperature normals", "Mean fire season temperature normals", "Vapor pressure deficit normals",
                "Day of burn", "Mean daily buildup index", "Mean daily initial spread index", "Mean daily relative humidity", "Mean daily vapor pressure deficit",
                "Compound topographic index", "Clay", "Coarse", "Eastness", "Elevation", "Direction to large lakes", "Distance to large lakes (log10)", "Northness", "Profile curvature", "Permafrost",
                "Proportion urban (Pontone) (1200 m)", "Proportion urban (Pontone) (300 m)", "Proportion agriculture (Pontone) (1200 m)", "Proportion agriculture (Pontone) (300 m)", "Proportion bog (1200 m)", "Proportion bog (300 m)",
                "Proportion min. wetlands (1200 m)", "Proportion min. wetlands (300 m)", "Proportion peat/permafrost complex (1200 m)", "Proportion peat/permafrost complex (300 m)",
                "Proportion poor fen (1200 m)", "Proportion poor fen (300 m)", "Proportion rich fen (1200 m)", "Proportion rich fen (300 m)",
                "Proportion uplands (1200 m)", "Proportion uplands (300 m)", 
                "Sand", "Silt", "Slope", "Soil carbon histel", "Soil carbon histo", "Tangential curvature", "Topographic position index (1200 m)", "Topographic position index (300 m)",  "TRI",                        
                "Water occurrence (1200 m)", "Water occurrence (300 m)", "Water occurrence",
                "Proportion cutblock (0-10yrs) (1200 m)", "Proportion cutblock (0-10yrs) (300 m)", "Proportion cutblock (10-20yrs) (1200 m)", "Proportion cutblock (10-20yrs) (300 m)",
                "Past fire (10-20yrs) (1200 m)", "Past fire (10-20yrs) (300 m)", "Past fire (1-10yrs) (1200 m)", "Past fire (1-10yrs) (300 m)", "Past fire (20-30yrs) (1200 m)", "Past fire (20-30yrs) (300 m)",
                "Insect disturbance (0-10yrs) (1200 m)", "Insect disturbance (0-10yrs) (300 m)", "Insect disturbance (10-20yrs) (1200 m)", "Insect disturbance (10-20yrs) (300 m)",
                "Biomass (1200 m)", "Biomass (300 m)", "Canopy closure (1200 m)", "Canopy closure (300 m)", 
                "Proportion deciduous (1200 m)", "Proportion deciduous (300 m)", "Vegetation height (1200 m)", "Vegetation height (300 m)", "Proportion non-pine conifers (1200 m)", "Proportion non-pine conifers (300 m)",
                "Proportion pine (1200 m)", "Proportion pine (300 m)", "Proportion barren (1200 m)","Proportion barren (300 m)", "Proportion rock (Herm) (1200 m)", "Proportion rock (Herm) (300 m)",
                "Proportion snow/ice (1200 m)", "Proportion snow/ice (300 m)", "Proportion treed wetland (1200 m)", "Proportion treed wetland (300 m)", "Proportion open wetland (1200 m)", "Proportion open wetland (300 m)", 
                "Proportion crop (NALCMS) (1200 m)", "Proportion crop (NALCMS) (300 m)","Proportion urban (NALCMS) (1200 m)", "Proportion urban (NALCMS) (300 m)",
                "Proportion herbs (1200 m)", "Proportion herbs (300 m)", "Proportion lichen (1200 m)","Proportion lichen (300 m)", 
                "Proportion rock (SCANFI) (1200 m)", "Proportion rock (SCANFI) (300 m)", "Proportion low shrub (1200 m)", "Proportion low shrub (300 m)", "Proportion tall shrub (1200 m)", "Proportion tall shrub (300 m)",
                "Proportion soil (1200 m)", "Proportion soil (300 m)"))

########################################################################################
#######################################READ IN DF#######################################
########################################################################################

df <- fread("Z:/Christine/NWT Fire Refugia Project Z/Dataset CSVs/(Step 7) Final/V6/Training_Sample_V6.csv")
df$FRU <- as.factor(df$FRU)
df$ecozone <- as.factor(df$ecozone)



#Specify what variables are wanted
# variables <- c("Anom_CMI", "Anom_MAT", "Anom_MFST", "Anom_VPD")
# variables <- c("Pontone_Poor_Fen_300m", "Pontone_Rich_Fen_300m", "Pontone_Bogs_300m","Pontone_Min_Wetlands_300m", "Pontone_Uplands_300m")
# variables <- c("Fire_1_10_300m", "Fire_10_20_300m", "Fire_20_30_300m", "Insect_0_10_300m", "Insect_10_20_300m")
# variables <- c("TPI_300m", "TPI_1200m", "CTI", "Northness", "Elevation")
#  variables <- c("bui", "isi", "rh", "vpd", "DOB")
  variables <- c("SCANFI_Pine_300m", "SCANFI_Other_Con_300m", "SCANFI_Decid_300m", "SCANFI_Height_300m", "SCANFI_Closure_300m")
#  variables <- c("SCANFI_Herbs_300m", "SCANFI_Lichen_300m", "SCANFI_Shrub_Low_300m", "SCANFI_Shrub_Tall_300m", "SCANFI_Biomass_300m")
#  variables <- c("TPI_300m", "TPI_1200m", "CTI", "Northness", "Elevation")


#######################################################################################################################################
##############################################Boreal/Southern Taiga plains#############################################################
#######################################################################################################################################
FRU_31_df <- df %>%
  filter(FRU == 31) %>%
  droplevels()
set.seed(123) #Needs to be called before every subsample 
FRU_31_df <- FRU_31_df[sample(1:nrow(FRU_31_df), 25000,
                              replace=FALSE),]
FRU_31_df$Fire_20_30_300m <- FRU_31_df$Fire_20_30_300m * (0.001 / max(FRU_31_df$Fire_20_30_300m))

FRU_32_df <- df %>%
  filter(FRU == 32) %>%
  droplevels()
set.seed(123) #Needs to be called before every subsample 
FRU_32_df <- FRU_32_df[sample(1:nrow(FRU_32_df), 25000,
                              replace=FALSE),]
FRU_32_df$Fire_10_20_300m <- FRU_32_df$Fire_10_20_300m * (0.001 / max(FRU_32_df$Fire_10_20_300m))
FRU_32_df$Fire_20_30_300m <- FRU_32_df$Fire_20_30_300m * (0.001 / max(FRU_32_df$Fire_20_30_300m))

###################Read in models
FRU_31_model <- readRDS("Z:/Christine/NWT Fire Refugia Project Z/BRT Models/Models/Final/Refugia/V9/FRU_31_Final_V9.rds")
FRU_32_model <- readRDS("Z:/Christine/NWT Fire Refugia Project Z/BRT Models/Models/Final/Refugia/V9/FRU_32_Final_V9.rds")



#######################################################################################################################
##############################################Taiga plains#############################################################
#######################################################################################################################
FRU_26_df <- df %>%
  filter(FRU == 26, ecozone == 4 | ecozone == 9) %>%
  droplevels()
FRU_26_df$Fire_10_20_300m <- FRU_26_df$Fire_10_20_300m * (0.001 / max(FRU_26_df$Fire_10_20_300m))
FRU_26_df$Fire_20_30_300m <- FRU_26_df$Fire_20_30_300m * (0.001 / max(FRU_26_df$Fire_20_30_300m))
FRU_26_df$Northness <- FRU_26_df$Northness * (0.00001 / max(FRU_26_df$Northness))


FRU_33_df <- df %>%
  filter(FRU == 33) %>%
  droplevels()
set.seed(123) #Needs to be called before every subsample 
FRU_33_df <- FRU_33_df[sample(1:nrow(FRU_33_df), 25000,
                              replace=FALSE),]
FRU_33_df$Anom_MFST <- FRU_33_df$Anom_MFST * (0.0001 / max(FRU_33_df$Anom_MFST))
FRU_33_df$Fire_20_30_300m <- FRU_33_df$Fire_20_30_300m * (0.001 / max(FRU_33_df$Fire_20_30_300m))
FRU_33_df$Insect_10_20_300m <- FRU_33_df$Insect_10_20_300m * (0.001 / max(FRU_33_df$Insect_10_20_300m))


FRU_34_df <- df %>%
  filter(FRU == 34) %>%
  droplevels()
set.seed(123) #Needs to be called before every subsample 
FRU_34_df <- FRU_34_df[sample(1:nrow(FRU_34_df), 25000,
                              replace=FALSE),]
FRU_34_df$Insect_10_20_300m <- FRU_34_df$Insect_10_20_300m * (0.001 / max(FRU_34_df$Insect_10_20_300m))

FRU_35_df <- df %>%
  filter(FRU == 35) %>%
  droplevels()
FRU_35_df$Pontone_Bogs_300m <- FRU_35_df$Pontone_Bogs_300m * (0.001 / max(FRU_35_df$Pontone_Bogs_300m))
FRU_35_df$Pontone_Min_Wetlands_300m <- FRU_35_df$Pontone_Min_Wetlands_300m * (0.001 / max(FRU_35_df$Pontone_Min_Wetlands_300m))
FRU_35_df$Fire_20_30_300m <- FRU_35_df$Fire_20_30_300m * (0.001 / max(FRU_35_df$Fire_20_30_300m))
FRU_35_df$Insect_10_20_300m <- FRU_35_df$Insect_10_20_300m * (0.001 / max(FRU_35_df$Insect_10_20_300m))
FRU_35_df$SCANFI_Pine_300m <- FRU_35_df$SCANFI_Pine_300m * (0.001 / max(FRU_35_df$SCANFI_Pine_300m))




###################Read in models
FRU_26_model <- readRDS("Z:/Christine/NWT Fire Refugia Project Z/BRT Models/Models/Final/Refugia/V9/FRU_26_plains_Final_V9.rds")
FRU_33_model <- readRDS("Z:/Christine/NWT Fire Refugia Project Z/BRT Models/Models/Final/Refugia/V9/FRU_33_Final_V9.rds")
FRU_34_model <- readRDS("Z:/Christine/NWT Fire Refugia Project Z/BRT Models/Models/Final/Refugia/V9/FRU_34_Final_V9.rds")
FRU_35_model <- readRDS("Z:/Christine/NWT Fire Refugia Project Z/BRT Models/Models/Final/Refugia/V9/FRU_35_Final_V9.rds")



#######################################################################################################################
##############################################Taiga shield#############################################################
#######################################################################################################################
FRU_24_26_df <- df %>%
  filter(FRU == 24 | (FRU == 26 & (ecozone == 5 | ecozone == 6))) %>%
  droplevels()
set.seed(123) # Makes sure your sample is reproducible 
FRU_24_26_df <- FRU_24_26_df[sample(1:nrow(FRU_24_26_df), 25000,
                                    replace=FALSE),]
FRU_24_26_df$Fire_10_20_300m <- FRU_24_26_df$Fire_10_20_300m * (0.001 / max(FRU_24_26_df$Fire_10_20_300m))
FRU_24_26_df$Fire_20_30_300m <- FRU_24_26_df$Fire_20_30_300m * (0.001 / max(FRU_24_26_df$Fire_20_30_300m))

FRU_25_df <- df %>%
  filter(FRU == 25) %>%
  droplevels()
set.seed(123) #Needs to be called before every subsample 
FRU_25_df <- FRU_25_df[sample(1:nrow(FRU_25_df), 25000,
                              replace=FALSE),]
FRU_25_df$Pontone_Min_Wetlands_300m <- FRU_25_df$Pontone_Min_Wetlands_300m * (0.001 / max(FRU_25_df$Pontone_Min_Wetlands_300m))
FRU_25_df$Fire_20_30_300m <- FRU_25_df$Fire_20_30_300m * (0.001 / max(FRU_25_df$Fire_20_30_300m))
FRU_25_df$SCANFI_Other_Con_300m <- FRU_25_df$SCANFI_Other_Con_300m * (0.001 / max(FRU_25_df$SCANFI_Other_Con_300m))




###################Read in models
FRU_24_26_model <- readRDS("Z:/Christine/NWT Fire Refugia Project Z/BRT Models/Models/Final/Refugia/V9/FRU_24_26_shield_Final_V9.rds")
FRU_25_model <- readRDS("Z:/Christine/NWT Fire Refugia Project Z/BRT Models/Models/Final/Refugia/V9/FRU_25_Final_V9.rds")




#######################################################################################################################
##############################################Boreal cordillera########################################################
#######################################################################################################################
FRU_40_df <- df %>%
  filter(FRU == 40) %>%
  droplevels()
set.seed(123) #Needs to be called before every subsample 
FRU_40_df <- FRU_40_df[sample(1:nrow(FRU_40_df), 25000,
                              replace=FALSE),]
FRU_40_df$Pontone_Poor_Fen_300m <- FRU_40_df$Pontone_Poor_Fen_300m * (0.001 / max(FRU_40_df$Pontone_Poor_Fen_300m))
FRU_40_df$Pontone_Rich_Fen_300m <- FRU_40_df$Pontone_Rich_Fen_300m * (0.001 / max(FRU_40_df$Pontone_Rich_Fen_300m))
FRU_40_df$Pontone_Bogs_300m <- FRU_40_df$Pontone_Bogs_300m * (0.001 / max(FRU_40_df$Pontone_Bogs_300m))
FRU_40_df$Pontone_Min_Wetlands_300m <- FRU_40_df$Pontone_Min_Wetlands_300m * (0.001 / max(FRU_40_df$Pontone_Min_Wetlands_300m))
FRU_40_df$Pontone_Uplands_300m <- FRU_40_df$Pontone_Uplands_300m * (0.001 / max(FRU_40_df$Pontone_Uplands_300m))



FRU_41_df <- df %>%
  filter(FRU == 41) %>%
  droplevels()
set.seed(123) #Needs to be called before every subsample 
FRU_41_df <- FRU_41_df[sample(1:nrow(FRU_41_df), 25000,
                              replace=FALSE),]
FRU_41_df$Pontone_Poor_Fen_300m <- FRU_41_df$Pontone_Poor_Fen_300m * (0.001 / max(FRU_41_df$Pontone_Poor_Fen_300m))
FRU_41_df$Pontone_Rich_Fen_300m <- FRU_41_df$Pontone_Rich_Fen_300m * (0.001 / max(FRU_41_df$Pontone_Rich_Fen_300m))
FRU_41_df$Pontone_Bogs_300m <- FRU_41_df$Pontone_Bogs_300m * (0.001 / max(FRU_41_df$Pontone_Bogs_300m))
FRU_41_df$Pontone_Min_Wetlands_300m <- FRU_41_df$Pontone_Min_Wetlands_300m * (0.001 / max(FRU_41_df$Pontone_Min_Wetlands_300m))
FRU_41_df$Pontone_Uplands_300m <- FRU_41_df$Pontone_Uplands_300m * (0.001 / max(FRU_41_df$Pontone_Uplands_300m))



FRU_43_df <- df %>%
  filter(FRU == 43) %>%
  droplevels()
FRU_43_df$Pontone_Poor_Fen_300m <- FRU_43_df$Pontone_Poor_Fen_300m * (0.001 / max(FRU_43_df$Pontone_Poor_Fen_300m))
FRU_43_df$Pontone_Rich_Fen_300m <- FRU_43_df$Pontone_Rich_Fen_300m * (0.001 / max(FRU_43_df$Pontone_Rich_Fen_300m))
FRU_43_df$Pontone_Bogs_300m <- FRU_43_df$Pontone_Bogs_300m * (0.001 / max(FRU_43_df$Pontone_Bogs_300m))
FRU_43_df$Pontone_Min_Wetlands_300m <- FRU_43_df$Pontone_Min_Wetlands_300m * (0.001 / max(FRU_43_df$Pontone_Min_Wetlands_300m))
FRU_43_df$Pontone_Uplands_300m <- FRU_43_df$Pontone_Uplands_300m * (0.001 / max(FRU_43_df$Pontone_Uplands_300m))
FRU_43_df$Fire_1_10_300m <- FRU_43_df$Fire_1_10_300m * (0.001 / max(FRU_43_df$Fire_1_10_300m))
FRU_43_df$Fire_10_20_300m <- FRU_43_df$Fire_10_20_300m * (0.001 / max(FRU_43_df$Fire_10_20_300m))
FRU_43_df$Fire_20_30_300m <- FRU_43_df$Fire_20_30_300m * (0.001 / max(FRU_43_df$Fire_20_30_300m))
FRU_43_df$Insect_10_20_300m <- FRU_43_df$Insect_10_20_300m * (0.001 / max(FRU_43_df$Insect_10_20_300m))



FRU_45_df <- df %>%
  filter(FRU == 45) %>%
  droplevels()
set.seed(123) #Needs to be called before every subsample 
FRU_45_df <- FRU_45_df[sample(1:nrow(FRU_45_df), 25000,
                              replace=FALSE),]
FRU_45_df$Anom_MFST <- FRU_45_df$Anom_MFST * (0.0001 / max(FRU_45_df$Anom_MFST))
FRU_45_df$Pontone_Poor_Fen_300m <- FRU_45_df$Pontone_Poor_Fen_300m * (0.001 / max(FRU_45_df$Pontone_Poor_Fen_300m))
FRU_45_df$Pontone_Rich_Fen_300m <- FRU_45_df$Pontone_Rich_Fen_300m * (0.001 / max(FRU_45_df$Pontone_Rich_Fen_300m))
FRU_45_df$Pontone_Bogs_300m <- FRU_45_df$Pontone_Bogs_300m * (0.001 / max(FRU_45_df$Pontone_Bogs_300m))
FRU_45_df$Pontone_Min_Wetlands_300m <- FRU_45_df$Pontone_Min_Wetlands_300m * (0.001 / max(FRU_45_df$Pontone_Min_Wetlands_300m))
FRU_45_df$Pontone_Uplands_300m <- FRU_45_df$Pontone_Uplands_300m * (0.001 / max(FRU_45_df$Pontone_Uplands_300m))
FRU_45_df$Fire_10_20_300m <- FRU_45_df$Fire_10_20_300m * (0.001 / max(FRU_45_df$Fire_10_20_300m))



FRU_46_df <- df %>%
  filter(FRU == 46) %>%
  droplevels()
FRU_46_df$Anom_MFST <- FRU_46_df$Anom_MFST * (0.0001 / max(FRU_46_df$Anom_MFST))
FRU_46_df$Pontone_Poor_Fen_300m <- FRU_46_df$Pontone_Poor_Fen_300m * (0.001 / max(FRU_46_df$Pontone_Poor_Fen_300m))
FRU_46_df$Pontone_Rich_Fen_300m <- FRU_46_df$Pontone_Rich_Fen_300m * (0.001 / max(FRU_46_df$Pontone_Rich_Fen_300m))
FRU_46_df$Pontone_Bogs_300m <- FRU_46_df$Pontone_Bogs_300m * (0.001 / max(FRU_46_df$Pontone_Bogs_300m))
FRU_46_df$Pontone_Min_Wetlands_300m <- FRU_46_df$Pontone_Min_Wetlands_300m * (0.001 / max(FRU_46_df$Pontone_Min_Wetlands_300m))
FRU_46_df$Pontone_Uplands_300m <- FRU_46_df$Pontone_Uplands_300m * (0.001 / max(FRU_46_df$Pontone_Uplands_300m))





###################Read in models
FRU_40_model <- readRDS("Z:/Christine/NWT Fire Refugia Project Z/BRT Models/Models/Final/Refugia/V9/FRU_40_Final_V9.rds")
FRU_41_model <- readRDS("Z:/Christine/NWT Fire Refugia Project Z/BRT Models/Models/Final/Refugia/V9/FRU_41_Final_V9.rds")
FRU_43_model <- readRDS("Z:/Christine/NWT Fire Refugia Project Z/BRT Models/Models/Final/Refugia/V9/FRU_43_Final_V9.rds")
FRU_45_model <- readRDS("Z:/Christine/NWT Fire Refugia Project Z/BRT Models/Models/Final/Refugia/V9/FRU_45_Final_V9.rds")
FRU_46_model <- readRDS("Z:/Christine/NWT Fire Refugia Project Z/BRT Models/Models/Final/Refugia/V9/FRU_46_Final_V9.rds")





#######################################################################################################################
##############################################Taiga cordillera#########################################################
#######################################################################################################################
FRU_37_38_42_df <- df %>%
  filter(FRU == 37 | FRU == 38 | FRU == 42) %>%
  droplevels()
set.seed(123) #Needs to be called before every subsample 
FRU_37_38_42_df <- FRU_37_38_42_df[sample(1:nrow(FRU_37_38_42_df), 25000,
                                          replace=FALSE),]
FRU_37_38_42_df$SCANFI_Other_Con_300m <- FRU_37_38_42_df$SCANFI_Other_Con_300m * (0.001 / max(FRU_37_38_42_df$SCANFI_Other_Con_300m))


###################Read in models
FRU_37_38_42_model <- readRDS("Z:/Christine/NWT Fire Refugia Project Z/BRT Models/Models/Final/Refugia/V9/FRU_37_38_42_Final_V9.rds")


#####################################################################################
######################Make a list of all the models and datasets#####################
#####################################################################################

#####################Cordillera FRUs (5):
train_data_cord <- list(FRU_40_df, FRU_41_df, FRU_43_df, FRU_45_df, FRU_46_df, #Boreal cordillera 
                        FRU_37_38_42_df) #Taiga cordillera

models_cord <- list(FRU_40_model, FRU_41_model, FRU_43_model, FRU_45_model, FRU_46_model, #Boreal cordillera 
                    FRU_37_38_42_model) #Taiga cordillera
gc()  

# Assign names to your models for legend purposes
names(models_cord) <- c("B.Cord(40)", "B.Cord(41)", "B.Cord(43)", "B.Cord(45)", "B.Cord(46)", #Boreal cordillera
                        "T.Cord(37/38/42)") #Taiga cordillera


#####################Plains FRUs (5):
train_data_plains <- list(FRU_31_df, FRU_32_df, FRU_26_df, #Boreal plains & southern taiga plains
                          FRU_33_df, FRU_34_df, FRU_35_df) #plains taiga plains 

models_plains <- list(FRU_31_model, FRU_32_model, FRU_26_model, #Boreal plains & southern taiga plains
                      FRU_33_model, FRU_34_model, FRU_35_model) #plains taiga plains

# Assign names to your models for legend purposes
names(models_plains) <- c("Bor.Tai.P(31)", "Bor.Tai.P(32)     ", "Bor.P(26)",
                          "N.Tai.P(33)", "N.Tai.P(34)", "N.Tai.P(35)") 


#####################Shield FRUs (3):
train_data_shield <- list(FRU_24_26_df, FRU_25_df) #Taiga shield 


models_shield <- list(FRU_24_26_model, FRU_25_model) #Taiga shield 


# Assign names to your models for legend purposes
names(models_shield) <- c("T.Shield(24/26)", "T.Shield(25)") 
gc()



###########################################################################################################
############################################CREATE THE FUNCTION############################################
###########################################################################################################
#This version transforms the y-axis to be a probability scale
cord_func <- function(models, train_data, variables) {
  
  cord_colours <- c(
    "#ffade5",
    "#4bbf54",
    "#9e75ef",
    "#fe7349",
    "#ba0067",
    "deepskyblue2",
    "#682c00")
  plot_list <- list()
  
  for (variable in variables) {
    
    smoothed_data_list <- list()
    
    for (i in seq_along(models)) {
      
      model <- models[[i]]
      data <- train_data[[i]]
      
      pdp_data <- partial(
        model,
        pred.var = variable,
        grid.resolution = 75,
        train = data,
        n.trees = model$n.trees
      )
      
      colnames(pdp_data) <- c("Variable", "yhat")
      
      loess_model <- loess(yhat ~ Variable, data = pdp_data, span = 0.5)
      
      pred <- predict(loess_model, se = TRUE)
      
      smoothed_data <- data.frame(
        Variable = pdp_data$Variable,
        Smoothed = pred$fit,
        Lower = pred$fit - 1.96 * pred$se.fit,
        Upper = pred$fit + 1.96 * pred$se.fit
      )
      
      smoothed_data$Smoothed <- 1 / (1 + exp(-smoothed_data$Smoothed))
      smoothed_data$Lower <- 1 / (1 + exp(-smoothed_data$Lower))
      smoothed_data$Upper <- 1 / (1 + exp(-smoothed_data$Upper))
      
      smoothed_data$model <- names(models)[i]
      
      smoothed_data_list[[i]] <- smoothed_data
      
      gc()
    }
    
    combined_smoothed_data <- bind_rows(smoothed_data_list)
    
    p <- ggplot() +
      geom_line(data = combined_smoothed_data,
                aes(x = Variable, y = Smoothed, color = model),
                linewidth = 0.4) +
      geom_ribbon(data = combined_smoothed_data,
                  aes(x = Variable, ymin = Lower, ymax = Upper, fill = model),
                  alpha = 0.2) +
      scale_color_manual(values = cord_colours) +
      scale_fill_manual(values = cord_colours) +
      scale_y_continuous(labels = scales::label_number(accuracy = 0.01)) +
      labs(x = variable,
           y = "Partial Probability",
           color = "Cordillera",      # Add this line to set legend title for color
           fill = "Cordillera") +     # Add this line to set legend title for fill 
      theme_classic()
    
    plot_list[[variable]] <- p
    
    gc()
  }
  
  return(plot_list)
}

shield_func <- function(models, train_data, variables) {
  
  shield_colours <- c(
    "#a4b21d",
    "#00d0b9")
  plot_list <- list()
  
  for (variable in variables) {
    
    smoothed_data_list <- list()
    
    for (i in seq_along(models)) {
      
      model <- models[[i]]
      data <- train_data[[i]]
      
      pdp_data <- partial(
        model,
        pred.var = variable,
        grid.resolution = 75,
        train = data,
        n.trees = model$n.trees
      )
      
      colnames(pdp_data) <- c("Variable", "yhat")
      
      loess_model <- loess(yhat ~ Variable, data = pdp_data, span = 0.5)
      
      pred <- predict(loess_model, se = TRUE)
      
      smoothed_data <- data.frame(
        Variable = pdp_data$Variable,
        Smoothed = pred$fit,
        Lower = pred$fit - 1.96 * pred$se.fit,
        Upper = pred$fit + 1.96 * pred$se.fit
      )
      
      smoothed_data$Smoothed <- 1 / (1 + exp(-smoothed_data$Smoothed))
      smoothed_data$Lower <- 1 / (1 + exp(-smoothed_data$Lower))
      smoothed_data$Upper <- 1 / (1 + exp(-smoothed_data$Upper))
      
      smoothed_data$model <- names(models)[i]
      
      smoothed_data_list[[i]] <- smoothed_data
      
      gc()
    }
    
    combined_smoothed_data <- bind_rows(smoothed_data_list)
    
    p <- ggplot() +
      geom_line(data = combined_smoothed_data,
                aes(x = Variable, y = Smoothed, color = model),
                linewidth = 0.4) +
      geom_ribbon(data = combined_smoothed_data,
                  aes(x = Variable, ymin = Lower, ymax = Upper, fill = model),
                  alpha = 0.2) +
      scale_color_manual(values = shield_colours) +
      scale_fill_manual(values = shield_colours) +
      scale_y_continuous(labels = scales::label_number(accuracy = 0.01)) +
      labs(x = variable,
           y = "Partial Probability",
           color = "Shield",      # Add this line to set legend title for color
           fill = "Shield") +     # Add this line to set legend title for fill 
      theme_classic()
    
    plot_list[[variable]] <- p
    
    gc()
  }
  
  return(plot_list)
}

plains_func <- function(models, train_data, variables) {
  
  plains_colours <- c(
    "#ff6387",
    "#f0972d",
    "#7e0022",
    "#32145c",
    "#5fb282",
    "red")
  plot_list <- list()
  
  for (variable in variables) {
    
    smoothed_data_list <- list()
    
    for (i in seq_along(models)) {
      
      model <- models[[i]]
      data <- train_data[[i]]
      
      pdp_data <- partial(
        model,
        pred.var = variable,
        grid.resolution = 75,
        train = data,
        n.trees = model$n.trees
      )
      
      colnames(pdp_data) <- c("Variable", "yhat")
      
      loess_model <- loess(yhat ~ Variable, data = pdp_data, span = 0.5)
      
      pred <- predict(loess_model, se = TRUE)
      
      smoothed_data <- data.frame(
        Variable = pdp_data$Variable,
        Smoothed = pred$fit,
        Lower = pred$fit - 1.96 * pred$se.fit,
        Upper = pred$fit + 1.96 * pred$se.fit
      )
      
      smoothed_data$Smoothed <- 1 / (1 + exp(-smoothed_data$Smoothed))
      smoothed_data$Lower <- 1 / (1 + exp(-smoothed_data$Lower))
      smoothed_data$Upper <- 1 / (1 + exp(-smoothed_data$Upper))
      
      smoothed_data$model <- names(models)[i]
      
      smoothed_data_list[[i]] <- smoothed_data
      
      gc()
    }
    
    combined_smoothed_data <- bind_rows(smoothed_data_list)
    
    p <- ggplot() +
      geom_line(data = combined_smoothed_data,
                aes(x = Variable, y = Smoothed, color = model),
                linewidth = 0.4) +
      geom_ribbon(data = combined_smoothed_data,
                  aes(x = Variable, ymin = Lower, ymax = Upper, fill = model),
                  alpha = 0.2) +
      scale_color_manual(values = plains_colours) +
      scale_fill_manual(values = plains_colours) +
      scale_y_continuous(labels = scales::label_number(accuracy = 0.01)) +
      labs(x = variable,
           y = "Partial Probability",
           color = "Plains",      # Add this line to set legend title for color
           fill = "Plains") +     # Add this line to set legend title for fill 
      theme_classic()
    
    plot_list[[variable]] <- p
    
    gc()
  }
  
  return(plot_list)
}



##################################################################################################################################
############################################CALL THE FUNCTION FOR EACH GROUP OF MODELS############################################
##################################################################################################################################

# Call the function for the different FRU splits
cord_pdps <-cord_func(models_cord, train_data_cord, variables)
shield_pdps <-shield_func(models_shield, train_data_shield, variables)
plains_pdps <-plains_func(models_plains, train_data_plains, variables)


name_lookup <- setNames(Name_List$Full_Name, Name_List$Variable)





cord_plots_list <- lapply(seq_along(variables), function(i) {

  cord_pdps[[variables[[i]]]] +
    labs(
      x = NULL,   # automatic full name
      y = if (i == 1) "     " else NULL
    ) +
    scale_x_continuous(
      labels = function(x) {
        ifelse(
          x == 0 | x %% 1 == 0,
          sprintf("%.0f", x),
          sprintf("%.2f", x)
        )
      }
    ) +
    theme(axis.title.y = element_text(size = 7),   # Adjust the y-axis title font size
          axis.title.x = element_text(size = 7),  # Adjust the x-axis title font size
          axis.text = element_text(size = 5.5),      # Adjust the font size for both axes (x and y)
          legend.title = element_text(size = 6),   # Increase the legend title font size
          legend.text = element_text(size = 6),    # Increase the legend content font size
          legend.key.size = unit(0.5, "lines")
    )

})
cord_plot <- ggarrange(
  plotlist = cord_plots_list,
  legend = "right",
  common.legend = TRUE,
  ncol = 5,
  nrow = 1
)
#plot(cord_plot)


plains_plots_list <- lapply(seq_along(variables), function(i) {

  plains_pdps[[variables[[i]]]] +
    labs(
      x = NULL,   # automatic full name
      y = if (i == 1) "     " else NULL
    ) +
    scale_x_continuous(
      labels = function(x) {
        ifelse(
          x == 0 | x %% 1 == 0,
          sprintf("%.0f", x),
          sprintf("%.2f", x)
        )
      }
    ) +
    theme(axis.title.y = element_text(size = 7),   # Adjust the y-axis title font size
          axis.title.x = element_text(size = 7),  # Adjust the x-axis title font size
          axis.text = element_text(size = 5.5),      # Adjust the font size for both axes (x and y)
          legend.title = element_text(size = 6),   # Increase the legend title font size
          legend.text = element_text(size = 6),    # Increase the legend content font size
          legend.key.size = unit(0.5, "lines")
    )

})
plains_plot <- ggarrange(
  plotlist = plains_plots_list,
  legend = "right",
  common.legend = TRUE,
  ncol = 5,
  nrow = 1
)
#plot(plains_plot)

shield_plots_list <- lapply(seq_along(variables), function(i) {

  shield_pdps[[variables[[i]]]] +
    labs(
      x = name_lookup[variables[[i]]],   # automatic full name
      y = if (i == 1) "     " else NULL
    ) +
    scale_x_continuous(
      labels = function(x) {
        ifelse(
          x == 0 | x %% 1 == 0,
          sprintf("%.0f", x),
          sprintf("%.2f", x)
        )
      }
    ) +
    theme(axis.title.y = element_text(size = 7),   # Adjust the y-axis title font size
          axis.title.x = element_text(size = 5.2),  # Adjust the x-axis title font size
          axis.text = element_text(size = 5.5),      # Adjust the font size for both axes (x and y)
          legend.title = element_text(size = 6),   # Increase the legend title font size
          legend.text = element_text(size = 6),    # Increase the legend content font size
          legend.key.size = unit(0.5, "lines")
    )

})
shield_plot <- ggarrange(
  plotlist = shield_plots_list,
  legend = "right",
  common.legend = TRUE,
  ncol = 5,
  nrow = 1
)
#plot(shield_plot)





final_combined_plot <- ggarrange(
  cord_plot,
  plains_plot,
  shield_plot,
  ncol = 1,  # Stack them vertically
  nrow = 3,
  common.legend = T
)



#Export
png(filename ="Z:/Christine/NWT Fire Refugia Project Z/Graphs/Refugia/V8/PDPs_Trees_300m.png", width = 19, height = 12, units ="cm", res=600)
plot(ggarrange(final_combined_plot))
dev.off()
gc()
