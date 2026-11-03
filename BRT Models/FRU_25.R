library(terra)
library(dplyr)
library(dismo)
library(gbm)
library(data.table)

df <- fread("W:/EDM/CCVegMod4/Christine Fire Refugia Projects/NWT Fire Refugia Project/Dataset CSVs/(Step 7) Final/V6/Training_Sample_V6.csv")
df <- as.data.frame(df)

df$FRU <- as.factor(df$FRU)

FRU_25 <- df %>%
  filter(FRU == 25) %>%
  droplevels()

set.seed(123) # Makes sure your sample is reproducible 
FRU_25 <- FRU_25[sample(1:nrow(FRU_25), 25000,
                                    replace=FALSE),]

corrs <- list('TRI',
              'Normal_MAT',
              'Fire_10_20_1200m',
              'Fire_20_30_1200m',
              'SCANFI_Biomass_1200m',
              'SCANFI_Other_Con_1200m',
              'SCANFI_Other_Con_300m',
            #Any with all 0s
              "Pontone_Agri_1200m",
              "Pontone_Agri_300m",
              "Pont_Urban_300m",
            #Now the ones that aren't for use in this FRU
              "NALCMS_Crop_300m", "NALCMS_Crop_1200m","NALCMS_Urban_1200m", "NALCMS_Urban_300m", 
              "Herm_Treed_Wetland_1200m", "Herm_Treed_Wetland_300m", "Herm_Wetland_1200m", "Herm_Wetland_300m")


FRU_25_Vars <- FRU_25 %>%
  dplyr::select("Refugia", 
                "Anom_CMI", "Anom_MAT", "Anom_MFST", "Anom_VPD",
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

FRU_25_Vars <- dplyr::select(FRU_25_Vars,
                                -all_of(unlist(corrs)))

##Now build the BRT
set.seed(123) # Makes sure your model is reproducible 
FRU_25_GBM <- gbm.step(data=FRU_25_Vars, gbm.x = 2:86, gbm.y = 1, family = "bernoulli", 
                          tree.complexity = 3, learning.rate = 0.01, bag.fraction = 0.5, max.trees=20000)
saveRDS(FRU_25_GBM, file = "W:/EDM/CCVegMod4/Christine Fire Refugia Projects/NWT Fire Refugia Project/BRT Models/Models/Original/Refugia/V9/FRU_25_Orig_V9.rds") #Save the model
gc()


#Simplify the model by telling it it's allowed to drop all but the top 20 covariates
set.seed(123)
FRU_25_GBM_Simp <- gbm.simplify(FRU_25_GBM, n.drops = 65) 
saveRDS(FRU_25_GBM_Simp, file =  "W:/EDM/CCVegMod4/Christine Fire Refugia Projects/NWT Fire Refugia Project/BRT Models/Models/Simplified/Refugia/V9/FRU_25_Simp_V9.rds") #Save the model
gc()


##Rerun with those X variables dropped and save the model to use later
set.seed(123)
FRU_25_GBM_Reduced <- gbm.step(FRU_25_Vars, gbm.x=FRU_25_GBM_Simp$pred.list[[14]], gbm.y=1, 
                                  tree.complexity=3, learning.rate=0.01, bag.fraction = 0.5, max.trees=20000)
saveRDS(FRU_25_GBM_Reduced, file = "W:/EDM/CCVegMod4/Christine Fire Refugia Projects/NWT Fire Refugia Project/BRT Models/Models/Final/Refugia/V9/FRU_25_Final_V9.rds") #Save the model
gc()

##Run the saved version on the rest of the dataset
summary(FRU_25_GBM_Reduced, cBars = 10, las = 2)


##Now Visualize Things
# Create partial dependence plots of the variables 
gbm.plot(FRU_25_GBM_Reduced, n.plots = 88, plot.layout=c(2, 3), write.title = F, show.contrib = T, smooth=T)

# Find the interactions and their strength
find.int <- gbm.interactions(FRU_25_GBM_Reduced)
find.int$interactions
find.int$rank.list

