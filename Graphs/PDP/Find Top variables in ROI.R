library(vip)
library(dismo)
library(gbm)
library(questionr)
library(ggpubr)
library(dplyr)
library(gridExtra)

#################Workflow##############
##Read in the full sample, subset by FRU
##Read in all models, find each model's pR2 value, multiply the importance of each retained variable in the models by that model's pR2 value

##Set up a seperate dataframe that contains only the full list of names. Will be used later to populate any missing variables with a value of 0 so final totals all sum to 0
Name_List <- as.data.frame(matrix(ncol = 1, nrow = 103))
x <- c("Variable")
colnames(Name_List) <- x
Name_List$Variable <- c("Anom_CMI", "Anom_MAT", "Anom_MFST", "Anom_VPD",
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


###############################################################################################################################
#############################################Read in all the models/split off each FRU/Find and multiply by pR2################
###############################################################################################################################
FRU_31_model <- readRDS("Z:/Christine/NWT Fire Refugia Project Z/BRT Models/Models/Final/Refugia/V9/FRU_31_Final_V9.rds")
varimp_FRU_31_model <- as.data.frame(FRU_31_model$contributions) 
cvstats_FRU_31_model <- as.data.frame(FRU_31_model$cv.statistics[c(1,3)])
cvstats_FRU_31_model$deviance.null <- FRU_31_model$self.statistics$mean.null
FRU_31_model_pr2 <- (cvstats_FRU_31_model$deviance.null-cvstats_FRU_31_model$deviance.mean)/cvstats_FRU_31_model$deviance.null

FRU_32_model <- readRDS("Z:/Christine/NWT Fire Refugia Project Z/BRT Models/Models/Final/Refugia/V9/FRU_32_Final_V9.rds")
varimp_FRU_32_model <- as.data.frame(FRU_32_model$contributions) 
cvstats_FRU_32_model <- as.data.frame(FRU_32_model$cv.statistics[c(1,3)])
cvstats_FRU_32_model$deviance.null <- FRU_32_model$self.statistics$mean.null
FRU_32_model_pr2 <- (cvstats_FRU_32_model$deviance.null-cvstats_FRU_32_model$deviance.mean)/cvstats_FRU_32_model$deviance.null

FRU_26_model <- readRDS("Z:/Christine/NWT Fire Refugia Project Z/BRT Models/Models/Final/Refugia/V9/FRU_26_plains_Final_V9.rds")
varimp_FRU_26_model <- as.data.frame(FRU_26_model$contributions) 
cvstats_FRU_26_model <- as.data.frame(FRU_26_model$cv.statistics[c(1,3)])
cvstats_FRU_26_model$deviance.null <- FRU_26_model$self.statistics$mean.null
FRU_26_model_pr2 <- (cvstats_FRU_26_model$deviance.null-cvstats_FRU_26_model$deviance.mean)/cvstats_FRU_26_model$deviance.null

FRU_33_model <- readRDS("Z:/Christine/NWT Fire Refugia Project Z/BRT Models/Models/Final/Refugia/V9/FRU_33_Final_V9.rds")
varimp_FRU_33_model <- as.data.frame(FRU_33_model$contributions) 
cvstats_FRU_33_model <- as.data.frame(FRU_33_model$cv.statistics[c(1,3)])
cvstats_FRU_33_model$deviance.null <- FRU_33_model$self.statistics$mean.null
FRU_33_model_pr2 <- (cvstats_FRU_33_model$deviance.null-cvstats_FRU_33_model$deviance.mean)/cvstats_FRU_33_model$deviance.null

FRU_34_model <- readRDS("Z:/Christine/NWT Fire Refugia Project Z/BRT Models/Models/Final/Refugia/V9/FRU_34_Final_V9.rds")
varimp_FRU_34_model <- as.data.frame(FRU_34_model$contributions) 
cvstats_FRU_34_model <- as.data.frame(FRU_34_model$cv.statistics[c(1,3)])
cvstats_FRU_34_model$deviance.null <- FRU_34_model$self.statistics$mean.null
FRU_34_model_pr2 <- (cvstats_FRU_34_model$deviance.null-cvstats_FRU_34_model$deviance.mean)/cvstats_FRU_34_model$deviance.null

FRU_35_model <- readRDS("Z:/Christine/NWT Fire Refugia Project Z/BRT Models/Models/Final/Refugia/V9/FRU_35_Final_V9.rds")
varimp_FRU_35_model <- as.data.frame(FRU_35_model$contributions) 
cvstats_FRU_35_model <- as.data.frame(FRU_35_model$cv.statistics[c(1,3)])
cvstats_FRU_35_model$deviance.null <- FRU_35_model$self.statistics$mean.null
FRU_35_model_pr2 <- (cvstats_FRU_35_model$deviance.null-cvstats_FRU_35_model$deviance.mean)/cvstats_FRU_35_model$deviance.null

FRU_24_26_model <- readRDS("Z:/Christine/NWT Fire Refugia Project Z/BRT Models/Models/Final/Refugia/V9/FRU_24_26_shield_Final_V9.rds")
varimp_FRU_24_26_model <- as.data.frame(FRU_24_26_model$contributions) 
cvstats_FRU_24_26_model <- as.data.frame(FRU_24_26_model$cv.statistics[c(1,3)])
cvstats_FRU_24_26_model$deviance.null <- FRU_24_26_model$self.statistics$mean.null
FRU_24_26_model_pr2 <- (cvstats_FRU_24_26_model$deviance.null-cvstats_FRU_24_26_model$deviance.mean)/cvstats_FRU_24_26_model$deviance.null

FRU_25_model <- readRDS("Z:/Christine/NWT Fire Refugia Project Z/BRT Models/Models/Final/Refugia/V9/FRU_25_Final_V9.rds")
varimp_FRU_25_model <- as.data.frame(FRU_25_model$contributions) 
cvstats_FRU_25_model <- as.data.frame(FRU_25_model$cv.statistics[c(1,3)])
cvstats_FRU_25_model$deviance.null <- FRU_25_model$self.statistics$mean.null
FRU_25_model_pr2 <- (cvstats_FRU_25_model$deviance.null-cvstats_FRU_25_model$deviance.mean)/cvstats_FRU_25_model$deviance.null

FRU_40_model <- readRDS("Z:/Christine/NWT Fire Refugia Project Z/BRT Models/Models/Final/Refugia/V9/FRU_40_Final_V9.rds")
varimp_FRU_40_model <- as.data.frame(FRU_40_model$contributions) 
cvstats_FRU_40_model <- as.data.frame(FRU_40_model$cv.statistics[c(1,3)])
cvstats_FRU_40_model$deviance.null <- FRU_40_model$self.statistics$mean.null
FRU_40_model_pr2 <- (cvstats_FRU_40_model$deviance.null-cvstats_FRU_40_model$deviance.mean)/cvstats_FRU_40_model$deviance.null

FRU_41_model <- readRDS("Z:/Christine/NWT Fire Refugia Project Z/BRT Models/Models/Final/Refugia/V9/FRU_41_Final_V9.rds")
varimp_FRU_41_model <- as.data.frame(FRU_41_model$contributions) 
cvstats_FRU_41_model <- as.data.frame(FRU_41_model$cv.statistics[c(1,3)])
cvstats_FRU_41_model$deviance.null <- FRU_41_model$self.statistics$mean.null
FRU_41_model_pr2 <- (cvstats_FRU_41_model$deviance.null-cvstats_FRU_41_model$deviance.mean)/cvstats_FRU_41_model$deviance.null

FRU_43_model <- readRDS("Z:/Christine/NWT Fire Refugia Project Z/BRT Models/Models/Final/Refugia/V9/FRU_43_Final_V9.rds")
varimp_FRU_43_model <- as.data.frame(FRU_43_model$contributions) 
cvstats_FRU_43_model <- as.data.frame(FRU_43_model$cv.statistics[c(1,3)])
cvstats_FRU_43_model$deviance.null <- FRU_43_model$self.statistics$mean.null
FRU_43_model_pr2 <- (cvstats_FRU_43_model$deviance.null-cvstats_FRU_43_model$deviance.mean)/cvstats_FRU_43_model$deviance.null

FRU_45_model <- readRDS("Z:/Christine/NWT Fire Refugia Project Z/BRT Models/Models/Final/Refugia/V9/FRU_45_Final_V9.rds")
varimp_FRU_45_model <- as.data.frame(FRU_45_model$contributions) 
cvstats_FRU_45_model <- as.data.frame(FRU_45_model$cv.statistics[c(1,3)])
cvstats_FRU_45_model$deviance.null <- FRU_45_model$self.statistics$mean.null
FRU_45_model_pr2 <- (cvstats_FRU_45_model$deviance.null-cvstats_FRU_45_model$deviance.mean)/cvstats_FRU_45_model$deviance.null

FRU_46_model <- readRDS("Z:/Christine/NWT Fire Refugia Project Z/BRT Models/Models/Final/Refugia/V9/FRU_46_Final_V9.rds")
varimp_FRU_46_model <- as.data.frame(FRU_46_model$contributions) 
cvstats_FRU_46_model <- as.data.frame(FRU_46_model$cv.statistics[c(1,3)])
cvstats_FRU_46_model$deviance.null <- FRU_46_model$self.statistics$mean.null
FRU_46_model_pr2 <- (cvstats_FRU_46_model$deviance.null-cvstats_FRU_46_model$deviance.mean)/cvstats_FRU_46_model$deviance.null

FRU_37_38_42_model <- readRDS("Z:/Christine/NWT Fire Refugia Project Z/BRT Models/Models/Final/Refugia/V9/FRU_37_38_42_Final_V9.rds")
varimp_FRU_37_38_42_model <- as.data.frame(FRU_37_38_42_model$contributions) 
cvstats_FRU_37_38_42_model <- as.data.frame(FRU_37_38_42_model$cv.statistics[c(1,3)])
cvstats_FRU_37_38_42_model$deviance.null <- FRU_37_38_42_model$self.statistics$mean.null
FRU_37_38_42_model_pr2 <- (cvstats_FRU_37_38_42_model$deviance.null-cvstats_FRU_37_38_42_model$deviance.mean)/cvstats_FRU_37_38_42_model$deviance.null



###########Calculate the importance the retained variables in each model and multiply each by the model's pR2
BST1 <- as.data.frame(vi(FRU_31_model))
BST1 <- left_join(Name_List, BST1, by = "Variable")
BST1$Importance[is.na(BST1$Importance)] <- 0
BST1$Importance <- (BST1$Importance)*FRU_31_model_pr2

BST2 <- as.data.frame(vi(FRU_32_model))
BST2 <- left_join(Name_List, BST2, by = "Variable")
BST2$Importance[is.na(BST2$Importance)] <- 0
BST2$Importance <- (BST2$Importance)*FRU_32_model_pr2

BST_Final <- as.data.frame(rbind(BST1, BST2))



TP1 <- as.data.frame(vi(FRU_26_model))
TP1 <- left_join(Name_List, TP1, by = "Variable")
TP1$Importance[is.na(TP1$Importance)] <- 0
TP1$Importance <- (TP1$Importance)*FRU_26_model_pr2

TP2 <- as.data.frame(vi(FRU_33_model))
TP2 <- left_join(Name_List, TP2, by = "Variable")
TP2$Importance[is.na(TP2$Importance)] <- 0
TP2$Importance <- (TP2$Importance)*FRU_33_model_pr2

TP3 <- as.data.frame(vi(FRU_34_model))
TP3 <- left_join(Name_List, TP3, by = "Variable")
TP3$Importance[is.na(TP3$Importance)] <- 0
TP3$Importance <- (TP3$Importance)*FRU_34_model_pr2

TP4 <- as.data.frame(vi(FRU_35_model))
TP4 <- left_join(Name_List, TP4, by = "Variable")
TP4$Importance[is.na(TP4$Importance)] <- 0
TP4$Importance <- (TP4$Importance)*FRU_35_model_pr2

TP_Final <- as.data.frame(rbind(TP1, TP2, TP3, TP4))




TS1 <- as.data.frame(vi(FRU_24_26_model))
TS1 <- left_join(Name_List, TS1, by = "Variable")
TS1$Importance[is.na(TS1$Importance)] <- 0
TS1$Importance <- (TS1$Importance)*FRU_24_26_model_pr2

TS2 <- as.data.frame(vi(FRU_25_model))
TS2 <- left_join(Name_List, TS2, by = "Variable")
TS2$Importance[is.na(TS2$Importance)] <- 0
TS2$Importance <- (TS2$Importance)*FRU_25_model_pr2

TS_Final <- as.data.frame(rbind(TS1, TS2))




BC1 <- as.data.frame(vi(FRU_40_model))
BC1 <- left_join(Name_List, BC1, by = "Variable")
BC1$Importance[is.na(BC1$Importance)] <- 0
BC1$Importance <- (BC1$Importance)*FRU_40_model_pr2

BC2 <- as.data.frame(vi(FRU_41_model))
BC2 <- left_join(Name_List, BC2, by = "Variable")
BC2$Importance[is.na(BC2$Importance)] <- 0
BC2$Importance <- (BC2$Importance)*FRU_41_model_pr2

BC3 <- as.data.frame(vi(FRU_43_model))
BC3 <- left_join(Name_List, BC3, by = "Variable")
BC3$Importance[is.na(BC3$Importance)] <- 0
BC3$Importance <- (BC3$Importance)*FRU_43_model_pr2

BC4 <- as.data.frame(vi(FRU_45_model))
BC4 <- left_join(Name_List, BC4, by = "Variable")
BC4$Importance[is.na(BC4$Importance)] <- 0
BC4$Importance <- (BC4$Importance)*FRU_45_model_pr2

BC5 <- as.data.frame(vi(FRU_46_model))
BC5 <- left_join(Name_List, BC5, by = "Variable")
BC5$Importance[is.na(BC5$Importance)] <- 0
BC5$Importance <- (BC5$Importance)*FRU_46_model_pr2

BC_Final <- as.data.frame(rbind(BC1, BC2, BC3, BC4, BC5))



TC1 <- as.data.frame(vi(FRU_37_38_42_model))
TC1 <- left_join(Name_List, TC1, by = "Variable")
TC1$Importance[is.na(TC1$Importance)] <- 0
TC1$Importance <- (TC1$Importance)*FRU_37_38_42_model_pr2
TC_Final <- as.data.frame(TC1)


####################################################################################################################################
#################################Bind all final versions together into one covering the province####################################
####################################################################################################################################

Province <- rbind(BST_Final, TP_Final, TS_Final, BC_Final, TC_Final)


####################################################################################################################################
#################################Find the top 5 most important variables for the province as a whole################################
####################################################################################################################################

Province$Variable <- as.factor(Province$Variable)
Prov_Ag <- aggregate(Province$Importance, list(Province$Variable), FUN = mean)
Prov_Sort <- Prov_Ag[sort.list(Prov_Ag$x, decreasing=T),]
Prov_5 <- Prov_Sort[1:10,]
Prov_5 <- Prov_5 %>%rename(Variables = Group.1, Importance = x)
Prov_5_un <- unique(Prov_5$Variables)
Prov_5_un
#Top 5 variables are: TPI_300m, SCANFI_Herbs_300m, Insect_0_10_300m, Fire_1_10_300m, Pontone_Uplands_300m
#Next important are: SCANFI_Closure_300m, DOB, SCANFI_Shrub_Tall_300m, Pontone_Peat_Perma_300m, SCANFI_Other_Con_300m
