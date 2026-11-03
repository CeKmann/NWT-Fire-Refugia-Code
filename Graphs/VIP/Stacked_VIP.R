library(vip)
library(dismo)
library(gbm)
library(ggplot2)
library(questionr)
library(ggpubr)
library(dplyr)
library(gridExtra)
theme_set(theme_classic())

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
  Group = c("(1) Climate anomalies", "(1) Climate anomalies", "(1) Climate anomalies", "(1) Climate anomalies",
            "(1) Climate normals", "(1) Climate normals", "(1) Climate normals", "(1) Climate normals",
            "(1) Day of burn", '(1) Daily fire weather', '(1) Daily fire weather', '(1) Daily fire weather', '(1) Daily fire weather',
            "(2) Wetlands", "(2) Soils/permafrost", "(2) Soils/permafrost", "(2) Topography", "(2) Topography", "(2) Water/non-fuels", "(2) Water/non-fuels", "(2) Topography", "(2) Topography", "(2) Soils/permafrost",
            "(3) Vegetation", "(3) Vegetation", "(3) Vegetation", "(3) Vegetation", "(2) Wetlands", "(2) Wetlands",
            "(2) Wetlands", "(2) Wetlands", "(2) Wetlands", "(2) Wetlands",
            "(2) Wetlands", "(2) Wetlands", "(2) Wetlands", "(2) Wetlands",
            "(2) Wetlands", "(2) Wetlands",       
            "(2) Soils/permafrost", "(2) Soils/permafrost", "(2) Topography", "(2) Soils/permafrost", "(2) Soils/permafrost", "(2) Topography","(2) Topography", "(2) Topography",  "(2) Topography",                        
            "(2) Water/non-fuels", "(2) Water/non-fuels", "(2) Water/non-fuels",
            "(3) Cutblocks", "(3) Cutblocks", "(3) Cutblocks", "(3) Cutblocks",
            "(3) Past fire", "(3) Past fire", "(3) Past fire", "(3) Past fire", "(3) Past fire", "(3) Past fire",
            "(3) Insect disturbance", "(3) Insect disturbance", "(3) Insect disturbance", "(3) Insect disturbance",
            "(3) Vegetation", "(3) Vegetation", "(3) Vegetation", "(3) Vegetation", "(3) Vegetation",        
            "(3) Vegetation", "(3) Vegetation", "(3) Vegetation", "(3) Vegetation", "(3) Vegetation",
            "(3) Vegetation", "(3) Vegetation", "(2) Water/non-fuels", "(2) Water/non-fuels", "(2) Water/non-fuels", "(2) Water/non-fuels",
            "(2) Water/non-fuels", "(2) Water/non-fuels", "(2) Wetlands", "(2) Wetlands", "(2) Wetlands",        
            "(2) Wetlands", "(3) Vegetation", "(3) Vegetation", "(3) Vegetation", "(3) Vegetation",
            "(3) Vegetation", "(3) Vegetation", "(3) Vegetation", "(3) Vegetation", "(3) Vegetation",
            "(3) Vegetation", "(3) Vegetation", "(3) Vegetation", "(3) Vegetation", "(3) Vegetation",
            "(3) Vegetation", "(3) Vegetation"),
  Class = c("(1) Top-down", "(1) Top-down", "(1) Top-down", "(1) Top-down",
            "(1) Top-down", "(1) Top-down", "(1) Top-down", "(1) Top-down",
            "(1) Top-down", "(1) Top-down", "(1) Top-down", "(1) Top-down", "(1) Top-down",
            "(2) Bottom-up (phys. setting)", "(2) Bottom-up (phys. setting)", "(2) Bottom-up (phys. setting)", "(2) Bottom-up (phys. setting)", "(2) Bottom-up (phys. setting)", "(2) Bottom-up (phys. setting)", "(2) Bottom-up (phys. setting)", "(2) Bottom-up (phys. setting)", "(2) Bottom-up (phys. setting)", "(2) Bottom-up (phys. setting)",
            "(3) Bottom-up (fuels)", "(3) Bottom-up (fuels)", "(3) Bottom-up (fuels)", "(3) Bottom-up (fuels)", "(2) Bottom-up (phys. setting)", "(2) Bottom-up (phys. setting)",
            "(2) Bottom-up (phys. setting)", "(2) Bottom-up (phys. setting)", "(2) Bottom-up (phys. setting)", "(2) Bottom-up (phys. setting)",
            "(2) Bottom-up (phys. setting)", "(2) Bottom-up (phys. setting)", "(2) Bottom-up (phys. setting)", "(2) Bottom-up (phys. setting)",
            "(2) Bottom-up (phys. setting)", "(2) Bottom-up (phys. setting)",       
            "(2) Bottom-up (phys. setting)", "(2) Bottom-up (phys. setting)", "(2) Bottom-up (phys. setting)", "(2) Bottom-up (phys. setting)", "(2) Bottom-up (phys. setting)", "(2) Bottom-up (phys. setting)","(2) Bottom-up (phys. setting)", "(2) Bottom-up (phys. setting)",  "(2) Bottom-up (phys. setting)",                        
            "(2) Bottom-up (phys. setting)", "(2) Bottom-up (phys. setting)", "(2) Bottom-up (phys. setting)",
            "(3) Bottom-up (fuels)", "(3) Bottom-up (fuels)", "(3) Bottom-up (fuels)", "(3) Bottom-up (fuels)",
            "(3) Bottom-up (fuels)", "(3) Bottom-up (fuels)", "(3) Bottom-up (fuels)", "(3) Bottom-up (fuels)", "(3) Bottom-up (fuels)", "(3) Bottom-up (fuels)",
            "(3) Bottom-up (fuels)", "(3) Bottom-up (fuels)", "(3) Bottom-up (fuels)", "(3) Bottom-up (fuels)",
            "(3) Bottom-up (fuels)", "(3) Bottom-up (fuels)", "(3) Bottom-up (fuels)", "(3) Bottom-up (fuels)", "(3) Bottom-up (fuels)",        
            "(3) Bottom-up (fuels)", "(3) Bottom-up (fuels)", "(3) Bottom-up (fuels)", "(3) Bottom-up (fuels)", "(3) Bottom-up (fuels)",
            "(3) Bottom-up (fuels)", "(3) Bottom-up (fuels)", "(2) Bottom-up (phys. setting)", "(2) Bottom-up (phys. setting)", "(2) Bottom-up (phys. setting)", "(2) Bottom-up (phys. setting)",
            "(2) Bottom-up (phys. setting)", "(2) Bottom-up (phys. setting)", "(2) Bottom-up (phys. setting)", "(2) Bottom-up (phys. setting)", "(2) Bottom-up (phys. setting)",        
            "(2) Bottom-up (phys. setting)", "(3) Bottom-up (fuels)", "(3) Bottom-up (fuels)", "(3) Bottom-up (fuels)", "(3) Bottom-up (fuels)",
            "(3) Bottom-up (fuels)", "(3) Bottom-up (fuels)", "(3) Bottom-up (fuels)", "(3) Bottom-up (fuels)", "(3) Bottom-up (fuels)",
            "(3) Bottom-up (fuels)", "(3) Bottom-up (fuels)", "(3) Bottom-up (fuels)", "(3) Bottom-up (fuels)", "(3) Bottom-up (fuels)",
            "(3) Bottom-up (fuels)", "(3) Bottom-up (fuels)"))


#######################################################################################################################################
##############################################Boreal/Southern Taiga plains#############################################################
#######################################################################################################################################

#Read in your models and find the sum of the variable groups in each of them
FRU_31 <- readRDS("Z:/Christine/NWT Fire Refugia Project Z/BRT Models/Models/Final/Refugia/V9/FRU_31_Final_V9.rds")
BST1 <- as.data.frame(vi(FRU_31))
BST1 <- full_join(Name_List, BST1, by = "Variable")
BST1$Importance[is.na(BST1$Importance)] <- 0
BST1_Ag_Vars <- aggregate(BST1$Importance, list(BST1$Group), FUN = sum)


FRU_32 <- readRDS("Z:/Christine/NWT Fire Refugia Project Z/BRT Models/Models/Final/Refugia/V9/FRU_32_Final_V9.rds")
BST2 <- as.data.frame(vi(FRU_32))
BST2 <- full_join(Name_List, BST2, by = "Variable")
BST2$Importance[is.na(BST2$Importance)] <- 0
BST2_Ag_Vars <- aggregate(BST2$Importance, list(BST2$Group), FUN = sum)



#Bring them all together into one and find the mean of all the variable groups between them
BST_Bound <- rbind(BST1_Ag_Vars, BST2_Ag_Vars) 
  colnames(BST_Bound) <- c("Group", "Importance")
BST_Ag_Vars <- aggregate(BST_Bound$Importance, list(BST_Bound$Group), FUN = mean)
  colnames(BST_Ag_Vars) <- c("Group", "Mean_Importance")
BST_Ag_Vars <- BST_Ag_Vars[sort.list(BST_Ag_Vars$Mean_Importance, decreasing=T),]


#You'll lose one of your columns in that last step. Add it back in here
Class_Map <- Name_List %>%
  group_by(Group) %>%
  summarise(Class = first(Class)) %>%
  ungroup()
BST_Ag_Vars <- BST_Ag_Vars %>%
  left_join(Class_Map, by = "Group")


#Add a column for the region and fill it in
Region <- rep(c("Boreal plains & southern taiga plains (FRUs 31, 32)"), each = 12)
dfreg1 <- data.frame(Region)
BST_Ag_Vars <- cbind(BST_Ag_Vars, dfreg1)


#Select order of classes along the x axis
BST_Ag_Vars$Class <- factor(BST_Ag_Vars$Class, levels = c(
  "(1) Top-down", "(2) Bottom-up (phys. setting)", "(3) Bottom-up (fuels)"))
#Select order of groups in stack
BST_Ag_Vars$Group <- factor(BST_Ag_Vars$Group, levels = c(
  "(1) Day of burn", "(1) Daily fire weather", "(1) Climate anomalies", "(1) Climate normals",  
  "(3) Cutblocks", "(3) Past fire", "(3) Insect disturbance", "(3) Vegetation", 
  "(2) Soils/permafrost", "(2) Water/non-fuels", "(2) Wetlands", "(2) Topography"))


#Build graph and plot
BST_Stacked <- ggplot(data = BST_Ag_Vars, aes(x = Class, y = Mean_Importance, fill = Group)) + 
  geom_bar(stat='identity') +
  scale_fill_manual(breaks = c("(1) Climate anomalies", "(1) Climate normals", "(1) Daily fire weather", "(1) Day of burn", 
                               "(2) Topography", "(2) Wetlands", "(2) Soils/permafrost", "(2) Water/non-fuels",
                               "(3) Vegetation", "(3) Cutblocks", "(3) Past fire", '(3) Insect disturbance'), 
                    values=c('chocolate1', 'darkgoldenrod1', 'maroon','darkorchid',
                             'sienna4','cadetblue1', 'burlywood3','blue',
                             'chartreuse4', 'yellow', 'red', 'azure2')) +
  #labs(fill = "Variable grouping") +
  theme(legend.position="Left") +
  ylab("Importance") +
  xlab("")+
  ylim(0, 60) +
  theme(axis.title.y = element_text(size = rel(1.5)))+
  ggtitle("") +
  facet_wrap(~Region)
plot(BST_Stacked)



#######################################################################################################################
##############################################Taiga plains#############################################################
#######################################################################################################################
#Read in your models and find the sum of the variable groups in each of them
FRU_26 <- readRDS("Z:/Christine/NWT Fire Refugia Project Z/BRT Models/Models/Final/Refugia/V9/FRU_26_plains_Final_V9.rds")
TP1 <- as.data.frame(vi(FRU_26))
TP1 <- full_join(Name_List, TP1, by = "Variable")
TP1$Importance[is.na(TP1$Importance)] <- 0
TP1_Ag_Vars <- aggregate(TP1$Importance, list(TP1$Group), FUN = sum)

FRU_33 <- readRDS("Z:/Christine/NWT Fire Refugia Project Z/BRT Models/Models/Final/Refugia/V9/FRU_33_Final_V9.rds")
TP2 <- as.data.frame(vi(FRU_33))
TP2 <- full_join(Name_List, TP2, by = "Variable")
TP2$Importance[is.na(TP2$Importance)] <- 0
TP2_Ag_Vars <- aggregate(TP2$Importance, list(TP2$Group), FUN = sum)

FRU_34 <- readRDS("Z:/Christine/NWT Fire Refugia Project Z/BRT Models/Models/Final/Refugia/V9/FRU_34_Final_V9.rds")
TP3 <- as.data.frame(vi(FRU_34))
TP3 <- full_join(Name_List, TP3, by = "Variable")
TP3$Importance[is.na(TP3$Importance)] <- 0
TP3_Ag_Vars <- aggregate(TP3$Importance, list(TP3$Group), FUN = sum)

FRU_35 <- readRDS("Z:/Christine/NWT Fire Refugia Project Z/BRT Models/Models/Final/Refugia/V9/FRU_35_Final_V9.rds")
TP4 <- as.data.frame(vi(FRU_35))
TP4 <- full_join(Name_List, TP4, by = "Variable")
TP4$Importance[is.na(TP4$Importance)] <- 0
TP4_Ag_Vars <- aggregate(TP4$Importance, list(TP4$Group), FUN = sum)


#Bring them all together into one and find the mean of all the variable groups between them
TP_Bound <- rbind(TP1_Ag_Vars, TP2_Ag_Vars, TP3_Ag_Vars, TP4_Ag_Vars) 
colnames(TP_Bound) <- c("Group", "Importance")
TP_Ag_Vars <- aggregate(TP_Bound$Importance, list(TP_Bound$Group), FUN = mean)
colnames(TP_Ag_Vars) <- c("Group", "Mean_Importance")
TP_Ag_Vars <- TP_Ag_Vars[sort.list(TP_Ag_Vars$Mean_Importance, decreasing=T),]


#You'll lose one of your columns in that last step. Add it back in here
Class_Map <- Name_List %>%
  group_by(Group) %>%
  summarise(Class = first(Class)) %>%
  ungroup()
TP_Ag_Vars <- TP_Ag_Vars %>%
  left_join(Class_Map, by = "Group")


#Add a column for the region and fill it in
Region <- rep(c("Taiga plains (FRUs 26, 33, 34, 35)"), each = 12)
dfreg1 <- data.frame(Region)
TP_Ag_Vars <- cbind(TP_Ag_Vars, dfreg1)


#Select order of classes along the x axis
TP_Ag_Vars$Class <- factor(TP_Ag_Vars$Class, levels = c(
  "(1) Top-down", "(2) Bottom-up (phys. setting)", "(3) Bottom-up (fuels)"))
#Select order of groups in stack
TP_Ag_Vars$Group <- factor(TP_Ag_Vars$Group, levels = c(
  "(1) Day of burn", "(1) Daily fire weather", "(1) Climate anomalies", "(1) Climate normals",  
  "(3) Cutblocks", "(3) Past fire", "(3) Insect disturbance", "(3) Vegetation", 
  "(2) Soils/permafrost", "(2) Water/non-fuels", "(2) Wetlands", "(2) Topography"))


#Build graph and plot
TP_Stacked <- ggplot(data = TP_Ag_Vars, aes(x = Class, y = Mean_Importance, fill = Group)) + 
  geom_bar(stat='identity') +
  scale_fill_manual(breaks = c("(1) Climate anomalies", "(1) Climate normals", "(1) Daily fire weather", "(1) Day of burn", 
                               "(2) Topography", "(2) Wetlands", "(2) Soils/permafrost", "(2) Water/non-fuels",
                               "(3) Vegetation", "(3) Cutblocks", "(3) Past fire", '(3) Insect disturbance'), 
                    values=c('chocolate1', 'darkgoldenrod1', 'maroon','darkorchid',
                             'sienna4','cadetblue1', 'burlywood3','blue',
                             'chartreuse4', 'yellow', 'red', 'azure2')) +
  #labs(fill = "Variable grouping") +
  theme(legend.position="Left") +
  ylab("Importance") +
  xlab("")+
  ylim(0, 60) +
  theme(axis.title.y = element_text(size = rel(1.5)))+
  ggtitle("") +
  facet_wrap(~Region)
plot(TP_Stacked)



#######################################################################################################################
##############################################Taiga shield#############################################################
#######################################################################################################################
#Read in your models and find the sum of the variable groups in each of them
FRU_24_26 <- readRDS("Z:/Christine/NWT Fire Refugia Project Z/BRT Models/Models/Final/Refugia/V9/FRU_24_26_shield_Final_V9.rds")
TS1 <- as.data.frame(vi(FRU_24_26))
TS1 <- full_join(Name_List, TS1, by = "Variable")
TS1$Importance[is.na(TS1$Importance)] <- 0
TS1_Ag_Vars <- aggregate(TS1$Importance, list(TS1$Group), FUN = sum)

FRU_25 <- readRDS("Z:/Christine/NWT Fire Refugia Project Z/BRT Models/Models/Final/Refugia/V9/FRU_25_Final_V9.rds")
TS2 <- as.data.frame(vi(FRU_25))
TS2 <- full_join(Name_List, TS2, by = "Variable")
TS2$Importance[is.na(TS2$Importance)] <- 0
TS2_Ag_Vars <- aggregate(TS2$Importance, list(TS2$Group), FUN = sum)


#Bring them all together into one and find the mean of all the variable groups between them
TS_Bound <- rbind(TS1_Ag_Vars, TS2_Ag_Vars) 
colnames(TS_Bound) <- c("Group", "Importance")
TS_Ag_Vars <- aggregate(TS_Bound$Importance, list(TS_Bound$Group), FUN = mean)
colnames(TS_Ag_Vars) <- c("Group", "Mean_Importance")
TS_Ag_Vars <- TS_Ag_Vars[sort.list(TS_Ag_Vars$Mean_Importance, decreasing=T),]


#You'll lose one of your columns in that last step. Add it back in here
Class_Map <- Name_List %>%
  group_by(Group) %>%
  summarise(Class = first(Class)) %>%
  ungroup()
TS_Ag_Vars <- TS_Ag_Vars %>%
  left_join(Class_Map, by = "Group")


#Add a column for the region and fill it in
Region <- rep(c("Taiga shield (FRUs 24/26, 25)"), each = 12)
dfreg1 <- data.frame(Region)
TS_Ag_Vars <- cbind(TS_Ag_Vars, dfreg1)


#Select order of classes along the x axis
TS_Ag_Vars$Class <- factor(TS_Ag_Vars$Class, levels = c(
  "(1) Top-down", "(2) Bottom-up (phys. setting)", "(3) Bottom-up (fuels)"))
#Select order of groups in stack
TS_Ag_Vars$Group <- factor(TS_Ag_Vars$Group, levels = c(
  "(1) Day of burn", "(1) Daily fire weather", "(1) Climate anomalies", "(1) Climate normals",  
  "(3) Cutblocks", "(3) Past fire", "(3) Insect disturbance", "(3) Vegetation", 
  "(2) Soils/permafrost", "(2) Water/non-fuels", "(2) Wetlands", "(2) Topography"))


#Build graph and plot
TS_Stacked <- ggplot(data = TS_Ag_Vars, aes(x = Class, y = Mean_Importance, fill = Group)) + 
  geom_bar(stat='identity') +
  scale_fill_manual(breaks = c("(1) Climate anomalies", "(1) Climate normals", "(1) Daily fire weather", "(1) Day of burn", 
                               "(2) Topography", "(2) Wetlands", "(2) Soils/permafrost", "(2) Water/non-fuels",
                               "(3) Vegetation", "(3) Cutblocks", "(3) Past fire", '(3) Insect disturbance'), 
                    values=c('chocolate1', 'darkgoldenrod1', 'maroon','darkorchid',
                             'sienna4','cadetblue1', 'burlywood3','blue',
                             'chartreuse4', 'yellow', 'red', 'azure2')) +
  #labs(fill = "Variable grouping") +
  theme(legend.position="Left") +
  ylab("Importance") +
  xlab("")+
  ylim(0, 60) +
  theme(axis.title.y = element_text(size = rel(1.5)))+
  ggtitle("") +
  facet_wrap(~Region)
plot(TS_Stacked)




#######################################################################################################################
##############################################Taiga cordillera#########################################################
#######################################################################################################################
#Read in your models and find the sum of the variable groups in each of them
FRU_37_38_42 <- readRDS("Z:/Christine/NWT Fire Refugia Project Z/BRT Models/Models/Final/Refugia/V9/FRU_37_38_42_Final_V9.rds")
TC1 <- as.data.frame(vi(FRU_37_38_42))
TC1 <- full_join(Name_List, TC1, by = "Variable")
TC1$Importance[is.na(TC1$Importance)] <- 0
TC1_Ag_Vars <- aggregate(TC1$Importance, list(TC1$Group), FUN = sum)


#Bring them all together into one and find the mean of all the variable groups between them
colnames(TC1_Ag_Vars) <- c("Group", "Importance")
TC_Ag_Vars <- aggregate(TC1_Ag_Vars$Importance, list(TC1_Ag_Vars$Group), FUN = mean)
colnames(TC_Ag_Vars) <- c("Group", "Mean_Importance")
TC_Ag_Vars <- TC_Ag_Vars[sort.list(TC_Ag_Vars$Mean_Importance, decreasing=T),]


#You'll lose one of your columns in that last step. Add it back in here
Class_Map <- Name_List %>%
  group_by(Group) %>%
  summarise(Class = first(Class)) %>%
  ungroup()
TC_Ag_Vars <- TC_Ag_Vars %>%
  left_join(Class_Map, by = "Group")


#Add a column for the region and fill it in
Region <- rep(c("Taiga cordillera (FRUs 37/38/42)"), each = 12)
dfreg1 <- data.frame(Region)
TC_Ag_Vars <- cbind(TC_Ag_Vars, dfreg1)


#Select order of classes along the x axis
TC_Ag_Vars$Class <- factor(TC_Ag_Vars$Class, levels = c(
  "(1) Top-down", "(2) Bottom-up (phys. setting)", "(3) Bottom-up (fuels)"))
#Select order of groups in stack
TC_Ag_Vars$Group <- factor(TC_Ag_Vars$Group, levels = c(
  "(1) Day of burn", "(1) Daily fire weather", "(1) Climate anomalies", "(1) Climate normals",  
  "(3) Cutblocks", "(3) Past fire", "(3) Insect disturbance", "(3) Vegetation", 
  "(2) Soils/permafrost", "(2) Water/non-fuels", "(2) Wetlands", "(2) Topography"))


#Build graph and plot
TC_Stacked <- ggplot(data = TC_Ag_Vars, aes(x = Class, y = Mean_Importance, fill = Group)) + 
  geom_bar(stat='identity') +
  scale_fill_manual(breaks = c("(1) Climate anomalies", "(1) Climate normals", "(1) Daily fire weather", "(1) Day of burn", 
                               "(2) Topography", "(2) Wetlands", "(2) Soils/permafrost", "(2) Water/non-fuels",
                               "(3) Vegetation", "(3) Cutblocks", "(3) Past fire", '(3) Insect disturbance'), 
                    values=c('chocolate1', 'darkgoldenrod1', 'maroon','darkorchid',
                             'sienna4','cadetblue1', 'burlywood3','blue',
                             'chartreuse4', 'yellow', 'red', 'azure2')) +
  #labs(fill = "Variable grouping") +
  theme(legend.position="Left") +
  ylab("Importance") +
  xlab("")+
  ylim(0, 60) +
  theme(axis.title.y = element_text(size = rel(1.5)))+
  ggtitle("") +
  facet_wrap(~Region)
plot(TC_Stacked)



#######################################################################################################################
##############################################Boreal cordillera########################################################
#######################################################################################################################
#Read in your models and find the sum of the variable groups in each of them
FRU_40 <- readRDS("Z:/Christine/NWT Fire Refugia Project Z/BRT Models/Models/Final/Refugia/V9/FRU_40_Final_V9.rds")
BC1 <- as.data.frame(vi(FRU_40))
BC1 <- full_join(Name_List, BC1, by = "Variable")
BC1$Importance[is.na(BC1$Importance)] <- 0
BC1_Ag_Vars <- aggregate(BC1$Importance, list(BC1$Group), FUN = sum)

FRU_41 <- readRDS("Z:/Christine/NWT Fire Refugia Project Z/BRT Models/Models/Final/Refugia/V9/FRU_41_Final_V9.rds")
BC2 <- as.data.frame(vi(FRU_41))
BC2 <- full_join(Name_List, BC2, by = "Variable")
BC2$Importance[is.na(BC2$Importance)] <- 0
BC2_Ag_Vars <- aggregate(BC2$Importance, list(BC2$Group), FUN = sum)

FRU_43 <- readRDS("Z:/Christine/NWT Fire Refugia Project Z/BRT Models/Models/Final/Refugia/V9/FRU_43_Final_V9.rds")
BC3 <- as.data.frame(vi(FRU_43))
BC3 <- full_join(Name_List, BC3, by = "Variable")
BC3$Importance[is.na(BC3$Importance)] <- 0
BC3_Ag_Vars <- aggregate(BC3$Importance, list(BC3$Group), FUN = sum)

FRU_45 <- readRDS("Z:/Christine/NWT Fire Refugia Project Z/BRT Models/Models/Final/Refugia/V9/FRU_45_Final_V9.rds")
BC4 <- as.data.frame(vi(FRU_45))
BC4 <- full_join(Name_List, BC4, by = "Variable")
BC4$Importance[is.na(BC4$Importance)] <- 0
BC4_Ag_Vars <- aggregate(BC4$Importance, list(BC4$Group), FUN = sum)

FRU_46 <- readRDS("Z:/Christine/NWT Fire Refugia Project Z/BRT Models/Models/Final/Refugia/V9/FRU_46_Final_V9.rds")
BC5 <- as.data.frame(vi(FRU_46))
BC5 <- full_join(Name_List, BC5, by = "Variable")
BC5$Importance[is.na(BC5$Importance)] <- 0
BC5_Ag_Vars <- aggregate(BC5$Importance, list(BC5$Group), FUN = sum)


#Bring them all together into one and find the mean of all the variable groups between them
BC_Bound <- rbind(BC1_Ag_Vars, BC2_Ag_Vars, BC3_Ag_Vars, BC4_Ag_Vars, BC5_Ag_Vars) 
colnames(BC_Bound) <- c("Group", "Importance")
BC_Ag_Vars <- aggregate(BC_Bound$Importance, list(BC_Bound$Group), FUN = mean)
colnames(BC_Ag_Vars) <- c("Group", "Mean_Importance")
BC_Ag_Vars <- BC_Ag_Vars[sort.list(BC_Ag_Vars$Mean_Importance, decreasing=T),]


#You'll lose one of your columns in that last step. Add it back in here
Class_Map <- Name_List %>%
  group_by(Group) %>%
  summarise(Class = first(Class)) %>%
  ungroup()
BC_Ag_Vars <- BC_Ag_Vars %>%
  left_join(Class_Map, by = "Group")


#Add a column for the region and fill it in
Region <- rep(c("Boreal cordillera (FRUs 40, 41, 43, 45, 46)"), each = 12)
dfreg1 <- data.frame(Region)
BC_Ag_Vars <- cbind(BC_Ag_Vars, dfreg1)


#Select order of classes along the x axis
BC_Ag_Vars$Class <- factor(BC_Ag_Vars$Class, levels = c(
  "(1) Top-down", "(2) Bottom-up (phys. setting)", "(3) Bottom-up (fuels)"))
#Select order of groups in stack
BC_Ag_Vars$Group <- factor(BC_Ag_Vars$Group, levels = c(
  "(1) Day of burn", "(1) Daily fire weather", "(1) Climate anomalies", "(1) Climate normals",  
  "(3) Cutblocks", "(3) Past fire", "(3) Insect disturbance", "(3) Vegetation", 
  "(2) Soils/permafrost", "(2) Water/non-fuels", "(2) Wetlands", "(2) Topography"))


#Build graph and plot
BC_Stacked <- ggplot(data = BC_Ag_Vars, aes(x = Class, y = Mean_Importance, fill = Group)) + 
  geom_bar(stat='identity') +
  scale_fill_manual(breaks = c("(1) Climate anomalies", "(1) Climate normals", "(1) Daily fire weather", "(1) Day of burn", 
                               "(2) Topography", "(2) Wetlands", "(2) Soils/permafrost", "(2) Water/non-fuels",
                               "(3) Vegetation", "(3) Cutblocks", "(3) Past fire", '(3) Insect disturbance'), 
                    values=c('chocolate1', 'darkgoldenrod1', 'maroon','darkorchid',
                             'sienna4','cadetblue1', 'burlywood3','blue',
                             'chartreuse4', 'yellow', 'red', 'azure2')) +
  #labs(fill = "Variable grouping") +
  theme(legend.position="Left") +
  ylab("Importance") +
  xlab("")+
  ylim(0, 60) +
  theme(axis.title.y = element_text(size = rel(1.5)))+
  ggtitle("") +
  facet_wrap(~Region)
plot(BC_Stacked)



####################################################################################################################################
##############################################Plot them together####################################################################
####################################################################################################################################
Bound <- rbind(BST_Ag_Vars, TP_Ag_Vars, TS_Ag_Vars, TC_Ag_Vars, BC_Ag_Vars)

Plot <- ggplot(data = Bound, aes(x = Class, y = Mean_Importance, fill = Group)) + 
  geom_bar(stat='identity') +
  scale_fill_manual(breaks = c("(1) Climate anomalies", "(1) Climate normals", "(1) Daily fire weather", "(1) Day of burn", 
                               "(2) Topography", "(2) Wetlands", "(2) Soils/permafrost", "(2) Water/non-fuels",
                               "(3) Vegetation", "(3) Cutblocks", "(3) Past fire", '(3) Insect disturbance'), 
                    values=c('chocolate1', 'darkgoldenrod1', 'maroon','darkorchid',
                             'sienna4','cadetblue1', 'burlywood3','blue',
                             'chartreuse4', 'yellow', 'red', 'azure2')) +
  labs(fill = "") +
  theme(legend.position="bottom") +
  ylab("Importance") +
  xlab("")+
  ylim(0, 55) +
  ggtitle("") +
  theme(axis.title.x = element_text(size = rel(3)),
        axis.text=element_text(size=14))+
  theme(axis.title.y = element_text(size = rel(3)))+
  facet_wrap(~Region, ncol=2) +
  theme(legend.background = element_blank(),
        legend.box.background = element_rect(colour = "black"),
        legend.text=element_text(size=20), #Legend text size
        strip.text = element_text(size = 15)) #Facet_wrap text
#Plot

png(filename ="Z:/Christine/NWT Fire Refugia Project Z/Graphs/Refugia/V8/Stacked_VIP_V8.png", width = 16, height = 18, units ="in", res=300)
plot(ggarrange(Plot))
dev.off()






