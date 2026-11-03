library(vip)
library(dismo)
library(gbm)
library(ggplot2)
library(questionr)
library(ggpubr)
library(dplyr)
library(gridExtra)
library(cowplot)
library(data.table)
library(tidyr)
theme_set(theme_classic())


#################Workflow##############
##Read in the full sample, subset by FRU
##Read in all models, find each model's pR2 value, multiply the importance of each retained variable in the models by that model's pR2 value
##Combine all variables/their importance into one dataframe, aggregate (mean) by variable name to find that variable's importance throughough region
##Select the top 20 variables within the region, ranked according to importance
##Pull out the data associated with those tiop 20 variables from each FRU subsample
##Calculate the correlation between each of those variables and refugia in each FRU subsample (this is where I begin to generate values for variables not retained across all models)
##Assign the correlation values to its appropriate variable in each FRU
##Assign +-V signs to the correlations based on our chosen threshold values
##Assign proper names for publication and group category to each variable (no "region" category assigned as this version doesn't work for faceting)
##Graph and plot

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
                "Compound topographic index", "Clay", "Coarse", "Eastness", "Elevation", "Direction to large lakes", "Distance to large lakes", "Northness", "Profile curvature", "Permafrost",
                "Proportion urban (Pontone) (1200 m)", "Proportion urban (Pontone) (300 m)", "Proportion agriculture (Pontone) (1200 m)", "Proportion agriculture (Pontone) (300 m)", "Proportion bog (1200 m)", "Proportion bog (300 m)",
                "Proportion mineral wetlands (1200 m)", "Proportion mineral wetlands (300 m)", "Proportion peat/permafrost complex (1200 m)", "Proportion peat/permafrost complex (300 m)",
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
                "Proportion soil (1200 m)", "Proportion soil (300 m)"),
  Group = c("(1) Climate anomalies", "(1) Climate anomalies", "(1) Climate anomalies", "(1) Climate anomalies",
            "(1) Climate normals", "(1) Climate normals", "(1) Climate normals", "(1) Climate normals",
            "(1) Day of burn", '(1) Daily fire weather', '(1) Daily fire weather', '(1) Daily fire weather', '(1) Daily fire weather',
            "(3) Wetlands", "(3) Soils/permafrost", "(3) Soils/permafrost", "(3) Topography", "(3) Topography", "(3) Water/non-fuels", "(3) Water/non-fuels", "(3) Topography", "(3) Topography", "(3) Soils/permafrost",
            "(2) Fuels", "(2) Fuels", "(2) Fuels", "(2) Fuels", "(3) Wetlands", "(3) Wetlands",
            "(3) Wetlands", "(3) Wetlands", "(3) Wetlands", "(3) Wetlands",
            "(3) Wetlands", "(3) Wetlands", "(3) Wetlands", "(3) Wetlands",
            "(3) Wetlands", "(3) Wetlands",       
            "(3) Soils/permafrost", "(3) Soils/permafrost", "(3) Topography", "(3) Soils/permafrost", "(3) Soils/permafrost", "(3) Topography","(3) Topography", "(3) Topography",  "(3) Topography",                        
            "(3) Water/non-fuels", "(3) Water/non-fuels", "(3) Water/non-fuels",
            "(2) Cutblocks", "(2) Cutblocks", "(2) Cutblocks", "(2) Cutblocks",
            "(2) Past fire", "(2) Past fire", "(2) Past fire", "(2) Past fire", "(2) Past fire", "(2) Past fire",
            "(2) Insect disturbance", "(2) Insect disturbance", "(2) Insect disturbance", "(2) Insect disturbance",
            "(2) Fuels", "(2) Fuels", "(2) Fuels", "(2) Fuels", "(2) Fuels",        
            "(2) Fuels", "(2) Fuels", "(2) Fuels", "(2) Fuels", "(2) Fuels",
            "(2) Fuels", "(2) Fuels", "(3) Water/non-fuels", "(3) Water/non-fuels", "(3) Water/non-fuels", "(3) Water/non-fuels",
            "(3) Water/non-fuels", "(3) Water/non-fuels", "(3) Wetlands", "(3) Wetlands", "(3) Wetlands",        
            "(3) Wetlands", "(2) Fuels", "(2) Fuels", "(2) Fuels", "(2) Fuels",
            "(2) Fuels", "(2) Fuels", "(2) Fuels", "(2) Fuels", "(2) Fuels",
            "(2) Fuels", "(2) Fuels", "(2) Fuels", "(2) Fuels", "(2) Fuels",
            "(2) Fuels", "(2) Fuels"),
  Class = c("(1) Top-down", "(1) Top-down", "(1) Top-down", "(1) Top-down",
            "(1) Top-down", "(1) Top-down", "(1) Top-down", "(1) Top-down",
            "(1) Top-down", "(1) Top-down", "(1) Top-down", "(1) Top-down", "(1) Top-down",
            "(3) Bottom-up (phys. setting)", "(3) Bottom-up (phys. setting)", "(3) Bottom-up (phys. setting)", "(3) Bottom-up (phys. setting)", "(3) Bottom-up (phys. setting)", "(3) Bottom-up (phys. setting)", "(3) Bottom-up (phys. setting)", "(3) Bottom-up (phys. setting)", "(3) Bottom-up (phys. setting)", "(3) Bottom-up (phys. setting)",
            "(2) Bottom-up (fuels)", "(2) Bottom-up (fuels)", "(2) Bottom-up (fuels)", "(2) Bottom-up (fuels)", "(3) Bottom-up (phys. setting)", "(3) Bottom-up (phys. setting)",
            "(3) Bottom-up (phys. setting)", "(3) Bottom-up (phys. setting)", "(3) Bottom-up (phys. setting)", "(3) Bottom-up (phys. setting)",
            "(3) Bottom-up (phys. setting)", "(3) Bottom-up (phys. setting)", "(3) Bottom-up (phys. setting)", "(3) Bottom-up (phys. setting)",
            "(3) Bottom-up (phys. setting)", "(3) Bottom-up (phys. setting)",       
            "(3) Bottom-up (phys. setting)", "(3) Bottom-up (phys. setting)", "(3) Bottom-up (phys. setting)", "(3) Bottom-up (phys. setting)", "(3) Bottom-up (phys. setting)", "(3) Bottom-up (phys. setting)","(3) Bottom-up (phys. setting)", "(3) Bottom-up (phys. setting)",  "(3) Bottom-up (phys. setting)",                        
            "(3) Bottom-up (phys. setting)", "(3) Bottom-up (phys. setting)", "(3) Bottom-up (phys. setting)",
            "(2) Bottom-up (fuels)", "(2) Bottom-up (fuels)", "(2) Bottom-up (fuels)", "(2) Bottom-up (fuels)",
            "(2) Bottom-up (fuels)", "(2) Bottom-up (fuels)", "(2) Bottom-up (fuels)", "(2) Bottom-up (fuels)", "(2) Bottom-up (fuels)", "(2) Bottom-up (fuels)",
            "(2) Bottom-up (fuels)", "(2) Bottom-up (fuels)", "(2) Bottom-up (fuels)", "(2) Bottom-up (fuels)",
            "(2) Bottom-up (fuels)", "(2) Bottom-up (fuels)", "(2) Bottom-up (fuels)", "(2) Bottom-up (fuels)", "(2) Bottom-up (fuels)",        
            "(2) Bottom-up (fuels)", "(2) Bottom-up (fuels)", "(2) Bottom-up (fuels)", "(2) Bottom-up (fuels)", "(2) Bottom-up (fuels)",
            "(2) Bottom-up (fuels)", "(2) Bottom-up (fuels)", "(3) Bottom-up (phys. setting)", "(3) Bottom-up (phys. setting)", "(3) Bottom-up (phys. setting)", "(3) Bottom-up (phys. setting)",
            "(3) Bottom-up (phys. setting)", "(3) Bottom-up (phys. setting)", "(3) Bottom-up (phys. setting)", "(3) Bottom-up (phys. setting)", "(3) Bottom-up (phys. setting)",        
            "(3) Bottom-up (phys. setting)", "(2) Bottom-up (fuels)", "(2) Bottom-up (fuels)", "(2) Bottom-up (fuels)", "(2) Bottom-up (fuels)",
            "(2) Bottom-up (fuels)", "(2) Bottom-up (fuels)", "(2) Bottom-up (fuels)", "(2) Bottom-up (fuels)", "(2) Bottom-up (fuels)",
            "(2) Bottom-up (fuels)", "(2) Bottom-up (fuels)", "(2) Bottom-up (fuels)", "(2) Bottom-up (fuels)", "(2) Bottom-up (fuels)",
            "(2) Bottom-up (fuels)", "(2) Bottom-up (fuels)"))


##Read in master dataframe
df <- fread("Z:/Christine/NWT Fire Refugia Project Z/Dataset CSVs/(Step 7) Final/V6/Training_Sample_V6.csv")
df <- as.data.frame(df)
df$FRU <- as.factor(df$FRU)
df$ecozone <- as.factor(df$ecozone)


#######################################################################################################################################
##############################################Boreal/Southern Taiga plains#############################################################
#######################################################################################################################################
FRU_31_df <- df %>%
  filter(FRU == 31) %>%
  droplevels()
set.seed(123) #Needs to be called before every subsample 
FRU_31_df <- FRU_31_df[sample(1:nrow(FRU_31_df), 25000,
                              replace=FALSE),]
FRU_32_df <- df %>%
  filter(FRU == 32) %>%
  droplevels()
set.seed(123) #Needs to be called before every subsample 
FRU_32_df <- FRU_32_df[sample(1:nrow(FRU_32_df), 25000,
                              replace=FALSE),]

###################Read in all models and find the pR2 for each
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


###########Calculate the importance the retained variables in each model and multiply each by the model's pR2
BST1 <- as.data.frame(vi(FRU_31_model))
BST1 <- left_join(Name_List, BST1, by = "Variable")
BST1$Importance[is.na(BST1$Importance)] <- 0
BST1$Importance <- (BST1$Importance)*FRU_31_model_pr2

BST2 <- as.data.frame(vi(FRU_32_model))
BST2 <- left_join(Name_List, BST2, by = "Variable")
BST2$Importance[is.na(BST2$Importance)] <- 0
BST2$Importance <- (BST2$Importance)*FRU_32_model_pr2

#Add identifiers before binding so you can know which values belong where, if you need to check later
BST1$FRU <- 31
BST2$FRU <- 32

BST_Final <- rbind(BST1, BST2)

######################Find the top 15 most important variables for the region as a whole
BST_Ag <- BST_Final %>%
  group_by(Variable) %>%
  summarise(Importance = mean(Importance), .groups = "drop")
BST_15 <- BST_Ag[sort.list(BST_Ag$Importance, decreasing=T),]
BST_15 <- BST_15[1:15,]
BST_15_un <- unique(BST_15$Variable)
BST_15_un


############################For each model, pull out those same variables and find their correlation to get the effect direction in each FRU
FRU_list_BST <- list( #Make a list of the different FRUs in this region
  "31" = FRU_31_df,
  "32" = FRU_32_df)

cor_list_BST <- lapply(names(FRU_list_BST ), function(fru) {
  df_sub <- FRU_list_BST [[fru]]                #For each FRU in that list
  
  cor_vals <- sapply(BST_15_un, function(v) {   #Find the correlations between refugia and the top 15 covariates
    cor(df_sub$Refugia, df_sub[[v]], 
        method = "pearson", 
        use = "complete.obs")
  })
  
  data.frame(                                  #Make sure it ends up being a dataframe with the correct column names
    FRU = fru,
    Variable = names(cor_vals),
    Correlation = cor_vals)
})

BST_Correlations_Final <- bind_rows(cor_list_BST)


#############################Bring in the correlations from each FRU as their own column
BST_cor_wide <- BST_Correlations_Final %>%
  pivot_wider(
    names_from  = FRU,
    values_from = Correlation,
    names_prefix = "Corrls_BST_"
  )
BST_15 <- BST_15 %>%
  left_join(BST_cor_wide, by = "Variable")

threshold <- 0.01  # your specified threshold

BST_15 <- BST_15 %>%
  mutate(
    across(
      starts_with("Corrls_BST_"),          # all correlation columns for this region
      ~ case_when(
        . >= threshold  ~ "+",           # correlation >= 0.01 → "+"
        . <= -threshold ~ "-",           # correlation <= -0.01 → "-"
        TRUE            ~ "v"            # everything else → "v"
      ),
      .names = "signs_{sub('Corrls_BST_', 'BST_', .col)}"  # clean column names: signs_BST_31, signs_BST_32, etc.
    )
  )

BST_15 <- BST_15 %>% #Bring those groups and columns back into it
  left_join(Name_List, by = "Variable")

##################################Add a column for the region and fill it in (used for facet_wrap attempts)
BST_15 <- BST_15 %>% mutate(Region = "Boreal & southern taiga plains (FRUs 31, 32)")
BST_15 <- BST_15[order(BST_15$Importance, decreasing = TRUE),]

BST_15$signs <- paste(BST_15$signs_BST_31, BST_15$signs_BST_32, sep = ",")

BST_VIP <- ggplot(BST_15, aes(x = Importance, y = reorder(Full_Name, Importance), fill=Group)) + 
  scale_fill_manual(breaks = c("(1) Climate anomalies", "(1) Climate normals", "(1) Daily fire weather", "(1) Day of burn", 
                               "(2) Fuels", "(2) Cutblocks", "(2) Past fire", '(2) Insect disturbance',
                               "(3) Topography", "(3) Wetlands", "(3) Soils/permafrost", "(3) Water/non-fuels"), 
                    values=c('chocolate1', 'darkgoldenrod1', 'maroon','darkorchid',
                             'chartreuse4', 'yellow', 'red', 'azure2',
                             'sienna4','cadetblue1', 'burlywood3','blue')) +
  geom_col() +
  geom_text(aes(label = signs), nudge_x = 0.2, size =6) +
  #labs(fill = "Variable grouping") +
  theme(legend.position="none") +
  theme(axis.text=element_text(size=15)) +  
  theme(plot.title = element_text(size=18)) +
  ylab("") +
  xlab("") + xlim(0,3.5) +
  ggtitle("Boreal & southern taiga plains (FRUs 31, 32)")
#plot(BST_VIP)
gc()


#######################################################################################################################
##############################################Taiga plains#############################################################
#######################################################################################################################
FRU_26_df <- df %>%
  filter(FRU == 26, ecozone == 4 | ecozone == 9) %>%
  droplevels()

FRU_33_df <- df %>%
  filter(FRU == 33) %>%
  droplevels()
set.seed(123) #Needs to be called before every subsample 
FRU_33_df <- FRU_33_df[sample(1:nrow(FRU_33_df), 25000,
                              replace=FALSE),]

FRU_34_df <- df %>%
  filter(FRU == 34) %>%
  droplevels()
set.seed(123) #Needs to be called before every subsample 
FRU_34_df <- FRU_34_df[sample(1:nrow(FRU_34_df), 25000,
                              replace=FALSE),]

FRU_35_df <- df %>%
  filter(FRU == 35) %>%
  droplevels()

###################Read in all models and find the pR2 for each
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


###########Calculate the importance the retained variables in each model and multiply each by the model's pR2
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

#Add identifiers before binding so you can know which values belong where, if you need to check later
TP1$FRU <- 26
TP2$FRU <- 33
TP3$FRU <- 34
TP4$FRU <- 35

TP_Final <- rbind(TP1, TP2, TP3, TP4)

######################Find the top 15 most important variables for the region as a whole
TP_Ag <- TP_Final %>%
  group_by(Variable) %>%
  summarise(Importance = mean(Importance), .groups = "drop")
TP_15 <- TP_Ag[sort.list(TP_Ag$Importance, decreasing=T),]
TP_15 <- TP_15[1:15,]
TP_15_un <- unique(TP_15$Variable)
TP_15_un


############################For each model, pull out those same variables and find their correlation to get the effect direction in each FRU
FRU_list_TP <- list( #Make a list of the different FRUs in this region
  "26" = FRU_26_df,
  "33" = FRU_33_df,
  "34" = FRU_34_df,
  "35" = FRU_35_df)

cor_list_TP <- lapply(names(FRU_list_TP ), function(fru) {
  df_sub <- FRU_list_TP [[fru]]                #For each FRU in that list
  
  cor_vals <- sapply(TP_15_un, function(v) {   #Find the correlations between refugia and the top 15 covariates
    cor(df_sub$Refugia, df_sub[[v]], 
        method = "pearson", 
        use = "complete.obs")
  })
  
  data.frame(                                  #Make sure it ends up being a dataframe with the correct column names
    FRU = fru,
    Variable = names(cor_vals),
    Correlation = cor_vals)
})

TP_Correlations_Final <- bind_rows(cor_list_TP)


#############################Bring in the correlations from each FRU as their own column
TP_cor_wide <- TP_Correlations_Final %>%
  pivot_wider(
    names_from  = FRU,
    values_from = Correlation,
    names_prefix = "Corrls_TP_"
  )
TP_15 <- TP_15 %>%
  left_join(TP_cor_wide, by = "Variable")

threshold <- 0.01  # your specified threshold

TP_15 <- TP_15 %>%
  mutate(
    across(
      starts_with("Corrls_TP_"),          # all correlation columns for this region
      ~ case_when(
        . >= threshold  ~ "+",           # correlation >= 0.01 → "+"
        . <= -threshold ~ "-",           # correlation <= -0.01 → "-"
        TRUE            ~ "v"            # everything else → "v"
      ),
      .names = "signs_{sub('Corrls_TP_', 'TP_', .col)}"  # clean column names: signs_TP_26, signs_TP_33, etc.
    )
  )

TP_15 <- TP_15 %>% #Bring those groups and columns back into it
  left_join(Name_List, by = "Variable")

##################################Add a column for the region and fill it in (used for facet_wrap attempts)
TP_15 <- TP_15 %>% mutate(Region = "Taiga plains (FRUs 26, 33, 34, 35)")
TP_15 <- TP_15[order(TP_15$Importance, decreasing = TRUE),]

TP_15$signs <- paste(TP_15$signs_TP_26, TP_15$signs_TP_33, TP_15$signs_TP_34, TP_15$signs_TP_35, sep = ",")

TP_VIP <- ggplot(TP_15, aes(x = Importance, y = reorder(Full_Name, Importance), fill=Group)) + 
  scale_fill_manual(breaks = c("(1) Climate anomalies", "(1) Climate normals", "(1) Daily fire weather", "(1) Day of burn", 
                               "(2) Fuels", "(2) Cutblocks", "(2) Past fire", '(2) Insect disturbance',
                               "(3) Topography", "(3) Wetlands", "(3) Soils/permafrost", "(3) Water/non-fuels"), 
                    values=c('chocolate1', 'darkgoldenrod1', 'maroon','darkorchid',
                             'chartreuse4', 'yellow', 'red', 'azure2',
                             'sienna4','cadetblue1', 'burlywood3','blue')) +
  geom_col() +
  geom_text(aes(label = signs), nudge_x = 0.45, size =6) +
  #labs(fill = "Variable grouping") +
  theme(legend.position="none") +
  theme(axis.text=element_text(size=15)) +  
  theme(plot.title = element_text(size=18)) +
  ylab("") +
  xlab("") + xlim(0,3.5) +
  ggtitle("Taiga plains (FRUs 26, 33, 34, 35)")
#plot(TP_VIP)
gc()



#######################################################################################################################
##############################################Taiga shield#############################################################
#######################################################################################################################
FRU_24_26_df <- df %>%
  filter(FRU == 24 | (FRU == 26 & (ecozone == 5 | ecozone == 6))) %>%
  droplevels()
set.seed(123) # Makes sure your sample is reproducible 
FRU_24_26_df <- FRU_24_26_df[sample(1:nrow(FRU_24_26_df), 25000,
                              replace=FALSE),]

FRU_25_df <- df %>%
  filter(FRU == 25) %>%
  droplevels()
set.seed(123) #Needs to be called before every subsample 
FRU_25_df <- FRU_25_df[sample(1:nrow(FRU_25_df), 25000,
                              replace=FALSE),]


###################Read in all models and find the pR2 for each
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



###########Calculate the importance the retained variables in each model and multiply each by the model's pR2
TS1 <- as.data.frame(vi(FRU_24_26_model))
TS1 <- left_join(Name_List, TS1, by = "Variable")
TS1$Importance[is.na(TS1$Importance)] <- 0
TS1$Importance <- (TS1$Importance)*FRU_24_26_model_pr2

TS2 <- as.data.frame(vi(FRU_25_model))
TS2 <- left_join(Name_List, TS2, by = "Variable")
TS2$Importance[is.na(TS2$Importance)] <- 0
TS2$Importance <- (TS2$Importance)*FRU_25_model_pr2

#Add identifiers before binding so you can know which values belong where, if you need to check later
TS1$FRU <- "24_26"
TS2$FRU <- 25

TS_Final <- rbind(TS1, TS2)

######################Find the top 15 most important variables for the region as a whole
TS_Ag <- TS_Final %>%
  group_by(Variable) %>%
  summarise(Importance = mean(Importance), .groups = "drop")
TS_15 <- TS_Ag[sort.list(TS_Ag$Importance, decreasing=T),]
TS_15 <- TS_15[1:15,]
TS_15_un <- unique(TS_15$Variable)
TS_15_un


############################For each model, pull out those same variables and find their correlation to get the effect direction in each FRU
FRU_list_TS <- list( #Make a list of the different FRUs in this region
  "24_26" = FRU_24_26_df,
  "25" = FRU_25_df)

cor_list_TS <- lapply(names(FRU_list_TS ), function(fru) {
  df_sub <- FRU_list_TS [[fru]]                #For each FRU in that list
  
  cor_vals <- sapply(TS_15_un, function(v) {   #Find the correlations between refugia and the top 15 covariates
    cor(df_sub$Refugia, df_sub[[v]], 
        method = "pearson", 
        use = "complete.obs")
  })
  
  data.frame(                                  #Make sure it ends up being a dataframe with the correct column names
    FRU = fru,
    Variable = names(cor_vals),
    Correlation = cor_vals)
})

TS_Correlations_Final <- bind_rows(cor_list_TS)


#############################Bring in the correlations from each FRU as their own column
TS_cor_wide <- TS_Correlations_Final %>%
  pivot_wider(
    names_from  = FRU,
    values_from = Correlation,
    names_prefix = "Corrls_TS_"
  )
TS_15 <- TS_15 %>%
  left_join(TS_cor_wide, by = "Variable")

threshold <- 0.01  # your specified threshold

TS_15 <- TS_15 %>%
  mutate(
    across(
      starts_with("Corrls_TS_"),          # all correlation columns for this region
      ~ case_when(
        . >= threshold  ~ "+",           # correlation >= 0.01 → "+"
        . <= -threshold ~ "-",           # correlation <= -0.01 → "-"
        TRUE            ~ "v"            # everything else → "v"
      ),
      .names = "signs_{sub('Corrls_TS_', 'TS_', .col)}"  # clean column names: signs_TS_24_26, signs_TS_25, etc.
    )
  )

TS_15 <- TS_15 %>% #Bring those groups and columns back into it
  left_join(Name_List, by = "Variable")

##################################Add a column for the region and fill it in (used for facet_wrap attempts)
TS_15 <- TS_15 %>% mutate(Region = "Taiga shield (FRUs 24/26, 25)")
TS_15 <- TS_15[order(TS_15$Importance, decreasing = TRUE),]

TS_15$signs <- paste(TS_15$signs_TS_24_26, TS_15$signs_TS_25, sep = ",")

TS_VIP <- ggplot(TS_15, aes(x = Importance, y = reorder(Full_Name, Importance), fill=Group)) + 
  scale_fill_manual(breaks = c("(1) Climate anomalies", "(1) Climate normals", "(1) Daily fire weather", "(1) Day of burn", 
                               "(2) Fuels", "(2) Cutblocks", "(2) Past fire", '(2) Insect disturbance',
                               "(3) Topography", "(3) Wetlands", "(3) Soils/permafrost", "(3) Water/non-fuels"), 
                    values=c('chocolate1', 'darkgoldenrod1', 'maroon','darkorchid',
                             'chartreuse4', 'yellow', 'red', 'azure2',
                             'sienna4','cadetblue1', 'burlywood3','blue')) +
  geom_col() +
  geom_text(aes(label = signs), nudge_x = 0.2, size =6) +
  #labs(fill = "Variable grouping") +
  theme(legend.position="none") +
  theme(axis.text=element_text(size=15)) +  
  theme(plot.title = element_text(size=18)) +
  ylab("") +
  xlab("") + xlim(0,3.5) +
  ggtitle("Taiga shield (FRUs 24/26, 25)")
plot(TS_VIP)
gc()



#######################################################################################################################
##############################################Boreal cordillera########################################################
#######################################################################################################################
FRU_40_df <- df %>%
  filter(FRU == 40) %>%
  droplevels()
set.seed(123) #Needs to be called before every subsample 
FRU_40_df <- FRU_40_df[sample(1:nrow(FRU_40_df), 25000,
                              replace=FALSE),]

FRU_41_df <- df %>%
  filter(FRU == 41) %>%
  droplevels()
set.seed(123) #Needs to be called before every subsample 
FRU_41_df <- FRU_41_df[sample(1:nrow(FRU_41_df), 25000,
                              replace=FALSE),]

FRU_43_df <- df %>%
  filter(FRU == 43) %>%
  droplevels()

FRU_45_df <- df %>%
  filter(FRU == 45) %>%
  droplevels()
set.seed(123) #Needs to be called before every subsample 
FRU_45_df <- FRU_45_df[sample(1:nrow(FRU_45_df), 25000,
                              replace=FALSE),]

FRU_46_df <- df %>%
  filter(FRU == 46) %>%
  droplevels()

###################Read in all models and find the pR2 for each
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


###########Calculate the importance the retained variables in each model and multiply each by the model's pR2
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

#Add identifiers before binding so you can know which values belong where, if you need to check later
BC1$FRU <- 40
BC2$FRU <- 41
BC3$FRU <- 43
BC4$FRU <- 45
BC5$FRU <- 46

BC_Final <- rbind(BC1, BC2, BC3, BC4, BC5)

######################Find the top 15 most important variables for the region as a whole
BC_Ag <- BC_Final %>%
  group_by(Variable) %>%
  summarise(Importance = mean(Importance), .groups = "drop")
BC_15 <- BC_Ag[sort.list(BC_Ag$Importance, decreasing=T),]
BC_15 <- BC_15[1:15,]
BC_15_un <- unique(BC_15$Variable)
BC_15_un


############################For each model, pull out those same variables and find their correlation to get the effect direction in each FRU
FRU_list_BC <- list( #Make a list of the different FRUs in this region
  "40" = FRU_40_df,
  "41" = FRU_41_df,
  "43" = FRU_43_df,
  "45" = FRU_45_df,
  "46" = FRU_46_df)

cor_list_BC <- lapply(names(FRU_list_BC ), function(fru) {
  df_sub <- FRU_list_BC [[fru]]                #For each FRU in that list
  
  cor_vals <- sapply(BC_15_un, function(v) {   #Find the correlations between refugia and the top 15 covariates
    cor(df_sub$Refugia, df_sub[[v]], 
        method = "pearson", 
        use = "complete.obs")
  })
  
  data.frame(                                  #Make sure it ends up being a dataframe with the correct column names
    FRU = fru,
    Variable = names(cor_vals),
    Correlation = cor_vals)
})

BC_Correlations_Final <- bind_rows(cor_list_BC)


#############################Bring in the correlations from each FRU as their own column
BC_cor_wide <- BC_Correlations_Final %>%
  pivot_wider(
    names_from  = FRU,
    values_from = Correlation,
    names_prefix = "Corrls_BC_"
  )
BC_15 <- BC_15 %>%
  left_join(BC_cor_wide, by = "Variable")

threshold <- 0.01  # your specified threshold

BC_15 <- BC_15 %>%
  mutate(
    across(
      starts_with("Corrls_BC_"),          # all correlation columns for this region
      ~ case_when(
        . >= threshold  ~ "+",           # correlation >= 0.01 → "+"
        . <= -threshold ~ "-",           # correlation <= -0.01 → "-"
        TRUE            ~ "v"            # everything else → "v"
      ),
      .names = "signs_{sub('Corrls_BC_', 'BC_', .col)}"  # clean column names: signs_BC_40, signs_BC_41, eBC.
    )
  )

BC_15 <- BC_15 %>% #Bring those groups and columns back into it
  left_join(Name_List, by = "Variable")

##################################Add a column for the region and fill it in (used for facet_wrap attempts)
BC_15 <- BC_15 %>% mutate(Region = "Boreal cordillera (FRUs 40, 41, 43, 45, 46)")
BC_15 <- BC_15[order(BC_15$Importance, decreasing = TRUE),]

BC_15$signs <- paste(BC_15$signs_BC_40, BC_15$signs_BC_41, BC_15$signs_BC_43, BC_15$signs_BC_45, BC_15$signs_BC_46, sep = ",")

BC_VIP <- ggplot(BC_15, aes(x = Importance, y = reorder(Full_Name, Importance), fill=Group)) + 
  scale_fill_manual(breaks = c("(1) Climate anomalies", "(1) Climate normals", "(1) Daily fire weather", "(1) Day of burn", 
                               "(2) Fuels", "(2) Cutblocks", "(2) Past fire", '(2) Insect disturbance',
                               "(3) Topography", "(3) Wetlands", "(3) Soils/permafrost", "(3) Water/non-fuels"), 
                    values=c('chocolate1', 'darkgoldenrod1', 'maroon','darkorchid',
                             'chartreuse4', 'yellow', 'red', 'azure2',
                             'sienna4','cadetblue1', 'burlywood3','blue')) +
  geom_col() +
  geom_text(aes(label = signs), nudge_x = 0.45, size =6) +
  #labs(fill = "Variable grouping") +
  theme(legend.position="none") +
  theme(axis.text=element_text(size=15)) +  
  theme(plot.title = element_text(size=18)) +
  ylab("") +
  xlab("") + xlim(0,3.5) +
  ggtitle("Boreal cordillera (FRUs 40, 41, 43, 45, 46)")
#plot(BC_VIP)
gc()



#######################################################################################################################
##############################################Taiga cordillera#########################################################
#######################################################################################################################
FRU_37_38_42_df <- df %>%
  filter(FRU == 37 | FRU == 38 | FRU == 42) %>%
  droplevels()
set.seed(123) #Needs to be called before every subsample 
FRU_37_38_42_df <- FRU_37_38_42_df[sample(1:nrow(FRU_37_38_42_df), 25000,
                                          replace=FALSE),]

###################Read in all models and find the pR2 for each
FRU_37_38_42_model <- readRDS("Z:/Christine/NWT Fire Refugia Project Z/BRT Models/Models/Final/Refugia/V9/FRU_37_38_42_Final_V9.rds")
varimp_FRU_37_38_42_model <- as.data.frame(FRU_37_38_42_model$contributions) 
cvstats_FRU_37_38_42_model <- as.data.frame(FRU_37_38_42_model$cv.statistics[c(1,3)])
cvstats_FRU_37_38_42_model$deviance.null <- FRU_37_38_42_model$self.statistics$mean.null
FRU_37_38_42_model_pr2 <- (cvstats_FRU_37_38_42_model$deviance.null-cvstats_FRU_37_38_42_model$deviance.mean)/cvstats_FRU_37_38_42_model$deviance.null


###########Calculate the importance the retained variables in each model and multiply each by the model's pR2
TC1 <- as.data.frame(vi(FRU_37_38_42_model))
TC1 <- left_join(Name_List, TC1, by = "Variable")
TC1$Importance[is.na(TC1$Importance)] <- 0
TC1$Importance <- (TC1$Importance)*FRU_37_38_42_model_pr2

#Add identifiers before binding so you can know which values belong where, if you need to check later
TC1$FRU <- '37_38_42'

######################Find the top 15 most important variables for the region as a whole
TC_Ag <- TC1 %>%
  group_by(Variable) %>%
  summarise(Importance = mean(Importance), .groups = "drop")
TC_15 <- TC_Ag[sort.list(TC_Ag$Importance, decreasing=T),]
TC_15 <- TC_15[1:15,]
TC_15_un <- unique(TC_15$Variable)
TC_15_un

############################For each model, pull out those same variables and find their correlation to get the effect direction in each FRU
FRU_list_TC <- list( #Make a list of the different FRUs in this region
  "37_38_42" = FRU_37_38_42_df)

cor_list_TC <- lapply(names(FRU_list_TC ), function(fru) {
  df_sub <- FRU_list_TC [[fru]]                #For each FRU in that list
  
  cor_vals <- sapply(TC_15_un, function(v) {   #Find the correlations between refugia and the top 15 covariates
    cor(df_sub$Refugia, df_sub[[v]], 
        method = "pearson", 
        use = "complete.obs")
  })
  
  data.frame(                                  #Make sure it ends up being a dataframe with the correct column names
    FRU = fru,
    Variable = names(cor_vals),
    Correlation = cor_vals)
})

TC_Correlations_Final <- bind_rows(cor_list_TC)


#############################Bring in the correlations from each FRU as their own column
TC_cor_wide <- TC_Correlations_Final %>%
  pivot_wider(
    names_from  = FRU,
    values_from = Correlation,
    names_prefix = "Corrls_TC_"
  )
TC_15 <- TC_15 %>%
  left_join(TC_cor_wide, by = "Variable")

threshold <- 0.01  # your specified threshold

TC_15 <- TC_15 %>%
  mutate(
    across(
      starts_with("Corrls_TC_"),          # all correlation columns for this region
      ~ case_when(
        . >= threshold  ~ "+",           # correlation >= 0.01 → "+"
        . <= -threshold ~ "-",           # correlation <= -0.01 → "-"
        TRUE            ~ "v"            # everything else → "v"
      ),
      .names = "signs_{sub('Corrls_TC_', 'TC_', .col)}"  # clean column names: signs_TC_37_38_42, signs_TC_41, eTC.
    )
  )

TC_15 <- TC_15 %>% #Bring those groups and columns back into it
  left_join(Name_List, by = "Variable")

##################################Add a column for the region and fill it in (used for facet_wrap attempts)
TC_15 <- TC_15 %>% mutate(Region = "Taiga cordillera (FRUs 37/38/42)")
TC_15 <- TC_15[order(TC_15$Importance, decreasing = TRUE),]

TC_15$signs <- paste(TC_15$signs_TC_37_38_42, sep = ",")

TC_VIP <- ggplot(TC_15, aes(x = Importance, y = reorder(Full_Name, Importance), fill=Group)) + 
  scale_fill_manual(breaks = c("(1) Climate anomalies", "(1) Climate normals", "(1) Daily fire weather", "(1) Day of burn", 
                               "(2) Fuels", "(2) Cutblocks", "(2) Past fire", '(2) Insect disturbance',
                               "(3) Topography", "(3) Wetlands", "(3) Soils/permafrost", "(3) Water/non-fuels"), 
                    values=c('chocolate1', 'darkgoldenrod1', 'maroon','darkorchid',
                             'chartreuse4', 'yellow', 'red', 'azure2',
                             'sienna4','cadetblue1', 'burlywood3','blue')) +
  geom_col() +
  geom_text(aes(label = signs), nudge_x = 0.1, size =6) +
  #labs(fill = "Variable grouping") +
  theme(legend.position="none") +
  theme(axis.text=element_text(size=15)) +  
  theme(plot.title = element_text(size=18)) +
  ylab("") +
  xlab("") + xlim(0,3.5) +
  ggtitle("Taiga cordillera (FRUs 37/38/42)")
#plot(TC_VIP)
gc()





##############################################################################################################################################
###########################################################Make the big graph#################################################################
##############################################################################################################################################
#Solution to making this graph came from here: https://www.r-bloggers.com/2019/05/the-small-multiples-plot-how-to-combine-ggplot2-plots-with-one-shared-axis/
Graphs <- cowplot::plot_grid(BC_VIP + theme(axis.text.x = element_blank(),
                                            axis.ticks.x = element_blank(),
                                            axis.title.x = element_blank()), 
                             BST_VIP + theme(axis.text.x = element_blank(),
                                             axis.ticks.x = element_blank(),
                                             axis.title.x = element_blank()), 
                             TP_VIP + theme(axis.text.x = element_blank(),
                                             axis.ticks.x = element_blank(),
                                             axis.title.x = element_blank()), 
                             TC_VIP + theme(axis.text.x = element_blank(),
                                            axis.ticks.x = element_blank(),
                                            axis.title.x = element_blank()), 
                             TS_VIP,
                             nrow = 3,
                             labels = "")
png(filename ="Z:/Christine/NWT Fire Refugia Project Z/Graphs/Refugia/V8/Top15_V8.png", width = 18, height = 20, units ="in", res=300)
plot(ggarrange(Graphs))
dev.off()
gc()

