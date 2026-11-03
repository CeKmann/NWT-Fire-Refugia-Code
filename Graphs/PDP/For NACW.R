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
                "Proportion crop (NALCMS) (300 m)", "Proportion crop (NALCMS) (1200 m)","Proportion urban (NALCMS) (1200 m)", "Proportion urban (NALCMS) (300 m)",
                "Proportion herbs (1200 m)", "Proportion herbs (300 m)", "Proportion lichen (1200 m)","Proportion lichen (300 m)", 
                "Proportion rock (SCANFI) (1200 m)", "Proportion rock (SCANFI) (300 m)", "Proportion low shrub (1200 m)", "Proportion low shrub (300 m)", "Proportion tall shrub (1200 m)", "Proportion tall shrub (300 m)",
                "Proportion soil (1200 m)", "Proportion soil (300 m)"))

########################################################################################
#######################################READ IN DF#######################################
########################################################################################

df <- fread("W:/EDM/CCVegMod4/Christine Fire Refugia Projects/NWT Fire Refugia Project/Dataset CSVs/(Step 7) Final/V6/Training_Sample_V6.csv")
df$FRU <- as.factor(df$FRU)
df$ecozone <- as.factor(df$ecozone)



#Specify what variables are wanted
# variables <- c("TPI_300m", "SCANFI_Decid_300m", "SCANFI_Height_300m",  "SCANFI_Closure_300m")
# variables <- c("SCANFI_Decid_300m", "SCANFI_Herbs_300m", "SCANFI_Shrub_Low_300m", "SCANFI_Lichen_300m")
 variables <- c("TPI_300m", "Pontone_Peat_Perma_300m", "Pontone_Poor_Fen_300m", "Pontone_Uplands_300m")




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

FRU_35_df$Pontone_Peat_Perma_300m <- FRU_35_df$Pontone_Peat_Perma_300m * (0.001 / max(FRU_35_df$Pontone_Peat_Perma_300m))


###################Read in models
FRU_26_model <- readRDS("W:/EDM/CCVegMod4/Christine Fire Refugia Projects/NWT Fire Refugia Project/BRT Models/Models/Final/Refugia/V9/FRU_26_plains_Final_V9.rds")
FRU_33_model <- readRDS("W:/EDM/CCVegMod4/Christine Fire Refugia Projects/NWT Fire Refugia Project/BRT Models/Models/Final/Refugia/V9/FRU_33_Final_V9.rds")
FRU_34_model <- readRDS("W:/EDM/CCVegMod4/Christine Fire Refugia Projects/NWT Fire Refugia Project/BRT Models/Models/Final/Refugia/V9/FRU_34_Final_V9.rds")
FRU_35_model <- readRDS("W:/EDM/CCVegMod4/Christine Fire Refugia Projects/NWT Fire Refugia Project/BRT Models/Models/Final/Refugia/V9/FRU_35_Final_V9.rds")


 

#####################Plains FRUs (5):
train_data_plains <- list(FRU_26_df, #Boreal plains & southern taiga plains
                          FRU_33_df, FRU_34_df, FRU_35_df) #plains taiga plains 

models_plains <- list(FRU_26_model, #Boreal plains & southern taiga plains
                      FRU_33_model, FRU_34_model, FRU_35_model) #plains taiga plains

# Assign names to your models for legend purposes
names(models_plains) <- c("Bor.P(26)",
                          "N.Tai.P(33)", "N.Tai.P(34)", "N.Tai.P(35)") 




plains_func <- function(models, train_data, variables) {
  
  plains_colours <- c(
    "#ff6387",
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


plains_pdps <-plains_func(models_plains, train_data_plains, variables)

name_lookup <- setNames(Name_List$Full_Name, Name_List$Variable)


plains_plots_list <- lapply(seq_along(variables), function(i) {
  
  plains_pdps[[variables[[i]]]] +
    labs(
      x = name_lookup[variables[[i]]],   # automatic full name
      y = if (i == 1) "     " else NULL
    ) +
    theme(
      axis.title.y = element_text(size = 7),
      axis.title.x = element_text(size = 18),
      axis.text = element_text(size = 16),
      legend.title = element_text(size = 18),
      legend.text = element_text(size = 16),
      legend.key.size = unit(2, "lines")
    )
  
})

plains_plot <- ggarrange(
  plotlist = plains_plots_list,
  legend = "right",
  common.legend = TRUE,
  ncol =4 ,
  nrow = 1
)
#plot(plains_plot)


#Export
png(filename = "W:/EDM/CCVegMod4/Christine Fire Refugia Projects/NWT Fire Refugia Project/Presentations and Conferences/Workshops etc/NA Caribou Workshop/Images/Graphs/PDP3.png", width = 30, height = 8, units ="in", res=300)
plot(ggarrange(plains_plot))
dev.off()
gc()

