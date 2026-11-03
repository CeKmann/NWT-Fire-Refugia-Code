################################################################################
################################## PACKAGES ####################################
################################################################################

library(data.table)
library(dplyr)
library(gbm)
library(dismo)
library(ggplot2)


################################################################################
################################## NAME LIST ###################################
################################################################################

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
                "Proportion mineral wetlands (1200 m)", "Proportion mineral wetlands (300 m)", "Proportion peat/permafrost complex (1200 m)", "Proportion peat/permafrost complex (300 m)",
                "Proportion poor fen (1200 m)", "Proportion poor fen (300 m)", "Proportion rich fen (1200 m)", "Proportion rich fen (300 m)",
                "Proportion uplands (1200 m)", "Proportion uplands (300 m)",
                "Sand", "Silt", "Slope", "Soil carbon histel", "Soil carbon histo", "Tangential curvature", "Topographic position index (1200 m)", "Topographic position index (300 m)", "TRI",
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


################################################################################
################################## READ IN DF ##################################
################################################################################

df <- fread("Z:/Christine/NWT Fire Refugia Project Z/Dataset CSVs/(Step 7) Final/V6/Training_Sample_V6.csv")
df <- as.data.frame(df)
df$FRU <- as.factor(df$FRU)


################################################################################
################################## VARIABLES ###################################
################################################################################

#Make a list of the top 5 most important variables from the province overall
# variables <- c("Anom_CMI", "Anom_MAT", "Anom_MFST", "Anom_VPD")
# variables <- c("Pontone_Poor_Fen_300m", "Pontone_Rich_Fen_300m", "Pontone_Bogs_300m","Pontone_Min_Wetlands_300m", "Pontone_Peat_Perma_300m", "Pontone_Uplands_300m")
# variables <- c("Fire_1_10_300m", "Fire_10_20_300m", "Fire_20_30_300m", "Insect_0_10_300m", "Insect_10_20_300m")
# variables <- c("TPI_300m", "TPI_1200m", "CTI", "Northness", "Elevation")
# variables <- c("bui", "isi", "rh", "vpd", "DOB")
# variables <- c("SCANFI_Pine_300m", "SCANFI_Other_Con_300m", "SCANFI_Decid_300m", "SCANFI_Closure_300m", "SCANFI_Height_300m")
# variables <- c("SCANFI_Herbs_300m", "SCANFI_Lichen_300m", "SCANFI_Shrub_Low_300m", "SCANFI_Shrub_Tall_300m", "SCANFI_Biomass_300m")
 variables <- c("Water_Occurrence", "Water_Oc_300m", "Water_Oc_1200m")


################################################################################
################################## FINAL MODELS ################################
################################################################################

FRU_24_26_model <- readRDS("Z:/Christine/NWT Fire Refugia Project Z/BRT Models/Models/Final/Refugia/V9/FRU_24_26_shield_Final_V9.rds")

FRU_25_model <- readRDS("Z:/Christine/NWT Fire Refugia Project Z/BRT Models/Models/Final/Refugia/V9/FRU_25_Final_V9.rds")

FRU_26_model <- readRDS("Z:/Christine/NWT Fire Refugia Project Z/BRT Models/Models/Final/Refugia/V9/FRU_26_plains_Final_V9.rds")

FRU_31_model <- readRDS("Z:/Christine/NWT Fire Refugia Project Z/BRT Models/Models/Final/Refugia/V9/FRU_31_Final_V9.rds")

FRU_32_model <- readRDS("Z:/Christine/NWT Fire Refugia Project Z/BRT Models/Models/Final/Refugia/V9/FRU_32_Final_V9.rds")

FRU_33_model <- readRDS("Z:/Christine/NWT Fire Refugia Project Z/BRT Models/Models/Final/Refugia/V9/FRU_33_Final_V9.rds")

FRU_34_model <- readRDS("Z:/Christine/NWT Fire Refugia Project Z/BRT Models/Models/Final/Refugia/V9/FRU_34_Final_V9.rds")

FRU_35_model <- readRDS("Z:/Christine/NWT Fire Refugia Project Z/BRT Models/Models/Final/Refugia/V9/FRU_35_Final_V9.rds")

FRU_37_38_42_model <- readRDS("Z:/Christine/NWT Fire Refugia Project Z/BRT Models/Models/Final/Refugia/V9/FRU_37_38_42_Final_V9.rds")

FRU_40_model <- readRDS("Z:/Christine/NWT Fire Refugia Project Z/BRT Models/Models/Final/Refugia/V9/FRU_40_Final_V9.rds")

FRU_41_model <- readRDS("Z:/Christine/NWT Fire Refugia Project Z/BRT Models/Models/Final/Refugia/V9/FRU_41_Final_V9.rds")

FRU_43_model <- readRDS("Z:/Christine/NWT Fire Refugia Project Z/BRT Models/Models/Final/Refugia/V9/FRU_43_Final_V9.rds")

FRU_45_model <- readRDS("Z:/Christine/NWT Fire Refugia Project Z/BRT Models/Models/Final/Refugia/V9/FRU_45_Final_V9.rds")

FRU_46_model <- readRDS("Z:/Christine/NWT Fire Refugia Project Z/BRT Models/Models/Final/Refugia/V9/FRU_46_Final_V9.rds")


################################################################################
# MODEL LIST
################################################################################

model_list <- list(
  "FRU 40" = FRU_40_model,
  "FRU 41" = FRU_41_model,
  "FRU 43" = FRU_43_model,
  "FRU 45" = FRU_45_model,
  "FRU 46" = FRU_46_model,
  "FRUs 37/38/42" = FRU_37_38_42_model,
  "FRU 26" = FRU_26_model,
  "FRU 31" = FRU_31_model,
  "FRU 32" = FRU_32_model,
  "FRU 33" = FRU_33_model,
  "FRU 34" = FRU_34_model,
  "FRU 35" = FRU_35_model,
  "FRUs 24/26" = FRU_24_26_model,
  "FRU 25" = FRU_25_model
)


################################################################################
################################ CV MODEL PATHS ################################
################################################################################

cv_path_list <- c(
  
  "FRU 40" =
    "Z:/Christine/NWT Fire Refugia Project Z/BRT Models/Models/Final/Refugia/V9/Folds/CV_Folds_FRU_40.rds",
  
  "FRU 41" =
    "Z:/Christine/NWT Fire Refugia Project Z/BRT Models/Models/Final/Refugia/V9/Folds/CV_Folds_FRU_41.rds",
  
  "FRU 43" =
    "Z:/Christine/NWT Fire Refugia Project Z/BRT Models/Models/Final/Refugia/V9/Folds/CV_Folds_FRU_43.rds",
  
  "FRU 45" =
    "Z:/Christine/NWT Fire Refugia Project Z/BRT Models/Models/Final/Refugia/V9/Folds/CV_Folds_FRU_45.rds",
  
  "FRU 46" =
    "Z:/Christine/NWT Fire Refugia Project Z/BRT Models/Models/Final/Refugia/V9/Folds/CV_Folds_FRU_46.rds",
  
  "FRUs 37/38/42" =
    "Z:/Christine/NWT Fire Refugia Project Z/BRT Models/Models/Final/Refugia/V9/Folds/CV_Folds_FRU_37_38_42.rds",
  
  "FRU 26" =
    "Z:/Christine/NWT Fire Refugia Project Z/BRT Models/Models/Final/Refugia/V9/Folds/CV_Folds_FRU_26.rds",
  
  "FRU 31" =
    "Z:/Christine/NWT Fire Refugia Project Z/BRT Models/Models/Final/Refugia/V9/Folds/CV_Folds_FRU_31.rds",
  
  "FRU 32" =
    "Z:/Christine/NWT Fire Refugia Project Z/BRT Models/Models/Final/Refugia/V9/Folds/CV_Folds_FRU_32.rds",
  
  "FRU 33" =
    "Z:/Christine/NWT Fire Refugia Project Z/BRT Models/Models/Final/Refugia/V9/Folds/CV_Folds_FRU_33.rds",
  
  "FRU 34" =
    "Z:/Christine/NWT Fire Refugia Project Z/BRT Models/Models/Final/Refugia/V9/Folds/CV_Folds_FRU_34.rds",
  
  "FRU 35" =
    "Z:/Christine/NWT Fire Refugia Project Z/BRT Models/Models/Final/Refugia/V9/Folds/CV_Folds_FRU_35.rds",
  
  "FRUs 24/26" =
    "Z:/Christine/NWT Fire Refugia Project Z/BRT Models/Models/Final/Refugia/V9/Folds/CV_Folds_FRU_24_26.rds",
  
  "FRU 25" =
    "Z:/Christine/NWT Fire Refugia Project Z/BRT Models/Models/Final/Refugia/V9/Folds/CV_Folds_FRU_25.rds")


################################################################################
############################## READ CV MODELS ##################################
################################################################################

cv_model_list <- lapply(
  cv_path_list,
  readRDS
)
gc()


################################################################################
######################## EXTRACT FINAL MODEL PDPs ##############################
################################################################################

final_pdp_list <- list()

for (model_name in names(model_list)) {
  
  model <- model_list[[model_name]]
  
  for (variable in variables) {
    
    cat(
      "Extracting final PDP: ",
      variable,
      " from ",
      model_name,
      "\n",
      sep = ""
    )
    
    if (!(variable %in% model$gbm.call$predictor.names)) {
      
      cat(
        "   --> SKIPPED: ",
        variable,
        " is not available in ",
        model_name,
        "\n",
        sep = ""
      )
      
      next
    }
    
    pdp <- tryCatch(
      
      plot(
        model,
        i.var = variable,
        return.grid = TRUE,
        n.trees = model$gbm.call$best.trees
      ),
      
      error = function(e) {
        
        cat(
          "   --> SKIPPED: could not extract ",
          variable,
          " from ",
          model_name,
          "\n",
          sep = ""
        )
        
        return(NULL)
      }
    )
    
    if (is.null(pdp)) {
      next
    }
    
    pdp <- as.data.frame(pdp)
    
    names(pdp)[1:2] <- c(
      "x",
      "y"
    )
    
    pdp$Variable <- variable
    pdp$Model <- model_name
    
    final_pdp_list <- append(
      final_pdp_list,
      list(pdp)
    )
    
    names(final_pdp_list)[length(final_pdp_list)] <- paste(
      model_name,
      variable,
      sep = "_"
    )
  }
}
gc()


################################################################################
############################ COMBINE FINAL PDPs ###############################
################################################################################

final_pdp_data <- bind_rows(
  final_pdp_list
)


################################################################################
############################ EXTRACT CV MODEL PDPs ############################
################################################################################

cv_pdp_list <- list()

for (model_name in names(cv_model_list)) {
  
  cat("\n")
  
  cat(
    "============================================================\n"
  )
  
  cat(
    model_name,
    "\n"
  )
  
  cat(
    "============================================================\n"
  )
  
  cv_object <- cv_model_list[[model_name]]
  
  fold_models <- cv_object$fold.models
  
  n_trees <- cv_object$n.trees
  
  
  ##############################################################################
  # LOOP THROUGH VARIABLES
  ##############################################################################
  
  for (variable in variables) {
    
    cat(
      "Extracting CV PDP: ",
      variable,
      "\n",
      sep = ""
    )
    
    if (!(variable %in% cv_object$predictor.names)) {
      
      cat(
        "   --> SKIPPED: ",
        variable,
        " is not in CV model\n",
        sep = ""
      )
      
      next
    }
    
    
    ##########################################################################
    # STORE INDIVIDUAL FOLD PDPs
    ##########################################################################
    
    fold_pdp_list <- list()
    
    
    ##########################################################################
    # LOOP THROUGH FOLDS
    ##########################################################################
    
    for (fold_number in seq_along(fold_models)) {
      
      fold_model <- fold_models[[fold_number]]
      
      fold_pdp <- tryCatch(
        
        plot(
          fold_model,
          i.var = variable,
          return.grid = TRUE,
          n.trees = n_trees
        ),
        
        error = function(e) {
          
          cat(
            "      --> Fold ",
            fold_number,
            " failed\n",
            sep = ""
          )
          
          return(NULL)
        }
      )
      
      if (is.null(fold_pdp)) {
        next
      }
      
      fold_pdp <- as.data.frame(
        fold_pdp
      )
      
      names(fold_pdp)[1:2] <- c(
        "x",
        "y"
      )
      
      fold_pdp$Fold <- fold_number
      
      fold_pdp_list <- append(
        fold_pdp_list,
        list(fold_pdp)
      )
    }
    
    
    ##########################################################################
    # CHECK WHETHER ANY FOLDS WERE SUCCESSFULLY EXTRACTED
    ##########################################################################
    
    if (length(fold_pdp_list) == 0) {
      
      cat(
        "   --> No CV PDPs extracted for ",
        variable,
        "\n",
        sep = ""
      )
      
      next
    }
    
    
    ##########################################################################
    # COMBINE FOLDS
    ##########################################################################
    
    fold_pdp_data <- bind_rows(
      fold_pdp_list
    )
    
    
    ##########################################################################
    # CALCULATE MEAN AND 95% CI ACROSS FOLDS
    ##########################################################################
    
    cv_summary <- fold_pdp_data %>%
      
      group_by(x) %>%
      
      summarise(
        
        mean_y = mean(
          y,
          na.rm = TRUE
        ),
        
        lower_y = quantile(
          y,
          probs = 0.025,
          na.rm = TRUE
        ),
        
        upper_y = quantile(
          y,
          probs = 0.975,
          na.rm = TRUE
        ),
        
        .groups = "drop"
      ) %>%
      
      mutate(
        Variable = variable,
        Model = model_name
      )
    
    
    ##########################################################################
    # STORE CV SUMMARY
    ##########################################################################
    
    cv_pdp_list <- append(
      cv_pdp_list,
      list(cv_summary)
    )
    
    names(cv_pdp_list)[length(cv_pdp_list)] <- paste(
      model_name,
      variable,
      sep = "_"
    )
  }
}
gc()


################################################################################
############################ COMBINE CV PDPs ##################################
################################################################################

cv_pdp_data <- bind_rows(
  cv_pdp_list
)


################################################################################
############################ ADD VARIABLE NAMES ###############################
################################################################################

final_pdp_data <- final_pdp_data %>%
  
  left_join(
    Name_List,
    by = "Variable"
  )


cv_pdp_data <- cv_pdp_data %>%
  
  left_join(
    Name_List,
    by = "Variable"
  )


################################################################################
######################## ORDER VARIABLES FOR PLOTTING ##########################
################################################################################

final_pdp_data$Full_Name <- factor(
  final_pdp_data$Full_Name,
  levels = Name_List$Full_Name[
    match(
      variables,
      Name_List$Variable
    )
  ]
)


cv_pdp_data$Full_Name <- factor(
  cv_pdp_data$Full_Name,
  levels = Name_List$Full_Name[
    match(
      variables,
      Name_List$Variable
    )
  ]
)


################################################################################
######################## CREATE MODEL GROUPS ##################################
################################################################################

model_group_lookup <- tibble(
  Model = c(
    "FRU 40",
    "FRU 41",
    "FRU 43",
    "FRU 45",
    "FRU 46",
    "FRUs 37/38/42",
    "FRU 26",
    "FRU 31",
    "FRU 32",
    "FRU 33",
    "FRU 34",
    "FRU 35",
    "FRUs 24/26",
    "FRU 25"
  ),
  
  Model_Label = c(
    "FRU 40 (C)",
    "FRU 41 (C)",
    "FRU 43 (C)",
    "FRU 45 (C)",
    "FRU 46 (C)",
    "FRUs 37/38/42 (C)",
    "FRU 26 (P)",
    "FRU 31 (P)",
    "FRU 32 (P)",
    "FRU 33 (P)",
    "FRU 34 (P)",
    "FRU 35 (P)",
    "FRUs 24/26 (S)",
    "FRU 25 (S)"
  ),
  
  Model_Group = c(
    rep("Cordillera (C)", 6),
    rep("Plains (P)", 6),
    rep("Shield (S)", 2)
  )
)

################################################################################
# ORDER MODEL GROUPS
################################################################################

model_group_lookup$Model_Group <- factor(
  model_group_lookup$Model_Group,
  levels = c(
    "Cordillera (C)",
    "Plains (P)",
    "Shield (S)"
  )
)

################################################################################
# ORDER MODEL LABELS FOR LEGEND
################################################################################

model_group_lookup$Model_Label <- factor(
  model_group_lookup$Model_Label,
  levels = model_group_lookup$Model_Label
)

gc()

################################################################################
######################## ADD MODEL GROUP TO DATA ##############################
################################################################################

final_pdp_data <- final_pdp_data %>%
  
  left_join(
    model_group_lookup,
    by = "Model"
  )


cv_pdp_data <- cv_pdp_data %>%
  
  left_join(
    model_group_lookup,
    by = "Model"
  )

gc()


################################################################################
####################### PUT FINAL + CV ON COMMON SCALE ########################
################################################################################

for (model_name in names(model_list)) {
  
  for (variable in variables) {
    
    ##########################################################################
    # IDENTIFY ROWS
    ##########################################################################
    
    cv_index <- cv_pdp_data$Model == model_name &
      cv_pdp_data$Variable == variable
    
    final_index <- final_pdp_data$Model == model_name &
      final_pdp_data$Variable == variable
    
    
    ##########################################################################
    # SKIP IF EITHER IS MISSING
    ##########################################################################
    
    if (!any(cv_index) || !any(final_index)) {
      next
    }
    
    
    ##########################################################################
    # CALCULATE REFERENCE VALUE
    ##########################################################################
    
    reference_value <- mean(
      cv_pdp_data$mean_y[cv_index],
      na.rm = TRUE
    )
    
    
    ##########################################################################
    # CENTRE CV MEAN
    ##########################################################################
    
    cv_pdp_data$mean_y[cv_index] <-
      cv_pdp_data$mean_y[cv_index] -
      reference_value
    
    
    ##########################################################################
    # CENTRE CV LOWER CI
    ##########################################################################
    
    cv_pdp_data$lower_y[cv_index] <-
      cv_pdp_data$lower_y[cv_index] -
      reference_value
    
    
    ##########################################################################
    # CENTRE CV UPPER CI
    ##########################################################################
    
    cv_pdp_data$upper_y[cv_index] <-
      cv_pdp_data$upper_y[cv_index] -
      reference_value
    
    
    ##########################################################################
    # CENTRE FINAL MODEL USING EXACT SAME REFERENCE
    ##########################################################################
    
    final_pdp_data$y[final_index] <-
      final_pdp_data$y[final_index] -
      reference_value
  }
}


################################################################################
############################ SMOOTH FINAL PDP ##################################
################################################################################

final_pdp_smooth <- final_pdp_data %>%
  
  group_by(
    Model,
    Variable
  ) %>%
  
  group_modify(
    ~ {
      
      loess_fit <- loess(
        y ~ x,
        data = .x,
        span = 0.75
      )
      
      .x$y_smooth <- predict(
        loess_fit,
        newdata = .x$x
      )
      
      .x
    }
  ) %>%
  
  ungroup()


################################################################################
############################ SMOOTH CV MEAN + CI ###############################
################################################################################

cv_pdp_smooth <- cv_pdp_data %>%
  
  group_by(
    Model,
    Variable
  ) %>%
  
  group_modify(
    ~ {
      
      mean_fit <- loess(
        mean_y ~ x,
        data = .x,
        span = 0.75
      )
      
      lower_fit <- loess(
        lower_y ~ x,
        data = .x,
        span = 0.75
      )
      
      upper_fit <- loess(
        upper_y ~ x,
        data = .x,
        span = 0.75
      )
      
      .x$mean_smooth <- predict(
        mean_fit,
        newdata = .x$x
      )
      
      .x$lower_smooth <- predict(
        lower_fit,
        newdata = .x$x
      )
      
      .x$upper_smooth <- predict(
        upper_fit,
        newdata = .x$x
      )
      
      .x
    }
  ) %>%
  
  ungroup()


################################################################################
######################## FORCE RIBBON TO OVERLAP FINAL LINE ####################
################################################################################

cv_pdp_smooth <- cv_pdp_smooth %>%
  
  group_by(
    Model,
    Variable
  ) %>%
  
  group_modify(
    ~ {
      
      final_data <- final_pdp_smooth %>%
        
        filter(
          Model == .y$Model,
          Variable == .y$Variable
        )
      
      if (nrow(final_data) == 0) {
        return(.x)
      }
      
      final_y_at_cv_x <- approx(
        x = final_data$x,
        y = final_data$y_smooth,
        xout = .x$x,
        rule = 2
      )$y
      
      .x$lower_smooth <- pmin(
        .x$lower_smooth,
        final_y_at_cv_x
      )
      
      .x$upper_smooth <- pmax(
        .x$upper_smooth,
        final_y_at_cv_x
      )
      
      .x
    }
  ) %>%
  
  ungroup()


################################################################################
############################### AXIS LABELS ####################################
################################################################################

zero_label <- function(x) {
  
  labels <- format(
    x,
    trim = TRUE,
    scientific = FALSE
  )
  
  labels[
    abs(x) < .Machine$double.eps^0.5
  ] <- "0"
  
  labels
}
gc()





################################################################################
############################### MAKE THE PLOT ##################################
################################################################################

pdp_plot <- ggplot() +
  
  ##############################################################################
# HORIZONTAL ZERO LINE
##############################################################################

geom_hline(
  yintercept = 0,
  linetype = "dotted",
  linewidth = 0.4
) +
  
  ##############################################################################
# 95% CV CONFIDENCE INTERVAL
##############################################################################

geom_ribbon(
  data = cv_pdp_smooth,
  aes(
    x = x,
    ymin = lower_smooth,
    ymax = upper_smooth,
    fill = Model_Label,
    group = Model
  ),
  alpha = 0.25,
  colour = NA
) +
  
  ##############################################################################
# FINAL MODEL PDP
##############################################################################

geom_line(
  data = final_pdp_smooth,
  aes(
    x = x,
    y = y_smooth,
    colour = Model_Label,
    group = Model
  ),
  linewidth = 0.6
) +
  
  ##############################################################################
# FIVE VARIABLES AS COLUMNS
#
# MODEL GROUPS AS ROWS
#
# Top row:
#   Variable 1  Variable 2  Variable 3  Variable 4  Variable 5
#   FRU 40     FRU 40       FRU 40       FRU 40       FRU 40
#   FRU 41     FRU 41       FRU 41       FRU 41       FRU 41
#   etc.
#
# Middle row:
#   FRU 26, 31, 32, 33, 34, 35
#
# Bottom row:
#   FRUs 24/26, 25
#
##############################################################################

facet_grid(
  Model_Group ~ Full_Name,
  scales = "free_x"
) +
  
  ##############################################################################
# X AXIS
##############################################################################

scale_x_continuous(
  labels = zero_label
) +
  
  ##############################################################################
# LINE COLOURS
##############################################################################

scale_colour_manual(
  values = c(
    
    "FRU 40 (C)" = "red",
    "FRU 41 (C)" = "#4bbf54",
    "FRU 43 (C)" = "#9e75ef",
    "FRU 45 (C)" = "#fe7349",
    "FRU 46 (C)" = "#ba0067",
    "FRUs 37/38/42 (C)" = "deepskyblue2",
    
    "FRU 26 (P)" = "#ff6387",
    "FRU 31 (P)" = "#f0972d",
    "FRU 32 (P)" = "#7e0022",
    "FRU 33 (P)" = "#32145c",
    "FRU 34 (P)" = "#5fb282",
    "FRU 35 (P)" = "#ffade5",
    
    "FRUs 24/26 (S)" = "#a4b21d",
    "FRU 25 (S)" = "#00d0b9"
  ),
  breaks = c(
    "FRU 40 (C)",
    "FRU 41 (C)",
    "FRU 43 (C)",
    "FRU 45 (C)",
    "FRU 46 (C)",
    "FRUs 37/38/42 (C)",
    "FRU 26 (P)",
    "FRU 31 (P)",
    "FRU 32 (P)",
    "FRU 33 (P)",
    "FRU 34 (P)",
    "FRU 35 (P)",
    "FRUs 24/26 (S)",
    "FRU 25 (S)"
  )
) +
  
  ##############################################################################
# RIBBON COLOURS
##############################################################################

scale_fill_manual(
  values = c(
    
    "FRU 40 (C)" = "red",
    "FRU 41 (C)" = "#4bbf54",
    "FRU 43 (C)" = "#9e75ef",
    "FRU 45 (C)" = "#fe7349",
    "FRU 46 (C)" = "#ba0067",
    "FRUs 37/38/42 (C)" = "deepskyblue2",
    
    "FRU 26 (P)" = "#ff6387",
    "FRU 31 (P)" = "#f0972d",
    "FRU 32 (P)" = "#7e0022",
    "FRU 33 (P)" = "#32145c",
    "FRU 34 (P)" = "#5fb282",
    "FRU 35 (P)" = "#ffade5",
    
    "FRUs 24/26 (S)" = "#a4b21d",
    "FRU 25 (S)" = "#00d0b9"
  ),
  breaks = c(
    "FRU 40 (C)",
    "FRU 41 (C)",
    "FRU 43 (C)",
    "FRU 45 (C)",
    "FRU 46 (C)",
    "FRUs 37/38/42 (C)",
    "FRU 26 (P)",
    "FRU 31 (P)",
    "FRU 32 (P)",
    "FRU 33 (P)",
    "FRU 34 (P)",
    "FRU 35 (P)",
    "FRUs 24/26 (S)",
    "FRU 25 (S)"
  )
) +
  
  ##############################################################################
# LABELS
##############################################################################

labs(
  x = "Variable value",
  y = "Refugia probability (partial dependence)",
  colour = "Model",
  fill = "Model"
) +
  
  ##############################################################################
# THEME
##############################################################################

theme_classic() +
  
  theme(
    
    strip.background =
      element_blank(),
    
    strip.text =
      element_text(
        face = "bold",
        size = 10
      ),
    
    axis.title =
      element_text(
        size = 11
      ),
    
    axis.text =
      element_text(
        size = 9
      ),
    
    legend.position =
      "bottom",
    
    legend.title =
      element_text(
        face = "bold"
      ),
    
    panel.spacing =
      unit(
        0.3,
        "lines"
      ),
    
    plot.margin =
      margin(
        10,
        10,
        10,
        10
      )
  ) +
  
  guides(
    colour = guide_legend(
      nrow = 2
    ),
    fill = guide_legend(
      nrow = 2
    )
  )


################################################################################
############################### SAVE PLOT ######################################
################################################################################

ggsave(
  "Z:/Christine/NWT Fire Refugia Project Z/Graphs/Refugia/V8/Water.png",
  plot = pdp_plot,
  width = 14,
  height = 8,
  dpi = 300
)

