###########################################################################################################
############################ CREATE 10-FOLD CV MODELS FOR UNCERTAINTY ####################################
###########################################################################################################

library(gbm)
library(dismo)

#Load the models:
# FRU_24_26_model <- readRDS("Z:/Christine/NWT Fire Refugia Project Z/BRT Models/Models/Final/Refugia/V9/FRU_24_26_shield_Final_V9.rds")
# FRU_25_model <- readRDS("Z:/Christine/NWT Fire Refugia Project Z/BRT Models/Models/Final/Refugia/V9/FRU_25_Final_V9.rds")
# FRU_26_model <- readRDS("Z:/Christine/NWT Fire Refugia Project Z/BRT Models/Models/Final/Refugia/V9/FRU_26_plains_Final_V9.rds")
# FRU_31_model <- readRDS("Z:/Christine/NWT Fire Refugia Project Z/BRT Models/Models/Final/Refugia/V9/FRU_31_Final_V9.rds")
# FRU_32_model <- readRDS("Z:/Christine/NWT Fire Refugia Project Z/BRT Models/Models/Final/Refugia/V9/FRU_32_Final_V9.rds")
# FRU_33_model <- readRDS("Z:/Christine/NWT Fire Refugia Project Z/BRT Models/Models/Final/Refugia/V9/FRU_33_Final_V9.rds")
# FRU_34_model <- readRDS("Z:/Christine/NWT Fire Refugia Project Z/BRT Models/Models/Final/Refugia/V9/FRU_34_Final_V9.rds")
# FRU_35_model <- readRDS("Z:/Christine/NWT Fire Refugia Project Z/BRT Models/Models/Final/Refugia/V9/FRU_35_Final_V9.rds")
# FRU_37_38_42_model <- readRDS("Z:/Christine/NWT Fire Refugia Project Z/BRT Models/Models/Final/Refugia/V9/FRU_37_38_42_Final_V9.rds")
# FRU_40_model <- readRDS("Z:/Christine/NWT Fire Refugia Project Z/BRT Models/Models/Final/Refugia/V9/FRU_40_Final_V9.rds")
# FRU_41_model <- readRDS("Z:/Christine/NWT Fire Refugia Project Z/BRT Models/Models/Final/Refugia/V9/FRU_41_Final_V9.rds")
# FRU_43_model <- readRDS("Z:/Christine/NWT Fire Refugia Project Z/BRT Models/Models/Final/Refugia/V9/FRU_43_Final_V9.rds")
FRU_45_model <- readRDS("Z:/Christine/NWT Fire Refugia Project Z/BRT Models/Models/Final/Refugia/V9/FRU_45_Final_V9.rds")
FRU_46_model <- readRDS("Z:/Christine/NWT Fire Refugia Project Z/BRT Models/Models/Final/Refugia/V9/FRU_46_Final_V9.rds")




# This function takes an existing FINAL BRT model and its training data,
# and fits 10 new fold-specific GBMs using the exact same:
# - response variable
# - predictor variables
# - tree complexity
# - learning rate
# - bag fraction
# - number of trees
# - family
#
# These fold models will later be used to estimate uncertainty in the PDPs.

create_cv_models <- function(model, seed = 123) {
  
  set.seed(seed)
  
  # Use the exact training data stored in the final model
  model_data <- model$gbm.call$dataframe
  
  # Get the predictor names and response name
  predictor_names <- model$gbm.call$predictor.names
  response_name <- model$gbm.call$response.name
  
  # Keep only the response and predictors used in the final model
  model_data <- model_data[, c(response_name, predictor_names), drop = FALSE]
  
  # Final model settings
  tree_complexity <- model$gbm.call$tree.complexity
  learning_rate <- model$gbm.call$learning.rate
  bag_fraction <- model$gbm.call$bag.fraction
  n_trees <- model$n.trees
  family <- model$gbm.call$family
  
  # Create a formula using the actual variable names
  model_formula <- as.formula(
    paste(
      response_name,
      "~",
      paste(predictor_names, collapse = " + ")
    )
  )
  
  # Create 10 folds
  n <- nrow(model_data)
  folds <- sample(rep(1:10, length.out = n))
  
  fold_models <- vector("list", 10)
  
  for (k in 1:10) {
    
    cat("Fitting fold", k, "of 10...\n")
    
    # Training data for this fold
    train_fold <- model_data[folds != k, , drop = FALSE]
    
    # Fit fold-specific GBM
    fold_model <- gbm(
      formula = model_formula,
      data = train_fold,
      distribution = family,
      n.trees = n_trees,
      interaction.depth = tree_complexity,
      shrinkage = learning_rate,
      bag.fraction = bag_fraction,
      train.fraction = 1,
      verbose = FALSE
    )
    
    fold_models[[k]] <- fold_model
    
    gc()
  }
  
  names(fold_models) <- paste0("Fold_", 1:10)
  
  return(list(
    fold.models = fold_models,
    fold.vector = folds,
    predictor.names = predictor_names,
    response.name = response_name,
    n.trees = n_trees,
    tree.complexity = tree_complexity,
    learning.rate = learning_rate,
    bag.fraction = bag_fraction,
    family = family
  ))
}


###########################################################################################################
######################################## FIT CV MODELS ####################################################
###########################################################################################################
# FRU_24_26_CV <- create_cv_models(
#   FRU_24_26_model,
#   seed = 123)
#       saveRDS(
#         FRU_24_26_CV,
#         file = "Z:/Christine/NWT Fire Refugia Project Z/BRT Models/Models/Final/Refugia/V9/Folds/CV_Folds_FRU_24_26.rds")
#       gc()
# 
# FRU_25_CV <- create_cv_models(
#   FRU_25_model,
#   seed = 123)
#       saveRDS(
#         FRU_25_CV,
#         file = "Z:/Christine/NWT Fire Refugia Project Z/BRT Models/Models/Final/Refugia/V9/Folds/CV_Folds_FRU_25.rds")
#       gc()

# FRU_26_CV <- create_cv_models(
#   FRU_26_model,
#   seed = 123)
#       saveRDS(
#         FRU_26_CV,
#         file = "Z:/Christine/NWT Fire Refugia Project Z/BRT Models/Models/Final/Refugia/V9/Folds/CV_Folds_FRU_26.rds")
#       gc()
# 
# FRU_31_CV <- create_cv_models(
#   FRU_31_model,
#   seed = 123)
#       saveRDS(
#         FRU_31_CV,
#         file = "Z:/Christine/NWT Fire Refugia Project Z/BRT Models/Models/Final/Refugia/V9/Folds/CV_Folds_FRU_31.rds")
#       gc()

# FRU_32_CV <- create_cv_models(
#   FRU_32_model,
#   seed = 123)
#       saveRDS(
#         FRU_32_CV,
#         file = "Z:/Christine/NWT Fire Refugia Project Z/BRT Models/Models/Final/Refugia/V9/Folds/CV_Folds_FRU_32.rds")
#       gc()
# 
# FRU_33_CV <- create_cv_models(
#   FRU_33_model,
#   seed = 123)
#       saveRDS(
#         FRU_33_CV,
#         file = "Z:/Christine/NWT Fire Refugia Project Z/BRT Models/Models/Final/Refugia/V9/Folds/CV_Folds_FRU_33.rds")
#       gc()

# FRU_34_CV <- create_cv_models(
#   FRU_34_model,
#   seed = 123)
#       saveRDS(
#         FRU_34_CV,
#         file = "Z:/Christine/NWT Fire Refugia Project Z/BRT Models/Models/Final/Refugia/V9/Folds/CV_Folds_FRU_34.rds")
#       gc()
# 
# FRU_35_CV <- create_cv_models(
#   FRU_35_model,
#   seed = 123)
#       saveRDS(
#         FRU_35_CV,
#         file = "Z:/Christine/NWT Fire Refugia Project Z/BRT Models/Models/Final/Refugia/V9/Folds/CV_Folds_FRU_35.rds")
#       gc()

# FRU_37_38_42_CV <- create_cv_models(
#   FRU_37_38_42_model,
#   seed = 123)
#       saveRDS(
#         FRU_37_38_42_CV,
#         file = "Z:/Christine/NWT Fire Refugia Project Z/BRT Models/Models/Final/Refugia/V9/Folds/CV_Folds_FRU_37_38_42.rds")
#       gc()
# 
# FRU_40_CV <- create_cv_models(
#   FRU_40_model,
#   seed = 123)
#       saveRDS(
#         FRU_40_CV,
#         file = "Z:/Christine/NWT Fire Refugia Project Z/BRT Models/Models/Final/Refugia/V9/Folds/CV_Folds_FRU_40.rds")
#       gc()

# FRU_41_CV <- create_cv_models(
#   FRU_41_model,
#   seed = 123)
#       saveRDS(
#         FRU_41_CV,
#         file = "Z:/Christine/NWT Fire Refugia Project Z/BRT Models/Models/Final/Refugia/V9/Folds/CV_Folds_FRU_41.rds")
#       gc()
# 
# FRU_43_CV <- create_cv_models(
#   FRU_43_model,
#   seed = 123)
#       saveRDS(
#         FRU_43_CV,
#         file = "Z:/Christine/NWT Fire Refugia Project Z/BRT Models/Models/Final/Refugia/V9/Folds/CV_Folds_FRU_43.rds")
#       gc()

FRU_45_CV <- create_cv_models(
  FRU_45_model,
  seed = 123)
      saveRDS(
        FRU_45_CV,
        file = "Z:/Christine/NWT Fire Refugia Project Z/BRT Models/Models/Final/Refugia/V9/Folds/CV_Folds_FRU_45.rds")
      gc()

FRU_46_CV <- create_cv_models(
  FRU_46_model,
  seed = 123)
      saveRDS(
        FRU_46_CV,
        file = "Z:/Christine/NWT Fire Refugia Project Z/BRT Models/Models/Final/Refugia/V9/Folds/CV_Folds_FRU_46.rds")
      gc()



