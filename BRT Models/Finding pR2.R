library(dismo)
library(gbm)


################Find pR2 of the models (just for documentation, not future calcs - those are done in-code)################

FRU_46_model <- readRDS("W:/EDM/CCVegMod4/Christine Fire Refugia Projects/NWT Fire Refugia Project/BRT Models/Models/Final/Refugia/V9/FRU_46_Final_V9.rds")
varimp_FRU_46_model <- as.data.frame(FRU_46_model$contributions) 
cvstats_FRU_46_model <- as.data.frame(FRU_46_model$cv.statistics[c(1,3)])
cvstats_FRU_46_model$deviance.null <- FRU_46_model$self.statistics$mean.null
FRU_46_model_pr2 <- (cvstats_FRU_46_model$deviance.null-cvstats_FRU_46_model$deviance.mean)/cvstats_FRU_46_model$deviance.null
FRU_46_model_pr2
