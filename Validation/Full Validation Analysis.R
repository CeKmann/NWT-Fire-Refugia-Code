library(dplyr)
library(gbm)
library(cutpointr)
library(data.table)
library(questionr)

#Load the sample and the model
df <- fread("W:/EDM/CCVegMod4/Christine Fire Refugia Projects/NWT Fire Refugia Project/Dataset CSVs/(Step 7) Final/V6/Training_Sample_V6.csv")
df <- as.data.frame(df)
df$FRU <- as.factor(df$FRU)
df$ecozone <- as.factor(df$ecozone)

model <- readRDS("W:/EDM/CCVegMod4/Christine Fire Refugia Projects/NWT Fire Refugia Project/BRT Models/Models/Final/Refugia/V9/FRU_46_Final_V9.rds")


# Conditional truncation
FRU_46 <- df %>%
  filter(FRU == 46) %>%
  droplevels()

set.seed(123) # Makes sure your sample is reproducible 
FRU_46 <- FRU_46[sample(1:nrow(FRU_46), 25000,
                              replace=FALSE),]

# Predict
FRU_46$pred <- predict(model, newdata = FRU_46, type = "response")

# Ensure binary factor
FRU_46$class <- factor(FRU_46$Refugia, levels = c(0, 1))

# Remove NAs if any
FRU_46 <- FRU_46[!is.na(FRU_46$pred), ]

# Optimal threshold
opt_cut <- cutpointr(
  FRU_46,
  x = pred,
  class = class,
  method = maximize_metric,
  metric = youden
)

summary(opt_cut)
gc()

# Extract threshold
threshold <- opt_cut$optimal_cutpoint





# Load validation sample
val_df <- fread("W:/EDM/CCVegMod4/Christine Fire Refugia Projects/NWT Fire Refugia Project/Dataset CSVs/(Step 7) Final/V6/Val_Sample_V6.csv")
val_df <- as.data.frame(val_df)
val_df$FRU <- as.factor(val_df$FRU)
val_df$ecozone <- as.factor(val_df$ecozone)

# Subset to the FRU of interest and year
FRU_46_val <- val_df %>%
  filter(FRU == 46) %>%
  droplevels() %>%
  filter(YEAR == 2024)

# Predict using trained BRT
FRU_46_val$pred_prob <- predict(model, newdata = FRU_46_val, type = "response")

# Remove any rows with NA predictions or Refugia
FRU_46_val <- FRU_46_val[!is.na(FRU_46_val$pred_prob) & !is.na(FRU_46_val$Refugia), ]

# Apply threshold automatically
FRU_46_val$pred_class <- ifelse(FRU_46_val$pred_prob >= threshold, 1, 0)

# Convert to factors
FRU_46_val$Refugia <- factor(FRU_46_val$Refugia, levels = c(0,1))
FRU_46_val$pred_class <- factor(FRU_46_val$pred_class, levels = c(0,1))

# Confusion matrix
Contingency <- table(FRU_46_val$Refugia, FRU_46_val$pred_class)

# Compute validation metrics
TP <- Contingency[2,2]
TN <- Contingency[1,1]
FP <- Contingency[1,2]
FN <- Contingency[2,1]

sensitivity <- TP / (TP + FN)
specificity <- TN / (TN + FP)
TSS <- sensitivity + specificity - 1
accuracy <- (TP + TN) / sum(Contingency)

# Optional: odds ratio
print(Contingency)
#pred_class
#Refugia    0    1
#       0  TN   FP (F = False, T = True)
#       1  FN   TP (N = Negative, P = Positive)


# Create a new table with clearer column names for display
friendly_table <- matrix(
  c(Contingency[1,1], Contingency[1,2],
    Contingency[2,1], Contingency[2,2]),
  nrow = 2,
  byrow = TRUE,
  dimnames = list(
    `Actual` = c("Real Non", "Real Refugia"),
    `Predicted` = c("Pred Non", "Pred Refugia")
  )
)
print(friendly_table)

odds.ratio(Contingency)
