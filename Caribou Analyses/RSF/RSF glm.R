library(terra)
library(questionr)
library(flextable)
library(officer)
library(dplyr)

#Load masked Topography predictions and RSF
Topo_Preds <- rast("W:/EDM/CCVegMod4/Christine Fire Refugia Projects/NWT Fire Refugia Project/Prediction/V6/Predictions/Mosaics/Masked/Masked_NWT_Topo_Median_Mosaic_V6.tif")
RSF <- rast("W:/EDM/CCVegMod4/Christine Fire Refugia Projects/NWT Fire Refugia Project/Data/Final Data/Caribou/RSF/RSF_2017_EarlyWinter_120m.tif")


#Now mask out any pixels in the preds that don't also have pixels in the resampled RSF
Preds_Masked1 <- !is.na(RSF) 
gc()
Preds_Masked <- mask(Topo_Preds, Preds_Masked1, maskvalue=FALSE)
gc()


#Turn the predictions into a df and remove any NA values
pred_df1 <- as.data.frame(Preds_Masked, xy=T)
gc()
gc()
pred_df1 <- na.omit(pred_df1)
gc()
gc()

# Extract RSF values using terra
pred_df1$RSF <- extract(
  RSF,
  pred_df1[, c("x", "y")]
)[, 2]
gc()

# Remove any remaining NAs
pred_df1 <- na.omit(pred_df1)
gc()


#This is just testing out a linear model on the data. When the graph is made, the relationship needs to flip
# (i.e., refugia ~ RSF instead of RSF ~ refugia) so that the bars can be shown vertically instead of horizontally
pred_df1$refugia <- pred_df1$FRU_24_26_shield_Topo_Median_V6
pred_df5 <- pred_df1[pred_df1$refugia >= 0.1553196 & pred_df1$refugia < 0.5, ]
lmcaribou <- lm(pred_df5$RSF ~ pred_df5$refugia)
summary(lmcaribou)

set.seed(123) # Makes sure your sample is reproducible 
pred_df10 <- pred_df5[sample(1:nrow(pred_df5), 50000,
                        replace=FALSE),]
hist(pred_df10$refugia)
hist(pred_df10$RSF)



library(ggplot2)
library(dplyr)
library(mgcv)


pred_df10$RSF <- as.numeric(pred_df10$RSF)

pred_df10$RSF_group <- cut(
  pred_df10$RSF,
  breaks = c(0, 5, 7, 10),
  labels = c("Avoided", "Moderately selected", "Highly selected"),
  include.lowest = TRUE
)

# -----------------------------
# GAM (nonlinear trend)
# -----------------------------
gam_fit <- gam(
  refugia ~ s(RSF),
  data = pred_df10,
  method = "REML"
)

# -----------------------------
# LINEAR MODEL (straight trend)
# -----------------------------
lm_fit <- lm(refugia ~ RSF, data = pred_df10)

# prediction grid
newdat <- data.frame(
  RSF = seq(1, 10, length.out = 200)
)

newdat$gam_pred <- predict(gam_fit, newdata = newdat)
newdat$lm_pred  <- predict(lm_fit, newdata = newdat)

# -----------------------------
# PLOT
# -----------------------------
out <- ggplot(pred_df10, aes(x = RSF, y = refugia, fill = RSF_group)) +
  
  geom_boxplot(aes(group = RSF), outlier.alpha = 0.2) +
  
  # GAM (wavy line)
  geom_line(
    data = newdat,
    aes(x = RSF, y = gam_pred),
    inherit.aes = FALSE,
    color = "black",
    linewidth = 2
  ) +
  
  # Linear trend (straight line)
  geom_line(
    data = newdat,
    aes(x = RSF, y = lm_pred),
    inherit.aes = FALSE,
    color = "red",
    linewidth = 2,
    linetype = "dashed"
  ) +
  
  scale_x_continuous(breaks = 1:10) +
  
  scale_fill_manual(
    values = c(
      "Avoided" = "#8B3A2B",
      "Moderately selected" = "#2E8B57",
      "Highly selected" = "#1F3A93"
    ),
    labels = c(
      "Avoided" = "Avoid",
      "Moderately selected" = "Moderately\nselect",
      "Highly selected" = "Strongly\nselect"
    )
  ) +
  
  labs(
    x = "RSF value",
    y = "Refugia probability",
    fill = "Early winter\nhabitat"
  ) +
  
  theme_classic()

final <- out + theme(
  axis.title = element_text(size = 30),
  axis.text = element_text(size = 25),
  legend.title = element_text(size = 35),
  legend.text = element_text(size = 25),
  legend.key.size = unit(2, "cm")
)
final

png(filename ="W:/EDM/CCVegMod4/Christine Fire Refugia Projects/NWT Fire Refugia Project/Presentations and Conferences/Workshops etc/NA Caribou Workshop/Images/GAMS/EarlyWinter_GAM.png", width = 22, height =14, units ="in", res=300)
plot(final)
dev.off()
gc()

