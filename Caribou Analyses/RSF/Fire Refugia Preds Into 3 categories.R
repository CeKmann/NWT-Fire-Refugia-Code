library(terra)
library(questionr)

#Load Unmasked fuelsgraphy predictions and RSF
fuels_Preds <- rast("Z:/Christine/NWT Fire Refugia Project Z/Prediction/V6/Predictions/Mosaics/Masked_Fuels_RSF_Median_Mosaic_V6.tif")
RSF <- rast("Z:/Christine/NWT Fire Refugia Project Z/Data/Final Data/Caribou/RSF/2020/RSF_2020_Late winter_120m.tif")

#Now mask out any pixels in the preds that don't also have pixels in the resampled RSF
Preds_Unmasked1 <- !is.na(RSF) 
gc()
Preds_Unmasked <- mask(fuels_Preds, Preds_Unmasked1, maskvalue=FALSE)
gc()


#Turn the predictions into a df and remove any NA values
pred_df1 <- as.data.frame(Preds_Unmasked, xy=T)
gc()
gc()
pred_df1 <- na.omit(pred_df1)
gc()
gc()

#Extract the RSF values
pred_df1$RSF <- extract(
  RSF,
  pred_df1[, c("x", "y")]
)[, 2]
gc()


#Change the name of the column for preds and get rid of coords
pred_df<- pred_df1[-c(1,2)] #Remove coords
names(pred_df)[1] <- "Preds"
gc()
pred_df$RSF <- as.numeric(pred_df$RSF)

#Make 3 new columns to use in next steps and get rid of the original one  
pred_df$Preds_Low <- pred_df$Preds
pred_df$Preds_Moderate <- pred_df$Preds
pred_df$Preds_High <- pred_df$Preds
pred_df <- pred_df[-c(1)]

#Do the same with the RSF one
pred_df$RSF_Low <- pred_df$RSF
pred_df$RSF_Moderate <- pred_df$RSF
pred_df$RSF_High <- pred_df$RSF
pred_df <- pred_df[-c(1)]

#Next, turn the RSF into a binary and the predictions into 3 tiers of low, Moderate, high probability
#Low probability is below optimal, Moderate is optimal-0.50, high is 0.51 and above
#RSF values from 6-10 are now 1 and 1-5 are now 0 (i.e., everything selected vs everything avoided)

####LOW####
pred_df$Preds_Low_bin <- ifelse(pred_df$Preds_Low < 0.1552978, 1, 0)
pred_df$RSF_Low_bin <- ifelse(pred_df$RSF_Low >= 6, 1, 0)  # RSF values >=6 are zero, everything below is 1


#To check if RSF really are just 0 and 1 after running the code, use this:
unique(pred_df$Preds_Low_bin)
unique(pred_df$RSF_Low_bin)

#Factor categories for cross-tab
pred_df$RSF_Low_cat <- ifelse(pred_df$RSF_Low_bin == 0, "Avoided", "Selected")
pred_df$Preds_Low_cat <- ifelse(pred_df$Preds_Low_bin == 0, "Other", "Low")
  #NOTE!! This and following naming code is different from the version of preds/RSF that has 3 RSF classes to ensure that I'm comparing the proper categories (i.e., low vs selected)
  pred_df$RSF_Low_cat <- factor(pred_df$RSF_Low_cat, levels = c("Avoided", "Selected"))
  pred_df$Preds_Low_cat <- factor(pred_df$Preds_Low_cat, levels = c("Other", "Low"))
  gc()

#If running this checks code, will need to update it to match current column names
# #Do the number of non and refugia match the number of 0 and 1 in another column? (Harder to do with RSF, just checking code here)
sum(pred_df$Preds_Low_bin == 0, na.rm = TRUE)
sum(pred_df$Preds_Low_cat == "Other", na.rm = TRUE) #These match
sum(pred_df$Preds_Low_bin == 1, na.rm = TRUE)
sum(pred_df$Preds_Low_cat == "Low", na.rm = TRUE) #These match

#To look at Late the associated columns and make sure they make sense:
check1 <- pred_df[c("Preds_Low", "Preds_Low_bin", "Preds_Low_cat",
                    "RSF_Low", "RSF_Low_bin", "RSF_Low_cat")]

Low_Table <- table(pred_df$Preds_Low_cat, pred_df$RSF_Low_cat)
Low_Table
odds.ratio(Low_Table)
gc()


####Moderate####
pred_df$Preds_Moderate_bin <- ifelse(pred_df$Preds_Moderate >= 0.1552978 & pred_df$Preds_Moderate <= 0.5, 1, 0)
pred_df$RSF_Moderate_bin <- ifelse(pred_df$RSF_Moderate >= 6, 1, 0)  # RSF values >=6 are zero, everything below is 1


#To check if Late really are just 0 and 1 after running the code, use this:
unique(pred_df$Preds_Moderate_bin)
unique(pred_df$RSF_Moderate_bin)

#Factor categories for cross-tab
pred_df$RSF_Moderate_cat <- ifelse(pred_df$RSF_Moderate_bin == 0, "Avoided", "Selected")
pred_df$Preds_Moderate_cat <- ifelse(pred_df$Preds_Moderate_bin == 0, "Other", "Moderate")
  pred_df$RSF_Moderate_cat <- factor(pred_df$RSF_Moderate_cat, levels = c("Avoided", "Selected"))
  pred_df$Preds_Moderate_cat <- factor(pred_df$Preds_Moderate_cat, levels = c("Other", "Moderate"))
  gc()

#If running this checks code, will need to update it to match current column names
# #Do the number of non and refugia match the number of 0 and 1 in another column? (Harder to do with RSF, just checking code here)
sum(pred_df$Preds_Moderate_bin == 0, na.rm = TRUE)
sum(pred_df$Preds_Moderate_cat == "Other", na.rm = TRUE) #These match
sum(pred_df$Preds_Moderate_bin == 1, na.rm = TRUE)
sum(pred_df$Preds_Moderate_cat == "Moderate", na.rm = TRUE) #These match

#To look at Late the associated columns and make sure they make sense:
check1 <- pred_df[c("Preds_Moderate", "Preds_Moderate_bin", "Preds_Moderate_cat",
                    "RSF_Moderate", "RSF_Moderate_bin", "RSF_Moderate_cat")]

Moderate_Table <- table(pred_df$Preds_Moderate_cat, pred_df$RSF_Moderate_cat)
Moderate_Table
odds.ratio(Moderate_Table)
gc()


####HIGH####
pred_df$Preds_High_bin <- ifelse(pred_df$Preds_High > 0.5, 1, 0)
pred_df$RSF_High_bin <- ifelse(pred_df$RSF_High >= 6, 1, 0)  # Late >=6 zero, Late else one


#To check if Late reLatey are just 0 and 1 after running the code, use this:
unique(pred_df$Preds_High_bin)
unique(pred_df$RSF_High_bin)

#Factor categories for cross-tab
pred_df$RSF_High_cat <- ifelse(pred_df$RSF_High_bin == 0, "Avoided", "Selected")
pred_df$Preds_High_cat <- ifelse(pred_df$Preds_High_bin == 0, "Other", "High")
pred_df$RSF_High_cat <- factor(pred_df$RSF_High_cat, levels = c("Avoided", "Selected"))
pred_df$Preds_High_cat <- factor(pred_df$Preds_High_cat, levels = c("Other", "High"))
gc()

#If running this checks code, will need to update it to match current column names
# #Do the number of non and refugia match the number of 0 and 1 in another column? (Harder to do with RSF, just checking code here)
sum(pred_df$Preds_High_bin == 0, na.rm = TRUE)
sum(pred_df$Preds_High_cat == "Other", na.rm = TRUE) #These match
sum(pred_df$Preds_High_bin == 1, na.rm = TRUE)
sum(pred_df$Preds_High_cat == "High", na.rm = TRUE) #These match

#To look at Late the associated columns and make sure they make sense:
check1 <- pred_df[c("Preds_High", "Preds_High_bin", "Preds_High_cat",
                    "RSF_High", "RSF_High_bin", "RSF_High_cat")]

High_Table <- table(pred_df$Preds_High_cat, pred_df$RSF_High_cat)
High_Table
odds.ratio(High_Table)
gc()



#Finally, make plots of the proportion of points in ref/non for each of the caribou habitat classes (here, we drop the "Other" category to just focus on the main results)
library(ggplot2)
library(patchwork)
library(cowplot)

Low <- as.data.frame(Low_Table)
Moderate <- as.data.frame(Moderate_Table)
High <- as.data.frame(High_Table)


Low_df <- subset(Low, Var1 == "Low")
#Low_df$Proportion <- Low_df$Freq / 30448077 #Use this to compare Low rows to the whole RSF sample
Low_df$Proportion <- Low_df$Freq / sum(Low_df$Freq) #If you want proportion compared to just what was in the "Low" row, use this


Moderate_df <- subset(Moderate, Var1 == "Moderate")
#Moderate_df$Proportion <- Moderate_df$Freq / 30448077
Moderate_df$Proportion <- Moderate_df$Freq / sum(Moderate_df$Freq) #If you want proportion compared to just what was in the "Moderate" row, use this


High_df <- subset(High, Var1 == "High")
#High_df$Proportion <- High_df$Freq / 30448077
High_df$Proportion <- High_df$Freq / sum(High_df$Freq) #If you want proportion compared to just what was in the "High" row, use this



p1 <- ggplot(Low_df, aes(x = Var2, y = Proportion, fill = Var2)) +
  geom_bar(stat = "identity", position = position_nudge(x = 0.6)) +
  labs(x = "Low", y = "Proportion", fill = "Caribou habitat use\n(Late winter 2020 RSF)") +
  scale_fill_manual(values = c("purple1", "aquamarine3")) +
  coord_cartesian(ylim = c(0,0.65), expand = FALSE) +
  scale_x_discrete(expand = expansion(add = 0.5)) +  # Add space before first bar
  theme_classic(base_size = 12) +
  theme(
    axis.line.x = element_line(),
    axis.line.y = element_line(),
    axis.ticks.x = element_blank(),
    axis.ticks.y = element_line(),
    axis.text.x = element_blank(),  # Remove tick labels under bars
    axis.text.y = element_text(),
    plot.margin = margin(5, 5, 5, 5)
  )



p2 <- ggplot(Moderate_df, aes(x = Var2, y = Proportion, fill = Var2)) +
  geom_bar(stat = "identity") +
  labs(x = "Moderate", fill = "Caribou habitat use\n(Late winter 2020 RSF)") +
  scale_fill_manual(values = c("purple1", "aquamarine3")) +
  coord_cartesian(ylim = c(0,0.65), expand = FALSE) +
  theme_classic(base_size = 12) +
  theme(
    axis.title.y = element_blank(),  # no y-axis title
    axis.text.y = element_blank(),   # no y-axis text
    axis.ticks.y = element_blank(),  # no y-axis ticks
    axis.line.y = element_blank(),   # no y-axis line
    axis.line.x = element_line(),    # x-axis line ON
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    plot.margin = margin(5, 5, 5, 5)
  )


p3 <- ggplot(High_df, aes(x = Var2, y = Proportion, fill = Var2)) +
  geom_bar(stat = "identity") +
  labs(x = "High", fill = "Caribou habitat use\n(Late winter 2020 RSF)") +
  scale_fill_manual(values = c("purple1", "aquamarine3")) +
  coord_cartesian(ylim = c(0,0.65), expand = FALSE) +
  theme_classic(base_size = 12) +
  theme(
    axis.title.y = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.line.y = element_blank(),
    axis.line.x = element_line(),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    plot.margin = margin(5, 5, 5, 5)
  )


library(grid)

combined <- p1 + p2 + p3 +
  plot_layout(ncol = 3, guides = "collect") +
  plot_annotation(theme = theme(plot.margin = margin(5, 5, 15, 5)))  # give bottom room

# Create centered label
x_label <- grid::textGrob(
  "Refugia probability (fuels)",
  x = 0.5,
  hjust = 0.5,
  gp = gpar(fontsize = 14, fontface= "bold") #Adjust fond size of x axis
)

# Stack: 3 plots / label
final_plot <- combined / wrap_elements(full = x_label) +
  plot_layout(heights = c(10, 1))  # adjust as needed

final_plot


#Export
ggsave(
  filename = "Z:/Christine/NWT Fire Refugia Project Z/Caribou_Validation Analyses/Caribou/RSF/V6/Graphs/LMH Fire Refugia Predictions/Late winter_2020_RSF_LMH_Ref_V6.png",
  plot = final_plot,                       # your combined plot object
  width = 18,                              # width in cm (adjust as needed)
  height = 10,                             # height in cm
  units = "cm",                            #specify measurement units so you don't end up with inches
  dpi = 300                                # print-quality resolution
)
gc()
 
