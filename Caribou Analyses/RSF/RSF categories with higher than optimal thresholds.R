library(terra)
library(questionr)
library(flextable)
library(officer)
library(dplyr)

#Load masked Fuels predictions and RSF
Full_Preds <- rast("W:/EDM/CCVegMod4/Christine Fire Refugia Projects/NWT Fire Refugia Project/Prediction/V6/Predictions/Mosaics/Masked/Masked_NWT_Full_Median_Mosaic_V6.tif")
RSF <- rast("W:/EDM/CCVegMod4/Christine Fire Refugia Projects/NWT Fire Refugia Project/Data/Final Data/Caribou/RSF/2017/RSF_2017_LateWinter_120m.tif")


#Now mask out any pixels in the preds that don't also have pixels in the resampled RSF
Preds_Masked1 <- !is.na(RSF) 
  gc()
Preds_Masked <- mask(Full_Preds, Preds_Masked1, maskvalue=FALSE)
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
    

#Change the name of the column for preds and get rid of coords
pred_df<- pred_df1[-c(1,2)] #Remove coords
names(pred_df)[1] <- "Preds"
  gc()
pred_df$RSF <- as.numeric(pred_df$RSF)

#Make 3 new columns to use in next steps and get rid of the original one  
pred_df$Preds_Avoid <- pred_df$Preds
pred_df$Preds_Select <- pred_df$Preds
pred_df$Preds_SSelect <- pred_df$Preds
pred_df <- pred_df[-c(1)]

#Do the same with the RSF one
pred_df$RSF_Avoid <- pred_df$RSF
pred_df$RSF_Select <- pred_df$RSF
pred_df$RSF_SSelect <- pred_df$RSF
pred_df <- pred_df[-c(1)]
head(pred_df)


#Turn predictions into a binary based on the selection values for the RSF, then calcuLate Odds Ratio
#For this, values of 8-10 are strongly selected, 6-7 are selected, and 1-5 are avoided
#To find optimal threshold used, see code titled "Finding Optimal Threshold based on SAMPLES within RSF"

####AVOIDED####
pred_df$Preds_Avoid_bin <- ifelse(
  pred_df$Preds_Avoid >0.5, NA, #Anything >0.5 is removed completely
  ifelse(pred_df$Preds_Avoid >= 0.1553196, 1, 0)) #Anything remaining that's >= optimal threshold is refugia, rest is non-refugia
pred_df$RSF_Avoid_bin <- ifelse(pred_df$RSF_Avoid >= 6, 0, 1)  # all >=6 zero, all else one

pred_df <- na.omit(pred_df)

#To check if all really are just 0 and 1 after running the code, use this:
unique(pred_df$Preds_Avoid_bin)
unique(pred_df$RSF_Avoid_bin)

#Factor categories for cross-tab
pred_df$RSF_Avoid_cat <- ifelse(pred_df$RSF_Avoid_bin == 0, "Other", "Avoided")
pred_df$Preds_Avoid_cat <- ifelse(pred_df$Preds_Avoid_bin == 0, "Non", "Refugia")

pred_df$RSF_Avoid_cat <- na.rm(pred_df$RSF_Avoid_cat)

#If running this checks code, will need to update it to match current column names
# #Do the number of non and refugia match the number of 0 and 1 in another column? (Harder to do with RSF, just checking code here)
sum(pred_df$Preds_Avoid_bin == 0, na.rm = TRUE)
  sum(pred_df$Preds_Avoid_cat == "Non", na.rm = TRUE) #These match
sum(pred_df$Preds_Avoid_bin == 1, na.rm = TRUE)
  sum(pred_df$Preds_Avoid_cat == "Refugia", na.rm = TRUE) #These match

#To look at all the associated columns and make sure they make sense:
check1 <- pred_df[c("Preds_Avoid", "Preds_Avoid_bin", "Preds_Avoid_cat",
                    "RSF_Avoid", "RSF_Avoid_bin", "RSF_Avoid_cat")]

Avoid_Table <- table(pred_df$RSF_Avoid_cat, pred_df$Preds_Avoid_cat)
Avoid_Table
odds.ratio(Avoid_Table)
  gc()



####SELECTED####
pred_df$Preds_Select_bin <- ifelse(
    pred_df$Preds_Select >0.5, NA, #Anything >0.5 is removed completely
    ifelse(pred_df$Preds_Select >= 0.1553196, 1, 0)) #Anything remaining that's >= optimal threshold is refugia, rest is non-refugia
pred_df$RSF_Select_bin <- ifelse(pred_df$RSF_Select >= 6 & pred_df$RSF_Select < 8, 1, 0) #If values are gte 6 and less than 8, they become 1. Otherwise they're 0
pred_df <- na.omit(pred_df)

#To check if all really are just 0 and 1 after running the code, use this:
unique(pred_df$Preds_Select_bin)
unique(pred_df$RSF_Select_bin)


pred_df$Preds_Select_cat <- ifelse(pred_df$Preds_Select_bin == 0, "Non", "Refugia")
pred_df$RSF_Select_cat <- ifelse(pred_df$RSF_Select_bin == 0, "Other", "Selected")
  gc()

#To look at all the associated columns and make sure they make sense:
check2 <- pred_df[c("RSF_Select", "RSF_Select_bin", "RSF_Select_cat",
                    "Preds_Select", 'Preds_Select_bin', 'Preds_Select_cat')]

Select_Table <- table(pred_df$RSF_Select_cat, pred_df$Preds_Select_cat)
Select_Table
odds.ratio(Select_Table)
  gc()



####STRONGLY SELECTED####
pred_df$Preds_SSelect_bin <- ifelse(
    pred_df$Preds_SSelect >0.5, NA, #Anything >0.5 is removed completely
    ifelse(pred_df$Preds_SSelect >= 0.1553196, 1, 0)) #Anything remaining that's >= optimal threshold is refugia, rest is non-refugia
pred_df$RSF_SSelect_bin <- ifelse(pred_df$RSF_SSelect >= 8, 1, 0) #If values are 8 or above, they become 1. Otherwise they become 0
pred_df <- na.omit(pred_df)

#To check if all really are just 0 and 1 after running the code, use this:
unique(pred_df$Preds_SSelect_bin)
unique(pred_df$RSF_SSelect_bin)

pred_df$Preds_SSelect_cat <- ifelse(pred_df$Preds_SSelect_bin == 0, "Non", "Refugia")
pred_df$RSF_SSelect_cat <- ifelse(pred_df$RSF_SSelect_bin == 0, "Other", "SSelected")
  gc()

#To look at all the associated columns and make sure they make sense:
check3 <- pred_df[c("RSF_SSelect", "RSF_SSelect_bin", "RSF_SSelect_cat")]
check4 <- pred_df[c("Preds_SSelect", "Preds_SSelect_bin", "Preds_SSelect_cat")]

SSelect_Table <- table(pred_df$RSF_SSelect_cat, pred_df$Preds_SSelect_cat)
SSelect_Table
odds.ratio(SSelect_Table)
  gc()
  

  
#############################
#Finally, make plots of the proportion of points in ref/non for each of the caribou habitat classes (here, we drop the "Other" category to just focus on the main results)
library(ggplot2)
library(patchwork)
library(cowplot)
  
Avoid <- as.data.frame(Avoid_Table)
Select <- as.data.frame(Select_Table)
SSelect <- as.data.frame(SSelect_Table)


Avoided_df <- subset(Avoid, Var1 == "Avoided")
#Avoided_df$Proportion <- Avoided_df$Freq / 30448077 #Use this to compare Avoid rows to the whole RSF sample
Avoided_df$Proportion <- Avoided_df$Freq / sum(Avoided_df$Freq) #If you want proportion compared to just what was in the "Avoid" row, use this


Selected_df <- subset(Select, Var1 == "Selected")
#Selected_df$Proportion <- Selected_df$Freq / 30448077
Selected_df$Proportion <- Selected_df$Freq / sum(Selected_df$Freq) 


SSelected_df <- subset(SSelect, Var1 == "SSelected")
#SSelected_df$Proportion <- SSelected_df$Freq / 30448077
SSelected_df$Proportion <- SSelected_df$Freq / sum(SSelected_df$Freq) 



p1 <- ggplot(Avoided_df, aes(x = Var2, y = Proportion, fill = Var2)) +
  geom_bar(stat = "identity", position = position_nudge(x = 0.6)) +
  labs(x = "Avoided", y = "Proportion", fill = "Fuels\npredictions\n(0.5)") +
  scale_fill_manual(values = c("sienna3", "lightblue")) +
  coord_cartesian(ylim = c(0, 0.8), expand = FALSE) +
  scale_x_discrete(expand = expansion(add = 0.9)) +  # Add space before first bar
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



p2 <- ggplot(Selected_df, aes(x = Var2, y = Proportion, fill = Var2)) +
  geom_bar(stat = "identity") +
  labs(x = "Moderately selected", fill = "Fuels\npredictions\n(0.5)") +
  scale_fill_manual(values = c("sienna3", "lightblue")) +
  coord_cartesian(ylim = c(0, 0.8), expand = FALSE) +
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


p3 <- ggplot(SSelected_df, aes(x = Var2, y = Proportion, fill = Var2)) +
  geom_bar(stat = "identity") +
  labs(x = "Strongly selected", fill = "Fuels\npredictions\n(0.5)") +
  scale_fill_manual(values = c("sienna3", "lightblue")) +
  coord_cartesian(ylim = c(0, 0.8), expand = FALSE) +
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
  "LateWinter RSF",
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
  filename = "W:/EDM/CCVegMod4/Christine Fire Refugia Projects/NWT Fire Refugia Project/Caribou_Validation Analyses/Caribou/RSF/V6/Graphs/Fuels/2017/RSF Classes/Fuels_2017_LateWinter_RSF_0.5_V6.png",
  plot = final_plot,                       # your combined plot object
  width = 18,                              # width in cm (adjust as needed)
  height = 10,                             # height in cm
  units = "cm",                            #specify measurement units so you don't end up with inches
  dpi = 300                                # print-quality resolution
)
  gc()


