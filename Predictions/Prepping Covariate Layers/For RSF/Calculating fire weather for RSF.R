library(terra)
library(data.table)
library(dplyr)
library(ggplot2)
library(flextable)   # for Word tables
library(officer)     # for exporting to Word


#########################################################################
##############################JULY WEATHER##############################
#########################################################################

##Load df
df <- fread("Z:/Christine/NWT Fire Refugia Project Z/Dataset CSVs/(Step 7) Final/V6/Training_Sample_V6.csv")

df <- as.data.frame(df)
df$ecozone <- as.factor(df$ecozone)

################################################################################
# CREATE CUSTOM FRU GROUPS
################################################################################

df$FRU_GROUP <- NA

# Ecozone split example
df$FRU_GROUP[df$FRU == 26 & df$ecozone %in% c(5, 6)] <- "FRU_26_shield" #Shield

df$FRU_GROUP[df$FRU == 26 & df$ecozone %in% c(4, 9)] <- "FRU_26" #Plains

# Everything else stays as its own FRU
remaining <- is.na(df$FRU_GROUP)
df$FRU_GROUP[remaining] <- paste0("FRU_", df$FRU[remaining])

# List of all FRU groups
all_FRUs <- sort(unique(df$FRU_GROUP))

################################################################################
# FILTER JULY PERIOD
################################################################################

df_specified <- df %>% 
  dplyr::filter(YEAR == 2021,
                DOB >= 152, #From June 1
                DOB <= 243) #to Aug 31


# Summarise for the filtered DOB range
weather_table <- df_specified %>%
  group_by(FRU_GROUP) %>%
  summarise(
    vpd_median = median(vpd, na.rm = TRUE),
    #vpd_extreme = quantile(vpd, 0.90, na.rm = TRUE),
    isi_median = median(isi, na.rm = TRUE),
    #isi_extreme = quantile(isi, 0.90, na.rm = TRUE),
    bui_median = median(bui, na.rm = TRUE),
    #bui_extreme = quantile(bui, 0.90, na.rm = TRUE),
    rh_median = median(rh, na.rm = TRUE),
    #rh_extreme = quantile(rh, 0.90, na.rm = TRUE)
  ) %>%
  right_join(data.frame(FRU_GROUP = all_FRUs), by = "FRU_GROUP") %>%
  arrange(FRU_GROUP)
weather_table

weather_df <- as.data.frame(weather_table)

#Export as a table to keep track
weather_ft <- flextable(weather_table)
weather_ft <- set_caption(weather_ft, caption = "Fire weather per FRU for RSF median") #Add title
read_docx() %>% # Save as Word document
  body_add_flextable(weather_ft) %>%
  print(target = "Z:/Christine/NWT Fire Refugia Project Z/Prediction/V6/Processed GEE Layers/RSF V6/Fire Weather/Fire_weather_RSF_median.docx")


#The hot years were missing some values for FRU, so filling those in here
fill_map <- c(
  "FRU_42" = "FRU_37"
)

vars <- c(
  "vpd_median",
  "isi_median",
  "bui_median",
  "rh_median"
)

for (target in names(fill_map)) {
  source <- fill_map[target]
  
  weather_table[weather_table$FRU_GROUP == target, vars] <-
    weather_table[weather_table$FRU_GROUP == source, vars]
}
weather_table
weather_df <- as.data.frame(weather_table)



###Start creating rasters
##Load ROI and make all values 0
ROI <- rast("Z:/Christine/NWT Fire Refugia Project Z/Data/Final Data/Study Area/NWT_Base_Lam_120m.tif")
ROI <- ROI*0

out_dir <- "Z:/Christine/NWT Fire Refugia Project Z/Prediction/V6/Processed GEE Layers/RSF V6/Fire Weather/"

for (v in vars) {
  
  for (i in 1:nrow(weather_df)) {
    
    fru_id <- weather_df$FRU_GROUP[i]
    
    val <- weather_df[[v]][i]
    
    r <- ROI + val
    
    ## Default folder name
    folder_name <- gsub("_", " ", fru_id)
    
    ## Special cases
    if (fru_id == "FRU_26_shield") {
      folder_name <- "FRU 26 shield"
    }
    
    if (fru_id == "FRU_26") {
      folder_name <- "FRU 26"
    }
    
    ## CREATE FOLDER IF IT DOESN'T EXIST
    dir.create(
      file.path(out_dir, folder_name),
      recursive = TRUE,
      showWarnings = FALSE
    )
    
    ## Output file
    out_file <- file.path(
      out_dir,
      folder_name,
      paste0(v, "_RSF_median.tif")
    )
    
    ## Export
    writeRaster(r, out_file, overwrite = TRUE)
    
    gc()
  }
}
