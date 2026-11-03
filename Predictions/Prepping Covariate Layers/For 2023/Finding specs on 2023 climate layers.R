library(terra)
library(dplyr)
library(tidyr)

cmi <- rast("W:/EDM/CCVegMod4/Christine Fire Refugia Projects/NWT Fire Refugia Project/Prediction/V6/Processed GEE Layers/Climate Anoms V6/Anoms 2023/Anom_CMI_2023.tif")
mat <- rast("W:/EDM/CCVegMod4/Christine Fire Refugia Projects/NWT Fire Refugia Project/Prediction/V6/Processed GEE Layers/Climate Anoms V6/Anoms 2023/Anom_MAT_2023.tif")
mfst <- rast("W:/EDM/CCVegMod4/Christine Fire Refugia Projects/NWT Fire Refugia Project/Prediction/V6/Processed GEE Layers/Climate Anoms V6/Anoms 2023/Anom_MFST_2023.tif")
vpd <- rast("W:/EDM/CCVegMod4/Christine Fire Refugia Projects/NWT Fire Refugia Project/Prediction/V6/Processed GEE Layers/Climate Anoms V6/Anoms 2023/Anom_VPD_2023.tif")

FRU <- rast("W:/EDM/CCVegMod4/Christine Fire Refugia Projects/NWT Fire Refugia Project/Prediction/V6/FRUs/Individual/FRU_46.tif")
cmi_masked <- mask(cmi, FRU)
mat_masked <- mask(mat, FRU)
mfst_masked <- mask(mfst, FRU)
vpd_masked <- mask(vpd, FRU)
plot(cmi_masked)
gc()

cmi_df <- as.data.frame(cmi_masked, na.rm=T)
gc()
mat_df <- as.data.frame(mat_masked, na.rm=T)
gc()
mfst_df <- as.data.frame(mfst_masked, na.rm=T)
gc()
vpd_df <- as.data.frame(vpd_masked, na.rm=T)
gc()

df <- cbind(cmi_df, mat_df, mfst_df, vpd_df)
gc()



summary_table <- df %>%
  summarise(
    across(
      c(Anom_CMI, Anom_MAT, Anom_MFST, Anom_VPD),
      list(
        max = ~max(.x, na.rm = TRUE),
        min = ~min(.x, na.rm = TRUE),
        median = ~median(.x, na.rm = TRUE)
      )
    )
  ) %>%
  pivot_longer(
    everything(),
    names_to = c("Variable", ".value"),
    names_sep = "_(?=[^_]+$)"
  )

summary_table
