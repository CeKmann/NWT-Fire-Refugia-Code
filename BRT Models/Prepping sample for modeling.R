library(data.table)

##Load up the files from GEE (all have the same number of rows)
Climate <- "W:/EDM/CCVegMod4/Christine Fire Refugia Projects/NWT Fire Refugia Project/Dataset CSVs/(Step 6) Raw from GEE/Climate"
Climate_Files <- list.files(Climate, pattern = "\\.csv$", full.names = TRUE)
Out_Climate <- data.table::rbindlist(lapply(Climate_Files, data.table::fread))

Terrain_Peats <- "W:/EDM/CCVegMod4/Christine Fire Refugia Projects/NWT Fire Refugia Project/Dataset CSVs/(Step 6) Raw from GEE/Terrain_Peats"
Terrain_Peats_Files <- list.files(Terrain_Peats, pattern = "\\.csv$", full.names = TRUE)
Out_Terrain_Peats <- data.table::rbindlist(lapply(Terrain_Peats_Files, data.table::fread))

Disturbance <- "W:/EDM/CCVegMod4/Christine Fire Refugia Projects/NWT Fire Refugia Project/Dataset CSVs/(Step 6) Raw from GEE/Disturbance"
Disturbance_Files <- list.files(Disturbance, pattern = "\\.csv$", full.names = TRUE)
Out_Disturbance <- data.table::rbindlist(lapply(Disturbance_Files, data.table::fread))

Fuels <- "W:/EDM/CCVegMod4/Christine Fire Refugia Projects/NWT Fire Refugia Project/Dataset CSVs/(Step 6) Raw from GEE/Fuels"
Fuels_Files <- list.files(Fuels, pattern = "\\.csv$", full.names = TRUE)
Out_Fuels <- data.table::rbindlist(lapply(Fuels_Files, data.table::fread))

Open_Extras <- "W:/EDM/CCVegMod4/Christine Fire Refugia Projects/NWT Fire Refugia Project/Dataset CSVs/(Step 6) Raw from GEE/Open_Areas_Extras"
Open_Extras_Files <- list.files(Open_Extras, pattern = "\\.csv$", full.names = TRUE)
Out_Open_Extras <- data.table::rbindlist(lapply(Open_Extras_Files, data.table::fread))



#####################Check to make sure there's the same number of samples in what went into GEE (confirmed the numbers are the same)
Check <- "W:/EDM/CCVegMod4/Christine Fire Refugia Projects/NWT Fire Refugia Project/Dataset CSVs/(Step 5) Sample for GEE/V6"
Check_Files <- list.files(Check, pattern = "\\.csv$", full.names = TRUE)
Out_Check <- data.table::rbindlist(lapply(Check_Files, data.table::fread))

Val <- "W:/EDM/CCVegMod4/Christine Fire Refugia Projects/NWT Fire Refugia Project/Dataset CSVs/(Step 5) Sample for GEE/V6/Validation"
Val_Files <- list.files(Val, pattern = "\\.csv$", full.names = TRUE)
Out_Val <- data.table::rbindlist(lapply(Val_Files, data.table::fread))

nrow(Out_Open_Extras) == nrow(Out_Check) + nrow(Out_Val)




#####################Do they all also have the same unique IDs and is there one ID for each row? (yes on both counts)
length(unique(Out_Climate$Unique_ID)) == nrow(Out_Climate)
length(unique(Out_Terrain_Peats$Unique_ID)) == nrow(Out_Terrain_Peats)
length(unique(Out_Disturbance$Unique_ID)) == nrow(Out_Disturbance)
length(unique(Out_Fuels$Unique_ID)) == nrow(Out_Fuels)
length(unique(Out_Open_Extras$Unique_ID)) == nrow(Out_Open_Extras)


##################Do they all have the same unique IDs?
Climate_IDS <- unique(Out_Climate$Unique_ID)  
Terrain_IDS <- unique(Out_Terrain_Peats$Unique_ID)
Dist_IDS <- unique(Out_Disturbance$Unique_ID)
Fuels_IDS <- unique(Out_Fuels$Unique_ID)
Open_Extras_IDS <- unique(Out_Open_Extras$Unique_ID)
setequal(Climate_IDS, Terrain_IDS)     #same
setequal(Climate_IDS, Dist_IDS)        #same
setequal(Climate_IDS, Fuels_IDS)       #same
setequal(Climate_IDS, Open_Extras_IDS) #same


##################Are there any duplicate unique IDs?
any(duplicated(Out_Climate$Unique_ID))       #no
any(duplicated(Out_Terrain_Peats$Unique_ID)) #no
any(duplicated(Out_Disturbance$Unique_ID))   #no
any(duplicated(Out_Fuels$Unique_ID))         #no
any(duplicated(Out_Open_Extras$Unique_ID))   #no


##################Bind them all together into a single dataframe
dfs <- list(
  Out_Climate,
  Out_Terrain_Peats,
  Out_Disturbance,
  Out_Fuels,
  Out_Open_Extras
)

dfs <- lapply(dfs, as.data.table)

# Set key
dfs <- lapply(dfs, function(x) { setkey(x, Unique_ID); x })

Final_Data <- Reduce(function(x, y) {
  
  dupes <- intersect(names(x), names(y))
  dupes <- dupes[dupes != "Unique_ID"]
  
  y[, (dupes) := NULL]
  
  x[y]   # keyed join
  
}, dfs)
gc()

##################Make date of burn into months. Remove the ones from November
# Ensure DOB is numeric
Final_Data[, DOB := as.integer(DOB)]

# Convert DOY to proper Date using the Year column
Final_Data[, BurnDate := as.Date(DOB - 1, origin = paste0(YEAR, "-01-01"))]

# Extract numeric month (1–12)
Final_Data[, BurnMonth := as.integer(format(BurnDate, "%m"))]
gc()
range(Final_Data$BurnMonth) #There are pixels that burned into November, remove them

Final_Data <- Final_Data[Final_Data$BurnMonth < 11, ]



##################Check for NAs and remove only the appropriate ones (the only NAs were in columns they were expected to happen)
colSums(is.na(Final_Data))

Final_Data <- Final_Data[!is.na(Soil_Carbon_Histo)]
Final_Data <- Final_Data[!is.na(Sand)] #I'm left with 928,824 points (1696 removed)
gc()


##################Remove unwanted columns and change some variable names
Final_Data[, c(".geo", "system:index") := NULL] 

colnames(Final_Data)
setnames(Final_Data,
         c("SCANFI_Closure_Bio_1200m", "SCANFI_Closure_Bio_300m"),
         c("SCANFI_Closure_1200m", "SCANFI_Closure_300m"))



##################Round all numeric/integer columns up to 4 decimal places
# test <- Final_Data
# num_cols <- names(Final_Data)[sapply(Final_Data, is.numeric)]
# Final_Data[, (num_cols) := lapply(.SD, function(x) round(x, 4)), .SDcols = num_cols]
# 
#   ##Since this messed with FRU before turning it into a factor, do a quick check to make sure everything stayed in the right order/still matches exactly in vars meant to be unchanged
#     setkey(Final_Data, Unique_ID)
#     setkey(test, Unique_ID)
#     
#     identical(Final_Data$Refugia, test$Refugia)
#     identical(Final_Data$FRU, test$FRU)
#     identical(Final_Data$Lam_x, test$Lam_x)
#     identical(Final_Data$Lam_y, test$Lam_y)
#     identical(Final_Data$ecozone, test$ecozone)
#     identical(Final_Data$BurnMonth, test$BurnMonth) ##All show TRUE, so they check out



##################Final checks on fire info
min(Final_Data$ADJ_HA) #>=500? (yes)
range(Final_Data$YEAR) #2002-2024? (yes)
range(Final_Data$month_num) #4-10? (yes)
range(Final_Data$PRESCRIBED) #any prescribed fires? (no)


###################Break off training from Val and save separately 
# Rows with YEAR >= 2022
Final_Val <- Final_Data[YEAR >= 2022] #371,796 points

# Rows with YEAR 2002–2021
Final_Train <- Final_Data[YEAR >= 2002 & YEAR <= 2021] #557,028 points


###################Save to different files
fwrite(Final_Val, file = "W:/EDM/CCVegMod4/Christine Fire Refugia Projects/NWT Fire Refugia Project/Dataset CSVs/(Step 7) Final/V6/Val_Sample_V6.csv")
fwrite(Final_Train, file = "W:/EDM/CCVegMod4/Christine Fire Refugia Projects/NWT Fire Refugia Project/Dataset CSVs/(Step 7) Final/V6/Training_Sample_V6.csv")

