library(terra)
library(data.table)
df <- fread("W:/EDM/CCVegMod4/Christine Fire Refugia Projects/NWT Fire Refugia Project/Dataset CSVs/(Step 4) Prepping sample for GEE/V6/C_Fire_Weather_Extracted_V6.csv")

#Are all fires >=500ha?
range(df$ADJ_HA) #Yes

#Do they all range from 2002-2024?
range(df$YEAR) #Yes
#Any years missing?
df$YEAR <- as.factor(df$YEAR)
levels(df$YEAR) #Nope, all present 

#Do all start months range from April (4) to October (10)?
range(df$month_num) #Yes

#Are all IDs unique?
UID <- unique(df$Unique_ID) #Both df and UID have 675599 points. All IDs are unique

#Are there any duplicate points within the same year?
duplicates_within_year <- df[ , .N, by = .(YEAR, Lam_x, Lam_y)][N > 1]
duplicates_within_year #Says no duplicate points within the same year

#Does the sample properly reflect aggregation of the NBAC polygon data?
NBAC <- vect("W:/EDM/CCVegMod4/Christine Fire Refugia Projects/AB Fire Refugia Project/Data/Original Data/Fires/Polygons/NBAC/NBAC_1972to2024_20250506.shp")
NBAC_ag <- terra::aggregate( #Aggregate polygons by GID
  NBAC[, c("GID", "ADJ_HA")], 
  by = "GID", 
  dissolve = TRUE, 
  fun = sum,       
  na.rm = TRUE
)
gc()

agg_dt <- data.table( #Create data.table from aggregated layer
  GID = NBAC_ag$GID,
  ADJ_HA_ag = NBAC_ag$agg_ADJ_HA
)
gc()

agg_subset <- agg_dt[GID %in% df$GID, .(GID, ADJ_HA_ag)] #Subset to only GIDs present in sample

df <- merge( #Merge df with aggregated values (in-place update)
  df,
  agg_subset,
  by = "GID",
  all.x = TRUE #Use 'all.x = TRUE' to keep all rows of df
)

tol <- 1e-6 #Need to have this or there will be a lot of mismatches that are just off by decimals
df[, diff := ADJ_HA - ADJ_HA_ag] #Identify mismatches
df[, needs_fix := abs(diff) > tol]

mismatch_log <- unique(df[needs_fix == TRUE, .(GID, original_ADJ_HA = ADJ_HA, corrected_ADJ_HA = ADJ_HA_ag)]) #Log which fires need correction
cat("Number of fires needing correction:", nrow(mismatch_log), "\n")
print(mismatch_log) #Shows 0 fires needing correction

df[needs_fix == TRUE, ADJ_HA := ADJ_HA_ag] #Correct ADJ_HA in df


##I didn't do the below since 0 fires needed correcting and everything seemed to have worked in AB
######Tests to make sure the values were properly corrected and that ones that don't need updating stayed the same:
# test_orig1 <- df[df$GID== ""]
# test_orig2 <- df[df$GID== ""]
# 
# test_nbac1 <- NBAC_ag[NBAC_ag$GID == ""]
# test_nbac2 <- NBAC_ag[NBAC_ag$GID== ""]
# gc()
# 
# unique(test_orig2$ADJ_HA)
# unique(test_orig2$ADJ_HA_ag)
# test_nbac2$agg_ADJ_HA
# 
# #Try on ones that should not have been aggregated
# test_df_full <- fread("W:/EDM/CCVegMod4/Christine Fire Refugia Projects/NWT Fire Refugia Project/Dataset CSVs/(Step 4) Prepping sample for GEE/V6/C_Fire_Weather_Extracted_V6.csv")
# test_df <- test_df_full[test_df_full$GID == ",]
# test_nbac3 <- NBAC[NBAC$GID == "",]
# gc()
# unique(test_df$ADJ_HA)
# test_nbac3$ADJ_HA

# #I didn't do the below since I did the aggregating before masking when I took the NWT sample, as suggested in the note
# #Here I'm checking to see if there are any instances where this happened but the <500ha side is what's in the study area and therefore the whole fire was removed prior to sampling
# result <- NBAC_SA %>% 
#   group_by(GID) %>% #Note that this was run on the version that was already masked to study area, since I don't want ALL <500 polygons. If running again, just update to use that version instead of original NBAC
#   filter(n() > 1 & any(ADJ_HA < 500)) %>%
#   ungroup()
# fires <- unique(result$GID) #This shows there are 25 fires like this. However, I checked them in Arc and all of them have both polygons included in SA. No need to go back, but next time aggregate before masking to study area



#####Now move on to cleaning it up a bit before exporting
#Get rid of unwanted columns (they didn't end up existing, but leaving this here in case something like this is needed in another version)
colnames(df)
df_pruned <- df[, c("ADJ_HA_ag", "diff", "needs_fix", "year", "Quinn_lat", "Quinn_lon", "Quinn_UID") := NULL]
colnames(df_pruned)

#Removed points with distance to FW >=4km
range(df_pruned$Dist_to_FW)
df_pruned <- df_pruned[Dist_to_FW < 4000] #76 points removed, 1,809,104 remain


#Then, finally, break into 10k chunks for upload to GEE (don't forget to go through them after and get rid of the reduntant columns it doesn't accept)
#Parameters
Chunks <- 10000
df_pruned$YEAR <- as.factor(df_pruned$YEAR)
Year_List <- levels(df_pruned$YEAR)

# Loop over years
for (yr in Year_List) {
  
  # Subset current year sequentially
  Current_Year <- df_pruned[YEAR == yr]
  Number_of_Rows <- nrow(Current_Year)
  
  # Create chunk indices
  Bits <- rep(seq_len(ceiling(Number_of_Rows / Chunks)), each = Chunks)[1:Number_of_Rows]
  
  # Split by chunks
  Export <- split(Current_Year, Bits)
  
  for (i in seq_along(Export)) {
    Out <- Export[[i]]
    
    # Safe letter suffix; if more than 26 chunks, fallback to double letters
    if (i <= 26) {
      Part_Label <- LETTERS[i]
    } else {
      Part_Label <- paste0("AA", i-26)  # simple extension beyond Z
    }
    
    # Build filename
    FileName <- file.path(
      "W:/EDM/CCVegMod4/Christine Fire Refugia Projects/NWT Fire Refugia Project/Dataset CSVs/(Step 5) Sample for GEE/V6",
      paste0("Fire_Sample_", yr, "_Part_", Part_Label, "_V6.csv")
    )
    
    # Write CSV
    fwrite(Out, FileName)
  }
}
#I just manually moved the validation years (2022-2024) over into their own folder within that path since I forgot to include something for that here