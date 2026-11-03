#I realized after the fact that there were a TON of samples in the validation years. Here all I'm doing is reading it back in from the new subfolder I put that sample in and truncating down
#to 50k per FRU, then writing it back into the proper subfolder
#If the sample is ever taken again, just do the truncation of the validation sample at the same time you do it for the training sample

library(data.table)
library(dplyr)

# Folder containing your CSVs
folder <- "W:/EDM/CCVegMod4/Christine Fire Refugia Projects/NWT Fire Refugia Project/Dataset CSVs/(Step 5) Sample for GEE/V6/Validation/Before truncation"

# Get all matching files
files <- list.files(
  path = folder,
  pattern = "^Fire_Sample_\\d{4}_Part_[A-Za-z0-9]+_V6\\.csv$",
  full.names = TRUE
)

# Read and combine (FAST)
all_data <- rbindlist(lapply(files, fread), use.names = TRUE, fill = TRUE)


# ----------------------------
# Validation checks
# ----------------------------

# Row counts check (FAST version)
row_counts <- sapply(files, function(f) nrow(fread(f)))
sum(row_counts) == nrow(all_data) #TRUE - they match

# Duplicate check
sum(duplicated(all_data$Unique_ID))

# Missing IDs
sum(is.na(all_data$Unique_ID)) #0, no duplicates

# Uniqueness check
length(unique(all_data$Unique_ID)) == nrow(all_data) #TRUE, all are unique


# ----------------------------
# Truncate to max 50k per FRU
# ----------------------------

set.seed(123)

subsampled <- as.data.table(all_data)[
  , .SD[sample(.N, min(.N, 50000))], by = FRU
]


# ----------------------------
# Compare counts before/after
# ----------------------------

counts_before <- all_data %>%
  count(FRU, name = "n_before")

counts_after <- subsampled %>%
  count(FRU, name = "n_after")

comparison <- counts_before %>%
  left_join(counts_after, by = "FRU") %>%
  mutate(
    n_after = ifelse(is.na(n_after), 0, n_after),
    difference = n_before - n_after
  )

comparison


# ----------------------------
# Export in 10k chunks by year
# ----------------------------

Chunks <- 10000
subsampled <- as.data.table(subsampled)
subsampled[, YEAR := as.factor(YEAR)]

Year_List <- levels(subsampled$YEAR)

out_folder <- "W:/EDM/CCVegMod4/Christine Fire Refugia Projects/NWT Fire Refugia Project/Dataset CSVs/(Step 5) Sample for GEE/V6/Validation"

for (yr in Year_List) {
  
  Current_Year <- subsampled[YEAR == yr]
  setorder(Current_Year, Unique_ID)  # keeps order stable
  
  Number_of_Rows <- nrow(Current_Year)
  
  Bits <- rep(seq_len(ceiling(Number_of_Rows / Chunks)), each = Chunks)[1:Number_of_Rows]
  
  Export <- split(Current_Year, Bits)
  
  for (i in seq_along(Export)) {
    
    Out <- Export[[i]]
    
    Part_Label <- if (i <= 26) {
      LETTERS[i]
    } else {
      paste0("AA", i - 26)
    }
    
    FileName <- file.path(
      out_folder,
      paste0("Fire_Sample_", yr, "_Part_", Part_Label, "_V6.csv")
    )
    
    fwrite(Out, FileName)
  }
}
#I did check separately and the number of points in subsampled is the same number of points when the 10k chunk samples are brought back together into a single df
#i.e., no points were lost or introduced when running the 10k chunk code