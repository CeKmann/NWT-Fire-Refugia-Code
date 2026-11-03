library(dplyr)
library(data.table)

df <- fread("W:/EDM/CCVegMod4/Christine Fire Refugia Projects/NWT Fire Refugia Project/Dataset CSVs/(Step 7) Final/V6/Training_Sample_V6.csv")
df <- as.data.frame(df)

df$FRU <- as.factor(df$FRU)
df$ecozone <- as.factor(df$ecozone)


#FRU 24/26
FRU1 <- df %>%
  filter(FRU == 24 | (FRU == 26 & (ecozone == 5 | ecozone == 6))) %>%
  droplevels()
set.seed(123) # Makes sure your sample is reproducible 
FRU1 <- FRU1[sample(1:nrow(FRU1), 25000,
                              replace=FALSE),]


#FRU 25
FRU2 <- df %>%
  filter(FRU == 25) %>%
  droplevels()
set.seed(123) # Makes sure your sample is reproducible 
FRU2 <- FRU2[sample(1:nrow(FRU2), 25000,
                        replace=FALSE),]


#FRU 26
FRU3 <- df %>%
  filter(FRU == 26, ecozone == 4 | ecozone == 9) %>%
  droplevels()


#FRU 31
FRU4 <- df %>%
  filter(FRU == 31) %>%
  droplevels()
set.seed(123) # Makes sure your sample is reproducible 
FRU4 <- FRU4[sample(1:nrow(FRU4), 25000,
                        replace=FALSE),]


#FRU 32
FRU5 <- df %>%
  filter(FRU == 32) %>%
  droplevels()
set.seed(123) # Makes sure your sample is reproducible 
FRU5 <- FRU5[sample(1:nrow(FRU5), 25000,
                        replace=FALSE),]


#FRU 33
FRU6 <- df %>%
  filter(FRU == 33) %>%
  droplevels()
set.seed(123) # Makes sure your sample is reproducible 
FRU6 <- FRU6[sample(1:nrow(FRU6), 25000,
                        replace=FALSE),]


#FRU 34
FRU7 <- df %>%
  filter(FRU == 34) %>%
  droplevels()
set.seed(123) # Makes sure your sample is reproducible 
FRU7 <- FRU7[sample(1:nrow(FRU7), 25000,
                        replace=FALSE),]


#FRU 35
FRU8 <- df %>%
  filter(FRU == 35) %>%
  droplevels()


#FRU 37/38/42
FRU9 <- df %>%
  filter(FRU == 37 | FRU == 38 | FRU == 42) %>%
  droplevels()
set.seed(123) # Makes sure your sample is reproducible 
FRU9 <- FRU9[sample(1:nrow(FRU9), 25000,
                                    replace=FALSE),]


#FRU 40
FRU10 <- df %>%
  filter(FRU == 40) %>%
  droplevels()
set.seed(123) # Makes sure your sample is reproducible 
FRU10 <- FRU10[sample(1:nrow(FRU10), 25000,
                        replace=FALSE),]


#FRU 41
FRU11 <- df %>%
  filter(FRU == 41) %>%
  droplevels()
set.seed(123) # Makes sure your sample is reproducible 
FRU11 <- FRU11[sample(1:nrow(FRU11), 25000,
                        replace=FALSE),]


#FRU 43
FRU12 <- df %>%
  filter(FRU == 43) %>%
  droplevels()


#FRU 45
FRU13 <- df %>%
  filter(FRU == 45) %>%
  droplevels()
set.seed(123) # Makes sure your sample is reproducible 
FRU13 <- FRU13[sample(1:nrow(FRU13), 25000,
                        replace=FALSE),]


#FRU 46
FRU14 <- df %>%
  filter(FRU == 46) %>%
  droplevels()



full <- rbind(FRU1, FRU2, FRU3, FRU4, 
              FRU5, FRU6, FRU7, FRU8,
              FRU9, FRU10, FRU11, FRU12,
              FRU13, FRU14)

fwrite(full, file = "Z:/Christine/NWT Fire Refugia Project Z/Zenodo/V2/Training_Sample_Truncated.csv")
