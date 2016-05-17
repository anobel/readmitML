# Packages
library(dplyr)
library(tidyr)
library(reshape2)
library(stringr)
library(icd)

# Import full data
pt <- readRDS("data/patient/tidy/pt.rds")

# isolate primary diagnoses (must always be present on admission), and will add back later
diag_p <- pt[,c("rln", "admtdate", "diag_p")]
colnames(diag_p) <- c("rln", "admtdate", "icd_code")

# Create vectors to identify other diagnoses and present on admission column names
odiags <- paste("odiag",1:24,sep="")
opoas <- paste("opoa", 1:24, sep="")

# Limit DF for columns of interest
dx <- pt[,c("rln", "admtdate", odiags, opoas)]

#rm pt data
rm(pt)

dx[,c(odiags, opoas)] <- as.data.frame(lapply(dx[,c(odiags, opoas)], as.character), stringsAsFactors = F)

# Convert wide to long and rename
dx <- dx %>%
  gather(var, value, -rln, -admtdate, na.rm=T)

dx$value <- factor(dx$value)

# Split the odiag1-24 and opoa1-24 columns into two so that I can identify the number associated with opoa==no
colsplit <- rbind(data.frame(var=paste("odiag",1:24, sep=""), var="odiag", number=1:24), data.frame(var=paste("opoa",1:24, sep=""), var="opoa", number=1:24))

# Join dx data with split column names
dx <- dx %>%
  left_join(colsplit) %>%
  select(-var) %>%
  rename(var = var.1)

rm(colsplit)

# Made a DF of just the visitId and numbers associated with diagnoses NOT Present on Admission
temp <- dx[dx$value=="No",c("rln", "admtdate","number")]

# drop NAs
temp <- temp[!is.na(temp$number),]
# Assign a flag for drops
temp$drop <- TRUE

# Merge working list of ICD9s with temp DF to identify rows to drop, drop them, and simplify DF
dx <- dx %>%
  left_join(temp) %>%
  filter(is.na(drop)) %>%
  filter(var == "odiag") %>%
  select(rln, admtdate, icd_code = value) %>%
  filter(!is.na(icd_code))

rm(temp)

dx <- rbind(dx, diag_p)
rm(diag_p)

################# USE NEW FUNCTION FROM ICD HERE#################

load("data/icd_map_cc.RData")
load("data/icd_map_cc_hcc.RData")

# add column for ICD (all data in this example are ICD9)
dx$icdversion <- 9

# Convert date and add column for year
dx$admtdate <- as.Date(dx$admtdate)
dx$year <- as.numeric(format(dx$admtdate, "%Y"))

# merge CCs to patient data based on ICD/year/version, and drop ICD info
dx <- merge(dx, icd_map_cc, all.x = T)

# Convert CC to numeric and drop those with missing CC (not all ICDs resolve to a CC by definition)
dx <- dx[!is.na(dx$cc),]
dx$cc <- as.numeric(dx$cc)

# keep id, admtdate, and cc columns only
dx <- dx[,c("rln", "admtdate", "year", "cc")]

# Keep only unique records (multiple ICDs for a patient can resolve to same CC)
# VERY SLOW consider multidplyr
dx <- unique(dx)

#############################################
######### Apply Hierarchies
#############################################
hierarchy <- icd_map_cc_hcc

# Duplicate ifcc column into CC column to merge with pt data, keep dup column
hierarchy$cc <- hierarchy$ifcc

# Merge hierarchy rules with patient data
dx <- merge(dx, hierarchy, all.x = TRUE)

todrop <- list()

# create a list of dataframes that contain the CCs that will be zero'd out
for (i in 1:6) {
  todrop[[i]] <- dx[!is.na(dx$ifcc),c(3, 4, 5 + i)]
}
rm(i)

# rename all dfs in list to same column names, rbind into one df
todrop <- lapply(1:length(todrop), function(x) {
  colnames(todrop[[x]]) <- c("rln", "admtdate", "cc")
  return(todrop[[x]])
}
)

todrop <- do.call(rbind, todrop)

# remove all NAs from CC field
todrop <- todrop[!is.na(todrop$cc),]

# set flag, all of these CCs will be dropped
todrop$todrop <- T

# merge drop flags with pt data
dx <- merge(dx, todrop, all.x = T)
rm(todrop)

# drop flagged patients and keep columns of interest
dx <- dx[is.na(dx$todrop), ]
dx <- dx[,c("rln", "admtdate", "cc")]
colnames(dx) <- c("rln", "admtdate", "hcc")


# Convert dx data to wide
dx <- dcast(dx, rln + admtdate ~ hcc)

# rename column names to start with hcc
names(dx) <- str_replace(names(dx), "([0-9]+)", "hcc_\\1")

# Convert HCCs into True/False
# make a DF of just the HCCs
dxtf <- dx[,3:length(dx)]

# Assign TRUE if there is a number in the HCC field
# This resulted from the dcast
dxtf <- data.frame(lapply(dxtf, function(x) x>0))
# If the HCC is NA, set it to false
dxtf <- data.frame(lapply(dxtf, function(x) !is.na(x)))

# Combine HCC T/F DF with the identifying columns (rln, admtdate)
dx <- cbind(dx[,1:2], dxtf)

rm(dxtf)

pt <- pt %>%
  select(rln, admtdate, cohort, isreadmit30dc, agyradm, sex, )

saveRDS(dx, file = "data/patient/tidy/dx.rds")



