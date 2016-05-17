library(tidyr)
library(dplyr)

###################################
#### Assign Charlson-Deyo Comorbidity
###################################

# create vector listing just the fields with diagnosis codes
diags <- c("diag_p", paste("odiag",1:24,sep=""))
odiags <- c(paste("odiag",1:24,sep=""))
opoas <- c(paste("opoa",1:24,sep=""))

# Subset the "Other Diagnoses" (everything except the principal diagnosis), and their corresponding POA fields
charlson <- pt[,c(odiags, opoas)]

# Convert factors to characters, combine with visitIds 
charlson <- as.data.frame(lapply(charlson, as.character), stringsAsFactors = F)
charlson <- cbind(visitId = pt$visitId, charlson)

# Need to drop all "Other Diagnoses" that were NOT Present on Admission
# Convert from wide to long format, identify and drop all diagnoses that were not present on admission
# Then add principal diagnosis and calculate Elixhauser before merging with main data

# Convert wide to long and rename, factor columns
charlson <- gather(charlson, visitId, value, na.rm=T)
colnames(charlson) <- c("visitId", "var", "value")
charlson$value <- factor(charlson$value)

# Split the odiag1-24 and opoa1-24 columns into two so that I can identify the number associated with opoa==no
colsplit <- rbind(data.frame(var=paste("odiag",1:24, sep=""), var="odiag", number=1:24), data.frame(var=paste("opoa",1:24, sep=""), var="opoa", number=1:24))

# Join elix data with split column names
charlson <- charlson %>%
  left_join(colsplit) %>%
  select(-var) %>%
  rename(var = var.1)

rm(colsplit)

# Made a DF of just the visitId and numbers associated with diagnoses NOT Present on Admission
temp <- elix[elix$value=="No",c("visitId","number")]

# drop NAs
temp <- temp[!is.na(temp$number),]
# Assign a flag for drops
temp$drop <- TRUE

# Merge working list of ICD9s with temp DF to identify rows to drop, drop them, and simplify DF, prep for icd9ComorbidElix()
elix <- elix %>%
  left_join(temp) %>%
  filter(is.na(drop)) %>%
  filter(var=="odiag") %>%
  select(visitId, icd9=value) %>%
  filter(!is.na(icd9))

rm(temp)

# diag_p: bring in primary diagnoses
diag_p <- pt[,c("visitId", "diag_p")]
colnames(diag_p) <- c("visitId", "icd9")
elix <- rbind(elix, diag_p)
rm(diag_p)

# based on ICD9s for each patient/admission, excluding ICDs NOT Present on Admission,
# make matrix of all 30 Elixhauser categories (T/F)

elix <- as.data.frame(icd_comorbid_elix(elix, visitId="visitId", icd9Field="icd9"))

# add visitId as index and drop rownames
elix$visitId <- rownames(elix)
row.names(elix) <- NULL

# Sum the total number of positive Elixhauser categories per patient, add at end of DF)
elix <- cbind(elix, elixsum = rowSums(elix[-length(elix)]))

# Merge with main data
pt <- left_join(pt, elix)

# Clean Up Environment
rm(elix, diags, odiags, opoas)
