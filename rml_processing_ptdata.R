##################
#### Load Packages
##################

# Data Management Packages
library(data.table)
library(stringr)
library(tidyr)
library(dplyr)
library(reshape2)
library(lubridate)

# Specialty Packages
library(icd)

##################
#### Defined Data
##################
codes <- list()

### Diagnosis Codes
# Bladder Cancer ICD9 Diagnoses Codes
codes$DxBladderCa <- c(icd9Children("188"), "2337", "2339", "2367", "2394")

# Kidney Cancer ICD9 Codes (Miller DC, Gore JL)
codes$DxKidneyCa <- c("189", "1890", "1891", "1898", "1899",
                      "1580", "1715",
                      "1940", "1952", "1976", "1980", "1981", "19889", "1991",
                      "2239",
                      "2339", "2354", "2369", "23690", "23691", "23699", "2372", "2381", "2388", "2395", "2399")

# Prostate Cancer ICD9 Diagnosis Codes
codes$DxProstateCa <- c("185", "1850")

# Testis Cancer ICD9 Diagnosis Codes (Mossanen M 2014)
codes$DxTestisCa <- c("186", "1860", "1869", "1580", "158","1976", "2118", "2354")

### Procedure Codes
# Radical Nephrectomy Procedure Codes (DCM, JLG)
codes$SxRadNx <- c("5551", "5552", "5554")

# Partial Nephrectomy Procedure Codes (DCM, JLG)
codes$SxPartialNx <- c("5501", "5524", "5531", "5539", "554", "5540", "5581", "5589", "5591",
                       "5902", "5909", "5921")

# Radical Cystectomy Procedure Codes
codes$SxCystectomy <- c("577", "5771", "5779")

# Radical Prostatectomy Procedure Codes
codes$SxRP <- c("605", "6050")

# RPLND Procedure Codes (Mossanen 2014)
codes$SxRPLND <- c("590", "5900", "5902", "5909", "4029", "403", "4052", "4059")

# Codes for lap or robot
codes$minimal <- c("1742", "1743", "1744", "1749", "5421")

##################
#### Import Data
##################
# Import OSHPD data into a list of dataframes
pt <- apply(data.frame(paste("data/patient/raw/oshpd/",list.files("data/patient/raw/oshpd/"),sep="")), 1, FUN=fread, na.strings=c(""), header=TRUE, stringsAsFactors=TRUE)

# row binds all the dataframes in the list into one frame
pt <- rbind_all(pt)

saveRDS(pt, file="data/patient/raw/ptraw.rds")

# Use the combined data as RDS
pt <- readRDS("data/patient/raw/ptraw.rds")
# place columns in alphabetical order (ignore incorrect numerical ordering)
pt <- pt[,sort(colnames(pt))]

# make a smaller subset to make it easier to work with (random sample of 100k rows)
# In future, can just remove this line for full analysis
# pt <- droplevels(pt[sample(1:nrow(pt), 10^4, replace=F),])

##################
#### Data Cleaning
##################
# Drop if RLN is missing (based on Social Security Number, so patients without SSN have missing RLN)
# Cannot track outcomes for these patients. May introduce bias.
pt <- pt %>% filter(rln!="---------")

# Drop some of the fields not used in this analysis
pt <- pt %>% select(-pls_id, -pls_wrtin, -pls_abbr, -race, -race_grp, -ethncty,
                    -charge, -mdc, -msdrg, -drg, -sev_code, 
                    -pay_plan, -pay_cat, -pay_type, -cat_code
                    -hplcnty, -patcnty,
                    -agyrdsch, -bthdate)

# Format all dates to POSIX
# make a vector identifying the date fields
d <- c("admtdate", "dschdate", "proc_pdt", paste("procdt", 1:20, sep=""))

# Convert date fields from factor to character, replace in dataframe
pt[,d] <- as.data.frame(lapply(pt[,d], as.character), stringsAsFactors = F)

# Convert date fields from character to POSIXct using lubridate, replace in dataframe
pt[,d] <- as.data.frame(lapply(pt[,d], parse_date_time, orders="mdY"))

# Drop if admit date is prior to 2006, these must be data entry errors
pt <- pt[year(pt$admtdate)>2005,]

# Clean workspace
rm(d)

# Admission Type
pt$admtype <- factor(pt$admtype, levels=c(1,2,3,4,0), labels=c("Scheduled", "Unscheduled", "Infant", "Unknown", "Blank"))
pt <- pt %>% filter(admtype!="Infant")

# Licensure of Hospital
pt$typcare <- factor(pt$typcare, levels = c(0, 1, 3, 4, 5, 6), labels=c("Blank", "Acute Care", "SNF", "Psychiatric", "Drug Rehab", "Physical Rehab"))

# Disposition
pt$disp <- factor(pt$disp, levels=0:13, labels=c("Invalid", "Home", "Acute Care", "Other Care Level", "SNF", "Acute-Other Facility", "Other Care Level-Other Facility", "SNF-Other Facility", "Residential Care", "Incarcerated", "AMA", "Died", "Home Health", "Other"))

### Demographics
# Sex
# Keep only male/female categories
pt <- pt %>% filter(sex<3)
# factor sex variable
pt$sex <- factor(pt$sex, levels=1:2, labels=c("Male", "Female"))

# Was Diagnosis Present On Admission?
# Principal Diagnosis Present on Admission
pt$poa_p <- factor(pt$poa_p, levels = c("Y", "N", "E", "U", "W", "0"), labels=c("Yes", "No", "Exempt", "Unknown", "Clinically Undetermined", "Invalid"))

# Other Diagnoses Present on Admission
# create vector for all 24 Other Present on Admission diagnosis fields
opoa <- paste("opoa", 1:24, sep = "")

# Relabel Factors
pt[, opoa] <- as.data.frame(lapply(pt[, opoa], factor, levels = c("Y", "N", "E", "U", "W", "0"), labels=c("Yes", "No", "Exempt", "Unknown", "Clinically Undetermined", "Invalid")))
rm(opoa)

# create visitId variable (combining RLN and admission date) for assigning Elixhauser codes
pt$visitId = paste(pt$rln, pt$admtdate, sep = "_")
gc()

###################################
#### Assign Elixhauser Comorbidity
###################################

# create vector listing just the fields with diagnosis codes
diags <- c("diag_p", paste("odiag", 1:24,sep = ""))
odiags <- c(paste("odiag", 1:24, sep = ""))
opoas <- c(paste("opoa", 1:24, sep = ""))

# Calculate the total number of listed ICD9 diagnoses per patient
pt$totaldx <- apply(pt[, diags], 1, function(x) sum(!is.na(x)))

# Subset the "Other Diagnoses" (everything except the principal diagnosis), and their corresponding POA fields
elix <- pt[, c(odiags, opoas)]

# Convert factors to characters, combine with visitIds
elix <- as.data.frame(lapply(elix, as.character), stringsAsFactors = F)
elix <- cbind(visitId = pt$visitId, elix)

# Need to drop all "Other Diagnoses" that were NOT Present on Admission
# Convert from wide to long format, identify and drop all diagnoses that were not present on admission
# Then add principal diagnosis and calculate Elixhauser before merging with main data

# Convert wide to long and rename, factor columns
elix <- gather(elix, visitId, value, na.rm = T)
colnames(elix) <- c("visitId", "var", "value")
elix$value <- factor(elix$value)

# Split the odiag1-24 and opoa1-24 columns into two so that I can identify the number associated with opoa==no
colsplit <- rbind(data.frame(var = paste("odiag", 1:24, sep = ""), var = "odiag", number = 1:24), data.frame(var = paste("opoa", 1:24, sep = ""), var = "opoa", number = 1:24))

# Join elix data with split column names
elix <- elix %>%
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

# Prepend all column names with elix_ to denote charlson-deyo
colnames(elix)[1:(length(elix)-2)] <-   paste("elix_", colnames(elix)[1:(length(elix)-2)], sep="")

# Merge with main data
pt <- left_join(pt, elix)

# Clean Up Environment
rm(elix, diags, odiags, opoas)
gc()

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
temp <- charlson[charlson$value=="No",c("visitId","number")]

# drop NAs
temp <- temp[!is.na(temp$number),]
# Assign a flag for drops
temp$drop <- TRUE

# Merge working list of ICD9s with temp DF to identify rows to drop, drop them, and simplify DF, prep for icd9ComorbidElix()
charlson <- charlson %>%
  left_join(temp) %>%
  filter(is.na(drop)) %>%
  filter(var=="odiag") %>%
  select(visitId, icd9=value) %>%
  filter(!is.na(icd9))

rm(temp)

# diag_p: bring in primary diagnoses
diag_p <- pt[,c("visitId", "diag_p")]
colnames(diag_p) <- c("visitId", "icd9")
charlson <- rbind(charlson, diag_p)
rm(diag_p)

# based on ICD9s for each patient/admission, excluding ICDs NOT Present on Admission,
# make matrix of all 30 Elixhauser categories (T/F)
charlson <- as.data.frame(icd_comorbid_quan_deyo(charlson, visitId="visitId"))

# add visitId as index and drop rownames
charlson$visitId <- rownames(charlson)
row.names(charlson) <- NULL

# Sum the total number of positive Elixhauser categories per patient, add at end of DF)
charlson <- cbind(charlson, charlson = rowSums(charlson[-length(charlson)]))

# Prepend all column names with cd_ to denote charlson-deyo
colnames(charlson)[1:(length(charlson) - 2)] <-   paste("cd_", colnames(charlson)[1:(length(charlson) - 2)], sep = "")

# Merge with main data
pt <- left_join(pt, charlson)

# Clean Up Environment
# drop visitID
pt <- pt %>% select(-visitId)
rm(charlson, diags, odiags, opoas)

# Save as backup
# saveRDS(pt, file = "data/patient/temp/ptelixcharlson.rds")

#####################################
#### Assign HCCs
#####################################
# Data must be first formatted/prepared for analysis
# isolate primary diagnoses (must always be present on admission), and will add back later
diag_p <- pt[,c("rln", "admtdate", "diag_p")]
colnames(diag_p) <- c("rln", "admtdate", "icd_code")

# Create vectors to identify other diagnoses and present on admission column names
odiags <- paste("odiag",1:24,sep="")
opoas <- paste("opoa", 1:24, sep="")

# Limit DF for columns of interest
dx <- pt[,c("rln", "admtdate", odiags, opoas)]

# remove pt to free memory
rm(pt)

# convert from factors to character
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
rm(diag_p, opoas, odiags)

# saveRDS(dx, "data/patient/temp/dx.rds")
# In order to assign HCCs, have to split the data due to memory limits
# manually partition the dx records into sets of 10m rows
# write to disk and remove from memory
# this whole process is not pretty but running to memory limitations
dx1 <- dx[1:10000000,]
saveRDS(dx1, "data/patient/temp/dx1.rds")
rm(dx1)

dx2 <- dx[10000001:20000000,]
saveRDS(dx2, "data/patient/temp/dx2.rds")
rm(dx2)

dx3 <- dx[20000001:30000000,]
saveRDS(dx3, "data/patient/temp/dx3.rds")
rm(dx3)

dx4 <- dx[30000001:40000000,]
saveRDS(dx4, "data/patient/temp/dx4.rds")
rm(dx4)

dx5 <- dx[40000001:50000000,]
saveRDS(dx5, "data/patient/temp/dx5.rds")
rm(dx5)

dx6 <- dx[60000001:70000000,]
saveRDS(dx6, "data/patient/temp/dx6.rds")
rm(dx6)

dx7 <- dx[70000001:80000000,]
saveRDS(dx7, "data/patient/temp/dx7.rds")
rm(dx7)

dx8 <- dx[80000001:90000000,]
saveRDS(dx8, "data/patient/temp/dx8.rds")
rm(dx8)

dx9 <- dx[90000001:100000000,]
saveRDS(dx9, "data/patient/temp/dx9.rds")
rm(dx9)

dx10 <- dx[100000001:110000000,]
saveRDS(dx10, "data/patient/temp/dx10.rds")
rm(dx10)

dx11 <- dx[110000001:nrow(dx),]
saveRDS(dx11, "data/patient/temp/dx11.rds")
rm(dx11)

# Remove dx data (the object with all 114m rows)
rm(dx)

# reimport each shard, process, save as backup
dx1 <- readRDS("data/patient/temp/dx1.rds")
dx1 <- icd_comorbid_hcc(dx1, date_name = "admtdate", visit_name = "rln")
saveRDS(dx1, file="data/patient/temp/dx1_result.rds")

dx2 <- readRDS("data/patient/temp/dx2.rds")
dx2 <- icd_comorbid_hcc(dx2, date_name = "admtdate", visit_name = "rln")
saveRDS(dx2, file="data/patient/temp/dx2_result.rds")

dx3 <- readRDS("data/patient/temp/dx3.rds")
dx3 <- icd_comorbid_hcc(dx3, date_name = "admtdate", visit_name = "rln")
saveRDS(dx3, file="data/patient/temp/dx3_result.rds")

dx4 <- readRDS("data/patient/temp/dx4.rds")
dx4 <- icd_comorbid_hcc(dx4, date_name = "admtdate", visit_name = "rln")
saveRDS(dx4, file="data/patient/temp/dx4_result.rds")

dx5 <- readRDS("data/patient/temp/dx5.rds")
dx5 <- icd_comorbid_hcc(dx5, date_name = "admtdate", visit_name = "rln")
saveRDS(dx5, file="data/patient/temp/dx5_result.rds")

dx6 <- readRDS("data/patient/temp/dx6.rds")
dx6 <- icd_comorbid_hcc(dx6, date_name = "admtdate", visit_name = "rln")
saveRDS(dx6, file="data/patient/temp/dx6_result.rds")

dx7 <- readRDS("data/patient/temp/dx7.rds")
dx7 <- icd_comorbid_hcc(dx7, date_name = "admtdate", visit_name = "rln")
saveRDS(dx7, file="data/patient/temp/dx7_result.rds")

dx8 <- readRDS("data/patient/temp/dx8.rds")
dx8 <- icd_comorbid_hcc(dx8, date_name = "admtdate", visit_name = "rln")
saveRDS(dx8, file="data/patient/temp/dx8_result.rds")

dx9 <- readRDS("data/patient/temp/dx9.rds")
dx9 <- icd_comorbid_hcc(dx9, date_name = "admtdate", visit_name = "rln")
saveRDS(dx9, file="data/patient/temp/dx9_result.rds")

dx10 <- readRDS("data/patient/temp/dx10.rds")
dx10 <- icd_comorbid_hcc(dx10, date_name = "admtdate", visit_name = "rln")
saveRDS(dx10, file="data/patient/temp/dx10_result.rds")

dx11 <- readRDS("data/patient/temp/dx11.rds")
dx11 <- icd_comorbid_hcc(dx11, date_name = "admtdate", visit_name = "rln")
saveRDS(dx11, file="data/patient/temp/dx11_result.rds")

# combine and save HCCs
dx <- rbind(dx1, dx2, dx3, dx4, dx5, dx6, dx7, dx8, dx9, dx10, dx11)
saveRDS(dx, "data/patient/temp/dx_hcc.RDS")
rm(dx1, dx2, dx3, dx4, dx5, dx6, dx7, dx8, dx9, dx10, dx11)

dx <- as.data.table(dx)

## THIS PORTION OF ANALYSIS IF RAM INTENSIVE
# Crashed on laptop w 16gb ram
# instead, run AWS r3.4xl (120gb ram)
# Convert HCC table to wide
dx <- dcast(dx, rln + admtdate ~ hcc)

# Rename column names to start with hcc
names(dx) <- gsub("([0-9]+)", "hcc_\\1", names(dx))

# Convert HCCs into True/False
# make a DF of just the HCCs
dx <- as.data.frame(dx)
dxtf <- dx[,c(3:length(dx))]

# Assign TRUE if there is a number in the HCC field
# This resulted from the dcast
dxtf <- data.frame(lapply(dxtf, function(x) x>0))

# Combine HCC T/F DF with the identifying columns (rln, admtdate)
dx <- cbind(dx[,1:2], dxtf)

# remove TF dataframe 
rm(dxtf)

# Merge HCCs back to PT data
pt <- readRDS("data/patient/temp/ptelixcharlson.rds")

pt <- pt %>%
  left_join(dx)

saveRDS(pt, "data/patient/temp/ptcomorbs.rds")

#####################################
#### Identify Readmissions
#####################################
# Will do these manipulations using parallel processing enabled by multidplyr package
library(multidplyr)

# set up 8 core cluster
cluster <- create_cluster(8)
set_default_cluster(cluster)

# Limit to the fields necessary for this calculation
# group by rln and arrange in preparation for splitting for parallel
readmit <- pt %>%
  select(rln, admtype, admtdate, dschdate, disp) %>%
  group_by(rln) %>%
  arrange(rln, admtdate)

# Partition the data into equal sized shards for parallelized calculation
readmit <- partition(readmit, rln)

# Assign Readmissions
# Must have been within 30/90 days of discharge date
# Exclude if discharge data and admission date the same (transfers)
# Does not count as readmission if it was a scheduled admission
# Exclude admissions from being eligible for readmission if the dispo was:
# AMA, Incarcerated, Died, Acute-Other Facility, Other Care Level-Other Facility
readmit <- readmit %>%
  mutate(readmitdaysdc = difftime(lead(admtdate),dschdate, units="days")) %>%
  mutate(within30dc = ifelse(readmitdaysdc<= 30 & readmitdaysdc!=0, T, F)) %>%
  mutate(isreadmit30dc = ifelse(within30dc==T & lead(admtype) !="Scheduled" & !(disp %in% c("AMA", "Incarcerated", "Died", "Acute-Other Facility", "Other Care Level-Other Facility")), T, F))

# Recombine
readmit <- collect(readmit)
rm(cluster)

# Any fields that were not tagged as readmits will be marked FALSE
readmit$within30dc[is.na(readmit$within30dc)] <- F
readmit$isreadmit30dc[is.na(readmit$isreadmit30dc)] <- F

# Merge readmit assignments back to main patient data
pt <- readmit %>%
  select(rln, admtdate, readmitdaysdc, within30dc, isreadmit30dc) %>%
  right_join(pt)
names(pt)
rm(readmit)

# Keep ONLY acute care visits (remove pychiatric/physical rehab admissions as allowable index admissions)
# Drop type care variable
pt <- pt %>% filter(typcare=="Acute Care") %>% select(-typcare)

saveRDS(pt, "data/patient/temp/pt_comorbs.rds")

#####################################
#### Procedure Specific Cohorts
#####################################
# Create vectors for future use
diags <- c("diag_p", paste("odiag",1:24,sep=""))
procs <- c("proc_p", paste("oproc",1:20,sep=""))

# Create empty DF with RLNs to populate
cohort <- as.data.frame(pt$rln)

# Assign Diagnoses
# Use lapply across all diagnosis fields (not just principal diagnosis)
# Assign 1 if matches something in codes vector, and then add up rowsums
cohort$DxBladderCa <- rowSums(as.data.frame(lapply(select(pt, one_of(c(diags))), function(x) x %in% codes$DxBladderCa)))
cohort$DxKidneyCa <- rowSums(as.data.frame(lapply(select(pt, one_of(c(diags))), function(x) x %in% codes$DxKidneyCa)))
cohort$DxProstateCa <- rowSums(as.data.frame(lapply(select(pt, one_of(c(diags))), function(x) x %in% codes$DxProstateCa)))
cohort$DxTestisCa <- rowSums(as.data.frame(lapply(select(pt, one_of(c(diags))), function(x) x %in% codes$DxTestisCa & pt$sex=="Male")))

# Use lapply across all procedure fields (not just principal procedure)
# Assign 1 if matches something in codes vector, and then add up rowsums
cohort$SxCystectomy <- rowSums(as.data.frame(lapply(select(pt, one_of(c(procs))), function(x) x %in% codes$SxCystectomy)))
cohort$SxRadNx <- rowSums(as.data.frame(lapply(select(pt, one_of(c(procs))), function(x) x %in% codes$SxRadNx)))
cohort$SxPartialNx <- rowSums(as.data.frame(lapply(select(pt, one_of(c(procs))), function(x) x %in% codes$SxPartialNx)))
cohort$SxRP <- rowSums(as.data.frame(lapply(select(pt, one_of(c(procs))), function(x) x %in% codes$SxRP)))
cohort$SxRPLND <- rowSums(as.data.frame(lapply(select(pt, one_of(c(procs))), function(x) x %in% codes$SxRPLND)))

# Assign cohorts in main patient dataframe
# Each patient can only be in one cohort (for now)
# Can create possibilities for multiple cohorts if necessary for other analyses
pt$cohort <- NA
pt$cohort[cohort$DxKidneyCa>0 & cohort$SxRadNx>0] <- "RadNx"
pt$cohort[cohort$DxKidneyCa>0 & cohort$SxPartialNx>0] <- "PartialNx"
pt$cohort[cohort$DxProstateCa>0 & cohort$SxRP>0 & pt$sex=="Male"] <- "RP"

# Assign cystectomy AFTER prostatectomy
# often cystoprostatectomy is coded as cystectomy AND prostatectomy, this way its assigned to cystectomy cohort
pt$cohort[cohort$DxBladderCa>0 & cohort$SxCystectomy>0] <- "Cystectomy"

# Limiting RPLND to Males only. Although it should be obvious from Dx codes for testis cancer,
# 198.82 "Secondary malignant neoplasm of genital organs" is often used for gyn malignancies (>1300 cases)
pt$cohort[cohort$DxTestisCa>0 & cohort$SxRPLND>0 & pt$sex=="Male"] <- "RPLND"

# Exclude if patients have the following combinations of surgeries AND diagnoses
# Cystectomy and RPLND, Bladder cancer AND Testis Cancer
pt$cohort[cohort$SxCystectomy>0 & cohort$DxBladderCa>0 & cohort$SxRPLND>0 & cohort$DxTestisCa>0] <- "Multiple GU Sx"

# Cystectomy AND Radical or Partial Nx, Bladder and Kidney Cancer
pt$cohort[cohort$SxCystectomy>0 & cohort$DxBladderCa>0 & cohort$DxKidneyCa>0 & (cohort$SxRadNx>0 |cohort$SxPartialNx>0)] <- "Multiple GU Sx"

# Prostate and Kidney cancer, RP and Radical or Partial Nx
pt$cohort[cohort$SxRP>0 & cohort$DxProstateCa>0 & cohort$DxKidneyCa>0 & (cohort$SxRadNx>0 |cohort$SxPartialNx>0)] <- "Multiple GU Sx"

# Testis and Kidney cancer, RPLND and Radical or Partial Nx
pt$cohort[cohort$SxRPLND>0 & cohort$DxTestisCa>0 & cohort$DxKidneyCa>0 & (cohort$SxRadNx>0 |cohort$SxPartialNx>0) & pt$sex!="Male"] <- "Multiple GU Sx"

# assign if case was Open or Lap/Robot
pt$open <- ifelse(
  rowSums(as.data.frame(lapply(select(pt, one_of(c(procs))), function(x) x %in% codes$minimal)))==0,
  T,F)

rm(cohort)

# save patient data
saveRDS(pt, file="data/patient/tidy/pt_all.rds")

# drop all diagnosis, procedure, and associated date fields
diags <- c("diag_p", paste("odiag", 1:24, sep = ""))
poa <- c("poa_p", paste("opoa", 1:24, sep = ""))
procs <- c("proc_p", paste("oproc", 1:20, sep = ""))
procdts <- c("proc_pdt", paste("procdt", 1:20, sep = ""))

pt <- pt[,!(names(pt) %in% c(diags, poa, procs, procdts))]
rm(diags, poa, procs, procdts)

# Create cohort of 250k random patients
set.seed(7)

# of the patients not assigned to a GU cohort,
# randomly sample 250k of htem
pt$cohort[sample(which(is.na(pt$cohort)),250000)] <- "Random"

# Save tidy patient data
saveRDS(pt, file="data/patient/tidy/pt.rds")

# Make subset with only the specified cohorts
# Excluding patients with multiple GU surgeries
pt_rml <- pt %>%
  filter(cohort!="Multiple GU Sx") %>%
  filter(!is.na(cohort))

saveRDS(pt_rml, file="data/patient/tidy/pt_rml.rds")