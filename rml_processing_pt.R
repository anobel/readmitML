library(data.table)
library(dplyr)

# Import OSHPD data into a list of dataframes
pt <- apply(data.frame(paste("data/patient/raw/oshpd/",list.files("data/patient/raw/oshpd/"),sep="")), 1, FUN=fread, na.strings=c(""), header=TRUE, stringsAsFactors=TRUE)

# row binds all the dataframes in the list into one frame
pt <- rbind_all(pt)

saveRDS(pt, file="data/patient/raw/ptraw.rds")