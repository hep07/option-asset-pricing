#!/apps/R-3.4.0/bin/Rscript



args = commandArgs(trailingOnly=TRUE)
library(dplyr)
library(lubridate)
library(tidyr)
source("../helper_functions/Hurst_estimation_helper.R")

qVec <- c(0.5,1,1.5,2,3)

year_ <- as.numeric(args[1])
month_ <- as.numeric(args[2])

max_lag <- as.numeric(args[3])
nonNA_threshold <- as.numeric(args[4])
rolling_window_length <- as.numeric(args[5])
lagVec <- 1:max_lag


# lagVec_short <- 1:10
# rolling_window_length <- 3# use 3 month rolling window

delta50_ATM_imp_vol <- readRDS("./combined_df/delta50_ATM_imp_vol.rds")

# H_panel <- do.call(rbind,lapply(1996:2016, function(year_) {
#   temp <- do.call(rbind,lapply(1:12, function(month_) {
#     print(year_)
#     print(month_)
#    estimate_Hurst_rolling(delta50_ATM_imp_vol, year_, month_, rolling_window_length, qVec, lagVec, nonNA_threshold)
#   }))  
# }))

out <- estimate_Hurst_rolling(delta50_ATM_imp_vol, year_, month_, rolling_window_length, qVec, lagVec, nonNA_threshold)
print(dim(out))
print(out)
saveRDS(out, paste("./Hurst_rolling_",rolling_window_length,"_lag_", max_lag, "_nonNAthreshold_", nonNA_threshold,
                   "/",year_,"_", month_, ".rds",sep=""))