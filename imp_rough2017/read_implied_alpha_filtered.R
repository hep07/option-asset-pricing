library(dplyr)
library(tidyr)
result_file_names <- list.files("./imp_rough_results_1")
res <- lapply(result_file_names, function(f) readRDS(paste("./imp_rough_results_1/", f, sep="")))

# temp <- list()
# temp[[1]] <- readRDS("2011_3_imp_alpha.rds")
# temp[[2]] <- readRDS("2011_3_imp_alpha.rds")
res <- do.call(rbind, res)


# remove NA caused by only 1 valid point on the ATM skew term structure
res <- res %>% drop_na() 


# unfiltered version
temp <- res %>% select(date, PERMNO, VOL, ATM_skew, ATMC_imp_vol, XZZ_skew, imp_vol_slope, ATM_skew_slope, tot_option_VOL, tot_open_interest,alpha, P_alpha)
write.csv(temp, "imp_rough_df_filtered_threshold2.csv", row.names = F)
saveRDS(res, "imp_rough_results_filtered_threshold2.rds")


# filtered version: 
# 1) alpha must have at least 3 data points on the term structure
# 2) for each month at least 10 trading days have data available for alpha in order to be included 


threshold_ATM_termstructure <- 3
res$P_alpha[res$P_num_ATM_skew<threshold_ATM_termstructure] <- NA
res$C_alpha[res$C_num_ATM_skew<threshold_ATM_termstructure] <- NA 
summary(res$alpha)
res$alpha <- rowMeans(cbind(res$P_alpha, res$C_alpha), na.rm = T)



temp <- res %>% select(date, PERMNO, VOL, ATM_skew, ATMC_imp_vol, XZZ_skew, imp_vol_slope, ATM_skew_slope, tot_option_VOL, tot_open_interest,alpha, P_alpha)
write.csv(temp, "imp_rough_df_filtered_threshold3.csv", row.names = F)
saveRDS(res, "imp_rough_results_filtered_threshold3.rds")


threshold_ATM_termstructure <- 4
res$P_alpha[res$P_num_ATM_skew<threshold_ATM_termstructure] <- NA
res$C_alpha[res$C_num_ATM_skew<threshold_ATM_termstructure] <- NA 
summary(res$alpha)
res$alpha <- rowMeans(cbind(res$P_alpha, res$C_alpha), na.rm = T)



temp <- res %>% select(date, PERMNO, VOL, ATM_skew, ATMC_imp_vol, XZZ_skew, imp_vol_slope, ATM_skew_slope, tot_option_VOL, tot_open_interest,alpha, P_alpha)
write.csv(temp, "imp_rough_df_filtered_threshold4.csv", row.names = F)
saveRDS(res, "imp_rough_results_filtered_threshold4.rds")

threshold_ATM_termstructure <- 5
res$P_alpha[res$P_num_ATM_skew<threshold_ATM_termstructure] <- NA
res$C_alpha[res$C_num_ATM_skew<threshold_ATM_termstructure] <- NA 
summary(res$alpha)
res$alpha <- rowMeans(cbind(res$P_alpha, res$C_alpha), na.rm = T)



temp <- res %>% select(date, PERMNO, VOL, ATM_skew, ATMC_imp_vol, XZZ_skew, imp_vol_slope, ATM_skew_slope, tot_option_VOL, tot_open_interest,alpha, P_alpha)
write.csv(temp, "imp_rough_df_filtered_threshold5.csv", row.names = F)
saveRDS(res, "imp_rough_results_filtered_threshold5.rds")