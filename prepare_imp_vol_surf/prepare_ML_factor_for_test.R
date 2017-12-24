# read XGB factor combined results
xgb_factor <- readRDS("./xgb_factor_combined_res.rds")

# read previously generated df for implied roughness and implied vol surf anaylsis
df_final_complete_imp_rough <- read.csv("../imp_rough2017/results_df/imp_alpha_df_filtered_ATMskew_5_ts_3_month_threshold_15.csv")

df_final_complete_imp_surf <- read.csv("../Xie_2015/results_df/Xie_final_df.csv")


temp <- xgb_factor %>% filter(Date >= 200101) %>% select(PERMNO,Date,retadj_next, next_resid, target_ret_xgb_pred, target_resid_xgb_pred)

# joining with implied vol surf features
temp <- temp %>% left_join(df_final_complete_imp_surf, by = c("Date"="yearmonth","PERMNO"="PERMNO"))

# joining with implie rough features
# ideally this would be all the features and rows we could possibly calculate from option prices data. A lot of rows will have NA implied roughness but that is fine
final_df <- temp %>% left_join(df_final_complete_imp_rough %>% select(yearmonth, PERMNO, imp_alpha_avg_threshold, logME, BM.m, ret.6m, ret_sigma), 
                               by = c("Date"="yearmonth","PERMNO"="PERMNO"))


final_df <- final_df %>% select(-retadj_next, -next_resid)

write.csv(final_df, "./results_df/xgb_factor.csv", row.names = F)






# as.data.frame(final_df[143489,])
# df_final_complete_imp_surf %>% filter(PERMNO==88451, yearmonth==200711)
# df_final_complete_imp_rough %>% filter(PERMNO==88451, yearmonth==200711)
# 
# data_2007_11 <- read.csv("./2007_11.csv")
# 
# 
# sec_PERMNO_merged <- readRDS("../related_data/sec_PERMNO_merged.rds")
# 
# # close price merged with secid 
# sec_PERMNO_merged <- sec_PERMNO_merged %>% 
#   distinct(secid, .keep_all=T) %>% distinct(PERMNO, .keep_all=T) %>% select(PERMNO,secid)
# 
# sec_PERMNO_merged %>% filter(PERMNO==88451) %>% select(secid)
# 
# temp <- data_2007_11 %>% filter(secid ==106666,days <= 91, date=="30NOV2007")
# mean((temp %>% filter(delta%in% c(50,-50), days ==30) %>% select(impl_volatility))[,1])
