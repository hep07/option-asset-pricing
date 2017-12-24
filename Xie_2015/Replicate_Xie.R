################################################################
# load library
################################################################
library(dplyr)
library(tibble)
library(moments)

################################################################
# load data needed
################################################################
imp_rough_df <- readRDS("./imp_vol_suf_results/imp_vol_surf_features.rds")
crsp_daily <- readRDS("../related_data/crsp_daily.rds")
crsp_daily <- as_tibble(crsp_daily)

################################################################
# prepare for Fama-Macbeth
################################################################

# we can choose different start days but for now lets use just the maximal number of data points since FM procedure can take very few stocks
start_date <- min(imp_rough_df$date)
to_keep <- crsp_daily$date >= year(start_date) * 10000 + month(start_date) * 100 + day(start_date)
crsp_daily <- crsp_daily[to_keep,]

crsp_daily$date <- as.Date(as.character(crsp_daily$date), format = "%Y%m%d")
crsp_daily_preprocessed <- crsp_daily %>% select(date, PERMNO, RET, SHROUT, VOL, EXCHCD) %>% mutate(RET = as.numeric(as.character(RET)), year = year(date), month= month(date),
                                                                                            SHROUT = as.numeric(SHROUT)/1000, VOL = as.numeric(VOL)/1000) 
crsp_daily_preprocessed <- crsp_daily_preprocessed %>%
  group_by(year, month, PERMNO) %>% summarise(ret_sigma = sd(RET,na.rm=T), 
                                              ret_skew = skewness(RET, na.rm = T),
                                              turnover = sum(VOL)/mean(SHROUT,na.rm=T)/1000,
                                              exchcd = first(EXCHCD)) %>% ungroup()




                                              


#df <- imp_rough_df %>% left_join(crsp_daily %>% select(date, PERMNO, RET, SHROUT) %>% mutate(RET = as.numeric(RET)), by = c("date"="date", "PERMNO" = "PERMNO"))

df <- as_tibble(imp_rough_df) %>% mutate(year = year(date), month = month(date))



################################################################
# to-do: for the entire period or from something like 2004
# FM for weekly on crsp daily ret intersect imp_rough
# FM for weekly on crsp daily ret intersect imp_rough intersect FF monthly universe
# FM for monthly on intersect imp_rough intersect FF monthly universe
# boosting for weekly or monthly
################################################################


###################################################################
# monthly return 
###################################################################

###################################################################
# filtering based on modified XZZ rules and average for 
###################################################################



# # FM for monthly on intersect imp_rough intersect FF monthly universe
final_ccm_data <- readRDS("../related_data/final_ccm_data.rds")


#saveRDS(final_ccm_data[,2:ncol(final_ccm_data)], "D:/Dropbox/AssetPricingPy/FF1992/WC_Rreplication/final_ccm_data.rds")

final_ccm_data$Date[1]

final_ccm_data <- final_ccm_data[final_ccm_data$Date>=199500,]

final_ccm_data <- final_ccm_data %>% mutate(year= as.integer(Date/100), month = Date - year*100)

final_ccm_data_to_merge <- final_ccm_data %>% select(Date, year, month, PERMNO, ME, BM.m, retadj.1mn) %>% group_by(PERMNO) %>% 
  arrange(PERMNO,Date) %>%
  mutate(retadj.nextmonth = lead(retadj.1mn,1)) %>% mutate(ret.6m = (retadj.1mn+1)*(lag(retadj.1mn,1)+1)*(lag(retadj.1mn,2)+1)*(lag(retadj.1mn,3)+1)*(lag(retadj.1mn,4)+1)*
                                                             (lag(retadj.1mn,5)+1)-1,
                                                           ret.12m = (retadj.1mn+1) *(lag(retadj.1mn,1)+1)*(lag(retadj.1mn,2)+1)*(lag(retadj.1mn,3)+1)*(lag(retadj.1mn,4)+1)*
                                                             (lag(retadj.1mn,5)+1)*(lag(retadj.1mn,6)+1)*(lag(retadj.1mn,7)+1)*(lag(retadj.1mn,8)+1)*
                                                             (lag(retadj.1mn,9)+1)*(lag(retadj.1mn,10)+1)*(lag(retadj.1mn,11)+1)-1) %>% ungroup()


# calculate features from imp rough df
df_processed <- df %>%group_by(year, month, PERMNO) %>% arrange(PERMNO, date)  %>%
  summarise(XZZ_moneyness_skew_tau30 = last(XZZ_moneyness_skew_tau30),
            PC_avg_ATM_vol_slope = last(PC_avg_ATM_vol_slope),
            XZZ_ATM_C_imp_vol_tau30= last(XZZ_ATM_C_imp_vol_tau30),
            XZZ_moneyness_skew_slope = last(XZZ_moneyness_skew_slope)) %>% ungroup


# merge further with calculated features from crsp daily data
df_processed <- df_processed %>% left_join(crsp_daily_preprocessed, by = c("year"="year", "month"="month", "PERMNO"="PERMNO")) 

# merge ccm data with imp rough data


df_processed <- df_processed %>% left_join(final_ccm_data_to_merge, 
                                           by = c("year"="year", "month"="month", "PERMNO"="PERMNO"))  %>% select(-retadj.1mn) 


df_final <- df_processed %>% 
  mutate(yearmonth = year*100 + month) %>%  filter(exchcd %in% c(1,2,3)) %>% select(yearmonth, PERMNO, XZZ_moneyness_skew_tau30, XZZ_ATM_C_imp_vol_tau30,
                                                                                    XZZ_moneyness_skew_slope,
                                                                                    PC_avg_ATM_vol_slope,
                                                                                    retadj.nextmonth) %>% drop_na()

# Xie table 4.1
Chen_thesis_table4_1 <- t(sapply(df_final  %>% filter(yearmonth<=201312)%>% select(-c(yearmonth, PERMNO)), function(x) {
  res <- c(mean = mean(x, na.rm=T), sd = sd(x,na.rm=T))
  res_2 <- sapply(c(0.05,0.25,0.5,0.75,0.95), function(q) quantile(x,q,na.rm=T))
  names(res_2) <- paste("q_", c(0.05,0.25,0.5,0.75,0.95), sep="")
  c(res,res_2)
}))

nrow(df_final  %>% filter(yearmonth<=201312))/216
# for_table1 <- df %>% left_join(crsp_daily %>% select(date, PERMNO, RET, SHROUT, VOL, EXCHCD), by = c("date", "PERMNO"="PERMNO"))
# table1 <- for_table1 %>% filter(EXCHCD %in% c(1,2,3), PERMNO %in%  unique(final_ccm_data$PERMNO))
# sapply(table1$imp_vol_slope, summary)
# summary(table1$imp_vol_slope)
# mean(table1$imp_vol_slope, na.rm=T)
# sd(table1$imp_vol_slope, na.rm=T)
# sapply(c(0.05,0.25,0.5,0.75,0.95), function(q) quantile(table1$imp_vol_slope,q,na.rm=T))

write.csv(df_final, "./results_df/Xie_final_df.csv", row.names = F)
# Run Fama-Macbeth regression as mean group 
# library(plm)


#########################################################################################
# FM regression main results from Xie
#########################################################################################
# FM_imp_vol_slope <- pmg(formula = retadj.nextmonth~imp_vol_slope, 
#                             data =df_final[df_final$yearmonth<=201312,], index=c("yearmonth","PERMNO"))
# FM_imp_vol_slope_res <- summary(FM_imp_vol_slope)
# FM_imp_vol_slope_res
# 
# FM_imp_vol_Xie <- pmg(formula = retadj.nextmonth~imp_vol_slope + ATM_skew + ATM_IV, 
#                         data =df_final[df_final$yearmonth<=201612,], index=c("yearmonth","PERMNO"))
# FM_imp_vol_Xie_res <- summary(FM_imp_vol_Xie)
# FM_imp_vol_Xie_res
# 
# 
# FM_ATM_skew_slope <- pmg(formula = retadj.nextmonth~ATM_skew_slope, 
#                         data =df_final[df_final$yearmonth<=201612,], index=c("yearmonth","PERMNO"))
# FM_ATM_skew_slope_res <- summary(FM_ATM_skew_slope)
# FM_ATM_skew_slope_res
# 
# FM_ATM_skew_slope_Xie <- pmg(formula = retadj.nextmonth~imp_vol_slope + ATM_skew + ATM_IV + ATM_skew_slope, 
#                       data =df_final[df_final$yearmonth<=201612,], index=c("yearmonth","PERMNO"))
# FM_ATM_skew_slope_Xie_res <- summary(FM_ATM_skew_slope_Xie)
# FM_ATM_skew_slope_Xie_res
# 
# 
# 
# 
# FM_all_threshold <- pmg(formula = retadj.nextmonth~imp_alpha_avg + XZZ_skew + imp_vol_slope + ATM_skew_slope + tot_option_VOL +
#                           tot_open_interest+ ret_sigma + ret_skew + turnover + Pvol + BM.m + ret.6m + logME + ret.12m, 
#                         data =df_final_complete_no_threshold[df_final_complete_no_threshold$yearmonth>=199701,], index=c("yearmonth","PERMNO"))
# FM_res_all_threshold_1997 <- summary(FM_all_threshold)
# 
# FM_all_threshold <- pmg(formula = retadj.nextmonth~imp_alpha_avg + XZZ_skew + imp_vol_slope + ATM_skew_slope + tot_option_VOL +
#                           tot_open_interest+ ret_sigma + ret_skew + turnover + Pvol + BM.m + ret.6m + logME + ret.12m, 
#                         data =df_final_complete_no_threshold[df_final_complete_no_threshold$yearmonth>=200001,], index=c("yearmonth","PERMNO"))
# FM_res_all_threshold_2000 <- summary(FM_all_threshold)
# 
# FM_all_threshold <- pmg(formula = retadj.nextmonth~imp_alpha_avg + XZZ_skew + imp_vol_slope + ATM_skew_slope + tot_option_VOL +
#                           tot_open_interest+ ret_sigma + ret_skew + turnover + Pvol + BM.m + ret.6m + logME + ret.12m, 
#                         data =df_final_complete[df_final_complete$yearmonth>=200601,], index=c("yearmonth","PERMNO"))
# FM_res_all_threshold_2004 <- summary(FM_all_threshold)
# 
# 
# 
# FM_threshold <- pmg(formula = retadj.nextmonth~imp_alpha_avg_threshold, 
#                     data =df_final_complete[df_final_complete$yearmonth>=200001,], index=c("yearmonth","PERMNO"))
# FM_res_threshold <- summary(FM_threshold)
# FM_res_threshold
# 
# 
# FM_all_threshold <- pmg(formula = retadj.nextmonth~imp_alpha_avg_threshold + XZZ_skew + imp_vol_slope + ATM_skew_slope + tot_option_VOL +
#                           tot_open_interest+ ret_sigma + ret_skew + turnover + Pvol + BM.m + ret.6m + logME + ret.12m, 
#                         data =df_final_complete[df_final_complete$yearmonth>=199701,], index=c("yearmonth","PERMNO"))
# FM_res_all_threshold_1997 <- summary(FM_all_threshold)
# 
# FM_all_threshold <- pmg(formula = retadj.nextmonth~imp_alpha_avg_threshold + XZZ_skew + imp_vol_slope + ATM_skew_slope + tot_option_VOL +
#                     tot_open_interest+ ret_sigma + ret_skew + turnover + Pvol + BM.m + ret.6m + logME + ret.12m, 
#                     data =df_final_complete[df_final_complete$yearmonth>=200001,], index=c("yearmonth","PERMNO"))
# FM_res_all_threshold_2000 <- summary(FM_all_threshold)
# 
# FM_all_threshold <- pmg(formula = retadj.nextmonth~imp_alpha_avg_threshold + XZZ_skew + imp_vol_slope + ATM_skew_slope + tot_option_VOL +
#                           tot_open_interest+ ret_sigma + ret_skew + turnover + Pvol + BM.m + ret.6m + logME + ret.12m, 
#                         data =df_final_complete[df_final_complete$yearmonth>=200401,], index=c("yearmonth","PERMNO"))
# FM_res_all_threshold_2004 <- summary(FM_all_threshold)
# 
# #saveRDS(FM_res_all_threshold, "imp_filtered_FM_res.rds")
# 
# FM_threshold <- pmg(formula = retadj.nextmonth~imp_alpha_avg_threshold, 
#                     data =df_final_complete[df_final_complete$yearmonth>=200501,], index=c("yearmonth","PERMNO"))
# FM_res_threshold <- summary(FM_threshold)
# FM_res_threshold
# 
# 
# FM_all_threshold <- pmg(formula = retadj.nextmonth~imp_alpha_avg_threshold + XZZ_skew + imp_vol_slope + tot_option_VOL +
#                           tot_open_interest+ ret_sigma + ret_skew + turnover + Pvol + BM.m + ret.6m + logME + ret.12m, 
#                         data =df_final_complete[df_final_complete$yearmonth>=200501,], index=c("yearmonth","PERMNO"))
# FM_res_all_threshold <- summary(FM_all_threshold)
# FM_res_all_threshold
# 
# 
# ############################
# # main results analysis
# ############################
# 
# # prediction scale
# 12*(summary(df$alpha)[5] - summary(df$alpha)[2])* 
#   FM_res_all_threshold$coefficients[2]
# 
# # on average number of stocks in the universe per month 
# signal_universe <- unique(df_final_complete$PERMNO)
# length(signal_universe)
# check_availability
# mean(check_availability$count_with_imp_rough[check_availability$yearmonth>=199701])
# 
# 
# saveRDS(list(check_availability = check_availability, FM_res_all_threshold_2000 = FM_res_all_threshold_2000,
#              FM_res_all_threshold_2004 = FM_res_all_threshold_2004,
#              FM_res_all_threshold_1997 = FM_res_all_threshold_1997,
#              pred_75_25 = 12*(summary(df$alpha)[5] - summary(df$alpha)[2])* 
#                FM_res_all_threshold$coefficients[2]), "imp_filtered_FM_res.rds")
# 
# 
# readRDS("./")
# # to do, replicate XXZ, use Machine learning to predict s
