source("../helper_functions/imp-vol-other-factor-helper.R")
source("../helper_functions/implied_rough_helper.R")

# imp_rough_df_filtered_old <- readRDS("./imp_rough_res/skew_5/imp_rough_results_filtered_threshold3.rds")
# imp_rough_df_nonfiltered <- readRDS("./imp_rough_res/skew_5_skew_ts_2345_daily/imp_rough_df_filtered0_threshold3.rds")
# 
# imp_rough_df_filtered2 <- readRDS("./imp_rough_combined_res/imp_rough_df_filtered2_skewthreshold_4.rds")

# closest so far
imp_rough_df <- readRDS("./combined_df/imp_rough_results_2_3.rds")

imp_rough_df_best <- readRDS("../imp_rough_res_old_best_res/imp_rough_results_filtered_threshold3.rds")

res <- imp_rough_df_best %>% select(date,PERMNO, P_num_ATM_skew, P_alpha) %>% 
  full_join(imp_rough_df %>% select(date,PERMNO,PRC, P_num_ATM_skew, P_alpha), by = c("PERMNO"="PERMNO","date"="date"))

length(unique(imp_rough_df_best$PERMNO))
length(unique(imp_rough_df_filtered$PERMNO))
length(unique(imp_rough_df_nonfiltered$PERMNO))

# best, filtered, unfiltered
head(res,50)



which((res$P_num_ATM_skew.x < res$P_num_ATM_skew.y-2) & !(is.na(res$P_alpha.x)))[1:5]

res[530,]
res[531,]
res[670,]

print(dim(imp_rough_df_best %>% drop_na))
print(dim(imp_rough_df_filtered %>% drop_na))
print(dim(imp_rough_df_nonfiltered %>% drop_na))

skew_num_threshold <- 3
year_ <- 1996
month_ <- 1
crsp_daily <- readRDS("../related_data/crsp_daily.rds")


idx <- crsp_daily$date >= year_*10000 & crsp_daily$date < (year_+1) * 10000
crsp_daily_all <- crsp_daily[idx,]

date_CRSP <- as.Date(as.character(crsp_daily_all$date), format="%Y%m%d")
crsp_daily <- crsp_daily_all[year(date_CRSP) == year_ & month(date_CRSP) == month_,]

# read the PERMNO_secid merged file
sec_PERMNO_merged <- readRDS("../related_data/sec_PERMNO_merged.rds")

# close price merged with secid 
crsp_daily_secid <- crsp_daily %>% left_join(sec_PERMNO_merged, by = c("PERMNO"="PERMNO")) %>% 
  select(PERMNO, date, PRC, VOL, SICCD.x, secid) %>% mutate(date = as.Date(as.character(date), format = "%Y%m%d")) %>% 
  distinct(secid,date, .keep_all=T) %>% distinct(PERMNO,date, .keep_all=T) #%>%
  #mutate(PRC = abs(PRC)) # crucial for later screening based on PRC

option_price <- read.csv(paste("../option-prices/",year_,"_", month_,".csv",sep=""))

# merge with CRSP
option_price_CRSP_merged <- option_price %>% mutate(date = as.Date(as.character(date), format = "%d%b%Y"),exdate = as.Date(as.character(exdate), format = "%d%b%Y")) %>% 
  left_join(crsp_daily_secid, by = c("secid"="secid", "date"="date"))
option_price_CRSP_merged <- option_price_CRSP_merged %>% mutate(logKS = log(strike_price/1000/PRC)) %>% drop_na(logKS)



option_price_CRSP_merged <- option_price_CRSP_merged %>% mutate(tau = as.numeric((exdate - date)/365))


option_price_CRSP_merged <- option_price_CRSP_merged %>% mutate(opt_price = rowMeans(cbind(best_bid, best_offer),na.rm=T), day_to_m = exdate-date) %>% 
  filter(PRC>5, VOL>0, !is.na(volume), open_interest>0, impl_volatility>=0.03, impl_volatility<=2,
         day_to_m>=5, day_to_m<=365)


# use put option and spline to calculate ATM_skew 
ATM_skew_P <- option_price_CRSP_merged %>% filter(cp_flag == "P", PERMNO==59176, date=="1996-01-31")  %>% distinct(date,PERMNO,exdate,strike_price,.keep_all = T) %>% group_by(date, PERMNO, exdate) %>%
  summarize(nonpara_ATM_skew = ATMskew_term_structure_spline(logKS, impl_volatility,skew_num_threshold), tau = first(tau), secid = first(secid), tot_option_VOL = sum(volume), tot_open_interest=sum(open_interest))

ATM_skew_P <- option_price_CRSP_merged %>% filter(cp_flag == "P", PERMNO==86454, date=="2006-09-12")  %>% distinct(date,PERMNO,exdate,strike_price,.keep_all = T) %>% group_by(date, PERMNO, exdate) %>%
  summarize(nonpara_ATM_skew = ATMskew_term_structure_spline(logKS, impl_volatility,skew_num_threshold), tau = first(tau), secid = first(secid), tot_option_VOL = sum(volume), tot_open_interest=sum(open_interest))



P_imp_rough <- ATM_skew_P  %>% summarize(tot_option_VOL = sum(tot_option_VOL), tot_open_interest=sum(tot_open_interest),
                                         alpha = alpha_estimation(tau, nonpara_ATM_skew,1), alpha_rsquare = alpha_estimation_rsquare(tau, nonpara_ATM_skew,1), 
                                         num_ATM_skew = sum(!is.na(nonpara_ATM_skew)), secid = first(secid)) %>% ungroup()

res <- imp_rough_df_best %>% select(date,PERMNO, P_num_ATM_skew, P_alpha) %>% filter 
  full_join(P_imp_rough %>% select(date,PERMNO,num_ATM_skew, alpha), by = c("PERMNO"="PERMNO","date"="date"))


# option_price_CRSP_merged_test <- option_price_CRSP_merged %>% filter(PERMNO==10104, date=="1996-01-04")
# option_price_CRSP_merged_test <- option_price_CRSP_merged_test%>% mutate(tau = as.numeric((exdate - date)/365))
# 
# 
# 
# 
# 
# 
# option_price_CRSP_merged_test_filtered <- option_price_CRSP_merged_test %>% mutate(day_to_m = exdate-date) %>% 
#   filter(PRC>5, VOL>0, !is.na(volume), open_interest>0, impl_volatility>=0.03, impl_volatility<=2,
#          day_to_m>=5, day_to_m<=365)
# 
# temp <- option_price_CRSP_merged_test %>% filter(cp_flag == "P", exdate == "1996-01-20") %>% group_by(date, PERMNO, exdate) %>% drop_na() %>%
#   distinct(date,PERMNO,exdate,strike_price,.keep_all = T) %>% group_by(date, PERMNO, exdate)
# 
# temp %>% summarize(nonpara_ATM_skew = ATMskew_term_structure_spline(logKS, impl_volatility,skew_num_threshold))
# ATMskew_term_structure_spline(temp$logKS, temp$impl_volatility,skew_num_threshold)
# 
# option_price_CRSP_merged_test_filtered %>% filter(cp_flag == "P", exdate == "1996-01-20")


# ATM_skew_P <- option_price_CRSP_merged_test %>% filter(cp_flag == "P") %>% drop_na() %>% 
#   distinct(date,PERMNO,exdate,strike_price,.keep_all = T) %>% group_by(date, PERMNO, exdate) %>%
#   summarize(nonpara_ATM_skew = ATMskew_term_structure_spline(logKS, impl_volatility,skew_num_threshold), 
#             tau = first(tau), secid = first(secid), tot_option_VOL = sum(volume), tot_open_interest=sum(open_interest))
ATM_skew_P <- option_price_CRSP_merged_test_filtered %>% filter(cp_flag == "P") %>% drop_na() %>% 
  distinct(date,PERMNO,exdate,strike_price,.keep_all = T) %>% group_by(date, PERMNO, exdate) %>%
  summarize(nonpara_ATM_skew = ATMskew_term_structure_spline(logKS, impl_volatility,3), 
            tau = first(tau), secid = first(secid), tot_option_VOL = sum(volume), tot_open_interest=sum(open_interest))

P_imp_rough <- ATM_skew_P  %>% summarize(tot_option_VOL = sum(tot_option_VOL), tot_open_interest=sum(tot_open_interest),
                                         alpha = alpha_estimation(tau, nonpara_ATM_skew,1), alpha_rsquare = alpha_estimation_rsquare(tau, nonpara_ATM_skew,1), 
                                         num_ATM_skew = sum(!is.na(nonpara_ATM_skew)), secid = first(secid)) %>% ungroup()
P_imp_rough
