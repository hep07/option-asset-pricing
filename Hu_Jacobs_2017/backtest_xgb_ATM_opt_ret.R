library(tidyr)
#library(lokern)
library(lubridate)
library(dplyr)
library(timeDate)
library(moments)

###################################################################
# read prediction 
###################################################################
xgb_factor_ATM_C <- readRDS("./combined_df/xgb_factor_ATM_C.rds")
xgb_factor_ATM_P <- readRDS("./combined_df/xgb_factor_ATM_P.rds")

###################################################################
# read data
###################################################################
opt_df <- readRDS("./combined_df/expected_opt_ret.rds")
xgb_factor_ATM_P <- readRDS("./combined_df/xgb_factor_ATM_P.rds")



om_div <- readRDS("../related_data/om_distribution.rds")
# make sure the event recorded is regular dividend distribution (type "1") and alos it is not cancelled
om_div <- om_div %>% filter(distr_type=="1", cancel_flag==0)
sec_PERMNO_merged <- readRDS("../related_data/sec_PERMNO_merged.rds")
opt_exp_dates <- readRDS("./opt_exp_dates.rds")
port_formation_dates <- readRDS("./port_formation_dates.rds")
terminal_payoff_dates <- readRDS("./terminal_payoff_dates.rds")



crsp_daily <- readRDS("../related_data/crsp_daily.rds")
crsp_daily <- as_tibble(crsp_daily)


df <- readRDS("./combined_df/expected_opt_ret.rds")

start_date <- min(df$date) - 30 # -30 since we need to keep the CRSP data that we use to form the ret_sigma signal for the first port formation date min(df$date)
to_keep <- crsp_daily$date >= year(start_date) * 10000 + month(start_date) * 100 + day(start_date)
crsp_daily <- crsp_daily[to_keep,]

crsp_daily$date <- as.Date(as.character(crsp_daily$date), format = "%Y%m%d")
crsp_daily_preprocessed <- crsp_daily %>% select(date, PERMNO, RET, SHROUT, VOL) %>% mutate(RET = as.numeric(as.character(RET)), year = year(date), month= month(date),
                                                                                            SHROUT = as.numeric(SHROUT), VOL = as.numeric(VOL)) 

################################
# to replicate Hu and Jacobs, we need to calcualte the prevous 30 calendar day daily return volatility 
################################
crsp_daily_preprocessed <- crsp_daily_preprocessed %>% mutate(port_form_index = findInterval(date, port_formation_dates),
                                   formation_date = port_formation_dates[port_form_index+1]) %>% select(-c(year,month)) #+1 since at each formation date, we use past return vol as signal

crsp_daily_preprocessed <- crsp_daily_preprocessed %>%
  group_by(formation_date, PERMNO) %>% summarise(ret_sigma = sd(RET,na.rm=T),
                                                 count_this_formation_perdio = sum(!is.na(RET)),
                                             # ret_skew = skewness(RET, na.rm = T),
                                              turnover = sum(VOL)/mean(SHROUT,na.rm=T)/1000) %>% ungroup()


#############################################
# exclude trades that has dividend from date to execute_dates
#############################################
# find the formation period that each ex dividend date lies in
om_div_this <- om_div %>% mutate(ex_date = as.Date(as.character(ex_date), format="%Y%m%d")) %>% 
  filter(ex_date>= port_formation_dates[1]) %>%
    mutate(port_form_index = findInterval(ex_date, port_formation_dates),
             to_remove_formation_date = port_formation_dates[port_form_index])
  #filter(ex_date <= opt_exp_dates[this_month_formation_date_idx+1], ex_date >= port_formation_dates[this_month_formation_date_idx])

# only filter out regular dividend which is marked as "1" in distr_type and also make sure it is not cancelled

om_div_this <- om_div_this %>% filter(distr_type=="1", cancel_flag==0) %>% mutate(has_dividend=1)

# match by to_remove_formation_date and secid
df <- df %>% left_join(om_div_this%>% select(secid, has_dividend, to_remove_formation_date), 
                       by = c("secid"="secid", "date"="to_remove_formation_date"))

df_no_div <- df %>% filter(is.na(has_dividend)) %>% select(-c(has_dividend))

##################################################
# since the selection only based on moneyness, it is possible that one ticker has multiple ATM option, pick the most ATM one
##################################################
df_no_div <- df_no_div %>% mutate(opt_id  = 1:nrow(df_no_div))

df_no_div <- df_no_div %>% group_by(secid, date, cp_flag) %>% mutate(dist_ATM = abs(1-moneyness), most_ATM_optID= opt_id[which.min(dist_ATM)[1]])

df_no_div_most_ATM <- df_no_div %>% filter(opt_id == most_ATM_optID) %>% ungroup()
#############################################
# different filtering based on option input, 0 indicates Hu and Jacobs
#############################################
if (filter_idx==0) {
  # 0 denotes Hu and Jacobs
} else if (filter_idx==1) {
  # 1 means just take out dividend and missing imp vols and requiring >0 option volume, >0 open interests
  filtered_df <- df_no_div_most_ATM %>% drop_na(impl_volatility) %>% filter(volume >0, open_interest>0)
}

# merge with daily vol signal from CRSP to replicate Hu and Jacbos
filtered_df_final <-  filtered_df %>% left_join(crsp_daily_preprocessed, by = c("date"="formation_date", "CRSP_PERMNO"="PERMNO"))

# caclualte option returns and drop NAs
filtered_df_final_complete <- filtered_df_final %>% select(secid,date, cp_flag, best_bid, best_offer, impl_volatility, payoff_T, ret_sigma, volume) %>% 
  mutate(optret_using_ask = payoff_T/best_offer-1, optret_using_mid = payoff_T*2/(best_offer+best_bid)-1) %>% drop_na

filtered_df_final_complete <- filtered_df_final_complete %>%mutate(dolvolume = (best_offer+best_bid)/2*volume*100) %>% drop_na 
# there is a 100 above when calculating dolvolume since one contract corresponds to 100 shares

#############################################
# form option portfolio by sorting
#############################################


sort_into_n <- function(x, n) {
  # return index 1:n for each x
  q_vec <- seq(0,1,1/n)
  
  res <- sapply(q_vec[2:(length(q_vec)-1)], function(z) c(quantile(x, z)))
  brk_points <- c(min(x),res)
  return(findInterval(x, brk_points))
}

# sorting based on ret_sigma  


# ls_ts %>% group_by(date) %>% rbind(c(6, eqw_opt_ret_using_ask[port_idx==5] - eqw_opt_ret_using_ask[port_idx==1],
#                                      eqw_opt_ret_using_mid[port_idx==5] - eqw_opt_ret_using_mid[port_idx==1],
#                                      NA,NA,NA))

backtest_res_C <- filtered_df_final_complete %>% filter(cp_flag=="C") %>% group_by(date) %>% mutate(port_idx = sort_into_n(ret_sigma, 5)) %>% ungroup() %>%
  group_by(date, port_idx) %>% 
  summarise(eqw_opt_ret_using_ask = mean(optret_using_ask, na.rm=T), 
            eqw_opt_ret_using_mid = mean(optret_using_mid, na.rm=T),
            dolvolweighted_opt_ret_using_ask = sum(optret_using_ask*dolvolume)/sum(dolvolume), 
            dolvolweighted_opt_ret_using_mid = sum(optret_using_mid*dolvolume)/sum(dolvolume),
            opt_count = sum(!is.na(optret_using_mid)),
            avg_signal = mean(ret_sigma,na.rm=T),
            sum_opt_volume = sum(volume,na.rm=T),
            sum_opt_dolvolume = sum(dolvolume,na.rm=T)) %>% ungroup()  %>%
  group_by(port_idx) %>% 
  summarise(avg_eqw_opt_ret_using_ask = mean(eqw_opt_ret_using_ask, na.rm=T),
            avg_eqw_opt_ret_using_mid = mean(eqw_opt_ret_using_mid, na.rm=T),
            sd_eqw_opt_ret_using_ask = sd(eqw_opt_ret_using_ask, na.rm=T),
            sd_eqw_opt_ret_using_mid = sd(eqw_opt_ret_using_mid, na.rm=T),
            avg_dolvolweighted_opt_ret_using_ask = mean(dolvolweighted_opt_ret_using_ask, na.rm=T),
            avg_dolvolweighted_opt_ret_using_mid = mean(dolvolweighted_opt_ret_using_mid, na.rm=T),
            sd_dolvolweighted_opt_ret_using_ask = sd(dolvolweighted_opt_ret_using_ask, na.rm=T),
            sd_dolvolweighted_opt_ret_using_mid = sd(dolvolweighted_opt_ret_using_mid, na.rm=T),
            opt_count = sum(opt_count),
            avg_count_per_period = mean(opt_count),
            avg_signal = mean(avg_signal),
            avg_monthly_opt_volume = mean(sum_opt_volume),
            avg_monthly_opt_dolvolume = mean(sum_opt_dolvolume)) %>% data.frame

backtest_res_P <- filtered_df_final_complete %>% filter(cp_flag=="P") %>% group_by(date) %>% mutate(port_idx = sort_into_n(ret_sigma, 5)) %>% ungroup() %>%
  group_by(date, port_idx) %>% 
  summarise(eqw_opt_ret_using_ask = mean(optret_using_ask, na.rm=T), 
            eqw_opt_ret_using_mid = mean(optret_using_mid, na.rm=T),
            dolvolweighted_opt_ret_using_ask = sum(optret_using_ask*dolvolume)/sum(dolvolume), 
            dolvolweighted_opt_ret_using_mid = sum(optret_using_mid*dolvolume)/sum(dolvolume),
            opt_count = sum(!is.na(optret_using_mid)),
            avg_signal = mean(ret_sigma,na.rm=T),
            sum_opt_volume = sum(volume,na.rm=T),
            sum_opt_dolvolume = sum(dolvolume,na.rm=T)) %>% ungroup()  %>%
  group_by(port_idx) %>% 
  summarise(avg_eqw_opt_ret_using_ask = mean(eqw_opt_ret_using_ask, na.rm=T),
            avg_eqw_opt_ret_using_mid = mean(eqw_opt_ret_using_mid, na.rm=T),
            sd_eqw_opt_ret_using_ask = sd(eqw_opt_ret_using_ask, na.rm=T),
            sd_eqw_opt_ret_using_mid = sd(eqw_opt_ret_using_mid, na.rm=T),
            avg_dolvolweighted_opt_ret_using_ask = mean(dolvolweighted_opt_ret_using_ask, na.rm=T),
            avg_dolvolweighted_opt_ret_using_mid = mean(dolvolweighted_opt_ret_using_mid, na.rm=T),
            sd_dolvolweighted_opt_ret_using_ask = sd(dolvolweighted_opt_ret_using_ask, na.rm=T),
            sd_dolvolweighted_opt_ret_using_mid = sd(dolvolweighted_opt_ret_using_mid, na.rm=T),
            opt_count = sum(opt_count),
            avg_count_per_period = mean(opt_count),
            avg_signal = mean(avg_signal),
            avg_monthly_opt_volume = mean(sum_opt_volume),
            avg_monthly_opt_dolvolume = mean(sum_opt_dolvolume)) %>% data.frame

##########################################################################################
# roughly the same with Hu and Jacobs
##########################################################################################

# next we use call and put to form long short portfolio, long call with port index 1 and long put with port index 5 

ls_ts_C <-  filtered_df_final_complete %>% filter(cp_flag=="C") %>% group_by(date) %>% mutate(port_idx = sort_into_n(ret_sigma, 5)) %>% ungroup() %>%
  group_by(date, port_idx) %>% 
  summarise(eqw_opt_ret_using_ask = mean(optret_using_ask, na.rm=T), 
            eqw_opt_ret_using_mid = mean(optret_using_mid, na.rm=T),
            dolvolweighted_opt_ret_using_ask = sum(optret_using_ask*dolvolume)/sum(dolvolume), 
            dolvolweighted_opt_ret_using_mid = sum(optret_using_mid*dolvolume)/sum(dolvolume),
            opt_count = sum(!is.na(optret_using_mid)),
            avg_signal = mean(ret_sigma,na.rm=T),
            sum_opt_volume = sum(volume,na.rm=T),
            sum_opt_dolvolume = sum(dolvolume,na.rm=T)) %>% ungroup()  %>% arrange(date, port_idx)

ls_ts_P <-  filtered_df_final_complete %>% filter(cp_flag=="P") %>% group_by(date) %>% mutate(port_idx = sort_into_n(ret_sigma, 5)) %>% ungroup() %>%
  group_by(date, port_idx) %>% 
  summarise(eqw_opt_ret_using_ask = mean(optret_using_ask, na.rm=T), 
            eqw_opt_ret_using_mid = mean(optret_using_mid, na.rm=T),
            dolvolweighted_opt_ret_using_ask = sum(optret_using_ask*dolvolume)/sum(dolvolume), 
            dolvolweighted_opt_ret_using_mid = sum(optret_using_mid*dolvolume)/sum(dolvolume),
            opt_count = sum(!is.na(optret_using_mid)),
            avg_signal = mean(ret_sigma,na.rm=T),
            sum_opt_volume = sum(volume,na.rm=T),
            sum_opt_dolvolume = sum(dolvolume,na.rm=T)) %>% ungroup()  %>% arrange(date, port_idx)



ls_ts <- ls_ts_P %>% filter(port_idx==5)  %>% select(dolvolweighted_opt_ret_using_ask, date) %>% rename(P_dolvolweighted_opt_ret_using_ask_n = dolvolweighted_opt_ret_using_ask) %>%
  left_join(ls_ts_C %>% filter(port_idx==1) %>% select(dolvolweighted_opt_ret_using_ask,date) %>% rename(C_dolvolweighted_opt_ret_using_ask_1 = dolvolweighted_opt_ret_using_ask), by = c("date"="date")) %>%
  mutate(ls_ret = 0.5*(P_dolvolweighted_opt_ret_using_ask_n + C_dolvolweighted_opt_ret_using_ask_1))

tail(ls_ts)


ls_ts <- ls_ts %>% mutate(ls = eqw_opt_ret_using_ask_n - eqw_opt_ret_using_ask_1)
print(sapply(ls_ts, mean))
print(sapply(ls_ts, sd))
print(sapply(ls_ts, mean)/sapply(ls_ts, sd)*sqrt(nrow(ls_ts)))