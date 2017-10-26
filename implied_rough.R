#!/apps/R-3.4.0/bin/Rscript

# when running this R script we could use the following line in a shell script 
# sge_run --grid_mem=15g --grid_submit=batch "./implied_rough.R $y $m"
args = commandArgs(trailingOnly=TRUE)
source("./implied_rough_helper.R")
year_ = as.numeric(args[1])
month_ = as.numeric(args[2])

library(tidyr)
#library(lokern)
library(data.table)
library(dplyr)
#library(ggplot2)

crsp_daily <- read.csv("../crsp_daily.csv")
idx <- crsp_daily$date >= year_*10000 & crsp_daily$date < (year_+1) * 10000
crsp_daily <- crsp_daily[idx,]

date_CRSP <- as.Date(as.character(crsp_daily$date), format="%Y%m%d")
crsp_daily <- crsp_daily[year(date_CRSP) == year_ & month(date_CRSP) == month_,]

# read the PERMNO_secid merged file
sec_PERMNO_merged <- readRDS("./sec_PERMNO_merged.rds")
# read option price data for this year and month

option_price <- read.csv(paste("../tickdata_bash_200/",year_,"_", month_,".csv",sep=""))

# close price merged with secid 

crsp_daily_secid <- crsp_daily %>% select(PERMNO, date, PRC) %>% left_join(sec_PERMNO_merged, by = c("PERMNO"="PERMNO")) %>% 
  select(PERMNO, date, PRC, secid) %>% mutate(date = as.Date(as.character(date), format = "%Y%m%d"))

head(crsp_daily_secid)

# how many underlyings are there in the option data 
length(unique(option_price$secid))

# how many closing prices in CRSP data for that month 
length(unique(crsp_daily_secid$PERMNO))


# option data merged with closed price data
option_price_CRSP_merged <- option_price %>% mutate(date = as.Date(as.character(date), format = "%d%b%Y"),exdate = as.Date(as.character(exdate), format = "%d%b%Y")) %>% left_join(crsp_daily_secid, by = c("secid"="secid", "date"="date"))
option_price_CRSP_merged <- option_price_CRSP_merged %>% mutate(logKS = log(strike_price/1000/PRC)) %>% drop_na(logKS)

# filter out the potential mismatch, where for a given maturity day the logKS does not cross 0, i.e., min * max >= 0 
temp <- option_price_CRSP_merged %>% group_by(secid, date, exdate) %>% summarise(max_KS = max(logKS), min_KS = min(logKS), cross= max_KS * min_KS) %>% filter(cross<0)
temp <- temp %>% ungroup

# merged with the filtered secid, date, exdate 
option_price_CRSP_merged <- temp %>% select(date,secid, exdate) %>% left_join(option_price_CRSP_merged, by=c("date"="date","secid"="secid", "exdate"="exdate"))

# tau 
option_price_CRSP_merged <- option_price_CRSP_merged %>% mutate(tau = as.numeric((exdate - date)/365))

# we can plot one day one ticker here 
# put_1day_spline <- look_one_day_one_stock(option_price_CRSP_merged, 5005, 19970107, "P", ATMskew_term_structure_spline, "spline")


# use put option and spline to calculate ATM_skew 
temp <- option_price_CRSP_merged %>% filter(cp_flag == "P") %>% drop_na() %>% distinct(date,PERMNO,exdate,strike_price,.keep_all = T) %>% group_by(date, PERMNO, exdate) %>%
  summarize(ATM_skew = ATMskew_term_structure_spline(logKS, impl_volatility,3), tau = first(tau), secid = first(secid))

put_1month_spline_final <- temp %>% drop_na %>% summarize(alpha = alpha_estimation(tau, ATM_skew,1), alpha_rsquare = alpha_estimation_rsquare(tau, ATM_skew,1), num_ATM_skew = n(), secid = first(secid)) %>% ungroup()

# save the results
saveRDS(put_1month_spline_final, paste("./imp_alpha_results/", year_, "_",month_, "_imp_alpha.rds", sep=""))
