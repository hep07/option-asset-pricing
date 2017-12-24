#!/apps/R-3.4.0/bin/Rscript

########################################################################
# this script tries to collect `replicate what Jacobs and Hu found in SSRN working paper version 2017
########################################################################



args = commandArgs(trailingOnly=TRUE)
year_ = as.numeric(args[1])
month_ = as.numeric(args[2])
diff_close_threshold <- 0.05
#########################################
# first read in crsp and PERMNO_secid merge data and merge secid onto crsp daily return and closing price data
#########################################
library(tidyr)
#library(lokern)
library(lubridate)
library(dplyr)
library(timeDate)
#library(ggplot2)

source("../helper_functions/imp-vol-other-factor-helper.R")
source("../helper_functions/implied_rough_helper.R")

# temp <- read.csv("../related_data/om_distribution.csv", stringsAsFactors = F)
# saveRDS(temp, "../related_data/om_distribution.rds")
# om_div <- readRDS("../related_data/om_distribution.rds")

# length(port_formation_dates)
# length(opt_exp_dates)
# temp <- read.csv("../related_data/om_underlying_closing.csv")
# saveRDS(temp, "../related_data/om_underlying_closing.rds")
om_closing <- readRDS("../related_data/om_underlying_closing.rds")
colnames(om_closing) <- paste("om_sec_", colnames(om_closing), sep="")
crsp_daily <- readRDS("../related_data/crsp_daily.rds")
crsp_daily <- crsp_daily[crsp_daily$date>=19950601,]
date_CRSP <- as.Date(as.character(crsp_daily$date), format="%Y%m%d")
opt_exp_dates <- readRDS("./opt_exp_dates.rds")
port_formation_dates <- readRDS("./port_formation_dates.rds")
terminal_payoff_dates <- readRDS("./terminal_payoff_dates.rds")
sec_PERMNO_merged <- readRDS("../related_data/sec_PERMNO_merged.rds")
out <- list()

#for (month_ in 1:12) {
print("processing month")
print(month_)

# get the port formation dates for this month and the opt exp dates for the next months
this_month_formation_date_idx <- which(year_ == year(port_formation_dates) & month_ == month(port_formation_dates))

terminal_payoff_dates[this_month_formation_date_idx+1]

# crsp_daily <- read.csv("F:/Dropbox/AssetPricingPy/data/CRSP_data/crsp_daily.csv")
#saveRDS(crsp_daily, "F:/Dropbox/AssetPricingPy/data/CRSP_data/crsp_daily.rds")
#crsp_daily <- readRDS("D:/Dropbox/AssetPricingPy/data/CRSP_data/crsp_daily.rds")







# select the previous and the next year data for this month 
# idx <- crsp_daily$date >= (year_-1)*10000 & crsp_daily$date <= (year_+1) * 10000
# crsp_daily <- crsp_daily[idx,]


# we want to keep this month and the next months return data 
temp_date <- as.Date(paste(year_,month_,"01",sep="-")) 
month(temp_date) <- month(temp_date)+1
keep_idx <- (year(date_CRSP) == year_ & month(date_CRSP) == month_) | 
  (year(date_CRSP) == year(temp_date) & month(date_CRSP) == month(temp_date))
  

crsp_daily_this <- crsp_daily[keep_idx,]
rm(crsp_daily)


# read the PERMNO_secid merged file


# close price merged with secid 
crsp_daily_secid <- crsp_daily_this %>% left_join(sec_PERMNO_merged, by = c("PERMNO"="PERMNO")) %>% 
  select(PERMNO, date, PRC, VOL, SICCD.x, secid) %>% mutate(date = as.Date(as.character(date), format = "%Y%m%d")) %>% 
  distinct(secid,date, .keep_all=T) %>% distinct(PERMNO,date, .keep_all=T) %>%
  mutate(PRC = abs(PRC)) 
colnames(crsp_daily_secid) <- paste("CRSP_", colnames(crsp_daily_secid), sep="")

#########################################
# next get the option price data and merge it with option metric closing price data
#########################################
option_price <- read.csv(paste("../option-prices/",year_,"_", month_,".csv",sep=""))

#om_closing <- read.csv("../related_data/om_underlying_closing.csv")
#saveRDS(om_closing, "../related_data/om_underlying_closing.rds")

# om_div <- read.csv("../related_data/om_dividend.csv")
# om_div <- saveRDS(om_div, "./om_dividend.rds")





om_closing_this <- om_closing %>% mutate(om_sec_date = as.Date(as.character(om_sec_date), format = "%Y%m%d")) %>% 
  filter((year(om_sec_date)==year_ & month(om_sec_date)==month_) | (year(om_sec_date)==year(temp_date) & month(om_sec_date)==month(temp_date)))

om_closing_this <- om_closing_this %>% mutate(om_sec_close = abs(om_sec_close))


# merge with OM security prices file 
option_price_sec_merged <- option_price %>% mutate(date = as.Date(as.character(date), format = "%d%b%Y"),exdate = as.Date(as.character(exdate), format = "%d%b%Y")) %>% 
  left_join(om_closing_this, by = c("secid"="om_sec_secid", "date"="om_sec_date"))

# merge with CRSP and filter out the bad matches where the closing price from CRSP and option metrics differ by more than say 5%

option_price_sec_CRSP_merged <- option_price_sec_merged %>% 
  left_join(crsp_daily_secid, by = c("secid"="CRSP_secid", "date"="CRSP_date")) %>%
  mutate(diff_close = abs((CRSP_PRC-om_sec_close)/om_sec_close), bad_match = diff_close > diff_close_threshold) %>%
  filter(!bad_match)

option_price_sec_CRSP_merged <- option_price_sec_CRSP_merged %>% 
  mutate(moneyness = strike_price/1000/om_sec_close, logKS = log(strike_price/1000/om_sec_close)) 
# bad_match_idx <- which(abs(option_price_sec_CRSP_merged$CRSP_PRC - option_price_sec_CRSP_merged$om_sec_close)/abs(abs(option_price_sec_CRSP_merged$CRSP_PRC))>0.05)

option_price_sec_CRSP_merged_ATM <- option_price_sec_CRSP_merged %>% filter(abs(moneyness)>=0.975, abs(moneyness)<=1.025)

# take options that are expiring on third Fri/Sat of the month only
option_price_sec_CRSP_merged_ATM <- option_price_sec_CRSP_merged_ATM %>% filter(exdate %in% opt_exp_dates)


#############################################
# two things to calculate: future terminal payoff of the selected options and the price of those options at formation date
#############################################
# first get the prices 
#############################################




df_this_month <- option_price_sec_CRSP_merged_ATM %>% 
  filter(date==port_formation_dates[this_month_formation_date_idx],
         exdate==opt_exp_dates[this_month_formation_date_idx+1]) %>% 
  select(-c(logKS, diff_close, bad_match)) %>% 
  mutate(execute_dates = terminal_payoff_dates[this_month_formation_date_idx+1])

# merge execute dates with the CRSP and om closing price then in order to calculate terminal payoff
# note that this could potentially have problems when there are stock splits and merging during that month 
df_this_month <- df_this_month %>% left_join(crsp_daily_secid %>% rename(CRSP_PRC_terminal = CRSP_PRC) %>% select(CRSP_date, CRSP_secid, CRSP_PRC_terminal),
                            by = c("execute_dates"= "CRSP_date", "secid"="CRSP_secid"))

# merge with terminal price from om_closing too just to make sure                 
df_this_month <- df_this_month %>% left_join(om_closing_this %>% rename(om_sec_close_terminal = om_sec_close, om_sec_cfadj_terminal = om_sec_cfadj) %>% select(om_sec_date, om_sec_secid,om_sec_cfadj_terminal, om_sec_close_terminal),
                                             by = c("execute_dates"= "om_sec_date", "secid"="om_sec_secid"))

df_this_month <- df_this_month %>% mutate(relative_diff_terminal_PRC = abs((om_sec_close_terminal-CRSP_PRC_terminal)/om_sec_close_terminal))

df_this_month <- df_this_month %>% filter(relative_diff_terminal_PRC<= diff_close_threshold) %>% 
  drop_na(impl_volatility, relative_diff_terminal_PRC) %>% select(-relative_diff_terminal_PRC)

df_this_month <- df_this_month %>% mutate(om_sec_close_terminal_adj = om_sec_close_terminal * om_sec_cfadj_terminal/om_sec_cfadj) %>% drop_na(om_sec_close_terminal_adj)

#############################################
# calculate terminal payoff and the training target of a nonlinear transformation of return 
#############################################
df_this_month <- df_this_month %>% mutate(payoff_T = ifelse(cp_flag=="C", pmax(om_sec_close_terminal_adj - strike_price/1000,0), pmax(- om_sec_close_terminal_adj + strike_price/1000,0)),
                                          ATM_call_y = pmax(om_sec_close_terminal_adj/om_sec_close - 1,0),
                                          ATM_put_y = pmax(1 - om_sec_close_terminal_adj/om_sec_close,0))



#out[[month_]] <- df_this_month
# gc()
# }

#out <- do.call(rbind, out)
saveRDS(df_this_month, paste("./expected_opt_ret/", year_,"_",month_, ".rds", sep=""))

