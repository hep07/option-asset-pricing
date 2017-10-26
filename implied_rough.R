#!/apps/R-3.2.2/bin/Rscript
args = commandArgs(trailingOnly=TRUE)

year_ = args[1]
month_ = args[2]

library(tidyr)
library(lokern)
library(data.table)
library(dplyr)
library(ggplot2)

crsp_daily <- read.csv("../../Dropbox/AssetPricingPy/data/CRSP_data/crsp_daily.csv")
idx <- crsp_daily$date >= year_*10000 & crsp_daily$date < (year_+1) * 10000
crsp_daily <- crsp_daily[idx,]

date_CRSP <- as.Date(as.character(crsp_daily$date), format="%Y%m%d")
crsp_daily <- crsp_daily[year(date_CRSP) == year_ & month(date_CRSP) == month_,]

# read the PERMNO_secid merged file
sec_PERMNO_merged <- readRDS("./sec_PERMNO_merged.rds")
# read option price data for this year and month

option_price <- read.csv(paste(year_,"_", month_,".csv",sep=""))

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

# try one day plot
put_1day_spline <- look_one_day_one_stock(option_price_CRSP_merged, 5005, as.Date("19970107", format="%Y%m%d"), "P", ATMskew_term_structure_spline, "spline")


option_price_CRSP_merged[which(option_price_CRSP_merged$logKS>2)[1:5],]
head(temp)



#  <- year*10000 + month*12
spx_vol_surface <- read.csv("spx_impl_vol_surface.csv")
spx_option <- read.csv("SPX_option_prices.csv")

# lookat_a_day <- function(datenum) {
#   temp <- spx_vol_surface[spx_vol_surface$date==datenum,] %>% filter(cp_flag=="P", 45<=abs(delta), abs(delta)<=55) %>% select(date, days, delta, impl_volatility, impl_strike) %>%
#     group_by(days) %>% arrange(days) %>% mutate(delta_abs = abs(delta), log_K_S = log(impl_strike))  %>%
#     summarize(ATMskew_day=abs(impl_volatility[delta_abs==55]-impl_volatility[delta_abs==45])/abs(log_K_S[delta_abs==55] - log_K_S[delta_abs==45])) %>% 
#     mutate(tau=days/365) %>% select(tau, ATMskew_day) #  
#   
#   plot(temp)
#   
#   res <- spx_vol_surface[spx_vol_surface$date==datenum,] %>% filter(cp_flag=="P", 45<=abs(delta), abs(delta)<=55) %>% select(date, days, delta, impl_volatility, impl_strike) %>%
#     group_by(days) %>% arrange(days) %>% mutate(delta_abs = abs(delta), log_K_S = log(impl_strike))
#   return(list(basic_data = res, ATM_skew_res = temp))
# }

# testdate <- 20130620
# res_oneday <- lookat_a_day(testdate)
# 
# temp = res_oneday$ATM_skew_res
# # try to replicate the ATM skew term structure in Gatheral's presentation
# x <- seq(0.01,2,0.01)
# res <- lm(log(temp$ATMskew_day)~log(temp$tau))
# 
# imposing_powerlaw <- data.frame(fitted = exp(res$coefficients[1]+res$coefficients[2]*log(x)), x =x)
# 
# ggplot(temp, aes(x = tau, y = ATMskew_day)) + geom_point() + geom_line(data = imposing_powerlaw, aes(x=x, y=fitted)) + 
#   labs(title = paste(as.character(testdate), " ,the fitted line is tao^", round(res$coefficients[2],digits = 3), sep=""))



#summary(res)

# does not quite close, we try option prices data next 
SPX <- read.csv("SPX.csv")

#closing <- SPX$Close[SPX$Date == "6/20/2013"]
#%>% mutate(delta_5 = delta + 0.5) %>% arrange(delta_5)
spx_option$date_rdate <- as.Date(as.character(spx_option$date), format="%Y%m%d")
spx_option$exdate_rdate <- as.Date(as.character(spx_option$exdate), format="%Y%m%d")
spx_option$tau <- as.numeric(spx_option$exdate_rdate - spx_option$date_rdate)/365 # time to maturity in # of days/365




# 
SPX$Date <-as.numeric(as.character(as.Date(as.character(SPX$Date), format = "%m/%d/%Y"), format="%Y%m%d"))
spx_option <- spx_option %>% left_join(SPX, by = c("date"="Date"))
#closing <- SPX$Close[SPX$Date == "6/20/2013"]

# SPX_closing <- function(datenum) {
#  SPX$Close[SPX$Date==as.character(datenum)]
# }


# spline estimates 
ATMskew_term_structure_spline <- function(logKS,imp_vol, delta_KS) {
  if (length(logKS)>10) {
    spline_res <- smooth.spline(x = logKS, y = imp_vol)
    temp <- predict(spline_res,c(delta_KS,-delta_KS))
    res <- abs(temp$y[2] - temp$y[1])/2/delta_KS
    #print(res)
    return(res)  
  } else {
    return(NA)
  }
  
}

# kernel regression estimates 

ATMskew_term_structure_lokerns <- function(logKS,imp_vol, delta_KS) {
  if (length(logKS)>20) {
    lokern_fit <- lokerns(x = logKS, y = imp_vol)
    temp <- predict(lokern_fit,x = 0, deriv = 1)
    #res <- abs(temp$y[2] - temp$y[1])/2/delta_KS
    #print(res)
    return(abs(temp$y))
  } else {
    return(NA)
  }
  
}

alpha_estimation <- function(tau, ATMskew) {
  ATMskew[ATMskew==0] <- NA
  tau[tau<=0.02] <- NA
  df <- data.frame(log_ATMskew = log(ATMskew), log_tau = log(tau))
  df <- df[complete.cases(df),]
  if (nrow(df) < 5) {
    return(NA)
  } else {
    res<- lm(log_ATMskew~log_tau, data = df)
    return(res$coefficients[2])  
  }

}


alpha_estimation_rsquare <- function(tau, ATMskew) {
  ATMskew[ATMskew==0] <- NA
  tau[tau<=0.02] <- NA
  df <- data.frame(log_ATMskew = log(ATMskew), log_tau = log(tau))
  df <- df[complete.cases(df),]
  if (nrow(df) < 5) {
    return(NA)
  } else {
    res<- lm(log_ATMskew~log_tau, data = df)
    return(summary(res)$r.squared)  
  }
  
}



look_one_day <- function(datenum, put_or_call, slope_estimation_f, plot_name) {
  put_1day_spline <- spx_option  %>% filter(date==datenum, cp_flag==put_or_call) %>% mutate(logKS = log(strike_price/1000/Close)) %>%
    drop_na() %>% group_by(date,exdate) %>% arrange(date,exdate, strike_price)  %>% summarize(ATM_skew = slope_estimation_f(logKS, impl_volatility,0.0001), tau = first(tau)) 
  
  put_1day_spline_final <- put_1day_spline %>% summarize(alpha = alpha_estimation(tau, ATM_skew), alpha_rsquare = alpha_estimation_rsquare(tau, ATM_skew))
  # plot the term structure
  x <- seq(0.02,2.4,0.01)
  ATMskew <- put_1day_spline$ATM_skew
  tau <- put_1day_spline$tau
  ATMskew[ATMskew==0] <- NA
  tau[tau<=0.02] <- NA
  
  res <- lm(log(ATMskew)~log(tau))
  
  imposing_powerlaw <- data.frame(fitted = exp(res$coefficients[1]+res$coefficients[2]*log(x)), x =x)
  
  myggplot <- ggplot(put_1day_spline, aes(x = tau, y = ATM_skew)) + geom_point() + geom_line(data = imposing_powerlaw, aes(x=x, y=fitted), col = "red") + 
    labs(title = paste(as.character(datenum), " ,ATM skew is estimated via ", plot_name,"; the fitted line is tao^", round(res$coefficients[2],digits = 3), sep=""))
  
  print(myggplot)
  
  list(put_1day_spline, put_1day_spline_final)
}

imp_alpha_ts <- function(put_or_call, slope_estimation_f, plot_name) {
  put_1day_spline <- spx_option  %>% filter(cp_flag==put_or_call) %>% mutate(logKS = log(strike_price/1000/Close)) %>%
    drop_na() %>% group_by(date,exdate) %>% arrange(date,exdate, strike_price)  %>% summarize(ATM_skew = slope_estimation_f(logKS, impl_volatility,0.0001), tau = first(tau)) 
  put_1day_spline_final <- put_1day_spline %>% summarize(alpha = alpha_estimation(tau, ATM_skew), alpha_rsquare = alpha_estimation_rsquare(tau, ATM_skew))
  
  # # plot the term structure
  # x <- seq(0.02,2.4,0.01)
  # res <- lm(log(put_1day_spline$ATM_skew)~log(put_1day_spline$tau))
  # 
  # imposing_powerlaw <- data.frame(fitted = exp(res$coefficients[1]+res$coefficients[2]*log(x)), x =x)
  # 
  # myggplot <- ggplot(put_1day_spline, aes(x = tau, y = ATM_skew)) + geom_point() + geom_line(data = imposing_powerlaw, aes(x=x, y=fitted), col = "red") + 
  #   labs(title = paste(as.character(datenum), " ,ATM skew is estimated via ", plot_name,"; the fitted line is tao^", round(res$coefficients[2],digits = 3), sep=""))
  # 
  # print(myggplot)
  return(put_1day_spline_final)
}



put_1day_lokerns <- look_one_day(20130620, "P", ATMskew_term_structure_lokerns, "kernel regression")
put_1day_spline <- look_one_day(20130620, "P", ATMskew_term_structure_spline, "spline")
put_1day_lokerns <- look_one_day(20050915, "P", ATMskew_term_structure_lokerns, "kernel regression")
put_1day_spline <- look_one_day(20050915, "P", ATMskew_term_structure_spline, "spline")

put_ts_lokerns <- imp_alpha_ts("P", ATMskew_term_structure_lokerns, "kernel")
put_ts_spline <- imp_alpha_ts("P", ATMskew_term_structure_spline, "spline")


# put_1day_spline <- spx_option  %>% filter(cp_flag=="P") %>% mutate(logKS = log(strike_price/1000/Close)) %>%
#   drop_na() %>% group_by(date,exdate) %>% arrange(date,exdate, strike_price)  %>% summarize(ATM_skew = ATMskew_term_structure_lokerns(logKS, impl_volatility,0.0001), tau = first(tau)) 

# temp <- put_1day_spline %>% summarize(alpha = alpha_estimation(tau, ATM_skew), alpha_rsquare = alpha_estimation_rsquare(tau, ATM_skew))

# filter out some outliers
sum((put_ts_lokerns$alpha_rsquare <= 0.7) , na.rm = T)
sum((put_ts_lokerns$alpha_rsquare <= 0.5) | (put_ts_lokerns$alpha>0), na.rm = T)
# sum((temp$alpha>0), na.rm = T)
idx <- which((put_ts_lokerns$alpha_rsquare < 0.5) | (put_ts_lokerns$alpha>=0))
res_plot <- put_ts_lokerns
res_plot$alpha[idx] <- NA
res_plot$date <- as.Date(as.character(res_plot$date), format ="%Y%m%d")
qplot(x=date, y=alpha,
      data=res_plot, na.rm=TRUE,
      main="kernel implied alpha time series",
      xlab="Date", ylab="alpha")
ggplot(res_plot, aes(x=date, y=alpha)) + geom_line() + 
      labs(title="kernel implied alpha time series")
#      xlab="Date", ylab="alpha")

# summary(alpha)
# plot(temp$alpha_rsquare)
# plot(temp$alpha)
# 
# 
# put_1day_spline <- spx_option  %>% filter(date==20130620, cp_flag=="P") %>% mutate(logKS = log(strike_price/1000/Close)) %>%
#   drop_na() %>% group_by(date,exdate) %>% arrange(date,exdate, strike_price)  %>% summarize(ATM_skew = ATMskew_term_structure_spline(logKS, impl_volatility,0.0001), tau = first(tau)) %>%
#   summarize(alpha = alpha_estimation(tau, ATM_skew), alpha_rsquare = alpha_estimation_rsquare(tau, ATM_skew))
# 
# put_1day_lokerns <- spx_option  %>% filter(date==20050915, cp_flag=="P") %>% mutate(logKS = log(strike_price/1000/Close)) %>%
#   drop_na() %>% group_by(date,exdate) %>% arrange(date,exdate, strike_price)  %>% summarize(ATM_skew = ATMskew_term_structure_lokerns(logKS, impl_volatility,0.0001), tau = first(tau)) %>%
#   summarize(alpha = alpha_estimation(tau, ATM_skew), alpha_rsquare = alpha_estimation_rsquare(tau, ATM_skew))
# 

#   

# plot(put_1day$tau, put_1day$ATM_skew)
# skew_1day <- spx_option %>% filter(date==20130620,cp_flag=="P") %>% mutate_at(vars(strike_price), function(.) ./1000) %>%
#   select(date_rdate,tau, exdate, delta, impl_volatility, strike_price) %>% na.omit() %>% group_by(exdate) %>% mutate(delta_5 = abs(delta+0.5)) %>% arrange(delta_5) %>%
#   filter(row_number() <= 2) %>% summarise(tau = first(tau), date = dplyr::first(date_rdate), ATMskew = abs((impl_volatility[1]-impl_volatility[2])/log(strike_price[1]/strike_price[2])))

# try to replicate the ATM skew term structure in Gatheral's presentation
# x <- seq(0.05,2,0.01)
# 
# res_optionprice <- lm(log(skew_1day$ATMskew)~log(skew_1day$tau))
# imposing_powerlaw_optionprice <- data.frame(fitted = exp(res_optionprice$coefficients[1]+res_optionprice$coefficients[2]*log(x)), x =x)
# ggplot(skew_1day, aes(x = tau, y = ATMskew)) + geom_point() + geom_line(data = imposing_powerlaw_optionprice, aes(x=x, y=fitted))
#   
  
#  seemes that the first method is more promising

# 
# 
# spline_res <- smooth.spline(x = call_1day$logKS, y = call_1day$impl_volatility)
# delta_KS <- 0.0001
# temp <- predict(spline_res,c(delta_KS,-delta_KS))
# abs(temp$y[2] - temp$y[1])/2/delta_KS
# 
# 
# ATMskew_term_structure_spline <- function(logKS,imp_vol, delta_KS) {
#   if (length(logKS)>30) {
#     spline_res <- smooth.spline(x = logKS, y = imp_vol)
#     temp <- predict(spline_res,c(delta_KS,-delta_KS))
#     res <- abs(temp$y[2] - temp$y[1])/2/delta_KS
#     #print(res)
#     return(res)  
#   } else {
#     return(NA)
#   }
#   
# }
# 
# 
# 
# 
# # use put or call only 
# 
# alpha_ts_from_impl_vol <- spx_vol_surface %>% na.omit() %>%filter(cp_flag=="C", impl_strike>0, impl_premium>0, 45<=abs(delta), abs(delta)<=55) %>% select(date, days, delta, impl_volatility, impl_strike) %>%
#   group_by(date,days) %>% arrange(days) %>% mutate(delta_abs = abs(delta), log_K_S = log(impl_strike))  %>%
#   summarize(ATMskew_day=abs(impl_volatility[delta_abs==55]-impl_volatility[delta_abs==45])/abs(log_K_S[delta_abs==55] - log_K_S[delta_abs==45])) %>% 
#   mutate(tau=days/365) %>% arrange(tau) %>% summarize(alpha = alpha_estimation(tau,ATMskew_day), rsquare = alpha_estimation_rsquare(tau,ATMskew_day))# estimate for each day the alpha
# summary(alpha_ts_from_impl_vol)
# sum(alpha_ts_from_impl_vol$rsquare>0.8)/dim(alpha_ts_from_impl_vol)[1]
# plot(alpha_ts_from_impl_vol$alpha)
# plot(alpha_ts_from_impl_vol$alpha[alpha_ts_from_impl_vol$rsquare>0.9])
# 
# alpha_ts_from_impl_vol <- spx_vol_surface %>% na.omit() %>%filter(cp_flag=="P", impl_strike>0, impl_premium>0, 45<=abs(delta), abs(delta)<=55) %>% select(date, days, delta, impl_volatility, impl_strike) %>%
#   group_by(date,days) %>% arrange(days) %>% mutate(delta_abs = abs(delta), log_K_S = log(impl_strike))  %>%
#   summarize(ATMskew_day=abs(impl_volatility[delta_abs==55]-impl_volatility[delta_abs==45])/abs(log_K_S[delta_abs==55] - log_K_S[delta_abs==45])) %>% 
#   mutate(tau=days/365) %>% arrange(tau) %>% summarize(alpha = alpha_estimation(tau,ATMskew_day), rsquare = alpha_estimation_rsquare(tau,ATMskew_day))# estimate for each day the alpha
# summary(alpha_ts_from_impl_vol)
# sum(alpha_ts_from_impl_vol$rsquare>0.8)/dim(alpha_ts_from_impl_vol)[1]
# plot(alpha_ts_from_impl_vol$alpha)
# plot(alpha_ts_from_impl_vol$alpha[alpha_ts_from_impl_vol$rsquare>0.9])
# alpha_ts_from_impl_vol$datenum <- as.numeric(as.Date(as.character(alpha_ts_from_impl_vol$date), format = "%Y%m%d"))
#   
# correlation with our time series specification
spx_roughness_ts <- read.csv("../signal_creation/spx_roughness.csv")
spx_roughness_ts$datechar <- as.character(as.Date(spx_roughness_ts$date, format = "%Y-%m-%d"))
res_plot$datechar <- as.character(res_plot$date, format = "%Y-%m-%d")

merged_ts <- left_join(res_plot,  spx_roughness_ts, by=c("datechar")) %>% na.omit()
#merged_ts <- merged_ts %>% filter(rsquare>0.8)



# very low correlation!
cor(merged_ts %>% select(roughness_alpha, alpha))
temp <- merged_ts %>% select(roughness_alpha, alpha)
ret_daily_roughness<- do.call(cbind, lapply(temp, diff))
cor(ret_daily_roughness)

write.csv(res_plot[,c("date","alpha")],"imp_alpha_kernel_daily_ts.csv", row.names = F)


library(zoo)
alpha_ts_monthly <- merged_ts %>% mutate(year = format(date.y, "%Y"), month = format(date.y, "%m")) %>% group_by(year,month) %>% mutate_at(vars(alpha), na.locf) %>% summarize(date = first(date.x), ts_alpha = last(roughness_alpha), vol_alpha = last(alpha)) %>% ungroup()
cor(as.matrix(alpha_ts_monthly %>% select(ts_alpha, vol_alpha)))

# give monthly data to csv so that python can do asset pricing test
write.csv(alpha_ts_monthly,"vol_alpha.csv", row.names = F)


# try use average implied vol from put and call

alpha_ts_from_impl_vol <- spx_vol_surface %>% na.omit() %>% group_by(date,days) %>% summarise(impl_volatility = mean(impl_volatility), delta = first(delta)) filter(cp_flag=="C", impl_strike>0, impl_premium>0, 45<=abs(delta), abs(delta)<=55) %>% select(date, days, delta, impl_volatility, impl_strike) %>%
  group_by(date,days) %>% arrange(days) %>% mutate(delta_abs = abs(delta), log_K_S = log(impl_strike))  %>%
  summarize(ATMskew_day=abs(impl_volatility[delta_abs==55]-impl_volatility[delta_abs==45])/abs(log_K_S[delta_abs==55] - log_K_S[delta_abs==45])) %>% 
  mutate(tau=days/365) %>% arrange(tau) %>% summarize(alpha = alpha_estimation(tau,ATMskew_day), rsquare = alpha_estimation_rsquare(tau,ATMskew_day))# estimate for each day the alpha
summary(alpha_ts_from_impl_vol)





which(alpha_ts_from_impl_vol$alpha>0)

check <- lookat_a_day(alpha_ts_from_impl_vol$date[36])
