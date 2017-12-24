library(timeDate)

tS1 = timeSequence(from = "1996-01-01", to = "2015-01-31", by = "month")
tS2 = timeSequence(from = "2015-02-01", to = "2016-12-31", by = "month")
# standard option expiration date, Saturday after the thrid Friday in each month for all options with expiration date before 2015-02-01
# and thsi holds whether the third Friday is a holiday or not
ts_opt_exp1 <- as.Date(timeNthNdayInMonth(tS1, nday = 5, nth = 3, format = "%Y-%m-%d")) + 1
ts_opt_exp2 <- as.Date(timeNthNdayInMonth(tS2, nday = 5, nth = 3, format = "%Y-%m-%d"))
res <- c(as.Date(ts_opt_exp1), as.Date(ts_opt_exp2))

#expiring_Fris<- c(as.Date(ts_opt_exp1)-1, as.Date(ts_opt_exp2))

# Fri_holiday_idx <- which(expiring_Fris %in% NYSE_holiday)

# expiring_Fris[Fri_holiday_idx]

saveRDS(res, "./opt_exp_dates.rds")

port_formation_dates <- c(ts_opt_exp1+2, ts_opt_exp2 + 3)
table(weekdays(port_formation_dates))

# if Monday is holiday, use Tue 
NYSE_holiday <- as.Date(holidayNYSE(year=1996:2016))

Mon_holiday_idx <- which(port_formation_dates %in% NYSE_holiday)

port_formation_dates[Mon_holiday_idx] <- port_formation_dates[Mon_holiday_idx] + 1


# check the formation dates does not contain any NYSE holidays
sum(port_formation_dates %in% NYSE_holiday)
# distribution of the weekdays of formation dates 
table(weekdays(port_formation_dates))


saveRDS(port_formation_dates, "./port_formation_dates.rds")


# we need a vector of option last trading day to calculate the terminal payoff of the option
# if there is holiday, use previous day, i.e. the Thursday
terminal_payoff_dates <- c(as.Date(ts_opt_exp1)-1, as.Date(ts_opt_exp2))
table(weekdays(terminal_payoff_dates))
terminal_payoff_dates_NYSEholiday_idx <- which(terminal_payoff_dates %in% NYSE_holiday)
terminal_payoff_dates[terminal_payoff_dates_NYSEholiday_idx] <- 
  terminal_payoff_dates[terminal_payoff_dates_NYSEholiday_idx] -1 

table(weekdays(terminal_payoff_dates))
sum(terminal_payoff_dates %in% NYSE_holiday) # check no more holiday

saveRDS(terminal_payoff_dates, "./terminal_payoff_dates.rds")
