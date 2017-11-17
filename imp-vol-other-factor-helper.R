
# this is for implie vol surface that is filtered with only 30 days
cal_avg_skew <- function(imp_vol, days, delta, cp_flag) {
  # this is from Xie, and uses delta +/- 25 and both calls and puts
  temp <- imp_vol[(delta==-25)] - imp_vol[(delta==+25)]
  return(temp)
}


# this is for implie vol surface that is filtered with only 30 days
cal_avg_vol_slope <- function(imp_vol, days, delta, cp_flag) {
  # this is from Xie, and uses delta +/- 25 and both calls and puts

  slope_P <- imp_vol[(delta==-50) & (days==60)] - imp_vol[(delta==-50) & (days==30)]
  slope_C <- imp_vol[(delta==50) & (days==60)] - imp_vol[(delta==50) & (days==30)]
  
  return(mean(c(slope_P,slope_C), na.rm = T))
}
