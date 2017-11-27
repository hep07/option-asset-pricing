# ff3_regression <- function(x,ff4, to_keep_threshold) {
#   core <- cbind(x,ff4) #%>% inner_join(ff4, by=c("Date"="Date")) %>% select(-RF,-Date,-PERMNO)
#   idx <- complete.cases(core)
#   core <- core[idx,]
#   if(nrow(croe)<to_keep_threshold) {
#     return(rep(NA,length(x)))
#   } else {
#     res <- lm(retadj.1mn~., data=core)
#     return(res$residuals)
#   }
#   
#   #summary(res)
#   #summary(res$residuals)
# }


ff3_regression <- function(x, to_keep_threshold) {
  core <- cbind(y=x,X_mat)
  idx <- complete.cases(core)
  core <- core[idx,]
  out <- rep(NA,length(x))
  if(nrow(core)<to_keep_threshold) {
    return(out)
  } else {
    res <- lm(y~., data=core)
    out[idx] <- res$residuals
    return(out)
  }
  
  
}
