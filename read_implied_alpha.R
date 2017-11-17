result_file_names <- list.files("./imp_alpha_results")
res <- lapply(result_file_names, function(f) readRDS(paste("./imp_alpha_results/", f)))

# temp <- list()
# temp[[1]] <- readRDS("2011_3_imp_alpha.rds")
# temp[[2]] <- readRDS("2011_3_imp_alpha.rds")
res <- do.call(rbind, res)


# remove NA caused by only 1 valid point on the ATM skew term structure
res <- res %>% drop_na() 
saveRDS(res, "imp_alpha_results.rds")