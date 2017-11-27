
file_dir <- "./monthly_ML_prepare/"
result_file_names <- list.files(file_dir)
res <- lapply(result_file_names, function(f) readRDS(paste(file_dir, f,sep="")))

# temp <- list()
# temp[[1]] <- readRDS("2011_3_imp_alpha.rds")
# temp[[2]] <- readRDS("2011_3_imp_alpha.rds")
res <- do.call(rbind, res)
saveRDS(res, "imp_vol_surf_features_ret.rds")
