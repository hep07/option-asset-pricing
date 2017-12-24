#!/apps/R-3.4.0/bin/Rscript


args = commandArgs(trailingOnly=TRUE)
file_dir = as.character(args[1])
saved_file_name <- as.character(args[2])
#file_dir <- "./expected_opt_ret/"
result_file_names <- list.files(file_dir)
res <- lapply(result_file_names, function(f) readRDS(paste(file_dir, f,sep="")))

# temp <- list()
# temp[[1]] <- readRDS("2011_3_imp_alpha.rds")
# temp[[2]] <- readRDS("2011_3_imp_alpha.rds")
res <- do.call(rbind, res)
saveRDS(res, paste("./combined_df/", saved_file_name, sep=""))
