import.functions <- function(fpath = NA) {
  
  fpath <- ifelse(is.na(fpath),
                  "/home/minoli/git_lpjmlToolKit/development/functions/",
                  fpath)
  cat("\nImporting functions from:", fpath)
  
  fsources <- paste0(fpath, list.files(fpath, pattern = ".R", recursive = T))
  invisible(lapply(fsources, source))
  cat("\nImporting functions:\n", "-----------------",
      list.files(fpath, pattern = ".R", recursive = T), sep = "\n")
  
}

