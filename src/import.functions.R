import.functions <- function(fpath = NA) {
  
  if (is.na(fpath)) {
    fpath <- "/home/minoli/git_lpjmlToolKit/development/functions/"
    cat("\nImporting functions from:", fpath)
  }
  fsources <- paste0(fpath, list.files(fpath, pattern = ".R", recursive = T))
  invisible(lapply(fsources, source))
  cat("\nImporting functions:\n", "-----------------",
      list.files(fpath, pattern = ".R", recursive = T), sep = "\n")  
  
}
