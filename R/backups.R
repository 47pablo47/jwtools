#' Backup only .csv files
#'
#' Search given directory recursively and copy all .csv files to destination
#' @param source_dir A string refering to a source directory to browse
#' @param dest_dir A string refering to destination. Using only drive letter is not recommended
#' @examples 
#' backup_csv("D:/important_data/", "E:/backup")
#' @return nothing
#' @export
#' @import dplyr
backup_csv <- function(source_dir, dest_dir) {
  # Create the destination directory if it doesn't already exist
  dir.create(dest_dir, showWarnings = FALSE)
  # Recursively list all .csv files in the source directory and its subdirectories
  csv_files <- list.files(path = source_dir, pattern = "\\.csv$", recursive = TRUE, full.names = TRUE)
  # Loop through the csv_files list and copy each file to the destination directory,
  # preserving the directory structure using file.path()
  for (i in seq_along(csv_files)) {
    dest_path <- file.path(dest_dir, sub(source_dir, "", csv_files[i]))
    dir.create(dirname(dest_path), recursive = TRUE, showWarnings = FALSE)
    if (file.exists(dest_path)) {
      cat("Warning: ", dest_path, " already exists and will be overwritten.\n")
    }
    file.copy(from = csv_files[i], to = dest_path, overwrite = TRUE)
    cat(sprintf("Copied %s to %s\n", csv_files[i], dest_path))
  }
  # Print a message indicating the number of files copied
  cat(sprintf("Copied %d specified files to %s\n", length(csv_files), dest_dir))
}

#' Backup all data files
#'
#'Search given directory for csv, R, imj, and excell files to copy to destination
#' @param source_dir A string refering to a source directory to browse
#' @param dest_dir A string refering to destination. Using only drive letter is not recommended
#'
#' @return nothing
#'
#' @examples
#' backup_csv("D:/important_data/", "E:/backup")
#' @export
backup_data_files <- function(source_dir, dest_dir) {
  # Create the destination directory if it doesn't already exist
  if (dir.exists(dest_dir)==FALSE) {
    dir.create(dest_dir, showWarnings = FALSE)
  } 
  # Define the pattern for the specified file types
  pattern <- paste0(".*\\.(csv|xlsx|xls|R|rmd|ijm)$")
  
  # Recursively list all specified files in the source directory and its subdirectories
  all_files <- list.files(path = source_dir, pattern = pattern, recursive = TRUE, full.names = TRUE)
  
  # Loop through the all_files list and copy each file to the destination directory,
  # preserving the directory structure using file.path()
  for (i in seq_along(all_files)) {
    dest_path <- file.path(dest_dir, sub(source_dir, "", all_files[i]))
    dir.create(dirname(dest_path), recursive = TRUE, showWarnings = FALSE)
    if (file.exists(dest_path)) {
      cat("Warning: ", dest_path, " already exists and will be overwritten.\n")
    }
    file.copy(from = all_files[i], to = dest_path, overwrite = TRUE)
    cat(sprintf("Copied %s to %s\n", all_files[i], dest_path))
  }
  # Print a message indicating the number of files copied
  cat(sprintf("Copied %d specified files to %s\n", length(all_files), dest_dir))
}
