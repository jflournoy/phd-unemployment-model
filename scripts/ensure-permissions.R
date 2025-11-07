#' Ensure Proper File Permissions for Docker Volume
#'
#' Helper function to fix file ownership when working in Docker containers.
#' Call this at the end of analysis scripts to ensure files are accessible
#' from the host filesystem.
#'
#' @export
fix_permissions <- function() {
  script_path <- here::here("scripts", "set-ownership.sh")

  if (!file.exists(script_path)) {
    warning("Permission fix script not found: ", script_path)
    return(invisible(FALSE))
  }

  # Check if running in Docker
  in_docker <- file.exists("/.dockerenv")

  if (!in_docker) {
    message("Not running in Docker - skipping permission fix")
    return(invisible(FALSE))
  }

  # Run the fix script
  result <- system(script_path, wait = FALSE, ignore.stdout = TRUE, ignore.stderr = TRUE)

  if (result == 0) {
    message("\u2713 File permissions updated for host access")
    return(invisible(TRUE))
  } else {
    warning("Permission fix script failed")
    return(invisible(FALSE))
  }
}

#' Check Current File Ownership
#'
#' Diagnostic function to check if files are owned by root.
#'
#' @param path Character. Directory to check (default: project root)
#' @return Data frame with file ownership information
#' @export
check_ownership <- function(path = here::here()) {
  # Get file list with ownership
  cmd <- sprintf("find '%s' -type f -printf '%%u %%g %%p\\n' | head -20", path)
  output <- system(cmd, intern = TRUE)

  if (length(output) == 0) {
    message("No files found")
    return(invisible(NULL))
  }

  # Parse output
  ownership <- strsplit(output, " ")
  df <- data.frame(
    user = sapply(ownership, `[`, 1),
    group = sapply(ownership, `[`, 2),
    file = sapply(ownership, function(x) paste(x[-(1:2)], collapse = " ")),
    stringsAsFactors = FALSE
  )

  # Count by owner
  cat("\n=== File Ownership Summary ===\n")
  cat("User ownership:\n")
  print(table(df$user))

  # Highlight root-owned files
  root_files <- sum(df$user == "root")
  if (root_files > 0) {
    cat("\n\u26a0 ", root_files, " files owned by root\n")
    cat("Run fix_permissions() to fix\n")
  } else {
    cat("\n\u2713 No root-owned files\n")
  }

  return(invisible(df))
}
