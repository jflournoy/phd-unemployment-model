#!/usr/bin/env Rscript
#' Test IPUMS API Connection
#'
#' Quick script to verify IPUMS API key works before downloading data

# Load environment
readRenviron(".Renviron")

# Load ipumsr
library(ipumsr)

cat("🔍 Testing IPUMS API Connection\n")
cat("================================\n\n")

# Check API key
api_key <- Sys.getenv("IPUMS_API_KEY")
if (api_key == "") {
  stop("❌ IPUMS_API_KEY not found. Run readRenviron('.Renviron') first.")
}
cat("✅ API key loaded (", nchar(api_key), " characters)\n\n", sep = "")

# Test API connection by fetching sample info
cat("📡 Testing API connection...\n")
tryCatch(
  {
    samples <- get_sample_info("usa")
    cat("✅ API connection successful!\n\n")

    cat("📊 Available IPUMS USA samples:\n")
    cat("   Total samples:", nrow(samples), "\n")

    # Show recent ACS samples
    recent_acs <- samples[grep("^us20[2-9][0-9]a$", samples$name), ]
    if (nrow(recent_acs) > 0) {
      cat("\n   Recent ACS samples:\n")
      for (i in 1:min(10, nrow(recent_acs))) {
        cat("   - ", recent_acs$name[i], ": ", recent_acs$description[i], "\n", sep = "")
      }
    }

    cat("\n✅ Ready to download IPUMS data!\n")
  },
  error = function(e) {
    cat("❌ API connection failed:\n")
    cat("   ", conditionMessage(e), "\n")
    cat("\nPossible issues:\n")
    cat("   - Invalid API key\n")
    cat("   - No internet connection\n")
    cat("   - IPUMS API is down\n")
  }
)
