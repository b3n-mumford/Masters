# auto_git_backup.R
# Automatically commit and push changes to GitHub if online

setwd("~/Library/CloudStorage/OneDrive-UniversitedeMontreal/MSc_Thesis/Coding")

# Helper to check internet
is_online <- function() {
  tryCatch({
    ping <- system("ping -c 1 github.com", ignore.stdout = TRUE, ignore.stderr = TRUE)
    ping == 0
  }, error = function(e) FALSE)
}

if (is_online()) {
  message("Internet detected — committing and pushing to GitHub.")
  system("git add .")
  msg <- paste("Auto commit:", Sys.Date())
  system(paste("git commit -m", shQuote(msg)))
  system("git push origin main")
} else {
  message("No internet detected — skipping push for now.")
}
