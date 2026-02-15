# Helper script to update DESCRIPTION and NAMESPACE without reinstalling/compiling
# Run this after editing R/*.R files to update dependencies and documentation.

message("Step 1: Updating DESCRIPTION dependencies...")
# document = FALSE avoids triggering roxygen/devtools which might try to compile
# update.config = TRUE ensures we overwrite any stale settings in dev/config_attachment.yaml
attachment::att_amend_desc(path = ".", document = FALSE, update.config = TRUE)

message("Step 2: Updating NAMESPACE and man/ files...")
# roxygenise usually loads code without triggering src/ compilation
roxygen2::roxygenise()

message(
  "Done! Metadata updated. Use devtools::load_all() to test R changes interactively."
)
