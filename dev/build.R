# Build and Install Package
# Based on instantiate development workflow

# Step 1: Build vignettes (if they exist)
if (dir.exists("vignettes") && length(list.files("vignettes", pattern = "\\.Rmd$")) > 0) {
  message("Building vignettes...")
  devtools::build_vignettes()
}

# Step 2: Configure Stan models for package compilation
message("Configuring Stan package...")
instantiate::stan_package_configure()

# Step 3: Generate documentation
message("Generating documentation...")
roxygen2::roxygenize()

# Step 4: Install package (compiles Stan models)
# Note: Do NOT use pkgload::load_all() - install the standard way
message("Installing package...")
install.packages(".", repos = NULL, type = "source")

# Step 5: Verify models are accessible
message("Verifying models...")
library(nflendzoneModel)
fit_mod <- get_team_strength_fit_model()
gq_mod <- get_team_strength_gq_model()
message("✓ Package built and models loaded successfully")