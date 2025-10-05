# Build and Install Package
# Standard R package development workflow with instantiate

# Step 1: Configure Stan models (generates Makevars, cleanup scripts, etc.)
message("Configuring Stan package...")
instantiate::stan_package_configure()

# Step 2: Document package (generates NAMESPACE and .Rd files from roxygen)
message("Documenting package...")
devtools::document()

attachment::att_amend_desc()

# Step 3: Build vignettes
message("Building vignettes...")
devtools::build_vignettes()

# Step 4: Check package (optional but recommended before install)
message("Checking package...")
devtools::check()

# Step 5: Install package (compiles Stan models during installation)
message("Installing package...")
devtools::install()

message("✓ Build complete!")
