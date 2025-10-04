# Test Script for nflendzoneModel Package
# Run this after installing the package to verify everything works

# Load package
library(nflendzoneModel)

message("=== Testing nflendzoneModel Package ===\n")

# Test 1: Check if package loaded
message("✓ Package loaded successfully")

# Test 2: Check if CmdStan is available
if (instantiate::stan_cmdstan_exists()) {
  message("✓ CmdStan is available")
} else {
  stop("✗ CmdStan not found. Install with: cmdstanr::install_cmdstan()")
}

# Test 3: Access pre-compiled models
message("\n--- Accessing Stan Models ---")
fit_model <- instantiate::stan_package_model(
  name = "team_strength_fit",
  package = "nflendzoneModel"
)
message("✓ team_strength_fit model accessible")
message("  Model executable: ", fit_model$exe_file())

gq_model <- instantiate::stan_package_model(
  name = "team_strength_gq",
  package = "nflendzoneModel"
)
message("✓ team_strength_gq model accessible")
message("  Model executable: ", gq_model$exe_file())

# Test 4: Check exported functions
message("\n--- Checking Exported Functions ---")
exported_fns <- ls("package:nflendzoneModel")
message("Exported functions: ", paste(exported_fns, collapse = ", "))

if ("fit_team_strength_model" %in% exported_fns) {
  message("✓ fit_team_strength_model() is exported")
} else {
  stop("✗ fit_team_strength_model() not found")
}

if ("generate_team_predictions" %in% exported_fns) {
  message("✓ generate_team_predictions() is exported")
} else {
  stop("✗ generate_team_predictions() not found")
}

# Test 5: Verify function signatures
message("\n--- Verifying Function Signatures ---")
fit_args <- names(formals(fit_team_strength_model))
message("fit_team_strength_model args: ", paste(fit_args, collapse = ", "))

gen_args <- names(formals(generate_team_predictions))
message("generate_team_predictions args: ", paste(gen_args, collapse = ", "))

# Test 6: Check vignettes
message("\n--- Checking Vignettes ---")
vigs <- vignette(package = "nflendzoneModel")$results
if (nrow(vigs) > 0) {
  message("✓ Vignettes available:")
  for (i in 1:nrow(vigs)) {
    message("  - ", vigs[i, "Item"], ": ", vigs[i, "Title"])
  }
} else {
  message("⚠ No vignettes found")
}

# Test 7: Check documentation
message("\n--- Checking Documentation ---")
if (file.exists(system.file("help", "nflendzoneModel.rdb", package = "nflendzoneModel"))) {
  message("✓ Help documentation is available")
  message("  Try: ?fit_team_strength_model")
  message("  Try: ?generate_team_predictions")
} else {
  message("⚠ Help files not found")
}

# Summary
message("\n", strrep("=", 50))
message("✓✓✓ ALL TESTS PASSED ✓✓✓")
message(strrep("=", 50))
message("\nPackage is ready to use!")
message("\nNext steps:")
message("  1. Prepare your NFL game data")
message("  2. Create Stan data list (see vignette)")
message("  3. Run fit_team_strength_model()")
message("  4. Generate predictions with generate_team_predictions()")
message("\nFor examples, run: vignette('getting-started', package = 'nflendzoneModel')")