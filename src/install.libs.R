libs <- file.path(R_PACKAGE_DIR, "libs", R_ARCH)
dir.create(libs, recursive = TRUE, showWarnings = FALSE)
for (file in c("symbols.rds", Sys.glob(paste0("*", SHLIB_EXT)))) {
  if (file.exists(file)) {
    file.copy(file, file.path(libs, file))
  }
}
inst_stan <- file.path("..", "inst", "stan")
if (dir.exists(inst_stan)) {
  warning(
    "Stan models in inst/stan/ are deprecated in {instantiate} ",
    ">= 0.0.4.9001 (2024-01-03). Please put them in src/stan/ instead."
  )
  if (file.exists("stan")) {
    warning("src/stan/ already exists. Not copying models from inst/stan/.")
  } else {
    message("Copying inst/stan/ to src/stan/.")
    fs::dir_copy(path = inst_stan, new_path = "stan")
  }
}
bin <- file.path(R_PACKAGE_DIR, "bin")
if (!file.exists(bin)) {
  dir.create(bin, recursive = TRUE, showWarnings = FALSE)
}
bin_stan <- file.path(bin, "stan")
fs::dir_copy(path = "stan", new_path = bin_stan)
callr::r(
  func = function(bin_stan) {
    models <- instantiate::stan_package_model_files(path = bin_stan)

    # Filter models based on .stan-compile-ignore
    ignore_file <- file.path(bin_stan, ".stan-compile-ignore")
    if (file.exists(ignore_file)) {
      lines <- readLines(ignore_file)
      # Remove comments and empty lines
      patterns <- lines[!grepl("^\\s*#", lines) & nzchar(trimws(lines))]

      excluded_models <- unique(unlist(lapply(patterns, function(pat) {
        Sys.glob(file.path(bin_stan, pat))
      })))

      # Use normalizePath for robust comparison
      models_norm <- normalizePath(models, mustWork = FALSE)
      excluded_norm <- normalizePath(excluded_models, mustWork = FALSE)

      keep_idx <- !models_norm %in% excluded_norm
      models <- models[keep_idx]

      if (length(excluded_models) > 0) {
        message(sprintf(
          "Ignoring %d models based on .stan-compile-ignore",
          length(excluded_models)
        ))
      }
    }

    instantiate::stan_package_compile(
      models = models
    )
  },
  args = list(bin_stan = bin_stan),
  show = TRUE,
  stderr = "2>&1"
)
