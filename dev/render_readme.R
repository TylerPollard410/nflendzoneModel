# Render README from R Markdown
# Run this after editing README.Rmd

if (!requireNamespace("rmarkdown", quietly = TRUE)) {
  stop("Please install rmarkdown package: install.packages('rmarkdown')")
}

message("Rendering README.Rmd to README.md...")
rmarkdown::render("README.Rmd", output_format = "github_document")

message("✓ README.md generated from README.Rmd")
message("\nNext: Commit both README.Rmd and README.md")