# Regenerate the Ostara mechanism meeting webpage without re-running W07.
# Run from the Ostara_collective_action_map project directory after downloading
# ostara_harmonised_mechanisms_hierarchy_review_v2.csv into that directory.
root <- normalizePath(getwd(), mustWork = TRUE)
w07 <- file.path(root, "runs", "W07")
runs <- c(
  targeted = "full-170-5e43f57800-23f44de7f9-model-gpt-6-luna",
  expanded = "full-631-5e43f57800-23f44de7f9-model-gpt-6-luna"
)
paths <- file.path(w07, unname(runs))
if (!all(dir.exists(paths))) {
  stop("Expected W07 run directories were not found beneath: ", w07)
}
harmonised <- file.path(root, "ostara_harmonised_mechanisms_hierarchy_review_v2.csv")
if (!file.exists(harmonised))
  stop("Place the downloaded harmonisation CSV here: ", harmonised)
pkg <- if (dir.exists(file.path(root, "evidencelandscaper"))) {
  file.path(root, "evidencelandscaper")
} else {
  NA_character_
}
if (!is.na(pkg) && requireNamespace("pkgload", quietly = TRUE)) {
  pkgload::load_all(pkg, quiet = TRUE)
} else if (!"evidencelandscaper" %in% loadedNamespaces()) {
  if (!requireNamespace("evidencelandscaper", quietly = TRUE))
    stop("Install the evidencelandscaper package from the hierarchy-review branch first.")
}
output <- file.path(root, "reports", "mechanism_typology",
                    "ostara_mechanisms_hierarchy_review.html")
if (file.exists(output)) stop("Output already exists: ", output)
result <- evidencelandscaper::publish_ostara_mechanisms(
  targeted_w07 = paths[[1]],
  expanded_w07 = paths[[2]],
  output_file = output,
  harmonised_csv = harmonised
)
message("Website: ", result)
