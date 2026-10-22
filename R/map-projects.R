#' Set the default directory for evidence maps
#'
#' Call this once with a folder on your computer (including a synced Google Drive
#' folder). The path is stored in your user configuration, never in the package.
#' @param path Existing directory for evidence-map projects.
#' @return Normalised path, invisibly.
#' @export
set_map_root <- function(path) {
  if (length(path) != 1L || is.na(path) || !nzchar(path)) {
    stop("Supply one non-empty path.", call. = FALSE)
  }
  path <- path.expand(path)
  if (!dir.exists(path)) stop("Map root does not exist: ", path, call. = FALSE)
  path <- normalizePath(path, winslash = "/", mustWork = TRUE)
  file <- .map_root_file()
  dir.create(dirname(file), recursive = TRUE, showWarnings = FALSE)
  writeLines(path, file, useBytes = TRUE)
  invisible(path)
}

.map_root_file <- function() {
  file.path(tools::R_user_dir("evidencelandscaper", which = "config"),
            "map_root.txt")
}

#' Get the default map directory
#' @return Directory path.
#' @export
map_root <- function() {
  file <- .map_root_file()
  if (!file.exists(file)) {
    stop("No map root configured. Call set_map_root('/path/to/EvidenceMaps') first.",
         call. = FALSE)
  }
  path <- readLines(file, n = 1L, warn = FALSE)
  if (!dir.exists(path)) stop("Configured map root is not accessible: ", path,
                              call. = FALSE)
  normalizePath(path, winslash = "/", mustWork = TRUE)
}

#' Create an independent local evidence map
#'
#' @param name Folder name using letters, numbers, underscores or hyphens.
#' @param root Parent directory. Defaults to the directory set by set_map_root().
#' @return Created directory path, invisibly.
#' @export
create_map <- function(name, root = NULL) {
  if (length(name) != 1L || is.na(name) ||
      !grepl("^[A-Za-z0-9][A-Za-z0-9_-]*$", name)) {
    stop("name must contain only letters, numbers, underscores or hyphens.",
         call. = FALSE)
  }
  if (is.null(root)) root <- map_root()
  root <- normalizePath(path.expand(root), winslash = "/", mustWork = TRUE)
  if (!dir.exists(root)) stop("Map root must be a directory.", call. = FALSE)
  path <- file.path(root, name)
  if (file.exists(path) || dir.exists(path)) {
    stop("Map destination already exists: ", path, call. = FALSE)
  }
  dir.create(path, showWarnings = FALSE)
  folders <- c("inputs/search", "inputs/manual_sources", "inputs/ontology",
               "data/raw", "data/intermediate", "data/final",
               "runs", "outputs/reports", "outputs/dashboard")
  for (folder in folders) {
    dir.create(file.path(path, folder), recursive = TRUE, showWarnings = FALSE)
  }
  metadata <- c(paste0("name: ", name),
                paste0("created_utc: ", format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")),
                "archive: manual")
  writeLines(metadata, file.path(path, "map.yml"))
  invisible(path)
}

#' Open an existing evidence map
#' @param path Absolute path, or a map name inside the configured map root.
#' @return Normalised project directory.
#' @export
open_map <- function(path) {
  if (length(path) != 1L || is.na(path) || !nzchar(path)) {
    stop("Supply one map path or name.", call. = FALSE)
  }
  resolved <- path.expand(path)
  if (!dir.exists(resolved) && !grepl("[/\\\\]", path)) {
    resolved <- file.path(map_root(), path)
  }
  if (!file.exists(file.path(resolved, "map.yml"))) {
    stop("No map.yml found at: ", resolved, call. = FALSE)
  }
  normalizePath(resolved, winslash = "/", mustWork = TRUE)
}
