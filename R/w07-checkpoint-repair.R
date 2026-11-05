#' Repair literal JSON null entries in a W07 validated checkpoint
#'
#' Only removes JSONL lines that equal the literal "null". The complete original
#' checkpoint is copied into a SHA-256-named backup and verified before an
#' atomic replacement. All other lines must have unique non-empty record IDs.
#' Repeated calls do nothing when no null lines remain.
#' @param w07 Exact W07 run directory.
#' @return Number of quarantined null lines, invisibly.
#' @export
repair_w07_null_checkpoint <- function(w07) {
  w07 <- normalizePath(w07,winslash="/",mustWork=TRUE)
  path <- file.path(w07,"coding.jsonl")
  if(!file.exists(path))stop("Missing W07 checkpoint: ",path)
  lines <- readLines(path,warn=FALSE,encoding="UTF-8")
  if(any(!nzchar(trimws(lines))))
    stop("Checkpoint has empty lines; inspect manually before repair.")
  null_idx <- which(trimws(lines)=="null")
  if(!length(null_idx)) {
    message("[W07 repair] No literal null entries; checkpoint unchanged.")
    return(invisible(0L))
  }
  parsed <- lapply(lines,function(line)
    jsonlite::fromJSON(line,simplifyVector=FALSE))
  valid_idx <- setdiff(seq_along(lines),null_idx)
  ids <- vapply(parsed[valid_idx],function(z) {
    if(!is.list(z)||!is.character(z$record_id)||
       length(z$record_id)!=1L||is.na(z$record_id)||
       !nzchar(z$record_id))
      stop("Unexpected non-null W07 checkpoint record. No changes made.")
    z$record_id
  },character(1))
  if(anyDuplicated(ids))stop("Duplicate W07 record IDs. No changes made.")
  hash <- function(p)digest::digest(file=p,algo="sha256",serialize=FALSE)
  original_hash <- hash(path)
  backup <- file.path(w07,paste0("coding-before-null-repair-",
                                 original_hash,".jsonl"))
  if(!file.exists(backup)) {
    if(!file.copy(path,backup,overwrite=FALSE))
      stop("Unable to create checkpoint backup.")
  }
  if(!identical(hash(backup),original_hash))
    stop("Checkpoint backup verification failed. No changes made.")
  tmp <- tempfile(pattern=".w07-repair-",tmpdir=w07,fileext=".jsonl")
  on.exit(if(file.exists(tmp))unlink(tmp),add=TRUE)
  writeLines(lines[valid_idx],tmp,useBytes=TRUE)
  if(!identical(readLines(tmp,warn=FALSE,encoding="UTF-8"),lines[valid_idx]))
    stop("Checkpoint repair staging verification failed.")
  if(!identical(hash(path),original_hash))
    stop("Checkpoint changed concurrently. No changes made.")
  if(!file.rename(tmp,path))
    stop("Unable to replace checkpoint atomically; original backup is safe.")
  audit <- file.path(w07,paste0("null-repair-",original_hash,".json"))
  jsonlite::write_json(list(schema="w07-null-repair-v1",
    source_sha256=original_hash,backup=backup,
    quarantined_line_numbers=as.list(as.integer(null_idx)),
    preserved_valid_records=length(ids),result_sha256=hash(path)),
    audit,pretty=TRUE,auto_unbox=TRUE)
  message("[W07 repair] Quarantined ",length(null_idx)," literal null line(s); ",
          length(ids)," original assessments preserved. Backup: ",backup)
  invisible(as.integer(length(null_idx)))
}
