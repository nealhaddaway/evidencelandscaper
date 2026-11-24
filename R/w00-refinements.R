# Internal renderer for structured, reusable proximity refinement inputs.
# Each pair uses individual operands, never Boolean groups within proximity.
.w00_refinement_query <- function(spec, source) {
  if (is.character(spec) && length(spec)==1L && nzchar(spec)) return(spec)
  if (!is.list(spec) || !identical(spec$kind, "pairwise_proximity"))
    stop("Unsupported refinement query specification.")
  if (!source %in% c("openalex", "lens")) stop("Unsupported proximity provider.")
  distance <- spec$distance
  if (!is.numeric(distance) || length(distance)!=1L || is.na(distance) ||
      distance < 1 || distance != as.integer(distance)) stop("Invalid proximity distance.")
  quote_term <- function(x) {
    if (any(!grepl("^[[:alnum:] _?*-]+$", x))) stop("Invalid proximity term.")
    paste0('"', x, '"')
  }
  pairs <- unlist(lapply(spec$groups, function(g) {
    left <- unlist(g$left, use.names=FALSE)
    right <- unlist(g$right, use.names=FALSE)
    if (!length(left) || !length(right)) stop("Empty proximity operand group.")
    grid <- expand.grid(left=left, right=right, stringsAsFactors=FALSE)
    if (source=="openalex") {
      return(sprintf("(%s and %s within %d words of each other)",
        quote_term(grid$left), quote_term(grid$right), as.integer(distance)))
    }
    if (any(grepl("[*?]", c(left,right)))) stop("Lens phrase proximity requires explicit word forms.")
    sprintf('"%s %s"~%d', grid$left, grid$right, as.integer(distance))
  }), use.names=FALSE)
  if (!length(pairs)) stop("No proximity pairs.")
  terms <- paste0("(", paste(unique(c(pairs, unlist(spec$additional_phrases,use.names=FALSE))), collapse=" OR "), ")")
  expr <- paste(terms, spec$remainder, sep=" AND ")
  fields <- unlist(spec$fields,use.names=FALSE)
  if (source=="openalex") {
    if (length(fields)!=1L || !fields %in% c("title-abstract","title-abstract-keywords")) stop("Invalid OpenAlex fields.")
    return(sprintf("works where %s has (%s) and type is not [dataset](dataset)", fields, expr))
  }
  if (!length(fields) || any(!fields %in% c("title","abstract","keyword"))) stop("Invalid Lens fields.")
  paste0("(", paste(sprintf("%s:(%s)",fields,expr),collapse=" OR "), ") AND NOT publication_type:dataset")
}

#' Scope database-specific search refinements without harvesting records
#' @param map Existing evidence map path or name.
#' @param variants JSON file containing variants with id and source-specific complete queries, and baseline_counts and baseline_run.
#' @param strategy Base strategy JSON file.
#' @return Data frame of baseline and refinement counts, invisibly.
#' @export
scope_w00_refinements <- function(map, variants, strategy) {
  map_dir <- open_map(map)
  variants <- normalizePath(variants, mustWork=TRUE)
  refinement_dir <- dirname(variants)
  cfg <- jsonlite::fromJSON(variants, simplifyVector=FALSE)
  base_strategy <- jsonlite::fromJSON(strategy, simplifyVector=FALSE)
  sources <- names(cfg$baseline_counts)
  stopifnot(length(sources)>0L, length(cfg$variants)>0L, !anyDuplicated(sources))
  ids <- vapply(cfg$variants, function(v) v$id, character(1))
  stopifnot(!anyDuplicated(ids), all(grepl("^[A-Za-z0-9_-]+$", ids)), all(grepl("^[A-Za-z0-9_-]+$", sources)))
  out_dir <- file.path(map_dir, "outputs", "reports", "w00_refinement")
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  stamp <- format(Sys.time(), "%Y%m%dT%H%M%S", tz = "UTC")
  summary_path <- file.path(out_dir, paste0("counts-", stamp, ".csv"))
  selection <- file.path(map_dir, "data", "workflow00", "source_selection.json")
  old_selection <- if (file.exists(selection)) readBin(selection, "raw",
                                       n = file.info(selection)$size) else NULL
  
  run_refinement <- function() {
    on.exit({
      if (!is.null(old_selection)) writeBin(old_selection, selection)
      else if (file.exists(selection)) unlink(selection)
    }, add = TRUE)
    results <- list()
    for (src in sources) {
      results[[length(results)+1L]] <- data.frame(
        variant="00_baseline", source_slug=src,
        hits=as.numeric(cfg$baseline_counts[[src]]), status="archived baseline",
        run_dir=cfg$baseline_run)
    }
    for (v in cfg$variants) {
      for (src in intersect(sources, names(v))) {
        message("\n[refinement] ", v$id, " | ", src)
        strategy <- base_strategy
        query <- .w00_refinement_query(v[[src]], src)
        strategy$database_overrides[[src]] <- query
        path <- file.path(refinement_dir, paste0(v$id, "-", src, ".json"))
        jsonlite::write_json(strategy, path, auto_unbox=TRUE, pretty=TRUE)
        run_id <- NA_character_
        row <- tryCatch({
          p <- plan_w00(map_dir, strategy=path, sources=src, mode="full")
          run_id <- basename(p$run_dir)
          actual <- jsonlite::fromJSON(p$search_plan, simplifyVector=FALSE)$source_queries[[src]]
          stopifnot(identical(actual, query))
          saveRDS(p, file.path(p$run_dir, "plan_reference.rds"))
          cts <- read.csv(scope_w00(p), stringsAsFactors=FALSE)
          stopifnot(nrow(cts)==1L, identical(cts$source_slug, src))
          data.frame(variant=v$id, source_slug=src, hits=cts$hits,
                     status=cts$status, run_dir=run_id)
        }, error=function(e) data.frame(variant=v$id, source_slug=src,
                           hits=NA_real_, status=conditionMessage(e), run_dir=run_id))
        results[[length(results)+1L]] <- row
        write.csv(do.call(rbind, results), summary_path, row.names=FALSE, na="")
        print(row[, c("variant","source_slug","hits","status")])
      }
    }
    result <- do.call(rbind, results)
    print(result[, c("variant","source_slug","hits","status")])
    message("Saved: ", summary_path)
    invisible(result)
  }
  
  run_refinement()
}
