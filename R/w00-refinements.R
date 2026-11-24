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
        strategy$database_overrides[[src]] <- v[[src]]
        path <- file.path(refinement_dir, paste0(v$id, "-", src, ".json"))
        jsonlite::write_json(strategy, path, auto_unbox=TRUE, pretty=TRUE)
        run_id <- NA_character_
        row <- tryCatch({
          p <- plan_w00(map_dir, strategy=path, sources=src, mode="full")
          run_id <- basename(p$run_dir)
          actual <- jsonlite::fromJSON(p$search_plan, simplifyVector=FALSE)$source_queries[[src]]
          stopifnot(identical(actual, v[[src]]))
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
