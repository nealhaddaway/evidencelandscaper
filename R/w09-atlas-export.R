#' Export a provenance-linked W09 case atlas dataset
#'
#' Deterministically joins the immutable W09 extraction ledger to the verified
#' bibliographic inventory by exact record_id. No model or retrieval calls.
#' The publication-case observation ID is content-addressed; it is stable for
#' identical observations within the same extraction, but is NOT a persistent
#' real-world initiative ID across model revisions.
#' @param w09 W09 run directory.
#' @param extraction_dir Completed versioned W09 case-extraction directory.
#' @param coordinates_csv Optional human-reviewed coordinates CSV with columns
#'   observation_id,latitude,longitude,coordinate_basis,coordinate_source.
#' @return Output directory, invisibly.
#' @export
export_w09_evidence_atlas <- function(w09, extraction_dir, coordinates_csv = NULL) {
  w09 <- normalizePath(w09, winslash = "/", mustWork = TRUE)
  extraction_dir <- normalizePath(extraction_dir, winslash = "/", mustWork = TRUE)
  sha <- function(p) digest::digest(file = p, algo = "sha256", serialize = FALSE)
  inv_path <- file.path(w09, "w09_extraction_ready_inventory.csv")
  registry_path <- file.path(extraction_dir, "fulltext_case_registry.jsonl")
  manifest_path <- file.path(extraction_dir, "manifest.json")
  prompt_path <- file.path(extraction_dir, "prompt.txt")
  ontology_path <- file.path(extraction_dir, "ontology.csv")
  required <- c(inv_path, registry_path, manifest_path, prompt_path, ontology_path)
  if (!all(file.exists(required))) stop("W09 export missing input: ", paste(required[!file.exists(required)], collapse = "; "))
  inventory <- utils::read.csv(inv_path, stringsAsFactors = FALSE, check.names = FALSE)
  fields <- c("record_id","title","doi","file","sha256","status")
  if (!all(fields %in% names(inventory))) stop("W09 inventory missing required fields.")
  if (anyNA(inventory$record_id) || anyDuplicated(inventory$record_id) ||
      any(!nzchar(trimws(inventory$record_id)))) stop("W09 inventory has invalid publication IDs.")
  lines <- readLines(registry_path, warn = FALSE, encoding = "UTF-8")
  lines <- lines[nzchar(trimws(lines))]
  records <- lapply(lines, jsonlite::fromJSON, simplifyVector = FALSE)
  rec_ids <- vapply(records, function(z) z$record_id, character(1))
  if (anyDuplicated(rec_ids) || !setequal(rec_ids, inventory$record_id))
    stop("W09 case registry and extraction-ready inventory publication IDs do not match exactly.")
  manifest <- jsonlite::fromJSON(manifest_path, simplifyVector = FALSE)
  if (!identical(manifest$prompt_sha256, sha(prompt_path)) ||
      !identical(manifest$ontology_sha256, sha(ontology_path)))
    stop("W09 prompt/ontology provenance mismatch.")
  if (any(is.na(inventory$sha256) | !nzchar(inventory$sha256)))
    stop("W09 inventory has missing source checksums.")
  cases <- list()
  n <- 0L
  for (i in seq_along(records)) {
    z <- records[[i]]
    publication <- inventory[match(z$record_id, inventory$record_id), , drop = FALSE]
    if (!identical(z$provenance$source_sha256, publication$sha256[[1L]]))
      stop("Source hash mismatch for ", z$record_id)
    if (!identical(z$provenance$prompt_sha256, manifest$prompt_sha256) ||
        !identical(z$provenance$ontology_sha256, manifest$ontology_sha256) ||
        !identical(z$provenance$w07_coding_sha256, manifest$coding_sha256) ||
        !identical(z$provenance$model, manifest$model))
      stop("Extraction provenance mismatch for ", z$record_id)
    title <- publication$title[[1L]]
    if (is.na(title) || !nzchar(trimws(title))) stop("Missing verified article title: ", z$record_id)
    doi <- publication$doi[[1L]]
    doi <- if (is.na(doi)) "" else trimws(as.character(doi))
    doi <- sub("^https?://(?:dx\\.)?doi\\.org/", "", doi, ignore.case = TRUE)
    doi <- sub("^doi:", "", doi, ignore.case = TRUE)
    doi_url <- if(nzchar(doi)) paste0("https://doi.org/",utils::URLencode(doi, reserved=FALSE)) else NULL
    scholar_url <- paste0("https://scholar.google.com/scholar?q=",
                          utils::URLencode(paste0('"',title,'"'), reserved=TRUE))
    for (j in seq_along(z$cases)) {
      ca <- z$cases[[j]]
      n <- n + 1L
      # Deterministic within an extraction, not a cross-run entity reconciliation.
      observation_id <- paste0(z$record_id, "-case-", sprintf("%03d",j))
      cases[[n]] <- list(
        observation_id=observation_id,
        observation_index=j,
        publication=list(record_id=z$record_id,title=title,
          doi=if(nzchar(doi)) doi else NULL,doi_url=doi_url,
          google_scholar_url=scholar_url,
          source_format=z$provenance$source_format,
          source_sha256=z$provenance$source_sha256),
        extraction=list(model=manifest$model,
          prompt_sha256=manifest$prompt_sha256,
          ontology_sha256=manifest$ontology_sha256,
          coding_sha256=manifest$coding_sha256,
          w07_run=manifest$w07_run),
        case=ca,
        coordinates=list(latitude=NULL,longitude=NULL,
          coordinate_basis="not_georeferenced", coordinate_source=NULL)
      )
    }
  }
  if (!is.null(coordinates_csv)) {
    loc <- utils::read.csv(coordinates_csv, stringsAsFactors=FALSE)
    req <- c("observation_id","latitude","longitude","coordinate_basis","coordinate_source")
    if(!all(req %in% names(loc)) || anyDuplicated(loc$observation_id) ||
       any(!loc$observation_id %in% vapply(cases, function(x)x$observation_id, character(1))))
      stop("Invalid coordinates CSV: require unique matched observation IDs and all coordinate provenance fields.")
    for(i in seq_along(cases)) {
      k <- match(cases[[i]]$observation_id, loc$observation_id)
      if(is.na(k)) next
      lat <- suppressWarnings(as.numeric(loc$latitude[[k]]))
      lon <- suppressWarnings(as.numeric(loc$longitude[[k]]))
      basis <- loc$coordinate_basis[[k]]
      source <- loc$coordinate_source[[k]]
      if(is.na(lat) || is.na(lon) || abs(lat)>90 || abs(lon)>180 ||
         is.na(basis) || !nzchar(trimws(basis)) ||
         is.na(source) || !nzchar(trimws(source)))
        stop("Invalid or unproven coordinates for ",cases[[i]]$observation_id)
      cases[[i]]$coordinates <- list(latitude=lat,longitude=lon,
        coordinate_basis=basis,coordinate_source=source)
    }
  }
  # Content-addressed output avoids replacing or mixing extractions.
  inputs <- c(inv_path,registry_path,manifest_path,prompt_path,ontology_path)
  if(!is.null(coordinates_csv)) inputs <- c(inputs,coordinates_csv)
  digest_key <- digest::digest(vapply(inputs,sha,character(1)),algo="sha256",serialize=TRUE)
  output <- file.path(w09,"atlas_exports",paste0("linked-",substr(digest_key,1,12)))
  dir.create(output,recursive=TRUE,showWarnings=FALSE)
  output_json <- file.path(output,"case_evidence.json")
  output_csv <- file.path(output,"case_index.csv")
  out_manifest <- file.path(output,"export_manifest.json")
  index <- do.call(rbind,lapply(cases,function(x) {
    data.frame(observation_id=x$observation_id,record_id=x$publication$record_id,
      case_name=if(is.null(x$case$case_name)) "" else as.character(x$case$case_name),
      article_title=x$publication$title,
      doi=if(is.null(x$publication$doi)) "" else x$publication$doi,
      doi_url=if(is.null(x$publication$doi_url)) "" else x$publication$doi_url,
      scholar_url=x$publication$google_scholar_url,
      latitude=if(is.null(x$coordinates$latitude)) NA_real_ else x$coordinates$latitude,
      longitude=if(is.null(x$coordinates$longitude)) NA_real_ else x$coordinates$longitude,
      coordinate_basis=x$coordinates$coordinate_basis,
      coordinate_source=if(is.null(x$coordinates$coordinate_source)) "" else x$coordinates$coordinate_source,
      stringsAsFactors=FALSE)
  }))
  # Output files are immutable per input digest, including human geocoding edits.
  if(file.exists(out_manifest)) {
    old <- jsonlite::fromJSON(out_manifest)
    if(!identical(old$input_sha256,unname(vapply(inputs,sha,character(1)))) ||
       !identical(old$output_sha256$case_evidence,sha(output_json)) ||
       !identical(old$output_sha256$case_index,sha(output_csv)))
      stop("Archived atlas export differs from inputs; refusing overwrite.")
    message("[W09 atlas] ",length(cases),"/",length(cases)," existing cases verified: ",output)
    return(invisible(output))
  }
  if(file.exists(output_json) || file.exists(output_csv))stop("Existing partial atlas export: ",output)
  jsonlite::write_json(list(schema="w09-linked-evidence-atlas-v1",cases=cases),
                       output_json,auto_unbox=TRUE,pretty=TRUE,null="null")
  utils::write.csv(index,output_csv,row.names=FALSE,na="")
  meta <- list(schema="w09-linked-evidence-atlas-manifest-v1",
    input_files=basename(inputs),input_sha256=unname(vapply(inputs,sha,character(1))),
    publication_count=length(records),case_count=length(cases),
    output_sha256=list(case_evidence=sha(output_json),case_index=sha(output_csv)))
  jsonlite::write_json(meta,out_manifest,auto_unbox=TRUE,pretty=TRUE)
  message("[W09 atlas] ",length(cases),"/",length(cases),
          " cases linked across ",length(records)," publications; export: ",output)
  invisible(output)
}
