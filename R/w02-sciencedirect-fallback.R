#' Add ScienceDirect abstracts after original W02-main enrichment
#'
#' Only canonical records still lacking abstracts are considered. Reuses DOI
#' response caches from the earlier W02 ScienceDirect trial when available.
#' Never requests FULL content or overwrites enriched W02 records.
#' @param scored W01 full-scoring directory.
#' @param max_requests Maximum new Elsevier calls, default 0 (offline).
#' @return Directory containing separate ScienceDirect fallback overlay.
#' @export
add_w02_sciencedirect <- function(scored,max_requests=0L) {
  scored<-normalizePath(scored,winslash="/",mustWork=TRUE)
  root<-dirname(scored)
  main<-file.path(root,"W02_main")
  source<-file.path(main,"enriched.jsonl")
  if(!file.exists(source)||!file.exists(file.path(main,"report.json")))
    stop("Run original W02-main enrichment first.")
  cache<-file.path(root,"W02_sciencedirect_abstracts")
  cache_meta<-file.path(cache,"plan_identity.json")
  if(!file.exists(cache_meta))
    stop("Original ScienceDirect cache plan missing. Run existing dry-run plan first.")
  outdir<-file.path(main,"sciencedirect_fallback")
  output<-file.path(outdir,"suggestions.jsonl")
  manifest<-file.path(outdir,"manifest.json")
  sha<-function(p)digest::digest(file=p,algo="sha256",serialize=FALSE)
  if(file.exists(manifest)) {
    m<-jsonlite::fromJSON(manifest)
    if(!identical(m$input_sha256,sha(source))||
       !file.exists(output)||!identical(m$output_sha256,sha(output)))
      stop("ScienceDirect fallback cache failed checksum validation.")
    message("[W02] Verified previous ScienceDirect suggestions; checking for additional eligible requests.")
  }
  if(!file.exists(manifest) && dir.exists(outdir) &&
     length(list.files(outdir,all.files=TRUE,no..=TRUE)))
    stop("Incomplete ScienceDirect fallback output; preserve for inspection.")
  max_requests<-as.integer(max_requests)
  if(is.na(max_requests)||max_requests<0L)stop("max_requests must be non-negative.")
  lines<-readLines(source,warn=FALSE,encoding="UTF-8")
  lines<-lines[nzchar(trimws(lines))]
  records<-lapply(lines,jsonlite::fromJSON,simplifyVector=FALSE)
  scalar<-function(x) {
    if(is.null(x)||!length(x))return("")
    y<-as.character(unlist(x,use.names=FALSE))
    y<-y[!is.na(y)&nzchar(trimws(y))]
    if(length(y))trimws(y[[1L]]) else ""
  }
  normalize_doi<-function(x) {
    x<-tolower(scalar(x))
    x<-sub("^https?://(dx\\.)?doi\\.org/","",x)
    x<-sub("^doi:\\s*","",x)
    x
  }
  requests<-0L
  suggestions<-list()
  eligible_count<-0L
  cached_count<-0L
  for(record in records) {
    if(nzchar(scalar(record$canonical$abstract))) next
    rid<-scalar(record$identity$record_id)
    doi<-normalize_doi(record$canonical$doi)
    if(!grepl("^10\\.[0-9]{4,9}/[^[:space:]]+$",doi))next
    eligible_count<-eligible_count+1L
    cache_file<-file.path(cache,paste0("doi-",
      digest::digest(doi,algo="sha256",serialize=FALSE),".json"))
    if(!file.exists(cache_file) && requests<max_requests) {
      # The original ScienceDirect adapter performs META_ABS and caches results.
      # Use the same source keys and XML parsing; only eligible missing records.
      key<-Sys.getenv("SCOPUS_API_TOKEN")
      token<-Sys.getenv("SCOPUS_INSTTOKEN")
      if(!nzchar(key)||!nzchar(token))stop("Elsevier credentials not configured.")
      if(!requireNamespace("xml2",quietly=TRUE))
        stop("xml2 required to parse ScienceDirect XML.")
      retrieval<-tryCatch({
        resp<-httr2::request(paste0("https://api.elsevier.com/content/article/doi/",
                                   utils::URLencode(doi,reserved=FALSE))) |>
          httr2::req_url_query(view="META_ABS") |>
          httr2::req_headers("X-ELS-APIKey"=key,
                "X-ELS-Insttoken"=token,Accept="text/xml") |>
          httr2::req_error(is_error=function(x)FALSE) |>
          httr2::req_timeout(45) |> httr2::req_perform()
        status<-httr2::resp_status(resp)
        returned_doi<-""; abstract<-""
        if(status==200L) {
          xml<-xml2::read_xml(httr2::resp_body_string(resp))
          dn<-xml2::xml_find_first(xml,"//*[local-name()='doi']")
          if(!inherits(dn,"xml_missing"))
            returned_doi<-normalize_doi(xml2::xml_text(dn))
          an<-xml2::xml_find_first(xml,"//*[local-name()='abstract']")
          if(!inherits(an,"xml_missing"))abstract<-trimws(xml2::xml_text(an))
        }
        matched<-identical(doi,returned_doi)
        list(schema="local-w02-sciencedirect-result-v1",cluster_id=rid,
             requested_doi=doi,returned_doi=returned_doi,status=status,
             abstract=if(matched)abstract else "",
             result=if(status!=200L)"http_error" else if(!matched)
                    "identity_unverified" else if(!nzchar(abstract))
                    "abstract_missing" else "abstract_retrieved")
      },error=function(e)list(schema="local-w02-sciencedirect-result-v1",
              cluster_id=rid,requested_doi=doi,result="request_error",
              error=conditionMessage(e)))
      jsonlite::write_json(retrieval,cache_file,auto_unbox=TRUE,pretty=TRUE)
      requests<-requests+1L
      Sys.sleep(0.5)
    }
    if(!file.exists(cache_file))next
    cached_count<-cached_count+1L
    cached<-jsonlite::fromJSON(cache_file,simplifyVector=FALSE)
    # A cached request may have been associated with a different cluster;
    # accept only by requested and returned DOI, never by cached cluster index.
    if(!identical(cached$result,"abstract_retrieved") ||
       !identical(normalize_doi(cached$requested_doi),doi) ||
       !identical(normalize_doi(cached$returned_doi),doi) ||
       !nzchar(scalar(cached$abstract)))next
    suggestions[[length(suggestions)+1L]]<-list(
      schema="local-w02-abstract-suggestion-v1",
      record_id=rid,doi=doi,provider="sciencedirect",
      abstract=scalar(cached$abstract),
      cache_sha256=sha(cache_file),review_status="pending_identity_review")
  }
  dir.create(outdir,recursive=TRUE,showWarnings=FALSE)
  previous<-if(file.exists(manifest)) jsonlite::fromJSON(manifest) else NULL
  if(!is.null(previous) && requests==0L &&
     identical(as.integer(previous$suggestions),as.integer(length(suggestions)))) {
    message("[W02] No changes; ",eligible_count," DOI-linked records still missing abstracts; ",
            cached_count," cached responses available.")
    return(invisible(outdir))
  }
  if(file.exists(manifest)) {
    previous_dir<-file.path(outdir,"previous")
    dir.create(previous_dir,showWarnings=FALSE)
    stamp<-format(Sys.time(),"%Y%m%dT%H%M%S",tz="UTC")
    for(p in c(output,manifest)) if(file.exists(p) &&
       !file.copy(p,file.path(previous_dir,paste0(stamp,"-",basename(p)))))
      stop("Could not preserve previous W02 ScienceDirect overlay.")
  }
  writeLines(vapply(suggestions,function(x)
    jsonlite::toJSON(x,auto_unbox=TRUE,null="null"),character(1)),output)
  jsonlite::write_json(list(schema="local-w02-sciencedirect-fallback-v1",
    input_sha256=sha(source),output_sha256=sha(output),
    suggestions=length(suggestions),eligible_missing_abstracts=eligible_count,
    cached_responses=cached_count,new_requests=requests,
    automatic_changes_to_canonical=0L),
    manifest,auto_unbox=TRUE,pretty=TRUE)
  message("[W02] ScienceDirect: ",eligible_count," DOI-linked abstracts still missing; ",
          length(suggestions)," suggestions; ",requests,
          " new requests. No canonical abstracts overwritten.")
  invisible(outdir)
}
