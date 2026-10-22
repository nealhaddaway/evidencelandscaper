#' Retrieve missing publication abstracts from ScienceDirect
#'
#' Uses only Elsevier META_ABS, never FULL. First checks every source
#' manifestation in each final W01 cluster for an existing abstract.
#' Source records and W01 clusters remain immutable.
#' @param scored Existing completed W01 full-scoring directory.
#' @param max_requests Maximum DOI requests (0 = dry run).
#' @param pause_seconds Minimum delay between requests.
#' @return W02 ScienceDirect abstract-enrichment directory, invisibly.
#' @export
enrich_sciencedirect_abstracts <- function(scored,max_requests=0L,pause_seconds=0.5) {
  if(!dir.exists(scored)) stop("W01 scoring directory not found.")
  scored<-normalizePath(scored,winslash="/",mustWork=TRUE)
  run<-dirname(scored)
  final<-file.path(run,"finalised")
  map_file<-file.path(final,"manifestation_cluster_map.csv")
  if(!file.exists(map_file) || !file.exists(file.path(final,"summary.json")))
    stop("Finalised W01 cluster map is required.")
  final_meta<-jsonlite::fromJSON(file.path(final,"summary.json"))
  if(!identical(final_meta$status,"final")) stop("W01 finalisation not complete.")
  map<-utils::read.csv(map_file,stringsAsFactors=FALSE,check.names=FALSE)
  required<-c("source","source_record_id","cluster_id")
  if(length(setdiff(required,names(map))) ||
     anyDuplicated(paste(map$source,map$source_record_id,sep="::")))
    stop("Invalid W01 manifestation map.")
  src_manifest<-file.path(run,"union","source_inputs.json")
  if(!file.exists(src_manifest)) stop("Original W01 source manifest missing.")
  src<-jsonlite::fromJSON(src_manifest,simplifyVector=FALSE)$sources
  if(is.null(src)||!length(src)) stop("W01 source manifest empty.")
  scalar<-function(x) {
    if(is.null(x)||!length(x)) return("")
    y<-as.character(unlist(x,use.names=FALSE));y<-y[!is.na(y)&nzchar(trimws(y))]
    if(!length(y)) "" else y[[1L]]
  }
  manifest_record_id<-function(x,source) {
    if(identical(source,"lens")) return(scalar(x$identity$lens_id))
    scalar(x$sidecar_identity$sidecar_record_id)
  }
  doi_clean<-function(x) {
    x<-scalar(x)
    x<-sub("^https?://(dx\\.)?doi\\.org/","",x,ignore.case=TRUE)
    x<-sub("^doi:\\s*","",x,ignore.case=TRUE)
    x<-trimws(x)
    if(!grepl("^10\\.[0-9]{4,9}/[^[:space:]]+$",x)) return("")
    x
  }
  lookup<-setNames(as.character(map$cluster_id),
      paste(map$source,map$source_record_id,sep="::"))
  found<-vector("list",nrow(map))
  pos<-0L
  for(source in names(src)) {
    path<-scalar(src[[source]])
    if(!file.exists(path)) stop("Missing W01 source input: ",source)
    con<-file(path,"rt",encoding="UTF-8")
    repeat {
      lines<-readLines(con,n=500L,warn=FALSE)
      if(!length(lines)) break
      for(line in lines) {
        if(!nzchar(trimws(line))) next
        z<-jsonlite::fromJSON(line,simplifyVector=FALSE)
        id<-manifest_record_id(z,source)
        key<-paste(source,id,sep="::")
        cid<-lookup[[key]]
        if(is.null(cid)) stop("Source record missing in final W01 cluster: ",key)
        fields<-if(identical(source,"lens")) z$canonical else z$mapped_fields
        raw<-if(identical(source,"lens")) z$lens$raw_payload else NULL
        abs<-scalar(fields$abstract)
        if(!nzchar(abs)) abs<-scalar(raw$abstract)
        doi<-doi_clean(fields$doi)
        if(!nzchar(doi)) doi<-doi_clean(raw$doi)
        pos<-pos+1L
        found[[pos]]<-data.frame(cluster_id=cid,source=source,
           source_record_id=id,doi=doi,abstract_present=nzchar(abs),
           stringsAsFactors=FALSE)
      }
    }
    close(con)
  }
  if(pos!=nrow(map)) stop("Source manifestation count differs from final W01 map.")
  records<-do.call(rbind,found)
  groups<-split(seq_len(nrow(records)),records$cluster_id)
  needed<-lapply(names(groups),function(cid) {
    ix<-groups[[cid]]
    if(any(records$abstract_present[ix])) return(NULL)
    dois<-unique(records$doi[ix]);dois<-dois[nzchar(dois)]
    if(!length(dois)) return(data.frame(cluster_id=cid,doi="",reason="no_doi"))
    if(length(dois)>1L) return(data.frame(cluster_id=cid,doi="",reason="conflicting_dois"))
    data.frame(cluster_id=cid,doi=dois[[1L]],reason="eligible")
  })
  needed<-Filter(Negate(is.null),needed)
  plan<-if(length(needed))do.call(rbind,needed) else data.frame(
    cluster_id=character(),doi=character(),reason=character())
  dest<-file.path(run,"W02_sciencedirect_abstracts")
  if(!dir.exists(dest))dir.create(dest,recursive=TRUE)
  plan_file<-file.path(dest,"missing_abstract_plan.csv")
  source_sha<-digest::digest(file=map_file,algo="sha256",serialize=FALSE)
  plan_sha<-digest::digest(serialize(plan,NULL),algo="sha256")
  state_file<-file.path(dest,"plan_identity.json")
  if(file.exists(state_file)) {
    old<-jsonlite::fromJSON(state_file)
    if(!identical(old$w01_map_sha256,source_sha)||
       !identical(old$plan_sha256,plan_sha))
      stop("W01 inputs or enrichment plan changed: refusing existing output directory.")
  } else {
    if(length(list.files(dest,all.files=TRUE,no..=TRUE)))
      stop("Existing untracked W02 directory; preserve files for inspection.")
    utils::write.csv(plan,plan_file,row.names=FALSE)
    jsonlite::write_json(list(schema="local-w02-sciencedirect-plan-v1",
      w01_map_sha256=source_sha,plan_sha256=plan_sha,
      clusters=length(groups),missing_abstract_clusters=nrow(plan)),
      state_file,auto_unbox=TRUE,pretty=TRUE)
  }
  cat("[W02] ",length(groups)," clusters; ",nrow(plan),
      " clusters lack abstracts across their source manifestations; ",
      sum(plan$reason=="eligible")," have a single candidate DOI.\n",sep="")
  max_requests<-as.integer(max_requests)
  if(is.na(max_requests)||max_requests<0L)stop("max_requests must be >= 0.")
  if(max_requests==0L) {
    message("[W02] Dry run only; no API requests.")
    return(invisible(dest))
  }
  api_key<-Sys.getenv("SCOPUS_API_TOKEN")
  inst<-Sys.getenv("SCOPUS_INSTTOKEN")
  if(!nzchar(api_key)||!nzchar(inst))
    stop("Existing SCOPUS_API_TOKEN and SCOPUS_INSTTOKEN must be configured.")
  if(!requireNamespace("xml2",quietly=TRUE))
    stop("Package xml2 is required to parse ScienceDirect XML.")
  eligible<-plan[plan$reason=="eligible",,drop=FALSE]
  attempts<-0L
  for(i in seq_len(nrow(eligible))) {
    if(attempts>=max_requests)break
    row<-eligible[i,,drop=FALSE]
    key<-digest::digest(row$doi,algo="sha256",serialize=FALSE)
    file<-file.path(dest,paste0("doi-",key,".json"))
    if(file.exists(file))next
    attempts<-attempts+1L
    result<-tryCatch({
      resp<-httr2::request(paste0(
        "https://api.elsevier.com/content/article/doi/",
        utils::URLencode(row$doi,reserved=FALSE))) |>
        httr2::req_url_query(view="META_ABS") |>
        httr2::req_headers("X-ELS-APIKey"=api_key,
          "X-ELS-Insttoken"=inst,Accept="text/xml") |>
        httr2::req_error(is_error=function(resp) FALSE) |>
        httr2::req_timeout(45) |> httr2::req_perform()
      status<-httr2::resp_status(resp)
      xml<-httr2::resp_body_string(resp)
      abstract<-""
      returned_doi<-""
      if(status==200L) {
        if(!requireNamespace("xml2",quietly=TRUE))
          stop("Install xml2 to parse Elsevier XML.")
        doc<-xml2::read_xml(xml)
        nodes<-xml2::xml_find_all(doc,
          "//*[local-name()='abstract' or local-name()='abstracts']")
        if(length(nodes)) abstract<-trimws(xml2::xml_text(nodes[[1L]]))
        dn<-xml2::xml_find_first(doc,"//*[local-name()='doi']")
        if(!inherits(dn,"xml_missing"))returned_doi<-doi_clean(xml2::xml_text(dn))
      }
      match_doi<-nzchar(returned_doi) &&
        identical(tolower(returned_doi),tolower(row$doi))
      list(schema="local-w02-sciencedirect-result-v1",cluster_id=row$cluster_id,
        requested_doi=row$doi,status=status,
        abstract=if(match_doi) abstract else "",
        returned_doi=returned_doi,
        result=if(status!=200L) "http_error" else if(!match_doi)
          "identity_unverified" else if(!nzchar(abstract))
          "abstract_missing" else "abstract_retrieved",
        retrieved_at_utc=format(Sys.time(),tz="UTC",
                                format="%Y-%m-%dT%H:%M:%SZ"))
    },error=function(e) list(schema="local-w02-sciencedirect-result-v1",
      cluster_id=row$cluster_id,requested_doi=row$doi,result="request_error",
      error=conditionMessage(e),retrieved_at_utc=format(Sys.time(),
        tz="UTC",format="%Y-%m-%dT%H:%M:%SZ")))
    jsonlite::write_json(result,file,auto_unbox=TRUE,pretty=TRUE)
    if(pause_seconds>0)Sys.sleep(pause_seconds)
  }
  message("[W02] ",attempts," new ScienceDirect abstract requests; cached results retained.")
  invisible(dest)
}
