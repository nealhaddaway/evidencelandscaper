#' Prepare the original W02 canonical-record input from final W01 clusters
#'
#' Reads the unmodified W01 union and final manifestation-cluster mapping.
#' Does not call providers or modify any W00/W01 artefacts.
#' @param scored Existing W01 full_scoring directory.
#' @return Path of canonical JSONL input for W02-main.
#' @export
prepare_w02 <- function(scored) {
  scored<-normalizePath(scored,winslash="/",mustWork=TRUE)
  root<-dirname(scored)
  final_map<-file.path(root,"finalised","manifestation_cluster_map.csv")
  union_manifest<-file.path(root,"union","source_inputs.json")
  for(p in c(final_map,union_manifest,file.path(root,"finalised","summary.json")))
    if(!file.exists(p))stop("Missing finalised W01 input: ",p)
  w01<-jsonlite::fromJSON(file.path(root,"finalised","summary.json"))
  if(!identical(w01$status,"final")) stop("W01 not finalised.")
  map<-utils::read.csv(final_map,stringsAsFactors=FALSE)
  if(anyDuplicated(paste(map$source,map$source_record_id,sep="::")))
    stop("W01 source identities are not unique.")
  paths<-jsonlite::fromJSON(union_manifest,simplifyVector=FALSE)$sources
  if(is.null(paths))stop("W01 source inputs missing.")
  scalar<-function(x) {
    if(is.null(x)||!length(x))return(NULL)
    x<-as.character(unlist(x,use.names=FALSE))
    x<-x[!is.na(x)&nzchar(trimws(x))]
    if(length(x))x[[1L]] else NULL
  }
  clusters<-split(seq_len(nrow(map)),map$cluster_id)
  manifests<-vector("list",nrow(map))
  keys<-setNames(seq_len(nrow(map)),paste(map$source,map$source_record_id,sep="::"))
  seen<-rep(FALSE,nrow(map))
  for(source in names(paths)) {
    p<-scalar(paths[[source]])
    if(!file.exists(p))stop("Missing W01 union source ",source)
    con<-file(p,"rt",encoding="UTF-8")
    repeat {
      lines<-readLines(con,n=500L,warn=FALSE)
      if(!length(lines))break
      for(line in lines) {
        if(!nzchar(trimws(line)))next
        x<-jsonlite::fromJSON(line,simplifyVector=FALSE)
        rid<-if(identical(source,"lens"))scalar(x$identity$lens_id) else
          scalar(x$sidecar_identity$sidecar_record_id)
        k<-paste(source,rid,sep="::")
        idx<-keys[[k]]
        if(is.null(idx)||seen[[idx]])stop("W01 source identity mismatch: ",k)
        seen[[idx]]<-TRUE
        fields<-if(identical(source,"lens"))x$canonical else x$mapped_fields
        manifests[[idx]]<-list(
          source=source,source_record_id=rid,
          manifestation_metadata=list(
            title=fields$title,abstract=fields$abstract,doi=fields$doi,
            authors=fields$authors,year=fields$year,
            source=fields$source,author_keywords=fields$keywords),
          original_source_identity=list(source=source,source_record_id=rid))
      }
    }
    close(con)
  }
  if(!all(seen))stop("W01 source manifestations missing from union.")
  cleaned<-function(x) {z<-scalar(x);if(is.null(z))NULL else trimws(z)}
  normdoi<-function(x) {
    x<-cleaned(x);if(is.null(x))return(NULL)
    x<-sub("^https?://(dx\\.)?doi\\.org/","",x,ignore.case=TRUE)
    x<-sub("^doi:\\s*","",x,ignore.case=TRUE)
    if(grepl("^10\\.[0-9]{4,9}/",x))tolower(x) else NULL
  }
  rows<-lapply(names(clusters),function(cid) {
    subset<-manifests[clusters[[cid]]]
    get_field<-function(key) {
      vals<-lapply(subset,function(m)m$manifestation_metadata[[key]])
      for(v in vals){x<-cleaned(v);if(!is.null(x))return(x)}
      NULL
    }
    dois<-unique(Filter(Negate(is.null),lapply(subset,function(m)
      normdoi(m$manifestation_metadata$doi))))
    list(
      identity=list(record_id=cid),
      canonical=list(
        title=get_field("title"),
        abstract=get_field("abstract"),
        doi=if(length(dois)==1L)dois[[1L]] else NULL,
        author_keywords=get_field("author_keywords")),
      manifestations=subset
    )
  })
  if(length(rows)!=as.integer(w01$clusters) || nrow(map)!=as.integer(w01$source_manifestations))
    stop("W02 input does not reconcile with final W01 counts.")
  dest<-file.path(root,"W02_main")
  file<-file.path(dest,"canonical_input.jsonl")
  provenance<-file.path(dest,"input_manifest.json")
  sha<-function(p)digest::digest(file=p,algo="sha256",serialize=FALSE)
  if(file.exists(provenance)) {
    meta<-jsonlite::fromJSON(provenance)
    if(!identical(meta$w01_map_sha256,sha(final_map))||
       !file.exists(file)||!identical(meta$canonical_sha256,sha(file)))
      stop("Existing W02 canonical input failed provenance validation.")
    message("[W02] Reusing verified canonical input.")
    return(invisible(file))
  }
  if(dir.exists(dest)&&length(list.files(dest,all.files=TRUE,no..=TRUE)))
    stop("Untracked W02 input state exists; refusing overwrite.")
  dir.create(dest,recursive=TRUE,showWarnings=FALSE)
  data<-vapply(rows,function(x)jsonlite::toJSON(x,auto_unbox=TRUE,null="null"),
                character(1))
  writeLines(data,file,useBytes=TRUE)
  jsonlite::write_json(list(schema="local-w02-input-v1",
    w01_map_sha256=sha(final_map),canonical_sha256=sha(file),
    publication_clusters=length(rows),source_manifestations=nrow(map),
    missing_abstracts=sum(vapply(rows,function(x)is.null(x$canonical$abstract),
                                  logical(1)))),
    provenance,auto_unbox=TRUE,pretty=TRUE)
  message("[W02] Prepared ",length(rows)," canonical publication records from W01.")
  invisible(file)
}
