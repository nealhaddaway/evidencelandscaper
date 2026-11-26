#' Recover remaining W02 abstracts using Semantic Scholar and DataCite
#' @param scored Final W01 scoring directory.
#' @param input_dir Latest validated W02 corpus.
#' @return New checksummed W02 corpus directory.
#' @export
recover_w02_doi_metadata <- function(scored,input_dir) {
  input_dir <- normalizePath(input_dir,mustWork=TRUE)
  scored <- normalizePath(scored,mustWork=TRUE)
  input <- file.path(input_dir,"canonical.jsonl")
  sha <- function(p) digest::digest(file=p,algo="sha256",serialize=FALSE)
  meta <- jsonlite::fromJSON(file.path(input_dir,"summary.json"))
  input_sha <- sha(input)
  stopifnot(identical(meta$output_sha256,input_sha))
  for(p in c("xml2","stringdist")) if(!requireNamespace(p,quietly=TRUE)) stop("Required package: ",p)
  scalar <- function(x) {
    z <- as.character(unlist(x,use.names=FALSE))
    z <- z[!is.na(z)&nzchar(trimws(z))]
    if(length(z)) trimws(z[[1L]]) else ""
  }
  doi <- function(x) {
    d <- tolower(sub("^https?://(dx\\.)?doi\\.org/","",scalar(x),ignore.case=TRUE))
    d <- sub("^doi:\\s*","",d,ignore.case=TRUE)
    if(grepl("^10\\.[0-9]{4,9}/[^[:space:]]+$",d)) d else ""
  }
  norm <- function(x) trimws(gsub("[^[:alnum:]]+"," ",tolower(scalar(x))))
  clean <- function(x) {
    s <- scalar(x)
    if(grepl("<[^>]+>",s)) {
      doc <- tryCatch(xml2::read_html(paste0("<div>",s,"</div>")),error=function(e)NULL)
      if(!is.null(doc)) s <- xml2::xml_text(xml2::xml_find_first(doc,"//div"))
    }
    s <- trimws(gsub("[[:space:]]+"," ",s))
    s <- sub("^(Abstract|Summary)[[:space:]:.]+","",s,ignore.case=TRUE)
    if(nchar(s)<40L || grepl("^(no abstract|abstract unavailable|not available)",s,ignore.case=TRUE)) "" else s
  }
  get <- function(req) {
    resp <- req |> httr2::req_timeout(30) |> httr2::req_retry(max_tries=2,max_seconds=60) |>
      httr2::req_error(is_error=function(r)FALSE) |> httr2::req_perform()
    st <- httr2::resp_status(resp)
    if(st %in% c(404L,410L)) return(NULL)
    if(st!=200L) stop("HTTP ",st)
    resp
  }
  rows <- lapply(readLines(input,warn=FALSE,encoding="UTF-8"),jsonlite::fromJSON,simplifyVector=FALSE)
  ids <- vapply(rows,function(r)scalar(r$identity$record_id),character(1))
  ds <- vapply(rows,function(r)doi(r$canonical$doi),character(1))
  ts <- vapply(rows,function(r)norm(r$canonical$title),character(1))
  has <- function(r)nzchar(scalar(r$canonical$abstract))
  before <- vapply(rows,has,logical(1))
  stopifnot(!anyDuplicated(ids),all(nzchar(ids)),length(rows)==meta$publication_clusters)
  effective <- ds
  cp <- file.path(input_dir,"discovered_dois.csv")
  if(file.exists(cp)) {
    x <- utils::read.csv(cp,stringsAsFactors=FALSE)
    for(j in seq_len(nrow(x))) {
      i <- match(x$record_id[j],ids)
      d <- doi(x$candidate_doi[j])
      if(!is.na(i)&&!before[i]&&!nzchar(ds[i])&&nzchar(d)&&!d%in%ds)
        effective[i] <- d
    }
  }
  groups <- split(which(nzchar(effective)),effective[nzchar(effective)])
  ix <- which(!before&nzchar(effective)&nzchar(ts))
  out <- file.path(dirname(scored),"W02_main",paste0("doi_metadata_",substr(input_sha,1L,12L)))
  dir.create(out,recursive=TRUE,showWarnings=FALSE)
  cache <- file.path(out,"checkpoints");dir.create(cache,showWarnings=FALSE)
  key <- Sys.getenv("SEMANTIC_SCHOLAR_API_KEY")
  if(!nzchar(key))key <- Sys.getenv("S2_API_KEY")
  if(!nzchar(key))key <- Sys.getenv("SEMANTICSCHOLAR_API_KEY")
  fetch_s2 <- function(d) {
    req <- httr2::request("https://api.semanticscholar.org/graph/v1/paper/batch") |>
      httr2::req_url_query(fields="title,abstract,externalIds,paperId") |>
      httr2::req_body_json(list(ids=as.list(paste0("DOI:",d))),auto_unbox=TRUE)
    if(nzchar(key))req <- httr2::req_headers(req,"x-api-key"=key)
    r <- get(req)
    if(is.null(r))return(list())
    z <- httr2::resp_body_json(r,simplifyVector=FALSE)
    Filter(Negate(is.null),lapply(z,function(h) {
      if(is.null(h))return(NULL)
      list(doi=doi(h$externalIds$DOI),title=scalar(h$title),abstract=clean(h$abstract))
    }))
  }
  fetch_dc <- function(d) {
    r <- get(httr2::request(paste0("https://api.datacite.org/dois/",utils::URLencode(d,reserved=TRUE))))
    if(is.null(r))return(NULL)
    h <- httr2::resp_body_json(r,simplifyVector=FALSE)$data$attributes
    if(identical(scalar(h$types$resourceTypeGeneral),"Dataset"))return(NULL)
    ab <- Filter(function(z)identical(scalar(z$descriptionType),"Abstract"),h$descriptions)
    ti <- Filter(function(z)!nzchar(scalar(z$titleType)),h$titles)
    if(length(ab)!=1L||length(ti)!=1L)return(NULL)
    list(doi=doi(h$doi),title=scalar(ti[[1L]]$title),abstract=clean(ab[[1L]]$description))
  }
  validate <- function(h,i) {
    if(is.null(h)||!identical(h$doi,effective[i])||!nzchar(h$abstract)||!nzchar(norm(h$title)))return(FALSE)
    t <- ts[i]; g <- groups[[effective[i]]]
    if(length(g)>1L)return(identical(t,norm(h$title))&&sum(ts[g]==t)==1L)
    sim <- 1-stringdist::stringdist(t,norm(h$title),method="jw",p=0.1)
    !is.na(sim)&&sim>=0.90
  }
  message("[W02 DOI metadata] ",length(ix)," eligible records; Semantic Scholar key available: ",nzchar(key))
  unique_d <- unique(effective[ix])
  s2 <- list(); s2_errors <- character()
  for(b in split(unique_d,ceiling(seq_along(unique_d)/50L))) {
    p <- file.path(cache,paste0("s2_",digest::digest(b,algo="sha256"),".rds"))
    if(file.exists(p)) z <- readRDS(p) else {
      z <- tryCatch(list(hits=fetch_s2(b)),error=function(e)list(error=TRUE))
      if(!isTRUE(z$error)){tmp<-tempfile(tmpdir=cache);saveRDS(z,tmp);if(!file.rename(tmp,p))stop("Checkpoint write failed.")}
    }
    if(isTRUE(z$error))s2_errors <- c(s2_errors,b) else s2 <- c(s2,z$hits)
    message("[W02 Semantic Scholar] Batch processed: ",length(b),"; error: ",isTRUE(z$error))
    Sys.sleep(1)
  }
  audit <- vector("list",length(ix))
  for(j in seq_along(ix)) {
    i <- ix[j];d <- effective[i]
    a <- list(record_id=ids[i],doi=d,canonical_doi=ds[i],provider=NULL,match=NULL,attempts=list())
    h <- Filter(function(z)identical(z$doi,d),s2)
    if(d%in%s2_errors)a$attempts$semantic_scholar <- "request_error"
    else if(length(h)==1L&&validate(h[[1L]],i)){
      a$provider <- "semantic_scholar";a$match <- h[[1L]];a$attempts$semantic_scholar <- "accepted"
    } else a$attempts$semantic_scholar <- "no_verified_abstract"
    if(is.null(a$match)) {
      p <- file.path(cache,paste0("dc_",digest::digest(d,algo="sha256"),".rds"))
      if(file.exists(p))z<-readRDS(p) else {
        z<-tryCatch(list(hit=fetch_dc(d)),error=function(e)list(error=TRUE))
        if(!isTRUE(z$error)){tmp<-tempfile(tmpdir=cache);saveRDS(z,tmp);if(!file.rename(tmp,p))stop("Checkpoint write failed.")}
        Sys.sleep(0.25)
      }
      if(isTRUE(z$error))a$attempts$datacite <- "request_error"
      else if(validate(z$hit,i)){
        a$provider <- "datacite";a$match <- z$hit;a$attempts$datacite <- "accepted"
      } else a$attempts$datacite <- "no_verified_abstract"
    }
    if(!is.null(a$match)){
      rows[[i]]$canonical$abstract <- a$match$abstract
      rows[[i]]$doi_metadata_provenance <- list(provider=a$provider,returned_doi=d,returned_title=a$match$title)
    }
    audit[[j]]<-a
    if(j==1L||j%%25L==0L||j==length(ix))message("[W02 DOI metadata] ",j,"/",length(ix),
      "; recovered ",sum(vapply(audit[seq_len(j)],function(a)!is.null(a$match),logical(1))))
  }
  after <- vapply(rows,has,logical(1))
  stopifnot(all(after[before]),identical(ids,vapply(rows,function(r)scalar(r$identity$record_id),character(1))),
    identical(ds,vapply(rows,function(r)doi(r$canonical$doi),character(1))))
  write_jsonl <- function(x,p){
    con<-file(p,"wt",encoding="UTF-8");on.exit(close(con))
    for(z in x)writeLines(jsonlite::toJSON(z,auto_unbox=TRUE,null="null",na="null",digits=NA),con,useBytes=TRUE)
  }
  canonical<-file.path(out,"canonical.jsonl");ap<-file.path(out,"audit.jsonl")
  write_jsonl(rows,canonical);write_jsonl(audit,ap)
  utils::write.csv(data.frame(record_id=ids[!after],doi=ds[!after],title=vapply(rows[!after],function(r)scalar(r$canonical$title),character(1))),
    file.path(out,"remaining_missing_abstracts.csv"),row.names=FALSE)
  if(file.exists(cp)){
    x$abstract_recovered <- after[match(x$record_id,ids)]
    utils::write.csv(x,file.path(out,"discovered_dois.csv"),row.names=FALSE)
  }
  outcomes <- vapply(audit,function(a)if(is.null(a$provider))"unresolved" else a$provider,character(1))
  attempts <- unlist(lapply(audit,function(a)unlist(a$attempts)),use.names=FALSE)
  jsonlite::write_json(list(status="complete",publication_clusters=length(rows),
    missing_before=sum(!before),targeted_records=length(ix),recovered=sum(after)-sum(before),
    missing_after=sum(!after),abstracts_after=sum(after),remaining_with_canonical_doi=sum(!after&nzchar(ds)),
    remaining_with_lookup_doi=sum(!after&nzchar(effective)),outcomes=as.list(table(outcomes)),
    request_errors=sum(attempts=="request_error"),semantic_scholar_error_records=sum(effective[ix]%in%s2_errors),
    input_sha256=input_sha,output_sha256=sha(canonical),audit_sha256=sha(ap)),
    file.path(out,"summary.json"),auto_unbox=TRUE,pretty=TRUE)
  invisible(out)
}
