#' Recover remaining W02 abstracts using final bounded metadata searches
#' @param scored Final W01 scoring directory.
#' @param input_dir Latest validated W02 corpus.
#' @return New checksummed W02 corpus directory.
#' @export
recover_w02_final_lookups <- function(scored,input_dir) {
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
  out <- file.path(dirname(scored),"W02_main",paste0("final_lookups_",substr(input_sha,1L,12L)))
  dir.create(out,recursive=TRUE,showWarnings=FALSE)
  cache <- file.path(out,"checkpoints");dir.create(cache,showWarnings=FALSE)
  validate <- function(h,i) {
    if(is.null(h)||!identical(h$doi,effective[i])||!nzchar(h$abstract)||!nzchar(norm(h$title)))return(FALSE)
    t <- ts[i];g <- groups[[effective[i]]]
    if(length(g)>1L)return(identical(t,norm(h$title))&&sum(ts[g]==t)==1L)
    sim <- 1-stringdist::stringdist(t,norm(h$title),method="jw",p=0.1)
    !is.na(sim)&&sim>=0.90
  }
  audit <- lapply(ix,function(i)list(record_id=ids[i],doi=effective[i],provider=NULL,match=NULL,attempts=list()))
  provider_status <- list()
  pending <- function()which(vapply(audit,function(a)is.null(a$match),logical(1)))
  apply_hits <- function(jj,hits,provider) {
    for(j in jj){
      h <- Filter(function(h)validate(h,ix[j]),hits)
      if(length(h)) {
        ab <- unique(vapply(h,function(z)z$abstract,character(1)))
        if(length(ab)==1L){
          audit[[j]]$match <<- h[[1L]]
          audit[[j]]$provider <<- provider
          audit[[j]]$attempts[[provider]] <<- "accepted"
          next
        }
      }
      audit[[j]]$attempts[[provider]] <<- if(length(h))"ambiguous_abstract" else "no_verified_abstract"
    }
  }
  cached <- function(label,fun) {
    p <- file.path(cache,paste0(digest::digest(label,algo="sha256"),".rds"))
    if(file.exists(p))return(readRDS(p))
    z <- tryCatch(list(hits=fun()),error=function(e)list(error=TRUE))
    tmp<-tempfile(tmpdir=cache);saveRDS(z,tmp);if(!file.rename(tmp,p))stop("Checkpoint write failed.")
    z
  }
  run_batches <- function(provider,fetch,size=20L,max_errors=2L) {
    jj <- pending(); d <- unique(effective[ix[jj]])
    errors <- 0L; done <- 0L
    if(!length(d)){provider_status[[provider]] <<- "no_remaining_targets";return(invisible(NULL))}
    for(b in split(d,ceiling(seq_along(d)/size))){
      k <- jj[effective[ix[jj]]%in%b]
      if(errors>=max_errors){
        for(j in k)audit[[j]]$attempts[[provider]] <<- "skipped_after_errors"
        next
      }
      z <- cached(list(provider,b),function()fetch(b))
      if(isTRUE(z$error)){
        errors<-errors+1L
        for(j in k)audit[[j]]$attempts[[provider]] <<- "request_error"
      } else apply_hits(k,z$hits,provider)
      done<-done+length(b)
      message("[W02 final] ",provider,": ",done,"/",length(d)," DOI lookups; remaining records: ",length(pending()))
      Sys.sleep(0.3)
    }
    provider_status[[provider]] <<- if(errors>=max_errors)"stopped_after_errors" else if(errors)"completed_with_errors" else "complete"
  }
  lens_token <- Sys.getenv("LENS_API_TOKEN")
  lens <- function(b){
    req <- httr2::request("https://api.lens.org/scholarly/search") |>
      httr2::req_headers(Authorization=paste("Bearer",lens_token),Accept="application/json") |>
      httr2::req_body_json(list(query=list(terms=setNames(list(as.list(b)),"ids.doi")),size=500L,
        include=as.list(c("title","abstract","external_ids","ids","publication_type"))),auto_unbox=TRUE)
    r<-get(req);if(is.null(r))return(list())
    z<-httr2::resp_body_json(r,simplifyVector=FALSE)
    if(is.null(z$data)&&is.null(z$total))stop("Unexpected Lens response")
    if(length(z$total)&&as.numeric(z$total)>length(z$data))stop("Truncated Lens batch")
    Filter(Negate(is.null),lapply(z$data,function(h){
      if(identical(tolower(scalar(h$publication_type)),"dataset"))return(NULL)
      d<-doi(h$ids$doi)
      if(!nzchar(d)){
        e<-Filter(function(x)identical(tolower(scalar(x$type)),"doi"),h$external_ids)
        if(length(e)==1L)d<-doi(e[[1L]]$value)
      }
      list(doi=d,title=scalar(h$title),abstract=clean(h$abstract))
    }))
  }
  if(nzchar(lens_token))run_batches("lens",lens,40L) else provider_status$lens <- "missing_credentials"
  uid<-Sys.getenv("EBSCO_EHOST_UID");pwd<-Sys.getenv("EBSCO_EHOST_PWD")
  ebsco_xml <- function(endpoint,params){
    r<-get(httr2::request(paste0("https://eit.ebscohost.com/Services/SearchService.asmx/",endpoint)) |>
      httr2::req_url_query(!!!params))
    if(is.null(r))stop("EBSCO endpoint unavailable")
    doc<-xml2::read_xml(httr2::resp_body_string(r),options="NONET")
    if(length(xml2::xml_find_all(doc,"//*[local-name()='Error' or local-name()='error']")))stop("EBSCO API error")
    doc
  }
  auth <- list(prof=uid,pwd=pwd,authType="profile")
  if(nzchar(uid)&&nzchar(pwd)&&length(pending())){
    info<-tryCatch(ebsco_xml("Info",auth),error=function(e)NULL)
    cfg<-jsonlite::fromJSON(system.file("w00","config/workflow00_ebsco_sources.json",package="evidencelandscaper"),simplifyVector=FALSE)
    allowed<-vapply(cfg$sources,function(z)z$db_code,character(1))
    dbs<-if(is.null(info))list() else xml2::xml_find_all(info,"//*[local-name()='db']")
    # Only explicit DOI fields in live entitlement metadata; do not guess field syntax.
    eligible<-character()
    for(db in dbs){
      code<-xml2::xml_attr(db,"shortName")
      tags<-xml2::xml_attr(xml2::xml_find_all(db,".//*[local-name()='dbTag']"),"name")
      if(!is.na(code)&&code%in%allowed&&"DO"%in%tags)eligible<-c(eligible,code)
    }
    if(length(eligible)){
      for(code in unique(eligible)){
        fetch<-function(b){
          q<-paste(paste0('DO "',b,'"'),collapse=" OR ")
          doc<-ebsco_xml("Search",c(auth,list(db=code,query=q,format="detailed",startrec="1",numrec="100")))
          hits<-suppressWarnings(as.integer(xml2::xml_text(xml2::xml_find_all(doc,"//*[local-name()='Hits']"))))
          if(!length(hits)||anyNA(hits)||max(hits)>100L)stop("Unverified or truncated EBSCO result")
          node<-function(r,x)scalar(xml2::xml_text(xml2::xml_find_all(r,x)))
          lapply(xml2::xml_find_all(doc,"//*[local-name()='rec']"),function(r){
            title<-node(r,".//*[local-name()='atl']")
            if(!nzchar(title))title<-node(r,".//*[local-name()='btl']")
            list(doi=doi(node(r,".//*[local-name()='ui' and translate(@type,'DOI','doi')='doi']")),
              title=title,abstract=clean(node(r,".//*[local-name()='ab']")))
          })
        }
        run_batches(paste0("ebsco_",code),fetch,25L,1L)
      }
      provider_status$ebsco <- "see_database_status"
    } else provider_status$ebsco <- if(is.null(info))"info_request_failed" else "no_explicit_DO_field"
  } else provider_status$ebsco <- "missing_credentials_or_no_targets"
  oa <- function(b){
    r<-get(httr2::request("https://api.openaire.eu/search/publications") |>
      httr2::req_url_query(doi=paste(b,collapse=","),size=100L,format="xml"))
    if(is.null(r))return(list())
    doc<-xml2::read_xml(httr2::resp_body_string(r),options="NONET")
    records<-xml2::xml_find_all(doc,"//*[local-name()='result' and *[local-name()='title']]")
    lapply(records,function(h){
      vals<-function(x)unique(trimws(xml2::xml_text(xml2::xml_find_all(h,x))))
      d<-vals("./*[local-name()='pid' and @classid='doi']")
      t<-vals("./*[local-name()='title' and @classid='main title']")
      if(!length(t))t<-vals("./*[local-name()='title']")
      a<-vals("./*[local-name()='description']")
      list(doi=if(length(d)==1L)doi(d) else "",title=if(length(t)==1L)t else "",
        abstract=if(length(a)==1L)clean(a) else "")
    })
  }
  run_batches("openaire",oa,15L)
  # One targeted repeat of Crossref DOI errors from the previous publisher audit.
  prior<-file.path(dirname(scored),"W02_main","publishers_3ce80826d1e9","audit.jsonl")
  if(file.exists(prior)){
    aa<-lapply(readLines(prior,warn=FALSE),jsonlite::fromJSON,simplifyVector=FALSE)
    err_ids<-vapply(Filter(function(a)identical(a$attempts$crossref,"request_error"),aa),function(a)scalar(a$record_id),character(1))
    jj<-pending();jj<-jj[ids[ix[jj]]%in%err_ids]
    for(j in jj){
      d<-effective[ix[j]]
      z<-cached(list("crossref_retry",d),function(){
        r<-get(httr2::request(paste0("https://api.crossref.org/works/",utils::URLencode(d,reserved=TRUE))))
        if(is.null(r))return(list())
        h<-httr2::resp_body_json(r,simplifyVector=FALSE)$message
        list(list(doi=doi(h$DOI),title=scalar(h$title),abstract=clean(h$abstract)))
      })
      if(isTRUE(z$error))audit[[j]]$attempts$crossref_retry<-"request_error" else apply_hits(j,z$hits,"crossref_retry")
    }
    provider_status$crossref_retry <- paste(length(jj),"records attempted")
  } else provider_status$crossref_retry <- "prior_audit_unavailable"
  for(j in seq_along(ix)){
    a<-audit[[j]];i<-ix[j]
    if(!is.null(a$match)){
      rows[[i]]$canonical$abstract<-a$match$abstract
      rows[[i]]$final_lookup_provenance<-list(provider=a$provider,returned_doi=a$doi,returned_title=a$match$title)
    }
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
    request_errors=sum(attempts=="request_error"),provider_status=provider_status,
    input_sha256=input_sha,output_sha256=sha(canonical),audit_sha256=sha(ap)),
    file.path(out,"summary.json"),auto_unbox=TRUE,pretty=TRUE)
  invisible(out)
}
