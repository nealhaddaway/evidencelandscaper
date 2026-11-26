#' Recover remaining W02 abstracts from Crossref and publisher metadata
#' @param scored Final W01 scoring directory.
#' @param input_dir Latest validated W02 corpus.
#' @param springer_only Run only Springer metadata recovery on unresolved Springer DOI records.
#' @return New checksummed W02 corpus directory.
#' @export
recover_w02_publishers <- function(scored,input_dir,springer_only=FALSE) {
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
  crossref <- function(d) {
    req <- httr2::request(paste0("https://api.crossref.org/works/",utils::URLencode(d,reserved=TRUE))) |>
      httr2::req_user_agent("evidencelandscaper-W02/1.0")
    r <- get(req)
    if(is.null(r)) return(NULL)
    z <- httr2::resp_body_json(r,simplifyVector=FALSE)$message
    list(doi=doi(z$DOI),title=scalar(z$title),abstract=clean(z$abstract))
  }
  sciencedirect <- function(d) {
    r <- get(httr2::request(paste0("https://api.elsevier.com/content/article/doi/",utils::URLencode(d,reserved=TRUE))) |>
      httr2::req_url_query(view="META_ABS") |>
      httr2::req_headers("X-ELS-APIKey"=Sys.getenv("SCOPUS_API_TOKEN"),
        "X-ELS-Insttoken"=Sys.getenv("SCOPUS_INSTTOKEN"),Accept="text/xml"))
    if(is.null(r)) return(NULL)
    doc <- xml2::read_xml(httr2::resp_body_string(r),options="NONET")
    node <- function(q) {
      n <- xml2::xml_find_first(doc,q)
      if(inherits(n,"xml_missing")) "" else xml2::xml_text(n)
    }
    list(doi=doi(node("//*[local-name()='coredata']/*[local-name()='doi']")),
      title=node("//*[local-name()='coredata']/*[local-name()='title']"),
      abstract=clean(node("//*[local-name()='abstract'] | //*[local-name()='coredata']/*[local-name()='description']")))
  }
  springer_key <- Sys.getenv("SpringerNature_API_KEY")
  if(!nzchar(springer_key)) springer_key <- Sys.getenv("SPRINGER_API_KEY")
  if(springer_only&&!nzchar(springer_key)) stop("SpringerNature_API_KEY (or SPRINGER_API_KEY) is required.")
  springer <- function(d) {
    r <- get(httr2::request("https://api.springernature.com/meta/v2/json") |>
      httr2::req_url_query(q=paste0("doi:",d),api_key=springer_key,p=10))
    if(is.null(r)) return(NULL)
    z <- httr2::resp_body_json(r,simplifyVector=FALSE)$records
    z <- Filter(function(h)identical(doi(h$doi),d),z)
    if(length(z)!=1L) return(NULL)
    list(doi=doi(z[[1L]]$doi),title=scalar(z[[1L]]$title),abstract=clean(z[[1L]]$abstract))
  }
  # Public bibliographic metadata only; no access-control bypass.
  landing <- function(d) {
    r <- get(httr2::request(paste0("https://doi.org/",utils::URLencode(d,reserved=FALSE))) |>
      httr2::req_headers(Accept="text/html"))
    if(is.null(r)) return(NULL)
    doc <- xml2::read_html(httr2::resp_body_string(r),options=c("RECOVER","NONET","NOERROR","NOWARNING"))
    tag <- function(n) {
      z <- xml2::xml_find_all(doc,paste0("//meta[translate(@name,'ABCDEFGHIJKLMNOPQRSTUVWXYZ','abcdefghijklmnopqrstuvwxyz')='",n,"']"))
      v <- unique(trimws(xml2::xml_attr(z,"content")))
      v <- v[!is.na(v)&nzchar(v)]
      if(length(v)==1L) v[[1L]] else ""
    }
    list(doi=doi(tag("citation_doi")),title=tag("citation_title"),
      abstract=clean(tag("citation_abstract")))
  }
  rows <- lapply(readLines(input,warn=FALSE,encoding="UTF-8"),jsonlite::fromJSON,simplifyVector=FALSE)
  has <- function(r) nzchar(scalar(r$canonical$abstract))
  before <- vapply(rows,has,logical(1))
  ids <- vapply(rows,function(r)scalar(r$identity$record_id),character(1))
  ds <- vapply(rows,function(r)doi(r$canonical$doi),character(1))
  ts <- vapply(rows,function(r)norm(r$canonical$title),character(1))
  stopifnot(!anyDuplicated(ids),all(nzchar(ids)))
  groups <- split(which(nzchar(ds)),ds[nzchar(ds)])
  ix <- which(!before)
  if(springer_only) ix <- ix[grepl("^10\\.(1007|1038|1186)/",ds[ix]) & nzchar(ts[ix])]
  out <- file.path(dirname(scored),"W02_main",paste0(if(springer_only)"springer_" else "publishers_",substr(input_sha,1L,12L)))
  dir.create(out,recursive=TRUE,showWarnings=FALSE)
  cache <- file.path(out,"checkpoints")
  dir.create(cache,showWarnings=FALSE)
  audit <- vector("list",length(ix))
  sd_enabled <- !springer_only&&nzchar(Sys.getenv("SCOPUS_API_TOKEN"))&&nzchar(Sys.getenv("SCOPUS_INSTTOKEN"))
  message("[W02] Missing abstracts: ",length(ix),"; with DOI: ",sum(nzchar(ds[ix])),
    "; ScienceDirect enabled: ",sd_enabled,"; Springer key available: ",nzchar(springer_key))
  if(springer_only&&length(ix)) {
    message("[W02] Springer-only target: ",length(ix)," publications.")
    # Check authentication and transport before creating record checkpoints.
    tryCatch(springer(ds[ix[[1L]]]),error=function(e)
      stop("Springer metadata preflight failed. Check key entitlement and network; no record checkpoints written by preflight.",call.=FALSE))
    message("[W02] Springer metadata preflight passed.")
  }
  for(j in seq_along(ix)) {
    i <- ix[[j]]; d <- ds[[i]]; t <- ts[[i]]
    p <- file.path(cache,paste0(digest::digest(ids[[i]],algo="sha256",serialize=FALSE),".rds"))
    if(file.exists(p)) {
      a <- readRDS(p)
      stopifnot(identical(a$input_sha256,input_sha),identical(a$record_id,ids[[i]]))
    } else {
      a <- list(record_id=ids[[i]],input_sha256=input_sha,doi=d,provider=NULL,match=NULL,attempts=list())
      attempt <- function(provider,fun) {
        z <- tryCatch(list(hit=fun(d)),error=function(e)list(error=TRUE))
        h <- z$hit
        ok <- !is.null(h)&&identical(h$doi,d)&&nzchar(h$abstract)&&nzchar(t)&&nzchar(norm(h$title))
        if(ok) {
          sim <- 1-stringdist::stringdist(t,norm(h$title),method="jw",p=0.1)
          ok <- !is.na(sim)&&sim>=0.90
          g <- groups[[d]]
          if(length(g)>1L) ok <- ok&&identical(t,norm(h$title))&&sum(ts[g]==t)==1L
        }
        a$attempts[[provider]] <<- if(isTRUE(z$error)) "request_error" else if(is.null(h)) "no_result" else if(ok) "accepted" else "missing_abstract_or_identity_guard"
        if(ok) {a$provider <<- provider; a$match <<- h}
        Sys.sleep(0.25)
      }
      if(nzchar(d)&&nzchar(t)) {
        if(!springer_only) attempt("crossref",crossref)
        if(is.null(a$match)&&sd_enabled&&grepl("^10\\.(1016|1006|1053|1067|1011)/",d)) attempt("sciencedirect",sciencedirect)
        if(is.null(a$match)&&nzchar(springer_key)&&grepl("^10\\.(1007|1038|1186)/",d)) attempt("springer",springer)
        if(!springer_only&&is.null(a$match)) attempt("publisher_citation_metadata",landing)
      }
      tmp <- tempfile(tmpdir=cache); saveRDS(a,tmp)
      if(!file.rename(tmp,p)) stop("Cannot save checkpoint.")
    }
    if(!is.null(a$match)) {
      rows[[i]]$canonical$abstract <- a$match$abstract
      rows[[i]]$publisher_abstract_provenance <- list(provider=a$provider,returned_doi=a$match$doi,returned_title=a$match$title)
    }
    audit[[j]] <- a
    if(j==1L||j%%25L==0L||j==length(ix)) message("[W02 publishers] ",j,"/",length(ix),
      "; recovered ",sum(vapply(audit[seq_len(j)],function(a)!is.null(a$match),logical(1))))
  }
  after <- vapply(rows,has,logical(1))
  stopifnot(all(after[before]),identical(ids,vapply(rows,function(r)scalar(r$identity$record_id),character(1))))
  write_jsonl <- function(x,p) {
    con <- file(p,"wt",encoding="UTF-8"); on.exit(close(con))
    for(z in x) writeLines(jsonlite::toJSON(z,auto_unbox=TRUE,null="null",na="null",digits=NA),con,useBytes=TRUE)
  }
  canonical <- file.path(out,"canonical.jsonl"); ap <- file.path(out,"audit.jsonl")
  write_jsonl(rows,canonical); write_jsonl(audit,ap)
  utils::write.csv(data.frame(record_id=ids[!after],doi=ds[!after],title=vapply(rows[!after],function(r)scalar(r$canonical$title),character(1))),
    file.path(out,"remaining_missing_abstracts.csv"),row.names=FALSE)
  outcomes <- vapply(audit,function(a)if(is.null(a$provider))"unresolved" else a$provider,character(1))
  attempts <- unlist(lapply(audit,function(a)unlist(a$attempts)),use.names=FALSE)
  jsonlite::write_json(list(status="complete",publication_clusters=length(rows),
    missing_before=sum(!before),targeted_records=length(ix),springer_only=springer_only,recovered=sum(after)-sum(before),missing_after=sum(!after),
    abstracts_after=sum(after),remaining_with_doi=sum(!after&nzchar(ds)),outcomes=as.list(table(outcomes)),
    request_errors=sum(attempts=="request_error"),springer_enabled=nzchar(springer_key),
    sciencedirect_enabled=sd_enabled,input_sha256=input_sha,output_sha256=sha(canonical),audit_sha256=sha(ap)),
    file.path(out,"summary.json"),auto_unbox=TRUE,pretty=TRUE)
  invisible(out)
}
