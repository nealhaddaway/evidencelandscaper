#' Targeted supplementary W02 abstract recovery from OpenAlex and Europe PMC
#' @param scored Finalised W01 full_scoring directory.
#' @return Separate supplementary corpus and audit directory.
#' @export
recover_w02_abstracts <- function(scored) {
  base <- finalize_w02(scored)
  input <- file.path(base,"canonical.jsonl")
  sha <- function(p) digest::digest(file=p,algo="sha256",serialize=FALSE)
  input_sha <- sha(input)
  key <- Sys.getenv("OPENALEX_API_KEY")
  if(!nzchar(key)) key <- Sys.getenv("OPENALEX_API_TOKEN")
  if(!nzchar(key)) stop("OpenAlex API key required.")
  if(!requireNamespace("stringdist",quietly=TRUE)) stop("stringdist required.")
  out <- file.path(dirname(base),"supplementary")
  dir.create(out,recursive=TRUE,showWarnings=FALSE)
  cache <- file.path(out,"checkpoints")
  dir.create(cache,showWarnings=FALSE)
  spec <- file.path(out,"input_manifest.json")
  if(file.exists(spec)) {
    if(!identical(jsonlite::fromJSON(spec)$input_sha256,input_sha))
      stop("Supplementary input changed.")
  } else jsonlite::write_json(list(input_sha256=input_sha,
    schema="w02-supplementary-v1"),spec,auto_unbox=TRUE,pretty=TRUE)
  scalar <- function(x) {
    if(is.null(x)||!length(x)) return("")
    v <- as.character(unlist(x,use.names=FALSE))
    v <- v[!is.na(v)&nzchar(trimws(v))]
    if(length(v)) trimws(v[[1L]]) else ""
  }
  doi <- function(x) tolower(sub("^doi:\\s*","",sub("^https?://(dx\\.)?doi\\.org/","",scalar(x),ignore.case=TRUE),ignore.case=TRUE))
  norm <- function(x) trimws(gsub("[^[:alnum:]]+"," ",tolower(scalar(x))))
  clean <- function(x) trimws(gsub("[[:space:]]+"," ",gsub("<[^>]+>"," ",scalar(x))))
  reconstruct <- function(x) {
    if(!length(x)||is.null(names(x))) return("")
    p <- suppressWarnings(as.integer(unlist(x,use.names=FALSE)))
    if(!length(p)||anyNA(p)||any(p<0L)||max(p)>100000L||
       anyDuplicated(p)||!identical(sort(p),seq.int(0L,length(p)-1L))) return("")
    words <- rep(names(x),lengths(x))
    paste(words[order(p)],collapse=" ")
  }
  get <- function(req) {
    resp <- req |> httr2::req_timeout(30) |>
      httr2::req_retry(max_tries=2,max_seconds=60) |>
      httr2::req_error(is_error=function(r) FALSE) |> httr2::req_perform()
    st <- httr2::resp_status(resp)
    if(st==404L) return(NULL)
    if(st!=200L) stop("HTTP ",st)
    httr2::resp_body_json(resp,simplifyVector=FALSE)
  }
  oa <- function(d,t) {
    req <- httr2::request("https://api.openalex.org/works") |>
      httr2::req_url_query(api_key=key,select="id,doi,title,publication_year,abstract_inverted_index")
    if(nzchar(d)) {
      req <- req |> httr2::req_url_query(filter=paste0("doi:",d),per_page=10)
    } else {
      req <- req |> httr2::req_url_query(search=t,per_page=10)
    }
    z <- get(req)
    lapply(z$results,function(h)list(id=scalar(h$id),doi=doi(h$doi),
      title=scalar(h$title),year=scalar(h$publication_year),
      abstract=clean(reconstruct(h$abstract_inverted_index))))
  }
  ep <- function(t) {
    t <- gsub('"'," ",t,fixed=TRUE)
    z <- get(httr2::request("https://www.ebi.ac.uk/europepmc/webservices/rest/search") |>
      httr2::req_url_query(query=paste0('TITLE:"',t,'"'),format="json",resultType="core",pageSize=25))
    lapply(z$resultList$result,function(h)list(id=paste(scalar(h$source),scalar(h$id),sep=":"),
      doi=doi(h$doi),title=scalar(h$title),year=scalar(h$pubYear),abstract=clean(h$abstractText)))
  }
  choose <- function(hits,d,t,years,by_doi=FALSE) {
    if(!length(hits)) return(NULL)
    ok <- vapply(hits,function(h) {
      if(!nzchar(h$abstract)||!nzchar(norm(t))||!nzchar(norm(h$title))) return(FALSE)
      if(nzchar(d)&&nzchar(h$doi)&&!identical(d,h$doi)) return(FALSE)
      if(by_doi) return(identical(d,h$doi)&&
        1-stringdist::stringdist(norm(t),norm(h$title),method="jw",p=0.1)>=0.90)
      # Title lookup without DOI requires exact normalised title and corroborating year.
      if(nchar(norm(t))<20L||!identical(norm(t),norm(h$title))) return(FALSE)
      if(nzchar(d)&&identical(d,h$doi)) return(TRUE)
      length(years)==1L && nzchar(h$year) && h$year %in% years
    },logical(1))
    z <- hits[ok]
    if(length(z)==1L) z[[1L]] else NULL
  }
  rows <- lapply(readLines(input,warn=FALSE,encoding="UTF-8"),jsonlite::fromJSON,simplifyVector=FALSE)
  has <- function(r) nzchar(scalar(r$canonical$abstract))
  missing <- which(!vapply(rows,has,logical(1)))
  audit <- vector("list",length(missing))
  for(j in seq_along(missing)) {
    i <- missing[[j]]
    r <- rows[[i]]
    rid <- scalar(r$identity$record_id)
    d <- doi(r$canonical$doi)
    t <- scalar(r$canonical$title)
    years <- unique(vapply(r$manifestations,function(m)scalar(m$manifestation_metadata$year),character(1)))
    years <- years[grepl("^[0-9]{4}$",years)]
    p <- file.path(cache,paste0(digest::digest(rid,algo="sha256",serialize=FALSE),".rds"))
    if(file.exists(p)) {
      a <- readRDS(p)
      if(!identical(a$record_id,rid)||!identical(a$input_sha256,input_sha)) stop("Invalid checkpoint.")
    } else {
      a <- list(record_id=rid,input_sha256=input_sha,provider=NULL,match=NULL,attempts=list())
      attempt <- function(provider,fun,by_doi=FALSE) {
        result <- tryCatch(list(hits=fun()),error=function(e)list(error="request_failed"))
        h <- choose(result$hits,d,t,years,by_doi)
        a$attempts[[provider]] <<- list(candidates=length(result$hits),error=result$error,
          accepted=!is.null(h))
        if(!is.null(h)) { a$provider <<- provider; a$match <<- h }
      }
      if(nzchar(d)) attempt("openalex_doi",function()oa(d,t),TRUE)
      if(is.null(a$match)&&nzchar(t)) attempt("openalex_title",function()oa("",t))
      if(is.null(a$match)&&nzchar(t)) attempt("europe_pmc_title",function()ep(t))
      tmp <- tempfile(tmpdir=cache)
      saveRDS(a,tmp)
      if(!file.rename(tmp,p)) stop("Cannot save supplementary checkpoint.")
    }
    if(!is.null(a$match)) {
      rows[[i]]$canonical$abstract <- a$match$abstract
      rows[[i]]$supplementary_abstract_provenance <- list(provider=a$provider,
        provider_id=a$match$id,returned_doi=a$match$doi,returned_title=a$match$title,
        returned_year=a$match$year)
    }
    audit[[j]] <- a
    if(j==1L||j%%25L==0L||j==length(missing))
      message("[W02 supplementary] ",j,"/",length(missing),
        "; recovered ",sum(vapply(audit[seq_len(j)],function(a)!is.null(a$match),logical(1))))
  }
  write_jsonl <- function(x,p) {
    con <- file(p,"wt",encoding="UTF-8"); on.exit(close(con))
    for(z in x) writeLines(jsonlite::toJSON(z,auto_unbox=TRUE,null="null",na="null"),con,useBytes=TRUE)
  }
  canonical <- file.path(out,"canonical.jsonl")
  audit_path <- file.path(out,"audit.jsonl")
  write_jsonl(rows,canonical)
  write_jsonl(audit,audit_path)
  remaining <- rows[!vapply(rows,has,logical(1))]
  utils::write.csv(data.frame(record_id=vapply(remaining,function(r)scalar(r$identity$record_id),character(1)),
    doi=vapply(remaining,function(r)scalar(r$canonical$doi),character(1)),
    title=vapply(remaining,function(r)scalar(r$canonical$title),character(1))),
    file.path(out,"remaining_missing_abstracts.csv"),row.names=FALSE)
  providers <- vapply(audit,function(a)if(is.null(a$provider))"unresolved" else a$provider,character(1))
  jsonlite::write_json(list(status="complete",publication_clusters=length(rows),
    missing_before=length(missing),recovered=length(missing)-length(remaining),
    missing_after=length(remaining),abstracts_after=length(rows)-length(remaining),
    outcomes=as.list(table(providers)),input_sha256=input_sha,
    output_sha256=sha(canonical),audit_sha256=sha(audit_path)),
    file.path(out,"summary.json"),auto_unbox=TRUE,pretty=TRUE)
  invisible(out)
}
