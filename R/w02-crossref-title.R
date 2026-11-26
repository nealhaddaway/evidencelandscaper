#' Recover missing abstracts by Crossref bibliographic title matching
#' @param scored Final W01 scoring directory.
#' @param input_dir Latest validated W02 corpus.
#' @return Separate checksummed corpus directory.
#' @export
recover_w02_crossref_titles <- function(scored,input_dir) {
  input_dir <- normalizePath(input_dir,mustWork=TRUE)
  scored <- normalizePath(scored,mustWork=TRUE)
  sha <- function(p) digest::digest(file=p,algo="sha256",serialize=FALSE)
  input <- file.path(input_dir,"canonical.jsonl")
  meta <- jsonlite::fromJSON(file.path(input_dir,"summary.json"))
  input_sha <- sha(input)
  stopifnot(identical(meta$output_sha256,input_sha))
  if(!requireNamespace("xml2",quietly=TRUE)) stop("xml2 required.")
  scalar <- function(x) {
    z <- as.character(unlist(x,use.names=FALSE))
    z <- z[!is.na(z)&nzchar(trimws(z))]
    if(length(z)) trimws(z[[1L]]) else ""
  }
  norm <- function(x) {
    z <- iconv(scalar(x),to="ASCII//TRANSLIT",sub="")
    if(is.na(z)) return("")
    trimws(gsub("[^a-z0-9]+"," ",tolower(z)))
  }
  doi <- function(x) {
    z <- tolower(sub("^https?://(dx\\.)?doi\\.org/","",scalar(x),ignore.case=TRUE))
    z <- sub("^doi:\\s*","",z,ignore.case=TRUE)
    if(grepl("^10\\.[0-9]{4,9}/[^[:space:]]+$",z)) z else ""
  }
  year <- function(x) {
    z <- scalar(x)
    if(grepl("^[12][0-9]{3}",z)) substr(z,1L,4L) else ""
  }
  author_name <- function(x) {
    if(is.null(x)||!length(x)) return(list(family="",full=""))
    if(is.list(x)&&is.null(names(x))) return(author_name(x[[1L]]))
    if(is.list(x)) {
      f <- ""
      for(k in c("family","last_name","family_name","surname","lastName","familyName")) {
        f <- norm(x[[k]])
        if(nzchar(f)) break
      }
      full <- ""
      for(k in c("display_name","full_name","name","displayName","fullName","author_name")) {
        full <- scalar(x[[k]])
        if(nzchar(full)) break
      }
      if(!nzchar(full)&&!nzchar(f)&&!is.null(x$author)) return(author_name(x$author))
    } else {
      f <- ""; full <- scalar(x)
    }
    full <- strsplit(full,";",fixed=TRUE)[[1L]]
    full <- if(length(full))full[[1L]] else ""
    if(!nzchar(f)&&grepl(",",full,fixed=TRUE)) f <- norm(sub(",.*$","",full))
    if(!nzchar(f)&&grepl("^[[:alpha:]'-]+[[:space:]]+[[:alpha:]][.]?$",full))
      f <- norm(sub("[[:space:]].*$","",full))
    list(family=f,full=norm(full))
  }
  clean <- function(x) {
    z <- scalar(x)
    if(grepl("<[^>]+>",z)) {
      doc <- tryCatch(xml2::read_html(paste0("<div>",z,"</div>"),
        options=c("RECOVER","NONET","NOERROR","NOWARNING")),error=function(e)NULL)
      if(is.null(doc)) return("")
      z <- xml2::xml_text(xml2::xml_find_first(doc,"//div"))
    }
    z <- trimws(gsub("[[:space:]]+"," ",z))
    z <- sub("^(Abstract|Summary)[[:space:]:.]+","",z,ignore.case=TRUE)
    if(nchar(z)<40L||grepl("^(no abstract|abstract unavailable|not available)",z,ignore.case=TRUE)) "" else z
  }
  rows <- lapply(readLines(input,warn=FALSE,encoding="UTF-8"),jsonlite::fromJSON,simplifyVector=FALSE)
  ids <- vapply(rows,function(r)scalar(r$identity$record_id),character(1))
  titles <- vapply(rows,function(r)norm(r$canonical$title),character(1))
  ds <- vapply(rows,function(r)doi(r$canonical$doi),character(1))
  has <- function(r) nzchar(scalar(r$canonical$abstract))
  before <- vapply(rows,has,logical(1))
  stopifnot(!anyDuplicated(ids),all(nzchar(ids)),length(rows)==meta$publication_clusters)
  groups <- split(which(nzchar(titles)),titles[nzchar(titles)])
  doi_groups <- split(which(nzchar(ds)),ds[nzchar(ds)])
  ix <- which(!before & !nzchar(ds))
  out <- file.path(dirname(scored),"W02_main",paste0("crossref_titles_",substr(input_sha,1L,12L)))
  dir.create(out,recursive=TRUE,showWarnings=FALSE)
  cache <- file.path(out,"checkpoints");dir.create(cache,showWarnings=FALSE)
  audit <- vector("list",length(ix))
  query <- function(title,author) {
    req <- httr2::request("https://api.crossref.org/works") |>
      httr2::req_url_query(query.bibliographic=title,rows=10L) |>
      httr2::req_user_agent("evidencelandscaper-W02/1.0") |>
      httr2::req_timeout(30) |> httr2::req_retry(max_tries=2,max_seconds=60) |>
      httr2::req_error(is_error=function(r)FALSE)
    if(nzchar(author)) req <- httr2::req_url_query(req,query.author=author)
    resp <- httr2::req_perform(req)
    if(httr2::resp_status(resp)!=200L) stop("Crossref HTTP ",httr2::resp_status(resp))
    httr2::resp_body_json(resp,simplifyVector=FALSE)$message$items
  }
  candidate <- function(h) {
    ys <- unique(vapply(c("published","published-print","published-online","issued"),function(k){
      z <- h[[k]][["date-parts"]]
      if(length(z)) year(z[[1L]]) else ""
    },character(1)))
    a <- if(length(h$author))h$author[[1L]] else list()
    list(doi=doi(h$DOI),title=scalar(h$title),years=ys[nzchar(ys)],
      family=norm(a$family),full=norm(paste(scalar(a$given),scalar(a$family))),
      full_reverse=norm(paste(scalar(a$family),scalar(a$given))),
      abstract=clean(h$abstract))
  }
  message("[W02 Crossref titles] ",length(ix)," DOI-less missing-abstract records.")
  for(j in seq_along(ix)) {
    i <- ix[[j]]; r <- rows[[i]]; t <- titles[[i]]
    p <- file.path(cache,paste0(digest::digest(ids[[i]],algo="sha256",serialize=FALSE),".rds"))
    if(file.exists(p)) {
      a <- readRDS(p)
      stopifnot(identical(a$input_sha256,input_sha),identical(a$record_id,ids[[i]]))
    } else {
      a <- list(record_id=ids[[i]],input_sha256=input_sha,outcome="insufficient_metadata",match=NULL)
      evidence <- lapply(r$manifestations,function(m) {
        z <- m$manifestation_metadata
        c(list(year=year(z$year)),author_name(z$authors))
      })
      evidence <- Filter(function(z)nzchar(z$year)&&(nzchar(z$family)||nzchar(z$full)),evidence)
      if(nchar(t)>=20L && length(groups[[t]])==1L && length(evidence)) {
        author <- evidence[[1L]]$family
        if(!nzchar(author)) author <- evidence[[1L]]$full
        res <- tryCatch(list(items=query(scalar(r$canonical$title),author)),error=function(e)list(error=conditionMessage(e)))
        if(!is.null(res$error)) {
          a$outcome <- "request_error"
          a$error <- res$error
        } else {
          hits <- lapply(res$items,candidate)
          ok <- vapply(hits,function(h) {
            if(!nzchar(h$doi)||!identical(norm(h$title),t)||length(doi_groups[[h$doi]]))return(FALSE)
            any(vapply(evidence,function(z) {
              z$year %in% h$years &&
                ((nzchar(z$family)&&identical(z$family,h$family))||
                 (nzchar(z$full)&&z$full %in% c(h$full,h$full_reverse)))
            },logical(1)))
          },logical(1))
          matched <- hits[ok]
          a$candidate_count <- length(hits)
          a$identity_matches <- length(matched)
          a$outcome <- "no_verified_match"
          if(length(matched)==1L) {
            a$match <- matched[[1L]]
            a$outcome <- if(nzchar(a$match$abstract))"abstract_recovered" else "doi_discovered_no_abstract"
          } else if(length(matched)>1L) a$outcome <- "ambiguous_match"
        }
        Sys.sleep(0.25)
      }
      tmp <- tempfile(tmpdir=cache);saveRDS(a,tmp)
      if(!file.rename(tmp,p))stop("Checkpoint write failed.")
    }
    # A discovered DOI is kept as a separate verified candidate, not assigned
    # to canonical identity. Prevent duplicate assignment across new matches.
    if(!is.null(a$match)&&nzchar(a$match$abstract)) {
      rows[[i]]$canonical$abstract <- a$match$abstract
      rows[[i]]$crossref_title_provenance <- list(returned_doi=a$match$doi,
        returned_title=a$match$title,years=a$match$years,first_author=a$match$family,
        match_method="unique_exact_title_year_first_author")
    }
    audit[[j]] <- a
    if(j==1L||j%%25L==0L||j==length(ix)) message("[W02 Crossref titles] ",j,"/",length(ix),
      "; provisional abstracts: ",sum(vapply(audit[seq_len(j)],function(z)identical(z$outcome,"abstract_recovered"),logical(1))))
  }
  found <- vapply(audit,function(a)if(is.null(a$match))"" else a$match$doi,character(1))
  dup <- nzchar(found)&(duplicated(found)|duplicated(found,fromLast=TRUE))
  for(j in which(dup)) {
    i <- ix[[j]]
    rows[[i]]$canonical$abstract <- NULL
    rows[[i]]$crossref_title_provenance <- NULL
    audit[[j]]$outcome <- "duplicate_discovered_doi_quarantined"
  }
  after <- vapply(rows,has,logical(1))
  stopifnot(all(after[before]),identical(ids,vapply(rows,function(r)scalar(r$identity$record_id),character(1))),
    identical(ds,vapply(rows,function(r)doi(r$canonical$doi),character(1))))
  write_jsonl <- function(x,p) {
    con <- file(p,"wt",encoding="UTF-8");on.exit(close(con))
    for(z in x)writeLines(jsonlite::toJSON(z,auto_unbox=TRUE,null="null",na="null",digits=NA),con,useBytes=TRUE)
  }
  canonical <- file.path(out,"canonical.jsonl");ap <- file.path(out,"audit.jsonl")
  write_jsonl(rows,canonical);write_jsonl(audit,ap)
  utils::write.csv(data.frame(record_id=ids[!after],doi=ds[!after],title=vapply(rows[!after],function(r)scalar(r$canonical$title),character(1))),
    file.path(out,"remaining_missing_abstracts.csv"),row.names=FALSE)
  verified <- nzchar(found)&!dup
  utils::write.csv(data.frame(record_id=ids[ix][verified],candidate_doi=found[verified],
    abstract_recovered=after[ix][verified]),file.path(out,"discovered_dois.csv"),row.names=FALSE)
  jsonlite::write_json(list(status="complete",publication_clusters=length(rows),
    missing_before=sum(!before),targeted_records=length(ix),recovered=sum(after)-sum(before),
    missing_after=sum(!after),abstracts_after=sum(after),
    remaining_with_canonical_doi=sum(!after&nzchar(ds)),verified_doi_candidates=sum(verified),
    doi_candidates_still_missing_abstract=sum(verified&!after[ix]),
    outcomes=as.list(table(vapply(audit,function(a)a$outcome,character(1)))),
    input_sha256=input_sha,output_sha256=sha(canonical),audit_sha256=sha(ap)),
    file.path(out,"summary.json"),auto_unbox=TRUE,pretty=TRUE)
  invisible(out)
}
