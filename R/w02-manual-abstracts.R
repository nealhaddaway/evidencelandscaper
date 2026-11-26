#' Integrate manually exported abstracts into a W02 corpus
#'
#' Only missing abstracts are filled. Exact normalised DOI and title agreement
#' are required. No records are added or deleted. The input corpus and export
#' remain unchanged, and a separate checksummed result is created.
#' @param scored Finalised W01 full_scoring directory.
#' @param csv Path to a manually downloaded UTF-8 CSV or RIS export.
#' @param provider Source database, "scopus" or "wos".
#' @param input_dir Previous W02 corpus directory; defaults to supplementary.
#' @return Directory containing updated canonical corpus, audit and summary.
#' @export
import_w02_manual_abstracts <- function(scored,csv,provider="scopus",input_dir=NULL) {
  provider <- match.arg(provider,c("scopus","wos"))
  scored <- normalizePath(scored,mustWork=TRUE)
  csv <- normalizePath(csv,mustWork=TRUE)
  if(is.null(input_dir)) input_dir <- file.path(dirname(scored),"W02_main","supplementary")
  input_dir <- normalizePath(input_dir,mustWork=TRUE)
  input <- file.path(input_dir,"canonical.jsonl")
  meta_path <- file.path(input_dir,"summary.json")
  sha <- function(p) digest::digest(file=p,algo="sha256",serialize=FALSE)
  if(!all(file.exists(c(input,meta_path)))) stop("Completed supplementary W02 corpus required.")
  meta <- jsonlite::fromJSON(meta_path)
  input_sha <- sha(input)
  if(!identical(meta$output_sha256,input_sha)) stop("Input corpus checksum mismatch.")
  csv_sha <- sha(csv)
  if(!requireNamespace("stringdist",quietly=TRUE)) stop("stringdist required.")
  scalar <- function(x) {
    if(is.null(x)||!length(x)) return("")
    y <- as.character(unlist(x,use.names=FALSE))
    y <- y[!is.na(y)&nzchar(trimws(y))]
    if(length(y)) trimws(y[[1L]]) else ""
  }
  doi <- function(x) {
    v <- tolower(sub("^doi:\\s*","",sub("^https?://(dx\\.)?doi\\.org/","",scalar(x),ignore.case=TRUE),ignore.case=TRUE))
    if(grepl("^10\\.[0-9]{4,9}/[^[:space:]]+$",v)) v else ""
  }
  norm <- function(x) trimws(gsub("[^[:alnum:]]+"," ",tolower(scalar(x))))
  clean <- function(x) {
    s <- trimws(gsub("[[:space:]]+"," ",gsub("<[^>]+>"," ",scalar(x))))
    if(tolower(s) %in% c("","[no abstract available]","no abstract available","[no abstract]","no abstract","abstract unavailable","n/a","na","[no abstract provided]","no abstract provided")) "" else s
  }
  if(tolower(tools::file_ext(csv))=="ris") {
    lines <- readLines(csv,warn=FALSE,encoding="UTF-8")
    if(length(lines)) lines[1L] <- sub("^\ufeff","",lines[1L])
    records <- list(); rec <- NULL; tag <- NULL
    for(line in lines) {
      if(grepl("^[A-Z0-9]{2}  -",line)) {
        tag <- substr(line,1L,2L)
        value <- trimws(substring(line,6L))
        if(tag=="TY") {
          if(!is.null(rec)) stop("Unterminated RIS record.")
          rec <- list()
        } else if(tag=="ER") {
          if(is.null(rec)) stop("Unexpected RIS record terminator.")
          records[[length(records)+1L]] <- rec
          rec <- NULL; tag <- NULL
        } else if(!is.null(rec)) {
          rec[[tag]] <- c(rec[[tag]],value)
        }
      } else if(!is.null(rec)&&!is.null(tag)&&length(rec[[tag]])&&nzchar(trimws(line))) {
        n <- length(rec[[tag]])
        rec[[tag]][n] <- paste(rec[[tag]][n],trimws(line))
      }
    }
    if(!is.null(rec)) stop("Unterminated RIS record.")
    if(!length(records)) stop("No complete RIS records found.")
    field <- function(r,tags) {
      for(t in tags) {
        v <- scalar(r[[t]])
        if(nzchar(v)) return(v)
      }
      ""
    }
    tab <- data.frame(
      DOI=vapply(records,field,character(1),tags=c("DO")),
      Title=vapply(records,field,character(1),tags=c("TI","T1","CT")),
      Abstract=vapply(records,field,character(1),tags=c("AB","N2")),
      EID=vapply(records,field,character(1),tags=c("AN","ID")),
      Year=vapply(records,field,character(1),tags=c("PY","Y1","DA")),
      Authors=vapply(records,function(r)paste(c(r$AU,r$A1),collapse="; "),character(1)),
      stringsAsFactors=FALSE)
  } else {
    tab <- utils::read.csv(csv,check.names=FALSE,stringsAsFactors=FALSE,
      colClasses="character",fileEncoding="UTF-8-BOM",na.strings=character())
  }
  headers <- tolower(gsub("[^[:alnum:]]","",names(tab)))
  pick <- function(aliases,required=TRUE) {
    i <- match(aliases,headers,nomatch=0L); i <- i[i>0L]
    if(!length(i)) {
      if(required) stop("Required CSV column missing: ",paste(aliases,collapse="/"),
        ". Available columns: ",paste(names(tab),collapse=", "))
      return(rep("",nrow(tab)))
    }
    tab[[i[[1L]]]]
  }
  exported <- data.frame(
    doi=vapply(pick(c("doi","digitalobjectidentifier","di")),doi,character(1)),
    title=pick(c("title","articletitle","documenttitle","ti")),
    abstract=vapply(pick(c("abstract","ab")),clean,character(1)),
    provider_id=pick(c("eid","ut","accessionnumber","wosaccessionnumber"),FALSE),
    stringsAsFactors=FALSE)
  exported$norm_title <- vapply(exported$title,norm,character(1))
  year <- function(x) {
    x <- scalar(x)
    if(grepl("^[12][0-9]{3}",x)) substr(x,1L,4L) else ""
  }
  surname <- function(x) {
    x <- scalar(x)
    if(!nzchar(x)) return("")
    if(grepl(",",x,fixed=TRUE)) return(norm(sub(",.*$","",x)))
    # Only use unambiguously surname-first strings with a trailing initial.
    if(grepl("^[[:alpha:]'-]+[[:space:]]+[[:alpha:]][.]?$",x))
      return(norm(sub("[[:space:]].*$","",x)))
    ""
  }
  first_author <- function(x) {
    if(is.null(x)||!length(x)) return("")
    if(is.list(x)&&!is.null(names(x))) {
      for(k in c("last_name","family_name","family","surname","lastName","familyName")) {
        v <- norm(x[[k]])
        if(nzchar(v)) return(v)
      }
      for(k in c("name","display_name","full_name","fullName","author_name")) {
        v <- surname(x[[k]])
        if(nzchar(v)) return(v)
      }
      if(!is.null(x$author)) return(first_author(x$author))
      return("")
    }
    if(is.list(x)) return(first_author(x[[1L]]))
    surname(strsplit(scalar(x),";",fixed=TRUE)[[1L]][1L])
  }
  exported$year <- vapply(pick(c("year","publicationyear","py"),FALSE),year,character(1))
  exported$first_author <- vapply(pick(c("authors","author","au"),FALSE),first_author,character(1))
  
  rows <- lapply(readLines(input,warn=FALSE,encoding="UTF-8"),jsonlite::fromJSON,simplifyVector=FALSE)
  rid <- vapply(rows,function(r)scalar(r$identity$record_id),character(1))
  if(any(!nzchar(rid))||anyDuplicated(rid)) stop("Invalid canonical record identities.")
  has <- function(r) nzchar(scalar(r$canonical$abstract))
  before <- vapply(rows,has,logical(1))
  ds <- vapply(rows,function(r)doi(r$canonical$doi),character(1))
  ts <- vapply(rows,function(r)norm(r$canonical$title),character(1))
  ix <- which(!before)
  by_doi <- split(which(nzchar(exported$doi)),exported$doi[nzchar(exported$doi)])
  target_groups <- split(which(nzchar(ds)),ds[nzchar(ds)])
  by_title <- split(which(nzchar(exported$norm_title)),exported$norm_title[nzchar(exported$norm_title)])
  title_groups <- split(which(nzchar(ts)),ts[nzchar(ts)])
  audit <- vector("list",length(ix))
  used <- integer()
  for(j in seq_along(ix)) {
    i <- ix[[j]]
    candidates <- if(nzchar(ds[[i]])) by_doi[[ds[[i]]]] else NULL
    outcome <- "no_doi_match"
    accepted <- integer()
    similarity <- numeric()
    if(length(candidates)) {
      outcome <- "no_exported_abstract"
      cand <- candidates[nzchar(exported$abstract[candidates])]
      if(length(cand)) {
        outcome <- "title_guard_failed"
        if(nzchar(ts[[i]])) {
          similarity <- 1-stringdist::stringdist(ts[[i]],exported$norm_title[cand],method="jw",p=0.1)
          ok <- !is.na(similarity)&similarity>=0.90&nzchar(exported$norm_title[cand])
          # A shared DOI must be disambiguated with an exact title unique
          # across all canonical clusters bearing that DOI.
          group <- target_groups[[ds[[i]]]]
          if(length(group)>1L) {
            ok <- ok & exported$norm_title[cand]==ts[[i]] &
              sum(ts[group]==ts[[i]])==1L
          }
          accepted <- cand[ok]
          if(length(accepted)) {
            outcome <- "conflicting_exported_abstracts"
            if(length(unique(exported$abstract[accepted]))==1L) {
              k <- accepted[[1L]]
              rows[[i]]$canonical$abstract <- exported$abstract[[k]]
              rows[[i]]$manual_abstract_provenance <- list(provider=provider,
                export_sha256=csv_sha,export_row=k,provider_id=exported$provider_id[[k]],
                returned_doi=exported$doi[[k]],returned_title=exported$title[[k]],
                title_similarity=1-stringdist::stringdist(ts[[i]],exported$norm_title[[k]],method="jw",p=0.1))
              used <- c(used,accepted)
              outcome <- "recovered"
            }
          }
        }
      }
    }
    # Title recovery never overrides a conflicting nonempty DOI. Require
    # unique exact title, matching year and matching first author from the
    # same original manifestation. Reject conflicting candidate abstracts.
    if(outcome!="recovered" && nchar(ts[[i]])>=20L &&
       length(title_groups[[ts[[i]]]])==1L) {
      tc <- by_title[[ts[[i]]]]
      tc <- tc[nzchar(exported$abstract[tc]) &
        (!nzchar(ds[[i]]) | !nzchar(exported$doi[tc]) | exported$doi[tc]==ds[[i]])]
      if(length(tc)) {
        mans <- rows[[i]]$manifestations
        ok <- vapply(tc,function(k) {
          if(!nzchar(exported$year[[k]])||!nzchar(exported$first_author[[k]])) return(FALSE)
          any(vapply(mans,function(m) {
            z <- m$manifestation_metadata
            y <- year(z$year)
            a <- first_author(z$authors)
            nzchar(y)&&nzchar(a)&&identical(y,exported$year[[k]])&&
              identical(a,exported$first_author[[k]])
          },logical(1)))
        },logical(1))
        tc <- tc[ok]
        if(length(tc)&&length(unique(exported$abstract[tc]))==1L) {
          k <- tc[[1L]]
          rows[[i]]$canonical$abstract <- exported$abstract[[k]]
          rows[[i]]$manual_abstract_provenance <- list(provider=provider,
            export_sha256=csv_sha,export_row=k,provider_id=exported$provider_id[[k]],
            returned_doi=exported$doi[[k]],returned_title=exported$title[[k]],
            returned_year=exported$year[[k]],first_author=exported$first_author[[k]],
            match_method="unique_exact_title_year_first_author")
          used <- c(used,tc)
          accepted <- tc
          outcome <- "recovered_title_year_author"
        }
      }
    }
    audit[[j]] <- list(record_id=rid[[i]],doi=ds[[i]],outcome=outcome,
      candidate_export_rows=as.list(candidates),accepted_export_rows=as.list(accepted))
  }
  after <- vapply(rows,has,logical(1))
  stopifnot(length(rows)==length(rid),identical(rid,vapply(rows,function(r)scalar(r$identity$record_id),character(1))),
    all(after[before]))
  dest <- file.path(dirname(scored),"W02_main",paste0("manual_",provider,"_",substr(input_sha,1L,12L),"_",substr(csv_sha,1L,12L)))
  if(dir.exists(dest)) {
    old <- jsonlite::fromJSON(file.path(dest,"summary.json"))
    if(!identical(old$input_sha256,input_sha)||!identical(old$export_sha256,csv_sha)||
       !identical(old$output_sha256,sha(file.path(dest,"canonical.jsonl")))||
       !identical(old$audit_sha256,sha(file.path(dest,"audit.jsonl"))))
      stop("Existing manual import failed validation.")
    return(invisible(dest))
  }
  tmp <- tempfile(pattern=".manual-",tmpdir=dirname(dest))
  dir.create(tmp)
  on.exit(unlink(tmp,recursive=TRUE),add=TRUE)
  write_jsonl <- function(x,p) {
    con <- file(p,"wt",encoding="UTF-8"); on.exit(close(con))
    for(z in x) writeLines(jsonlite::toJSON(z,auto_unbox=TRUE,null="null",na="null",digits=NA),con,useBytes=TRUE)
  }
  canonical <- file.path(tmp,"canonical.jsonl")
  audit_path <- file.path(tmp,"audit.jsonl")
  write_jsonl(rows,canonical)
  write_jsonl(audit,audit_path)
  utils::write.csv(data.frame(record_id=rid[!after],doi=ds[!after],
    title=vapply(rows[!after],function(r)scalar(r$canonical$title),character(1))),
    file.path(tmp,"remaining_missing_abstracts.csv"),row.names=FALSE)
  jsonlite::write_json(list(schema="w02-manual-abstract-import-v1",status="complete",provider=provider,
    publication_clusters=length(rows),export_rows=nrow(exported),
    export_rows_with_abstract=sum(nzchar(exported$abstract)),
    export_distinct_dois=length(unique(exported$doi[nzchar(exported$doi)])),
    export_rows_used=length(unique(used)),
    title_year_author_recovered=sum(vapply(audit,function(a)identical(a$outcome,"recovered_title_year_author"),logical(1))),
    missing_before=sum(!before),recovered=sum(after)-sum(before),
    missing_after=sum(!after),abstracts_after=sum(after),
    outcomes=as.list(table(vapply(audit,function(a)a$outcome,character(1)))),
    input_sha256=input_sha,export_sha256=csv_sha,
    output_sha256=sha(canonical),audit_sha256=sha(audit_path)),
    file.path(tmp,"summary.json"),auto_unbox=TRUE,pretty=TRUE)
  remaining_dois <- sort(unique(ds[!after & nzchar(ds)]))
  safe <- remaining_dois[!grepl('"',remaining_dois,fixed=TRUE)]
  if(length(safe)) {
    query <- paste0('DO=(',paste(sprintf('"%s"',safe),collapse=" OR "),')')
    writeLines(query,file.path(tmp,"wos_remaining_dois.txt"),useBytes=TRUE)
  } else writeLines("",file.path(tmp,"wos_remaining_dois.txt"))
  if(!file.rename(tmp,dest)) stop("Cannot publish manual abstract import.")
  message("[W02] ",provider,": ",sum(after)-sum(before)," abstracts recovered; ",sum(!after)," remaining.")
  invisible(dest)
}
