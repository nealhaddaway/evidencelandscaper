#' Lock a completed W02 corpus after supplementary recovery
#' @param scored Final W01 scoring directory.
#' @param stage_dirs Ordered supplementary stage directory names under W02_main.
#' @param notes Character vector of methodological limitations.
#' @return Verified locked directory; no provider requests or W03 execution.
#' @export
lock_w02 <- function(scored,stage_dirs,notes=character()) {
  scored <- normalizePath(scored,mustWork=TRUE)
  main <- file.path(dirname(scored),"W02_main")
  sha <- function(p) digest::digest(file=p,algo="sha256",serialize=FALSE)
  readj <- function(p) jsonlite::fromJSON(p,simplifyVector=FALSE)
  writej <- function(z,p) jsonlite::write_json(z,p,auto_unbox=TRUE,pretty=TRUE,null="null")
  scalar <- function(x) {
    if(is.null(x)||!length(x))return("")
    z<-as.character(unlist(x,use.names=FALSE))
    z<-z[!is.na(z)&nzchar(trimws(z))]
    if(length(z))z[[1L]] else ""
  }
  read_rows <- function(p) lapply(readLines(p,warn=FALSE,encoding="UTF-8"),jsonlite::fromJSON,simplifyVector=FALSE)
  get_ids <- function(x) vapply(x,function(r)scalar(r$identity$record_id),character(1))
  get_abs <- function(x) vapply(x,function(r)scalar(r$canonical$abstract),character(1))
  get_doi <- function(x) vapply(x,function(r)scalar(r$canonical$doi),character(1))
  if(!is.character(stage_dirs)||!length(stage_dirs)||anyDuplicated(stage_dirs)||
     any(basename(stage_dirs)!=stage_dirs)||any(stage_dirs%in%c(".","..","locked","finalised")))
    stop("Supply the ordered supplementary directory names.")
  pointer <- file.path(main,"final_lock.json")
  out <- file.path(main,"locked")
  check_lock <- function() {
    p<-readj(pointer); m<-file.path(out,"manifest.json")
    stopifnot(identical(p$schema,"local-w02-final-lock-v1"),identical(p$directory,"locked"),
      identical(p$manifest_sha256,sha(m)))
    z<-readj(m)
    stopifnot(identical(z$status,"final"),identical(unlist(z$stage_dirs,use.names=FALSE),stage_dirs))
    for(n in names(z$files))stopifnot(identical(z$files[[n]],sha(file.path(out,n))))
    invisible(out)
  }
  if(file.exists(pointer))return(check_lock())
  if(dir.exists(out))stop("Unregistered locked directory exists; refusing overwrite.")
  input<-file.path(main,"canonical_input.jsonl")
  base<-file.path(main,"finalised","canonical.jsonl")
  base_dir<-file.path(main,"finalised")
  app<-readj(file.path(base_dir,"application_report.json"))
  rep<-readj(file.path(main,"report.json"))
  base_summary<-readj(file.path(base_dir,"summary.json"))
  initial_sha<-sha(input);base_sha<-sha(base)
  stopifnot(identical(rep$input_sha256,initial_sha),identical(rep$output_sha256,sha(file.path(main,"enriched.jsonl"))),
    identical(rep$audit_sha256,sha(file.path(main,"audit.jsonl"))),
    identical(app$status,"PASS"),identical(app$input_sha256,initial_sha),
    identical(app$output_sha256,base_sha),identical(base_summary$status,"final"))
  rows<-read_rows(input);ids<-get_ids(rows);previous_abs<-get_abs(rows);original<-nzchar(previous_abs)
  stopifnot(!anyDuplicated(ids),all(nzchar(ids)),length(ids)==as.integer(base_summary$publication_clusters))
  original_doi<-get_doi(rows)
  source<-ifelse(original,"original","missing")
  source_stage<-ifelse(original,"W01 canonical input","")
  ledger<-list(); history<-list()
  check_transition <- function(next_rows,stage,expected=NULL) {
    stopifnot(identical(ids,get_ids(next_rows)),identical(original_doi,get_doi(next_rows)))
    a<-get_abs(next_rows);had<-nzchar(previous_abs)
    if(!identical(a[had],previous_abs[had]))stop("Existing abstract changed in ",stage)
    gained<-which(!had&nzchar(a))
    if(!is.null(expected)&&length(gained)!=as.integer(expected))stop("Recovery count mismatch: ",stage)
    list(abstracts=a,gained=gained)
  }
  rows<-read_rows(base)
  transition<-check_transition(rows,"main")
  main_audit<-read_rows(file.path(main,"audit.jsonl"))
  for(a in main_audit){
    i<-match(scalar(a$record_id),ids)
    if(is.na(i)||!i%in%transition$gained)next
    applied<-Filter(function(z)identical(z$field,"abstract"),a$applied)
    if(length(applied)==1L)source[i]<-scalar(applied[[1L]]$provider)
  }
  if(any(source[transition$gained]=="missing"))stop("Main abstract provenance incomplete.")
  source_stage[transition$gained]<-"main"
  ledger[[1L]]<-data.frame(stage="main",before=sum(original),recovered=length(transition$gained),
    after=sum(nzchar(transition$abstracts)),input_sha256=initial_sha,output_sha256=base_sha,
    audit_sha256=rep$audit_sha256)
  history$main<-rep
  previous_abs<-transition$abstracts;prev_sha<-base_sha
  provenance_fields<-c("supplementary_abstract_provenance","manual_abstract_provenance",
    "publisher_abstract_provenance","crossref_title_provenance","doi_metadata_provenance","final_lookup_provenance")
  for(stage in stage_dirs){
    d<-file.path(main,stage);m<-readj(file.path(d,"summary.json"))
    p<-file.path(d,"canonical.jsonl");ap<-file.path(d,"audit.jsonl")
    next_sha<-sha(p)
    stopifnot(identical(m$status,"complete"),identical(m$input_sha256,prev_sha),
      identical(m$output_sha256,next_sha),identical(m$audit_sha256,sha(ap)),
      as.integer(m$publication_clusters)==length(ids))
    rows<-read_rows(p)
    tr<-check_transition(rows,stage,m$recovered)
    stopifnot(as.integer(m$missing_before)==sum(!nzchar(previous_abs)),
      as.integer(m$missing_after)==sum(!nzchar(tr$abstracts)))
    for(i in tr$gained){
      r<-rows[[i]]
      if(!is.null(r$crossref_title_provenance)&&startsWith(stage,"crossref_titles_"))provider<-"crossref_title"
      else {
        vals<-unique(vapply(provenance_fields,function(k)scalar(r[[k]]$provider),character(1)))
        vals<-vals[nzchar(vals)]
        if(length(vals)!=1L)stop("Ambiguous recovery provenance in ",stage,": ",ids[i])
        provider<-vals[[1L]]
      }
      source[i]<-provider;source_stage[i]<-stage
    }
    ledger[[length(ledger)+1L]]<-data.frame(stage=stage,before=sum(nzchar(previous_abs)),
      recovered=length(tr$gained),after=sum(nzchar(tr$abstracts)),input_sha256=prev_sha,
      output_sha256=next_sha,audit_sha256=m$audit_sha256)
    history[[stage]]<-m
    previous_abs<-tr$abstracts;prev_sha<-next_sha
    message("[W02 lock] Verified ",stage,"; abstracts ",sum(nzchar(previous_abs)))
  }
  stopifnot(all(source[nzchar(previous_abs)]!="missing"))
  if(!requireNamespace("xml2",quietly=TRUE))stop("xml2 required.")
  # Conservative cleaning: only known markup, whitespace and an initial
  # abstract label. Never infer or delete substantive sentences/translations.
  clean_one<-function(s){
    if(!nzchar(s))return(s)
    x<-s
    if(grepl("</?(?:[[:alnum:]_-]+:)?(?:abstract|p|sec|title|bold|italic|b|i|sup|sub|br|div|span|strong|em)(?:[[:space:]/>])",x,ignore.case=TRUE,perl=TRUE)){
      # Avoid flattening math, tables, or unknown XML: flag these instead.
      tags<-regmatches(x,gregexpr("</?[[:alpha:]_][[:alnum:]_:.\\-]*",x,perl=TRUE))[[1L]]
      tags<-tolower(sub("^</?","",tags))
      tags<-sub("^.*:","",tags)
      known<-c("abstract","p","sec","title","bold","italic","b","i","sup","sub","br","div","span","strong","em")
      if(all(tags%in%known)){
        x<-gsub("</?(?:[[:alnum:]_-]+:)?(?:p|sec|title|br|div|abstract)(?:[[:space:]][^>]*)?/?>"," ",x,ignore.case=TRUE,perl=TRUE)
        x<-gsub("</?(?:[[:alnum:]_-]+:)?(?:bold|italic|b|i|sup|sub|span|strong|em)(?:[[:space:]][^>]*)?/?>","",x,ignore.case=TRUE,perl=TRUE)
      }
    }
    # Decode entities without interpreting ordinary comparison symbols as tags.
    if(grepl("&(?:amp|lt|gt|quot|apos|nbsp|#[0-9]+|#x[0-9a-fA-F]+);",x,perl=TRUE)){
      safe<-gsub("<","&lt;",x,fixed=TRUE)
      safe<-gsub(">","&gt;",safe,fixed=TRUE)
      doc<-tryCatch(xml2::read_html(paste0("<div>",safe,"</div>"),options=c("RECOVER","NONET","NOERROR","NOWARNING")),error=function(e)NULL)
      if(!is.null(doc))x<-xml2::xml_text(xml2::xml_find_first(doc,"//div"))
    }
    x<-gsub("\u00a0"," ",x,fixed=TRUE)
    x<-trimws(gsub("[[:space:]]+"," ",x))
    x<-sub("^Abstract[[:space:]]*:[[:space:]]*","",x,ignore.case=TRUE)
    if(!nzchar(x))s else x
  }
  cleaned<-vapply(previous_abs,clean_one,character(1),USE.NAMES=FALSE)
  stopifnot(identical(nzchar(cleaned),nzchar(previous_abs)))
  changed<-which(cleaned!=previous_abs)
  suspicious<-which(nzchar(cleaned)&(nchar(cleaned)<40L|
    grepl("^(no abstract|abstract unavailable|not available)[[:space:].]*$",cleaned,ignore.case=TRUE)|
    grepl("</?[[:alpha:]_][[:alnum:]_:.\\-]*(?:[[:space:]/>])",cleaned,perl=TRUE)))
  tmp<-tempfile("lock_",tmpdir=main);dir.create(tmp)
  on.exit(if(dir.exists(tmp))unlink(tmp,recursive=TRUE),add=TRUE)
  for(i in changed)rows[[i]]$canonical$abstract<-cleaned[i]
  canonical<-file.path(tmp,"canonical.jsonl")
  con<-file(canonical,"wt",encoding="UTF-8")
  tryCatch(for(r in rows)writeLines(jsonlite::toJSON(r,auto_unbox=TRUE,null="null",na="null",digits=NA),con,useBytes=TRUE),finally=close(con))
  check<-read_rows(canonical)
  stopifnot(identical(ids,get_ids(check)),identical(original_doi,get_doi(check)),identical(cleaned,get_abs(check)))
  # Confirm no other record fields changed during cleaning/serialization.
  for(i in seq_along(rows))if(!identical(rows[[i]],check[[i]]))stop("Serialization changed record ",ids[i])
  counts<-as.data.frame(table(source[!original&nzchar(cleaned)]),stringsAsFactors=FALSE)
  names(counts)<-c("provider","recovered")
  stopifnot(sum(counts$recovered)==sum(nzchar(cleaned))-sum(original))
  steps<-do.call(rbind,ledger)
  utils::write.csv(steps,file.path(tmp,"recovery_stages.csv"),row.names=FALSE)
  utils::write.csv(counts,file.path(tmp,"recovery_sources.csv"),row.names=FALSE)
  utils::write.csv(data.frame(record_id=ids,abstract_source=source,recovery_stage=source_stage,
    cleaned=seq_along(ids)%in%changed,quality_flag=seq_along(ids)%in%suspicious),
    file.path(tmp,"abstract_provenance.csv"),row.names=FALSE)
  hash_text<-function(x)vapply(x,function(s)digest::digest(s,algo="sha256",serialize=FALSE),character(1))
  utils::write.csv(data.frame(record_id=ids[changed],before_sha256=hash_text(previous_abs[changed]),
    after_sha256=hash_text(cleaned[changed])),file.path(tmp,"cleaning_audit.csv"),row.names=FALSE)
  utils::write.csv(data.frame(record_id=ids[suspicious],abstract_characters=nchar(cleaned[suspicious])),
    file.path(tmp,"abstract_quality_flags.csv"),row.names=FALSE)
  miss<-which(!nzchar(cleaned))
  utils::write.csv(data.frame(record_id=ids[miss],doi=original_doi[miss],
    title=vapply(rows[miss],function(r)scalar(r$canonical$title),character(1))),
    file.path(tmp,"remaining_missing_abstracts.csv"),row.names=FALSE)
  candidates<-file.path(main,tail(stage_dirs,1L),"discovered_dois.csv")
  if(file.exists(candidates))stopifnot(file.copy(candidates,file.path(tmp,"discovered_dois.csv")))
  writej(history,file.path(tmp,"recovery_history.json"))
  n<-length(ids);a<-sum(nzchar(cleaned));b<-sum(original)
  summary<-list(status="final",publication_clusters=n,abstracts_before=b,abstracts_after=a,
    recovered=a-b,remaining_without_abstract=n-a,coverage_percent=100*a/n,
    recovery_percent=if(n>b)100*(a-b)/(n-b) else 100,
    abstracts_cleaned=length(changed),quality_flags_retained=length(suspicious),
    source_sha256=prev_sha,output_sha256=sha(canonical))
  writej(summary,file.path(tmp,"summary.json"))
  md<-c("# W02 final metadata enrichment report","",
    paste("Finalised (UTC):",format(Sys.time(),tz="UTC",usetz=TRUE)),"",
    sprintf("The corpus contains %s unique publication clusters. Author-written abstracts were recovered from bibliographic metadata and imported database exports; no full-text extraction or synthetic abstracts were used.",n),
    sprintf("Original abstracts: %s; recovered: %s; final abstracts: %s; missing: %s (coverage %.2f%%).",b,a-b,a,n-a,100*a/n),"",
    "## Recovery by source","","| Source | Recovered |","|---|---:|",
    sprintf("| %s | %s |",counts$provider,counts$recovered),"",
    "## Stage history","","| Stage | Before | Recovered | After |","|---|---:|---:|---:|",
    sprintf("| %s | %s | %s | %s |",steps$stage,steps$before,steps$recovered,steps$after),"",
    "## Cleaning and integrity","",
    sprintf("Conservative markup, entity and whitespace cleaning changed %s abstracts. %s abstracts with residual markup or other simple quality indicators were flagged and retained. No substantive translation or sentence deletion was attempted.",length(changed),length(suspicious)),
    "All publication IDs, order and canonical DOIs were retained. Recovery stages were validated against input, output and audit SHA-256 hashes and checked for loss or replacement of previously available abstracts.",
    "Missing abstracts remain missing; no publication is excluded on that basis. Discovered DOI candidates are retained separately.","",
    "## Limitations","",paste0("- ",notes),
    "- Provider results and errors are retained in the original audits and recovery_history.json. The lock verifies lineage and integrity, not independent bibliographic accuracy or completeness.",
    "- No further retrieval was performed during finalisation.","")
  writeLines(md,file.path(tmp,"w02_report.md"),useBytes=TRUE)
  files<-list.files(tmp)
  hashes<-setNames(lapply(file.path(tmp,files),sha),files)
  manifest<-list(schema="local-w02-final-lock-v1",status="final",stage_dirs=as.list(stage_dirs),
    publication_clusters=n,source_sha256=prev_sha,output_sha256=sha(canonical),files=hashes)
  writej(manifest,file.path(tmp,"manifest.json"))
  if(!file.rename(tmp,out))stop("Cannot publish W02 lock.")
  ptmp<-tempfile(tmpdir=main)
  writej(list(schema="local-w02-final-lock-v1",directory="locked",
    manifest_sha256=sha(file.path(out,"manifest.json"))),ptmp)
  if(!file.rename(ptmp,pointer))stop("Cannot register W02 lock.")
  check_lock()
  message("[W02 lock] PASS: ",n," publications; ",a," abstracts; ",n-a," missing; ",length(changed)," cleaned.")
  invisible(out)
}
