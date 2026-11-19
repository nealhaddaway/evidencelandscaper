#' Export completed OSTARA grey-literature case extraction without API calls
#'
#' Reads only existing versioned case_data.jsonl checkpoints and archived
#' schema. Does not call the model, stage documents, or change original ledgers.
#' Exports one CSV per extracted case, one publication status CSV, and an HTML
#' audit report. Zero-case assessments are retained and flagged for review,
#' never silently treated as proven absence of collective action.
#'
#' @param map_path OSTARA project directory.
#' @param batch_dir Exact grey-literature run directory; if NULL, discover the
#'   single matching "grey_fulltexts__" run under runs/W09/grey_literature.
#' @return Paths and audited counts, invisibly.
#' @export
export_ostara_grey_literature_partial <- function(map_path,batch_dir=NULL) {
  norm<-function(p)normalizePath(p,winslash="/",mustWork=TRUE)
  sha<-function(p)digest::digest(file=p,algo="sha256",serialize=FALSE)
  csv<-function(p)utils::read.csv(p,stringsAsFactors=FALSE,check.names=FALSE,
                                  fileEncoding="UTF-8-BOM",colClasses="character",
                                  na.strings=character())
  scalar<-function(x) {
    if(is.null(x)||!length(x))return("")
    paste(as.character(unlist(x,use.names=FALSE)),collapse=" | ")
  }
  save_csv<-function(x,p) {
    tmp<-tempfile(pattern=".ostara-partial-",tmpdir=dirname(p),fileext=".csv")
    on.exit(if(file.exists(tmp))unlink(tmp),add=TRUE)
    con<-file(tmp,open="wb")
    writeBin(as.raw(c(239L,187L,191L)),con)
    utils::write.table(x,con,sep=",",row.names=FALSE,col.names=TRUE,
                       quote=TRUE,qmethod="double",na="")
    close(con)
    if(!file.copy(tmp,p,overwrite=TRUE))stop("Cannot export ",p)
  }
  jl<-function(p) {
    if(!file.exists(p))return(list())
    txt<-readLines(p,warn=FALSE,encoding="UTF-8")
    txt<-txt[nzchar(trimws(txt))]
    if(!length(txt))return(list())
    lapply(seq_along(txt),function(i)tryCatch(
      jsonlite::fromJSON(txt[[i]],simplifyVector=FALSE),
      error=function(e)stop("Malformed JSONL checkpoint at ",p,": line ",i,
                            ": ",conditionMessage(e))))
  }
  map_path<-norm(map_path)
  if(is.null(batch_dir)) {
    root<-file.path(map_path,"runs","W09","grey_literature")
    if(!dir.exists(root))stop("No grey literature W09 folder: ",root)
    dirs<-list.dirs(root,recursive=FALSE,full.names=TRUE)
    dirs<-dirs[startsWith(basename(dirs),"grey_fulltexts__")&
               file.exists(file.path(dirs,"grey_sources_manifest.json"))]
    if(length(dirs)!=1L)
      stop("Expected exactly one grey_fulltexts run, found ",length(dirs),
           ". Specify batch_dir explicitly. Candidates: ",
           paste(dirs,collapse=" ; "))
    batch_dir<-dirs[[1L]]
  }
  batch_dir<-norm(batch_dir)
  manifest_file<-file.path(batch_dir,"grey_sources_manifest.json")
  if(!file.exists(manifest_file))stop("Missing grey source manifest: ",manifest_file)
  manifest<-jsonlite::fromJSON(manifest_file,simplifyVector=FALSE)
  if(!identical(manifest$schema,"ostara-grey-literature-filename-only-batch-v2"))
    stop("Unrecognised grey source manifest; no export attempted")
  raw_sources<-manifest$source_records
  if(!length(raw_sources))stop("No sources listed in source manifest")
  inventory<-do.call(rbind,lapply(raw_sources,function(z)data.frame(
    record_id=scalar(z$record_id),
    file_name=scalar(z$file_name),
    title=scalar(z$title),
    source_format=scalar(z$source_format),
    source_sha256=scalar(z$source_sha256),
    stringsAsFactors=FALSE)))
  if(anyDuplicated(inventory$record_id)||any(!nzchar(inventory$record_id)))
    stop("Duplicate/missing source IDs in grey source register")
  ledger_root<-file.path(batch_dir,"case_data_extraction")
  vers<-if(dir.exists(ledger_root))
    list.dirs(ledger_root,recursive=FALSE,full.names=TRUE) else character()
  vers<-vers[file.exists(file.path(vers,"manifest.json"))]
  if(length(vers)!=1L)
    stop("Expected exactly one case extraction version under ",ledger_root,
         ", found ",length(vers),". No ledgers were modified.")
  xdir<-vers[[1L]]
  em<-jsonlite::fromJSON(file.path(xdir,"manifest.json"),simplifyVector=FALSE)
  spec_path<-file.path(xdir,"case_schema.csv")
  ledger_path<-file.path(xdir,"case_data.jsonl")
  if(!file.exists(spec_path)||!identical(sha(spec_path),em$csv_sha256))
    stop("Archived schema missing or does not match extraction manifest")
  if(!identical(scalar(em$prompt_sha256),scalar(manifest$locked_prompt_sha256)))
    stop("Extraction prompt hash differs from grey source manifest")
  spec<-csv(spec_path)
  if(!all(c("Question","Definition")%in%names(spec)))
    stop("Archived schema has no case-field definitions")
  spec<-spec[!grepl("mechanisms of the collective action",
                    spec$Question,ignore.case=TRUE),,drop=FALSE]
  if(nrow(spec)!=41L||!identical(trimws(spec$Question[[1L]]),"Type of source"))
    stop("Expected the archived revised grey-literature 41-field schema")
  done<-jl(ledger_path)
  done_ids<-vapply(done,function(z)scalar(z$record_id),character(1))
  if(anyDuplicated(done_ids)||any(!done_ids%in%inventory$record_id))
    stop("Completed ledger contains unexpected or duplicate record IDs")
  if(any(!vapply(done,function(z)is.list(z$cases),logical(1))))
    stop("Completed checkpoint contains a missing cases array")
  status<-inventory
  status$extraction_status<-ifelse(status$record_id%in%done_ids,
                               "validated","not_yet_validated")
  counts<-vapply(done,function(z)length(z$cases),integer(1))
  status$case_count<-as.integer(NA)
  status$case_count[match(done_ids,status$record_id)]<-counts
  status$publication_extraction_status<-""
  status$publication_extraction_status[match(done_ids,status$record_id)]<-
    vapply(done,function(z)scalar(z$publication_extraction_status),character(1))
  status$review_priority<-ifelse(status$extraction_status!="validated",
    "not_assessed",
    ifelse(!is.na(status$case_count)&status$case_count==0L,
      "check_zero_case_classification","review_extracted_cases"))
  status$readable_full_text<-""
  status$readability_issue<-""
  readable_path<-file.path(batch_dir,"grey_source_readability.csv")
  if(file.exists(readable_path)) {
    readable<-csv(readable_path)
    k<-match(status$record_id,readable$record_id)
    if("readable_full_text"%in%names(readable))
      status$readable_full_text<-ifelse(is.na(k),"",
                                      readable$readable_full_text[k])
    if("readability_issue"%in%names(readable))
      status$readability_issue<-ifelse(is.na(k),"",readable$readability_issue[k])
    status$extraction_status[
      status$extraction_status=="not_yet_validated"&
      status$readable_full_text=="FALSE"]<-"unreadable_full_text"
    status$review_priority[
      status$extraction_status=="unreadable_full_text"]<-"fix_source"
  }
  names_out<-unlist(lapply(seq_len(nrow(spec)),function(i) {
    nm<-paste0(sprintf("%02d",i),". ",spec$Question[[i]])
    c(nm,paste0(nm," [evidence]"),
      paste0(nm," [reporting status]"))
  }),use.names=FALSE)
  names_out<-make.unique(names_out)
  header<-c("record_id","source_filename","source_title","case_id",
            "assessment_reason","case_boundary_uncertainty",names_out)
  rows<-list()
  for(z in done) {
    id<-scalar(z$record_id)
    src<-inventory[match(id,inventory$record_id),,drop=FALSE]
    for(onecase in z$cases) {
      fields<-onecase$fields
      if(!is.list(fields))stop("Case lacks field data: ",scalar(onecase$case_id))
      values<-character()
      for(i in seq_len(nrow(spec))) {
        f<-fields[[sprintf("field_%03d",i)]]
        if(is.null(f))stop("Case missing field_",sprintf("%03d",i),
                           ": ",scalar(onecase$case_id))
        v<-if(!is.null(f$values))unlist(f$values,use.names=FALSE)
           else unlist(f$value,use.names=FALSE)
        value<-if(!length(v))"" else paste(as.character(v),collapse="\n")
        evidence<-if(length(f$evidence))
          vapply(f$evidence,function(e)
            paste0(scalar(e$anchor),": ",scalar(e$quote)),character(1))
          else character()
        values<-c(values,value,paste(evidence,collapse="\n"),
                  scalar(f$reporting_status))
      }
      rows[[length(rows)+1L]]<-c(
        id,src$file_name[[1L]],src$title[[1L]],
        scalar(onecase$case_id),scalar(onecase$assessment_reason),
        scalar(onecase$case_boundary_uncertainty),values)
    }
  }
  if(length(rows)) {
    cases<-as.data.frame(do.call(rbind,rows),stringsAsFactors=FALSE,
                         check.names=FALSE)
    names(cases)<-header
  } else {
    cases<-as.data.frame(matrix(character(),nrow=0L,ncol=length(header)),
                         stringsAsFactors=FALSE,check.names=FALSE)
    names(cases)<-header
  }
  outdir<-file.path(batch_dir,"partial_extraction_report")
  dir.create(outdir,recursive=TRUE,showWarnings=FALSE)
  casefile<-file.path(outdir,"grey_cases_extracted_so_far.csv")
  statusfile<-file.path(outdir,"grey_publication_status_so_far.csv")
  save_csv(cases,casefile)
  save_csv(status,statusfile)
  # Preserve the original input filenames for repeated PDF copies.
  duplicate_file<-file.path(batch_dir,"grey_duplicate_files.csv")
  duplicate_total<-NA_integer_
  if(file.exists(duplicate_file)) {
    dup<-csv(duplicate_file)
    if("selected_for_extraction"%in%names(dup))
      duplicate_total<-sum(dup$selected_for_extraction=="FALSE")
  }
  invalid_file<-file.path(xdir,"invalid_responses.jsonl")
  invalid<-jl(invalid_file)
  n_invalid<-length(invalid) # attempts, not distinct publications
  attempted_ids<-unique(c(done_ids,
    vapply(invalid,function(z)scalar(z$record_id),character(1))))
  # Checkpointed zero-case responses are extraction outputs, not verified negatives.
  n_zero<-sum(counts==0L)
  n_pos<-sum(counts>0L)
  n_unreadable<-sum(status$extraction_status=="unreadable_full_text")
  n_outstanding<-sum(status$extraction_status=="not_yet_validated")
  report<-file.path(outdir,"grey_partial_extraction_report.html")
  esc<-function(x) {
    x<-gsub("&","&amp;",x,fixed=TRUE)
    x<-gsub("<","&lt;",x,fixed=TRUE)
    x<-gsub(">","&gt;",x,fixed=TRUE)
    x
  }
  summary<-data.frame(
    Metric=c("Unique source documents","Completed/validated assessments",
      "Completed assessments with >=1 extracted case",
      "Completed assessments with zero extracted cases (REVIEW)",
      "Extracted cases (not manually adjudicated)",
      "Unprocessed but readable","Unreadable sources",
      "Recorded invalid model attempts","Duplicate input files skipped"),
    Count=c(nrow(inventory),length(done),n_pos,n_zero,nrow(cases),
            n_outstanding,n_unreadable,n_invalid,duplicate_total),
    stringsAsFactors=FALSE)
  html_table<-function(df) {
    h<-paste0("<tr>",paste0("<th>",esc(names(df)),"</th>",
                            collapse=""),"</tr>")
    body<-apply(df,1L,function(row)paste0("<tr>",
      paste0("<td>",esc(ifelse(is.na(row),"",row)),"</td>",collapse=""),
      "</tr>"))
    paste0("<table>",h,paste(body,collapse="\n"),"</table>")
  }
  timestamp<-format(Sys.time(),"%Y-%m-%d %H:%M %Z")
  out<-c(
    "<!doctype html><html><head><meta charset='UTF-8'>",
    "<title>OSTARA grey literature: partial extraction audit</title>",
    "<style>body{font-family:system-ui,Arial,sans-serif;max-width:1100px;margin:45px auto;padding:0 20px;color:#222;line-height:1.5}table{border-collapse:collapse;width:100%;margin:18px 0}td,th{border-bottom:1px solid #ddd;padding:8px;text-align:left;vertical-align:top}th{background:#f5f5f5}h1{font-size:25px}h2{font-size:19px}code{overflow-wrap:anywhere}</style></head><body>",
    "<h1>OSTARA grey-literature full-text extraction: interim report</h1>",
    paste0("<p>Generated: ",esc(timestamp),"</p>"),
    "<p><strong>Scope:</strong> only already checkpointed case extractions. No API requests, revalidation or fresh screening were performed for this report.</p>",
    html_table(summary),
    "<h2>Interpretation and quality review</h2>",
    paste0("<p><strong>",n_zero,
           " completed documents returned zero cases.</strong> These are model outputs, not confirmed absence of qualifying collective action. Because the extraction was interrupted over concerns about false negatives, review these first against the source documents and the revised eligibility rules.</p>"),
    "<p>Unprocessed and unreadable documents are not treated as negative results. Case counts reflect the saved checkpoint, not deduplicated, adjudicated real-world initiatives across different publications.</p>",
    "<h2>Provenance</h2>",
    paste0("<p>Source batch: <code>",esc(batch_dir),"</code></p>"),
    paste0("<p>Checkpoint: <code>",esc(ledger_path),"</code></p>"),
    paste0("<p>Case schema SHA-256: <code>",sha(spec_path),"</code></p>"),
    paste0("<p>Prompt SHA-256: <code>",esc(scalar(em$prompt_sha256)),"</code></p>"),
    paste0("<p>Completed ledger SHA-256: ",if(file.exists(ledger_path))
      paste0("<code>",sha(ledger_path),"</code>") else "No checkpoint file found","</p>"),
    "<h2>Data outputs</h2>",
    "<p><code>grey_cases_extracted_so_far.csv</code>: one row per extracted case with answers, evidence quotations, anchors and reporting status for 41 grey-literature fields.</p>",
    "<p><code>grey_publication_status_so_far.csv</code>: all registered unique documents, including zero-case, unreadable and not-yet-assessed records, with review priorities.</p>",
    "<p>CSV files are written in UTF-8 with BOM for Excel compatibility.</p>",
    "</body></html>"
  )
  writeLines(out,report,useBytes=TRUE)
  message("[OSTARA grey interim] ",length(done),"/",nrow(inventory),
    " completed; ",nrow(cases)," extracted cases; ",n_zero,
    " zero-case assessments flagged for review; ",n_outstanding,
    " pending readable. REPORT: ",report)
  invisible(list(report=report,cases_csv=casefile,status_csv=statusfile,
    unique_sources=nrow(inventory),validated=length(done),
    zero_case_publications=n_zero,positive_publications=n_pos,
    extracted_cases=nrow(cases),not_yet_validated=n_outstanding,
    unreadable=n_unreadable,invalid_attempts=n_invalid))
}
