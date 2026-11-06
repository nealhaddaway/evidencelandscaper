#' Export all additional full-text case-positive publications for colleague review
#'
#' The W08 and full-text sources are explicitly pinned here to the observed
#' 266/2440 final cohorts and the approved full-text mechanism prompt/ontology.
#' It never infers a missing human W08 classification to be negative.
#' Abstracts are recovered from checksum-verified canonical W02 bibliographic
#' records, with reconciled W04 abstract corrections where recorded.
#' @param destination_zip New portable review ZIP.
#' @param map_path OSTARA map root; NULL resolves the named map.
#' @return Absolute ZIP path, invisibly.
#' @export
export_ostara_new_case_review <- function(destination_zip,map_path=NULL) {
  if(is.null(map_path))map_path<-open_map("Ostara_collective_action_map")
  map_path<-normalizePath(map_path,winslash="/",mustWork=TRUE)
  if(length(destination_zip)!=1L||is.na(destination_zip)||
     !grepl("\\.zip$",destination_zip,ignore.case=TRUE))
    stop("Supply a single destination_zip ending in .zip.")
  destination_zip<-file.path(normalizePath(dirname(destination_zip),
     winslash="/",mustWork=TRUE),basename(destination_zip))
  if(file.exists(destination_zip))stop("ZIP already exists: ",destination_zip)
  w08root<-file.path(map_path,"runs","W08")
  w09root<-file.path(map_path,"runs","W09")
  w08<-list(
    first10=file.path(w08root,
      "sample-266-seed-20261023-f83478f861-5997a8f147-model-gpt-5.6-luna"),
    remaining90=file.path(w08root,
      "sample-2440-seed-20261023-f0c2bd834e-5997a8f147-model-gpt-5.6-luna"))
  abstract_files<-list(
    first10=file.path(w08$first10,"human_adjudication",
      "v1-b7a30eb3123c","effective_case_classifications.csv"),
    remaining90=file.path(w08root,"remaining90-final-case-export-3d194580b61a",
      "final_case_classifications_2440.csv"))
  mechanism<-list(
    first10=file.path(w09root,"ft-mechanism-stage-first10",
      "fulltext_mechanism_inventory","v4-566b51e0959d"),
    remaining90=file.path(w09root,"ft-mechanism-stage-remaining90",
      "fulltext_mechanism_inventory","v4-4a462b89bacb"))
  sha<-function(p)digest::digest(file=p,algo="sha256",serialize=FALSE)
  j<-function(p)jsonlite::fromJSON(p,simplifyVector=FALSE)
  jl<-function(p) {
    if(!file.exists(p))stop("Missing source: ",p)
    s<-readLines(p,warn=FALSE,encoding="UTF-8")
    lapply(s[nzchar(trimws(s))],jsonlite::fromJSON,simplifyVector=FALSE)
  }
  sc<-function(x)if(is.null(x)||!length(x)||all(is.na(x)))"" else
    paste(as.character(unlist(x,use.names=FALSE)),collapse="; ")
  title_search<-function(x) {
    x<-gsub("[^[:alnum:] ]","",x)
    x<-trimws(gsub("[[:space:]]+"," ",x))
    paste0("https://scholar.google.co.uk/scholar?start=0&q=",
           gsub(" ","+",x,fixed=TRUE))
  }
  doi_link<-function(x) {
    x<-trimws(x)
    x<-sub("^https?://(dx\\.)?doi\\.org/","",x,ignore.case=TRUE)
    x<-sub("^doi:[[:space:]]*","",x,ignore.case=TRUE)
    if(!nzchar(x))return("")
    paste0("https://doi.org/",utils::URLencode(x,reserved=FALSE))
  }
  records<-list()
  sources<-list()
  for(cohort in names(w08)) {
    d<-w08[[cohort]]
    m<-j(file.path(d,"manifest.json"))
    selected<-as.character(unlist(m$selected_record_ids,use.names=FALSE))
    if(anyDuplicated(selected))stop("Duplicate W08 selected records.")
    final<-utils::read.csv(abstract_files[[cohort]],
                           stringsAsFactors=FALSE)
    if(!all(c("record_id","final_case_status")%in%names(final))||
       anyDuplicated(final$record_id)||any(!final$record_id%in%selected))
      stop("Invalid W08 final classifications: ",cohort)
    if(cohort=="first10") {
      hm<-j(file.path(dirname(abstract_files[[cohort]]),"manifest.json"))
      if(!identical(hm$effective_case_classifications_sha256,
                    sha(abstract_files[[cohort]])))
        stop("W08 human adjudication checksum changed.")
    }
    model<-jl(file.path(d,"case_assessments.jsonl"))
    mid<-vapply(model,function(z)sc(z$record_id),character(1))
    if(anyDuplicated(mid)||any(!mid%in%selected))
      stop("W08 model source IDs invalid: ",cohort)
    # Recover the exact bibliographic source used in W08 abstract detection.
    canonical<-file.path(dirname(dirname(m$source_w04)),"W02_main",
                         "finalised","canonical.jsonl")
    if(!file.exists(canonical)||
       !identical(sha(canonical),m$canonical_w02_sha256))
      stop("W02 abstract provenance mismatch for ",cohort)
    canonical_rows<-jl(canonical)
    cid<-vapply(canonical_rows,function(z)sc(z$identity$record_id),
                 character(1))
    if(anyDuplicated(cid))stop("Duplicate W02 canonical IDs.")
    byid<-setNames(canonical_rows,cid)
    # Same corrected-abstract handling used by the portable W08 reviewer.
    corrections<-list()
    w04<-m$source_w04
    layer_dir<-if(identical(m$source_w04_layer,"human_authoritative"))
      "human_authoritative" else "finalised"
    sumfile<-file.path(w04,layer_dir,"summary.json")
    if(file.exists(sumfile)) {
      w04sum<-j(sumfile)
      if(isTRUE(w04sum$reconciled_abstracts)) {
        paths<-list.dirs(file.path(w04,"abstract_rescreens"),
                         recursive=FALSE,full.names=TRUE)
        paths<-paths[file.exists(file.path(paths,"manifest.json"))]
        if(length(paths)!=1L)
          stop("Cannot identify corrected W04 abstracts for ",cohort)
        csv<-file.path(paths[[1L]],"inputs","submitted_abstracts.csv")
        corrected<-utils::read.csv(csv,stringsAsFactors=FALSE,
                                   fileEncoding="UTF-8-BOM")
        if(anyDuplicated(corrected$record_id))
          stop("Duplicate corrected W04 abstract IDs.")
        corrections<-setNames(as.list(corrected$abstract),
                              corrected$record_id)
      }
    }
    r<-mechanism[[cohort]]
    meta<-j(file.path(r,"manifest.json"))
    if(!identical(meta$schema,"ostara-fulltext-mechanism-all-v4")||
       !startsWith(meta$prompt_sha256,"f52af5566e56")||
       !startsWith(meta$approved_ontology_sha256,"f980a7b995c8")||
       !identical(sha(file.path(r,"prompt.txt")),meta$prompt_sha256)||
       !identical(sha(file.path(r,"ontology.csv")),meta$approved_ontology_sha256))
      stop("Unapproved full-text mechanism run: ",r)
    fulltext<-jl(file.path(r,"mechanism_assessments.jsonl"))
    fid<-vapply(fulltext,function(z)sc(z$record_id),character(1))
    if(anyDuplicated(fid))stop("Duplicate full-text mechanism IDs.")
    if(any(!fid%in%selected))
      stop("Full-text inventory contains records outside W08 selection: ",
           paste(head(setdiff(fid,selected),5),collapse=", "))
    qpath<-file.path(meta$source_w09,"retrieval_queue.csv")
    if(!file.exists(qpath)||!identical(sha(qpath),meta$w09_queue_sha256))
      stop("W09 retrieval queue provenance mismatch: ",cohort)
    q<-utils::read.csv(qpath,stringsAsFactors=FALSE)
    if(anyDuplicated(q$record_id))stop("Duplicate W09 IDs.")
    sources[[cohort]]<-list(
      w08_manifest_sha256=sha(file.path(d,"manifest.json")),
      w08_classifications_sha256=sha(abstract_files[[cohort]]),
      mechanism_manifest_sha256=sha(file.path(r,"manifest.json")),
      mechanism_assessments_sha256=sha(file.path(r,"mechanism_assessments.jsonl")),
      source_w02_sha256=sha(canonical))
    for(z in fulltext) {
      ca<-z$case_presence_assessment
      if(!identical(sc(ca$case_status),"case_present"))next
      id<-sc(z$record_id)
      idx<-match(id,final$record_id)
      abstract_status<-if(!is.na(idx))sc(final$final_case_status[[idx]])
                       else "missing_final_W08_classification"
      model_idx<-match(id,mid)
      model_status<-if(!is.na(model_idx))
        sc(model[[model_idx]]$case_evidence_status) else ""
      if(abstract_status%in%c("specific_case","multiple_cases"))next
      if(!abstract_status%in%c("no_identifiable_case","unclear",
          "unvalidated_W08","missing_final_W08_classification"))
        stop("Invalid W08 status: ",abstract_status)
      b<-byid[[id]]
      if(is.null(b))stop("Original W02 abstract unavailable: ",id)
      ci<-match(id,q$record_id)
      if(is.na(ci))stop("No matching W09 queue record: ",id)
      zc<-b$canonical
      title<-sc(zc$title)
      if(!nzchar(title)||!nzchar(sc(zc$abstract))&&
         is.null(corrections[[id]])) {
        # Missing abstracts are displayed explicitly, never invented.
        if(!nzchar(title))stop("Missing canonical title for ",id)
      }
      doi<-sc(zc$doi)
      records[[length(records)+1L]]<-list(
        record_id=id,cohort=cohort,
        bibliographic=list(
          title=title,authors=sc(if(!is.null(zc$authors))zc$authors else zc$author),
          year=sc(zc$year),
          journal=sc(if(!is.null(zc$source_title))zc$source_title else zc$journal),
          keywords=sc(if(!is.null(zc$author_keywords))
                      zc$author_keywords else zc$keywords),
          abstract=sc(if(!is.null(corrections[[id]]))
                      corrections[[id]] else zc$abstract),
          abstract_source=if(!is.null(corrections[[id]]))
              "W04_reconciled_abstract" else "W02_canonical",
          doi=doi,doi_url=doi_link(doi),scholar_url=title_search(title)),
        w08_status=abstract_status,w08_model_status=model_status,
        fulltext_status="case_present",
        fulltext_assessment_reason=sc(ca$assessment_reason),
        candidate_case_count=as.integer(ca$candidate_case_count),
        candidate_cases=ca$candidate_cases,
        evidence_review_required=isTRUE(z$evidence_review_required),
        evidence_review_flags=z$evidence_review_flags,
        mechanism_manifest_sha256=sources[[cohort]]$mechanism_manifest_sha256)
    }
  }
  if(!length(records))stop("No additional full-text cases to review.")
  ids<-vapply(records,function(z)z$record_id,character(1))
  if(anyDuplicated(ids))stop("Duplicate publications across cohorts.")
  counts<-table(factor(vapply(records,function(z)z$w08_status,character(1)),
    levels=c("no_identifiable_case","unclear","unvalidated_W08",
             "missing_final_W08_classification")))
  message("[OSTARA] Additional case-positive publications: ",length(records),
    " (negative ",counts[[1]],", unclear ",counts[[2]],
    ", unvalidated ",counts[[3]],", missing final ",counts[[4]],").")
  if(length(records)!=279L||!identical(as.integer(counts),c(268L,8L,1L,2L)))
    stop("Selected runs do not reproduce the verified 279-publication ",
         "reconciliation. No ZIP created.")
  app<-system.file("ostara","new_case_review_app.R",
                   package="evidencelandscaper")
  if(!nzchar(app))stop("Packaged colleague-review Shiny app missing.")
  work<-tempfile("ostara-new-case-review-")
  on.exit(unlink(work,recursive=TRUE,force=TRUE),add=TRUE)
  bundle<-file.path(work,"ostara_new_case_review")
  dir.create(file.path(bundle,"data"),recursive=TRUE)
  queue<-file.path(bundle,"data","review_queue.jsonl")
  writeLines(vapply(records,function(z)
    as.character(jsonlite::toJSON(z,auto_unbox=TRUE,null="null")),
    character(1)),queue,useBytes=TRUE)
  if(!file.copy(app,file.path(bundle,"app.R")))
    stop("Could not stage Shiny application.")
  manifest<-list(schema="ostara-new-cases-review-bundle-v1",
    records=length(records),categories=as.list(as.integer(counts)),
    queue_sha256=sha(queue),cohort_provenance=sources,
    classification_options=as.list(c("has_case","does_not_have_case")),
    candidate_case_count=sum(vapply(records,function(z)
      z$candidate_case_count,integer(1))))
  names(manifest$categories)<-names(counts)
  jsonlite::write_json(manifest,file.path(bundle,"manifest.json"),
    pretty=TRUE,auto_unbox=TRUE)
  writeLines(c(
    "OSTARA: HUMAN REVIEW OF ADDITIONAL FULL-TEXT CASE DETECTIONS",
    "",
    "1. Unzip the package to a writable folder.",
    "2. In RStudio install packages once:",
    "   install.packages(c('shiny','jsonlite','digest'))",
    "3. Open app.R in ostara_new_case_review, then click Run App.",
    "   Alternatively use shiny::runApp('/path/to/ostara_new_case_review').",
    "4. The original abstract, bibliographic details, DOI link, Google Scholar",
    "   title-search link, full-text case candidates and evidence are included.",
    "5. Decide Has case or Does not have case for each publication and save.",
    "6. Export the adjudications JSON and return it to the project lead.",
    "",
    "The review runs locally; no API key, project files or GitHub access needed.",
    "Work is saved in decisions.jsonl and can be resumed in the same folder.",
    "Do not modify manifest.json or data/review_queue.jsonl."
  ),file.path(bundle,"README.txt"),useBytes=TRUE)
  previous<-getwd()
  on.exit(setwd(previous),add=TRUE)
  setwd(work)
  utils::zip(destination_zip,files=c(
    "ostara_new_case_review/app.R",
    "ostara_new_case_review/README.txt",
    "ostara_new_case_review/manifest.json",
    "ostara_new_case_review/data/review_queue.jsonl"),flags="-q")
  if(!file.exists(destination_zip)||file.info(destination_zip)$size<=0)
    stop("Could not create portable Shiny review ZIP.")
  message("[OSTARA] Colleague review ZIP: ",destination_zip)
  invisible(destination_zip)
}

#' Import binary Has case / Does not have case colleague adjudications
#'
#' Validates the returned JSON against a checksum-pinned copy of the exact
#' original review queue inside a locally preserved ZIP. Never modifies W08,
#' mechanism assessments or historical W09 case extraction.
#' @param adjudications_json Colleague-exported JSON.
#' @param review_zip The exact original portable ZIP sent to colleague.
#' @param map_path OSTARA map root; NULL uses default named map.
#' @return Immutable import snapshot directory, invisibly.
#' @export
import_ostara_new_case_review <- function(
    adjudications_json,review_zip,map_path=NULL) {
  if(is.null(map_path))map_path<-open_map("Ostara_collective_action_map")
  map_path<-normalizePath(map_path,winslash="/",mustWork=TRUE)
  adjudications_json<-normalizePath(adjudications_json,mustWork=TRUE)
  review_zip<-normalizePath(review_zip,mustWork=TRUE)
  t<-tempfile("ostara-new-case-import-")
  dir.create(t)
  on.exit(unlink(t,recursive=TRUE,force=TRUE),add=TRUE)
  utils::unzip(review_zip,exdir=t)
  root<-file.path(t,"ostara_new_case_review")
  meta<-jsonlite::fromJSON(file.path(root,"manifest.json"),
                           simplifyVector=FALSE)
  queue<-file.path(root,"data","review_queue.jsonl")
  sha<-function(p)digest::digest(file=p,algo="sha256",serialize=FALSE)
  if(!identical(meta$schema,"ostara-new-cases-review-bundle-v1")||
     !identical(sha(queue),meta$queue_sha256))
    stop("The original review ZIP has an invalid queue or manifest.")
  x<-jsonlite::fromJSON(adjudications_json,simplifyVector=FALSE)
  if(!identical(x$schema,"ostara-new-cases-adjudications-v1")||
     !identical(x$queue_sha256,meta$queue_sha256)||
     !is.list(x$decisions))
    stop("Colleague JSON does not match this review ZIP.")
  lines<-readLines(queue,warn=FALSE,encoding="UTF-8")
  q<-lapply(lines,jsonlite::fromJSON,simplifyVector=FALSE)
  ids<-vapply(q,function(z)z$record_id,character(1))
  if(anyDuplicated(ids))stop("Duplicate IDs in original review queue.")
  done<-vapply(x$decisions,function(z)as.character(z$record_id),character(1))
  if(anyDuplicated(done)||any(!done%in%ids))
    stop("Adjudication contains duplicates or non-queue record IDs.")
  rows<-lapply(x$decisions,function(z) {
    needed<-c("record_id","decision","reviewer","notes","timestamp_utc")
    if(!all(needed%in%names(z))||
       !identical(length(z$decision),1L)||
       !z$decision%in%c("has_case","does_not_have_case")||
       !nzchar(trimws(z$reviewer)))
      stop("Invalid adjudication for record: ",z$record_id)
    source<-q[[match(z$record_id,ids)]]
    data.frame(record_id=z$record_id,cohort=source$cohort,
      abstract_status=source$w08_status,decision=z$decision,
      reviewer=z$reviewer,notes=z$notes,timestamp_utc=z$timestamp_utc,
      stringsAsFactors=FALSE)
  })
  out<-if(length(rows))do.call(rbind,rows) else data.frame(
    record_id=character(),cohort=character(),abstract_status=character(),
    decision=character(),reviewer=character(),notes=character(),
    timestamp_utc=character())
  dest<-file.path(map_path,"runs","W09","new_fulltext_cases_human_review",
    "imports",paste0("v1-",substr(sha(adjudications_json),1,16)))
  if(dir.exists(dest)&&length(list.files(dest,all.files=TRUE,no..=TRUE)))
    stop("This import already exists or its destination is occupied: ",dest)
  dir.create(dest,recursive=TRUE,showWarnings=FALSE)
  file.copy(adjudications_json,file.path(dest,"adjudications.json"))
  utils::write.csv(out,file.path(dest,"all_decisions.csv"),row.names=FALSE)
  accepted<-out[out$decision=="has_case",,drop=FALSE]
  utils::write.csv(accepted,file.path(dest,"approved_w09_case_ids.csv"),
                   row.names=FALSE)
  jsonlite::write_json(list(schema="ostara-new-cases-adjudication-import-v1",
    original_review_zip_sha256=sha(review_zip),
    queue_sha256=meta$queue_sha256,adjudications_sha256=sha(adjudications_json),
    decisions_imported=nrow(out),accepted_publications=nrow(accepted)),
    file.path(dest,"manifest.json"),auto_unbox=TRUE,pretty=TRUE)
  message("[OSTARA] Imported ",nrow(out)," decisions; ",
          nrow(accepted)," accepted for full-text case-data extraction.")
  invisible(dest)
}
