#' Export the full-text OSTARA mechanism explorer
#'
#' Creates a versioned, self-contained HTML site from the completed client
#' database export. No new extraction calls. Preserves the approved 30-code
#' ontology, publication-level status and source-anchored evidence. This
#' exporter does not publish to the collectiveactionmap website.
#'
#' @param map_path OSTARA project root.
#' @param client_export_dir Existing output of export_ostara_client_databases.
#'   NULL selects the most recently modified complete export.
#' @param output_dir Optional new/empty output directory.
#' @return Path to generated typology.html, invisibly.
#' @export
export_ostara_fulltext_mechanism_explorer <- function(
    map_path=NULL,client_export_dir=NULL,output_dir=NULL) {
  if(is.null(map_path))map_path<-open_map("Ostara_collective_action_map")
  map_path<-normalizePath(map_path,winslash="/",mustWork=TRUE)
  if(!requireNamespace("jsonlite",quietly=TRUE)||
     !requireNamespace("digest",quietly=TRUE))
    stop("Install jsonlite and digest.")
  names_needed<-c(
    "ostara_fulltext_mechanisms_with_adjacent_evidence.csv",
    "ostara_candidate_mechanisms_source.csv",
    "ostara_publications.csv",
    "ostara_export_source_manifest.csv")
  if(is.null(client_export_dir)) {
    root<-file.path(map_path,"reports","client_databases")
    if(!dir.exists(root))stop("No client database exports: ",root)
    candidates<-list.dirs(root,recursive=FALSE,full.names=TRUE)
    candidates<-candidates[vapply(candidates,function(d)
      all(file.exists(file.path(d,names_needed))),logical(1))]
    if(!length(candidates))stop("No complete client database export found.")
    client_export_dir<-candidates[[which.max(file.info(candidates)$mtime)]]
  }
  client_export_dir<-normalizePath(client_export_dir,winslash="/",mustWork=TRUE)
  files<-file.path(client_export_dir,names_needed)
  if(any(!file.exists(files)))
    stop("Missing required client database files: ",
         paste(names_needed[!file.exists(files)],collapse=", "))
  read_csv<-function(p)utils::read.csv(p,stringsAsFactors=FALSE,
     check.names=FALSE,fileEncoding="UTF-8-BOM",na.strings=NULL)
  inv<-read_csv(files[[1L]])
  candidates<-read_csv(files[[2L]])
  pubs<-read_csv(files[[3L]])
  provenance<-read_csv(files[[4L]])
  expected<-c("record_id","cohort","mechanism_id",
    "collective_action_type","high_level_mechanism",
    "fine_level_mechanism","definition","status",
    "evidence_quote","evidence_anchor","evidence_review_required","source_run")
  if(!all(expected%in%names(inv))||!nrow(inv))
    stop("Mechanism inventory missing canonical columns or rows.")
  if(anyDuplicated(paste(inv$record_id,inv$mechanism_id,sep="\u001f")))
    stop("Duplicate publication-by-code full-text assessments.")
  allowed<-c("implemented_case","mentioned_only","proposed_only",
             "not_identified","unclear")
  if(any(!inv$status%in%allowed))
    stop("Unrecognised full-text mechanism status.")
  ontcols<-c("mechanism_id","collective_action_type",
             "high_level_mechanism","fine_level_mechanism","definition")
  ontology<-unique(inv[,ontcols,drop=FALSE])
  if(nrow(ontology)!=30L||anyDuplicated(ontology$mechanism_id)||
     any(!grepl("^M[0-9]+\\.[0-9]+\\.[0-9]+$",ontology$mechanism_id)))
    stop("Expected exactly 30 unique M#.#.# ontology codes.")
  if(!all(c("record_id","cohort","title","doi")%in%names(pubs))||
     anyDuplicated(pubs$record_id))
    stop("Invalid publication register.")
  if(any(!inv$record_id%in%pubs$record_id))
    stop("Mechanism record IDs missing from publication register.")
  if(any(inv$cohort!=pubs$cohort[match(inv$record_id,pubs$record_id)]))
    stop("Mechanism and publication cohorts disagree.")
  # Every completed full-text inventory must have precisely the same 30
  # approved mechanism codes, including not_identified assessments.
  ids<-unique(inv$record_id)
  for(id in ids) {
    codes<-inv$mechanism_id[inv$record_id==id]
    if(length(codes)!=30L||!setequal(codes,ontology$mechanism_id))
      stop("Incomplete full-text inventory (not 30 codes): ",id)
  }
  candidate_columns<-c("record_id","cohort","source_stage",
    "proposed_label","working_definition","closest_existing_mechanism_id",
    "support_status","evidence_quote","evidence_anchor")
  if(!all(candidate_columns%in%names(candidates)))
    stop("Candidate proposal source export missing required columns.")
  ft_candidates<-candidates[candidates$source_stage=="full_text",
                            candidate_columns,drop=FALSE]
  if(any(!ft_candidates$record_id%in%ids))
    stop("Full-text candidate proposals include unassessed record IDs.")
  if(!all(ft_candidates$support_status%in%c("implemented","proposed","unclear")))
    stop("Unrecognised full-text novelty support status.")
  # Provenance must cover both approved mechanisms ledgers.
  mechanism_prov<-provenance[
    provenance$table=="mechanisms_and_ft_candidates",,drop=FALSE]
  if(nrow(mechanism_prov)!=2L||
     !all(c("source_file","sha256")%in%names(mechanism_prov)))
    stop("Expected two authoritative full-text mechanism sources in manifest.")
  plain<-function(df) {
    unname(lapply(seq_len(nrow(df)),function(i)
      as.list(setNames(as.character(unlist(df[i,,drop=FALSE],
                                     use.names=FALSE)),names(df)))))
  }
  payload<-list(schema="ostara-fulltext-mechanism-explorer-v1",
    ontology=plain(ontology),
    assessments=plain(inv[,c("record_id","cohort","mechanism_id","status",
          "evidence_quote","evidence_anchor","evidence_review_required"),
          drop=FALSE]),
    candidates=plain(ft_candidates),
    publications=plain(pubs[,c("record_id","cohort","title","doi"),drop=FALSE]),
    provenance=plain(mechanism_prov[,c("source_file","sha256"),drop=FALSE]))
  tpl<-system.file("ostara","fulltext_mechanism_explorer_template.html",
                   package="evidencelandscaper")
  if(!nzchar(tpl)||!file.exists(tpl))
    stop("Package missing full-text mechanism explorer template.")
  html<-paste(readLines(tpl,warn=FALSE,encoding="UTF-8"),collapse="\n")
  token<-"__OSTARA_MECHANISM_DATA__"
  pieces<-strsplit(html,token,fixed=TRUE)[[1L]]
  if(length(pieces)!=2L)stop("Missing or repeated HTML data placeholder.")
  data<-as.character(jsonlite::toJSON(payload,auto_unbox=TRUE,
    null="null",digits=NA))
  data<-gsub("</script","<\\/script",data,fixed=TRUE)
  html<-paste(pieces,collapse=data)
  sha<-function(p)digest::digest(file=p,algo="sha256",serialize=FALSE)
  if(is.null(output_dir)) {
    hash<-substr(digest::digest(c(vapply(files,sha,character(1)),
                               sha(tpl)),algo="sha256"),1L,12L)
    output_dir<-file.path(map_path,"reports","fulltext_mechanism_explorer",
                          hash)
  }
  file<-file.path(output_dir,"typology.html")
  if(dir.exists(output_dir)&&length(list.files(output_dir,
                                               all.files=TRUE,no..=TRUE))) {
    if(file.exists(file)&&identical(
      paste(readLines(file,warn=FALSE,encoding="UTF-8"),collapse="\n"),html)) {
      message("[OSTARA mechanism explorer] Existing identical build: ",file)
      return(invisible(file))
    }
    stop("Output directory is not empty and differs from this build: ",
         output_dir)
  }
  if(!dir.exists(output_dir)&&
     !dir.create(output_dir,recursive=TRUE,showWarnings=FALSE))
    stop("Cannot create output directory: ",output_dir)
  writeLines(html,file,useBytes=TRUE)
  status_counts<-as.data.frame(table(inv$status),stringsAsFactors=FALSE)
  names(status_counts)<-c("status","publication_code_assessments")
  utils::write.csv(status_counts,file.path(output_dir,"status_counts.csv"),
                   row.names=FALSE,na="")
  jsonlite::write_json(list(
    schema=payload$schema,source_export_dir=client_export_dir,
    source_files=as.list(stats::setNames(vapply(files,sha,character(1)),
                                         basename(files))),
    assessed_publications=length(ids),approved_codes=nrow(ontology),
    fulltext_candidate_proposals=nrow(ft_candidates),
    output_sha256=sha(file),
    note=paste("Publication-level assessment, not per-case mechanism linkage.",
               "W07 candidate suggestions excluded; only full-text",
               "novelty proposals shown. No live deployment made.")),
    file.path(output_dir,"provenance.json"),pretty=TRUE,auto_unbox=TRUE)
  message("[OSTARA mechanism explorer] ",length(ids),
    " full texts assessed; ",nrow(ontology)," codes; ",
    nrow(ft_candidates)," novel full-text suggestions: ",file)
  invisible(file)
}
