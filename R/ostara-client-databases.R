#' Export linked OSTARA client evidence tables with adjacent evidence columns
#'
#' Reads completed local checkpoints only; performs no model calls. Exports
#' source-level candidate mechanisms without pretending they are harmonised.
#' Does not change original results, classifications or source files.
#' @param map_path OSTARA root; NULL resolves the named map.
#' @param output_dir New/empty export destination.
#' @return Named exported file paths, invisibly.
#' @export
export_ostara_client_databases <- function(map_path=NULL,output_dir=NULL) {
  if(is.null(map_path))map_path<-open_map("Ostara_collective_action_map")
  map_path<-normalizePath(map_path,winslash="/",mustWork=TRUE)
  root<-file.path(map_path,"runs")
  if(is.null(output_dir))output_dir<-file.path(map_path,"reports",
    "client_databases",paste0("export-",format(Sys.time(),"%Y%m%d-%H%M%S")))
  if(dir.exists(output_dir)&&length(list.files(output_dir,all.files=TRUE,no..=TRUE)))
    stop("Export directory is not empty: ",output_dir)
  if(!requireNamespace("jsonlite",quietly=TRUE)||
     !requireNamespace("digest",quietly=TRUE))
    stop("Install jsonlite and digest.")
  sha<-function(x)digest::digest(file=x,algo="sha256",serialize=FALSE)
  j<-function(p)jsonlite::fromJSON(p,simplifyVector=FALSE)
  jl<-function(p) {
    if(!file.exists(p))return(list())
    l<-readLines(p,warn=FALSE,encoding="UTF-8")
    lapply(l[nzchar(trimws(l))],jsonlite::fromJSON,simplifyVector=FALSE)
  }
  sc<-function(x) {
    if(is.null(x)||!length(x))return("")
    y<-unlist(x,use.names=FALSE)
    if(!length(y))return("")
    paste(as.character(y[!is.na(y)]),collapse=" | ")
  }
  empty<-function(columns)as.data.frame(setNames(
    replicate(length(columns),character(),simplify=FALSE),columns),
    stringsAsFactors=FALSE,check.names=FALSE)
  rows_df<-function(rows,columns) {
    if(!length(rows))return(empty(columns))
    m<-do.call(rbind,lapply(rows,function(x) {
      out<-setNames(rep("",length(columns)),columns)
      out[names(x)]<-as.character(x)
      unname(out)
    }))
    df<-as.data.frame(m,stringsAsFactors=FALSE,check.names=FALSE)
    names(df)<-columns
    df
  }
  writeout<-function(name,data) {
    p<-file.path(output_dir,name)
    utils::write.csv(data,p,row.names=FALSE,na="",fileEncoding="UTF-8")
    p
  }
  paths<-list()
  source_rows<-list()
  note_source<-function(table,file,records) {
    source_rows[[length(source_rows)+1L]]<<-c(
      table=table,source_file=file,sha256=sha(file),records=as.character(records))
  }
  # Publication register: the two complete W09 included queues.
  publication_rows<-list()
  queue_dirs<-list.dirs(file.path(root,"W09"),recursive=FALSE,full.names=TRUE)
  queues<-list()
  for(cohort in c("first10","remaining90")) {
    matches<-queue_dirs[startsWith(basename(queue_dirs),
      paste0(cohort,"-all-included-"))&
      file.exists(file.path(queue_dirs,"retrieval_queue.csv"))]
    if(length(matches)!=1L)
      stop("Expected one authoritative ",cohort,
           " all-included W09 queue; found ",length(matches))
    queues[[cohort]]<-matches[[1L]]
    qp<-file.path(matches[[1L]],"retrieval_queue.csv")
    q<-utils::read.csv(qp,stringsAsFactors=FALSE,check.names=FALSE)
    if(anyDuplicated(q$record_id)||!all(c("record_id","title","doi")%in%names(q)))
      stop("Invalid W09 queue: ",qp)
    for(i in seq_len(nrow(q)))
      publication_rows[[length(publication_rows)+1L]]<-c(
        record_id=as.character(q$record_id[[i]]),
        cohort=cohort,title=sc(q$title[[i]]),doi=sc(q$doi[[i]]))
    note_source("publications",qp,nrow(q))
  }
  publications<-rows_df(publication_rows,c("record_id","cohort","title","doi"))
  if(anyDuplicated(publications$record_id))
    stop("Publication record IDs overlap between W09 cohorts.")
  # FULL-TEXT MECHANISM INVENTORY: approved, pinned current runs.
  mechanism_dirs<-list(
    first10=file.path(root,"W09","ft-mechanism-stage-first10",
       "fulltext_mechanism_inventory","v4-566b51e0959d"),
    remaining90=file.path(root,"W09","ft-mechanism-stage-remaining90",
       "fulltext_mechanism_inventory","v4-4a462b89bacb"))
  mechanism_rows<-list()
  novelty_rows<-list()
  for(cohort in names(mechanism_dirs)) {
    d<-mechanism_dirs[[cohort]]
    mp<-file.path(d,"manifest.json")
    ledger<-file.path(d,"mechanism_assessments.jsonl")
    ontology_path<-file.path(d,"ontology.csv")
    if(!all(file.exists(c(mp,ledger,ontology_path))))
      stop("Missing approved mechanism inventory: ",d)
    m<-j(mp)
    if(!identical(m$schema,"ostara-fulltext-mechanism-all-v4")||
       !startsWith(sc(m$prompt_sha256),"f52af5566e56")||
       !startsWith(sc(m$approved_ontology_sha256),"f980a7b995c8")||
       !identical(sha(ontology_path),sc(m$approved_ontology_sha256)))
      stop("Mechanism inventory provenance mismatch: ",d)
    ont<-utils::read.csv(ontology_path,stringsAsFactors=FALSE,
      check.names=FALSE,fileEncoding="UTF-8-BOM")
    if(!all(c("mechanism_id","collective_action_type",
       "high_level_mechanism","fine_level_mechanism","definition")%in%names(ont))||
       anyDuplicated(ont$mechanism_id))
      stop("Invalid saved mechanism ontology: ",d)
    bycode<-split(ont,ont$mechanism_id)
    assessments<-jl(ledger)
    ids<-vapply(assessments,function(z)sc(z$record_id),character(1))
    if(anyDuplicated(ids)||any(!ids%in%publications$record_id)||
       any(publications$cohort[match(ids,publications$record_id)]!=cohort))
      stop("Invalid or cross-cohort FT mechanism publication IDs: ",d)
    for(z in assessments) {
      id<-sc(z$record_id)
      for(v in z$mechanism_inventory) {
        code<-sc(v$mechanism_id)
        o<-bycode[[code]]
        if(is.null(o))stop("Unknown ontology code ",code)
        mechanism_rows[[length(mechanism_rows)+1L]]<-c(
          record_id=id,cohort=cohort,mechanism_id=code,
          collective_action_type=sc(o$collective_action_type[[1L]]),
          high_level_mechanism=sc(o$high_level_mechanism[[1L]]),
          fine_level_mechanism=sc(o$fine_level_mechanism[[1L]]),
          definition=sc(o$definition[[1L]]),
          status=sc(v$status),evidence_quote=sc(v$source_quote),
          evidence_anchor=sc(v$source_anchor),
          evidence_review_required=as.character(isTRUE(z$evidence_review_required)),
          source_run=basename(d))
      }
      for(i in seq_along(z$new_novelty)) {
        v<-z$new_novelty[[i]]
        novelty_rows[[length(novelty_rows)+1L]]<-c(
          candidate_id=paste0("FT-",id,"-",sprintf("%03d",i)),
          record_id=id,cohort=cohort,source_stage="full_text",
          proposed_label=sc(v$proposed_label),
          working_definition=sc(v$working_definition),
          proposed_reason=sc(v$working_definition),
          closest_existing_mechanism_id=sc(v$closest_approved_mechanism_id),
          support_status=sc(v$support_status),
          evidence_quote=sc(v$source_quote),
          evidence_anchor=sc(v$source_anchor),
          review_status="not_harmonised_in_this_export",
          source_run=basename(d))
      }
    }
    note_source("mechanisms_and_ft_candidates",ledger,length(assessments))
  }
  mechanism_cols<-c("record_id","cohort","mechanism_id",
    "collective_action_type","high_level_mechanism","fine_level_mechanism",
    "definition","status","evidence_quote","evidence_anchor",
    "evidence_review_required","source_run")
  mechanisms<-rows_df(mechanism_rows,mechanism_cols)
  if(anyDuplicated(paste(mechanisms$record_id,mechanisms$mechanism_id)))
    stop("Duplicate publication/mechanism combinations.")
  # INSIGHTS: select one verified v2 run per cohort. If there are multiple,
  # choose latest ledger but retain exact ledger/manifest provenance.
  insights_rows<-list()
  insight_status_rows<-list()
  for(cohort in c("first10","remaining90")) {
    base<-file.path(root,"W09",paste0("ft-insights-stage-",cohort),
                    "fulltext_insights_v2")
    ds<-if(dir.exists(base))list.dirs(base,recursive=FALSE,full.names=TRUE)
        else character()
    ds<-ds[file.exists(file.path(ds,"manifest.json"))&
           file.exists(file.path(ds,"insights_assessments.jsonl"))]
    if(!length(ds))stop("No completed revised Insights v2 run: ",cohort)
    if(length(ds)>1L)
      ds<-ds[[which.max(file.info(file.path(ds,
        "insights_assessments.jsonl"))$mtime)]]
    d<-ds[[1L]]
    m<-j(file.path(d,"manifest.json"))
    if(!identical(m$schema,"ostara-fulltext-insights-v2"))
      stop("Unexpected Insights checkpoint schema: ",d)
    rs<-jl(file.path(d,"insights_assessments.jsonl"))
    ids<-vapply(rs,function(z)sc(z$record_id),character(1))
    if(anyDuplicated(ids)||any(!ids%in%publications$record_id)||
       any(publications$cohort[match(ids,publications$record_id)]!=cohort))
      stop("Invalid or cross-cohort Insights record IDs: ",d)
    for(z in rs) {
      id<-sc(z$record_id)
      insight_status_rows[[length(insight_status_rows)+1L]]<-c(
        record_id=id,cohort=cohort,insights_status=sc(z$status),
        insight_count=as.character(length(z$insights)))
      for(v in z$insights) {
        insights_rows[[length(insights_rows)+1L]]<-c(
          record_id=id,cohort=cohort,insight_id=sc(v$insight_id),
          category=sc(v$category),insight_summary=sc(v$insight_summary),
          evidence_quote=sc(v$source_quote),
          evidence_anchor=sc(v$source_anchor),
          insight_subtype=sc(v$insight_subtype),
          evidence_basis=sc(v$evidence_basis),
          support_scope=sc(v$support_scope),
          context=sc(v$context),mechanism_ids=sc(v$mechanism_ids),
          caveats=sc(v$caveats),
          evidence_review_required=as.character(isTRUE(z$evidence_review_required)),
          source_run=basename(d))
      }
    }
    note_source("insights",file.path(d,"insights_assessments.jsonl"),length(rs))
  }
  insight_cols<-c("record_id","cohort","insight_id","category",
    "insight_summary","evidence_quote","evidence_anchor",
    "insight_subtype","evidence_basis","support_scope","context",
    "mechanism_ids","caveats","evidence_review_required","source_run")
  insights<-rows_df(insights_rows,insight_cols)
  if(anyDuplicated(insights$insight_id))stop("Duplicate Insights IDs.")
  # CASE DATA: all CSV-schema extraction ledgers; per publication choose the
  # most recent checkpoint. Preserve exact source selection in provenance.
  cf<-list.files(file.path(root,"W09"),pattern="^case_data\\.jsonl$",
                 recursive=TRUE,full.names=TRUE)
  cf<-cf[file.exists(file.path(dirname(cf),"field_mapping.csv"))&
         file.exists(file.path(dirname(cf),"manifest.json"))]
  if(!length(cf))stop("No W09 CSV-schema case extraction checkpoints.")
  eligible<-vapply(cf,function(p) {
    m<-tryCatch(j(file.path(dirname(p),"manifest.json")),error=function(e)NULL)
    !is.null(m)&&identical(m$schema,"ostara-w09-csv-case-data-extraction-v1")
  },logical(1))
  cf<-cf[eligible]
  if(!length(cf))stop("No compatible W09 CSV-schema case checkpoints.")
  cf<-cf[order(file.info(cf)$mtime,decreasing=TRUE)]
  case_sources<-list()
  for(p in cf) {
    source<-j(file.path(dirname(p),"manifest.json"))
    spec<-utils::read.csv(file.path(dirname(p),"field_mapping.csv"),
      check.names=FALSE,stringsAsFactors=FALSE,fileEncoding="UTF-8-BOM")
    if(!all(c("field_id","Question")%in%names(spec)))
      stop("Invalid field mapping for ",p)
    if(anyDuplicated(spec$field_id))stop("Duplicate case field IDs in ",p)
    records<-jl(p)
    ids<-vapply(records,function(z)sc(z$record_id),character(1))
    if(anyDuplicated(ids))stop("Duplicate extracted publications: ",p)
    case_sources[[p]]<-list(manifest=source,spec=spec,records=records)
    note_source("cases",p,length(records))
  }
  # Field keys are field_id plus exact question, preventing silent merging of
  # different questions that occupy the same ordinal field ID.
  allkeys<-unique(unlist(lapply(case_sources,function(s)
    paste(s$spec$field_id,s$spec$Question,sep=" :: ")),use.names=FALSE))
  ccols<-c("record_id","cohort","case_id","assessment_reason",
    "case_boundary_uncertainty","source_extraction_run")
  for(key in allkeys)
    ccols<-c(ccols,key,paste0(key," [evidence]"),
             paste0(key," [reporting status]"))
  case_rows<-list()
  case_status_rows<-list()
  chosen_ids<-character()
  for(p in names(case_sources)) {
    s<-case_sources[[p]]
    spec<-s$spec
    for(z in s$records) {
      id<-sc(z$record_id)
      if(id%in%chosen_ids)next
      if(!id%in%publications$record_id)next
      chosen_ids<-c(chosen_ids,id)
      cohort<-publications$cohort[[match(id,publications$record_id)]]
      case_status_rows[[length(case_status_rows)+1L]]<-c(
        record_id=id,cohort=cohort,
        case_extraction_status=sc(z$publication_extraction_status),
        extracted_case_count=as.character(length(z$cases)),
        case_source_run=p)
      for(v in z$cases) {
        row<-c(record_id=id,cohort=cohort,case_id=sc(v$case_id),
          assessment_reason=sc(v$assessment_reason),
          case_boundary_uncertainty=sc(v$case_boundary_uncertainty),
          source_extraction_run=p)
        for(i in seq_len(nrow(spec))) {
          key<-paste(spec$field_id[[i]],spec$Question[[i]],sep=" :: ")
          f<-v$fields[[spec$field_id[[i]]]]
          if(is.null(f))stop("Missing case field ",key," in ",id)
          value<-if(!is.null(f$values))sc(f$values) else sc(f$value)
          evidence<-if(length(f$evidence))
            paste(vapply(f$evidence,function(e)
              paste0(sc(e$anchor),": ",sc(e$quote)),character(1)),
              collapse="\n") else ""
          row[[key]]<-value
          row[[paste0(key," [evidence]")]]<-evidence
          row[[paste0(key," [reporting status]")]]<-sc(f$reporting_status)
        }
        case_rows[[length(case_rows)+1L]]<-row
      }
    }
  }
  cases<-rows_df(case_rows,ccols)
  if(anyDuplicated(cases$case_id))
    stop("Duplicate case IDs after checkpoint consolidation.")
  # W07 candidate proposals: archived full-study coding runs, not pilots.
  # Keep original source-level rows and preserve duplicate hypotheses.
  candidate_cols<-c("candidate_id","record_id","cohort","source_stage",
    "proposed_label","working_definition","proposed_reason",
    "closest_existing_mechanism_id","support_status",
    "evidence_quote","evidence_anchor","review_status","source_run")
  w07_root<-file.path(root,"W07")
  w07_candidates<-list()
  if(dir.exists(w07_root)) {
    full_dirs<-list.dirs(w07_root,recursive=FALSE,full.names=TRUE)
    full_dirs<-full_dirs[startsWith(basename(full_dirs),"full-")&
      file.exists(file.path(full_dirs,"coding.jsonl"))]
    # Use one latest ledger for each distinct selected-record population.
    if(length(full_dirs)) {
      sizes<-vapply(full_dirs,function(d) {
        mf<-file.path(d,"manifest.json")
        if(!file.exists(mf))return(NA_integer_)
        length(unlist(j(mf)$selected_record_ids,use.names=FALSE))
      },integer(1))
      full_dirs<-full_dirs[!is.na(sizes)]
      sizes<-sizes[!is.na(sizes)]
      chosen<-unlist(lapply(split(full_dirs,sizes),function(ds)
        ds[[which.max(file.info(file.path(ds,"coding.jsonl"))$mtime)]]),
        use.names=FALSE)
      for(d in chosen) {
        rs<-jl(file.path(d,"coding.jsonl"))
        for(z in rs) {
          if(!isTRUE(z$ontology_gap))next
          id<-sc(z$record_id)
          w07_candidates[[length(w07_candidates)+1L]]<-c(
            candidate_id=paste0("W07-",basename(d),"-",id),
            record_id=id,
            cohort=if(id%in%publications$record_id)
              publications$cohort[[match(id,publications$record_id)]] else "",
            source_stage="abstract",
            proposed_label=sc(z$proposed_new_mechanism),
            working_definition=sc(z$ontology_gap_reason),
            proposed_reason=sc(z$ontology_gap_reason),
            closest_existing_mechanism_id="",
            support_status=sc(z$initiative_stage),
            evidence_quote="",evidence_anchor="",
            review_status="not_harmonised_in_this_export",
            source_run=basename(d))
        }
        note_source("W07_candidates",file.path(d,"coding.jsonl"),length(rs))
      }
    }
  }
  candidates<-rows_df(c(w07_candidates,novelty_rows),candidate_cols)
  if(anyDuplicated(candidates$candidate_id))stop("Duplicate candidate IDs.")
  # Publication register receives explicit extraction statuses, including
  # publications for which an assessment is not yet present.
  add_status<-function(pub,table,cols) {
    idx<-match(pub$record_id,table$record_id)
    for(k in cols)pub[[k]]<-ifelse(is.na(idx),"not_assessed",table[[k]][idx])
    pub
  }
  publications<-add_status(publications,
    rows_df(case_status_rows,c("record_id","cohort","case_extraction_status",
                              "extracted_case_count","case_source_run")),
    c("case_extraction_status","extracted_case_count","case_source_run"))
  publications<-add_status(publications,
    rows_df(insight_status_rows,c("record_id","cohort","insights_status","insight_count")),
    c("insights_status","insight_count"))
  mech_ids<-unique(mechanisms$record_id)
  publications$mechanism_inventory_status<-ifelse(
    publications$record_id%in%mech_ids,"assessed","not_assessed")
  paths$publications<-writeout("ostara_publications.csv",publications)
  paths$cases<-writeout("ostara_cases_with_adjacent_evidence.csv",cases)
  paths$mechanisms<-writeout("ostara_fulltext_mechanisms_with_adjacent_evidence.csv",mechanisms)
  paths$insights<-writeout("ostara_fulltext_insights_with_adjacent_evidence.csv",insights)
  paths$candidates<-writeout("ostara_candidate_mechanisms_source.csv",candidates)
  # A review template, not a falsely completed harmonisation.
  harmonised<-candidates[,c("candidate_id","record_id","source_stage",
    "proposed_label","working_definition","evidence_quote","evidence_anchor"),drop=FALSE]
  harmonised$harmonised_concept_id<-""
  harmonised$harmonised_label<-""
  harmonised$harmonisation_decision<-"pending"
  harmonised$reviewer_notes<-""
  paths$harmonisation<-writeout(
    "ostara_candidate_mechanisms_harmonisation_review.csv",harmonised)
  paths$provenance<-writeout("ostara_export_source_manifest.csv",
    rows_df(source_rows,c("table","source_file","sha256","records")))
  summary<-data.frame(table=c("publications","cases","mechanisms","insights",
    "candidate_mechanisms","W09_case_publications","Insights_publications"),
    rows=c(nrow(publications),nrow(cases),nrow(mechanisms),nrow(insights),
      nrow(candidates),length(chosen_ids),length(insight_status_rows)),
    stringsAsFactors=FALSE)
  paths$summary<-writeout("ostara_export_summary.csv",summary)
  # Export the question mapping alongside the adjacent answer/evidence columns.
  case_field_rows<-unlist(lapply(case_sources,function(src)
    lapply(seq_len(nrow(src$spec)),function(i) {
      x<-src$spec[i,,drop=FALSE]
      c(field_id=sc(x$field_id),question=sc(x$Question),
        definition=if("Definition"%in%names(x))sc(x$Definition) else "",
        answer_form=if("Answer_form"%in%names(x))sc(x$Answer_form) else "",
        extraction_schema_sha256=sc(src$manifest$csv_sha256))
    })),recursive=FALSE)
  case_field_rows<-case_field_rows[!duplicated(vapply(case_field_rows,
    function(z)paste(z,collapse="\u001f"),character(1)))]
  paths$case_codebook<-writeout("ostara_case_field_codebook.csv",
    rows_df(case_field_rows,c("field_id","question","definition",
                              "answer_form","extraction_schema_sha256")))
  # A plain-language summary is supplied without altering source checkpoints.
  readme<-c(
    "OSTARA CLIENT EVIDENCE DATABASE EXPORT",
    "",
    "This is a snapshot of saved assessments; no new model calls were made.",
    "The following files use record_id to link to ostara_publications.csv.",
    "",
    "ostara_cases_with_adjacent_evidence.csv:",
    "  One row per case. Every field value is immediately followed by its",
    "  verbatim evidence quotation / source anchor, then reporting status.",
    "ostara_fulltext_mechanisms_with_adjacent_evidence.csv:",
    "  One row per publication and approved mechanism code; coded status is",
    "  immediately followed by evidence quotation and source anchor.",
    "ostara_fulltext_insights_with_adjacent_evidence.csv:",
    "  One row per insight; insight summary is immediately followed by evidence",
    "  quotation and source anchor.",
    "ostara_candidate_mechanisms_source.csv:",
    "  Source-level candidate proposals from saved W07 full runs and",
    "  the pinned full-text mechanism inventories, NOT yet harmonised.",
    "ostara_candidate_mechanisms_harmonisation_review.csv:",
    "  Review worksheet only; pending decisions are not final.",
    "ostara_publications.csv:",
    "  Linking register, including case and Insights assessment status.",
    "",
    "WARNING: A successful export does not guarantee all selected full texts",
    "have completed every assessment; inspect publication statuses.",
    "Case checkpoint selection: latest ledger per publication, including",
    "historical and supplementary extraction runs; inspect source_run.",
    "The candidate proposal inventory does not assert human harmonisation.",
    "Evidence flags in source JSONL are not automatically verified.",
    "No source full-text PDFs, XML or licensed abstracts are redistributed.",
    "See ostara_export_source_manifest.csv and README_provenance.json."
  )
  writeLines(readme,file.path(output_dir,"README.txt"),useBytes=TRUE)
  paths$readme<-file.path(output_dir,"README.txt")
  metadata<-list(schema="ostara-client-databases-export-v1",
    exported_at_utc=format(Sys.time(),"%Y-%m-%dT%H:%M:%SZ",tz="UTC"),
    source_stage="saved_checkpoint_exports_without_new_model_calls",
    caveats=list(
      mechanism_rows="30 ontology statuses per inventoried publication; not per-case frequencies",
      candidate_rows="Original source suggestions, NOT a human-harmonised ontology",
      case_deduplication="Latest case-data checkpoint per record_id, preserving original chosen source file in register",
      evidence_review="Evidence flags in source ledgers are not automatically human verified",
      document_copyright="No full-text PDFs/XML copied into this export",
      abstracts="Publication table contains title and DOI; no licensed abstracts"),
    files=lapply(paths,function(p)list(name=basename(p),sha256=sha(p))))
  jsonlite::write_json(metadata,file.path(output_dir,"README_provenance.json"),
                        pretty=TRUE,auto_unbox=TRUE,null="null")
  message("[OSTARA export] Saved ",length(paths)+1L,
          " files in: ",output_dir)
  invisible(c(paths,provenance_json=file.path(output_dir,"README_provenance.json")))
}
