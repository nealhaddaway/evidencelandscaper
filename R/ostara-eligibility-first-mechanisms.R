#' Prepare full-text mechanism review inputs from a new eligibility-first search
#'
#' Uses the approved, checksum-verified case eligibility registry and extracted
#' W09 cases from a named run. Does not fabricate a legacy W10 audit or modify
#' any source outputs. The worksheets created here are UNREVIEWED baselines.
#' @param run_name Existing named orchestrator run.
#' @param map Existing map name or path.
#' @return Directory containing manifest and initial worksheets, invisibly.
#' @export
prepare_ostara_mechanism_review_from_run <- function(
 run_name,map="Ostara_collective_action_map") {
 map<-open_map(map)
 state_path<-file.path(map,"runs","orchestrated",run_name,"state.json")
 if(!file.exists(state_path))stop("Named search state not found: ",run_name)
 state<-jsonlite::fromJSON(state_path,simplifyVector=FALSE)
 if(!state$status%in%c("extracted","published"))
  stop("Finish eligibility-first W09 case extraction before mechanism review")
 if(is.null(state$preextraction$run)||is.null(state$w09$extraction_dir)||
    is.null(state$w07$run))stop("Eligibility-first run state is incomplete")
 sha<-function(p)digest::digest(file=p,algo="sha256",serialize=FALSE)
 readjl<-function(f)lapply(Filter(nzchar,readLines(f,warn=FALSE,encoding="UTF-8")),
                            jsonlite::fromJSON,simplifyVector=FALSE)
 review_gate<-normalizePath(state$preextraction$run,mustWork=TRUE)
 extraction<-normalizePath(state$w09$extraction_dir,mustWork=TRUE)
 w09<-normalizePath(state$w09$run,mustWork=TRUE)
 w07<-normalizePath(state$w07$run,mustWork=TRUE)
 files<-c(
  gate=file.path(review_gate,"candidate_case_decisions.jsonl"),
  final=file.path(review_gate,"final_case_eligibility.csv"),
  final_manifest=file.path(review_gate,"final_review_manifest.json"),
  gate_manifest=file.path(review_gate,"manifest.json"),
  registry=file.path(extraction,"fulltext_case_registry_normalised.jsonl"),
  extraction_manifest=file.path(extraction,"manifest.json"),
  ontology=file.path(w07,"ontology.csv"),
  coding=file.path(w07,"coding.jsonl"))
 missing<-files[!file.exists(files)]
 if(length(missing))stop("Missing archived inputs: ",paste(missing,collapse=", "))
 gm<-jsonlite::fromJSON(files[["gate_manifest"]],simplifyVector=FALSE)
 fm<-jsonlite::fromJSON(files[["final_manifest"]],simplifyVector=FALSE)
 wm<-jsonlite::fromJSON(files[["extraction_manifest"]],simplifyVector=FALSE)
 if(!identical(gm$ledger_sha256,sha(files[["gate"]]))||
    !identical(normalizePath(gm$w09,mustWork=TRUE),w09)||
    !identical(fm$original_ledger_sha256,sha(files[["gate"]]))||
    !identical(fm$final_audit_sha256,sha(files[["final"]]))||
    !identical(wm$eligibility_ledger_sha256,sha(files[["final"]]))||
    !identical(normalizePath(wm$w07_run,mustWork=TRUE),w07)||
    !identical(wm$ontology_sha256,sha(files[["ontology"]]))||
    !identical(wm$coding_sha256,sha(files[["coding"]])))
   stop("Eligibility/W07/W09 provenance mismatch; refusing mechanism handover")
 if(!identical(state$preextraction$ledger_sha256,sha(files[["gate"]]))||
    !identical(state$preextraction$final_sha256,sha(files[["final"]])))
   stop("Named-run eligibility state no longer matches archived decisions")
 final<-utils::read.csv(files[["final"]],colClasses="character",
                        stringsAsFactors=FALSE,check.names=FALSE)
 required<-c("record_id","case_id","decision")
 if(!all(required%in%names(final))||
    anyNA(final[,required])||
    anyDuplicated(paste(final$record_id,final$case_id,sep="\r"))||
    any(!final$decision%in%c("eligible","ineligible")))
   stop("Final eligibility audit contains invalid or ambiguous case decisions")
 eligible<-final[final$decision=="eligible",c("record_id","case_id"),drop=FALSE]
 if(!nrow(eligible)||anyDuplicated(eligible$case_id))
  stop("No distinct human-approved cases for mechanism review")
 records<-readjl(files[["registry"]])
 ids<-vapply(records,function(z)z$record_id,character(1))
 if(anyDuplicated(ids)||!setequal(ids,unique(eligible$record_id)))
  stop("W09 extraction publications differ from approved eligibility")
 for(z in records) {
  expected<-eligible$case_id[eligible$record_id==z$record_id]
  actual<-vapply(z$cases,function(ca) {
   if(is.null(ca$eligibility_case_id))"" else as.character(ca$eligibility_case_id)
  },character(1))
  if(anyDuplicated(actual)||!setequal(actual,expected))
   stop("Extracted case IDs do not match approved eligibility for ",z$record_id)
 }
 coding<-readjl(files[["coding"]])
 cids<-vapply(coding,function(z)z$record_id,character(1))
 if(anyDuplicated(cids)||any(!ids%in%cids))
  stop("Approved W09 publications missing W07 coding")
 # Initial, UNREVIEWED W07 assignments for the supplementary review's
 # abstract comparison; not a claim of confirmed full-text implementation.
 initial<-do.call(rbind,lapply(coding[cids%in%ids],function(z){
   codes<-if(length(z$mechanisms))vapply(z$mechanisms,
     function(m)as.character(m$mechanism_id),character(1)) else character()
   if(!length(codes))return(NULL)
   data.frame(record_id=rep(z$record_id,length(codes)),
              mechanism_id=codes,abstract_assigned=TRUE,
              human_decision="",review_complete=FALSE,
              stringsAsFactors=FALSE)
 }))
 if(is.null(initial))initial<-data.frame(record_id=character(),
    mechanism_id=character(),abstract_assigned=logical(),
    human_decision=character(),review_complete=logical())
 novelty<-data.frame(record_id=character(),candidate_id=character(),
   case_id=character(),w07_candidate_id=character(),
   human_novelty_decision=character(),proposed_label=character(),
   working_definition=character(),evidence_quote=character(),
   source_anchor=character(),reviewer=character(),review_timestamp=character(),
   stringsAsFactors=FALSE)
 key<-substr(digest::digest(c(sha(files[["registry"]]),sha(files[["final"]]),
   sha(files[["coding"]])),algo="sha256"),1L,12L)
 output<-file.path(map,"reports","mechanism_review",
   paste0("eligibility-first-",run_name,"-",key))
 man<-list(schema="ostara-mechanism-review-eligibility-first-v1",
   mode="eligibility_first",run_name=run_name,w09=w09,
   extraction_dir=extraction,w07_run=w07,
   w09_registry_sha256=sha(files[["registry"]]),
   eligibility_ledger_sha256=sha(files[["gate"]]),
   eligibility_final_sha256=sha(files[["final"]]),
   w07_ontology_sha256=sha(files[["ontology"]]),
   w07_coding_sha256=sha(files[["coding"]]),
   eligible_case_ids=as.list(as.character(eligible$case_id)))
 manifest_path<-file.path(output,"manifest.json")
 if(file.exists(manifest_path)) {
  old<-jsonlite::fromJSON(manifest_path,simplifyVector=FALSE)
  if(!identical(old,man))stop("Existing mechanism review provenance differs")
  return(invisible(output))
 }
 if(dir.exists(output)&&length(list.files(output,all.files=TRUE,no..=TRUE)))
  stop("Mechanism review output already exists without manifest")
 dir.create(output,recursive=TRUE,showWarnings=FALSE)
 utils::write.csv(initial,file.path(output,"mechanism_review.csv"),row.names=FALSE)
 utils::write.csv(novelty,file.path(output,"novelty_review.csv"),row.names=FALSE)
 jsonlite::write_json(man,manifest_path,auto_unbox=TRUE,pretty=TRUE)
 message("Created UNREVIEWED mechanism baseline for ",length(ids),
         " publications and ",nrow(eligible)," approved cases: ",output)
 invisible(output)
}
