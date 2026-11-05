#' Register the authoritative OSTARA cohort outputs without rewriting them
#'
#' The latest run is never inferred from modification time. Exact W08/W07
#' paths are supplied or resolved by checksum-verified lineage. The local
#' current pointer and immutable versioned snapshots live in the map, not in
#' the package repository. This is a registry, not a data-merging operation.
#'
#' @param map_path Exact OSTARA map root.
#' @param w08_10 Exact original-10% W08 run.
#' @param w08_90 Exact remaining-90% W08 run.
#' @param final_90_csv Final 2440-record classifications CSV.
#' @param w07_90 Exact revised 90% W07 run, whether running or complete.
#' @param replace_existing Explicitly promote a changed registry, archiving
#'   the previous snapshot. Default FALSE prevents silent replacement.
#' @return Path to the current registry JSON.
#' @export
register_ostara_canonical_runs <- function(map_path,w08_10,w08_90,
                                            final_90_csv,w07_90,
                                            replace_existing=FALSE) {
  if(!is.logical(replace_existing)||length(replace_existing)!=1L||
     is.na(replace_existing))stop("replace_existing must be TRUE or FALSE.")
  map_path<-normalizePath(map_path,winslash="/",mustWork=TRUE)
  norm<-function(x)normalizePath(x,winslash="/",mustWork=TRUE)
  w08_10<-norm(w08_10);w08_90<-norm(w08_90)
  w07_90<-norm(w07_90);final_90_csv<-norm(final_90_csv)
  sha<-function(f)digest::digest(file=f,algo="sha256",serialize=FALSE)
  load<-function(p)jsonlite::fromJSON(p,simplifyVector=FALSE)
  list_ids<-function(x)as.character(unlist(x,use.names=FALSE))
  readjl<-function(p){
    if(!file.exists(p))stop("Missing JSONL: ",p)
    l<-readLines(p,warn=FALSE,encoding="UTF-8")
    lapply(l[nzchar(trimws(l))],jsonlite::fromJSON,simplifyVector=FALSE)
  }
  validate_w08<-function(dir,expected){
    mf<-file.path(dir,"manifest.json")
    sf<-file.path(dir,"summary.json")
    ledger<-file.path(dir,"case_assessments.jsonl")
    if(!all(file.exists(c(mf,sf,ledger,file.path(dir,"prompt.txt")))))
      stop("Incomplete W08 source: ",dir)
    m<-load(mf);s<-load(sf)
    ids<-list_ids(m$selected_record_ids)
    if(length(ids)!=expected||anyDuplicated(ids))
      stop("W08 source does not match expected cohort: ",dir)
    if(!identical(s$responses_sha256,sha(ledger))||
       !identical(m$prompt_sha256,sha(file.path(dir,"prompt.txt"))))
      stop("W08 source checksum mismatch: ",dir)
    if(!s$status%in%c("PILOT_COMPLETE","PILOT_PARTIAL"))
      stop("W08 must have an archived completed or partial summary.")
    model<-readjl(ledger)
    mids<-vapply(model,function(z)as.character(z$record_id),character(1))
    if(anyDuplicated(mids)||any(!mids%in%ids))stop("Invalid W08 IDs.")
    missing<-setdiff(ids,mids)
    if(identical(s$status,"PILOT_COMPLETE")&&length(missing))
      stop("W08 completed summary has missing records.")
    if(identical(s$status,"PILOT_PARTIAL")&&
       (!setequal(missing,list_ids(s$unresolved_record_ids))||
        length(model)!=as.integer(s$validated_records)))
      stop("W08 missing record IDs conflict with partial summary.")
    w04<-norm(m$source_w04)
    layer<-get(".w04_latest_authoritative",
               envir=asNamespace("evidencelandscaper"))(w04)
    if(is.null(layer)||!isTRUE(layer$complete)||
       !identical(sha(layer$layer),m$source_w04_sha256))
      stop("W08 provenance does not match completed authoritative W04.")
    canonical<-file.path(dirname(dirname(w04)),
                         "W02_main","finalised","canonical.jsonl")
    if(!file.exists(canonical)||
       !identical(sha(canonical),m$canonical_w02_sha256))
      stop("W08 source does not match its W02 canonical file.")
    list(w08=dir,w08_manifest_sha256=sha(mf),
      w08_model_ledger_sha256=sha(ledger),
      w08_status=s$status,selected_records=length(ids),
      validated_records=length(mids),unvalidated_ids=as.list(missing),
      w04=w04,w04_authoritative_layer=layer$layer,
      w04_layer_sha256=sha(layer$layer),
      w02_canonical=canonical,w02_sha256=sha(canonical))
  }
  ten<-validate_w08(w08_10,266L)
  ninety<-validate_w08(w08_90,2440L)
  if(identical(ten$w04,ninety$w04))
    stop("The original 10% and remaining 90% W04 runs must differ.")
  # Require a real human-review snapshot for both cohorts.
  human<-function(w08){
    root<-file.path(w08,"human_adjudication")
    dirs<-if(dir.exists(root))
      list.dirs(root,recursive=FALSE,full.names=TRUE) else character()
    dirs<-dirs[file.exists(file.path(dirs,"manifest.json"))&
               file.exists(file.path(dirs,"effective_case_classifications.csv"))]
    if(length(dirs)!=1L)
      stop("Expected one human adjudication snapshot for ",w08,
           "; found ",length(dirs),". Do not label this cohort final yet.")
    m<-load(file.path(dirs,"manifest.json"))
    csv<-file.path(dirs,"effective_case_classifications.csv")
    if(!identical(m$effective_case_classifications_sha256,sha(csv)))
      stop("Human adjudication checksum changed: ",csv)
    list(snapshot=dirs,classification_csv=csv,sha256=sha(csv))
  }
  ten$human_adjudication<-human(w08_10)
  ninety$human_adjudication<-human(w08_90)
  final<-utils::read.csv(final_90_csv,stringsAsFactors=FALSE)
  if(!all(c("record_id","final_case_status")%in%names(final))||
     nrow(final)!=2440L||anyDuplicated(final$record_id))
    stop("The remaining-90% final classification CSV must have 2440 unique IDs.")
  if(!setequal(final$record_id,list_ids(load(file.path(w08_90,
                            "manifest.json"))$selected_record_ids)))
    stop("The final classifications do not cover the exact 90% cohort.")
  allowed<-c("specific_case","multiple_cases","no_identifiable_case",
             "unclear","unvalidated_W08")
  if(anyNA(final$final_case_status)||any(!final$final_case_status%in%allowed))
    stop("Unexpected final case classification.")
  effective<-utils::read.csv(
    ninety$human_adjudication$classification_csv,stringsAsFactors=FALSE)
  matched<-match(effective$record_id,final$record_id)
  if(anyNA(matched)||
     !identical(as.character(effective$final_case_status),
                as.character(final$final_case_status[matched]))||
     sum(final$final_case_status=="unvalidated_W08")!=length(ninety$unvalidated_ids)||
     !setequal(final$record_id[final$final_case_status=="unvalidated_W08"],
               unlist(ninety$unvalidated_ids,use.names=FALSE)))
    stop("Final 90% classifications conflict with human decisions or W08 exceptions.")
  ninety$final_case_classifications<-final_90_csv
  ninety$final_case_classifications_sha256<-sha(final_90_csv)
  ninety$case_status_counts<-as.list(table(final$final_case_status))
  # Lock the revised prompt/ontology to the exact 90% W07 run.
  w07mf<-file.path(w07_90,"manifest.json")
  ontology<-file.path(w07_90,"ontology.csv")
  prompt<-file.path(w07_90,"prompt.txt")
  ledger<-file.path(w07_90,"coding.jsonl")
  if(!all(file.exists(c(w07mf,ontology,prompt,ledger))))
    stop("Revised 90% W07 archive is incomplete.")
  wm<-load(w07mf)
  if(!identical(wm$source_w04,ninety$w04)||
     !identical(wm$source_w04_sha256,ninety$w04_layer_sha256)||
     !identical(wm$ontology_sha256,sha(ontology))||
     !identical(wm$prompt_sha256,sha(prompt))||
     length(list_ids(wm$selected_record_ids))!=2440L)
    stop("W07 lineage, prompt, ontology or cohort checksum mismatch.")
  w07ids<-vapply(readjl(ledger),function(z){
    if(!is.list(z)||!is.character(z$record_id)||length(z$record_id)!=1L)
      stop("W07 checkpoint has invalid record IDs.")
    z$record_id
  },character(1))
  if(anyDuplicated(w07ids)||
     any(!w07ids%in%list_ids(wm$selected_record_ids)))
    stop("W07 checkpoint ID mismatch.")
  w07summary<-file.path(w07_90,"summary.json")
  w07meta<-if(file.exists(w07summary))load(w07summary) else NULL
  w07complete<-!is.null(w07meta)&&
    identical(w07meta$status,"TRIAL_COMPLETE")&&
    length(w07ids)==2440L&&
    identical(w07meta$coding_sha256,sha(ledger))
  # Identify revised first-10% W07 by BOTH matching W04 and prompt/ontology.
  w07root<-file.path(map_path,"runs","W07")
  dirs<-list.dirs(w07root,recursive=FALSE,full.names=TRUE)
  possible<-Filter(function(p){
    f<-file.path(p,"manifest.json")
    if(!file.exists(f))return(FALSE)
    x<-tryCatch(load(f),error=function(e)NULL)
    !is.null(x)&&identical(x$source_w04,ten$w04)&&
      identical(x$ontology_sha256,wm$ontology_sha256)&&
      identical(x$prompt_sha256,wm$prompt_sha256)&&
      length(list_ids(x$selected_record_ids))==266L
  },dirs)
  if(length(possible)>1L)
    stop("Multiple revised first-10% W07 runs match. Resolve ambiguity.")
  ten_w07<-if(length(possible))possible[[1L]] else NULL
  selected_10<-if(!is.null(ten_w07)) {
    md<-load(file.path(ten_w07,"manifest.json"))
    cp<-file.path(ten_w07,"coding.jsonl")
    cm<-file.path(ten_w07,"summary.json")
    done<-if(file.exists(cp))readjl(cp) else list()
    complete<-file.exists(cm)&&identical(load(cm)$status,"TRIAL_COMPLETE")&&
      length(done)==266L&&
      identical(load(cm)$coding_sha256,sha(cp))
    list(run=ten_w07,completed_assessments=length(done),
         status=if(complete)"complete" else "designated_pending",
         manifest_sha256=sha(file.path(ten_w07,"manifest.json")))
  } else NULL
  registry<-list(schema="ostara-canonical-runs-v1",
    project="Ostara_collective_action_map",
    stage_statuses=list(
      search_bibliographic_preparation="final_attested_W02_sources_verified",
      w04_abstract_screening="final",
      w08_case_detection="human_adjudication_saved_with_explicit_exceptions",
      w07_mechanisms=if(w07complete)"final_complete" else "designated_final_in_progress"),
    cohorts=list(original_10_percent=ten,remaining_90_percent=ninety),
    revised_w07=list(remaining_90_percent=list(run=w07_90,
      manifest_sha256=sha(w07mf),ontology_sha256=sha(ontology),
      prompt_sha256=sha(prompt),validated_assessments=length(w07ids),
      target_assessments=2440L,
      status=if(w07complete)"complete" else "designated_pending",
      coding_sha256=sha(ledger)),
      original_10_percent=selected_10),
    limitations=list(
      search_status="User-approved final search outputs; W02 source hashes verified; W00, W01 and W03 outputs are not individually audited by this registry.",
      case_counts="Publication-level W08 classifications, not an individually reconciled final case inventory.",
      mechanism_status="Do not treat W07 as complete until an audited TRIAL_COMPLETE summary matches the coding ledger."))
  dest<-file.path(map_path,"canonical")
  dir.create(dest,recursive=TRUE,showWarnings=FALSE)
  # Content-addressed immutable snapshots, with a separate current pointer.
  encoded<-as.character(jsonlite::toJSON(registry,auto_unbox=TRUE,
                                          pretty=TRUE,null="null"))
  fingerprint<-digest::digest(encoded,algo="sha256",serialize=FALSE)
  snapshot<-file.path(dest,paste0("ostara-canonical-",substr(fingerprint,1,16),".json"))
  current<-file.path(dest,"canonical_runs.json")
  if(!file.exists(snapshot))writeLines(encoded,snapshot,useBytes=TRUE)
  if(file.exists(current)){
    old<-readLines(current,warn=FALSE,encoding="UTF-8")
    if(!identical(paste(old,collapse="\n"),paste(readLines(snapshot,warn=FALSE),
                                                  collapse="\n"))){
      if(!replace_existing)
        stop("A different canonical registry is already active. Snapshot saved: ",
             snapshot,". Set replace_existing=TRUE to promote explicitly.")
    }
  }
  if(!file.copy(snapshot,current,overwrite=TRUE))
    stop("Could not publish current canonical registry.")
  message("[OSTARA canonical] Registry: ",current)
  message("[OSTARA canonical] W04 and W08 adjudication saved for both cohorts.")
  message("[OSTARA canonical] W07 90%: ",length(w07ids),"/2440; ",
          if(w07complete)"complete" else "designated final, still running")
  invisible(current)
}

#' Read the current OSTARA canonical run registry
#' @param map_path Exact OSTARA map directory.
#' @return Verified JSON registry (list).
#' @export
read_ostara_canonical_runs <- function(map_path) {
  path<-file.path(normalizePath(map_path,mustWork=TRUE),"canonical",
                  "canonical_runs.json")
  if(!file.exists(path))stop("No canonical registry has been published.")
  jsonlite::fromJSON(path,simplifyVector=FALSE)
}
