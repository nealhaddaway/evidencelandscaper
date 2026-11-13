#' Extract all currently accessible, confirmed OSTARA case-positive full texts
#'
#' Reuses the LOCKED case pilot's archived prompt and schema verbatim; never
#' changes those files and never reruns validated records in the identical
#' prompt/schema/model W09 checkpoint. Each cohort has its own W09 checkpoint,
#' because source records and retrieval queue differ. Selection is snapshotted
#' before any paid API calls; rerun this function to resume.
#'
#' @param w09 Exact W09 collection with retrieval_queue.csv.
#' @param classifications_csv W08 human-adjudicated effective classifications.
#' @param pilot_dir Completed approved case_pilot_20_20261113 archive.
#' @param cohort_label Unique descriptive label, e.g. remaining90 or first10.
#' @param max_records Maximum pending publications this invocation (0 = all).
#' @param map_path Root of the OSTARA map.
#' @return Invisible summary with batch CSV paths and counts.
#' @export
run_ostara_locked_case_fulltext <- function(w09,classifications_csv,pilot_dir,
                                            cohort_label,max_records=0L,
                                            map_path=NULL) {
  norm<-function(p)normalizePath(p,winslash="/",mustWork=TRUE)
  sha<-function(p)digest::digest(file=p,algo="sha256",serialize=FALSE)
  readjl<-function(p){
    if(!file.exists(p))return(list())
    z<-readLines(p,warn=FALSE,encoding="UTF-8")
    lapply(z[nzchar(trimws(z))],jsonlite::fromJSON,simplifyVector=FALSE)
  }
  scalar<-function(x) {
    y<-as.character(unlist(x,use.names=FALSE))
    if(!length(y))return("")
    paste(y,collapse=" | ")
  }
  if(length(cohort_label)!=1L||is.na(cohort_label)||
     !grepl("^[a-zA-Z0-9_-]+$",cohort_label))
    stop("cohort_label must be a simple nonempty label.")
  if(length(max_records)!=1L||is.na(max_records)||max_records<0L||
     max_records!=as.integer(max_records))
    stop("max_records must be an integer >= 0 (0 means all).")
  w09<-norm(w09);classifications_csv<-norm(classifications_csv)
  pilot_dir<-norm(pilot_dir)
  if(is.null(map_path))map_path<-dirname(dirname(dirname(w09)))
  map_path<-norm(map_path)
  prompt<-norm(file.path(pilot_dir,"prompt.txt"))
  schema<-norm(file.path(pilot_dir,"case_schema.csv"))
  pilot_manifest<-norm(file.path(pilot_dir,"batch_manifest.json"))
  old<-jsonlite::fromJSON(pilot_manifest,simplifyVector=FALSE)
  if(!identical(old$staged_prompt_sha256,sha(prompt))||
     !identical(old$schema_sha256,sha(schema)))
    stop("LOCKED prompt/schema hashes disagree with the approved pilot manifest.")
  spec<-utils::read.csv(schema,stringsAsFactors=FALSE,check.names=FALSE,
                         fileEncoding="UTF-8-BOM")
  required<-c("Question","Definition","Topics of interest",
              "Answer_form","Response_compulsory")
  if(!all(required%in%names(spec)))
    stop("Locked schema lacks required fields.")
  spec<-spec[!grepl("mechanisms of the collective action",spec$Question,
                     ignore.case=TRUE),,drop=FALSE]
  if(!nrow(spec)||anyDuplicated(spec$Question))stop("Invalid locked case schema.")
  queue_file<-norm(file.path(w09,"retrieval_queue.csv"))
  q<-utils::read.csv(queue_file,stringsAsFactors=FALSE,check.names=FALSE,
                     fileEncoding="UTF-8-BOM")
  cls<-utils::read.csv(classifications_csv,stringsAsFactors=FALSE,
                       check.names=FALSE,fileEncoding="UTF-8-BOM")
  if(!"record_id"%in%names(q)||anyDuplicated(q$record_id)||
     !all(c("record_id","final_case_status")%in%names(cls))||
     anyDuplicated(cls$record_id))
    stop("W09 queue or authoritative W08 classifications have invalid IDs.")
  eligible<-intersect(q$record_id,cls$record_id[
    cls$final_case_status%in%c("specific_case","multiple_cases")])
  if(!length(eligible))stop("No confirmed case-positive IDs in this cohort.")
  dest<-file.path(map_path,"reports","ostara_case_fulltext_locked",
                  cohort_label)
  dir.create(dest,recursive=TRUE,showWarnings=FALSE)
  inventory_file<-file.path(dest,"selected_accessible_fulltexts.csv")
  manifest_file<-file.path(dest,"selection_manifest.json")
  extraction_dir<-file.path(w09,"case_data_extraction",paste0(
    "csv-",substr(sha(schema),1,12),"-prompt-",
    substr(sha(prompt),1,12),"-model-gpt-5.6-luna"))
  # A complete snapshot includes BOTH existing pilot checkpoints and
  # unprocessed accessible publications. Prior sampling is NOT exclusionary.
  if(!file.exists(inventory_file)&&!file.exists(manifest_file)) {
    message("[OSTARA locked] Checking ",length(eligible),
            " confirmed-case IDs for available/readable full text...")
    source<-list()
    failed<-list()
    for(id in eligible) {
      payload<-tryCatch(w09_text_payload(w09,id,max_pages=0L),
                        error=function(e)e)
      if(inherits(payload,"error")){
        failed[[length(failed)+1L]]<-data.frame(
          record_id=id,reason=conditionMessage(payload))
        next
      }
      if(!is.character(payload$sha256)||length(payload$sha256)!=1L||
         !nzchar(payload$sha256)){
        failed[[length(failed)+1L]]<-data.frame(
          record_id=id,reason="Full-text payload missing SHA256")
        next
      }
      source[[length(source)+1L]]<-data.frame(
        record_id=id,source_sha256=payload$sha256,stringsAsFactors=FALSE)
    }
    if(!length(source))
      stop("No readable confirmed-case full texts in ",w09,
           ". No model requests made.")
    inventory<-do.call(rbind,source)
    if(anyDuplicated(inventory$record_id))stop("Duplicate inventoried IDs.")
    utils::write.csv(inventory,inventory_file,row.names=FALSE,
                     fileEncoding="UTF-8")
    failures<-if(length(failed))do.call(rbind,failed) else
      data.frame(record_id=character(),reason=character())
    utils::write.csv(failures,file.path(dest,"inaccessible_fulltexts.csv"),
                     row.names=FALSE,fileEncoding="UTF-8")
    jsonlite::write_json(list(
       schema="ostara-locked-fulltext-v1",cohort=cohort_label,w09=w09,
       queue_sha256=sha(queue_file),
       classifications_sha256=sha(classifications_csv),
       pilot_manifest_sha256=sha(pilot_manifest),
       prompt_sha256=sha(prompt),schema_sha256=sha(schema),
       inventory_sha256=sha(inventory_file)),
       manifest_file,pretty=TRUE,auto_unbox=TRUE)
  } else if(!file.exists(inventory_file)||!file.exists(manifest_file))
    stop("Incomplete fulltext inventory/manifest; repair without deleting the archive.")
  m<-jsonlite::fromJSON(manifest_file,simplifyVector=FALSE)
  expect<-list(schema="ostara-locked-fulltext-v1",cohort=cohort_label,
     w09=w09,queue_sha256=sha(queue_file),
     classifications_sha256=sha(classifications_csv),
     pilot_manifest_sha256=sha(pilot_manifest),
     prompt_sha256=sha(prompt),schema_sha256=sha(schema),
     inventory_sha256=sha(inventory_file))
  for(k in names(expect))
    if(!identical(m[[k]],expect[[k]]))
      stop("Locked fulltext batch provenance mismatch: ",k)
  inventory<-utils::read.csv(inventory_file,stringsAsFactors=FALSE,
                             fileEncoding="UTF-8-BOM")
  if(!all(c("record_id","source_sha256")%in%names(inventory))||
     anyDuplicated(inventory$record_id)||
     any(!inventory$record_id%in%eligible))
    stop("Saved inventory contains invalid IDs.")
  ids<-inventory$record_id
  ledger_file<-file.path(extraction_dir,"case_data.jsonl")
  already<-readjl(ledger_file)
  done<-if(length(already))vapply(already,function(z)scalar(z$record_id),
                                  character(1)) else character()
  if(anyDuplicated(done))stop("Existing extraction checkpoint has duplicate IDs.")
  pending<-setdiff(ids,done)
  selected<-if(max_records>0L)head(pending,max_records) else pending
  message("[OSTARA locked] ",cohort_label,": ",length(ids),
          " readable case-positive full texts; ",length(intersect(ids,done)),
          " already validated with exact locked inputs; ",
          length(selected)," queued this call; ",length(pending)," total pending.")
  # Prevent any paid requests until the complete, immutable candidate manifest
  # is present. The underlying W09 function resumes from its own ledger.
  if(length(selected)) {
    if(!nzchar(Sys.getenv("OPENAI_API_KEY")))
      stop("OPENAI_API_KEY is missing; no paid requests made.")
    run_w09_case_data_extraction(
      w09=w09,prompt_file=prompt,schema_file=schema,
      record_ids=selected,max_records=0L,
      model="gpt-5.6-luna",max_pages=0L,revalidate_only=FALSE)
  }
  # Produce complete cohort-wide outputs from validated JSONL checkpoint,
  # INCLUDING the approved pilot cases and any new cases. Never rewrite JSONL.
  records<-readjl(ledger_file)
  rid<-if(length(records))vapply(records,function(z)scalar(z$record_id),
                                 character(1)) else character()
  if(anyDuplicated(rid))stop("Duplicate checkpointed publications.")
  valid<-records[rid%in%ids]
  valid_ids<-intersect(ids,rid)
  status<-data.frame(record_id=ids,
    extraction_status=ifelse(ids%in%valid_ids,"validated","unresolved"),
    case_count=rep(NA_integer_,length(ids)),stringsAsFactors=FALSE)
  for(z in valid)
    status$case_count[match(scalar(z$record_id),ids)]<-length(z$cases)
  # One row per identified case. Explicit publication statuses include
  # publications containing no qualifying case and those still unresolved.
  field_cols<-unlist(lapply(seq_len(nrow(spec)),function(i){
    h<-paste0(sprintf("%02d",i),". ",spec$Question[[i]])
    c(h,paste0(h," [evidence]"),paste0(h," [reporting status]"))
  }),use.names=FALSE)
  field_cols<-make.unique(field_cols)
  base<-c("record_id","case_id","assessment_reason",
          "case_boundary_uncertainty")
  rows<-list()
  for(rec in valid)for(case in rec$cases){
    row<-c(record_id=scalar(rec$record_id),case_id=scalar(case$case_id),
      assessment_reason=scalar(case$assessment_reason),
      case_boundary_uncertainty=scalar(case$case_boundary_uncertainty))
    fields<-character()
    for(i in seq_len(nrow(spec))) {
      field_id<-sprintf("field_%03d",i)
      f<-case$fields[[field_id]]
      if(is.null(f))stop("Validated case has missing field ",field_id)
      vals<-if(!is.null(f$values))unlist(f$values,use.names=FALSE)
             else unlist(f$value,use.names=FALSE)
      vals<-as.character(vals)
      factor<-grepl("enabler|barrier",spec$Question[[i]],ignore.case=TRUE)
      answer<-if(!length(vals))"" else if(factor)
        paste0("\u2022 ",paste(vals,collapse="\n\u2022 "))
        else paste(vals,collapse="\n")
      evidence<-if(length(f$evidence))vapply(f$evidence,
        function(e)paste0(scalar(e$anchor),": ",scalar(e$quote)),
        character(1)) else character()
      fields<-c(fields,answer,paste(evidence,collapse="\n"),
                scalar(f$reporting_status))
    }
    names(fields)<-field_cols
    rows[[length(rows)+1L]]<-c(row,fields)
  }
  columns<-c(base,field_cols)
  wide<-if(length(rows)) {
    tab<-as.data.frame(do.call(rbind,rows),stringsAsFactors=FALSE,
                       check.names=FALSE)
    names(tab)<-columns
    tab
  } else {
    tab<-as.data.frame(matrix(character(0),nrow=0,ncol=length(columns)),
                       check.names=FALSE)
    names(tab)<-columns
    tab
  }
  write_excel_csv<-function(tab,path) {
    con<-file(path,open="wb")
    on.exit(close(con))
    writeBin(as.raw(c(0xEF,0xBB,0xBF)),con)
    utils::write.table(tab,con,sep=",",row.names=FALSE,col.names=TRUE,
                       quote=TRUE,na="",qmethod="double")
  }
  case_csv<-file.path(dest,"all_cases_wide.csv")
  status_csv<-file.path(dest,"all_publications_status.csv")
  write_excel_csv(wide,case_csv)
  write_excel_csv(status,status_csv)
  message("[OSTARA locked] ",cohort_label,": ",sum(status$extraction_status=="validated"),
          "/",length(ids)," validated; ",nrow(wide)," cases exported.")
  invisible(list(cohort=cohort_label,total=length(ids),
    validated=sum(status$extraction_status=="validated"),
    unresolved=status$record_id[status$extraction_status=="unresolved"],
    case_count=nrow(wide),cases_csv=case_csv,status_csv=status_csv,
    inventory_csv=inventory_file,extraction_dir=extraction_dir))
}
