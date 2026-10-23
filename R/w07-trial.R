#' Code a small sample of W04-retained publications with an Ostara W07 ontology
#' @param map_path Project directory; defaults to Ostara project.
#' @param ontology_file Path to mechanism ontology CSV; omitted reuses the latest archived W07 trial ontology.
#' @param prompt_file Path to W07 coding prompt TXT; omitted reuses the corresponding archived W07 prompt.
#' @param n Number of retained records to code; 0 codes all from finalised full W04.
#' @param w04_run Optional explicit finalised W04 run, including a selected-record run.\n#' @param model OpenAI Responses model.
#' @return Local W07 trial directory.
#' @export
trial_w07 <- function(ontology_file=NULL, prompt_file=NULL, n=5L, model="gpt-6-luna",
                      map_path=NULL,w04_run=NULL) {
  if(is.null(map_path))map_path<-open_map("Ostara_collective_action_map")
  map_path<-normalizePath(map_path,winslash="/",mustWork=TRUE)
  if(is.null(ontology_file) || is.null(prompt_file)) {
    # Reuse a self-contained, provenance-preserving W07 trial. Never prompt
    # for files or guess from unrelated W04 prompt archives.
    archive_root<-file.path(map_path,"runs","W07")
    archived<-if(dir.exists(archive_root))
      list.dirs(archive_root,recursive=FALSE,full.names=TRUE) else character()
    archived<-archived[file.exists(file.path(archived,"ontology.csv")) &
                       file.exists(file.path(archived,"prompt.txt")) &
                       file.exists(file.path(archived,"manifest.json"))]
    if(length(archived)) {
      archived<-archived[order(file.info(file.path(archived,"manifest.json"))$mtime,
                               decreasing=TRUE)]
      matching<-Filter(function(d) {
        m<-jsonlite::fromJSON(file.path(d,"manifest.json"),simplifyVector=FALSE)
        sha<-function(p)digest::digest(file=p,algo="sha256",serialize=FALSE)
        identical(m$ontology_sha256,sha(file.path(d,"ontology.csv"))) &&
          identical(m$prompt_sha256,sha(file.path(d,"prompt.txt")))
      },archived)
      if(length(matching)) {
        base<-matching[[1L]]
        if(is.null(ontology_file))ontology_file<-file.path(base,"ontology.csv")
        if(is.null(prompt_file))prompt_file<-file.path(base,"prompt.txt")
        message("[W07] Reusing archived ontology and prompt from: ",base)
      }
    }
    if(is.null(ontology_file)||is.null(prompt_file))
      stop("No verified archived W07 ontology/prompt pair found. Supply file paths once; they will be archived for reuse.")
  }
  ontology_file<-normalizePath(ontology_file,winslash="/",mustWork=TRUE)
  prompt_file<-normalizePath(prompt_file,winslash="/",mustWork=TRUE)
  archive_map_prompt(prompt_file,"W07",map_path)
  if(!nzchar(Sys.getenv("OPENAI_API_KEY")))stop("OPENAI_API_KEY is required.")
  n<-as.integer(n)
  if(is.na(n)||n<0L||n>50000L)stop("n must be 0 (all) or a positive integer.")
  sha<-function(p)digest::digest(file=p,algo="sha256",serialize=FALSE)
  `%||%`<-function(x,y)if(is.null(x))y else x
  rows<-utils::read.csv(ontology_file,stringsAsFactors=FALSE,
                        fileEncoding="UTF-8-BOM",check.names=FALSE)
  required<-c("category_id","mechanism_id","mechanism_source_label",
              "definition_draft","abstract_indicators_draft","not_sufficient_draft")
  if(!all(required %in% names(rows)))stop("Ontology missing required fields.")
  codes<-trimws(rows$mechanism_id)
  if(any(!nzchar(codes))||anyDuplicated(codes))stop("Ontology IDs missing or repeated.")
  prompt<-paste(readLines(prompt_file,warn=FALSE,encoding="UTF-8"),collapse="\n")
  if(!nzchar(trimws(prompt)))stop("Empty coding prompt.")
  roots<-list.dirs(file.path(map_path,"runs","W01"),recursive=FALSE,full.names=TRUE)
  options<-unlist(lapply(roots,function(r) {
    path<-file.path(r,"W04_local")
    if(!dir.exists(path))return(character())
    list.dirs(path,recursive=FALSE,full.names=TRUE)
  }),use.names=FALSE)
  options<-options[file.exists(file.path(options,"finalised","summary.json"))]
  if(!is.null(w04_run)) {\n    w04_run<-normalizePath(w04_run,winslash="/",mustWork=TRUE)\n    options<-options[options==w04_run]\n  } else {\n    options<-options[if(n==0L)grepl("^full-",basename(options)) else grepl("^trial-",basename(options))]\n  }
  if(!length(options))stop("No appropriate finalised W04 screening run: full W04 required when n=0.")
  options<-options[order(file.info(file.path(options,"finalised","summary.json"))$mtime,
                         decreasing=TRUE)]
  w04<-options[[1L]]
  summary_path<-file.path(w04,"finalised","summary.json")
  final_path<-file.path(w04,"finalised","workflow04_final_screening_layer.jsonl")
  summary<-jsonlite::fromJSON(summary_path)
  if(!identical(summary$status,"final")||
     (n==0L && is.null(w04_run) && !identical(summary$run_type,"full"))||
     !identical(summary$final_layer_sha256,sha(final_path))||
     !identical(summary$original_consensus_sha256 %||% summary$source_layer_sha256,
                sha(file.path(w04,"consensus","workflow04_consensus_layer.jsonl"))))
    stop("W04 trial finalisation checksum validation failed.")
  readjl<-function(p) {
    lines<-readLines(p,warn=FALSE,encoding="UTF-8")
    lapply(lines[nzchar(trimws(lines))],jsonlite::fromJSON,simplifyVector=FALSE)
  }
  screened<-readjl(final_path)
  retained<-Filter(function(x)identical(x$screening$decision,"retain"),screened)
  if(!length(retained))stop("No retained W04 trial records found.")
  if(n==0L && (length(screened)!=summary$records || length(retained)!=summary$retain))
    stop("W07 input counts do not match finalised W04 summary.")
  selected<-if(n==0L)retained else head(retained,n)
  root<-dirname(dirname(w04))
  canonical_file<-file.path(root,"W02_main","finalised","canonical.jsonl")
  canonical<-readjl(canonical_file)
  source<-setNames(canonical,vapply(canonical,function(x)x$identity$record_id,character(1)))
  id<-vapply(selected,function(x)x$record_id,character(1))
  amended<-list()
  if(isTRUE(summary$reconciled_abstracts)) {
    rescreens<-list.dirs(file.path(w04,"abstract_rescreens"),recursive=FALSE,full.names=TRUE)
    rescreens<-rescreens[file.exists(file.path(rescreens,"manifest.json"))]
    if(length(rescreens)!=1L)stop("Cannot uniquely locate corrected W04 abstracts.")
    correction_file<-file.path(rescreens[[1]],"inputs","submitted_abstracts.csv")
    if(!file.exists(correction_file))stop("Missing corrected W04 abstracts.")
    corrections<-utils::read.csv(correction_file,stringsAsFactors=FALSE,fileEncoding="UTF-8-BOM")
    if(anyDuplicated(corrections$record_id))stop("Duplicate corrected abstract IDs.")
    amended<-setNames(as.list(corrections$abstract),corrections$record_id)
  }
  if(any(!id %in% names(source)))stop("Selected W04 IDs missing from canonical W02.")
  organise_w07_trials(map_path)
  dest<-file.path(map_path,"runs","W07",paste0(if(n==0L)"full-" else "trial-",length(id),"-",
          substr(sha(ontology_file),1,10),"-",substr(sha(prompt_file),1,10)))
  if(!identical(model,"gpt-5.6")) dest<-paste0(dest,"-model-",gsub("[^A-Za-z0-9._-]","_",model))
  dir.create(dest,recursive=TRUE,showWarnings=FALSE)
  mpath<-file.path(dest,"manifest.json")
  outpath<-file.path(dest,"coding.jsonl")
  provenance<-list(schema="local-w07-abstract-trial-v1",
    source_type=if(n==0L)"W04_FINALISED_FULL" else "W04_FINALISED_TRIAL_ONLY",source_w04=w04,
    source_w04_sha256=sha(final_path),
    canonical_w02_sha256=sha(canonical_file),
    ontology_sha256=sha(ontology_file),prompt_sha256=sha(prompt_file),
    selected_record_ids=as.list(id),model=model)
  if(file.exists(mpath)) {
    m<-jsonlite::fromJSON(mpath,simplifyVector=FALSE)
    if(!identical(m$source_w04_sha256,provenance$source_w04_sha256)||
       !identical(m$ontology_sha256,provenance$ontology_sha256)||
       !identical(m$prompt_sha256,provenance$prompt_sha256)||
       !identical(m$model,model))stop("W07 trial provenance conflict.")
  } else {
    if(length(list.files(dest,all.files=TRUE,no..=TRUE)))
      stop("Unexpected existing W07 folder without manifest.")
    file.copy(ontology_file,file.path(dest,"ontology.csv"))
    file.copy(prompt_file,file.path(dest,"prompt.txt"))
    jsonlite::write_json(provenance,mpath,pretty=TRUE,auto_unbox=TRUE)
  }
  # Append-only results allow a safe resume after an API or network error.
  done<-if(file.exists(outpath))readjl(outpath) else list()
  done_ids<-vapply(done,function(x)x$record_id,character(1))
  if(anyDuplicated(done_ids)||any(!done_ids%in%id))stop("Existing W07 results have mismatched IDs.")
  ontology_text<-paste(utils::capture.output(utils::write.csv(
     rows[,required,drop=FALSE],row.names=FALSE)),collapse="\n")
  instructions<-paste(prompt,"\n\nCONTROLLED ONTOLOGY CSV:\n",ontology_text,
    "\n\nReturn only the requested JSON object. Preserve the supplied record_id.")
  message("[W07] Progress: ",length(done_ids)," of ",length(id)," records completed; ",length(id)-length(done_ids)," remaining.")
  progress_index<-length(done_ids)
  for(record_id in id[!id%in%done_ids]){
    z<-source[[record_id]]$canonical
    abstract<-if(!is.null(amended[[record_id]]))amended[[record_id]] else z$abstract
    record<-list(record_id=record_id,title=z$title,abstract=abstract,
                 keywords=if(!is.null(z$author_keywords))z$author_keywords else z$keywords)
    body<-list(model=model,store=FALSE,reasoning=list(effort="low"),
      input=list(list(role="system",content=list(list(type="input_text",text=instructions))),
                 list(role="user",content=list(list(type="input_text",
                    text=jsonlite::toJSON(record,auto_unbox=TRUE,null="null"))))),
      text=list(format=list(type="json_object")))
    resp<-httr2::request("https://api.openai.com/v1/responses") |>
      httr2::req_auth_bearer_token(Sys.getenv("OPENAI_API_KEY")) |>
      httr2::req_body_json(body,auto_unbox=TRUE) |>
      httr2::req_timeout(180) |> httr2::req_perform() |>
      httr2::resp_body_json(simplifyVector=FALSE)
    texts<-unlist(lapply(resp$output,function(item)
      unlist(lapply(item$content,function(part)
        if(identical(part$type,"output_text"))part$text else NULL),use.names=FALSE)),
      use.names=FALSE)
    if(!length(texts))stop("No W07 response text for ",record_id)
    result<-jsonlite::fromJSON(texts[[1L]],simplifyVector=FALSE)
    if(!identical(result$record_id,record_id))stop("W07 record ID mismatch.")
    assigned<-vapply(result$mechanisms,function(x)x$mechanism_id,character(1))
    if(any(!assigned %in% codes)||anyDuplicated(assigned))
      stop("W07 invalid or duplicate mechanism ID for ",record_id)
    if(!result$coding_status%in%c("coded","insufficient_information","no_supported_mechanism"))
      stop("Invalid W07 coding status for ",record_id)
    cat(jsonlite::toJSON(result,auto_unbox=TRUE,null="null"),"\n",
        file=outpath,append=TRUE,sep="")
    progress_index<-progress_index+1L
    message("[W07] Record ",progress_index," of ",length(id),": ",record_id,"; ",length(assigned)," mechanism(s)")
  }
  final<-readjl(outpath)
  if(length(final)!=length(id))stop("W07 trial incomplete.")
  jsonlite::write_json(c(provenance,list(status="TRIAL_COMPLETE",
       records=length(final),coding_sha256=sha(outpath))),
    file.path(dest,"summary.json"),pretty=TRUE,auto_unbox=TRUE)
  export_w07_candidates(dest)
  message("[W07] Coding completed: ",dest)
  invisible(dest)
}
