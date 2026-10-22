#' Adjudicate W01 review cases with the original W01-main LLM prompt
#'
#' Runs model calls in independently auditable chunks. A completed chunk is
#' verified against its immutable case IDs before reuse. On a failure the
#' previous chunks remain intact and can be resumed.
#' @param queue Validated W01 case queue from prepare_w01_review().
#' @param model OpenAI Responses model ID.
#' @param auto_threshold Minimum confidence for an automatic LLM decision.
#' @param batch_size Maximum cases per independent chunk.
#' @return Path to local W01 LLM output directory, invisibly.
#' @export
run_w01_llm <- function(queue, model = Sys.getenv("OPENAI_DUPLICATE_MODEL","gpt-5.6"),
                        auto_threshold=0.95, batch_size=20L) {
  if (!file.exists(queue)) stop("W01 case queue does not exist.")
  queue <- normalizePath(queue,winslash="/",mustWork=TRUE)
  if(!nzchar(Sys.getenv("OPENAI_API_KEY")))
    stop("OPENAI_API_KEY is not configured.")
  if(!is.character(model)||length(model)!=1L||!nzchar(model))
    stop("Explicit non-empty model ID required.")
  if(!is.numeric(auto_threshold)||length(auto_threshold)!=1L||
     !is.finite(auto_threshold)||auto_threshold<0||auto_threshold>1)
    stop("auto_threshold must be between 0 and 1.")
  batch_size<-as.integer(batch_size)
  if(is.na(batch_size)||batch_size<1L) stop("batch_size must be positive.")
  lines<-readLines(queue,warn=FALSE,encoding="UTF-8")
  lines<-lines[nzchar(trimws(lines))]
  ids<-unname(vapply(lines,function(line) {
    jsonlite::fromJSON(line,simplifyVector=FALSE)$review_case_id
  },character(1)))
  if(anyDuplicated(ids)||any(!nzchar(ids))) stop("Invalid case identities.")
  sha<-function(p) digest::digest(file=p,algo="sha256",serialize=FALSE)
  dest<-file.path(dirname(queue),"llm_adjudication")
  spec<-list(schema="local-w01-llm-execution-v1",input_sha256=sha(queue),
             model=model,auto_threshold=auto_threshold,batch_size=batch_size)
  spec_file<-file.path(dest,"execution.json")
  if(dir.exists(dest)) {
    if(!file.exists(spec_file)) stop("Incomplete LLM run metadata; cannot resume.")
    prior<-jsonlite::fromJSON(spec_file,simplifyVector=FALSE)
    if(!identical(prior,spec)) stop("LLM inputs or configuration changed. Refusing resume.")
  } else {
    dir.create(dest,recursive=TRUE)
    jsonlite::write_json(spec,spec_file,auto_unbox=TRUE,pretty=TRUE)
  }
  original<-system.file("w01","workflow_01_llm_adjudicate_duplicates.R",
                         package="evidencelandscaper")
  if(!nzchar(original)) stop("Original W01 LLM script missing.")
  if(!length(lines)) {
    for(f in c("adjudications.jsonl","human_review_cases.jsonl"))
      if(!file.exists(file.path(dest,f))) file.create(file.path(dest,f))
    message("[W01 LLM] No review cases; no model calls.")
    return(invisible(dest))
  }
  chunks<-split(seq_along(lines),ceiling(seq_along(lines)/batch_size))
  for(k in seq_along(chunks)) {
    rows<-chunks[[k]]
    prefix<-file.path(dest,sprintf("chunk-%05d",k))
    output<-paste0(prefix,".adjudications.jsonl")
    human<-paste0(prefix,".human_review.jsonl")
    manifest<-paste0(output,".manifest.json")
    if(file.exists(manifest)) {
      m<-jsonlite::fromJSON(manifest,simplifyVector=FALSE)
      if(!file.exists(output)||!file.exists(human)||
         !identical(m$input_sha256,sha(queue))||
         !identical(m$output_sha256,sha(output))||
         !identical(m$human_review_sha256,sha(human))||
         as.integer(m$start_row)!=min(rows)||
         as.integer(m$end_row)!=max(rows))
        stop("Previously completed LLM chunk failed provenance validation: ",k)
      found<-unname(vapply(readLines(output,warn=FALSE),function(x)
        jsonlite::fromJSON(x,simplifyVector=FALSE)$review_case_id,character(1)))
      if(!identical(found,ids[rows]))
        stop("LLM chunk has unexpected case identifiers: ",k)
      message("[W01 LLM] Reused verified chunk ",k,"/",length(chunks))
      next
    }
    if(file.exists(output)||file.exists(human))
      stop("Incomplete LLM chunk: ",k,
           ". Preserve existing files; inspect before any retry.")
    args<-c(original,"--input",queue,"--output",output,
            "--human-review-output",human,"--model",model,
            "--auto-threshold",format(auto_threshold,scientific=FALSE),
            "--start-row",as.character(min(rows)),
            "--end-row",as.character(max(rows)))
    result<-suppressWarnings(system2(file.path(R.home("bin"),"Rscript"),
       args=vapply(args,shQuote,character(1)),stdout=TRUE,stderr=TRUE))
    status<-attr(result,"status")
    if(!is.null(status)&&status!=0L)
      stop("W01 LLM chunk ",k," failed:\n",
           paste(tail(result,30L),collapse="\n"),call.=FALSE)
    if(!file.exists(manifest))stop("W01 LLM chunk missing validation manifest.")
    message("[W01 LLM] Completed chunk ",k,"/",length(chunks))
  }
  output<-file.path(dest,"adjudications.jsonl")
  human<-file.path(dest,"human_review_cases.jsonl")
  if(file.exists(output)||file.exists(human)) {
    stop("Combined LLM files already exist. Preserve previous result; no overwrite.")
  }
  all_records<-unlist(lapply(seq_along(chunks),function(k)
      readLines(file.path(dest,sprintf("chunk-%05d.adjudications.jsonl",k)),
                warn=FALSE)),use.names=FALSE)
  human_records<-unlist(lapply(seq_along(chunks),function(k)
      readLines(file.path(dest,sprintf("chunk-%05d.human_review.jsonl",k)),
                warn=FALSE)),use.names=FALSE)
  actual<-unname(vapply(all_records,function(x)
    jsonlite::fromJSON(x,simplifyVector=FALSE)$review_case_id,character(1)))
  if(!identical(actual,ids))stop("Merged LLM case IDs do not match input.")
  writeLines(all_records,output,useBytes=TRUE)
  writeLines(human_records,human,useBytes=TRUE)
  jsonlite::write_json(list(schema="local-w01-llm-summary-v1",
    status="success",cases=length(ids),human_review_cases=length(human_records),
    automatic_cases=length(ids)-length(human_records),
    input_sha256=sha(queue),output_sha256=sha(output),
    human_review_sha256=sha(human),model=model),
    file.path(dest,"summary.json"),auto_unbox=TRUE,pretty=TRUE)
  message("[W01 LLM] PASS: ",length(ids)," assessed; ",
          length(human_records)," sent to human review.")
  message("[W01] No final clusters generated yet.")
  invisible(dest)
}
