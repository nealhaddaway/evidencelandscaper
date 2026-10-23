#' Store API usage and estimated costs in a local map
#'
#' Records are append-only and keyed by the provider response ID.
#' Cost estimates use a user-maintained model pricing CSV; usage remains
#' available even when no verified price has been supplied.
#' @keywords internal
record_api_usage <- function(map_path,workflow,record_id,response,model=NULL) {
 if(is.null(response$usage))return(invisible(FALSE))
 u<-response$usage
 val<-function(x)if(is.null(x)||!length(x))0 else as.numeric(x[[1L]])
 input<-val(u$input_tokens)
 cached<-val(u$input_tokens_details$cached_tokens)
 output<-val(u$output_tokens)
 if(cached>input)stop("Cached tokens exceed total input tokens.")
 model<-if(!is.null(response$model))as.character(response$model) else model
 rid<-if(!is.null(response$id))as.character(response$id) else ""
 if(!nzchar(rid))stop("API usage has no stable response ID.")
 root<-file.path(map_path,"api_costs")
 dir.create(root,recursive=TRUE,showWarnings=FALSE)
 ledger<-file.path(root,"usage.jsonl")
 if(file.exists(ledger)) {
   lines<-readLines(ledger,warn=FALSE)
   ids<-vapply(lapply(lines[nzchar(trimws(lines))],
       jsonlite::fromJSON,simplifyVector=FALSE),function(z)z$response_id,
       character(1))
   if(rid%in%ids)return(invisible(FALSE))
 }
 x<-list(timestamp_utc=format(Sys.time(),"%Y-%m-%dT%H:%M:%SZ",tz="UTC"),
   workflow=workflow,record_id=record_id,response_id=rid,
   model=model,input_tokens=input,cached_input_tokens=cached,
   output_tokens=output,provider="OpenAI",source="api_response_usage")
 cat(jsonlite::toJSON(x,auto_unbox=TRUE,null="null"),"\n",
     file=ledger,append=TRUE,sep="")
 invisible(TRUE)
}

#' Register explicit OpenAI pricing for cumulative cost reporting
#'
#' Prices are USD per million tokens, never inferred from model names.
#' @param map_path Map project directory.
#' @param model Exact API model identifier.
#' @param input_usd_per_m Standard input price per million tokens.
#' @param cached_input_usd_per_m Cached input price per million tokens.
#' @param output_usd_per_m Output price per million tokens.
#' @return Pricing file path.
#' @export
set_api_pricing <- function(map_path,model,input_usd_per_m,
                            cached_input_usd_per_m,output_usd_per_m) {
 map_path<-normalizePath(map_path,winslash="/",mustWork=TRUE)
 vals<-c(input_usd_per_m,cached_input_usd_per_m,output_usd_per_m)
 if(!is.character(model)||length(model)!=1L||!nzchar(model)||
    any(!is.finite(vals))||any(vals<0))stop("Invalid model or rates.")
 root<-file.path(map_path,"api_costs")
 dir.create(root,recursive=TRUE,showWarnings=FALSE)
 path<-file.path(root,"prices.csv")
 row<-data.frame(model=model,input_usd_per_m=vals[[1L]],
   cached_input_usd_per_m=vals[[2L]],output_usd_per_m=vals[[3L]])
 old<-if(file.exists(path))utils::read.csv(path,stringsAsFactors=FALSE)
    else row[FALSE,,drop=FALSE]
 if(model%in%old$model) {
   existing<-old[old$model==model,,drop=FALSE]
   if(!identical(as.numeric(existing[1,-1]),as.numeric(row[1,-1])))
     stop("Model rate already recorded; retain historical prices and use a new pricing version.")
 } else utils::write.csv(rbind(old,row),path,row.names=FALSE)
 invisible(path)
}

#' Summarise cumulative API usage and estimated cost by workflow
#' @param map_path Local evidence-map project folder.
#' @return Data frame, including total row and missing-price flags.
#' @export
api_cost_summary <- function(map_path) {
 map_path<-normalizePath(map_path,winslash="/",mustWork=TRUE)
 root<-file.path(map_path,"api_costs")
 p<-file.path(root,"usage.jsonl")
 if(!file.exists(p))return(data.frame())
 lines<-readLines(p,warn=FALSE)
 rows<-lapply(lines[nzchar(trimws(lines))],jsonlite::fromJSON)
 if(!length(rows))return(data.frame())
 df<-do.call(rbind,lapply(rows,function(z)data.frame(
   workflow=z$workflow,model=z$model,
   input_tokens=z$input_tokens,cached_input_tokens=z$cached_input_tokens,
   output_tokens=z$output_tokens)))
 prices<-file.path(root,"prices.csv")
 if(file.exists(prices)) {
   pr<-utils::read.csv(prices,stringsAsFactors=FALSE)
   df<-merge(df,pr,by="model",all.x=TRUE,sort=FALSE)
 } else {
   df$input_usd_per_m<-NA_real_
   df$cached_input_usd_per_m<-NA_real_
   df$output_usd_per_m<-NA_real_
 }
 df$estimated_usd<-with(df,
    ((input_tokens-cached_input_tokens)*input_usd_per_m+
     cached_input_tokens*cached_input_usd_per_m+
     output_tokens*output_usd_per_m)/1e6)
 splitrows<-split(df,df$workflow)
 summarize<-function(x,name)data.frame(workflow=name,requests=nrow(x),
   input_tokens=sum(x$input_tokens),cached_input_tokens=sum(x$cached_input_tokens),
   output_tokens=sum(x$output_tokens),
   estimated_usd=if(anyNA(x$estimated_usd))NA_real_ else sum(x$estimated_usd),
   unpriced_requests=sum(is.na(x$estimated_usd)))
 result<-do.call(rbind,Map(summarize,splitrows,names(splitrows)))
 result<-rbind(result,summarize(df,"TOTAL"))
 rownames(result)<-NULL
 result
}
