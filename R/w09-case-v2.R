#' Run a versioned W09 v2 case registry extraction
#'
#' Reads immutable final W07 abstract coding and its archived ontology.
#' Writes only under W09/v2-...; never overwrites W09 v1 or W07.
#' @param w09 Existing W09 pilot directory.
#' @param record_ids Human-verified full-text publication IDs.
#' @param model OpenAI model.
#' @param max_pages PDF page limit, 0 for all; XML/JSON are not truncated.
#' @param w07_run Optional explicit full W07 archive path. Required when more
#'   than one completed full W07 run exists; avoids silently selecting v2.
#' @param prompt_version Versioned W09 prompt; defaults to v2_5. Use v2_3 or v2_4 to resume historical runs.
#' @return W09 v2 directory.
#' @export
analyse_w09_cases_v2 <- function(w09,record_ids,model="gpt-6-luna",max_pages=0L,w07_run=NULL,prompt_version="v2_5",eligibility_dir=NULL) {
 if(!requireNamespace("countrycode",quietly=TRUE))
   stop("Install countrycode: install.packages('countrycode')")
 if(!nzchar(Sys.getenv("OPENAI_API_KEY")))stop("OPENAI_API_KEY is required.")
 w09<-normalizePath(w09,winslash="/",mustWork=TRUE)
 map<-dirname(dirname(dirname(w09)))
 queue<-utils::read.csv(file.path(w09,"retrieval_queue.csv"),
                       stringsAsFactors=FALSE)
 if(missing(record_ids)||!length(record_ids)||anyDuplicated(record_ids)||
    any(!record_ids%in%queue$record_id))
   stop("Supply distinct IDs whose full texts you have checked.")
 if(length(max_pages)!=1L||is.na(max_pages)||max_pages!=as.integer(max_pages)||
    max_pages<0L)stop("max_pages must be a nonnegative integer.")
 sha<-function(x)digest::digest(file=x,algo="sha256",serialize=FALSE)
 eligible_by_record<-NULL
 gate_sha<-NULL
 if(!is.null(eligibility_dir)) {
   eligibility_dir<-normalizePath(eligibility_dir,mustWork=TRUE)
   gate<-file.path(eligibility_dir,"candidate_case_decisions.jsonl")
   manifest_gate<-file.path(eligibility_dir,"manifest.json")
   if(!file.exists(gate)||!file.exists(manifest_gate))
     stop("Eligibility gate must be complete and provenance-verified.")
   gm<-jsonlite::fromJSON(manifest_gate,simplifyVector=FALSE)
   if(!identical(gm$ledger_sha256,sha(gate)) ||
      !identical(normalizePath(gm$w09,mustWork=TRUE),w09))
     stop("Eligibility gate provenance mismatch.")
   finalpath<-file.path(eligibility_dir,"final_case_eligibility.csv")
   finalmanifest<-file.path(eligibility_dir,"final_review_manifest.json")
   if(!all(file.exists(c(finalpath,finalmanifest))))
     stop("Human eligibility review must be finalised before extraction.")
   fm<-jsonlite::fromJSON(finalmanifest,simplifyVector=FALSE)
   if(!identical(fm$original_ledger_sha256,sha(gate))||
      !identical(fm$final_audit_sha256,sha(finalpath)))
     stop("Final human eligibility review checksum mismatch.")
   final<-utils::read.csv(finalpath,stringsAsFactors=FALSE,
                          colClasses="character")
   if(anyDuplicated(paste(final$record_id,final$case_id))||
      any(!final$decision%in%c("eligible","ineligible")))
     stop("Final eligibility review has invalid or uncertain decisions.")
   rows<-lapply(Filter(nzchar,readLines(gate,warn=FALSE)),
                jsonlite::fromJSON,simplifyVector=FALSE)
   gate_ids<-vapply(rows,function(z)z$record_id,character(1))
   if(anyDuplicated(gate_ids)||any(!record_ids%in%gate_ids))
     stop("Eligibility gate does not cover all requested publications.")
   eligible_by_record<-setNames(lapply(rows,function(z) {
     cases<-lapply(z$cases,function(ca) {
       idx<-which(final$record_id==z$record_id & final$case_id==ca$case_id)
       if(length(idx)!=1L)stop("Final review missing candidate case.")
       if(final$decision[idx]!="eligible")return(NULL)
       ca$original_overall<-ca$overall
       ca$overall<-"eligible"
       ca$adjudication_source<-final$decision_source[idx]
       ca$adjudication_rationale<-final$adjudication_rationale[idx]
       ca
     })
     Filter(Negate(is.null),cases)
   }),gate_ids)
   expected<-names(eligible_by_record)[vapply(eligible_by_record,length,integer(1))>0L]
   if(!setequal(record_ids,expected))
     stop("Extraction IDs must match exactly publications with approved cases.")
   gate_sha<-sha(finalpath)
 }
 readjl<-function(p) {
   l<-readLines(p,warn=FALSE,encoding="UTF-8")
   lapply(l[nzchar(trimws(l))],jsonlite::fromJSON,simplifyVector=FALSE)
 }
 root<-file.path(map,"runs","W07")
 runs<-if(dir.exists(root))list.dirs(root,recursive=FALSE,full.names=TRUE) else character()
 runs<-runs[grepl("^full-",basename(runs)) &
   file.exists(file.path(runs,"coding.jsonl")) &
   file.exists(file.path(runs,"ontology.csv")) &
   file.exists(file.path(runs,"manifest.json")) &
   file.exists(file.path(runs,"summary.json"))]
 if(is.null(w07_run)) {
   if(length(runs)!=1L)
     stop("Multiple (or no) completed W07 full runs. Supply w07_run explicitly; do not mix ontology versions.")
   w07<-runs[[1L]]
 } else {
   w07<-normalizePath(w07_run,winslash="/",mustWork=TRUE)
   if(!w07%in%normalizePath(runs,winslash="/",mustWork=TRUE))
     stop("Selected w07_run is not a completed full W07 archive within this map.")
 }
 manifest<-jsonlite::fromJSON(file.path(w07,"manifest.json"),
                             simplifyVector=FALSE)
 ontpath<-file.path(w07,"ontology.csv")
 codingpath<-file.path(w07,"coding.jsonl")
 if(!identical(manifest$ontology_sha256,sha(ontpath)))
   stop("W07 ontology checksum mismatch.")
 summary<-jsonlite::fromJSON(file.path(w07,"summary.json"),
                            simplifyVector=FALSE)
 if(!identical(summary$coding_sha256,sha(codingpath)))
   stop("W07 coding checksum mismatch.")
 codes<-utils::read.csv(ontpath,stringsAsFactors=FALSE,
                        fileEncoding="UTF-8-BOM",check.names=FALSE)
 if(anyDuplicated(codes$mechanism_id))stop("W07 ontology has duplicate codes.")
 known<-as.character(codes$mechanism_id)
 encoded<-readjl(codingpath)
 byid<-setNames(encoded,vapply(encoded,function(z)z$record_id,character(1)))
 if(any(!record_ids%in%names(byid)))
   stop("Selected record IDs absent from W07 full-corpus coding.")
 if(!prompt_version%in%c("v2_3","v2_4","v2_5"))
   stop("Unsupported W09 prompt version.")
 prompt_src<-system.file("w09",paste0("fulltext_case_prompt_",prompt_version,".txt"),
                         package="evidencelandscaper")
 if(!nzchar(prompt_src))stop("W09 v2 prompt missing from package.")
 target<-file.path(w09,"v2-",substr(sha(prompt_src),1L,12L),"-",
                    substr(sha(ontpath),1L,12L))
 if(!identical(model,"gpt-5.6")) target<-paste0(target,"-model-",gsub("[^A-Za-z0-9._-]","_",model))
 if(!is.null(gate_sha)) target<-paste0(target,"-preeligible-",substr(gate_sha,1L,12L))
 mpath<-file.path(target,"manifest.json")
 out<-file.path(target,"fulltext_case_registry.jsonl")
 pm<-list(schema="w09-case-extraction-v2",model=model,
   prompt_sha256=sha(prompt_src),ontology_sha256=sha(ontpath),
   coding_sha256=sha(codingpath),w07_run=w07,
   eligibility_ledger_sha256=gate_sha)
 if(file.exists(mpath)) {
   old<-jsonlite::fromJSON(mpath,simplifyVector=FALSE)
   for(k in names(pm))if(!identical(pm[[k]],old[[k]]))
     stop("W09 v2 provenance mismatch: ",k)
   if(!identical(sha(file.path(target,"prompt.txt")),pm$prompt_sha256) ||
      !identical(sha(file.path(target,"ontology.csv")),pm$ontology_sha256))
     stop("W09 v2 archived instructions changed.")
 } else {
   if(dir.exists(target)&&length(list.files(target,all.files=TRUE,no..=TRUE)))
     stop("W09 v2 target not empty and has no manifest.")
   dir.create(target,recursive=TRUE,showWarnings=FALSE)
   if(!file.copy(prompt_src,file.path(target,"prompt.txt"))||
      !file.copy(ontpath,file.path(target,"ontology.csv")))
     stop("Failed to archive W09 v2 prompt and ontology.")
   jsonlite::write_json(pm,mpath,auto_unbox=TRUE,pretty=TRUE)
 }
 prompt<-paste(readLines(file.path(target,"prompt.txt"),warn=FALSE),collapse="\n")
 onto<-paste(readLines(file.path(target,"ontology.csv"),warn=FALSE),collapse="\n")
 done<-if(file.exists(out))readjl(out) else list()
 ids<-vapply(done,function(z)z$record_id,character(1))
 if(anyDuplicated(ids)||any(!ids%in%names(byid)))
   stop("W09 v2 output contains duplicate or invalid IDs.")
 verify_country<-function(country) {
   nm<-country$country_name
   if(!is.character(nm)||length(nm)!=1L||!nzchar(trimws(nm)))
     stop("Missing country name.")
   expected<-suppressWarnings(countrycode::countrycode(nm,"country.name","iso3c",
                        warn=FALSE))
   if(is.na(expected))stop("Unrecognised ISO country name: ",nm)
   code<-country$country_iso3
   if(!is.character(code)||length(code)!=1L||!identical(code,expected))
     stop("Country ISO3 mismatch: ",nm," / ",code)
   invisible(code)
 }
 validate<-function(z,id,abstract_codes,fulltext) {
   if(!identical(z$record_id,id))stop("Wrong W09 v2 record ID.")
   if(!z$verification_status%in%c("cases_confirmed","no_collective_action_case",
                                 "uncertain","fulltext_unreadable"))
     stop("Invalid W09 v2 verification status.")
   if(!is.list(z$cases)||!is.list(z$abstract_mechanism_validation)||
      !is.list(z$ontology_gaps))stop("Missing W09 v2 arrays.")
   ids<-vapply(z$abstract_mechanism_validation,function(v)v$mechanism_id,
               character(1))
   if(anyDuplicated(ids)||!setequal(ids,abstract_codes))
     stop("W09 v2 did not assess each originally coded W07 mechanism.")
   if(any(!ids%in%known))stop("W09 v2 invented an ontology code.")
   valid_stages<-c("input_provision","production","processing",
       "aggregation_storage","transport_logistics","trading_distribution",
       "retail","consumption","cross_cutting","unclear")
   for(v in z$abstract_mechanism_validation) {
     if(!v$status%in%c("confirmed","not_supported","unclear"))
       stop("Invalid mechanism assessment.")
   }
   for(ca in z$cases) {
     if(!is.list(ca$countries)||!is.list(ca$locations))
       stop("Missing geography arrays.")
     countries<-vapply(ca$countries,verify_country,character(1))
     for(loc in ca$locations) {
       code<-loc$country_iso3
       if(!is.character(code)||length(code)!=1L||
          !code%in%countries||!nzchar(loc$location_name))
         stop("Subnational location has no matching operational country.")
     }
     stages<-unlist(ca$supply_chain_stages,use.names=FALSE)
     if(any(!stages%in%valid_stages))stop("Unknown supply-chain stage.")
     if(!is.list(ca$additional_mechanisms)||!is.list(ca$author_conclusions))
       stop("Missing mechanism/conclusion arrays.")
     added<-vapply(ca$additional_mechanisms,function(v)v$mechanism_id,
                   character(1))
     if(any(!added%in%known))stop("W09 v2 invented a mechanism code.")
   }
   if(!is.null(eligible_by_record)) {
     expected_ids<-vapply(eligible_by_record[[id]],function(x)x$case_id,character(1))
     produced_ids<-vapply(z$cases,function(x)
       if(is.null(x$eligibility_case_id)) "" else x$eligibility_case_id,
       character(1))
     if(!setequal(produced_ids,expected_ids)||anyDuplicated(produced_ids))
       stop("W09 extraction did not preserve exact eligible candidate IDs.")
   }
   invisible(TRUE)
 }
 total_records<-length(record_ids)
 completed_records<-sum(record_ids%in%ids)
 message("[W09 extraction] ",completed_records,"/",total_records,
         " publications completed; ",total_records-completed_records,
         " remaining (existing V2.5 outputs reused).")
 for(id in record_ids[!record_ids%in%ids]) {
   message("[W09 extraction] ",completed_records+1L,"/",total_records,
           " processing ",id)
   payload<-w09_text_payload(w09,id,as.integer(max_pages))
   original<-byid[[id]]
   assigned<-if(is.null(original$mechanisms))character() else
     vapply(original$mechanisms,function(m)m$mechanism_id,character(1))
   instruction<-paste(prompt,"\n\nARCHIVED W07 ONTOLOGY CSV:\n",onto,
     "\n\nSOURCE ANCHORS:",payload$anchors,
     "\nFor XML/JSON anchors are section or text block labels, not page numbers.")
   if(!is.null(eligible_by_record)) {
     instruction<-paste0(instruction,
       "\n\nELIGIBILITY-FIRST EXTRACTION RESTRICTION: Extract ONLY ",
       "the explicitly approved cases in eligible_cases below. ",
       "Each output case MUST include eligibility_case_id exactly matching ",
       "the candidate case_id. Do not extract any ineligible, uncertain, ",
       "additional or merely similar case. Return exactly one case object ",
       "per eligible candidate. Preserve all standard extraction fields.")
   }
   input<-list(record_id=id,w07_abstract_mechanism_ids=as.list(assigned),
     source_format=payload$format,source_sha256=payload$sha256,
     fulltext=payload$text)
   if(!is.null(eligible_by_record))
     input$eligible_cases<-eligible_by_record[[id]]
   body<-list(model=model,store=FALSE,reasoning=list(effort="low"),
      input=list(list(role="system",content=list(list(type="input_text",
        text=instruction))),list(role="user",content=list(list(type="input_text",
        text=jsonlite::toJSON(input,auto_unbox=TRUE,null="null"))))),
      text=list(format=list(type="json_object")))
   # Model output must satisfy the unchanged geographic and ontology checks.
   # Retry only a subnational/country inconsistency with explicit instructions.
   for(attempt in seq_len(3L)) {
     # Retry transient transport timeouts without repeating completed records.
     # Do not retry non-transient API errors.
     response<-NULL
     for(network_attempt in seq_len(3L)) {
       attempt_result<-tryCatch(
         httr2::request("https://api.openai.com/v1/responses")|>
           httr2::req_auth_bearer_token(Sys.getenv("OPENAI_API_KEY"))|>
           httr2::req_body_json(body,auto_unbox=TRUE)|>
           httr2::req_timeout(300)|>httr2::req_perform(),
         error=function(e)e)
       if(!inherits(attempt_result,"error")) {
         response<-attempt_result
         break
       }
       msg<-conditionMessage(attempt_result)
       transient<-grepl("timed? ?out|timeout|connection reset|could not resolve host|failed to connect",
                        msg,ignore.case=TRUE)
       if(!transient||network_attempt==3L)stop(attempt_result)
       message("[W09 extraction] ",id," transient network failure; retry ",
               network_attempt+1L,"/3: ",msg)
       Sys.sleep(2^network_attempt)
     }
     res<-httr2::resp_body_json(response,simplifyVector=FALSE)
     parts<-unlist(lapply(res$output,function(x)
       unlist(lapply(x$content,function(y)
         if(identical(y$type,"output_text"))y$text else NULL),use.names=FALSE)),
       use.names=FALSE)
     if(!length(parts))stop("W09 v2 model response missing.")
     z<-jsonlite::fromJSON(parts[[1]],simplifyVector=FALSE)
     issue<-tryCatch({validate(z,id,assigned,payload$text);NULL},
                     error=function(e)conditionMessage(e))
     if(is.null(issue))break
     case_mismatch<-grepl("W09 extraction did not preserve exact eligible candidate IDs.",
                          issue,fixed=TRUE)
     geography_mismatch<-identical(issue,"Subnational location has no matching operational country.")
     if(!geography_mismatch && !case_mismatch)stop(issue)
     if(case_mismatch) {
       invalid_path<-file.path(target,"invalid_eligibility_case_responses.jsonl")
       cat(jsonlite::toJSON(list(record_id=id,attempt=attempt,
         validation_error=issue,response=z),auto_unbox=TRUE,null="null"),
         "\n",file=invalid_path,append=TRUE,sep="")
       if(attempt==3L)
         stop("W09 pre-eligible extraction case-ID verification failed for ",
              id,"; invalid response archived.")
       body$input[[1L]]$content[[1L]]$text<-paste0(instruction,
         "\n\nCASE-ID CORRECTION: Return exactly the eligible cases in ",
         "eligible_cases in the input, no additional cases. For each extracted ",
         "case, copy its case_id unchanged to eligibility_case_id. ",
         "Preserve all normal W09 fields and full-text evidence.")
       next
     }
     invalid_path<-file.path(target,"invalid_geography_responses.jsonl")
     cat(jsonlite::toJSON(list(record_id=id,attempt=attempt,
       validation_error=issue,response=z),auto_unbox=TRUE,null="null"),
       "\n",file=invalid_path,append=TRUE,sep="")
     if(attempt==3L)stop("W09 geography validation failed after 3 attempts for ",id,
                         "; raw invalid outputs archived.")
     message("[W09 extraction] ",id," invalid subnational-country relation; retry ",attempt+1L,"/3")
     body$input[[1L]]$content[[1L]]$text<-paste0(instruction,
       "\n\nGEOGRAPHY VALIDATION RETRY: For every subnational location, ",
       "its country_iso3 MUST be present in the same case's countries array ",
       "with the matching operational country. Do not invent countries or ",
       "locations. If the source does not support the operational country, ",
       "omit the unsupported subnational location; preserve all other ",
       "supported evidence. Return the complete JSON response.")
   }
   z$provenance<-list(source_format=payload$format,
     source_sha256=payload$sha256,source_path=payload$path,
     ontology_sha256=pm$ontology_sha256,
     w07_coding_sha256=pm$coding_sha256,model=model,
     prompt_sha256=pm$prompt_sha256)
   cat(jsonlite::toJSON(z,auto_unbox=TRUE,null="null"),"\n",
       file=out,append=TRUE,sep="")
   completed_records<-completed_records+1L
   message("[W09 extraction] ",completed_records,"/",total_records,
           " completed ",id,": ",z$verification_status,
           "; cases ",length(z$cases))
 }
 message("[W09 extraction] ",completed_records,"/",total_records,
         " model assessments complete; finalising derived outputs.")
 # Deterministic derived layer; never modify or overwrite raw model responses.
 # Cross-cutting cannot coexist with a specifically identified supply-chain
 # stage. Keep the removal in a separate, auditable file.
 normalised_path<-file.path(target,"fulltext_case_registry_normalised.jsonl")
 audit_path<-file.path(target,"supply_chain_normalisation.jsonl")
 raw<-if(file.exists(out))readjl(out) else list()
 normalized<-lapply(raw,function(z) {
   for(j in seq_along(z$cases)) {
     ca<-z$cases[[j]]
     stages<-unlist(ca$supply_chain_stages,use.names=FALSE)
     specific<-setdiff(stages,c("cross_cutting","unclear"))
     if("cross_cutting"%in%stages && length(specific))
       z$cases[[j]]$supply_chain_stages<-as.list(
         stages[stages!="cross_cutting"])
   }
   z
 })
 events<-unlist(lapply(seq_along(raw),function(i) {
   lapply(seq_along(raw[[i]]$cases),function(j) {
     before<-unlist(raw[[i]]$cases[[j]]$supply_chain_stages,
                    use.names=FALSE)
     after<-unlist(normalized[[i]]$cases[[j]]$supply_chain_stages,
                   use.names=FALSE)
     if(identical(before,after))return(NULL)
     list(record_id=raw[[i]]$record_id,case_index=j,
          removed_code="cross_cutting",rule="specific_stage_present",
          raw_stages=as.list(before),normalised_stages=as.list(after))
   })
 }),recursive=FALSE)
 save_lines<-function(items,path) {
   lines<-vapply(items,function(z)as.character(jsonlite::toJSON(
     z,auto_unbox=TRUE,null="null")),character(1))
   # Derived files can only be generated once; do not silently replace edits.
   if(file.exists(path)) {
     old_lines<-readLines(path,warn=FALSE,encoding="UTF-8")
     if(length(old_lines)>length(lines) ||
        !identical(old_lines,head(lines,length(old_lines))))
       stop("W09 v2.2 derived file conflicts with raw ledger: ",path)
     if(length(lines)>length(old_lines))
       cat(paste0(lines[(length(old_lines)+1L):length(lines)],"\n"),
           file=path,append=TRUE,sep="")
   } else writeLines(lines,path,useBytes=TRUE)
 }
 save_lines(normalized,normalised_path)
 save_lines(Filter(Negate(is.null),events),audit_path)
 message("[W09 extraction] ",completed_records,"/",total_records,
         " complete; outputs: ",target)
 invisible(target)
}
