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
#' @return W09 v2 directory.
#' @export
analyse_w09_cases_v2 <- function(w09,record_ids,model="gpt-5.6",max_pages=0L,w07_run=NULL) {
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
 prompt_src<-system.file("w09","fulltext_case_prompt_v2_3.txt",
                         package="evidencelandscaper")
 if(!nzchar(prompt_src))stop("W09 v2 prompt missing from package.")
 target<-file.path(w09,"v2-",substr(sha(prompt_src),1L,12L),"-",
                    substr(sha(ontpath),1L,12L))
 mpath<-file.path(target,"manifest.json")
 out<-file.path(target,"fulltext_case_registry.jsonl")
 pm<-list(schema="w09-case-extraction-v2",model=model,
   prompt_sha256=sha(prompt_src),ontology_sha256=sha(ontpath),
   coding_sha256=sha(codingpath),w07_run=w07)
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
   invisible(TRUE)
 }
 for(id in record_ids[!record_ids%in%ids]) {
   payload<-w09_text_payload(w09,id,as.integer(max_pages))
   original<-byid[[id]]
   assigned<-if(is.null(original$mechanisms))character() else
     vapply(original$mechanisms,function(m)m$mechanism_id,character(1))
   instruction<-paste(prompt,"\n\nARCHIVED W07 ONTOLOGY CSV:\n",onto,
     "\n\nSOURCE ANCHORS:",payload$anchors,
     "\nFor XML/JSON anchors are section or text block labels, not page numbers.")
   input<-list(record_id=id,w07_abstract_mechanism_ids=as.list(assigned),
     source_format=payload$format,source_sha256=payload$sha256,
     fulltext=payload$text)
   body<-list(model=model,store=FALSE,reasoning=list(effort="low"),
      input=list(list(role="system",content=list(list(type="input_text",
        text=instruction))),list(role="user",content=list(list(type="input_text",
        text=jsonlite::toJSON(input,auto_unbox=TRUE,null="null"))))),
      text=list(format=list(type="json_object")))
   res<-httr2::request("https://api.openai.com/v1/responses")|>
      httr2::req_auth_bearer_token(Sys.getenv("OPENAI_API_KEY"))|>
      httr2::req_body_json(body,auto_unbox=TRUE)|>
      httr2::req_timeout(300)|>httr2::req_perform()|>
      httr2::resp_body_json(simplifyVector=FALSE)
   parts<-unlist(lapply(res$output,function(x)
     unlist(lapply(x$content,function(y)
       if(identical(y$type,"output_text"))y$text else NULL),use.names=FALSE)),
     use.names=FALSE)
   if(!length(parts))stop("W09 v2 model response missing.")
   z<-jsonlite::fromJSON(parts[[1]],simplifyVector=FALSE)
   validate(z,id,assigned,payload$text)
   z$provenance<-list(source_format=payload$format,
     source_sha256=payload$sha256,source_path=payload$path,
     ontology_sha256=pm$ontology_sha256,
     w07_coding_sha256=pm$coding_sha256,model=model,
     prompt_sha256=pm$prompt_sha256)
   cat(jsonlite::toJSON(z,auto_unbox=TRUE,null="null"),"\n",
       file=out,append=TRUE,sep="")
   message("[W09 v2] ",id,": ",z$verification_status,
           "; cases ",length(z$cases))
 }
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
 invisible(target)
}
