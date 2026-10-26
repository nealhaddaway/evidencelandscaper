#' Build interactive Ostara mechanism-ontology meeting webpage
#'
#' Uses the approved W07 ontology and saved mechanism coding from each search,
#' including recorded candidate mechanism gaps. Does not use extracted cases,
#' call a model, or modify the ontology.
#' @param targeted_w07 Full W07 run directory for the targeted search.
#' @param expanded_w07 Full W07 run directory for the expanded search.
#' @param output_file Optional HTML destination.
#' @param harmonised_csv Optional independently reviewed, harmonised mechanism dataset.
#' @return Path to a standalone HTML page.
#' @export
publish_ostara_mechanisms <- function(targeted_w07,expanded_w07,output_file=NULL,harmonised_csv=NULL) {
  sha<-function(p)digest::digest(file=p,algo="sha256",serialize=FALSE)
  readjl<-function(p)lapply(Filter(nzchar,readLines(p,warn=FALSE,encoding="UTF-8")),
                            jsonlite::fromJSON,simplifyVector=FALSE)
  load<-function(folder) {
    folder<-normalizePath(folder,mustWork=TRUE)
    o<-file.path(folder,"ontology.csv")
    c<-file.path(folder,"coding.jsonl")
    m<-file.path(folder,"manifest.json")
    if(!all(file.exists(c(o,c,m))))
      stop("Select a completed W07 folder with ontology.csv, coding.jsonl and manifest.json: ",folder)
    onto<-utils::read.csv(o,stringsAsFactors=FALSE,check.names=FALSE,
                          fileEncoding="UTF-8-BOM")
    if(!all(c("category_id","mechanism_id","mechanism_source_label")%in%names(onto))||
       anyDuplicated(onto$mechanism_id))
      stop("Invalid approved W07 ontology in ",folder)
    manifest<-jsonlite::fromJSON(m,simplifyVector=FALSE)
    if(!is.null(manifest$ontology_sha256)&&!identical(manifest$ontology_sha256,sha(o)))
      stop("Ontology source checksum mismatch: ",folder)
    records<-readjl(c)
    ids<-vapply(records,function(x)x$record_id,character(1))
    if(anyDuplicated(ids))stop("Duplicated W07 publication records: ",folder)
    list(folder=folder,ont=onto,ont_hash=sha(o),records=records,
         coding_hash=sha(c))
  }
  targeted<-load(targeted_w07)
  expanded<-load(expanded_w07)
  if(!identical(targeted$ont_hash,expanded$ont_hash))
    stop("Two W07 searches use different approved ontology files. Reconcile ontology versions before combining.")
  onto<-targeted$ont
  known<-as.character(onto$mechanism_id)
  tally<-function(run) {
    ids<-unlist(lapply(run$records,function(z) {
      ms<-z$mechanisms
      if(is.null(ms)||!length(ms))return(character())
      unique(vapply(ms,function(v)v$mechanism_id,character(1)))
    }),use.names=FALSE)
    if(any(!ids%in%known))
      stop("W07 coding contains codes absent from the approved ontology.")
    as.integer(table(factor(ids,levels=known)))
  }
  tcount<-tally(targeted);ecount<-tally(expanded)
  value<-function(x)if(is.null(x)||!length(x))"" else
    paste(as.character(unlist(x,use.names=FALSE)),collapse="; ")
  category_field<-intersect(c("category_label","category_name","category_title","category_source_label"),names(onto))
  category_names<-setNames(vapply(unique(onto$category_id),function(id){
    z<-onto[onto$category_id==id,,drop=FALSE]
    if(length(category_field)){
      name<-trimws(as.character(z[[category_field[[1L]]]][[1L]]))
      if(!is.na(name)&&nzchar(name))return(name)
    }
    paste(head(unique(as.character(z$mechanism_source_label)),3L),collapse=" · ")
  },character(1)),unique(onto$category_id))
  onto_rows<-lapply(seq_len(nrow(onto)),function(i) {
    z<-onto[i,,drop=FALSE]
    extras<-as.list(z[setdiff(names(z),c("category_id","mechanism_id",
                                            "mechanism_source_label")),drop=FALSE])
    extras<-lapply(extras,value)
    list(id=as.character(z$mechanism_id),
         category=as.character(z$category_id),
         category_label=unname(category_names[[as.character(z$category_id)]]),
         label=as.character(z$mechanism_source_label),
         details=value(extras[grep("definition|description|example|scope",
                                   names(extras),ignore.case=TRUE)]),
         metadata=extras,
         counts=list(targeted=tcount[[i]],expanded=ecount[[i]]))
  })
  candidates<-list()
  for(src in c("targeted","expanded")) {
    run<-if(src=="targeted")targeted else expanded
    for(z in run$records) {
      if(!isTRUE(z$ontology_gap))next
      candidates[[length(candidates)+1L]]<-list(
        source=src,description=value(z$proposed_new_mechanism),
        reason=value(z$ontology_gap_reason),
        stage=value(z$initiative_stage),category="")
    }
  }
  harmonised<-list()
  harmonised_hash<-""
  if(!is.null(harmonised_csv)) {
    harmonised_csv<-normalizePath(harmonised_csv,mustWork=TRUE)
    review<-utils::read.csv(harmonised_csv,stringsAsFactors=FALSE,check.names=FALSE)
    required<-c("proposed_label","working_definition","hierarchy_action",
                "parent_category","source_candidate_ids","review_status")
    if(!all(required%in%names(review)))
      stop("Harmonised dataset is missing: ",
           paste(setdiff(required,names(review)),collapse=", "))
    if(anyNA(review$proposed_label)||any(!nzchar(trimws(review$proposed_label))))
      stop("Harmonised labels must be non-empty.")
    ids<-vapply(seq_along(candidates),function(i)paste0("W07-",sprintf("%04d",i)),character(1))
    for(i in seq_len(nrow(review))) {
      row<-as.list(review[i,,drop=FALSE])
      linked<-trimws(strsplit(as.character(row$source_candidate_ids),";",fixed=TRUE)[[1L]])
      if(!length(linked)||any(!linked%in%ids))
        stop("Unknown original candidate ID in harmonised dataset row ",i)
      if(!as.character(row$hierarchy_action)%in%c("existing_mechanism","refine_existing",
          "new_lower_level","new_higher_level"))
        stop("Invalid hierarchy action in row ",i)
      harmonised[[i]]<-lapply(row,as.character)
    }
    harmonised_hash<-sha(harmonised_csv)
  }
  data<-list(schema="ostara-mechanism-meeting-v1",
    ontology=onto_rows,candidates=candidates,harmonised=harmonised,
    ontology_sha256=targeted$ont_hash,
    sources=list(targeted=list(path=targeted$folder,records=length(targeted$records),
                               coding_sha256=targeted$coding_hash),
                 expanded=list(path=expanded$folder,records=length(expanded$records),
                               coding_sha256=expanded$coding_hash)))
  template<-system.file("mechanisms","typology.html",package="evidencelandscaper")
  if(!nzchar(template))stop("Mechanism meeting template not installed.")
  html<-paste(readLines(template,warn=FALSE,encoding="UTF-8"),collapse="\n")
  json<-as.character(jsonlite::toJSON(data,auto_unbox=TRUE,null="null"))
  json<-gsub("</script","<\\/script",json,fixed=TRUE)
  html<-paste(strsplit(html,"__MECHANISM_DATA__",fixed=TRUE)[[1L]],collapse=json)
  if(is.null(output_file)) {
    dir<-file.path(dirname(dirname(dirname(targeted$folder))),"reports","mechanism_typology")
    dir.create(dir,recursive=TRUE,showWarnings=FALSE)
    key<-substr(digest::digest(c(targeted$ont_hash,targeted$coding_hash,
      expanded$coding_hash,harmonised_hash,sha(template)),algo="sha256"),1L,12L)
    output_file<-file.path(dir,paste0("ostara_mechanisms_",key,".html"))
  } else {
    dir.create(dirname(output_file),recursive=TRUE,showWarnings=FALSE)
  }
  if(file.exists(output_file)) {
    prev<-paste(readLines(output_file,warn=FALSE,encoding="UTF-8"),collapse="\n")
    if(!identical(prev,html))stop("Existing meeting webpage differs; refusing overwrite.")
  } else writeLines(html,output_file,useBytes=TRUE)
  message("[Ostara mechanisms] ",nrow(onto)," ontology mechanisms; ",
    length(candidates)," candidate suggestions; HTML: ",output_file)
  invisible(output_file)
}
