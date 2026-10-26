#' Combine two versioned Ostara case exports in one provenance-labelled atlas
#'
#' Accepts existing case_evidence.json exports, not arbitrary raw extraction
#' directories. Search membership and eligibility basis remain distinct.
#' Cases from different searches are never silently deduplicated or pooled
#' as if they had received the same eligibility review.
#' @param targeted_export Directory containing first-search case_evidence.json
#' @param broad_export Directory containing W10-filtered second-search case_evidence.json
#' @param map_path Ostara evidence map directory, used for versioned output
#' @return Path to the combined HTML atlas
#' @export
combine_ostara_search_atlases <- function(targeted_export,broad_export,map_path=NULL) {
  if(is.null(map_path))map_path<-open_map("Ostara_collective_action_map")
  map_path<-normalizePath(map_path,mustWork=TRUE)
  sha<-function(p)digest::digest(file=p,algo="sha256",serialize=FALSE)
  load<-function(folder) {
    folder<-normalizePath(folder,mustWork=TRUE)
    path<-file.path(folder,"case_evidence.json")
    manifest<-file.path(folder,"export_manifest.json")
    if(!file.exists(path)||!file.exists(manifest))
      stop("Both inputs must be completed, manifest-bearing W09 atlas exports: ",folder)
    m<-jsonlite::fromJSON(manifest,simplifyVector=FALSE)
    if(!identical(m$output_sha256$case_evidence,sha(path)))
      stop("Export provenance check failed: ",folder)
    z<-jsonlite::fromJSON(path,simplifyVector=FALSE)
    if(!identical(z$schema,"w09-linked-evidence-atlas-v2")||!is.list(z$cases))
      stop("Unexpected case atlas data format.")
    list(cases=z$cases,source_path=path,source_manifest=manifest)
  }
  first<-load(targeted_export);second<-load(broad_export)
  if(identical(first$source_path,second$source_path))
    stop("Both exports refer to the same source. Select distinct searches.")
  if(!length(first$cases)||!length(second$cases))
    stop("Both source exports must contain cases.")
  # This checks whether the second export is W10-filtered. Its own manifest
  # hashes the original W10 and second-pass decision files.
  second_manifest<-jsonlite::fromJSON(second$source_manifest,simplifyVector=FALSE)
  input_names<-unlist(second_manifest$input_files,use.names=FALSE)
  if(!"w10_decisions.jsonl"%in%input_names ||
     !"fulltext_decisions.jsonl"%in%input_names)
    stop("Second-search atlas must be a completed, W10 full-text filtered export.")
  sources<-list(
    list(id="targeted",label="Targeted search: collective action × food supply chains",
         eligibility_basis="Exploratory extraction; not assessed under W10 v2"),
    list(id="forest_commodity",label="Expanded search: sustainability × forest commodities",
         eligibility_basis="Provisionally eligible under W10 v2"))
  merged<-list()
  for(i in seq_along(sources)) {
    cases<-if(i==1L)first$cases else second$cases
    for(ca in cases) {
      if(!is.character(ca$observation_id)||length(ca$observation_id)!=1L)
        stop("Source atlas contains invalid observation IDs.")
      ca$source_observation_id<-ca$observation_id
      ca$observation_id<-paste0(sources[[i]]$id,"/",ca$observation_id)
      ca$search_provenance<-sources[[i]]
      merged[[length(merged)+1L]]<-ca
    }
  }
  ids<-vapply(merged,function(ca)ca$observation_id,character(1))
  if(anyDuplicated(ids))stop("Duplicate observation identifiers within a search.")
  if(!requireNamespace("maps",quietly=TRUE))
    stop("Install maps to render the offline world boundary.")
  output_key<-substr(digest::digest(c(sha(first$source_path),sha(first$source_manifest),
    sha(second$source_path),sha(second$source_manifest),
    sha(system.file("atlas","ostara_atlas_template.html",package="evidencelandscaper"))),
    algo="sha256"),1L,12L)
  output<-file.path(map_path,"reports","combined_atlas",output_key)
  dir.create(output,recursive=TRUE,showWarnings=FALSE)
  data<-file.path(output,"case_evidence.json")
  html_path<-file.path(output,"evidence_atlas.html")
  report<-file.path(output,"provenance.json")
  candidate_json<-as.character(jsonlite::toJSON(
    list(schema="ostara-combined-search-atlas-v1",cases=merged),
    auto_unbox=TRUE,null="null",pretty=TRUE))
  if(file.exists(data)) {
    old<-paste(readLines(data,warn=FALSE,encoding="UTF-8"),collapse="\n")
    if(!identical(old,candidate_json))stop("Existing combined atlas data differs; refusing overwrite.")
  } else writeLines(candidate_json,data,useBytes=TRUE)
  template<-system.file("atlas","ostara_atlas_template.html",package="evidencelandscaper")
  html<-paste(readLines(template,warn=FALSE,encoding="UTF-8"),collapse="\n")
  world<-maps::map("world",plot=FALSE,fill=FALSE)
  starts<-c(1L,which(is.na(world$x))+1L)
  ends<-c(which(is.na(world$x))-1L,length(world$x))
  paths<-character()
  for(j in seq_along(starts)) {
    ix<-seq.int(starts[[j]],ends[[j]])
    ix<-ix[is.finite(world$x[ix])&is.finite(world$y[ix])]
    if(length(ix)<2L)next
    xx<-(world$x[ix]+180)*1000/360
    yy<-(90-world$y[ix])*510/180
    if(any(abs(diff(xx))>500))next
    paths<-c(paths,paste0('<polyline class="land" fill="none" points="',
      paste(sprintf("%.1f,%.1f",xx,yy),collapse=" "),'"/>'))
  }
  grid<-c(vapply(seq(0,1000,by=125),function(x)
    paste0('<line class="graticule" x1="',x,'" y1="0" x2="',x,'" y2="510"/>'),
    character(1)),vapply(seq(0,510,by=85),function(y)
    paste0('<line class="graticule" x1="0" y1="',y,'" x2="1000" y2="',y,'"/>'),
    character(1)))
  html<-paste(strsplit(html,"__MAP_PATHS__",fixed=TRUE)[[1L]],
              collapse=paste(c(grid,paths),collapse="\n"))
  html<-paste(strsplit(html,"__CASE_DATA__",fixed=TRUE)[[1L]],
              collapse=gsub("</script","<\\/script",candidate_json,fixed=TRUE))
  if(file.exists(html_path)) {
    old<-paste(readLines(html_path,warn=FALSE,encoding="UTF-8"),collapse="\n")
    if(!identical(old,html))stop("Existing combined HTML differs; refusing overwrite.")
  } else writeLines(html,html_path,useBytes=TRUE)
  provenance<-list(schema="ostara-combined-search-atlas-v1",
    targeted_export=first$source_path,targeted_sha256=sha(first$source_path),
    broad_export=second$source_path,broad_sha256=sha(second$source_path),
    counts=list(targeted=length(first$cases),expanded=length(second$cases),
      total=length(merged)),notes=paste(
      "The two searches used different case-level eligibility methods.",
      "Source observation IDs are namespaced, not reconciled as real-world initiatives.",
      "Cases may overlap across searches."))
  provenance_json<-as.character(jsonlite::toJSON(provenance,auto_unbox=TRUE,pretty=TRUE))
  if(file.exists(report)) {
    old<-paste(readLines(report,warn=FALSE,encoding="UTF-8"),collapse="\n")
    if(!identical(old,provenance_json))
      stop("Existing provenance report differs; refusing overwrite.")
  } else writeLines(provenance_json,report,useBytes=TRUE)
  message("[Ostara combined atlas] ",length(first$cases)," targeted + ",
    length(second$cases)," expanded observations: ",html_path)
  invisible(html_path)
}
