#' Build an evidence-rich full-text atlas prototype from OSTARA client CSVs
#'
#' A versioned, standalone HTML prototype. Uses the extracted W09 case schema
#' in the evidence-rich case CSV, not the historical W09 atlas object schema.
#' The map generates auditable approximate gazetteer coordinates automatically. No LLM/API
#' calls and no overwrite of the historical atlas or case extraction outputs.
#'
#' @param map_path OSTARA project root.
#' @param client_export_dir A completed export_ostara_client_databases folder.
#'   Defaults to the most recently modified complete export.
#' @param coordinates_csv Optional externally reviewed coordinate override with
#'   case_id,latitude,longitude,coordinate_basis,coordinate_source. When NULL,
#'   generates approximate points automatically from extracted geography.
#' @param output_dir Optional new empty output directory; defaults to a versioned
#'   directory under reports/fulltext_atlas_prototype.
#' @return Path of generated standalone evidence_atlas.html, invisibly.
#' @export
export_ostara_fulltext_atlas_prototype <- function(
    map_path=NULL,client_export_dir=NULL,coordinates_csv=NULL,output_dir=NULL) {
  if(is.null(map_path))map_path<-open_map("Ostara_collective_action_map")
  map_path<-normalizePath(map_path,winslash="/",mustWork=TRUE)
  if(!requireNamespace("jsonlite",quietly=TRUE)||
     !requireNamespace("digest",quietly=TRUE)||
     !requireNamespace("maps",quietly=TRUE))
    stop("Install jsonlite, digest and maps.")
  csv_names<-c("ostara_publications.csv",
    "ostara_cases_with_adjacent_evidence.csv",
    "ostara_fulltext_mechanisms_with_adjacent_evidence.csv")
  if(is.null(client_export_dir)) {
    base<-file.path(map_path,"reports","client_databases")
    if(!dir.exists(base))stop("No client database export found: ",base)
    candidates<-list.dirs(base,recursive=FALSE,full.names=TRUE)
    candidates<-candidates[vapply(candidates,function(p)
      all(file.exists(file.path(p,csv_names))),logical(1))]
    if(!length(candidates))
      stop("No complete client database export found under ",base)
    client_export_dir<-candidates[order(file.info(candidates)$mtime,
      decreasing=TRUE)][[1L]]
  }
  client_export_dir<-normalizePath(client_export_dir,winslash="/",mustWork=TRUE)
  files<-file.path(client_export_dir,csv_names)
  if(any(!file.exists(files)))
    stop("Client export missing: ",paste(basename(files[!file.exists(files)]),
                                      collapse=", "))
  readcsv<-function(path)utils::read.csv(path,stringsAsFactors=FALSE,
    check.names=FALSE,fileEncoding="UTF-8-BOM",na.strings=NULL)
  pubs<-readcsv(files[[1L]])
  cases<-readcsv(files[[2L]])
  mechanisms<-readcsv(files[[3L]])
  if(!all(c("record_id","title","doi","cohort")%in%names(pubs))||
     anyDuplicated(pubs$record_id))
    stop("Invalid publication register.")
  if(!all(c("record_id","case_id","cohort")%in%names(cases))||
     !nrow(cases)||anyDuplicated(cases$case_id)||
     any(!cases$record_id%in%pubs$record_id))
    stop("Invalid or empty case database.")
  if(any(cases$cohort!=pubs$cohort[match(cases$record_id,pubs$record_id)]))
    stop("Case cohort differs from publication register.")
  if(!all(c("record_id","mechanism_id","status","fine_level_mechanism",
             "evidence_quote","evidence_anchor")%in%names(mechanisms)))
    stop("Mechanism database missing required columns.")
  field_cols<-grep("^field_[0-9]{3} :: ",names(cases),value=TRUE)
  field_cols<-field_cols[!grepl(" \\[(evidence|reporting status)\\]$",field_cols)]
  if(!length(field_cols))
    stop("No field_### :: Question columns found in case database.")
  for(f in field_cols) {
    needed<-c(paste0(f," [evidence]"),
              paste0(f," [reporting status]"))
    if(!all(needed%in%names(cases)))
      stop("Missing adjacent evidence/status columns for ",f)
  }
  # Distinguish schema question text from numerical field index.
  # Do not force a question from one schema into another schema's slot.
  strip_field<-function(x)sub("^field_[0-9]{3} :: ","",x)
  scalar<-function(x) {
    if(is.null(x)||!length(x)||is.na(x))return("")
    as.character(x)
  }
  # Generate offline provisional coordinates unless an explicit reviewed
  # coordinate CSV was provided. Retain an audit for every extracted case.
  geocoding<-NULL
  geo<-NULL
  if(is.null(coordinates_csv)) {
    geocoding<-.ostara_geocode_extracted_cases(cases)
    geo<-geocoding$points
  } else {
    coordinates_csv<-normalizePath(coordinates_csv,winslash="/",mustWork=TRUE)
    geo<-readcsv(coordinates_csv)
    required<-c("case_id","latitude","longitude",
                "coordinate_basis","coordinate_source")
    if(!all(required%in%names(geo))||anyDuplicated(geo$case_id)||
       any(!geo$case_id%in%cases$case_id))
      stop("Coordinates must have unique known case_id and all provenance columns.")
    lat<-suppressWarnings(as.numeric(geo$latitude))
    lon<-suppressWarnings(as.numeric(geo$longitude))
    if(any(!is.finite(lat)|!is.finite(lon)|abs(lat)>90|abs(lon)>180)||
       any(!nzchar(trimws(geo$coordinate_basis))|
           !nzchar(trimws(geo$coordinate_source))))
      stop("Coordinates include invalid or undocumented latitude/longitude.")
    geo$latitude<-lat
    geo$longitude<-lon
  }
  mechanism_by_id<-split(mechanisms,mechanisms$record_id)
  result<-vector("list",nrow(cases))
  for(i in seq_len(nrow(cases))) {
    r<-cases[i,,drop=FALSE]
    id<-as.character(r$record_id[[1L]])
    pub<-pubs[match(id,pubs$record_id),,drop=FALSE]
    fields<-lapply(field_cols,function(f) {
      value<-scalar(r[[f]][[1L]])
      status<-scalar(r[[paste0(f," [reporting status]")]][[1L]])
      evidence<-scalar(r[[paste0(f," [evidence]")]][[1L]])
      # Old checkpoint schemas may lack a field; skip only genuine absences.
      if(!nzchar(value)&&!nzchar(status)&&!nzchar(evidence))return(NULL)
      list(question=strip_field(f),value=value,
           evidence=evidence,status=if(nzchar(status))status else "unknown")
    })
    fields<-Filter(Negate(is.null),fields)
    # A meaningful display title comes only from relevant reported schema answers.
    matches<-which(vapply(fields,function(f)
      grepl("(name of (the )?(case|initiative)|name of (the )?(collective|organisation)|case name|initiative name)",
             f$question,ignore.case=TRUE)&&
      identical(f$status,"reported")&&nzchar(f$value),logical(1)))
    display_name<-if(length(matches))fields[[matches[[1L]]]]$value else
      paste0("Case ",scalar(r$case_id[[1L]]))
    ms<-mechanism_by_id[[id]]
    mrows<-list()
    if(!is.null(ms)&&nrow(ms)) {
      ms<-ms[ms$status=="implemented_case",,drop=FALSE]
      for(k in seq_len(nrow(ms)))mrows[[k]]<-list(
        code=scalar(ms$mechanism_id[[k]]),
        label=scalar(ms$fine_level_mechanism[[k]]),
        status=scalar(ms$status[[k]]),
        evidence=paste(c(scalar(ms$evidence_anchor[[k]]),
                         scalar(ms$evidence_quote[[k]])),collapse=": "))
    }
    coords<-NULL
    if(nrow(geo)) {
      k<-match(scalar(r$case_id[[1L]]),geo$case_id)
      if(!is.na(k))coords<-list(latitude=geo$latitude[[k]],
        longitude=geo$longitude[[k]],
        coordinate_basis=scalar(geo$coordinate_basis[[k]]),
        coordinate_source=scalar(geo$coordinate_source[[k]]))
    }
    doi<-trimws(scalar(pub$doi[[1L]]))
    doi<-sub("^https?://(dx\\.)?doi\\.org/","",doi,ignore.case=TRUE)
    doi<-sub("^doi:\\s*","",doi,ignore.case=TRUE)
    result[[i]]<-list(
      record_id=id,case_id=scalar(r$case_id[[1L]]),
      cohort=scalar(r$cohort[[1L]]),
      title=scalar(pub$title[[1L]]),
      doi_url=if(nzchar(doi))paste0("https://doi.org/",
        utils::URLencode(doi,reserved=FALSE)) else "",
      display_name=display_name,
      assessment_reason=if("assessment_reason"%in%names(r))
        scalar(r$assessment_reason[[1L]]) else "",
      boundary_uncertainty=if("case_boundary_uncertainty"%in%names(r))
        scalar(r$case_boundary_uncertainty[[1L]]) else "",
      fields=fields,mechanisms=mrows,coordinates=coords)
  }
  payload<-list(schema="ostara-fulltext-atlas-prototype-v1",
    publications=length(unique(cases$record_id)),
    fields=length(field_cols),cases=result)
  tpl<-system.file("atlas","ostara_fulltext_prototype.html",
                   package="evidencelandscaper")
  if(!nzchar(tpl)||!file.exists(tpl))
    stop("Atlas prototype template missing from installed package.")
  html<-paste(readLines(tpl,warn=FALSE,encoding="UTF-8"),collapse="\n")
  data<-as.character(jsonlite::toJSON(payload,auto_unbox=TRUE,
                                      null="null",digits=NA))
  data<-gsub("</script","<\\\\/script",data,fixed=TRUE)
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
    paths<-c(paths,paste0('<polyline class="land" points="',
      paste(sprintf("%.1f,%.1f",xx,yy),collapse=" "),'"/>'))
  }
  grid<-c(vapply(seq(0,1000,125),function(x)
    paste0('<line class="graticule" x1="',x,
      '" y1="0" x2="',x,'" y2="510"/>'),character(1)),
    vapply(seq(0,510,85),function(y)
      paste0('<line class="graticule" x1="0" y1="',y,
       '" x2="1000" y2="',y,'"/>'),character(1)))
  if(!grepl("__MAP_PATHS__",html,fixed=TRUE)||
     !grepl("__ATLAS_DATA__",html,fixed=TRUE))
    stop("Atlas template placeholders missing.")
  # Use literal placeholder splitting: JSON can contain backslashes and
  # replacement strings in gsub() would interpret them.
  inject<-function(html,token,value) {
    pieces<-strsplit(html,token,fixed=TRUE)[[1L]]
    if(length(pieces)!=2L)stop("Expected one template token: ",token)
    paste(pieces,collapse=value)
  }
  html<-inject(html,"__MAP_PATHS__",paste(c(grid,paths),collapse="\n"))
  html<-inject(html,"__ATLAS_DATA__",data)
  if(is.null(output_dir)) {
    key<-substr(digest::digest(c(
      vapply(files,function(p)digest::digest(file=p,algo="sha256",
                                             serialize=FALSE),character(1)),
      if(is.null(coordinates_csv))
        digest::digest(geo,algo="sha256") else
        digest::digest(file=coordinates_csv,algo="sha256",serialize=FALSE),
      digest::digest(file=tpl,algo="sha256",serialize=FALSE)),
      algo="sha256"),1L,12L)
    output_dir<-file.path(map_path,"reports","fulltext_atlas_prototype",key)
  }
  if(dir.exists(output_dir)&&length(list.files(output_dir,all.files=TRUE,no..=TRUE))) {
    existing_html<-file.path(output_dir,"evidence_atlas.html")
    existing_prov<-file.path(output_dir,"provenance.json")
    if(file.exists(existing_html)&&file.exists(existing_prov)) {
      old<-jsonlite::fromJSON(existing_prov,simplifyVector=FALSE)
      same<-identical(old$output_sha256,
        digest::digest(file=existing_html,algo="sha256",serialize=FALSE))&&
        identical(old$output_sha256,
          digest::digest(html,algo="sha256",serialize=FALSE))
      if(same) {
        message("[OSTARA atlas prototype] Reusing identical existing atlas: ",
                existing_html)
        return(invisible(existing_html))
      }
    }
    stop("Output folder is populated with different files; refusing overwrite: ",
         output_dir)
  }
  if(!dir.exists(output_dir)&&
     !dir.create(output_dir,recursive=TRUE,showWarnings=FALSE))
    stop("Unable to create output folder: ",output_dir)
  html_file<-file.path(output_dir,"evidence_atlas.html")
  writeLines(html,html_file,useBytes=TRUE)
  utils::write.csv(geo,file.path(output_dir,"atlas_coordinates.csv"),
                   row.names=FALSE,na="")
  if(!is.null(geocoding))
    utils::write.csv(geocoding$audit,
      file.path(output_dir,"atlas_geocoding_audit.csv"),
      row.names=FALSE,na="")
  provenance<-list(schema=payload$schema,source_dir=client_export_dir,
    source_files=as.list(stats::setNames(vapply(files,function(p)
      digest::digest(file=p,algo="sha256",serialize=FALSE),character(1)),
      basename(files))),
    coordinates_csv=if(is.null(coordinates_csv))
      "generated_offline_from_extracted_geography" else coordinates_csv,
    mapped_cases=nrow(geo),unmapped_cases=nrow(cases)-nrow(geo),
    coordinate_file_sha256=digest::digest(
      file=file.path(output_dir,"atlas_coordinates.csv"),
      algo="sha256",serialize=FALSE),
    output_sha256=digest::digest(file=html_file,algo="sha256",serialize=FALSE),
    cases=nrow(cases),publications=length(unique(cases$record_id)),
    note=paste("Approximate offline gazetteer positions generated from",
               "extracted country and unambiguous localities;",
               "country representative points are NOT actual site locations."))
  jsonlite::write_json(provenance,file.path(output_dir,"provenance.json"),
                        pretty=TRUE,auto_unbox=TRUE,null="null")
  message("[OSTARA atlas prototype] ",nrow(cases)," cases across ",
    length(unique(cases$record_id))," publications: ",html_file)
  message("[OSTARA atlas prototype] ",nrow(geo),"/",nrow(cases),
    " cases mapped (approximate); coordinate CSV and audit saved in output folder.")
  invisible(html_file)
}
