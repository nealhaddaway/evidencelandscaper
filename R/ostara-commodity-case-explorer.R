#' Build an independent, commodity-centred OSTARA case explorer
#'
#' Reads all saved, extracted W09 cases from the client database export.
#' No geographic filtering or coordinates are required. All cases remain
#' accessible, including those with no codable commodity.
#' Produces a versioned standalone HTML with an evidence-rich full-case panel.
#' Does not modify the original case ledger, atlas or approved codebook.
#'
#' @param map_path OSTARA project directory.
#' @param client_export_dir Completed client database export; when NULL,
#'   selects the most recently modified complete export.
#' @param output_dir New empty output directory; when NULL, writes under
#'   reports/commodity_case_explorer/<content_sha>.
#' @param atlas_url Optional URL for a separate geographic atlas navigation
#'   link; never embedded in the explorer.
#' @return Standalone HTML output path, invisibly.
#' @export
export_ostara_commodity_case_explorer <- function(
    map_path=NULL,client_export_dir=NULL,output_dir=NULL,atlas_url=NULL) {
  if(is.null(map_path))map_path<-open_map("Ostara_collective_action_map")
  map_path<-normalizePath(map_path,winslash="/",mustWork=TRUE)
  if(!requireNamespace("jsonlite",quietly=TRUE)||
     !requireNamespace("digest",quietly=TRUE)||
     !requireNamespace("countrycode",quietly=TRUE))
    stop("Please install jsonlite, digest and countrycode.")
  inputs<-c("ostara_cases_with_adjacent_evidence.csv",
            "ostara_publications.csv")
  if(is.null(client_export_dir)) {
    base<-file.path(map_path,"reports","client_databases")
    if(!dir.exists(base))stop("Client export directory missing: ",base)
    folders<-list.dirs(base,recursive=FALSE,full.names=TRUE)
    folders<-folders[vapply(folders,function(p)
      all(file.exists(file.path(p,inputs))),logical(1))]
    if(!length(folders))stop("No complete client case export found in ",base)
    client_export_dir<-folders[order(file.info(folders)$mtime,
      decreasing=TRUE)[[1L]]]
  }
  client_export_dir<-normalizePath(client_export_dir,winslash="/",mustWork=TRUE)
  input_paths<-file.path(client_export_dir,inputs)
  if(any(!file.exists(input_paths)))
    stop("Missing client export CSV: ",
         paste(basename(input_paths[!file.exists(input_paths)]),collapse=", "))
  readcsv<-function(path)utils::read.csv(path,check.names=FALSE,
    stringsAsFactors=FALSE,fileEncoding="UTF-8-BOM",na.strings=NULL)
  cases<-readcsv(input_paths[[1L]])
  pubs<-readcsv(input_paths[[2L]])
  if(!nrow(cases)||!all(c("record_id","case_id","cohort")%in%names(cases))||
     anyDuplicated(cases$case_id)||any(!nzchar(cases$case_id)))
    stop("Invalid or empty case extraction table.")
  if(!all(c("record_id","title","doi","cohort")%in%names(pubs))||
     anyDuplicated(pubs$record_id)||any(!cases$record_id%in%pubs$record_id))
    stop("Invalid publication register or unknown record IDs.")
  if(any(cases$cohort!=pubs$cohort[match(cases$record_id,pubs$record_id)]))
    stop("Case/publication cohort mismatch.")
  base_fields<-grep("^field_[0-9]{3} :: ",names(cases),value=TRUE)
  base_fields<-base_fields[!grepl(" \\[(evidence|reporting status)\\]$",
                                base_fields)]
  if(!length(base_fields))
    stop("Case table does not contain new W09 question columns.")
  for(f in base_fields) {
    if(!all(paste0(f,c(" [evidence]"," [reporting status]"))%in%names(cases)))
      stop("Missing evidence/status columns for ",f)
  }
  # No conversion is applied to raw extracted answers or quotations.
  scalar<-function(x) {
    if(is.null(x)||!length(x))return("")
    v<-x[[1L]]
    if(is.na(v))return("")
    as.character(v)
  }
  split_values<-function(x) {
    parts<-unlist(strsplit(scalar(x),"\\s*[;|\\n]\\s*",perl=TRUE))
    parts<-trimws(parts)
    unique(parts[nzchar(parts)])
  }
  normalise_commodity<-function(v) {
    v<-trimws(as.character(v))
    key<-tolower(gsub("\\s+"," ",v))
    key<-gsub("\\s*/\\s*","/",key)
    if(key%in%c("cocoa","cacao","cocoa/cacao","cacao/cocoa"))
      return("Cocoa")
    if(key%in%c("soy","soya","soybean","soybeans","soy bean","soy beans"))
      return("Soybeans")
    if(key%in%c("milk","dairy","dairy products"))
      return("Dairy")
    # Palm and palm oil remain distinct: palm alone is taxonomically ambiguous.
    labels<-c(bananas="Bananas",banana="Bananas",beef="Beef",
      cassava="Cassava",coffee="Coffee",cotton="Cotton",maize="Maize",
      palm="Palm", "palm oil"="Palm oil",
      "pulp and paper"="Pulp and paper",rice="Rice",rubber="Rubber",
      sugar="Sugar",sugarcane="Sugarcane",tea="Tea",timber="Timber")
    if(key%in%names(labels))return(unname(labels[[key]]))
    if(nzchar(v))paste0(toupper(substr(v,1,1)),substring(v,2)) else ""
  }
  normalise_country<-function(x) {
    x<-trimws(as.character(x))
    code<-toupper(x)
    if(grepl("^[A-Z]{3}$",code)) {
      resolved<-suppressWarnings(countrycode::countrycode(
        code,"iso3c","country.name",warn=FALSE))
      if(length(resolved)==1L&&!is.na(resolved))return(resolved)
    }
    x
  }
  # Schema-driven facet classification. A field is coded only if its archived
  # question text matches the facet; no free text is interpreted as a country.
  facet_for<-function(question) {
    q<-tolower(trimws(question))
    if(grepl("\\b(crop|crops|commodity|commodities)\\b",q,perl=TRUE))
      return("commodity")
    if(grepl("\\b(country|countries)\\b",q,perl=TRUE))
      return("country")
    if(grepl("supply.chain stages|value.chain stages",q))
      return("stage")
    if(grepl("what is the status|case status|initiative status",q))
      return("status")
    if(grepl("level at which|collective.action level",q))
      return("level")
    if(grepl("shared interest",q))
      return("interest")
    ""
  }
  question<-function(x)sub("^field_[0-9]{3} :: ","",x)
  groups<-vapply(base_fields,function(f)facet_for(question(f)),character(1))
  for(mandatory in c("commodity","country","stage")) {
    if(!any(groups==mandatory))
      warning("No archived W09 question mapped to explorer facet: ",mandatory)
  }
  rows<-vector("list",nrow(cases))
  all_commodities<-character()
  for(i in seq_len(nrow(cases))) {
    ca<-cases[i,,drop=FALSE]
    id<-scalar(ca$record_id)
    pub<-pubs[match(id,pubs$record_id),,drop=FALSE]
    fields<-list()
    facets<-setNames(replicate(6L,character(),simplify=FALSE),
      c("commodity","country","stage","status","level","interest"))
    for(j in seq_along(base_fields)) {
      f<-base_fields[[j]]
      value<-scalar(ca[[f]])
      status<-scalar(ca[[paste0(f," [reporting status]")]])
      evidence<-scalar(ca[[paste0(f," [evidence]")]])
      if(!nzchar(value)&&!nzchar(status)&&!nzchar(evidence))next
      title<-question(f)
      category<-groups[[j]]
      display_value<-value
      if(identical(status,"reported")&&nzchar(category)) {
        parts<-split_values(value)
        if(category=="commodity") {
          parts<-vapply(parts,normalise_commodity,character(1))
        } else if(category=="country") {
          parts<-vapply(parts,normalise_country,character(1))
        }
        parts<-unique(parts[nzchar(parts)])
        facets[[category]]<-unique(c(facets[[category]],parts))
        if(category%in%c("commodity","country"))
          display_value<-paste(parts,collapse="; ")
      }
      fields[[length(fields)+1L]]<-list(
        question=title,value=value,display_value=display_value,
        status=if(nzchar(status))status else "unknown",
        evidence=evidence)
    }
    all_commodities<-c(all_commodities,facets$commodity)
    find_reported<-function(pattern) {
      j<-which(vapply(fields,function(f)
        identical(f$status,"reported")&&nzchar(f$value)&&
          grepl(pattern,f$question,ignore.case=TRUE,perl=TRUE),logical(1)))
      if(length(j))fields[[j[[1L]]]]$display_value else ""
    }
    name<-find_reported(
      "^(case name|name of (the )?(case|initiative|collective|organisation))\\b")
    if(!nzchar(name))name<-paste0("Case ",scalar(ca$case_id))
    description<-find_reported(
      "(case description|description of (the )?(case|initiative)|brief description)")
    # Do not generate summary prose or substitute objectives as case description.
    doi<-trimws(scalar(pub$doi))
    doi<-sub("^https?://(dx\\.)?doi\\.org/","",doi,ignore.case=TRUE)
    doi<-sub("^doi:\\s*","",doi,ignore.case=TRUE)
    rows[[i]]<-list(
      case_id=scalar(ca$case_id),record_id=id,
      cohort=scalar(ca$cohort),title=scalar(pub$title),
      doi_url=if(nzchar(doi))paste0("https://doi.org/",
        utils::URLencode(doi,reserved=FALSE)) else "",
      name=name,description=description,
      assessment_reason=if("assessment_reason"%in%names(ca))
        scalar(ca$assessment_reason) else "",
      boundary_uncertainty=if("case_boundary_uncertainty"%in%names(ca))
        scalar(ca$case_boundary_uncertainty) else "",
      facets=lapply(facets,as.list),fields=fields)
  }
  all_commodities<-sort(unique(all_commodities[nzchar(all_commodities)]))
  # Only images verified against specific public-domain plate records are
  # included. Other commodity tiles use labelled neutral placeholders.
  image_manifest<-list(
    "Cocoa"=list(
      url="https://commons.wikimedia.org/wiki/Special:FilePath/Theobroma_cacao_-_K%C3%B6hler%E2%80%93s_Medizinal-Pflanzen-137.jpg?width=320",
      source="https://commons.wikimedia.org/wiki/File:Theobroma_cacao_-_K%C3%B6hler%E2%80%93s_Medizinal-Pflanzen-137.jpg",
      creator="Köhler's Medizinal-Pflanzen",rights="Public domain"),
    "Coffee"=list(
      url="https://commons.wikimedia.org/wiki/Special:FilePath/Coffea_arabica_-_K%C3%B6hler%E2%80%93s_Medizinal-Pflanzen-189.jpg?width=320",
      source="https://commons.wikimedia.org/wiki/File:Coffea_arabica_-_K%C3%B6hler%E2%80%93s_Medizinal-Pflanzen-189.jpg",
      creator="Köhler's Medizinal-Pflanzen",rights="Public domain"),
    "Maize"=list(
      url="https://commons.wikimedia.org/wiki/Special:FilePath/K%C3%B6hler%27s_Medizinal-Pflanzen_in_naturgetreuen_Abbildungen_mit_kurz_erl%C3%A4uterndem_Texte_-_Vol._3_-_Plate_78.jpg?width=320",
      source="https://commons.wikimedia.org/wiki/File:K%C3%B6hler%27s_Medizinal-Pflanzen_in_naturgetreuen_Abbildungen_mit_kurz_erl%C3%A4uterndem_Texte_-_Vol._3_-_Plate_78.jpg",
      creator="Köhler's Medizinal-Pflanzen",rights="Public domain"))
  # Additional checked historical plates from the same Köhler series.
  add_plate<-function(commodity,stem) {
    file<-paste0(stem,".jpg")
    url<-paste0("https://commons.wikimedia.org/wiki/Special:FilePath/",
      utils::URLencode(file,reserved=TRUE),"?width=320")
    source<-paste0("https://commons.wikimedia.org/wiki/File:",
      utils::URLencode(file,reserved=TRUE))
    image_manifest[[commodity]]<<-list(url=url,source=source,
      creator="Köhler's Medizinal-Pflanzen",rights="Public domain")
  }
  add_plate("Tea","Camellia sinensis - Köhler–s Medizinal-Pflanzen-025")
  add_plate("Cassava","Manihot esculenta - Köhler–s Medizinal-Pflanzen-090")
  add_plate("Cotton","Gossypium barbadense - Köhler–s Medizinal-Pflanzen-068")
  add_plate("Rice","Oryza sativa - Köhler–s Medizinal-Pflanzen-232")
  add_plate("Rubber","Hevea brasiliensis - Köhler–s Medizinal-Pflanzen-071")
  add_plate("Sugarcane","Saccharum officinarum - Köhler–s Medizinal-Pflanzen-125")
  add_plate("Palm oil","Elaeis guineensis - Köhler–s Medizinal-Pflanzen-056")
  # Different historic sources where a Köhler plate is unavailable.
  add_other_plate<-function(commodity,file,creator,rights) {
    image_manifest[[commodity]]<<-list(
      url=paste0("https://commons.wikimedia.org/wiki/Special:FilePath/",
        utils::URLencode(file,reserved=TRUE),"?width=320"),
      source=paste0("https://commons.wikimedia.org/wiki/File:",
        utils::URLencode(file,reserved=TRUE)),
      creator=creator,rights=rights)
  }
  add_other_plate("Bananas","Musa paradisiaca Blanco1.88-cropped.jpg",
    "Francisco Manuel Blanco, Flora de Filipinas (1880s)","Public domain")
  add_other_plate("Soybeans","Soja d'Étampes Vilmorin-Andrieux 1883.png",
    "Vilmorin-Andrieux, Les Plantes potagères (1883)","Public domain")
  cattle_file<-"Bos taurus Kaapse koe en kalf Caapse koey en kalf (titel op object), RP-T-1914-17-184.jpg"
  add_other_plate("Beef",cattle_file,"Rijksmuseum, 1778","CC0")
  add_other_plate("Dairy",cattle_file,"Rijksmuseum, 1778","CC0")
  image_manifest<-image_manifest[names(image_manifest)%in%all_commodities]
  payload<-list(schema="ostara-commodity-explorer-v1",cases=rows,
    commodities=as.list(all_commodities),artwork=image_manifest,
    atlas_url=if(is.null(atlas_url))"" else atlas_url)
  tpl<-system.file("ostara","commodity_explorer_template.html",
    package="evidencelandscaper")
  if(!nzchar(tpl)||!file.exists(tpl))
    stop("Installed package missing commodity explorer HTML template.")
  template<-paste(readLines(tpl,warn=FALSE,encoding="UTF-8"),collapse="\n")
  if(length(strsplit(template,"__DATA__",fixed=TRUE)[[1L]])!=2L)
    stop("Commodity explorer template must contain one data placeholder.")
  content<-as.character(jsonlite::toJSON(payload,auto_unbox=TRUE,
    null="null",digits=NA))
  content<-gsub("</script","<\\\\/script",content,fixed=TRUE)
  html<-paste(strsplit(template,"__DATA__",fixed=TRUE)[[1L]],
              collapse=content)
  sha_file<-function(p)digest::digest(file=p,algo="sha256",serialize=FALSE)
  if(is.null(output_dir)) {
    key<-substr(digest::digest(c(
      vapply(input_paths,sha_file,character(1)),sha_file(tpl),
      if(is.null(atlas_url))"" else as.character(atlas_url)),
      algo="sha256"),1L,12L)
    output_dir<-file.path(map_path,"reports","commodity_case_explorer",key)
  }
  if(dir.exists(output_dir)&&length(list.files(output_dir,
                                             all.files=TRUE,no..=TRUE))) {
    existing<-file.path(output_dir,"commodity_case_explorer.html")
    if(file.exists(existing)&&identical(paste(readLines(existing,warn=FALSE,
       encoding="UTF-8"),collapse="\n"),html)) {
      message("[OSTARA commodity explorer] Reusing identical export: ",existing)
      return(invisible(existing))
    }
    stop("Output directory already populated; refusing overwrite: ",output_dir)
  }
  if(!dir.exists(output_dir)&&
     !dir.create(output_dir,recursive=TRUE,showWarnings=FALSE))
    stop("Cannot create commodity explorer directory: ",output_dir)
  output<-file.path(output_dir,"commodity_case_explorer.html")
  writeLines(html,output,useBytes=TRUE)
  audit<-data.frame(
    case_id=vapply(rows,function(x)x$case_id,character(1)),
    commodity_count=vapply(rows,function(x)length(x$facets$commodity),integer(1)),
    country_count=vapply(rows,function(x)length(x$facets$country),integer(1)),
    stringsAsFactors=FALSE)
  utils::write.csv(audit,file.path(output_dir,"case_coverage_audit.csv"),
                   row.names=FALSE,na="")
  taxonomy<-data.frame(commodity=all_commodities,
    cases=vapply(all_commodities,function(label)
      sum(vapply(rows,function(x)label%in%x$facets$commodity,logical(1))),
      integer(1)),
    illustration_verified=all_commodities%in%names(image_manifest),
    stringsAsFactors=FALSE)
  utils::write.csv(taxonomy,file.path(output_dir,"commodity_inventory.csv"),
                   row.names=FALSE,na="")
  citation_rows<-lapply(names(image_manifest),function(k)
    data.frame(commodity=k,
       source_url=image_manifest[[k]]$source,
       creator=image_manifest[[k]]$creator,
       rights=image_manifest[[k]]$rights))
  if(length(citation_rows))
    utils::write.csv(do.call(rbind,citation_rows),
      file.path(output_dir,"illustration_sources.csv"),
      row.names=FALSE,na="")
  manifest<-list(schema=payload$schema,
    source_export_dir=client_export_dir,
    source_sha256=as.list(stats::setNames(vapply(input_paths,sha_file,
      character(1)),basename(input_paths))),
    case_count=nrow(cases),
    publications=length(unique(cases$record_id)),
    commodity_count=length(all_commodities),
    cases_without_reported_commodity=sum(audit$commodity_count==0),
    illustration_count=length(image_manifest),
    html_sha256=sha_file(output),
    note=paste("No coordinates or geographic exclusion.",
      "Original extracted values and evidence are preserved.",
      "Illustrations are externally loaded verified Commons files;",
      "all other commodity tiles show a typographic fallback."))
  jsonlite::write_json(manifest,file.path(output_dir,"provenance.json"),
    pretty=TRUE,auto_unbox=TRUE,null="null")
  message("[OSTARA commodity explorer] ",nrow(cases)," cases, ",
    length(unique(cases$record_id))," publications, ",
    length(all_commodities)," commodity categories: ",output)
  invisible(output)
}
