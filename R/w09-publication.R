#' Republish the Ostara atlas with a key-free, bundled world basemap
#'
#' Renders SVG world boundaries into a self-contained HTML file. No external
#' tile service, CDN, network connection or API key is required to view it.
#' Uses the provenance-linked W09 case export and embeds all case details.
#' @param w09 Completed W09 run directory.
#' @param extraction_dir Approved V2.5 extraction directory.
#' @param coordinates_csv Optional audited case-coordinate table.
#' @return Path to versioned standalone HTML atlas.
#' @export
republish_w09_atlas <- function(w09,extraction_dir,coordinates_csv=NULL) {
  if(!requireNamespace("maps",quietly=TRUE))
    stop("Install the R package 'maps' to embed world boundaries: install.packages('maps')")
  root <- export_w09_evidence_atlas(w09,extraction_dir,coordinates_csv)
  src <- file.path(root,"case_evidence.json")
  manifest <- jsonlite::fromJSON(file.path(root,"export_manifest.json"))
  if(!identical(digest::digest(file=src,algo="sha256",serialize=FALSE),
                manifest$output_sha256$case_evidence))
    stop("Publication source checksum mismatch.")
  template <- system.file("atlas","ostara_atlas_template.html",package="evidencelandscaper")
  if(!nzchar(template))stop("Installed offline atlas template missing.")
  html <- paste(readLines(template,warn=FALSE,encoding="UTF-8"),collapse="\n")
  raw_json <- paste(readLines(src,warn=FALSE,encoding="UTF-8"),collapse="\n")
  raw_json <- gsub("</script","<\\/script",raw_json,fixed=TRUE)
  # Generate SVG directly from the maps package's local boundary dataset.
  world <- maps::map("world",plot=FALSE,fill=FALSE)
  starts <- c(1L,which(is.na(world$x))+1L)
  ends <- c(which(is.na(world$x))-1L,length(world$x))
  paths <- character()
  for(j in seq_along(starts)) {
    ix <- seq.int(starts[[j]],ends[[j]])
    ix <- ix[is.finite(world$x[ix]) & is.finite(world$y[ix])]
    if(length(ix)<2L)next
    xx <- (world$x[ix]+180)*1000/360
    yy <- (90-world$y[ix])*510/180
    if(any(abs(diff(xx))>500))next # exclude anti-meridian wrap artifacts
    points <- paste(sprintf("%.1f,%.1f",xx,yy),collapse=" ")
    paths <- c(paths,paste0('<polyline class="land" fill="none" points="',points,'"/>'))
  }
  # Add a few land-graticule references while retaining the local map.
  grid <- c(vapply(seq(0,1000,by=125),function(x)
    paste0('<line class="graticule" x1="',x,'" y1="0" x2="',x,'" y2="510"/>'),character(1)),
    vapply(seq(0,510,by=85),function(y)
    paste0('<line class="graticule" x1="0" y1="',y,'" x2="1000" y2="',y,'"/>'),character(1)))
  html <- sub("__MAP_PATHS__",paste(c(grid,paths),collapse="\n"),html,fixed=TRUE)
  html <- sub("__CASE_DATA__",raw_json,html,fixed=TRUE)
  output <- file.path(root,"evidence_atlas.html")
  # Published files are reproducible and immutable, not silently overwritten.
  if(file.exists(output)) {
    current <- paste(readLines(output,warn=FALSE,encoding="UTF-8"),collapse="\n")
    if(!identical(current,html)) {
      output <- file.path(root,paste0("evidence_atlas-",
        substr(digest::digest(html,algo="sha256"),1,10),".html"))
      if(file.exists(output) &&
         !identical(paste(readLines(output,warn=FALSE,encoding="UTF-8"),collapse="\n"),html))
        stop("Existing versioned atlas differs: ",output)
    }
  }
  if(!file.exists(output))writeLines(html,output,useBytes=TRUE)
  message("[W09 atlas] Standalone basemap and evidence: ",output)
  invisible(output)
}

#' Republish an Ostara W09 evidence report from verified linked cases
#'
#' Reuses the same immutable W09 case-to-publication dataset as the HTML atlas.
#' Produces an independent PDF with descriptive findings, mechanisms, cases and
#' a geographic figure. The full search-to-screening methods report remains
#' a separate upstream audit and is not fabricated from W09 inputs.
#' @inheritParams republish_w09_atlas
#' @return Path to the versioned PDF.
#' @export
republish_w09_report <- function(w09,extraction_dir,coordinates_csv=NULL) {
  root <- export_w09_evidence_atlas(w09,extraction_dir,coordinates_csv)
  m <- jsonlite::fromJSON(file.path(root,"export_manifest.json"))
  json <- file.path(root,"case_evidence.json")
  if(!identical(digest::digest(file=json,algo="sha256",serialize=FALSE),
                m$output_sha256$case_evidence))stop("Report source checksum mismatch.")
  cases <- jsonlite::fromJSON(json,simplifyVector=FALSE)$cases
  output <- file.path(root,"evidence_report.pdf")
  if(file.exists(output)) {
    message("[W09 report] Reusing existing versioned report: ",output)
    return(invisible(output))
  }
  n <- length(cases)
  ids <- vapply(cases,function(x)x$publication$record_id,character(1))
  codes <- unlist(lapply(cases,function(x)vapply(x$case$additional_mechanisms,
                    function(z)z$mechanism_id,character(1))),use.names=FALSE)
  freq <- sort(table(codes),decreasing=TRUE)
  textpage <- function(title, lines) {
    graphics::plot.new()
    graphics::par(usr=c(0,1,0,1))
    graphics::text(.06,.94,title,adj=c(0,1),font=2,cex=1.45,col="#243b37")
    y <- .84
    for(s in lines) {
      for(t in strwrap(s,width=82)) {
        if(y<.08) {graphics::plot.new();y<-.94}
        graphics::text(.06,y,t,adj=c(0,1),cex=.79,col="#243b37")
        y <- y-.029
      }
      y<-y-.026
    }
  }
  grDevices::pdf(output,width=8.27,height=11.69,onefile=TRUE)
  on.exit(grDevices::dev.off(),add=TRUE)
  graphics::par(mar=c(4,4,4,2))
  textpage("Ostara | Collective action evidence landscape",c(
    paste(n,"case observations from",length(unique(ids)),"verified full-text publications."),
    "The map is a descriptive evidence atlas, not an evaluation of programme effectiveness.",
    "Method: Full-text documents were linked by exact record_id to the verified W09 bibliographic inventory. Each source and extraction retained a SHA-256 hash. The versioned W09 model prompt and approved W07 ontology were retained, together with quotations, source anchors and case-specific conclusions.",
    "Cases are publication-level observations; repeated arrangements in different publications have not been merged. Locations are representative, may be approximate and must retain their coordinate provenance.",
    paste("Prompt SHA-256:",cases[[1]]$extraction$prompt_sha256),
    paste("Ontology SHA-256:",cases[[1]]$extraction$ontology_sha256),
    "The full search, deduplication and abstract screening counts require their upstream archived workflow reports; they are not inferred from this full-text subset."
  ))
  if(length(freq)){
    graphics::par(mar=c(5,6,4,2))
    graphics::barplot(rev(freq),horiz=TRUE,las=1,col="#478d7b",
      border=NA,main="Mechanisms reported across extracted cases",
      xlab="Number of case observations")
  }
  lat<-vapply(cases,function(x)if(is.null(x$coordinates$latitude))NA_real_ else x$coordinates$latitude,numeric(1))
  lon<-vapply(cases,function(x)if(is.null(x$coordinates$longitude))NA_real_ else x$coordinates$longitude,numeric(1))
  graphics::par(mar=c(5,5,4,2))
  graphics::plot(lon,lat,xlim=c(-180,180),ylim=c(-90,90),
    xlab="Longitude",ylab="Latitude",main="Evidence atlas | indicative case coordinates",
    pch=21,bg="#c7643c",col="white",cex=1.1)
  graphics::grid(col="#d6dfd9")
  names <- vapply(cases,function(x)if(is.null(x$case$case_name))"Unnamed case" else x$case$case_name,character(1))
  textpage("Publication-linked case inventory",c(
    "Each case is linked to its original publication identifier.",
    paste(seq_len(n),names,paste0("[",ids,"]"),sep=" ")
  ))
  message("[W09 report] ",n,"/",n," cases published: ",output)
  invisible(output)
}
