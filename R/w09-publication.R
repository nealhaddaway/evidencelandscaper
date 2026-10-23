#' Republish the provenance-linked W09 atlas as HTML
#' @param w09 Completed W09 run.
#' @param extraction_dir Selected W09 V2 extraction directory.
#' @param coordinates_csv Optional reviewed case coordinates.
#' @return Path to the versioned HTML.
#' @export
republish_w09_atlas <- function(w09,extraction_dir,coordinates_csv=NULL) {
  root <- export_w09_evidence_atlas(w09,extraction_dir,coordinates_csv)
  data_path <- file.path(root,"case_evidence.json")
  manifest <- jsonlite::fromJSON(file.path(root,"export_manifest.json"))
  sha <- function(p)digest::digest(file=p,algo="sha256",serialize=FALSE)
  if(!identical(sha(data_path),manifest$output_sha256$case_evidence))
    stop("Atlas source checksum mismatch.")
  cases <- jsonlite::fromJSON(data_path,simplifyVector=FALSE)$cases
  esc <- function(x) {
    if(is.null(x)||!length(x)||is.na(x[1]))return("")
    x<-as.character(x[1]);x<-gsub("&","&amp;",x,fixed=TRUE)
    x<-gsub("<","&lt;",x,fixed=TRUE);x<-gsub(">","&gt;",x,fixed=TRUE)
    gsub('"',"&quot;",x,fixed=TRUE)
  }
  fmt <- function(z) paste(unlist(z,use.names=FALSE),collapse="; ")
  detail <- function(ca) {
    z<-ca$case;p<-ca$publication;k<-ca$coordinates
    s<-paste0("<h2>",esc(z$case_name),"</h2><p>",esc(z$case_description),"</p>",
      "<p><b>Publication:</b> ",esc(p$title),"</p>",
      if(!is.null(p$doi_url))paste0('<a target="_blank" href="',esc(p$doi_url),'">DOI: ',esc(p$doi),"</a> · ")else "",
      '<a target="_blank" href="',esc(p$google_scholar_url),'">Google Scholar title search</a>',
      "<p><b>Record:</b> ",esc(p$record_id),"<br><b>Observation:</b> ",esc(ca$observation_id),"</p>",
      "<p><b>Objectives:</b> ",esc(fmt(z$objectives)),"</p>",
      "<p><b>Actors:</b> ",esc(fmt(z$actors)),"</p>",
      "<p><b>Activities:</b> ",esc(fmt(z$collaboration_activities)),"</p>",
      "<p><b>Stages:</b> ",esc(fmt(z$supply_chain_stages)),"</p>",
      "<p><b>Mechanisms:</b> ",esc(paste(vapply(z$additional_mechanisms,function(x)x$mechanism_id,character(1)),collapse=", ")),"</p>",
      "<p><b>Coordinate basis:</b> ",esc(k$coordinate_basis),"</p>",
      "<p><b>Coordinate source:</b> ",esc(k$coordinate_source),"</p>")
    for(x in c(z$additional_mechanisms,z$author_conclusions)) {
      s<-paste0(s,"<p>",esc(x$mechanism_id),": ",esc(x$conclusion),
        "<br><i>",esc(x$source_quote),"</i> ",esc(x$source_anchor),"</p>")
    }
    paste0(s,"<p class='trace'>Source SHA-256: ",esc(p$source_sha256),
           "<br>Prompt SHA-256: ",esc(ca$extraction$prompt_sha256),"</p>")
  }
  markers <- lapply(cases,function(ca){
    list(lat=ca$coordinates$latitude,lon=ca$coordinates$longitude,
      label=ca$case$case_name,html=detail(ca))
  })
  payload <- jsonlite::toJSON(markers,auto_unbox=TRUE,null="null")
  payload <- gsub("</script","<\\/script",payload,fixed=TRUE)
  html <- paste0('<!doctype html><html lang="en"><head><meta charset="UTF-8">',
   '<meta name="viewport" content="width=device-width, initial-scale=1">',
   '<title>Ostara | Evidence atlas</title>',
   '<link rel="stylesheet" href="https://unpkg.com/leaflet@1.9.4/dist/leaflet.css">',
   '<style>body{margin:0;background:#f6f7f3;color:#243b37;font:15px system-ui,sans-serif}',
   'header{padding:22px 32px;background:white;border-bottom:1px solid #dce3da}',
   'h1{font:32px Georgia,serif;margin:0 0 7px}#map{height:72vh}',
   '.wrap{padding:25px 35px}.list{columns:2}.list article{break-inside:avoid;padding:12px;border-bottom:1px solid #ddd}',
   '.list h2{font:19px Georgia,serif}.trace{font-size:11px;color:#65736c;overflow-wrap:anywhere}',
   '.leaflet-popup-content{max-height:58vh;overflow-y:auto;width:330px!important}',
   'a{color:#a85332}small{color:#65736c}</style></head><body>',
   '<header><small>OSTARA / EVIDENCE SYNTHESIS</small><h1>Collective action evidence atlas</h1>',
   '<small>',length(cases),' publication-case observations. Points show indicative, source-qualified geography.</small></header>',
   '<div id="map"></div><div class="wrap"><h2>All case observations</h2><div class="list">',
   paste(vapply(cases,function(x)paste0("<article>",detail(x),"</article>"),character(1)),collapse=""),
   '</div></div><script src="https://unpkg.com/leaflet@1.9.4/dist/leaflet.js"></script>',
   '<script id="atlas-data" type="application/json">',payload,'</script>',
   '<script>if(typeof L!=="undefined"){',
   'let map=L.map("map").setView([20,0],2);',
   'L.tileLayer("https://tile.openstreetmap.org/{z}/{x}/{y}.png",{maxZoom:18,attribution:"© OpenStreetMap contributors"}).addTo(map);',
   'let points=JSON.parse(document.getElementById("atlas-data").textContent);',
   'points.forEach(p=>{if(p.lat!=null&&p.lon!=null){',
   'L.circleMarker([p.lat,p.lon],{radius:7,color:"#ffffff",weight:1.4,fillColor:"#c7643c",fillOpacity:0.95})',
   '.addTo(map).bindPopup(p.html,{maxWidth:370});}});',
   '}else{document.getElementById("map").innerHTML="<p>Map unavailable offline. Case evidence and links remain below.</p>";}',
   '</script></body></html>')
  output <- file.path(root,"evidence_atlas.html")
  if(file.exists(output)){
    if(!identical(paste(readLines(output,warn=FALSE,encoding="UTF-8"),collapse="\n"),html))
      stop("Archived atlas HTML conflicts with current renderer: ",output)
  } else writeLines(html,output,useBytes=TRUE)
  message("[W09 atlas] ",length(cases),"/",length(cases)," published: ",output)
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
