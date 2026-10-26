#' Prepare provisional coordinates for the W09 atlas from operational geography
#'
#' Matches case locations to the maps world.cities gazetteer within the
#' explicitly coded operational country. If no unambiguous locality match
#' exists, uses a representative city in that country, NOT the initiative's
#' actual location. Produces an accompanying audit for missing/multiple
#' countries. Never changes model extraction outputs.
#' @param extraction_dir Completed W09 extraction directory.
#' @return Path to coordinate CSV (with a companion .audit.csv).
#' @export
prepare_w09_atlas_coordinates <- function(extraction_dir) {
  extraction_dir<-normalizePath(extraction_dir,mustWork=TRUE)
  source<-file.path(extraction_dir,"fulltext_case_registry.jsonl")
  if(!file.exists(source))stop("W09 case registry not found.")
  if(!requireNamespace("maps",quietly=TRUE) ||
     !requireNamespace("countrycode",quietly=TRUE))
    stop("Install maps and countrycode packages.")
  e<-new.env(parent=emptyenv())
  utils::data("world.cities",package="maps",envir=e)
  if(!exists("world.cities",envir=e,inherits=FALSE))
    stop("maps world.cities gazetteer unavailable.")
  cities<-get("world.cities",envir=e)
  required<-c("name","country.etc","lat","long","pop")
  if(!all(required%in%names(cities)))
    stop("Unsupported world.cities gazetteer columns.")
  cities$iso3<-suppressWarnings(countrycode::countrycode(
    as.character(cities$country.etc),"country.name","iso3c",warn=FALSE))
  # Common map-country aliases not recognized by countrycode.
  aliases<-c("USA"="USA","UK"="GBR","Korea, South"="KOR",
             "South Korea"="KOR","Russia"="RUS","Ivory Coast"="CIV")
  ix<-is.na(cities$iso3)&as.character(cities$country.etc)%in%names(aliases)
  cities$iso3[ix]<-unname(aliases[as.character(cities$country.etc[ix])])
  norm<-function(x)trimws(gsub("[^a-z0-9]+"," ",tolower(as.character(x))))
  lines<-readLines(source,warn=FALSE,encoding="UTF-8")
  records<-lapply(lines[nzchar(trimws(lines))],jsonlite::fromJSON,simplifyVector=FALSE)
  sha<-digest::digest(file=source,algo="sha256",serialize=FALSE)
  dest<-file.path(dirname(extraction_dir),paste0("atlas_coordinates_provisional_",substr(sha,1,12),".csv"))
  audit<-sub("[.]csv$",".audit.csv",dest)
  points<-list();notes<-list()
  for(z in records) for(j in seq_along(z$cases)) {
    ca<-z$cases[[j]]
    id<-paste0(z$record_id,"-case-",sprintf("%03d",j))
    cc<-unique(vapply(ca$countries,function(x)
      if(is.null(x$country_iso3))"" else as.character(x$country_iso3),character(1)))
    cc<-cc[nzchar(cc)]
    locs<-ca$locations
    matchrow<-NULL;basis<-"";source_label<-""
    if(length(cc)) {
      for(loc in locs) {
        code<-as.character(loc$country_iso3)
        nm<-as.character(loc$location_name)
        if(length(code)!=1L||length(nm)!=1L||!code%in%cc)next
        hits<-which(cities$iso3==code & !is.na(cities$iso3) &
                    norm(cities$name)==norm(nm))
        if(length(hits)==1L) {
          matchrow<-hits
          basis<-"gazetteer named-locality representative point"
          source_label<-paste0("W09 operational locality: ",nm,
                               " (",code,"); maps world.cities gazetteer; approximate, not site coordinates")
          break
        }
      }
      if(is.null(matchrow)) {
        # Representative country location, never described as precise.
        region<-which(!is.na(cities$iso3)&cities$iso3==cc[[1L]] &
                      is.finite(cities$lat)&is.finite(cities$long))
        if(length(region)) {
          rank<-order(-as.numeric(cities$pop[region]),as.character(cities$name[region]),na.last=TRUE)
          matchrow<-region[rank[[1L]]]
          basis<-"country representative city (not case location)"
          source_label<-paste0("W09 operational country: ",cc[[1L]],
            "; representative city ",cities$name[[matchrow]],
            " from maps world.cities; NOT the initiative location")
        }
      }
    }
    notes[[length(notes)+1L]]<-data.frame(observation_id=id,
      countries=paste(cc,collapse="; "),coded_locations=paste(vapply(locs,function(x)
        as.character(x$location_name),character(1)),collapse="; "),
      georeferenced=!is.null(matchrow),coordinate_basis=basis,
      warning=if(length(cc)>1L)"Multiple operational countries; point represents first only"
              else if(!length(cc))"No coded operational country" else "",
      stringsAsFactors=FALSE)
    if(!is.null(matchrow))
      points[[length(points)+1L]]<-data.frame(observation_id=id,
        latitude=as.numeric(cities$lat[[matchrow]]),
        longitude=as.numeric(cities$long[[matchrow]]),
        coordinate_basis=basis,coordinate_source=source_label,
        stringsAsFactors=FALSE)
  }
  df<-if(length(points))do.call(rbind,points) else
    data.frame(observation_id=character(),latitude=numeric(),longitude=numeric(),
               coordinate_basis=character(),coordinate_source=character())
  adf<-if(length(notes))do.call(rbind,notes) else data.frame()
  if(anyDuplicated(df$observation_id))stop("Duplicate observation IDs.")
  # Preserve previous coordinates rather than overwriting edits.
  if(file.exists(dest)) {
    existing<-utils::read.csv(dest,stringsAsFactors=FALSE)
    if(!identical(existing,df))stop("Provisional coordinate file already differs; refusing overwrite.")
  } else utils::write.csv(df,dest,row.names=FALSE,na="")
  if(file.exists(audit)) {
    existing<-utils::read.csv(audit,stringsAsFactors=FALSE)
    if(!identical(existing,adf))stop("Geocoding audit already differs; refusing overwrite.")
  } else utils::write.csv(adf,audit,row.names=FALSE,na="")
  message("[W09 geocoding] ",nrow(df),"/",nrow(adf),
    " provisional points; see audit: ",audit)
  invisible(dest)
}
