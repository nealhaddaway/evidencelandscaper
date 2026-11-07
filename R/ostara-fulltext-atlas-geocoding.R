#' Resolve approximate map coordinates for full-text OSTARA cases
#'
#' Offline, deterministic gazetteer matching to maps::world.cities. Prefer
#' exact named localities within an explicitly extracted country. Otherwise
#' use a representative city of the country, clearly marked as approximate
#' and NOT as the actual initiative location. Never infer geographic location
#' from title or free-text narrative alone. Ambiguous country mappings are
#' left unmapped and recorded in the audit. Returns coordinates and audit.
#'
#' @param cases Data frame from ostara_cases_with_adjacent_evidence.csv
#' @return List with points, audit.
.ostara_geocode_extracted_cases <- function(cases) {
  if(!requireNamespace("maps",quietly=TRUE)||
     !requireNamespace("countrycode",quietly=TRUE))
    stop("Install packages 'maps' and 'countrycode' for offline atlas geocoding.")
  e<-new.env(parent=emptyenv())
  utils::data("world.cities",package="maps",envir=e)
  if(!exists("world.cities",envir=e,inherits=FALSE))
    stop("maps world.cities gazetteer is unavailable.")
  cities<-get("world.cities",envir=e)
  if(!all(c("name","country.etc","lat","long","pop")%in%names(cities)))
    stop("Unsupported world.cities gazetteer columns.")
  cities$iso3<-suppressWarnings(countrycode::countrycode(
    as.character(cities$country.etc),"country.name","iso3c",warn=FALSE))
  aliases<-c("USA"="USA","UK"="GBR","Korea, South"="KOR",
    "South Korea"="KOR","Russia"="RUS","Ivory Coast"="CIV")
  ix<-is.na(cities$iso3)&as.character(cities$country.etc)%in%names(aliases)
  cities$iso3[ix]<-unname(aliases[as.character(cities$country.etc[ix])])
  normalize<-function(x)trimws(gsub("[^a-z0-9]+"," ",tolower(x)))
  valid3<-unique(stats::na.omit(as.character(cities$iso3)))
  field_cols<-grep("^field_[0-9]{3} :: ",names(cases),value=TRUE)
  country_fields<-field_cols[grepl(
    "(where did the case take place.*country|country|countries)",
    field_cols,ignore.case=TRUE)]
  locality_fields<-field_cols[grepl(
    "(where did the case take place.*(region|province|district|locality|place)|^field_[0-9]{3} :: .*\\b(location|locations|region|province|district|municipality|town|city|village)\\b)",
    field_cols,ignore.case=TRUE,perl=TRUE)]
  locality_fields<-setdiff(locality_fields,country_fields)
  # ISO3 values are preferred. Country names may be converted, but never
  # silently assign a random country when multiple are reported.
  codes_for<-function(v) {
    items<-trimws(unlist(strsplit(as.character(v),"\\s*[;|,\\n]\\s*",perl=TRUE)))
    items<-unique(items[nzchar(items)])
    if(!length(items))return(character())
    upper<-toupper(items)
    cc<-ifelse(grepl("^[A-Z]{3}$",upper),upper,
      suppressWarnings(countrycode::countrycode(items,"country.name","iso3c",
                                                 warn=FALSE)))
    unique(cc[!is.na(cc)&cc%in%valid3])
  }
  # Cities can appear in a list; do not choose fuzzy matches or substrings.
  localities_for<-function(v) {
    x<-trimws(unlist(strsplit(as.character(v),"\\s*[;|\\n]\\s*",perl=TRUE)))
    unique(x[nzchar(x)])
  }
  points<-list();audit<-list()
  for(i in seq_len(nrow(cases))) {
    r<-cases[i,,drop=FALSE]
    id<-as.character(r$case_id[[1L]])
    country_values<-unlist(lapply(country_fields,function(f) {
      st<-as.character(r[[paste0(f," [reporting status]")]][[1L]])
      if(!identical(st,"reported"))return(character())
      as.character(r[[f]][[1L]])
    }),use.names=FALSE)
    codes<-unique(unlist(lapply(country_values,codes_for),
                         use.names=FALSE))
    localities<-unique(unlist(lapply(locality_fields,function(f) {
      st<-as.character(r[[paste0(f," [reporting status]")]][[1L]])
      if(!identical(st,"reported"))return(character())
      localities_for(r[[f]][[1L]])
    }),use.names=FALSE))
    chosen<-NA_integer_;basis<-"";source<-"";warning<-""
    if(length(codes)==1L) {
      code<-codes[[1L]]
      # Only a single exact city candidate in the declared country is safe.
      for(nm in localities) {
        hits<-which(!is.na(cities$iso3)&cities$iso3==code&
          normalize(cities$name)==normalize(nm)&
          is.finite(cities$lat)&is.finite(cities$long))
        if(length(hits)==1L) {
          chosen<-hits[[1L]]
          basis<-"gazetteer_named_locality_approximate"
          source<-paste0("Extracted locality: ",nm," (",code,
            "); maps::world.cities gazetteer; not verified site coordinates")
          break
        }
      }
      if(is.na(chosen)) {
        hits<-which(!is.na(cities$iso3)&cities$iso3==code&
          is.finite(cities$lat)&is.finite(cities$long))
        if(length(hits)) {
          ordered<-hits[order(-as.numeric(cities$pop[hits]),
            as.character(cities$name[hits]),na.last=TRUE)]
          chosen<-ordered[[1L]]
          basis<-"country_representative_city_not_case_location"
          source<-paste0("Extracted country ",code,"; representative city ",
            as.character(cities$name[[chosen]]),
            " from maps::world.cities; NOT the actual case location")
          warning<-"Only a country-scale representative point could be assigned"
        } else warning<-"No gazetteer city in extracted country"
      }
    } else if(length(codes)>1L)
      warning<-"Multiple countries reported; no single point assigned"
    else
      warning<-"No unambiguous extracted country code"
    if(!is.na(chosen))
      points[[length(points)+1L]]<-data.frame(
        case_id=id,latitude=as.numeric(cities$lat[[chosen]]),
        longitude=as.numeric(cities$long[[chosen]]),
        coordinate_basis=basis,coordinate_source=source,
        stringsAsFactors=FALSE)
    audit[[length(audit)+1L]]<-data.frame(
      case_id=id,extracted_countries=paste(codes,collapse="; "),
      extracted_localities=paste(localities,collapse="; "),
      mapped=!is.na(chosen),coordinate_basis=basis,
      warning=warning,stringsAsFactors=FALSE)
  }
  points_df<-if(length(points))do.call(rbind,points) else
    data.frame(case_id=character(),latitude=numeric(),longitude=numeric(),
      coordinate_basis=character(),coordinate_source=character())
  audit_df<-if(length(audit))do.call(rbind,audit) else
    data.frame(case_id=character(),extracted_countries=character(),
      extracted_localities=character(),mapped=logical(),
      coordinate_basis=character(),warning=character())
  if(anyDuplicated(points_df$case_id)||anyDuplicated(audit_df$case_id))
    stop("Duplicate case ID in coordinate output.")
  list(points=points_df,audit=audit_df)
}
