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
  field_cols<-field_cols[!grepl(" \\[(evidence|reporting status)\\]$",field_cols)]
  country_fields<-field_cols[grepl(
    "(where did the case take place.*country|country|countries)",
    field_cols,ignore.case=TRUE)]
  locality_fields<-field_cols[grepl(
    "(where did the case take place.*(region|province|district|locality|place)|^field_[0-9]{3} :: .*\\b(location|locations|region|province|district|municipality|town|city|village)\\b)",
    field_cols,ignore.case=TRUE,perl=TRUE)]
  locality_fields<-setdiff(locality_fields,country_fields)
  # ISO3 values are preferred. Each explicitly reported country receives
  # its own representative point when local coordinates are unavailable.
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
  # Extract explicit named locations from comma/semicolon-separated prose.
  # Only exact unique gazetteer matches qualify as indicative map positions.
  # Broad regions, vague descriptions and unknown names remain in the audit.
  localities_for<-function(v) {
    x<-trimws(unlist(strsplit(as.character(v),
      "\\s*(?:;|\\||\\n|,|\\s+and\\s+)\\s*",perl=TRUE)))
    unique(x[nzchar(x)&nchar(x)>=3L])
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
    mapped_count<-0L;warning<-"";resolved<-character()
    unresolved<-character()
    if(length(codes)) {
      # Find every unambiguous named city in the declared country/countries.
      # This prevents selecting just the first city from a multi-site case.
      locality_hits<-list()
      for(nm in localities) {
        hits<-which(!is.na(cities$iso3)&cities$iso3%in%codes&
          normalize(cities$name)==normalize(nm)&
          is.finite(cities$lat)&is.finite(cities$long))
        # A city name must resolve to exactly one gazetteer location
        # among the reported countries. Ambiguous place names are not guessed.
        if(length(hits)==1L) {
          locality_hits[[length(locality_hits)+1L]]<-list(
            ix=hits[[1L]],name=nm)
          resolved<-c(resolved,nm)
        } else unresolved<-c(unresolved,nm)
      }
      unique_location_keys<-character()
      country_located<-character()
      add_point<-function(ix,basis,source) {
        key<-paste(cities$lat[[ix]],cities$long[[ix]],sep="|")
        if(key%in%unique_location_keys)return(invisible(FALSE))
        unique_location_keys<<-c(unique_location_keys,key)
        points[[length(points)+1L]]<<-data.frame(
          case_id=id,latitude=as.numeric(cities$lat[[ix]]),
          longitude=as.numeric(cities$long[[ix]]),
          coordinate_basis=basis,coordinate_source=source,
          stringsAsFactors=FALSE)
        mapped_count<<-mapped_count+1L
        invisible(TRUE)
      }
      for(hit in locality_hits) {
        ix<-hit$ix
        code<-as.character(cities$iso3[[ix]])
        add_point(ix,"gazetteer_named_locality_approximate",
          paste0("Extracted named locality: ",hit$name," (",code,
            "); maps::world.cities gazetteer; not verified site coordinates"))
        country_located<-union(country_located,code)
      }
      # For every reported country with no resolvable named locality,
      # supply one explicitly labelled indicative country point.
      for(code in setdiff(codes,country_located)) {
        hits<-which(!is.na(cities$iso3)&cities$iso3==code&
          is.finite(cities$lat)&is.finite(cities$long))
        if(length(hits)) {
          ordered<-hits[order(-as.numeric(cities$pop[hits]),
            as.character(cities$name[hits]),na.last=TRUE)]
          chosen<-ordered[[1L]]
          add_point(chosen,"country_representative_city_not_case_location",
            paste0("Extracted country ",code,"; representative city ",
              as.character(cities$name[[chosen]]),
              " from maps::world.cities; NOT the actual case location"))
        }
      }
      if(length(unresolved))
        warning<-paste0(length(unresolved),
          " named location fragments not uniquely resolved; see extracted_localities")
      if(mapped_count<length(codes))
        warning<-paste(c(warning,
          "One or more extracted countries lack gazetteer coordinates"),
          collapse="; ")
      if(mapped_count&&length(codes)>1L)
        warning<-paste(c(warning,
          "Multi-country: indicative point for each resolvable country"),
          collapse="; ")
    } else warning<-"No unambiguous extracted country code"
    audit[[length(audit)+1L]]<-data.frame(
      case_id=id,extracted_countries=paste(codes,collapse="; "),
      extracted_localities=paste(localities,collapse="; "),
      unresolved_localities=paste(unresolved,collapse="; "),
      mapped_point_count=mapped_count,
      mapped=mapped_count>0L,coordinate_basis=if(mapped_count>0L)
        "see_points_csv" else "",
      warning=warning,stringsAsFactors=FALSE)
  }
  points_df<-if(length(points))do.call(rbind,points) else
    data.frame(case_id=character(),latitude=numeric(),longitude=numeric(),
      coordinate_basis=character(),coordinate_source=character())
  audit_df<-if(length(audit))do.call(rbind,audit) else
    data.frame(case_id=character(),extracted_countries=character(),
      extracted_localities=character(),unresolved_localities=character(),
      mapped_point_count=integer(),mapped=logical(),
      coordinate_basis=character(),warning=character())
  if(anyDuplicated(paste(points_df$case_id,points_df$latitude,
                         points_df$longitude,sep="|"))||
     anyDuplicated(audit_df$case_id))
    stop("Duplicate identical case/coordinate location or audit case ID.")
  list(points=points_df,audit=audit_df)
}
