#' Export existing blind manual screening batches as RIS
#' @param scored W01 full-scoring directory.
#' @param seed Seed of the existing validation batches.
#' @return Directory containing five RIS files and field coverage report.
#' @export
export_w04_validation_ris <- function(scored,seed=20261126L) {
  scored<-normalizePath(scored,mustWork=TRUE)
  root<-dirname(scored)
  batches<-sample_w04_validation(scored,seed)
  sha<-function(p)digest::digest(file=p,algo="sha256",serialize=FALSE)
  readj<-function(p)jsonlite::fromJSON(p,simplifyVector=FALSE)
  m<-readj(file.path(batches,"manifest.json"))
  mp<-file.path(batches,"membership.csv")
  stopifnot(identical(m$membership_sha256,sha(mp)))
  membership<-utils::read.csv(mp,colClasses=c("integer","integer","character"))
  stopifnot(nrow(membership)==1000L,!anyDuplicated(membership$record_id),
    identical(membership$batch,rep(1:5,each=200L)),identical(membership$position,rep(1:200,5)))
  canonical<-file.path(root,"W02_main","locked","canonical.jsonl")
  stopifnot(identical(m$spec$w02_sha256,sha(canonical)))
  vals<-function(x){
    z<-as.character(unlist(x,use.names=FALSE))
    unique(trimws(z[!is.na(z)&nzchar(trimws(z))]))
  }
  scalar<-function(x){z<-vals(x);if(length(z))z[1L] else ""}
  one<-function(...){for(x in list(...)){z<-scalar(x);if(nzchar(z))return(z)};""}
  read_selected<-function(path,key,targets){
    con<-file(path,"rt",encoding="UTF-8");on.exit(close(con))
    found<-list()
    repeat{
      lines<-readLines(con,n=500L,warn=FALSE)
      if(!length(lines))break
      for(line in lines){
        if(!nzchar(trimws(line)))next
        z<-jsonlite::fromJSON(line,simplifyVector=FALSE)
        k<-key(z)
        if(k%in%targets){
          if(!is.null(found[[k]]))stop("Duplicate source identity: ",k)
          found[[k]]<-z
        }
      }
    }
    found
  }
  rows<-read_selected(canonical,function(z)scalar(z$identity$record_id),membership$record_id)
  stopifnot(setequal(names(rows),membership$record_id))
  paths<-readj(file.path(root,"union","source_inputs.json"))$sources
  refs<-unlist(lapply(rows,function(r)lapply(r$manifestations,function(z)
    list(source=scalar(z$source),id=scalar(z$source_record_id)))),recursive=FALSE)
  wanted<-split(vapply(refs,function(x)x$id,character(1)),vapply(refs,function(x)x$source,character(1)))
  originals<-list()
  source_hashes<-list()
  for(s in names(wanted)){
    p<-scalar(paths[[s]])
    if(!file.exists(p))stop("Missing W01 source input: ",s)
    source_hashes[[s]]<-sha(p)
    originals[[s]]<-read_selected(p,function(z)if(identical(s,"lens"))scalar(z$identity$lens_id)
      else scalar(z$sidecar_identity$sidecar_record_id),unique(wanted[[s]]))
    if(!setequal(names(originals[[s]]),unique(wanted[[s]])))stop("Unmatched source manifestations: ",s)
  }
  author_names<-function(x){
    if(is.null(x)||!length(x))return(character())
    if(is.character(x))return(vals(x))
    if(is.list(x)&&!is.null(names(x)))x<-list(x)
    v<-vapply(x,function(a){
      if(!is.list(a))return(scalar(a))
      family<-one(a$family,a$last_name,a$family_name,a$surname,a$lastName)
      given<-one(a$given,a$first_name,a$given_name,a$firstName,a$initials)
      if(nzchar(family))return(if(nzchar(given))paste(family,given,sep=", ") else family)
      one(a$display_name,a$full_name,a$name,a$displayName,a$fullName,a$wos_standard)
    },character(1))
    v[nzchar(v)]
  }
  type_ris<-function(x){
    z<-tolower(trimws(x))
    if(z%in%tolower(c("JOUR","BOOK","CHAP","CONF","CPAPER","THES","RPRT","GEN","ELEC","NEWS","MGZN","UNPB","DATA","SER","EDIT","PAMP","MANSCPT","ABST")))return(toupper(z))
    if(grepl("book chapter|book-chapter|chapter",z))return("CHAP")
    if(grepl("conference|proceedings",z))return("CPAPER")
    if(grepl("thesis|dissertation",z))return("THES")
    if(grepl("book",z))return("BOOK")
    if(grepl("report",z))return("RPRT")
    if(grepl("preprint|posted-content",z))return("UNPB")
    if(grepl("article|journal|review|editorial|letter|erratum",z))return("JOUR")
    "GEN"
  }
  clean<-function(x)trimws(gsub("[\r\n\t]+"," ",x))
  make<-function(r){
    can<-r$canonical
    fs<-lapply(r$manifestations,function(z){
      s<-scalar(z$source);a<-originals[[s]][[scalar(z$source_record_id)]]
      f<-if(identical(s,"lens"))a$canonical else a$mapped_fields
      if(is.null(f))f<-list()
      # Retain original mapped fields and enrich with stored source payload
      # where the adapter exposes additional bibliographic fields.
      sc<-a$scopus$raw_payload
      if(is.null(f$volume))f$volume<-sc[["prism:volume"]]
      if(is.null(f$issue))f$issue<-sc[["prism:issueIdentifier"]]
      if(is.null(f$pages))f$pages<-sc[["prism:pageRange"]]
      if(is.null(f$issn))f$issn<-sc[["prism:issn"]]
      ris<-a$provenance$original_ris_fields
      if(is.null(f$author_keywords))f$author_keywords<-ris$KW
      if(is.null(f$isbn))f$isbn<-ris$SN
      f
    })
    allf<-c(list(can),fs)
    field<-function(keys){for(f in allf)for(k in keys){z<-scalar(f[[k]]);if(nzchar(z))return(z)};""}
    authors<-lapply(allf,function(f)author_names(f$authors))
    au<-authors[[which.max(lengths(authors))]]
    if(!length(au))au<-vals(field(c("first_author","author_string")))
    kw<-unique(unlist(lapply(allf,function(f)c(vals(f$author_keywords),vals(f$keywords))),use.names=FALSE))
    # Do not substitute abstracts or DOIs from other source manifestations.
    ty<-field("publication_type")
    journal<-field(c("source","source_title","journal","publication_venue"))
    yr<-field(c("year","publication_year","published_year"))
    pg<-field(c("pages","page_range"))
    sp<-field(c("start_page","first_page"));ep<-field(c("end_page","last_page"))
    if(!nzchar(sp)&&nzchar(pg)){
      p<-strsplit(pg,"[-–]",perl=TRUE)[[1L]]
      sp<-p[1L];if(length(p)==2L&&!nzchar(ep))ep<-p[2L]
    }
    fields<-list(TY=type_ris(ty),ID=scalar(r$identity$record_id),
      TI=scalar(can$title),AU=au,PY=yr,
      T2=journal,VL=field("volume"),IS=field("issue"),SP=sp,EP=ep,
      DO=scalar(can$doi),AB=scalar(can$abstract),KW=kw,
      SN=unique(c(vals(field("issn")),vals(field("eissn")),vals(field("isbn")))),
      PB=field("publisher"),UR=if(nzchar(scalar(can$doi)))paste0("https://doi.org/",scalar(can$doi))else field(c("url","publication_url")))
    lines<-unlist(lapply(names(fields),function(k){
      z<-vals(fields[[k]]);if(length(z))paste0(k,"  - ",clean(z))else character()
    }),use.names=FALSE)
    list(lines=c(lines,"ER  - ",""),present=vapply(fields,function(x)length(vals(x))>0L,logical(1)))
  }
  out<-file.path(batches,"ris")
  if(dir.exists(out)){
    rm<-readj(file.path(out,"manifest.json"))
    stopifnot(identical(rm$membership_sha256,sha(mp)),identical(rm$w02_sha256,sha(canonical)),
      identical(rm$source_sha256,source_hashes))
    for(n in names(rm$files))stopifnot(identical(rm$files[[n]],sha(file.path(out,n))))
    return(invisible(out))
  }
  tmp<-tempfile("ris_",tmpdir=batches);dir.create(tmp)
  on.exit(if(dir.exists(tmp))unlink(tmp,recursive=TRUE),add=TRUE)
  cov<-list()
  for(k in 1:5){
    ids<-membership$record_id[membership$batch==k]
    z<-lapply(rows[ids],make)
    lines<-unlist(lapply(z,function(x)x$lines),use.names=FALSE)
    stopifnot(sum(startsWith(lines,"TY  - "))==200L,sum(startsWith(lines,"ER  - "))==200L,
      identical(sub("^ID  - ","",lines[startsWith(lines,"ID  - ")]),ids))
    writeLines(enc2utf8(lines),file.path(tmp,paste0("batch ",k,".ris")),useBytes=TRUE)
    counts<-colSums(do.call(rbind,lapply(z,function(x)x$present)))
    cov[[k]]<-data.frame(batch=k,field=names(counts),records_with_field=as.integer(counts))
  }
  utils::write.csv(do.call(rbind,cov),file.path(tmp,"field_coverage.csv"),row.names=FALSE)
  files<-list.files(tmp)
  jsonlite::write_json(list(schema="w04-validation-ris-v1",membership_sha256=sha(mp),
    w02_sha256=sha(canonical),source_sha256=source_hashes,files=setNames(lapply(file.path(tmp,files),sha),files)),
    file.path(tmp,"manifest.json"),auto_unbox=TRUE,pretty=TRUE)
  if(!file.rename(tmp,out))stop("Cannot publish RIS export.")
  message("[W04] PASS: five RIS batches, 200 records each; existing sample membership preserved.")
  invisible(out)
}
