# Ostara repository-specific policy overlay. Original W01 scripts remain unchanged.
.apply_ostara_exact_abstract_policy <- function(rescored_csv, metadata_csv) {
  parent<-dirname(rescored_csv)
  audit<-file.path(parent,"ostara_exact_abstract_policy.json")
  backup<-file.path(parent,"rescored_sample.before_ostara_abstract_policy.csv")
  if(file.exists(audit)) {
    z<-jsonlite::fromJSON(audit,simplifyVector=FALSE)
    if(!identical(z$after_sha256,digest::digest(file=rescored_csv,algo="sha256",serialize=FALSE)))
      stop("Ostara abstract-policy output changed; refusing reuse.")
    message("[W01 Ostara] Reusing verified identical-abstract policy overlay.")
    return(invisible(z))
  }
  if(file.exists(backup))stop("Unfinished Ostara abstract-policy application; originals preserved. Inspect before retrying.")
  if(!file.exists(rescored_csv)||!file.exists(metadata_csv))
    stop("Ostara identical-abstract input missing.")
  d<-data.table::fread(rescored_csv,showProgress=FALSE)
  m<-data.table::fread(metadata_csv,showProgress=FALSE)
  needed<-c("record_i","record_j","rescored_classification","rescored_rule")
  if(!all(needed%in%names(d))||!all(c("idx","abstract_hash")%in%names(m)))
    stop("Ostara abstract matching metadata unavailable.")
  if(anyDuplicated(m$idx)||anyNA(m$idx))stop("Invalid W01 metadata indices.")
  i<-match(d$record_i,m$idx);j<-match(d$record_j,m$idx)
  if(anyNA(i)||anyNA(j))stop("Candidate pair indices do not match metadata.")
  a<-as.character(m$abstract_hash[i]);b<-as.character(m$abstract_hash[j])
  matched<-!is.na(a)&!is.na(b)&nzchar(trimws(a))&nzchar(trimws(b))&a==b
  # Only change previously non-duplicate pairs, preserving existing automatic rules.
  promote<-which(matched & d$rescored_classification!="duplicate")
  prior<-d$rescored_classification[promote]
  if(length(promote)) {
    data.table::set(d,i=promote,j="rescored_classification",value="duplicate")
    data.table::set(d,i=promote,j="rescored_rule",value="ostara_identical_nonempty_abstract")
    if("review_route"%in%names(d))
      data.table::set(d,i=promote,j="review_route",value="automatic_duplicate")
    if("manual_review_needed"%in%names(d))
      data.table::set(d,i=promote,j="manual_review_needed",value=FALSE)
    if("decision_changed"%in%names(d))
      data.table::set(d,i=promote,j="decision_changed",value=TRUE)
  }
  dest<-paste0(rescored_csv,".ostara.tmp")
  if(file.exists(dest))stop("Previous policy temporary file exists.")
  data.table::fwrite(d,dest)
  if(nrow(data.table::fread(dest,select=needed,showProgress=FALSE))!=nrow(d))
    stop("Policy output row-count readback failure.")
  before<-digest::digest(file=rescored_csv,algo="sha256",serialize=FALSE)
  if(!file.copy(rescored_csv,backup,overwrite=FALSE))
    stop("Cannot preserve original identifier rescore output.")
  if(!file.rename(dest,rescored_csv))
    stop("Cannot atomically publish Ostara abstract decisions. Original preserved at ",backup)
  after<-digest::digest(file=rescored_csv,algo="sha256",serialize=FALSE)
  result<-list(schema="ostara-identical-abstract-override-v1",
    policy="Nonblank identical normalised abstract hashes are sufficient duplicate evidence",
    before_sha256=before,after_sha256=after,
    metadata_sha256=digest::digest(file=metadata_csv,algo="sha256",serialize=FALSE),
    input_pairs=nrow(d),exact_abstract_pairs=sum(matched),
    promoted_pairs=length(promote),
    prior_classes=as.list(table(prior)),
    remaining_review_pairs=sum(d$rescored_classification=="review"),
    remaining_unresolved_pairs=sum(d$rescored_classification=="unresolved"))
  jsonlite::write_json(result,audit,auto_unbox=TRUE,pretty=TRUE)
  message("[W01 Ostara] ",length(promote)," exact-abstract pairs promoted to duplicate. ",
          result$remaining_review_pairs," review pairs remain; no LLM called.")
  invisible(result)
}
