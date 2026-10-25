# W01 scoring-output recovery is a local execution adapter, not a change to
# the original matching, scoring, clustering or adjudication rules.
.recover_w01_scoring_final_overlap <- function(output,candidates,expected_n) {
  requireNamespace("data.table",quietly=TRUE) ||
    stop("data.table is required to validate the completed W01 scoring.")
  f <- file.path(output,"expanded_summary.json")
  scored_file <- file.path(output,"scored_sample.csv")
  selection_file <- file.path(output,"expanded_scoring_sample.csv")
  if(any(!file.exists(c(f,scored_file,selection_file))))
    stop("Completed W01 scoring artefacts missing; refusing recovery.")
  x <- jsonlite::fromJSON(f,simplifyVector=FALSE)
  if(!identical(x$status,"success") ||
     as.integer(x$total_candidate_pairs)!=as.integer(expected_n))
    stop("W01 scoring source count or success status has changed.")
  d <- data.table::fread(scored_file,showProgress=FALSE)
  selected <- data.table::fread(selection_file,
                                 select=c("record_i","record_j"),showProgress=FALSE)
  if(!all(c("record_i","record_j","classification","rule","stratum") %in% names(d)) ||
     !all(c("record_i","record_j") %in% names(selected)))
    stop("W01 scoring columns unavailable for validation.")
  key <- function(z) paste(z$record_i,z$record_j,sep="::")
  selected_keys <- key(selected)
  if(length(selected_keys)!=expected_n || anyDuplicated(selected_keys))
    stop("W01 original scoring selection is not unique and complete.")
  observed_keys<-key(d)
  duplicate_rows<-duplicated(observed_keys)
  if(!setequal(observed_keys,selected_keys) ||
     length(unique(observed_keys))!=expected_n)
    stop("W01 scored pair identities do not match the complete scoring selection.")
  extra<-sum(duplicate_rows)
  if(extra==0L) {
    if(nrow(d)!=expected_n || as.integer(x$scoring_sample_n)!=expected_n)
      stop("W01 summary disagrees with its unique scoring rows.")
    message("[W01] Existing scored output already validated.")
    return(invisible(output))
  }
  # Only the established final-boundary overlap bug may be recovered.
  final_chunk <- expected_n %% 5000L
  expected_extra <- if(final_chunk==0L)0L else 5000L-final_chunk
  if(extra!=expected_extra ||
     nrow(d)!=as.integer(x$scoring_sample_n) ||
     as.integer(x$scoring_sample_n)!=expected_n+expected_extra)
    stop("Unexpected W01 scoring excess; refusing automatic repair.")
  full_row_duplicates<-duplicated(d)
  if(sum(full_row_duplicates)!=extra ||
     any(duplicate_rows != full_row_duplicates))
    stop("Repeated scoring identities have conflicting content; refusing recovery.")
  fixed<-d[!duplicate_rows]
  if(nrow(fixed)!=expected_n || anyDuplicated(key(fixed)))
    stop("Recovered W01 scores fail uniqueness validation.")
  x$scoring_sample_n<-as.integer(expected_n)
  x$expanded_pairs<-as.integer(expected_n-as.integer(x$original_calibration_pairs))
  x$stratum_counts<-as.list(table(fixed$stratum))
  x$base_classification_counts<-as.list(table(fixed$classification))
  x$base_rule_counts<-as.list(table(fixed$rule))
  x$recovery<-list(type="duplicate_final_checkpoint_overlap",
    removed_repeated_rows=extra,
    original_scored_sha256=digest::digest(file=scored_file,algo="sha256",serialize=FALSE),
    original_summary_sha256=digest::digest(file=f,algo="sha256",serialize=FALSE))
  replacement<-paste0(scored_file,".verified.tmp")
  replacement_summary<-paste0(f,".verified.tmp")
  if(file.exists(replacement)||file.exists(replacement_summary))
    stop("Existing W01 repair temporary files; inspect before retrying.")
  data.table::fwrite(fixed,replacement)
  jsonlite::write_json(x,replacement_summary,auto_unbox=TRUE,pretty=TRUE,null="null")
  check<-data.table::fread(replacement,select=c("record_i","record_j"),
                            showProgress=FALSE)
  if(nrow(check)!=expected_n || !setequal(key(check),selected_keys) ||
     anyDuplicated(key(check)))
    stop("W01 repaired file failed read-back verification; originals unchanged.")
  backup<-file.path(output,"original-scoring-before-checkpoint-repair")
  if(dir.exists(backup))stop("W01 scoring recovery backup already exists.")
  dir.create(backup)
  if(!file.copy(scored_file,file.path(backup,"scored_sample.csv")) ||
     !file.copy(f,file.path(backup,"expanded_summary.json")))
    stop("Could not preserve original scoring files; repair not applied.")
  if(!file.rename(replacement,scored_file) ||
     !file.rename(replacement_summary,f))
    stop("Could not publish validated W01 repair; preserved originals in ",backup)
  message("[W01] Recovered ",format(expected_n,big.mark=","),
          " unique scored pairs, removed ",extra,
          " duplicated final-checkpoint rows; originals backed up at ",backup)
  invisible(output)
}
