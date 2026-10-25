test_that("completed W01 scoring overlap is recovered without rescoring", {
  skip_if_not_installed("data.table")
  dir<-tempfile("w01-scores-"); dir.create(dir)
  on.exit(unlink(dir,recursive=TRUE),add=TRUE)
  n<-5124L
  base<-data.frame(record_i=seq_len(n),record_j=seq_len(n)+n,
    classification="unresolved",rule="candidate_only",stratum="expanded_stratified")
  # Final checkpoint starts at k-4999 rather than last_written+1:
  # 4876 previous rows reappear at the final checkpoint.
  scored<-rbind(base,base[125:5000,])
  data.table::fwrite(scored,file.path(dir,"scored_sample.csv"))
  data.table::fwrite(base[,c("record_i","record_j")],
    file.path(dir,"expanded_scoring_sample.csv"))
  jsonlite::write_json(list(status="success",total_candidate_pairs=n,
    original_calibration_pairs=0L,expanded_pairs=n,
    scoring_sample_n=nrow(scored),stratum_counts=list(expanded_stratified=nrow(scored)),
    base_classification_counts=list(unresolved=nrow(scored)),
    base_rule_counts=list(candidate_only=nrow(scored))),
    file.path(dir,"expanded_summary.json"),auto_unbox=TRUE)
  evidencelandscaper:::.recover_w01_scoring_final_overlap(dir,dir,n)
  corrected<-data.table::fread(file.path(dir,"scored_sample.csv"))
  expect_equal(nrow(corrected),n)
  expect_equal(sum(duplicated(corrected[,c("record_i","record_j"),with=FALSE])),0L)
  expect_true(file.exists(file.path(dir,"original-scoring-before-checkpoint-repair",
                                    "scored_sample.csv")))
  expect_message(evidencelandscaper:::.recover_w01_scoring_final_overlap(dir,dir,n),
                 "already validated")
})
