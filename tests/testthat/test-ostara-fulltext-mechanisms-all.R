testthat::test_that("Full-text mechanism + case sweep validator does not require case IDs", {
  f<-evidencelandscaper:::.w09_validate_independent_mechanism_sweep
  fulltext<-"[SECTION 1] Producers coordinated purchases and shared auditing."
  z<-list(record_id="work-1",
    case_presence_assessment=list(
      case_status="case_present",candidate_case_count=1L,
      candidate_cases=list(list(candidate_label="Producer group",
        case_description="Producer group coordinated purchases",
        eligibility_reason="Multiple actors coordinated towards sustainability",
        implementation_status="implemented",
        source_quote="Producers coordinated purchases",
        source_anchor="[SECTION 1]")),
      assessment_reason="Described coordinated arrangement"),
    mechanism_inventory=list(list(mechanism_id="C01.01",
      status="implemented_case",
      source_quote="shared auditing",source_anchor="[SECTION 1]",
      reason="Operational mechanism directly described")),
    abstract_novelty_validation=list(),new_novelty=list())
  testthat::expect_silent(f(z,"work-1","C01.01",fulltext))
  w<-z
  w$case_presence_assessment$case_status<-"no_case"
  w$case_presence_assessment$candidate_case_count<-0L
  w$case_presence_assessment$candidate_cases<-list()
  testthat::expect_silent(f(w,"work-1","C01.01",fulltext))
  w$mechanism_inventory[[1]]$case_ids<-list("work-1-case-001")
  testthat::expect_error(f(w,"work-1","C01.01",fulltext),"case_ids")
  w<-z
  w$mechanism_inventory[[1]]$source_quote<-"Invented evidence"
  testthat::expect_error(f(w,"work-1","C01.01",fulltext),"evidence/anchor")
  w<-z
  w$case_presence_assessment$candidate_cases[[1]]$implementation_status<-"invalid"
  testthat::expect_error(f(w,"work-1","C01.01",fulltext),"Incomplete provisional")
})

testthat::test_that("All-full-text runner prepares independent inventory without case extraction", {
  td<-tempfile("fulltext-all-")
  w09<-file.path(td,"W09");w07<-file.path(td,"W07")
  dir.create(w09,recursive=TRUE);dir.create(w07,recursive=TRUE)
  dir.create(file.path(w09,"pdfs"))
  utils::write.csv(data.frame(record_id=c("work-1","work-2")),
                   file.path(w09,"retrieval_queue.csv"),row.names=FALSE)
  ontology<-data.frame(
    mechanism_id=paste0("M",seq_len(30L),".1.1"),
    collective_action_type=rep("Coordination",30L),
    high_level_mechanism=rep("Coordination",30L),
    fine_level_mechanism=paste("Mechanism",seq_len(30L)),
    definition=paste("Definition for mechanism",seq_len(30L)))
  utils::write.csv(ontology,file.path(w07,"ontology.csv"),row.names=FALSE)
  writeLines(c('{"record_id":"work-1","mechanisms":[],"ontology_gap":false}',
               '{"record_id":"work-2","mechanisms":[],"ontology_gap":false}'),
             file.path(w07,"coding.jsonl"))
  writeLines("Source placeholder used only for verifying discovery",
             file.path(w09,"pdfs","work-1.pdf"))
  key<-Sys.getenv("OPENAI_API_KEY",unset=NA_character_)
  Sys.unsetenv("OPENAI_API_KEY")
  on.exit(if(is.na(key))Sys.unsetenv("OPENAI_API_KEY")
          else Sys.setenv(OPENAI_API_KEY=key),add=TRUE)
  testthat::expect_error(
    evidencelandscaper::extract_ostara_all_fulltext_mechanisms(w09,w07),
    "OPENAI_API_KEY required")
  dirs<-list.dirs(file.path(w09,"fulltext_mechanism_inventory"),
                  recursive=FALSE,full.names=TRUE)
  testthat::expect_length(dirs,1L)
  inventory<-utils::read.csv(file.path(dirs[[1L]],"source_inventory.csv"))
  testthat::expect_equal(nrow(inventory),2L)
  testthat::expect_equal(inventory$availability,
                         c("available","full_text_missing"))
  manifest<-jsonlite::fromJSON(file.path(dirs[[1L]],"manifest.json"))
  testthat::expect_identical(manifest$schema,"ostara-fulltext-mechanism-all-v4")
  testthat::expect_false("eligible_case_ids"%in%names(manifest))
  testthat::expect_false("w07_coding_sha256"%in%names(manifest))
  testthat::expect_true(file.exists(file.path(dirs[[1L]],"ontology.csv")))
})
