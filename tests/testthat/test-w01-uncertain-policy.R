testthat::test_that("W01 ambiguous-pair policy is explicit, auditable, and not labelled human", {
 root<-tempfile("w01-policy-")
 dir.create(file.path(root,"full_scoring"),recursive=TRUE)
 llm<-file.path(root,"review_preparation","llm_adjudication")
 dir.create(llm,recursive=TRUE)
 on.exit(unlink(root,recursive=TRUE),add=TRUE)
 queue<-file.path(llm,"human_review_cases.jsonl")
 record<-list(review_case_id="case-1",
   record_i=list(source="sourceA",source_record_id="id-1"),
   record_j=list(source="sourceB",source_record_id="id-2"))
 writeLines(jsonlite::toJSON(record,auto_unbox=TRUE),queue)
 out<-quarantine_w01_uncertain(file.path(root,"full_scoring"))
 testthat::expect_true(file.exists(out))
 q<-utils::read.csv(out,stringsAsFactors=FALSE)
 testthat::expect_setequal(q$source_record_id,c("id-1","id-2"))
 reviewer<-jsonlite::fromJSON(file.path(llm,"reviewer_state","reviewer.json"))
 testthat::expect_match(reviewer$reviewer,"NOT_HUMAN")
 event<-jsonlite::fromJSON(readLines(file.path(llm,"reviewer_state","decisions.jsonl"))[[1]])
 testthat::expect_identical(event$decision,"not_duplicate")
 testthat::expect_match(event$rationale,"CLUSTERING CONVENTION ONLY")
 testthat::expect_error(quarantine_w01_uncertain(file.path(root,"full_scoring")),
                        "already exists")
})
