testthat::test_that("W07 provisional source is explicit and isolated from finalised W04", {
  src<-paste(deparse(evidencelandscaper::trial_w07),collapse="\n")
  testthat::expect_match(src,"provisional_retains = FALSE",fixed=TRUE)
  testthat::expect_match(src,"W04_UNFINALISED_CONSENSUS_RETAINS",fixed=TRUE)
  testthat::expect_match(src,"final_unresolved",fixed=TRUE)
  testthat::expect_match(src,"model = model",fixed=TRUE)
  testthat::expect_match(src,"provisional-",fixed=TRUE)
  testthat::expect_match(src,"-w04-",fixed=TRUE)
  testthat::expect_silent(parse(file=testthat::test_path("..","..","R","w07-trial.R")))
  testthat::expect_silent(parse(file=testthat::test_path("..","..","R","w07-colleague-review.R")))
  testthat::expect_error(
    evidencelandscaper::trial_w07(w04_run=NULL,provisional_retains=TRUE),
    "explicit w04_run")
  testthat::expect_error(
    evidencelandscaper::trial_w07(provisional_retains=NA),
    "provisional_retains must")
  testthat::expect_error(
    evidencelandscaper::trial_w07(provisional_retains=TRUE,
                                  w04_run="/nonexistent",model="gpt-5.6"),
    "Luna")
})

testthat::test_that("W07 provisional mode matches only consensus retains", {
  # Simulate a 100-record W04 consensus with 59 unresolved cases.
  decisions<-c(rep("retain",18),rep("exclude",23),rep("uncertain",59))
  models<-lapply(seq_along(decisions),function(i)
    list(record_id=paste0("r",i),screening=list(decision=decisions[[i]])))
  chosen<-Filter(function(x)identical(x$screening$decision,"retain"),models)
  testthat::expect_length(chosen,18)
  testthat::expect_equal(sum(vapply(models,function(z)
    identical(z$screening$decision,"uncertain"),logical(1))),59)
  testthat::expect_false(any(vapply(chosen,function(z)
    identical(z$screening$decision,"uncertain"),logical(1))))
})
