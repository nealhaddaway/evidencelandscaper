testthat::test_that("W08 explicitly opts into provisional consensus retains", {
  source<-paste(deparse(evidencelandscaper::trial_w08_cases),collapse="\n")
  testthat::expect_match(source,"provisional_retains = FALSE",fixed=TRUE)
  testthat::expect_match(source,"provisional-",fixed=TRUE)
  testthat::expect_match(source,"source_w04_layer = layer_dir",fixed=TRUE)
  testthat::expect_match(source,"sum(decisions == \"uncertain\")",fixed=TRUE)
  testthat::expect_match(source,"sha(input_path)",fixed=TRUE)
  testthat::expect_error(
    evidencelandscaper::trial_w08_cases(provisional_retains=TRUE),
    "explicit w04_run")
  testthat::expect_error(
    evidencelandscaper::trial_w08_cases(provisional_retains=NA),
    "must be TRUE or FALSE")
  testthat::expect_error(
    evidencelandscaper::trial_w08_cases(provisional_retains=TRUE,
       w04_run="/nonexistent",model="gpt-5.6"),
    "Luna")
  testthat::expect_silent(parse(file=testthat::test_path(
      "..","..","R","w08-cases.R")))
  testthat::expect_silent(parse(file=testthat::test_path(
      "..","..","R","w08-colleague-review.R")))
})

testthat::test_that("Provisional W08 population excludes uncertain and excluded", {
  decisions<-c(rep("retain",100),rep("exclude",200),rep("uncertain",59))
  data<-lapply(seq_along(decisions),function(i)
    list(record_id=paste0("x",i),screening=list(decision=decisions[[i]])))
  retain<-Filter(function(z)identical(z$screening$decision,"retain"),data)
  testthat::expect_equal(length(retain),100)
  testthat::expect_equal(sum(decisions=="uncertain"),59)
  testthat::expect_false(any(vapply(retain,function(z)
    identical(z$screening$decision,"uncertain"),logical(1))))
})
