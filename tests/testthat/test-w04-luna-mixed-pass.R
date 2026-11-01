testthat::test_that("W04 mixed Sol first/partial second pass transition is append-only and verified", {
  testthat::skip_if_not_installed("jsonlite")
  td<-tempfile("w04-mixed-")
  dir.create(td)
  on.exit(unlink(td,recursive=TRUE,force=TRUE),add=TRUE)
  root<-file.path(td,"runs","W01","prepare-test")
  scored<-file.path(root,"W01_scored")
  w04_root<-file.path(root,"W04_local")
  dir.create(scored,recursive=TRUE,showWarnings=FALSE)
  dir.create(file.path(root,"W03_publication_status","full"),recursive=TRUE)
  readme<-file.path(td,"prompt.txt")
  writeLines("Screen test record",readme)
  sha<-function(p)digest::digest(file=p,algo="sha256",serialize=FALSE)
  psha<-digest::digest("Screen test record",algo="sha256",serialize=FALSE)
  selected<-file.path(td,"selected.csv")
  utils::write.csv(data.frame(record_id=c("a","b","c")),
                   selected,row.names=FALSE,quote=TRUE)
  select_sha<-sha(selected)
  original<-file.path(w04_root,paste0("selected-",substr(select_sha,1,12),"-",
                                       substr(psha,1,12)))
  dir.create(file.path(original,"consensus"),recursive=TRUE)
  writeLines("Screen test record",file.path(original,"screening_prompt.txt"))
  jsonlite::write_json(list(prompt_path=file.path(original,"screening_prompt.txt"),
    prompt_version="test",response_schema_name="ostara_w04_relevance",
    expected_prompt_sha256=psha),file.path(original,"screening_config.json"),
    auto_unbox=TRUE,pretty=TRUE)
  jl<-function(rows,path)writeLines(vapply(rows,function(x)
    as.character(jsonlite::toJSON(x,auto_unbox=TRUE,null="null")),
    character(1)),path,useBytes=TRUE)
  jl(lapply(c("a","b","c"),function(id)
     list(record_id=id,pass=1L,decision="exclude",reason="test",
          model_requested="gpt-5.6")),
     file.path(original,"consensus","pass1.jsonl"))
  jl(list(list(record_id="a",pass=2L,decision="exclude",
               reason="test",model_requested="gpt-5.6")),
     file.path(original,"consensus","pass2.jsonl"))
  jl(lapply(c("a","b","c"),function(id)
    list(record_id=id,publication_status=list(exclude_from_workflow04=FALSE))),
    file.path(root,"W03_publication_status","full","publication_status.jsonl"))
  # Stub the downstream run_w04 execution, never call any API.
  testthat::local_mocked_bindings(
    run_w04=function(scored,prompt_file,record_ids_csv,max_records,
                     model,open_app) {
      testthat::expect_identical(model,"gpt-5.6-luna")
      dest<-file.path(dirname(scored),"W04_local",
         paste0("selected-",substr(sha(record_ids_csv),1,12),"-",
                substr(psha,1,12),"-model-gpt-5.6-luna"))
      testthat::expect_true(file.exists(file.path(dest,"consensus","pass1.jsonl")))
      testthat::expect_true(file.exists(file.path(dest,"consensus","pass2.jsonl")))
      dest
    },
    .package="evidencelandscaper")
  result<-continue_w04_with_luna(original,scored,readme,selected)
  testthat::expect_true(dir.exists(result))
  testthat::expect_identical(
     sha(file.path(original,"consensus","pass1.jsonl")),
     sha(file.path(result,"consensus","pass1.jsonl")))
  testthat::expect_identical(
     sha(file.path(original,"consensus","pass2.jsonl")),
     sha(file.path(result,"consensus","pass2_seed.jsonl")))
  trans<-jsonlite::fromJSON(file.path(result,"model_transition.json"),
                             simplifyVector=FALSE)
  testthat::expect_identical(trans$from_model,"gpt-5.6")
  testthat::expect_identical(trans$to_model,"gpt-5.6-luna")
  testthat::expect_equal(trans$pass1_records,3)
  testthat::expect_equal(trans$pass2_sol_records,1)
  testthat::expect_error(continue_w04_with_luna(original,scored,readme,selected),
                         "already exists")
})
