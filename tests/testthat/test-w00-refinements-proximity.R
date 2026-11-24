test_that("structured refinement proximity expands individual operand pairs", {
  spec <- list(kind="pairwise_proximity", distance=5L,
    groups=list(list(left=c("research*", "stud*"),right=c("participat*", "collaborat*"))),
    remainder='(community OR stakeholder*) AND "climate change"',
    fields="title-abstract")
  q <- .w00_refinement_query(spec,"openalex")
  expect_match(q, '("research*" and "participat*" within 5 words of each other)', fixed=TRUE)
  expect_match(q, '("stud*" and "collaborat*" within 5 words of each other)', fixed=TRUE)
  expect_match(q, "type is not [dataset](dataset)", fixed=TRUE)
  expect_equal(length(regmatches(q, gregexpr("within 5 words",q,fixed=TRUE))[[1L]]),4L)
  spec$fields <- c("title","abstract")
  expect_error(.w00_refinement_query(spec,"lens"),"explicit word forms")
  spec$groups <- list(list(left=c("research","study"),right=c("participatory","collaboration")))
  q <- .w00_refinement_query(spec,"lens")
  expect_match(q,'"research participatory"~5',fixed=TRUE)
  expect_match(q,"AND NOT publication_type:dataset",fixed=TRUE)
})
