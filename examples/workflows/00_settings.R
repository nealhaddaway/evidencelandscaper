# EDIT THIS FILE ONCE before running the numbered scripts.
# These are wrappers around the installed evidencelandscaper package.
# Do not edit archived workflows or change a run_name after starting.
if (!requireNamespace("evidencelandscaper", quietly=TRUE))
  stop("Install evidencelandscaper using README.md first")
library(evidencelandscaper)
# Existing Ostara project, or another configured map name:
map_name <- "Ostara_collective_action_map"
create_new_map <- FALSE
# For a completely new map, configure set_map_root() beforehand.
if (isTRUE(create_new_map) &&
    !dir.exists(file.path(map_root(),map_name))) create_map(map_name)
map <- open_map(map_name)
# UNIQUE identifier for a new search. Keep unchanged for resuming.
run_name <- "new_search_2026_11"
strategy_file <- file.path(map,"inputs","search",paste0(run_name,".json"))
strategy_approved <- FALSE
# Select databases. Some require API keys or subscriptions.
sources <- c("openalex","lens","agricola","pubmed","ethos","epmc_preprints")
model <- "gpt-6-luna"
# Supply both manual RIS sources at run creation, or leave both NULL.
manual_ris <- NULL
manual_search_strings <- NULL
# Uncertain W01 pairs: human adjudication, or explicit quarantine.
w01_uncertain <- "adjudicate"
w01_llm_approved <- FALSE
# Select all unique publications or a documented probability sample.
abstract_sampling <- list(mode="all",uncertain="adjudicate")
abstract_sample_approved <- FALSE
abstract_finalisation_approved <- FALSE
case_sampling <- list(mode="all",uncertain="adjudicate")
case_sample_approved <- FALSE
fulltext_eligibility_review_complete <- FALSE
# Approved prompt/ontology INPUT PATHS, versioned and frozen at start:
screening_prompt <- NULL
w07_prompt <- NULL
ontology_file <- NULL
w08_prompt <- NULL
# Optional explicit review path. Stage 08 verifies it belongs to this named run.
mechanism_review_dir <- NULL
mechanism_supplement_dir <- NULL
message("Map: ", map," | Search: ",run_name)
