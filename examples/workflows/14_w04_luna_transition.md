# Urgent W04: resume after GPT-5.6 first pass using Luna

All **new W04 calls** are locked to `gpt-5.6-luna`.
The original running Rscript **does not change model automatically**: stop it
after first pass finishes, before second pass begins. Pass 2 starts immediately
after pass 1 in the legacy script. Stop the Rscript process, not just the
interactive console if Rscript remains alive.

**Do not modify or delete the original W04 run directory or its JSONL files.**

After the original Rscript is stopped and GitHub `main` is reinstalled in a
fresh R session:

```r
remotes::install_github("nealhaddaway/evidencelandscaper",
  ref="main",dependencies=FALSE,upgrade="never",force=TRUE)
library(evidencelandscaper)
map <- open_map("Ostara_collective_action_map")
state <- jsonlite::fromJSON(file.path(map,"runs","orchestrated",
  "forestcommodities_sustainability","state.json"),simplifyVector=FALSE)
broader_scored <- state$w01$scored
broader_sample <- state$decision1$selected_ids_path

old_root <- file.path(dirname(broader_scored),"W04_local")
sample_tag <- substr(digest::digest(file=broader_sample,
  algo="sha256",serialize=FALSE),1,12)
candidates <- list.dirs(old_root,recursive=FALSE,full.names=TRUE)
legacy <- candidates[grepl(paste0("^selected-",sample_tag,"-[0-9a-f]{12}$"),
  basename(candidates))]
stopifnot(length(legacy)==1L)
old_run <- legacy[[1]]
original_prompt <- file.path(old_run,"screening_prompt.txt")
stopifnot(file.exists(original_prompt))

luna_run <- continue_w04_with_luna(
  source_w04=old_run,scored=broader_scored,
  prompt_file=original_prompt,record_ids_csv=broader_sample)
print(luna_run)
```

The helper checks that all selected W03-eligible publications have pass-1
decisions and that the original prompt and exact sample match. It copies the
complete first pass **and any saved Sol pass-2 checkpoint**, preserving
model provenance. All remaining calls in pass 2 and pass 3 use **Luna only**.
An immutable snapshot of the pre-existing pass-2 records is retained and
validated before new requests. The old run remains unchanged.

The new engine refuses non-Luna W04 API requests and checks model identity
against saved checkpoints. The final W04 summary contains
`pass_models` and a `model_transition` manifest for auditability.

The recovery handles an already-started pass 2 if its saved responses have
valid provenance. It refuses to continue if pass 3 started, if a mixed-model
ledger fails validation, or if the Luna destination already exists.
