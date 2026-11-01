# Independent W04 screening validation for colleagues

The exported function `export_w04_colleague_review()` creates a portable ZIP
from a completed W04 model-consensus run. It contains the Shiny app and
ALL screened records (retained, excluded and uncertain), rather than only
records with unresolved model consensus. No W04 decisions are changed.

## Create the ZIP on your machine

After running the client's revised W04 prompt on all 196 records:

```r
library(evidencelandscaper)
stopifnot(dir.exists(revised_w04))
bundle_zip <- export_w04_colleague_review(
  w04 = revised_w04,
  destination_zip = file.path(
    path.expand('~/Downloads'), 'ostara_narrow_revised_W04_all_196.zip'
  )
)
```

Use the NEW revised prompt's W04 directory, not the original baseline W04.
The function verifies the model population and prepares a checksum-pinned
review queue from its completed consensus.

## Instructions for your colleague

1. Unzip the archive into a writable directory.
2. Install R packages once: `install.packages(c('shiny','jsonlite','digest'))`.
3. Open `w04_review_bundle/app.R` in RStudio and click Run App, or execute
   `shiny::runApp('/path/to/w04_review_bundle')`.
4. Enter your name, filter by model decision or review status, or search
   by title or record ID. Inspect the full abstract and individual model reasons.
5. Save a separate independent decision (retain, exclude, uncertain), plus
   an optional reason. Revisions append to `decisions.jsonl` without removing
   earlier review entries.
6. Export the complete CSV (including unreviewed records) and the JSONL audit
   history and return both to the project lead.

All case data are in `data/review_queue.jsonl`, checked with SHA-256 at startup.
The app does not use API keys or modify the original evidence map.
Use separate extracted copies for multiple reviewers; concurrent writes
to one bundle are not supported. Human reviews are NOT automatically imported
into the main W04 screening results.
