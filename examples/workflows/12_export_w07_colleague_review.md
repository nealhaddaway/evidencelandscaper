# W07: Portable Shiny app for client mechanism review

After **completed** `trial_w07()` coding, create an offline review ZIP:

```r
library(evidencelandscaper)
# v4_pilot was returned by trial_w07(...); it is the W07 run directory.
stopifnot(dir.exists(v4_pilot))
zip_path <- export_w07_colleague_review(
  w07 = v4_pilot,
  destination_zip = file.path(
    path.expand("~/Downloads"),
    "ostara_w07_mechanism_client_review.zip"
  )
)
print(zip_path)
```

If your R session was restarted, find the **exact** W07 run directory under
`<map>/runs/W07` that contains `coding.jsonl`, `manifest.json`,
`ontology.csv` and `summary.json`. Use this directory as `w07`.

The exporter checks coding completion, selected IDs, ontology checksums and
source bibliographic-record provenance. It includes all W07-coded records,
including records with no mechanism and ontology-gap proposals, with titles,
abstracts, model mechanism IDs, definitions, supporting evidence, confidence,
implementation status and model notes.

### Client instructions

1. Unzip the archive to a **writable** folder.
2. Open RStudio and install once:
   `install.packages(c("shiny","jsonlite","digest"))`.
3. Open the extracted `w07_review_bundle/app.R` in RStudio and click
   **Run App**, or run:
   `shiny::runApp("/path/to/w07_review_bundle")`.
4. Enter reviewer name. Review each publication's abstract, model-assigned
   mechanisms and ontology definitions. Filter by model coding status,
   review status, title, record ID or mechanism code.
5. Choose **Accept** or **Reject** for the publication-level mechanism coding.
   Use **Comments** and **Different or additional mechanisms** for corrections.
6. Click **Save assessment**. Responses append to a local
   `decisions.jsonl` audit log. Reopening the same extracted folder
   retains prior reviews.
7. Export **complete review CSV** and **audit JSONL**, and send both back.

Review is **publication-level**, not an individual accept/reject per mechanism.
If a publication has one correct mechanism and one incorrect mechanism,
choose Reject and specify the correct assignments in the alternatives field.

The app is local-only and does not support concurrent reviewers sharing
one extracted folder. Use separate copies if multiple reviewers are involved.
No model/API token or GitHub checkout is required by the client.

**Important**: Human feedback is not automatically applied to coding or
ontology and should be reconciled separately after review. Archived W07
model outputs and the source W04 screening are not modified.
