# W08 | Package case-detection results for independent Shiny review

This is **not** the W04 screening app or W07 mechanism-coding app.
It exports **completed W08 case detection**, with original abstracts, model
candidate cases, evidence, status and full-text priority, into a portable ZIP.

```r
# Restart R after installation
remotes::install_github("nealhaddaway/evidencelandscaper",
                        ref="main",dependencies=FALSE,upgrade="never",force=TRUE)
library(evidencelandscaper)

# If this R session still has the output of trial_w08_cases(), use it directly:
# broad_500_cases <- trial_w08_cases(n=0L,w04_run=broad_500_w04,
#                                    prompt_file=case_prompt,model="gpt-5.6",map_path=map)

bundle <- export_w08_colleague_review(
  w08 = broad_500_cases,
  destination_zip = file.path(path.expand("~/Downloads"),
                              "ostara_broad_500_W08_case_review.zip")
)
print(bundle)
```

If R was restarted, identify the completed W08 run under
`<map>/runs/W08` that contains `case_assessments.jsonl`,
`manifest.json`, `summary.json`, `prompt.txt` and `fields.csv`.
Use its **exact directory** as `w08`. W08 must have status
`PILOT_COMPLETE`; `PILOT_PARTIAL` cannot be exported.

### Colleague instructions

1. Unzip the ZIP **into a writable directory**.
2. In RStudio install once: `install.packages(c("shiny","jsonlite","digest"))`.
3. Open the extracted `w08_review_bundle/app.R` and click **Run App**,
   or run `shiny::runApp("/path/to/w08_review_bundle")`.
4. Enter reviewer name. Read the publication title, abstract, model
   case-detection status, rationale and each identified candidate case.
5. Choose **Accept** or **Reject** for the publication-level coding. Optionally
   accept/reject/mark uncertain every individual candidate in that record.
   Record missed cases, suggested case boundaries or other corrections in
   free-text boxes.
6. Click **Save review** for each publication. Reopening the *same* unzipped
   folder preserves the local `decisions.jsonl` append-only history.
7. Export **complete CSV** and **full JSONL audit**; return both.

The complete CSV includes publications without detected cases.
Multiple `unclear` candidates are preserved and individually reviewable.
Model results and W04/W08 upstream files remain unchanged. The reviewer needs
no GitHub access, API key or map folder.

One ZIP per reviewer: sharing the same extracted directory concurrently is
unsupported. Human review results are not automatically imported into W08.
