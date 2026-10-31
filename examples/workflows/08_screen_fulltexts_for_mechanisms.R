# STAGE 08: Ostara supplementary full-text mechanism extraction.
# IMPORTANT: The CURRENT function requires a review manifest tied to an
# existing legacy W09/W10 assessment. New eligibility-first named runs
# do not create that legacy W10 manifest. This is an INTEGRATION GAP;
# do not fabricate a W10 run or bypass a provenance check.
source("00_settings.R")
if (is.null(mechanism_review_dir))
  stop("Set mechanism_review_dir to a completed, verified legacy Ostara ",
       "mechanism review. For newly completed eligibility-first searches, ",
       "the package needs a new review-builder interface before this stage works.")
if (!dir.exists(mechanism_review_dir))
  stop("Mechanism review folder does not exist: ",mechanism_review_dir)
# Model calls are billed. max_records=0L covers all selected full texts.
if (is.null(mechanism_supplement_dir))
  mechanism_supplement_dir <- extract_ostara_fulltext_mechanisms(
    review_dir=mechanism_review_dir,max_records=0L,model=model)
message("Mechanism assessments: ",mechanism_supplement_dir)
review <- prepare_ostara_supplement_review(
  review_dir=mechanism_review_dir,supplement_dir=mechanism_supplement_dir)
print(review)
message("Human review of mechanism statuses and novelty proposals is required.")
