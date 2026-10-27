# Ostara HTML previews. Run: source(<path to this script>)
# Existing evidence sources are read-only.
build_ostara_extraction_forms <- function(map=evidencelandscaper::open_map("Ostara_collective_action_map")) {
  map <- normalizePath(map,mustWork=TRUE)
  root <- file.path(map,"reports","mechanism_review","review-43843f1a4977")
  manifest <- jsonlite::fromJSON(file.path(root,"manifest.json"),simplifyVector=FALSE)
  sha <- function(p) digest::digest(file=p,algo="sha256",serialize=FALSE)
  registries <- list.files(file.path(map,"runs","W09"),pattern="^fulltext_case_registry_normalised\\.jsonl$",recursive=TRUE,full.names=TRUE)
  registries <- registries[vapply(registries,function(p)identical(sha(p),manifest$w09_registry_sha256),logical(1))]
  if(length(registries)!=1L)stop("W09 registry provenance not uniquely resolved")
  readjl <- function(p) lapply(Filter(nzchar,readLines(p,warn=FALSE,encoding="UTF-8")),jsonlite::fromJSON,simplifyVector=FALSE)
  all_cases <- readjl(registries[[1L]])
  rec <- Filter(function(x)identical(x$record_id,"work-06e2595f7e438ea7"),all_cases)
  if(length(rec)!=1L||!length(rec[[1]]$cases))stop("Verified illustrative W09 case unavailable")
  case <- rec[[1]]$cases[[1]]
  supp <- list.files(file.path(root,"fulltext_mechanism_supplement"),pattern="^mechanism_assessments\\.jsonl$",recursive=TRUE,full.names=TRUE)
  supp <- supp[file.exists(supp)]
  if(length(supp)!=1L)stop("One completed mechanism inventory JSONL required")
  mechanisms <- readjl(supp[[1L]])
  model <- Filter(function(x)identical(x$record_id,"work-06e2595f7e438ea7"),mechanisms)
  if(length(model)!=1L)stop("Illustrative mechanism extraction unavailable")
  esc <- function(x) {
    x <- gsub("&","&amp;",x,fixed=TRUE)
    x <- gsub("<","&lt;",x,fixed=TRUE)
    x <- gsub(">","&gt;",x,fixed=TRUE)
    gsub('"',"&quot;",x,fixed=TRUE)
  }
  val <- function(x) {
    if(is.null(x)||!length(x))return("Not reported")
    if(is.list(x)) {
      text <- vapply(x,function(y) {
        if(is.list(y))paste(paste(names(y),vapply(y,function(z)paste(unlist(z),collapse=", "),character(1)),sep=": "),collapse="; ")
        else paste(unlist(y),collapse=", ")
      },character(1))
    } else text <- as.character(x)
    v <- paste(text[nzchar(text)],collapse="; ")
    if(nzchar(v))v else "Not reported"
  }
  table <- function(fields,x) {
    heads <- paste0("<th>",esc(tools::toTitleCase(gsub("_"," ",fields))),"</th>",collapse="")
    cells <- paste0("<td><em>",vapply(fields,function(f)esc(val(x[[f]])),character(1)),"</em></td>",collapse="")
    paste0("<div class='scroller'><table><thead><tr>",heads,"</tr></thead><tbody><tr>",cells,"</tr></tbody></table></div>")
  }
  css <- paste0("*{box-sizing:border-box}body{margin:0;background:#f9fbfa;color:#17313d;font:15px/1.65 system-ui,sans-serif}",
   "header{background:linear-gradient(120deg,#153645,#216458);color:white;padding:50px max(24px,calc((100vw - 1100px)/2))}",
   "h1{font-size:clamp(30px,4vw,44px)}header p{color:#d3e8e0}",
   "main{max-width:1180px;margin:auto;padding:35px 24px 80px}",
   "h2{margin:38px 0 16px;color:#1b554b}.scroller{overflow-x:auto;border:1px solid #dceae4;border-radius:12px;background:white}",
   "table{border-collapse:collapse;min-width:100%;width:max-content}th{background:#e7f2ec;text-align:left;color:#205d4d;font-size:13px}",
   "td,th{vertical-align:top;padding:16px;min-width:230px;max-width:420px;border-right:1px solid #e7efe9;overflow-wrap:anywhere}",
   "td em{font-style:italic;color:#395e5b;white-space:pre-wrap}",
   ".back{display:inline-block;background:white;color:#155d4f;text-decoration:none;border-radius:9px;padding:9px 16px;font-weight:700}",
   ".note{padding:16px 20px;border-left:4px solid #267d67;background:#ecf5f0;margin:25px 0}");
  page <- function(title,body) paste0('<!doctype html><html lang="en"><head><meta charset="utf-8"><meta name="viewport" content="width=device-width,initial-scale=1"><title>',
    esc(title),'</title><style>',css,'</style></head><body><header><a class="back" href="../index.html">← Back to prompt gallery</a><h1>',
    esc(title),'</h1><p>Draft extraction headings, with one authentic extracted example in italics.</p></header><main>',
    '<div class="note">Example publication: work-06e2595f7e438ea7. Values copied from archived extraction; “Not reported” marks missing values. Scroll tables horizontally.</div>',
    body,'</main></body></html>')
  fields <- c("case_name","case_description","objectives","actors","collaboration_activities","governance",
    "resources_incentives","enablers","barriers","commodities","supply_chain_stages","countries",
    "locations","additional_mechanisms","author_conclusions")
  # W09 case form: case-level fields only.
  case_html <- page("Full-text case data extraction form",paste0("<h2>Case-level extraction</h2>",table(fields,case)))
  z <- model[[1L]]
  first <- z$mechanism_inventory[[which(vapply(z$mechanism_inventory,function(m)identical(m$status,"implemented_case"),logical(1)))[[1L]]]]
  # Inventory uses one row per ontology code. Novelty gets its own tables.
  inv_fields <- c("mechanism_id","status","case_ids","source_quote","source_anchor","reason")
  novelty_fields <- c("abstract_candidate_id","original_proposal","status","closest_approved_mechanism_id",
                      "revised_label","revised_definition","case_ids","source_quote","source_anchor","reason")
  new_fields <- c("proposed_label","working_definition","closest_approved_mechanism_id","case_ids","source_quote","source_anchor","reason")
  empty <- list()
  mech_body <- paste0("<h2>Approved ontology mechanism assessments</h2>",table(inv_fields,first),
    "<h2>Abstract novelty reassessment</h2>",table(novelty_fields,if(length(z$abstract_novelty_validation))z$abstract_novelty_validation[[1L]] else empty),
    "<h2>New full-text mechanism proposals</h2>",table(new_fields,if(length(z$new_novelty))z$new_novelty[[1L]] else empty))
  out <- file.path(map,"reports","prompt_gallery","prompts")
  dir.create(out,recursive=TRUE,showWarnings=FALSE)
  case_path <- file.path(out,"draft-data-extraction.html")
  mechanism_path <- file.path(out,"fulltext-mechanism-form.html")
  writeLines(case_html,case_path,useBytes=TRUE)
  writeLines(page("Full-text mechanism data extraction form",mech_body),mechanism_path,useBytes=TRUE)
  message("Created:\n",case_path,"\n",mechanism_path)
  invisible(c(case_form=case_path,mechanism_form=mechanism_path))
}
build_ostara_extraction_forms()
