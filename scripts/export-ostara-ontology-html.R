# Render the exact archived W07 mechanism ontology into a standalone HTML table.
# Read only. Never alters any ontology or model-coding file.
export_ostara_ontology_html <- function(
    map, output_file=file.path(map,"reports","prompt_gallery","prompts","ontology.html"),
    ontology_csv=NULL) {
  map <- normalizePath(map,mustWork=TRUE)
  sha <- function(f) digest::digest(file=f,algo="sha256",serialize=FALSE)
  if(is.null(ontology_csv)) {
    root <- file.path(map,"runs","W07")
    if(!dir.exists(root))stop("W07 runs directory missing")
    paths <- list.files(root,pattern="^ontology\\.csv$",recursive=TRUE,full.names=TRUE)
    paths <- paths[file.exists(file.path(dirname(paths),"manifest.json"))]
    paths <- paths[vapply(paths,function(p) {
      x<-tryCatch(jsonlite::fromJSON(file.path(dirname(p),"manifest.json"),
                                      simplifyVector=FALSE),error=function(e)NULL)
      !is.null(x) && identical(x$ontology_sha256,sha(p))
    },logical(1))]
    if(!length(paths))stop("No provenance-verified W07 ontology.csv found")
    # User choice: latest W07 ontology. Prefer completed full W07 runs,
    # then choose the latest run manifest modification time.
    full <- grepl("^full-",basename(dirname(paths))) &
      file.exists(file.path(dirname(paths),"coding.jsonl")) &
      file.exists(file.path(dirname(paths),"summary.json"))
    if(any(full))paths <- paths[full]
    manifests <- file.path(dirname(paths),"manifest.json")
    dates <- file.info(manifests)$mtime
    if(anyNA(dates))stop("Unable to date verified W07 manifests")
    latest <- which(dates==max(dates))
    if(length(unique(vapply(paths[latest],sha,character(1))))>1L)
      stop("Latest W07 runs have conflicting ontology checksums")
    ontology_csv <- paths[[latest[[1L]]]]
    message("Selected latest verified W07 ontology: ",ontology_csv)
  }
  ontology_csv <- normalizePath(ontology_csv,mustWork=TRUE)
  df <- utils::read.csv(ontology_csv,stringsAsFactors=FALSE,check.names=FALSE,
                        fileEncoding="UTF-8-BOM")
  required <- c("category_id","mechanism_id","mechanism_source_label")
  if(!all(required %in% names(df)))stop("Ontology missing required columns")
  if(anyNA(df$mechanism_id)||anyDuplicated(df$mechanism_id))
    stop("Invalid/duplicate mechanism IDs")
  # Keep the original columns, ordering, language and wording.
  esc <- function(x) {
    x <- ifelse(is.na(x),"",as.character(x))
    x <- gsub("&","&amp;",x,fixed=TRUE)
    x <- gsub("<","&lt;",x,fixed=TRUE)
    x <- gsub(">","&gt;",x,fixed=TRUE)
    gsub('"',"&quot;",x,fixed=TRUE)
  }
  # Display-only title case: leave CSV column names unchanged.
  labels <- tools::toTitleCase(gsub("_"," ",names(df),fixed=TRUE))
  def_col <- match("definition_draft",names(df))
  header_classes <- ifelse(!is.na(def_col) & seq_along(labels)==def_col,
                           ' class="definition-col"',"")
  th <- paste0('<th scope="col"',header_classes,'>',esc(labels),'</th>',
               collapse="")
  trs <- vapply(seq_len(nrow(df)),function(i) {
    vals <- esc(as.character(unlist(df[i,,drop=FALSE],use.names=FALSE)))
    classes <- ifelse(seq_along(vals)==def_col,' class="definition-col"',"")
    cells <- paste0("<td",classes,">",vals,"</td>",collapse="")
    paste0("<tr>",cells,"</tr>")
  },character(1))
  checksum <- sha(ontology_csv)
  css <- paste0(
    ":root{--ink:#17313d;--muted:#5f7680;--green:#1a6658;--line:#deebe6}",
    "*{box-sizing:border-box}body{margin:0;background:#f9fbfa;color:var(--ink);",
    "font:15px/1.6 system-ui,-apple-system,'Segoe UI',sans-serif}",
    "header{background:linear-gradient(120deg,#153645,#216458);color:white;",
    "padding:52px max(24px,calc((100vw - 1100px)/2)) 60px}",
    "header h1{font-size:clamp(30px,4vw,46px);letter-spacing:-.035em;",
    "line-height:1.14;margin:22px 0 8px}header p{max-width:850px;color:#d3e8e0}",
    "main{max-width:1160px;padding:38px 24px 80px;margin:auto}",
    ".back{display:inline-flex;padding:9px 16px;border-radius:9px;",
    "font-size:14px;font-weight:700;text-decoration:none;border:1px solid #b8d6ce;",
    "background:white;color:#155d4f}.back:hover{background:#e4f3ed}",
    ".bar{display:flex;gap:16px;justify-content:space-between;",
    "flex-wrap:wrap;align-items:center;margin-bottom:20px}",
    "input{font:inherit;width:min(100%,430px);padding:12px 14px;",
    "border:1px solid #bacdc6;border-radius:10px;background:white}",
    ".count{font-size:13px;color:var(--muted)}",
    ".table-wrap{overflow:auto;background:white;border:1px solid var(--line);",
    "border-radius:14px;box-shadow:0 5px 22px #15364508}",
    "table{width:100%;border-collapse:collapse;min-width:1400px;table-layout:auto}",
    "th.definition-col,td.definition-col{min-width:460px;width:38%;}",
    "th{text-align:left;background:#eaf4ef;color:#225e50;",
    "font-size:12px;text-transform:uppercase;letter-spacing:.06em;",
    "padding:15px;position:sticky;top:0;z-index:2}",
    "td{padding:16px 15px;border-bottom:1px solid #e6eeea;vertical-align:top;",
    "min-width:100px;white-space:pre-wrap;overflow-wrap:anywhere}",
    "td:nth-child(2){font-weight:750;color:#155e50;white-space:nowrap}",
    "tr:hover td{background:#f5faf7}",
    ".foot{font-size:12px;color:var(--muted);margin:24px 0 0;",
    "overflow-wrap:anywhere}",
    "@media print{header{background:white;color:var(--ink)}",
    "header p{color:var(--muted)}.back,input{display:none}",
    ".table-wrap{overflow:visible}th{position:static}table{min-width:0}",
    "td{font-size:11px}tr{break-inside:avoid}}")
  html <- paste0('<!doctype html><html lang="en"><head><meta charset="utf-8">',
    '<meta name="viewport" content="width=device-width,initial-scale=1">',
    '<title>Ostara · Draft mechanism ontology</title><style>',css,'</style>',
    '</head><body><header><a class="back" href="../index.html">',
    '← Back to prompt gallery</a><h1>Draft mechanism ontology</h1>',
    '<p>Approved W07 mechanism codes and definitions, presented exactly as ',
    'stored in the versioned ontology CSV.</p></header><main>',
    '<div class="bar"><input id="filter" type="search" ',
    'placeholder="Search codes, labels and definitions…" aria-label="Filter ontology">',
    '<span class="count" id="count">',nrow(df),' mechanisms</span></div>',
    '<div class="table-wrap"><table id="ontology"><thead><tr>',th,
    '</tr></thead><tbody>',paste(trs,collapse="\n"),
    '</tbody></table></div>',
    '<p class="foot">Source SHA-256: ',checksum,
    ' · Source file: ',esc(basename(ontology_csv)),
    ' · No changes made to ontology text or coding.</p></main>',
    '<script>',
    '(function(){const input=document.getElementById("filter");',
    'const rows=[...document.querySelectorAll("#ontology tbody tr")];',
    'const count=document.getElementById("count");',
    'input.addEventListener("input",function(){',
    'const q=input.value.trim().toLocaleLowerCase();let n=0;',
    'for(const r of rows){const ok=r.textContent.toLocaleLowerCase().includes(q);',
    'r.hidden=!ok;if(ok)n++;}count.textContent=n+" of "+rows.length+" mechanisms";',
    '});})();</script></body></html>')
  dir.create(dirname(output_file),recursive=TRUE,showWarnings=FALSE)
  writeLines(html,output_file,useBytes=TRUE)
  message("Ontology HTML created: ",output_file," (",nrow(df)," mechanisms)")
  invisible(output_file)
}
