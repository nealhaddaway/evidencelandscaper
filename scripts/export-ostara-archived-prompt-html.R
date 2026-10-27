# Export the exact W04 and W07 prompts archived in a local Ostara map.
# This script is standalone and never changes the original source prompts.
export_ostara_archived_prompt_html <- function(map, out_dir=file.path(map,"reports","prompt_gallery")) {
  map <- normalizePath(map,mustWork=TRUE)
  esc <- function(x) {
    x <- gsub("&","&amp;",x,fixed=TRUE)
    x <- gsub("<","&lt;",x,fixed=TRUE)
    x <- gsub(">","&gt;",x,fixed=TRUE)
    gsub('"',"&quot;",x,fixed=TRUE)
  }
  root <- file.path(map,"runs")
  candidates <- character()
  if(dir.exists(root)) {
    candidates <- list.files(root, pattern="^(prompt|screening_prompt)\\.txt$",
                             recursive=TRUE,full.names=TRUE)
  }
  archived <- file.path(map,"data","prompts")
  if(dir.exists(archived)) {
    candidates <- c(candidates,list.files(archived,pattern="\\.txt$",
                                          recursive=TRUE,full.names=TRUE))
  }
  candidates <- unique(candidates[file.exists(candidates)])
  # Limit to stage-specific archived inputs, excluding incidental prompt copies
  selected <- candidates[grepl("(/W04/|/W07/|/W04_local/|/W07[^/]*/|/W04[^/]*/)", 
                                gsub("\\\\","/",candidates))]
  if(!length(selected))stop("No archived W04/W07 prompt files found in the map")
  digest <- function(f) unname(tools::md5sum(f))
  hashes <- vapply(selected,digest,character(1))
  selected <- selected[!duplicated(hashes)]
  dir.create(out_dir,recursive=TRUE,showWarnings=FALSE)
  outputs <- character()
  for(file in selected) {
    stage <- if(grepl("W04",file)) "W04 · Abstract screening" else "W07 · Mechanism detection"
    raw <- paste(readLines(file,warn=FALSE,encoding="UTF-8"),collapse="\n")
    h <- digest(file)
    filename <- paste0(if(grepl("W04",file))"w04" else "w07","-",
                       substr(h,1,12),".html")
    title <- paste(stage,"· archived prompt")
    # Escape all prompt text before injecting it into the document.
    content <- paste0(
'<!doctype html><html lang="en"><head><meta charset="utf-8"><meta name="viewport" content="width=device-width,initial-scale=1">',
'<title>',esc(title),'</title><style>',
'*{box-sizing:border-box}body{margin:0;background:#fbfcfa;color:#152837;font:16px/1.6 system-ui,-apple-system,sans-serif}',
'header{padding:60px max(28px,calc((100vw - 900px)/2));background:linear-gradient(135deg,#153140,#22695c);color:white}',
'small{letter-spacing:.15em;text-transform:uppercase;color:#bfe8dd;font-weight:700}',
'h1{font-size:clamp(30px,5vw,46px);line-height:1.1;margin:22px 0}',
'.source{font-size:12px;color:#d0eee4;overflow-wrap:anywhere}',
'main{max-width:960px;margin:32px auto 80px;padding:0 24px}',
'article{padding:34px;background:white;border:1px solid #e1e9e5;border-radius:16px;box-shadow:0 4px 16px #173b2b08}',
'pre{white-space:pre-wrap;overflow-wrap:anywhere;font:15px/1.8 ui-monospace,SFMono-Regular,Menlo,monospace;margin:0}',
'footer{max-width:960px;margin:auto;padding:0 24px 36px;color:#64786f;font-size:13px}',
'@media print{header{background:white;color:#152837;padding:24px}.source,small{color:#555}article{box-shadow:none}}</style></head><body>',
'<header><small>OSTARA / EXACT ARCHIVED PROMPT</small><h1>',esc(title),'</h1>',
'<div class="source">Source: ',esc(file),'<br>MD5: ',h,
'</div></header><main><article><pre>',esc(raw),'</pre></article></main>',
'<footer>Prompt text preserved exactly as archived. Presentation only.</footer></body></html>')
    output <- file.path(out_dir,filename)
    if(file.exists(output)) {
      if(!identical(readLines(output,warn=FALSE),strsplit(content,"\n",fixed=TRUE)[[1]]))
        stop("Refusing to replace existing HTML with same filename: ",output)
    } else writeLines(content,output,useBytes=TRUE)
    outputs <- c(outputs,output)
  }
  message("Exported ",length(outputs)," distinct W04/W07 archived prompts to: ",out_dir)
  invisible(outputs)
}
