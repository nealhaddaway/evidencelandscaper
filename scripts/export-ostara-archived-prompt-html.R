# Regenerate the W04/W07 prompt HTML in the same layout as W08-W10.
# Original archived TXT files are never changed. Existing generated HTML is
# replaced only if it has the corresponding archived source hash filename.
export_ostara_archived_prompt_html <- function(map,out_dir=file.path(map,"reports","prompt_gallery","prompts")) {
 map<-normalizePath(map,mustWork=TRUE)
 esc<-function(s) {
   s<-gsub("&","&amp;",s,fixed=TRUE)
   s<-gsub("<","&lt;",s,fixed=TRUE)
   s<-gsub(">","&gt;",s,fixed=TRUE)
   gsub('"',"&quot;",s,fixed=TRUE)
 }
 roots<-file.path(map,"runs")
 candidates<-if(dir.exists(roots))
   list.files(roots,pattern="^(prompt|screening_prompt)\\.txt$",
              recursive=TRUE,full.names=TRUE) else character()
 archive<-file.path(map,"data","prompts")
 if(dir.exists(archive))
   candidates<-c(candidates,list.files(archive,pattern="\\.txt$",
                  recursive=TRUE,full.names=TRUE))
 candidates<-unique(candidates)
 candidates<-candidates[
   grepl("(/W04/|/W07/|/W04_local/|/W07[^/]*/|/W04[^/]*/)",
     gsub("\\\\","/",candidates))]
 if(!length(candidates))stop("Archived W04 and W07 text prompts not found")
 hashes<-unname(tools::md5sum(candidates))
 candidates<-candidates[!duplicated(hashes)]
 dir.create(out_dir,recursive=TRUE,showWarnings=FALSE)
 css<-paste0(
 '*{box-sizing:border-box}body{margin:0;background:#f9fbfa;',
 'color:#17313d;font-family:system-ui,-apple-system,"Segoe UI",sans-serif;line-height:1.72}',
 'header{background:linear-gradient(120deg,#153645,#216458);color:white;',
 'padding:55px max(24px,calc((100vw - 900px)/2)) 60px}',
 '.eyebrow{text-transform:uppercase;letter-spacing:.16em;color:#c4e8db;',
 'font-size:12px;font-weight:800}h1{font-size:clamp(29px,4vw,44px);',
 'letter-spacing:-.035em;line-height:1.16;margin:14px 0 12px}',
 'header p{font-size:14px;color:#cbe4dc}main{max-width:900px;',
 'padding:40px 24px 80px;margin:auto}.prompt{padding:12px 6px}',
 'h2{font-size:18px;line-height:1.45;font-weight:780;letter-spacing:.008em;',
 'margin:34px 0 10px;color:#1d5349}h2:first-child{margin-top:0}',
 '.prompt-text{white-space:pre-wrap;overflow-wrap:anywhere;font-size:15.5px;',
 'line-height:1.8;margin:0 0 20px}.back{display:inline-flex;align-items:center;',
 'gap:8px;padding:10px 17px;border:1px solid #bbd6ce;background:white;',
 'color:#145f4f;border-radius:9px;text-decoration:none;font-weight:700;',
 'font-size:14px}.back:hover{background:#e4f3ed}.foot{margin-top:45px;',
 'padding-top:22px;border-top:1px solid #d9e6e1;color:#5c7270;',
 'font-size:13px}@media print{header{background:white;color:#17313d;',
 'padding:25px}.back{display:none}}')
 render<-function(src,title) {
   chunks<-strsplit(src,"\n[ \t]*\n",perl=TRUE)[[1]]
   paste(vapply(chunks,function(chunk){
     lines<-strsplit(trimws(chunk),"\n",fixed=TRUE)[[1]]
     first<-trimws(lines[[1]])
     heading<-nchar(first)<85&&nchar(first)>3&&
       grepl("^[A-Z0-9 /():.,-]+$",first)&&
       !grepl("[.]$",first)&&!grepl("[{}]",first)
     body<-if(heading)paste(lines[-1],collapse="\n") else paste(lines,collapse="\n")
     paste0(if(heading)paste0("<h2>",esc(first),"</h2>") else "",
       if(nzchar(trimws(body)))paste0("<p class=\"prompt-text\">",esc(body),"</p>") else "")
   },character(1)),collapse="\n")
 }
 out<-character()
 for(file in candidates) {
   stage<-if(grepl("W04",file))"W04" else "W07"
   title<-if(stage=="W04")"W04 · Abstract screening" else "W07 · Mechanism detection"
   hash<-unname(tools::md5sum(file))
   filename<-paste0(tolower(stage),"-",substr(hash,1,12),".html")
   raw<-paste(readLines(file,warn=FALSE,encoding="UTF-8"),collapse="\n")
   html<-paste0('<!doctype html><html lang="en"><head><meta charset="utf-8">',
    '<meta name="viewport" content="width=device-width,initial-scale=1">',
    '<title>',esc(title),' · Ostara</title><style>',css,'</style></head><body>',
    '<header><a class="back" href="../index.html">← Back to prompt gallery</a>',
    '<div class="eyebrow" style="margin-top:32px">OSTARA / ARCHIVED MODEL INSTRUCTIONS</div>',
    '<h1>',esc(title),'</h1><p>Versioned archived prompt · MD5: ',hash,'</p>',
    '</header><main><article class="prompt">',render(raw,title),'</article>',
    '<div class="foot"><a class="back" href="../index.html">← Back to prompt gallery</a>',
    '<p>Archived text preserved; headings presented typographically.</p>',
    '</div></main></body></html>')
   dest<-file.path(out_dir,filename)
   writeLines(html,dest,useBytes=TRUE)
   out<-c(out,dest)
 }
 message("Regenerated ",length(out)," W04/W07 HTML prompt pages: ",out_dir)
 invisible(out)
}
