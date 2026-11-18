#' Collect OSTARA grey-literature PDFs from an uploaded evidence-log CSV
#'
#' In an interactive R session opens only ONE file chooser for the evidence log.
#' Finds locally saved PDFs by "Name of PDF as saved", then tries "PDF link",
#' then "Link". Only PDF-signature-verified bytes are saved as PDFs. HTML pages
#' containing one unambiguous PDF link may be followed; ambiguous HTML links
#' are reported for manual review. Each record gets a stable title-based unique
#' filename. Checkpoints update an adjacent new CSV after each record.
#' The original evidence log is never changed and no LLM API is used.
#'
#' @param csv_file Input evidence log. If NULL, show file.choose() once.
#' @return Invisibly, the output CSV and directory paths with counts.
#' @export
download_ostara_grey_literature_pdfs <- function(csv_file=NULL) {
  if(is.null(csv_file)) {
    if(!interactive())stop("Select the evidence-log CSV in an interactive R session.")
    message("[OSTARA grey PDFs] Select your evidence-log CSV.")
    csv_file<-file.choose()
  }
  csv_file<-normalizePath(csv_file,winslash="/",mustWork=TRUE)
  if(!grepl("\\.csv$",csv_file,ignore.case=TRUE))
    stop("The selected file must be a CSV.")
  csv<-function(p)utils::read.csv(p,stringsAsFactors=FALSE,check.names=FALSE,
                                  fileEncoding="UTF-8-BOM",colClasses="character",
                                  na.strings=character())
  input<-csv(csv_file)
  expected<-c("Title","Name of PDF as saved","PDF link","Link")
  if(!all(expected%in%names(input)))
    stop("CSV missing required columns: ",
         paste(setdiff(expected,names(input)),collapse=", "))
  if(!nrow(input))stop("Selected CSV contains no records")
  if("Downloaded PDF filename"%in%names(input))
    stop("Select the ORIGINAL evidence-log CSV, not a previous output.")
  if(any(!nzchar(trimws(input$Title))))
    stop("Every row must have a nonempty Title for unique filenames.")
  path_base<-dirname(csv_file)
  root<-file.path(path_base,"grey_literature_pdf_collection")
  pdfdir<-file.path(root,"pdfs")
  dir.create(pdfdir,recursive=TRUE,showWarnings=FALSE)
  output<-file.path(root,"Grey_literature_evidence_log_with_PDF_filenames.csv")
  manifest_path<-file.path(root,"input_manifest.json")
  sha<-function(p)digest::digest(file=p,algo="sha256",serialize=FALSE)
  csv_hash<-sha(csv_file)
  # Never discard progress if the original evidence-log file changes mid-run.
  if(file.exists(manifest_path)) {
    m<-jsonlite::fromJSON(manifest_path,simplifyVector=FALSE)
    if(!identical(m$input_sha256,csv_hash)||
       !identical(as.integer(m$rows),as.integer(nrow(input))))
      stop("Evidence-log CSV differs from the earlier run. Keep the original ",
           "file unchanged, or save a revised input under a new folder.")
  } else jsonlite::write_json(
    list(schema="ostara-grey-literature-pdf-collection-v1",
         input_file=csv_file,input_sha256=csv_hash,rows=nrow(input)),
    manifest_path,pretty=TRUE,auto_unbox=TRUE)
  clean<-function(x) {
    x<-iconv(x,from="",to="ASCII//TRANSLIT",sub="")
    x[is.na(x)]<-"report"
    x<-tolower(gsub("[^a-zA-Z0-9]+","_",x))
    x<-gsub("^_+|_+$","",x)
    if(!nzchar(x))x<-"report"
    substr(x,1L,65L)
  }
  names_pdf<-vapply(seq_len(nrow(input)),function(i)
    paste0(sprintf("%04d",i),"_",clean(input$Title[[i]]),".pdf"),
    character(1))
  if(anyDuplicated(names_pdf))stop("Generated PDF filenames are not unique")
  status_cols<-c("Downloaded PDF filename","Download status",
                 "Download source","Download note")
  out<-input
  out[status_cols]<-""
  if(file.exists(output)) {
    prior<-csv(output)
    if(nrow(prior)!=nrow(input)||
       !identical(names(prior),names(out)))
      stop("Existing output CSV does not match the input/expected columns")
    for(field in names(input))
      if(!identical(prior[[field]],input[[field]]))
        stop("Input column changed since the checkpoint was saved: ",field)
    out<-prior
  }
  save_csv<-function() {
    tmp<-tempfile(pattern=".grey-",tmpdir=root,fileext=".csv")
    on.exit(if(file.exists(tmp))unlink(tmp),add=TRUE)
    con<-file(tmp,open="wb")
    writeBin(as.raw(c(239L,187L,191L)),con)
    utils::write.table(out,con,sep=",",row.names=FALSE,col.names=TRUE,
                       qmethod="double",quote=TRUE,na="")
    close(con)
    if(!file.copy(tmp,output,overwrite=TRUE))
      stop("Cannot update progress CSV: ",output)
  }
  pdf_signature<-function(p) {
    if(!file.exists(p)||dir.exists(p)||is.na(file.info(p)$size)||
       file.info(p)$size<5)return(FALSE)
    con<-file(p,"rb")
    on.exit(close(con),add=TRUE)
    buf<-readBin(con,"raw",n=1024L)
    if(length(buf)<5L)return(FALSE)
    txt<-rawToChar(buf,multiple=TRUE)
    any(vapply(seq_len(length(txt)-4L),function(k)
      identical(paste(txt[k:(k+4L)],collapse=""),"%PDF-"),
      logical(1)))
  }
  # Index likely folders once, without guessing filenames. Includes the CSV
  # folder and ordinary local download directories; avoids package-specific
  # paths and searching the whole disk.
  search_dirs<-unique(c(path_base,file.path(path_base,"pdfs"),
    file.path(path_base,"PDFs"),file.path(path_base,"Downloads"),
    path.expand("~/Downloads"),path.expand("~/Documents"),
    path.expand("~/Desktop")))
  search_dirs<-search_dirs[dir.exists(search_dirs)]
  local<-unique(unlist(lapply(search_dirs,function(d)
    list.files(d,pattern="\\.pdf$",recursive=TRUE,full.names=TRUE,
               ignore.case=TRUE,include.dirs=FALSE)),use.names=FALSE))
  local<-local[file.exists(local)]
  by_name<-split(local,tolower(basename(local)))
  # Existing original filenames are matched exactly, not by loose title
  # similarity (which risks silently selecting a different publication).
  lookup_local<-function(original) {
    original<-trimws(original)
    if(!nzchar(original))return(list(path="",note=""))
    candidates<-by_name[[tolower(basename(original))]]
    if(!length(candidates))
      return(list(path="",note="Named local PDF not found"))
    candidates<-candidates[vapply(candidates,pdf_signature,logical(1))]
    if(!length(candidates))
      return(list(path="",note="Named local file is not a PDF"))
    hashes<-vapply(candidates,sha,character(1))
    if(length(unique(hashes))>1L)
      return(list(path="",note="Multiple different local PDFs have this name"))
    list(path=candidates[[1L]],note="")
  }
  valid_url<-function(u) {
    u<-trimws(u)
    if(!grepl("^https?://",u,ignore.case=TRUE))return(FALSE)
    !grepl("[[:space:]]",u)
  }
  # Make HTML hrefs absolute without accepting non-web URI schemes.
  absolute_url<-function(base,href) {
    href<-trimws(gsub("&amp;","&",href,fixed=TRUE))
    if(valid_url(href))return(href)
    origin<-sub("^(https?://[^/]+).*$","\\1",base,ignore.case=TRUE)
    if(startsWith(href,"//"))return(paste0(sub(":.*$","",origin),":",href))
    if(startsWith(href,"/"))return(paste0(origin,href))
    path<-sub("[?#].*$","",base)
    folder<-sub("[^/]*$","",path)
    paste0(folder,href)
  }
  fetch<-function(u) {
    if(!valid_url(u))stop("Not a valid http(s) URL")
    response<-httr2::request(u)|>
      httr2::req_user_agent("evidencelandscaper/OSTARA-grey-PDF-collector")|>
      httr2::req_timeout(90)|>
      httr2::req_perform()
    bytes<-httr2::resp_body_raw(response)
    if(length(bytes)>100*1024*1024)
      stop("Downloaded file exceeds 100 MB")
    list(bytes=bytes,url=httr2::resp_url(response),
         content_type=httr2::resp_header(response,"content-type"))
  }
  is_pdf_bytes<-function(bytes) {
    if(length(bytes)<5L)return(FALSE)
    b<-rawToChar(bytes[seq_len(min(length(bytes),1024L))],multiple=TRUE)
    any(vapply(seq_len(length(b)-4L),function(k)
      identical(paste(b[k:(k+4L)],collapse=""),"%PDF-"),
      logical(1)))
  }
  save_pdf_bytes<-function(bytes,dest) {
    tmp<-tempfile(tmpdir=pdfdir,fileext=".pdf")
    on.exit(if(file.exists(tmp))unlink(tmp),add=TRUE)
    writeBin(bytes,tmp)
    if(!pdf_signature(tmp))stop("Downloaded response is not a real PDF")
    if(!file.copy(tmp,dest,overwrite=FALSE))
      stop("Could not save target PDF: ",dest)
  }
  html_pdf<-function(response) {
    if(length(response$bytes)>3*1024*1024)
      return(list(url="",note="HTML page too large to inspect"))
    html<-tryCatch(rawToChar(response$bytes),error=function(e)"")
    # Work only on genuine HTML document links, not arbitrary embedded scripts.
    hrefs<-regmatches(html,gregexpr(
      "(?i)href[[:space:]]*=[[:space:]]*['\\\"][^'\\\"]+['\\\"]",
      html,perl=TRUE))[[1L]]
    if(!length(hrefs))
      return(list(url="",note="Page contains no downloadable PDF link"))
    hrefs<-sub("(?i)^href[[:space:]]*=[[:space:]]*['\\\"]","",
               hrefs,perl=TRUE)
    hrefs<-sub("['\\\"]$","",hrefs)
    hrefs<-hrefs[grepl("\\.pdf([?#].*)?$",hrefs,ignore.case=TRUE)]
    hrefs<-unique(vapply(hrefs,function(h)
      absolute_url(response$url,h),character(1)))
    hrefs<-hrefs[vapply(hrefs,valid_url,logical(1))]
    if(length(hrefs)==1L)return(list(url=hrefs[[1L]],note=""))
    if(!length(hrefs))
      list(url="",note="No direct PDF link detected on page")
    else list(url="",note="Multiple PDF links on page; manual review needed")
  }
  retrieve<-function(url) {
    if(!nzchar(trimws(url)))return(list(bytes=NULL,note="No URL"))
    if(!valid_url(url))return(list(bytes=NULL,note="Invalid URL"))
    result<-tryCatch(fetch(url),error=function(e)e)
    if(inherits(result,"error"))
      return(list(bytes=NULL,note=conditionMessage(result)))
    if(is_pdf_bytes(result$bytes))
      return(list(bytes=result$bytes,note="Direct PDF",url=result$url))
    ct<-if(is.null(result$content_type))"" else result$content_type
    if(!grepl("html|text/",ct,ignore.case=TRUE) &&
       !grepl("^\\s*<",tryCatch(rawToChar(head(result$bytes,50L)),
                                error=function(e)"")))
      return(list(bytes=NULL,note="URL did not return a PDF"))
    follow<-html_pdf(result)
    if(!nzchar(follow$url))return(list(bytes=NULL,note=follow$note))
    second<-tryCatch(fetch(follow$url),error=function(e)e)
    if(inherits(second,"error"))
      return(list(bytes=NULL,note=paste("PDF link failed:",
                                       conditionMessage(second))))
    if(!is_pdf_bytes(second$bytes))
      return(list(bytes=NULL,note="Linked document is not a PDF"))
    list(bytes=second$bytes,note="PDF linked from HTML",url=second$url)
  }
  message("[OSTARA grey PDFs] ",nrow(out),
          " evidence-log rows; PDFs folder: ",pdfdir)
  for(i in seq_len(nrow(out))) {
    filename<-names_pdf[[i]]
    dest<-file.path(pdfdir,filename)
    if(pdf_signature(dest)) {
      out[["Downloaded PDF filename"]][[i]]<-filename
      out[["Download status"]][[i]]<-"saved"
      if(!nzchar(out[["Download source"]][[i]]))
        out[["Download source"]][[i]]<-"previously_saved"
      save_csv()
      next
    }
    if(file.exists(dest)) {
      out[["Downloaded PDF filename"]][[i]]<-""
      out[["Download status"]][[i]]<-"needs_review"
      out[["Download source"]][[i]]<-""
      out[["Download note"]][[i]]<-"Existing destination is not a valid PDF"
      save_csv()
      next
    }
    notes<-character()
    found<-lookup_local(input[["Name of PDF as saved"]][[i]])
    success<-FALSE
    if(nzchar(found$path)) {
      success<-isTRUE(file.copy(found$path,dest,overwrite=FALSE))&&
        pdf_signature(dest)
      if(success) {
        out[["Download source"]][[i]]<-"local_pdf"
        out[["Download note"]][[i]]<-found$path
      } else {
        if(file.exists(dest))unlink(dest)
        notes<-c(notes,"Local copy failed")
      }
    } else if(nzchar(found$note))notes<-c(notes,found$note)
    if(!success)for(field in c("PDF link","Link")) {
      url<-trimws(input[[field]][[i]])
      if(!nzchar(url))next
      res<-retrieve(url)
      if(!is.null(res$bytes)) {
        success<-tryCatch({
          save_pdf_bytes(res$bytes,dest)
          TRUE
        },error=function(e) {
          notes<<-c(notes,paste(field,conditionMessage(e)))
          FALSE
        })
        if(success) {
          out[["Download source"]][[i]]<-field
          out[["Download note"]][[i]]<-paste(res$note,res$url,sep=": ")
          break
        }
      } else notes<-c(notes,paste0(field,": ",res$note))
    }
    if(success) {
      out[["Downloaded PDF filename"]][[i]]<-filename
      out[["Download status"]][[i]]<-"saved"
    } else {
      out[["Downloaded PDF filename"]][[i]]<-""
      out[["Download status"]][[i]]<-"needs_review"
      out[["Download source"]][[i]]<-""
      out[["Download note"]][[i]]<-paste(notes,collapse=" | ")
    }
    save_csv()
    message("[OSTARA grey PDFs] ",i,"/",nrow(out)," ",
            out[["Download status"]][[i]],": ",input$Title[[i]])
  }
  message("[OSTARA grey PDFs] Saved ",
          sum(out[["Download status"]]=="saved"),"/",nrow(out),
          "; review ",sum(out[["Download status"]]=="needs_review"),
          ". Updated CSV: ",output)
  invisible(list(csv=output,pdf_directory=pdfdir,
    saved=sum(out[["Download status"]]=="saved"),
    needs_review=sum(out[["Download status"]]=="needs_review")))
}
