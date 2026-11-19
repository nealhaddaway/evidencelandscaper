#' Audit zero-case classifications by original grey literature file type
#'
#' Offline analysis of existing W09 case_data.jsonl checkpoints. Counts
#' "no case" only when a publication has a completed, validated extraction
#' checkpoint with zero cases. Unreadable and pending sources are not negative.
#' File type is taken from the original source register, not a staged XML
#' conversion of an HTML or ASPX file.
#'
#' @param map_path OSTARA project directory.
#' @param batch_dir Explicit grey-literature run folder, if more than one.
#' @return Paths and aggregate statistics, invisibly.
#' @export
audit_ostara_grey_zero_cases_by_filetype <- function(map_path,batch_dir=NULL) {
  # The partial export is checkpoint-only and never starts a model request.
  previous<-export_ostara_grey_literature_partial(
    map_path=map_path,batch_dir=batch_dir)
  statuses<-utils::read.csv(previous$status_csv,stringsAsFactors=FALSE,
                            check.names=FALSE,fileEncoding="UTF-8-BOM")
  req<-c("record_id","file_name","source_format","extraction_status",
         "case_count","review_priority")
  if(!all(req%in%names(statuses)))
    stop("Expected columns missing from partial status export: ",
         paste(setdiff(req,names(statuses)),collapse=", "))
  if(anyDuplicated(statuses$record_id))
    stop("Source identity duplication in status export")
  statuses$file_type<-tolower(as.character(statuses$source_format))
  statuses$file_type[statuses$file_type=="htm"]<-"html"
  statuses$file_type[statuses$file_type==""]<-"unknown"
  counts<-suppressWarnings(as.integer(statuses$case_count))
  completed<-statuses$extraction_status=="validated"
  if(any(completed&is.na(counts)))
    stop("Completed assessment with no recorded case count")
  statuses$audit_classification<-ifelse(
    completed&counts==0L,"validated_zero_cases",
    ifelse(completed&counts>0L,"validated_with_cases",
           ifelse(statuses$extraction_status=="unreadable_full_text",
             "unreadable","not_yet_validated")))
  statuses$potential_false_negative<-completed&counts==0L
  types<-sort(unique(statuses$file_type))
  summary_rows<-lapply(types,function(t) {
    idx<-statuses$file_type==t
    vals<-statuses$audit_classification[idx]
    n_completed<-sum(vals%in%c("validated_zero_cases","validated_with_cases"))
    n_zero<-sum(vals=="validated_zero_cases")
    data.frame(
      file_type=t,
      total_sources=sum(idx),
      validated=n_completed,
      validated_with_cases=sum(vals=="validated_with_cases"),
      validated_zero_cases=n_zero,
      zero_case_rate_pct=if(n_completed)round(100*n_zero/n_completed,1) else NA_real_,
      unreadable=sum(vals=="unreadable"),
      not_yet_validated=sum(vals=="not_yet_validated"),
      stringsAsFactors=FALSE)
  })
  summary<-do.call(rbind,summary_rows)
  # Small-group-aware descriptive comparison. Statistical testing is only
  # shown when at least two file formats have completed assessments and
  # both zero-case and case-positive results occur.
  eligible<-summary$validated>0L
  test_result<-"Not available: insufficient validated comparisons."
  p_value<-NA_real_
  test_name<-"not_performed"
  if(sum(eligible)>=2L&&sum(summary$validated_zero_cases)>0L&&
     sum(summary$validated_with_cases)>0L) {
    tab<-rbind(
      zero=summary$validated_zero_cases[eligible],
      with_cases=summary$validated_with_cases[eligible])
    colnames(tab)<-summary$file_type[eligible]
    if(ncol(tab)==2L) {
      fit<-stats::fisher.test(tab)
      test_name<-"Fisher exact test"
      p_value<-fit$p.value
    } else {
      # Keep results deterministic by setting/recovering the RNG state.
      # Large sparse multi-category tables need a simulation rather than
      # unreliable asymptotic chi-square approximation.
      if(all(suppressWarnings(stats::chisq.test(tab)$expected)>=5)) {
        fit<-suppressWarnings(stats::chisq.test(tab))
        test_name<-"Pearson chi-square test"
        p_value<-fit$p.value
      } else {
        test_name<-"not_performed_sparse_table"
      }
    }
    if(!is.na(p_value))
      test_result<-paste0(test_name,": p=",
                          format(p_value,digits=4,scientific=FALSE),
                          ". Exploratory only; processing order, source types ",
                          "and selection can confound this association.")
    else test_result<-"No inferential test: sparse or small contingency table."
  }
  outdir<-dirname(previous$status_csv)
  csv_write<-function(tab,p) {
    con<-file(p,open="wb")
    on.exit(close(con),add=TRUE)
    writeBin(as.raw(c(239L,187L,191L)),con)
    utils::write.table(tab,con,sep=",",row.names=FALSE,col.names=TRUE,
                       quote=TRUE,qmethod="double",na="")
    invisible(p)
  }
  summary_csv<-file.path(outdir,"zero_case_rates_by_filetype.csv")
  records_csv<-file.path(outdir,"zero_case_filetype_audit_all_records.csv")
  zero_csv<-file.path(outdir,"zero_case_sources_to_review.csv")
  csv_write(summary,summary_csv)
  csv_write(statuses,records_csv)
  csv_write(statuses[statuses$potential_false_negative,,drop=FALSE],zero_csv)
  plot_path<-file.path(outdir,"zero_case_rate_by_filetype.png")
  tryCatch({
    grDevices::png(plot_path,width=1400,height=800,res=150)
    on.exit(grDevices::dev.off(),add=TRUE)
    v<-summary$zero_case_rate_pct[eligible]
    names(v)<-paste0(summary$file_type[eligible]," (n=",
                     summary$validated[eligible],")")
    if(length(v)) {
      oldpar<-graphics::par(mar=c(5,12,4,2))
      on.exit(graphics::par(oldpar),add=TRUE)
      graphics::barplot(rev(v),horiz=TRUE,las=1,xlim=c(0,100),
                        xlab="Zero-case assessments / completed assessments (%)",
                        main="OSTARA grey literature: zero-case rates by original file type")
    } else graphics::plot.new()
  },error=function(e)warning("Could not draw audit chart: ",
                               conditionMessage(e)))
  escape<-function(x) {
    x<-gsub("&","&amp;",as.character(x),fixed=TRUE)
    x<-gsub("<","&lt;",x,fixed=TRUE)
    gsub(">","&gt;",x,fixed=TRUE)
  }
  table_html<-function(df) {
    head<-paste0("<tr>",paste0("<th>",escape(names(df)),"</th>",
                               collapse=""),"</tr>")
    body<-apply(df,1L,function(row)
      paste0("<tr>",paste0("<td>",escape(ifelse(is.na(row),"",row)),
                          "</td>",collapse=""),"</tr>"))
    paste0("<table>",head,paste(body,collapse="\n"),"</table>")
  }
  report<-file.path(outdir,"zero_case_filetype_audit.html")
  html<-c("<!doctype html><html><head><meta charset='UTF-8'>",
   "<title>OSTARA zero-case file-type audit</title>",
   "<style>body{font-family:system-ui,Arial,sans-serif;max-width:1100px;margin:35px auto;padding:0 20px}table{border-collapse:collapse;width:100%}td,th{border-bottom:1px solid #ddd;padding:8px;text-align:left}th{background:#eee}</style>",
   "</head><body><h1>Zero-case outcomes by original full-text file type</h1>",
   "<p>This is an offline diagnostic of completed case-extraction checkpoints. It does not reclassify publications or make model requests.</p>",
   table_html(summary),
   paste0("<p><strong>Association test:</strong> ",escape(test_result),"</p>"),
   "<p><strong>Interpretation:</strong> zero-case rate uses only completed model assessments as denominator; unreadable and not-yet-validated documents are shown separately. A zero-case response is not independently verified and may be a false negative. File-type association does not establish causation.</p>",
   paste0("<p>Zero-case publications requiring checking: ",
          sum(statuses$potential_false_negative),".</p>"),
   "<p>Check source PDFs/HTML against the text supplied to the model before changing the eligibility rules. The staged XML for HTML/ASPX is a conversion, not necessarily the full page. Also compare source lengths, tables and annexes in source documents.</p>",
   "<p>CSV files contain every record and the zero-case review shortlist, including original file names. Data are exported as UTF-8 with BOM for Excel.</p>",
   "</body></html>")
  writeLines(html,report,useBytes=TRUE)
  message("[OSTARA zero-case audit] Completed ",
    sum(completed),"; zero-case ",sum(statuses$potential_false_negative),
    "; outputs: ",outdir)
  invisible(list(report=report,summary_csv=summary_csv,
    records_csv=records_csv,zero_case_csv=zero_csv,plot=plot_path,
    summary=summary,comparison=test_result,
    validated=sum(completed),
    zero_case=sum(statuses$potential_false_negative)))
}
