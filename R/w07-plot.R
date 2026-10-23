#' Plot the frequency of mechanisms in a completed W07 run
#'
#' Counts distinct publications per mechanism; publications assigned multiple
#' mechanisms contribute once to each relevant bar. Reads existing local
#' W07 files without modifying them or making API calls.
#' @param w07 Path to a completed W07 run (contains coding.jsonl and ontology.csv).
#' @param include_zero Show approved mechanisms with zero assignments.
#' @param output Optional PNG or PDF destination. By default return a ggplot.
#' @param width,height Figure dimensions in inches for export.
#' @return A ggplot object, invisibly if written to a file.
#' @export
plot_w07_mechanisms <- function(w07, include_zero=TRUE, output=NULL,
                                width=12, height=10) {
  if(!requireNamespace("ggplot2",quietly=TRUE))
    stop("Install ggplot2 first: install.packages('ggplot2')")
  w07<-normalizePath(w07,winslash="/",mustWork=TRUE)
  coding<-file.path(w07,"coding.jsonl")
  ontology<-file.path(w07,"ontology.csv")
  if(!all(file.exists(c(coding,ontology))))
    stop("W07 run must contain coding.jsonl and ontology.csv.")
  lines<-readLines(coding,warn=FALSE,encoding="UTF-8")
  lines<-lines[nzchar(trimws(lines))]
  if(!length(lines))stop("W07 coding file is empty.")
  records<-lapply(lines,jsonlite::fromJSON,simplifyVector=FALSE)
  ids<-vapply(records,function(z)as.character(z$record_id),character(1))
  if(anyDuplicated(ids))stop("Duplicate publication IDs in coding.jsonl.")
  onto<-utils::read.csv(ontology,stringsAsFactors=FALSE,
                        check.names=FALSE,fileEncoding="UTF-8-BOM")
  required<-c("category_id","mechanism_id","mechanism_source_label")
  if(!all(required %in% names(onto)))stop("Ontology lacks expected ID/label columns.")
  if(anyDuplicated(onto$mechanism_id))stop("Duplicate mechanism IDs in ontology.")
  assignments<-unlist(lapply(records,function(z) {
    ms<-z$mechanisms
    if(is.null(ms)||!length(ms))return(character())
    unique(vapply(ms,function(m)as.character(m$mechanism_id),character(1)))
  }),use.names=FALSE)
  if(any(!assignments %in% onto$mechanism_id))
    stop("Coding includes mechanism IDs absent from ontology.")
  counts<-table(factor(assignments,levels=onto$mechanism_id))
  onto$n<-as.integer(counts)
  if(!include_zero)onto<-onto[onto$n>0L,,drop=FALSE]
  if(!nrow(onto))stop("No mechanisms to plot.")
  onto$category<-as.character(onto$category_id)
  # Wrap long descriptions without requiring another package.
  wrap<-function(x,width=42) vapply(as.character(x),function(s)
    paste(strwrap(s,width=width),collapse="\n"),character(1))
  onto$label<-paste0(onto$mechanism_id,"  ",wrap(onto$mechanism_source_label))
  onto$category<-factor(onto$category,levels=unique(as.character(onto$category)))
  # Ordering within facets: unique factor level for each coded mechanism.
  ordered<-onto[order(onto$category,-onto$n,onto$mechanism_id),,drop=FALSE]
  ordered$label<-factor(ordered$label,levels=rev(ordered$label))
  total<-length(records)
  active<-sum(vapply(records,function(z)length(z$mechanisms)>0,logical(1)))
  p<-ggplot2::ggplot(ordered,ggplot2::aes(x=n,y=label,fill=category))+
    ggplot2::geom_col(width=.72,show.legend=FALSE)+
    ggplot2::geom_text(ggplot2::aes(label=n),hjust=-.25,
                       size=3.4,fontface="bold",colour="#25394B")+
    ggplot2::facet_grid(category~.,scales="free_y",space="free_y",
                        switch="y")+
    ggplot2::scale_x_continuous(expand=ggplot2::expansion(mult=c(0,.13)))+
    ggplot2::scale_fill_brewer(palette="Set2")+
    ggplot2::labs(
      title="Collective action mechanisms in the evidence",
      subtitle=paste0(total," publications coded  ·  ",active,
                       " with at least one mechanism  ·  ",
                       sum(onto$n)," mechanism assignments"),
      x="Number of publications",y=NULL,
      caption="Provisional W07 abstract-level coding. Publications may receive multiple codes.\nCounts are not estimates of mechanism prevalence or effectiveness.")+
    ggplot2::theme_minimal(base_size=12)+
    ggplot2::theme(
      plot.background=ggplot2::element_rect(fill="#FFFFFF",colour=NA),
      panel.background=ggplot2::element_rect(fill="#FFFFFF",colour=NA),
      panel.grid.major.y=ggplot2::element_blank(),
      panel.grid.minor=ggplot2::element_blank(),
      panel.grid.major.x=ggplot2::element_line(colour="#E9EDF1",linewidth=.35),
      axis.text.y=ggplot2::element_text(colour="#25394B",size=9.5,
                                       lineheight=1.05),
      axis.text.x=ggplot2::element_text(colour="#657789"),
      strip.placement="outside",
      strip.background=ggplot2::element_blank(),
      strip.text.y.left=ggplot2::element_text(angle=0,face="bold",
                                               colour="#34566D",size=9),
      panel.spacing.y=grid::unit(1.1,"lines"),
      plot.title=ggplot2::element_text(face="bold",size=21,colour="#17364B",
                                       margin=ggplot2::margin(b=7)),
      plot.subtitle=ggplot2::element_text(colour="#597084",size=11,
                                         margin=ggplot2::margin(b=18)),
      plot.caption=ggplot2::element_text(colour="#667D8A",hjust=0,size=8.5,
                                        margin=ggplot2::margin(t=15)),
      plot.margin=ggplot2::margin(18,20,18,22))
  if(!is.null(output)) {
    ext<-tolower(tools::file_ext(output))
    if(!ext %in% c("png","pdf"))stop("Output must end in .png or .pdf")
    ggplot2::ggsave(output,p,width=width,height=height,
                    dpi=320,bg="white",limitsize=FALSE)
    message("[W07] Figure saved: ",normalizePath(output,mustWork=TRUE))
    return(invisible(p))
  }
  p
}
