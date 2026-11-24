#!/usr/bin/env Rscript
# Offline generic query translation. Fail closed on unsupported Boolean syntax.
suppressPackageStartupMessages(library(jsonlite))
args <- commandArgs(trailingOnly=TRUE)
arg <- function(flag,default=NULL){i<-match(flag,args);if(is.na(i))return(default);if(i==length(args))stop(paste("Missing value after",flag),call.=FALSE);args[[i+1L]]}
config_path<-arg("--config","user_input/workflow00_search_strategy.json")
output_dir<-arg("--output-dir","outputs/updater/workflow00_plan")
run_type<-arg("--run-type")
additional<-arg("--additional-search-term","")
target<-arg("--expansion-target","")
ebsco_path<-arg("--ebsco-config","config/workflow00_ebsco_sources.json")
if(is.null(run_type)||!run_type%in%c("full","fortnightly","expansion"))stop("W00: run type must be full, fortnightly, expansion",call.=FALSE)
cfg<-fromJSON(config_path,simplifyVector=FALSE)
if(is.null(cfg$concepts)&&is.null(cfg$advanced_boolean))stop("W00: new strategy needs concepts/expression or advanced_boolean",call.=FALSE)
if(run_type=="expansion"&&(!nzchar(target)||!nzchar(additional)))stop("W00 expansion: both --expansion-target and --additional-search-term are required",call.=FALSE)
if(run_type!="expansion"&&(nzchar(target)||nzchar(additional)))stop("W00: expansion options allowed only in expansion mode",call.=FALSE)
if(!file.exists(ebsco_path))stop("W00: missing EBSCO source catalogue",call.=FALSE)
selection_path<-arg("--source-selection")
if(is.null(selection_path)||!file.exists(selection_path))stop("W00: --source-selection is required to translate only selected databases",call.=FALSE)
selected<-unlist(fromJSON(selection_path,simplifyVector=FALSE)$selected_sources,use.names=FALSE)
if(!length(selected)||anyDuplicated(selected))stop("W00: expected unique selected database IDs",call.=FALSE)
ebsco<-fromJSON(ebsco_path,simplifyVector=FALSE)
concepts<-cfg$concepts
simple_term<-function(x){
  if(length(x)!=1||!is.character(x)||!nzchar(trimws(x)))stop("W00: invalid/empty term",call.=FALSE)
  if(grepl("[()]",x))stop(sprintf("W00: term '%s' contains parentheses; use an advanced query override",x),call.=FALSE)
  # Term entries are atoms, not Boolean fragments; accepting operators here
  # would change the intended concept grammar for some database providers.
  unquoted<-gsub('"[^"]*"','',x,perl=TRUE)
  if(grepl("\\b(AND|OR|NOT|NEAR|ADJ|WITHIN|PRE|W)\\b",unquoted,ignore.case=TRUE,perl=TRUE) ||
     grepl("(^|[[:space:]])[A-Za-z_]+[[:digit:]]*:",unquoted,perl=TRUE)) {
    stop(sprintf("W00: term '%s' contains Boolean/proximity/field syntax; use an explicit database-specific override",x),call.=FALSE)
  }
  x
}
if(!is.null(concepts)){
  if(is.null(names(concepts))||any(!nzchar(names(concepts))))stop("W00: concepts must be named",call.=FALSE)
  if(is.null(cfg$expression)||!nzchar(cfg$expression))stop("W00: concept strategy requires an expression",call.=FALSE)
  terms<-lapply(concepts,function(x){v<-unlist(x$terms,use.names=FALSE);if(!length(v))stop("W00: empty concept",call.=FALSE);vapply(v,simple_term,character(1))})
  names(terms)<-names(concepts)
  # Require a Boolean expression made only from concept IDs, AND, OR, NOT and parentheses.
  tokens<-regmatches(cfg$expression,gregexpr("[A-Za-z][A-Za-z0-9_]*|[()]",cfg$expression,perl=TRUE))[[1]]
  remainder<-gsub("[A-Za-z][A-Za-z0-9_]*|[()]", "",cfg$expression,perl=TRUE)
  if(nzchar(trimws(remainder)))stop("W00: expression has unsupported characters; provide per-database overrides",call.=FALSE)
  nonops<-tokens[!toupper(tokens)%in%c("AND","OR","NOT") & !tokens%in%c("(",")")]
  if(length(setdiff(nonops,names(terms))))stop(sprintf("W00: unknown concepts: %s",paste(setdiff(nonops,names(terms)),collapse=", ")),call.=FALSE)
  if(!length(nonops))stop("W00: expression does not reference a concept",call.=FALSE)
} else {
  if(run_type=="expansion")stop("W00: expansion requires a named concept/subexpression; raw advanced_boolean has no unambiguous target",call.=FALSE)
  tokens<-character()
}
if(run_type=="expansion"&&!target%in%names(terms))stop(sprintf("W00: expansion target '%s' is not a defined concept",target),call.=FALSE)
# Preserve all existing concepts in the expansion and replace only the selected concept
# with the new term, subtracting its original matches. Native-ID deduplication remains downstream.
# Only Lens expands a terminal wildcard in a quoted phrase.
lens_expand_phrase <- function(term) {
  if (!startsWith(term, '"') || !endsWith(term, '*"')) return(term)
  phrase <- substr(term, 2L, nchar(term) - 1L)
  star <- gregexpr('*', phrase, fixed = TRUE)[[1L]]
  if (length(star) != 1L || star[[1L]] < 0L) return(term)
  base <- substr(phrase, 1L, nchar(phrase) - 1L)
  if (!grepl('[[:alpha:]]$', base)) return(term)
  paste0('("', base, '" OR "', base, 's")')
}
# Translate unquoted Scopus-style unordered proximity operators only.
# Unsupported providers deliberately broaden proximity to Boolean AND.
translation_notes <- list()
# Translate simple unordered proximity expressions as individual pairs.
# A Lens wildcard has no general finite expansion; require explicit word forms.
expand_native_proximity <- function(expr, source) {
  atom_pattern <- '"([^"\\\\]|\\\\.)*"|\\(|\\)|[^[:space:]()"]+'
  toks <- regmatches(expr, gregexpr(atom_pattern, expr, perl=TRUE))[[1L]]
  if (!length(toks)) stop("W00: empty proximity expression.")
  is_prox <- function(x) grepl("^W/[0-9]+$", x, ignore.case=TRUE)
  if (!any(is_prox(toks))) return(list(expr=expr, notes=character()))
  atom <- function(x) {
    x <- sub('^"(.*)"translate_advanced <- function(expr, source, fields=NULL) {
  native <- if(source %in% c("openalex","lens")) expand_native_proximity(expr,source) else list(expr=expr,notes=character())
  expr <- native$expr
  # Split into quoted phrases and unquoted spans, retaining every character.
  parts <- regmatches(expr, gregexpr('"([^"\\\\]|\\\\.)*"|[^"]+', expr, perl=TRUE))[[1L]]
  if (!identical(paste0(parts, collapse=""), expr))
    stop("W00: cannot safely tokenize advanced expression", call.=FALSE)
  notes <- native$notes
  for (i in seq_along(parts)) {
    x <- parts[[i]]
    if (startsWith(x, '"')) {
      # Lens query-string phrase wildcard syntax is not equivalent to
      # wildcard tokens. Broaden to conjunction of the original tokens.
      if (source=="lens" && grepl("*", x, fixed=TRUE)) {
        explicit <- unlist(cfg$lens_phrase_forms[[substr(x,2L,nchar(x)-1L)]],use.names=FALSE)
        if(length(explicit)) {
          if(any(!grepl("^[[:alnum:] _-]+$",explicit))) stop("W00: invalid Lens phrase forms.")
          parts[[i]] <- paste0("(",paste(sprintf('"%s"',explicit),collapse=" OR "),")")
          notes <- c(notes,"Lens wildcard phrase expanded to supplied explicit phrase forms")
          next
        }
        if(length(native$notes)) stop("W00: supply lens_phrase_forms for ",x," or database_overrides.lens.",call.=FALSE)
        words <- strsplit(substr(x, 2L, nchar(x)-1L), "[[:space:]]+")[[1L]]
        if (any(!grepl("^[[:alnum:]_-]+[*?]?$", words)))
          stop("W00: complex Lens wildcard phrase requires database override: ", x, call.=FALSE)
        parts[[i]] <- paste0("(", paste(words, collapse=" AND "), ")")
        notes <- c(notes, "Lens wildcard phrases broadened to AND of constituent tokens")
      }
      next
    }
    pattern <- "\\bW/([0-9]+)\\b"
    if (grepl(pattern, x, ignore.case=TRUE, perl=TRUE)) {
      replacement <- if(source=="scopus") "W/\\1" else if(source=="wos") "NEAR/\\1" else if(!is.null(fields)) "N\\1" else "AND"
      parts[[i]] <- gsub(pattern, replacement, x, ignore.case=TRUE, perl=TRUE)
      notes <- c(notes, if(replacement=="AND") "Unordered W/n proximity broadened to AND" else paste("Unordered W/n translated for",source))
    }
  }
  translation_notes[[source]] <<- unique(notes)
  paste0(parts, collapse="")
}
render<-function(source,fields=NULL){
  override<-cfg$database_overrides[[source]]
  if(!is.null(override)){
    if(run_type=="expansion"){
      mode_overrides<-cfg$expansion_overrides
      specific<-if(!is.null(mode_overrides))mode_overrides[[source]] else NULL
      if(is.null(specific))stop(sprintf("W00: %s uses a fixed query override; expansion requires expansion_overrides.%s for the selected concept",source,source),call.=FALSE)
      override<-specific
    }
    if(!is.character(override)||length(override)!=1L||!nzchar(trimws(override)))stop(sprintf("W00: override for %s must be a non-empty string",source),call.=FALSE)
    return(override)
  }
  if(is.null(concepts)){
    expr<-as.character(cfg$advanced_boolean)
    expr<-translate_advanced(expr,source,fields)
    if(length(expr)!=1L||!nzchar(trimws(expr)))stop("W00: missing advanced Boolean expression",call.=FALSE)
    if(source=="openalex")return(paste0(sprintf("works where title-abstract-keywords has (%s)",expr),if(!identical(cfg$exclude_datasets,FALSE))" and type is not [dataset](dataset)" else ""))
    if(source=="lens")return(paste0(sprintf("(title:(%s) OR abstract:(%s) OR keyword:(%s))",expr,expr,expr),if(!identical(cfg$exclude_datasets,FALSE))" AND NOT publication_type:dataset" else ""))
    if(source=="scopus")return(sprintf("TITLE-ABS-KEY(%s)",expr))
    if(source%in%c("agricola","pubmed","ethos","cba","epmc_preprints")){
      src<-c(agricola="AGR",pubmed="MED",ethos="ETH",cba="CBA",epmc_preprints="PPR")[[source]]
      return(sprintf("SRC:%s AND TITLE_ABS:(%s)",src,expr))
    }
    if(source=="wos"){
      translation_notes[[source]] <<- unique(c(translation_notes[[source]], "Starter API TS includes title, abstract, author keywords and Keywords Plus"))
      return(sprintf("TS=(%s)",expr))
    }
    if(!is.null(fields))return(paste0("(",paste(sprintf("%s (%s)",fields,expr),collapse=" OR "),")"))
    stop(sprintf("W00: unsupported source %s",source),call.=FALSE)
  }
  # Render the complete Boolean expression inside one search field when the
  # source's legacy grammar scopes AND/OR within that field (Scopus/Europe PMC/WoS).
  plain<-function(vals){
    paste(vapply(tokens,function(t){
      if(t%in%names(vals))return(paste0("(",paste(vals[[t]],collapse=" OR "),")"))
      if(t%in%c("(",")"))return(t)
      toupper(t)
    },character(1)),collapse=" ")
  }
  fielded<-function(vals){
    joined<-lapply(vals,function(v)paste(v,collapse=" OR "))
    paste(vapply(tokens,function(t){
      if(t%in%names(joined)){
        term<-joined[[t]]
        if(source=="lens")return(paste0("(",paste(sprintf("%s:(%s)",c("title","abstract","keyword"),term),collapse=" OR "),")"))
        if(!is.null(fields))return(paste0("(",paste(sprintf("%s (%s)",fields,term),collapse=" OR "),")"))
      }
      if(t%in%c("(",")"))return(t)
      toupper(t)
    },character(1)),collapse=" ")
  }
  render_complete<-function(vals){
    expr<-plain(vals)
    # Match established legacy grouping exactly for two-concept AND searches.
    is_pair<-length(tokens)==3L && tokens[[2L]]=="AND" &&
      tokens[[1L]]%in%names(vals) && tokens[[3L]]%in%names(vals)
    if(is_pair){
      pair_expr<-sprintf("(%s) AND (%s)",
        paste(vals[[tokens[[1L]]]],collapse=" OR "),
        paste(vals[[tokens[[3L]]]],collapse=" OR "))
    } else pair_expr<-expr
    if(source=="openalex")return(sprintf("works where title-abstract-keywords has (%s)",pair_expr))
    if(source=="scopus")return(sprintf("TITLE-ABS-KEY(%s)",pair_expr))
    if(source%in%c("agricola","pubmed","ethos","cba","epmc_preprints")){
      src<-c(agricola="AGR",pubmed="MED",ethos="ETH",cba="CBA",epmc_preprints="PPR")[[source]]
      return(sprintf("SRC:%s AND TITLE_ABS:(%s)",src,pair_expr))
    }
    if(source=="wos"){
      translation_notes[[source]] <<- unique(c(translation_notes[[source]], "Starter API TS includes title, abstract, author keywords and Keywords Plus"))
      return(sprintf("TS=(%s)",pair_expr))
    }
    if(source=="lens"&&is_pair){
      block<-function(t){
        txt<-paste(t,collapse=" OR ")
        sprintf("(title:(%s) OR abstract:(%s) OR keyword:(%s))",txt,txt,txt)
      }
      return(sprintf("(%s AND %s)",block(vals[[tokens[[1L]]]]),block(vals[[tokens[[3L]]]])))
    }
    if(is_pair&&!is.null(fields)){
      block<-function(v)paste0("(",paste(sprintf("%s (%s)",fields,paste(v,collapse=" OR ")),collapse=" OR "),")")
      return(sprintf("(%s AND %s)",block(vals[[tokens[[1L]]]]),block(vals[[tokens[[3L]]]])))
    }
    if(source=="lens"||!is.null(fields))return(fielded(vals))
    stop(sprintf("W00: %s has no verified generic translation; supply database_overrides.%s",source,source),call.=FALSE)
  }
  exclude_dataset <- function(query) {
    if(identical(cfg$exclude_datasets,FALSE)) return(query)
    if(source=="lens") return(paste0("(",query,") AND NOT publication_type:dataset"))
    if(source=="openalex") return(paste0(query," and type is not [dataset](dataset)"))
    query
  }
  base<-render_complete(if(source=='lens') lapply(terms, function(v) vapply(v, lens_expand_phrase, character(1))) else terms)
  if(run_type!="expansion")return(exclude_dataset(base))
  amended<-terms
  amended[[target]]<-c(terms[[target]],additional)
  expanded<-render_complete(if(source=='lens') lapply(amended, function(v) vapply(v, lens_expand_phrase, character(1))) else amended)
  # Parenthesise complete source query; source-specific SRC filters must remain
  # scoped to both branches. This preserves nested AND/OR/NOT semantics.
  sprintf("((%s) AND NOT (%s))",expanded,base)
}
supported<-c("lens","scopus","openalex","agricola","pubmed","ethos","cba","epmc_preprints","wos",names(ebsco$sources))
if(length(setdiff(selected,supported)))stop(sprintf("W00: no translator for selected source(s): %s",paste(setdiff(selected,supported),collapse=", ")),call.=FALSE)
source_names<-selected
# Fortnightly overrides are complete source queries. Do not first translate the
# base strategy: that would incorrectly demand baseline overrides for sources
# whose dated query has been explicitly supplied.
queries<-if(run_type=="fortnightly")list() else setNames(lapply(source_names,function(n){
  fields<-if(n%in%names(ebsco$sources))unlist(ebsco$sources[[n]]$search_fields,use.names=FALSE) else NULL
  render(n,fields)
}),source_names)
if(run_type=="fortnightly"){
  # Explicit full provider queries preserve the user's selected indexing/publication
  # date semantics. Never replace a query with an arbitrary publication-year filter.
  missing<-setdiff(source_names,names(cfg$fortnightly_overrides))
  if(length(missing))stop(sprintf("W00: fortnightly mode needs explicit dated queries for: %s",paste(missing,collapse=", ")),call.=FALSE)
  queries<-setNames(lapply(source_names,function(name){
    q<-cfg$fortnightly_overrides[[name]]
    if(!is.character(q)||length(q)!=1L||!nzchar(trimws(q)))stop(sprintf("W00: empty/invalid fortnightly override for %s",name),call.=FALSE)
    q
  }),source_names)
}
dir.create(output_dir,recursive=TRUE,showWarnings=FALSE)
plan<-list(workflow="00",status="planned",run_type=run_type,search_version=cfg$search_version,
  source_queries=queries,expansion=list(target=if(run_type=="expansion")target else NULL,term=if(run_type=="expansion")additional else NULL),
  translation="topic-agnostic-v2",translation_notes=translation_notes,fortnightly_window=NULL,
  source_mode_support=setNames(lapply(source_names,function(x)list(full=TRUE,fortnightly=TRUE,expansion=TRUE)),source_names))
writeLines(toJSON(plan,auto_unbox=TRUE,pretty=TRUE,null="null"),file.path(output_dir,"search_plan.json"))
writeLines(toJSON(queries,auto_unbox=TRUE,pretty=TRUE,null="null"),file.path(output_dir,"source_queries.json"))
message(sprintf("W00: translated %d database queries (no API calls)",length(queries)))
, '\\1', x)
    if (!grepl("^[[:alnum:] _?*-]+$", x) || toupper(x) %in% c("AND","OR","NOT"))
      stop("W00: unsupported proximity operand; supply database_overrides.", call.=FALSE)
    x
  }
  operand <- function(i, direction) {
    if (i < 1L || i > length(toks)) stop("W00: missing proximity operand.")
    boundary <- if (direction<0L) ")" else "("
    if (toks[[i]] != boundary) return(list(lo=i, hi=i, terms=atom(toks[[i]])))
    j <- i+direction
    opposite <- if (direction<0L) "(" else ")"
    while (j>=1L && j<=length(toks) && toks[[j]]!=opposite) j<-j+direction
    if (j<1L || j>length(toks)) stop("W00: unbalanced proximity group.")
    lo<-min(i,j); hi<-max(i,j)
    inside<-toks[seq.int(lo+1L,hi-1L)]
    if (length(inside)%%2L!=1L || (length(inside)>1L &&
         any(toupper(inside[seq.int(2L,length(inside),by=2L)])!="OR")))
      stop("W00: proximity group must contain only OR-separated terms; supply database_overrides.",call.=FALSE)
    list(lo=lo,hi=hi,terms=vapply(inside[seq.int(1L,length(inside),by=2L)],atom,character(1)))
  }
  expand_lens <- function(x) {
    if (!grepl("[*?]",x)) return(x)
    vals <- unlist(cfg$lens_word_forms[[x]],use.names=FALSE)
    if (!length(vals) || any(!grepl("^[[:alnum:] _-]+$",vals)))
      stop("W00: Lens proximity requires lens_word_forms for ",x,
           " or a complete database_overrides.lens query.",call.=FALSE)
    unique(vals)
  }
  n <- 0L
  while (any(is_prox(toks))) {
    i <- which(is_prox(toks))[1L]
    l <- operand(i-1L,-1L); r <- operand(i+1L,1L)
    distance<-as.integer(sub("^W/","",toupper(toks[[i]])))
    if (is.na(distance)||distance<1L) stop("W00: invalid proximity distance.")
    left<-l$terms; right<-r$terms
    if(source=="lens") {
      left<-unique(unlist(lapply(left,expand_lens),use.names=FALSE))
      right<-unique(unlist(lapply(right,expand_lens),use.names=FALSE))
    }
    grid<-expand.grid(left=left,right=right,stringsAsFactors=FALSE)
    if(source=="openalex") pairs<-sprintf('("%s" and "%s" within %d words of each other)',grid$left,grid$right,distance)
    else pairs<-sprintf('"%s %s"~%d',grid$left,grid$right,distance)
    n<-n+length(pairs)
    toks<-c(if(l$lo>1L)toks[seq_len(l$lo-1L)],paste0("(",paste(unique(pairs),collapse=" OR "),")"),
            if(r$hi<length(toks))toks[seq.int(r$hi+1L,length(toks))])
  }
  list(expr=paste(toks,collapse=" "),
       notes=c(sprintf("Expanded unordered proximity into %d individual pairs",n),
       if(source=="lens")"Lens phrase slop and explicit word forms approximate original proximity"))
}
translate_advanced <- function(expr, source, fields=NULL) {
  # Split into quoted phrases and unquoted spans, retaining every character.
  parts <- regmatches(expr, gregexpr('"([^"\\\\]|\\\\.)*"|[^"]+', expr, perl=TRUE))[[1L]]
  if (!identical(paste0(parts, collapse=""), expr))
    stop("W00: cannot safely tokenize advanced expression", call.=FALSE)
  notes <- character()
  for (i in seq_along(parts)) {
    x <- parts[[i]]
    if (startsWith(x, '"')) {
      # Lens query-string phrase wildcard syntax is not equivalent to
      # wildcard tokens. Broaden to conjunction of the original tokens.
      if (source=="lens" && grepl("*", x, fixed=TRUE)) {
        words <- strsplit(substr(x, 2L, nchar(x)-1L), "[[:space:]]+")[[1L]]
        if (any(!grepl("^[[:alnum:]_-]+[*?]?$", words)))
          stop("W00: complex Lens wildcard phrase requires database override: ", x, call.=FALSE)
        parts[[i]] <- paste0("(", paste(words, collapse=" AND "), ")")
        notes <- c(notes, "Lens wildcard phrases broadened to AND of constituent tokens")
      }
      next
    }
    pattern <- "\\bW/([0-9]+)\\b"
    if (grepl(pattern, x, ignore.case=TRUE, perl=TRUE)) {
      replacement <- if(source=="scopus") "W/\\1" else if(source=="wos") "NEAR/\\1" else if(!is.null(fields)) "N\\1" else "AND"
      parts[[i]] <- gsub(pattern, replacement, x, ignore.case=TRUE, perl=TRUE)
      notes <- c(notes, if(replacement=="AND") "Unordered W/n proximity broadened to AND" else paste("Unordered W/n translated for",source))
    }
  }
  translation_notes[[source]] <<- unique(notes)
  paste0(parts, collapse="")
}
render<-function(source,fields=NULL){
  override<-cfg$database_overrides[[source]]
  if(!is.null(override)){
    if(run_type=="expansion"){
      mode_overrides<-cfg$expansion_overrides
      specific<-if(!is.null(mode_overrides))mode_overrides[[source]] else NULL
      if(is.null(specific))stop(sprintf("W00: %s uses a fixed query override; expansion requires expansion_overrides.%s for the selected concept",source,source),call.=FALSE)
      override<-specific
    }
    if(!is.character(override)||length(override)!=1L||!nzchar(trimws(override)))stop(sprintf("W00: override for %s must be a non-empty string",source),call.=FALSE)
    return(override)
  }
  if(is.null(concepts)){
    expr<-as.character(cfg$advanced_boolean)
    expr<-translate_advanced(expr,source,fields)
    if(length(expr)!=1L||!nzchar(trimws(expr)))stop("W00: missing advanced Boolean expression",call.=FALSE)
    if(source=="openalex")return(sprintf("works where title-abstract-keywords has (%s)",expr))
    if(source=="lens")return(sprintf("(title:(%s) OR abstract:(%s) OR keyword:(%s))",expr,expr,expr))
    if(source=="scopus")return(sprintf("TITLE-ABS-KEY(%s)",expr))
    if(source%in%c("agricola","pubmed","ethos","cba","epmc_preprints")){
      src<-c(agricola="AGR",pubmed="MED",ethos="ETH",cba="CBA",epmc_preprints="PPR")[[source]]
      return(sprintf("SRC:%s AND TITLE_ABS:(%s)",src,expr))
    }
    if(source=="wos"){
      translation_notes[[source]] <<- unique(c(translation_notes[[source]], "Starter API TS includes title, abstract, author keywords and Keywords Plus"))
      return(sprintf("TS=(%s)",expr))
    }
    if(!is.null(fields))return(paste0("(",paste(sprintf("%s (%s)",fields,expr),collapse=" OR "),")"))
    stop(sprintf("W00: unsupported source %s",source),call.=FALSE)
  }
  # Render the complete Boolean expression inside one search field when the
  # source's legacy grammar scopes AND/OR within that field (Scopus/Europe PMC/WoS).
  plain<-function(vals){
    paste(vapply(tokens,function(t){
      if(t%in%names(vals))return(paste0("(",paste(vals[[t]],collapse=" OR "),")"))
      if(t%in%c("(",")"))return(t)
      toupper(t)
    },character(1)),collapse=" ")
  }
  fielded<-function(vals){
    joined<-lapply(vals,function(v)paste(v,collapse=" OR "))
    paste(vapply(tokens,function(t){
      if(t%in%names(joined)){
        term<-joined[[t]]
        if(source=="lens")return(paste0("(",paste(sprintf("%s:(%s)",c("title","abstract","keyword"),term),collapse=" OR "),")"))
        if(!is.null(fields))return(paste0("(",paste(sprintf("%s (%s)",fields,term),collapse=" OR "),")"))
      }
      if(t%in%c("(",")"))return(t)
      toupper(t)
    },character(1)),collapse=" ")
  }
  render_complete<-function(vals){
    expr<-plain(vals)
    # Match established legacy grouping exactly for two-concept AND searches.
    is_pair<-length(tokens)==3L && tokens[[2L]]=="AND" &&
      tokens[[1L]]%in%names(vals) && tokens[[3L]]%in%names(vals)
    if(is_pair){
      pair_expr<-sprintf("(%s) AND (%s)",
        paste(vals[[tokens[[1L]]]],collapse=" OR "),
        paste(vals[[tokens[[3L]]]],collapse=" OR "))
    } else pair_expr<-expr
    if(source=="openalex")return(sprintf("works where title-abstract-keywords has (%s)",pair_expr))
    if(source=="scopus")return(sprintf("TITLE-ABS-KEY(%s)",pair_expr))
    if(source%in%c("agricola","pubmed","ethos","cba","epmc_preprints")){
      src<-c(agricola="AGR",pubmed="MED",ethos="ETH",cba="CBA",epmc_preprints="PPR")[[source]]
      return(sprintf("SRC:%s AND TITLE_ABS:(%s)",src,pair_expr))
    }
    if(source=="wos"){
      translation_notes[[source]] <<- unique(c(translation_notes[[source]], "Starter API TS includes title, abstract, author keywords and Keywords Plus"))
      return(sprintf("TS=(%s)",pair_expr))
    }
    if(source=="lens"&&is_pair){
      block<-function(t){
        txt<-paste(t,collapse=" OR ")
        sprintf("(title:(%s) OR abstract:(%s) OR keyword:(%s))",txt,txt,txt)
      }
      return(sprintf("(%s AND %s)",block(vals[[tokens[[1L]]]]),block(vals[[tokens[[3L]]]])))
    }
    if(is_pair&&!is.null(fields)){
      block<-function(v)paste0("(",paste(sprintf("%s (%s)",fields,paste(v,collapse=" OR ")),collapse=" OR "),")")
      return(sprintf("(%s AND %s)",block(vals[[tokens[[1L]]]]),block(vals[[tokens[[3L]]]])))
    }
    if(source=="lens"||!is.null(fields))return(fielded(vals))
    stop(sprintf("W00: %s has no verified generic translation; supply database_overrides.%s",source,source),call.=FALSE)
  }
  base<-render_complete(if(source=='lens') lapply(terms, function(v) vapply(v, lens_expand_phrase, character(1))) else terms)
  if(run_type!="expansion")return(base)
  amended<-terms
  amended[[target]]<-c(terms[[target]],additional)
  expanded<-render_complete(if(source=='lens') lapply(amended, function(v) vapply(v, lens_expand_phrase, character(1))) else amended)
  # Parenthesise complete source query; source-specific SRC filters must remain
  # scoped to both branches. This preserves nested AND/OR/NOT semantics.
  sprintf("((%s) AND NOT (%s))",expanded,base)
}
supported<-c("lens","scopus","openalex","agricola","pubmed","ethos","cba","epmc_preprints","wos",names(ebsco$sources))
if(length(setdiff(selected,supported)))stop(sprintf("W00: no translator for selected source(s): %s",paste(setdiff(selected,supported),collapse=", ")),call.=FALSE)
source_names<-selected
# Fortnightly overrides are complete source queries. Do not first translate the
# base strategy: that would incorrectly demand baseline overrides for sources
# whose dated query has been explicitly supplied.
queries<-if(run_type=="fortnightly")list() else setNames(lapply(source_names,function(n){
  fields<-if(n%in%names(ebsco$sources))unlist(ebsco$sources[[n]]$search_fields,use.names=FALSE) else NULL
  render(n,fields)
}),source_names)
if(run_type=="fortnightly"){
  # Explicit full provider queries preserve the user's selected indexing/publication
  # date semantics. Never replace a query with an arbitrary publication-year filter.
  missing<-setdiff(source_names,names(cfg$fortnightly_overrides))
  if(length(missing))stop(sprintf("W00: fortnightly mode needs explicit dated queries for: %s",paste(missing,collapse=", ")),call.=FALSE)
  queries<-setNames(lapply(source_names,function(name){
    q<-cfg$fortnightly_overrides[[name]]
    if(!is.character(q)||length(q)!=1L||!nzchar(trimws(q)))stop(sprintf("W00: empty/invalid fortnightly override for %s",name),call.=FALSE)
    q
  }),source_names)
}
dir.create(output_dir,recursive=TRUE,showWarnings=FALSE)
plan<-list(workflow="00",status="planned",run_type=run_type,search_version=cfg$search_version,
  source_queries=queries,expansion=list(target=if(run_type=="expansion")target else NULL,term=if(run_type=="expansion")additional else NULL),
  translation="topic-agnostic-v1",translation_notes=translation_notes,fortnightly_window=NULL,
  source_mode_support=setNames(lapply(source_names,function(x)list(full=TRUE,fortnightly=TRUE,expansion=TRUE)),source_names))
writeLines(toJSON(plan,auto_unbox=TRUE,pretty=TRUE,null="null"),file.path(output_dir,"search_plan.json"))
writeLines(toJSON(queries,auto_unbox=TRUE,pretty=TRUE,null="null"),file.path(output_dir,"source_queries.json"))
message(sprintf("W00: translated %d database queries (no API calls)",length(queries)))
