# Local W01 Shiny reviewer. No hosted backend.
suppressPackageStartupMessages({library(shiny);library(jsonlite);library(digest)})
argv <- commandArgs(trailingOnly=TRUE)
stopifnot(length(argv)>=2L)
queue_path<-normalizePath(argv[[1]],mustWork=TRUE)
out_dir<-normalizePath(argv[[2]],mustWork=TRUE)
lines<-readLines(queue_path,warn=FALSE)
cases<-lapply(lines[nzchar(trimws(lines))],jsonlite::fromJSON,simplifyVector=FALSE)
if(!length(cases)) stop("No cases supplied")
ids<-vapply(cases,function(z) as.character(z$review_case_id),character(1))
if(anyDuplicated(ids)||any(!nzchar(ids))) stop("Invalid or repeated case IDs")
if(any(!vapply(cases,function(z) identical(z$schema,"living-evidence-map-workflow01-duplicate-adjudication-case-v1"),logical(1)))) stop("Invalid queue schema")
sha<-digest::digest(file=queue_path,algo="sha256",serialize=FALSE)
identity_path<-file.path(out_dir,"reviewer.json")
decision_path<-file.path(out_dir,"decisions.jsonl")
correction_path<-file.path(out_dir,"abstract_corrections.jsonl")
fallback<-function(x,y) if(is.null(x)||!length(x)) y else x
value<-function(x) {
  if(is.null(x)||!length(x)) return("")
  paste(as.character(unlist(x,use.names=FALSE)),collapse="; ")
}
utc<-function() format(Sys.time(),tz="UTC",format="%Y-%m-%dT%H:%M:%SZ")
read_events<-function(p) {
  if(!file.exists(p)) return(list())
  x<-readLines(p,warn=FALSE)
  lapply(x[nzchar(trimws(x))],jsonlite::fromJSON,simplifyVector=FALSE)
}
append_event<-function(p,x) cat(jsonlite::toJSON(x,auto_unbox=TRUE,null="null"),
                                 "\n",file=p,append=TRUE,sep="")
latest<-function(events,field) {
  z<-list()
  for(x in events) z[[as.character(x[[field]])]]<-x
  z
}
doi_link<-function(x) {
  d<-trimws(value(x))
  d<-sub("^https?://(dx[.])?doi[.]org/","",d,ignore.case=TRUE)
  if(!grepl("^10[.][0-9]{4,9}/",d)) return(NULL)
  paste0("https://doi.org/",utils::URLencode(d,reserved=TRUE))
}
prior_name<-if(file.exists(identity_path)) value(jsonlite::fromJSON(identity_path)$reviewer) else ""
ui<-fluidPage(
  tags$head(tags$style(HTML(".entry{padding:16px;border:1px solid #ddd;border-radius:8px;overflow-wrap:anywhere}.entry pre{white-space:pre-wrap;font-family:inherit}.meta{margin:5px 0}"))),
  titlePanel("Evidence map · Local adjudication"),
  textOutput("who"),h4(textOutput("progress")),
  conditionalPanel("output.ready",
    fluidRow(column(6,actionButton("previous","Previous")),
             column(6,actionButton("following","Next"))),
    fluidRow(column(6,uiOutput("left")),column(6,uiOutput("right"))),
    radioButtons("decision","Decision",choices=c("Duplicate"="duplicate",
      "Not duplicate"="not_duplicate","Uncertain"="uncertain"),
      selected=character(0),inline=TRUE),
    textAreaInput("rationale","Rationale",rows=3,width="100%"),
    actionButton("save","Save decision",class="btn-primary"),
    downloadButton("download","Export decisions")))
server<-function(input,output,session) {
  reviewer<-reactiveVal(prior_name)
  idx<-reactiveVal(1L)
  decisions<-reactiveVal(read_events(decision_path))
  corrections<-reactiveVal(read_events(correction_path))
  output$ready<-reactive(nzchar(reviewer()))
  outputOptions(output,"ready",suspendWhenHidden=FALSE)
  observeEvent(TRUE,{
    if(!nzchar(reviewer())) showModal(modalDialog(title="Reviewer identification",
      textInput("name","Your name"),easyClose=FALSE,
      footer=actionButton("identify","Continue")))
  },once=TRUE)
  observeEvent(input$identify,{
    n<-trimws(fallback(input$name,""))
    if(!nzchar(n)){showNotification("Name required",type="error");return()}
    if(file.exists(identity_path)) stop("Existing reviewer identity cannot be replaced")
    jsonlite::write_json(list(reviewer=n,created_at_utc=utc()),identity_path,auto_unbox=TRUE)
    reviewer(n);removeModal()
  })
  output$who<-renderText(if(nzchar(reviewer())) paste("Reviewer:",reviewer()) else "Reviewer name required")
  current<-reactive(cases[[idx()]])
  output$progress<-renderText(paste("Case",idx(),"of",length(cases),"·",
                            length(latest(decisions(),"review_case_id")),"saved"))
  observeEvent(input$previous,idx(max(1L,idx()-1L)))
  observeEvent(input$following,idx(min(length(cases),idx()+1L)))
  observeEvent(idx(),{
    d<-latest(decisions(),"review_case_id")[[current()$review_case_id]]
    updateRadioButtons(session,"decision",selected=fallback(d$decision,character(0)))
    updateTextAreaInput(session,"rationale",value=fallback(d$rationale,""))
  })
  record_key<-function(rec) paste(value(rec$source),value(rec$source_record_id),sep="::")
  abstract<-function(rec) {
    c<-latest(corrections(),"record_key")[[record_key(rec)]]
    if(!is.null(c)) return(fallback(c$value,""))
    value(rec$abstract)
  }
  record_view<-function(side) renderUI({
    rec<-current()[[side]]
    title<-value(rec$title)
    doi<-doi_link(rec$doi)
    scholar<-paste0("https://scholar.google.co.uk/scholar?start=0&q=",
                    utils::URLencode(title,reserved=TRUE))
    div(class="entry",h4(if(side=="record_i") "Record A" else "Record B"),
      h4(title),p(strong("Authors: "),value(rec$authors)),
      p(strong("Year: "),value(rec$year)),
      p(strong("Journal/source: "),value(fallback(rec$journal,rec$source))),
      p(strong("Volume/issue/pages: "),paste(value(rec$volume),value(rec$issue),value(rec$pages))),
      p(strong("Source ID: "),record_key(rec)),
      if(!is.null(doi)) tags$a(href=doi,target="_blank",rel="noopener noreferrer","DOI"),
      tags$span("  "),
      tags$a(href=scholar,target="_blank",rel="noopener noreferrer","Google Scholar"),
      h5("Abstract"),pre(abstract(rec)),
      actionButton(paste0("edit_",side),"Edit abstract"),
      actionButton(paste0("delete_",side),"Delete abstract"))
  })
  output$left<-record_view("record_i")
  output$right<-record_view("record_j")
  for(side in c("record_i","record_j")) local({
    which<-side
    observeEvent(input[[paste0("edit_",which)]],{
      session$userData$edit_side<-which
      showModal(modalDialog(title="Edit abstract",
        textAreaInput("new_abstract","Abstract",rows=10,width="100%",
                      value=abstract(current()[[which]])),
        footer=tagList(modalButton("Cancel"),actionButton("save_abstract","Save correction"))))
    })
    observeEvent(input[[paste0("delete_",which)]],{
      session$userData$delete_side<-which
      showModal(modalDialog(title="Delete abstract",
        "Remove this abstract from the working record?",
        footer=tagList(modalButton("Cancel"),actionButton("confirm_delete","Delete"))))
    })
  })
  correction<-function(side,new,action) {
    req(nzchar(reviewer()))
    rec<-current()[[side]]
    append_event(correction_path,list(schema="local-abstract-correction-v1",
      record_key=record_key(rec),review_case_id=current()$review_case_id,
      reviewer=reviewer(),action=action,previous_value=abstract(rec),
      value=new,queue_sha256=sha,at_utc=utc()))
    corrections(read_events(correction_path));removeModal()
  }
  observeEvent(input$save_abstract,correction(session$userData$edit_side,input$new_abstract,"edit"))
  observeEvent(input$confirm_delete,correction(session$userData$delete_side,"","delete"))
  observeEvent(input$save,{
    req(nzchar(reviewer()))
    if(!input$decision %in% c("duplicate","not_duplicate","uncertain")) {
      showNotification("Choose a decision",type="error");return()
    }
    if(!nzchar(trimws(input$rationale))) {
      showNotification("Rationale required",type="error");return()
    }
    append_event(decision_path,list(schema="local-w01-decision-v1",
      review_case_id=current()$review_case_id,reviewer=reviewer(),
      decision=input$decision,rationale=trimws(input$rationale),
      queue_sha256=sha,resolved_at_utc=utc()))
    decisions(read_events(decision_path))
    if(idx()<length(cases))idx(idx()+1L)
  })
  output$download<-downloadHandler(filename=function() "w01-decisions.jsonl",
    content=function(file){
      ds<-latest(decisions(),"review_case_id")
      writeLines(vapply(ds,function(x) jsonlite::toJSON(x,auto_unbox=TRUE,null="null"),
                        character(1)),file)
    })
}
shiny::runApp(shinyApp(ui,server),launch.browser=TRUE)
