# Local W04 reviewer: bibliographic records, model decisions, local attribution.
suppressPackageStartupMessages({library(shiny);library(jsonlite);library(digest)})
args<-commandArgs(trailingOnly=TRUE)
stopifnot(length(args)>=2L)
queue<-normalizePath(args[[1]],mustWork=TRUE)
out<-normalizePath(args[[2]],mustWork=TRUE)
readjl<-function(p) {
  l<-readLines(p,warn=FALSE,encoding="UTF-8")
  lapply(l[nzchar(trimws(l))],jsonlite::fromJSON,simplifyVector=FALSE)
}
cases<-readjl(queue)
if(!length(cases))stop("W04 queue empty.")
ids<-vapply(cases,function(x)as.character(x$review_case_id),character(1))
if(anyDuplicated(ids)||any(!vapply(cases,function(x)
  identical(x$schema,"local-w04-review-case-v1"),logical(1))))
  stop("Invalid W04 review queue.")
queuehash<-digest::digest(file=queue,algo="sha256",serialize=FALSE)
identityfile<-file.path(out,"reviewer.json")
eventfile<-file.path(out,"decisions.jsonl")
abstractfile<-file.path(out,"abstract_corrections.jsonl")
who<-if(file.exists(identityfile))jsonlite::fromJSON(identityfile)$reviewer else ""
events<-if(file.exists(eventfile))readjl(eventfile) else list()
abstract_events<-if(file.exists(abstractfile))readjl(abstractfile) else list()
last_events<-function(e) {
  ans<-list()
  for(x in e)ans[[x$review_case_id]]<-x
  ans
}
val<-function(x)if(is.null(x)||!length(x))"" else paste(unlist(x,use.names=FALSE),collapse="; ")
utc<-function()format(Sys.time(),"%Y-%m-%dT%H:%M:%SZ",tz="UTC")
ui<-fluidPage(
  tags$head(tags$style(HTML("body{max-width:1220px;margin:auto}.box{border:1px solid #ddd;border-radius:8px;padding:18px;overflow-wrap:anywhere}pre{white-space:pre-wrap;font-family:inherit}"))),
  titlePanel("W04 · Collective action screening"),
  textOutput("reviewer"),h4(textOutput("progress")),
  conditionalPanel("output.ready && !output.complete",
    p(class="text-muted","Read the abstract and model assessment, choose a decision, and save. Use Previous/Next to review earlier decisions. Exclusions do not require a reason."),
    fluidRow(column(6,actionButton("prev","Previous")),column(6,actionButton("next","Next"))),
    fluidRow(
      column(8,uiOutput("bibliography"),uiOutput("model")),
      column(4,div(class="box",
        h4("Human screening decision"),
        radioButtons("decision",NULL,
          choices=c("Retain"="retain","Exclude"="exclude","Uncertain"="uncertain"),
          selected=character(0)),
        textAreaInput("reason","Reason (optional for exclusion)",rows=5,width="100%"),
        actionButton("save","Save decision",class="btn-primary"),
        tags$hr(),downloadButton("export","Export adjudication decisions"),
        tags$p(class="text-muted","Human decisions are saved separately from model results.")
      ))
    )
  ),
  conditionalPanel("output.ready && output.complete",
    div(class="box",h3("Adjudications complete"),
      p("Every record in this review queue has a saved human decision."),
      p("You can review or revise any decision. Changes are saved as new review events without overwriting earlier entries."),
      actionButton("review_again","Back to review decisions",class="btn-primary"),
      downloadButton("export_complete","Export decisions")))
)
server<-function(input,output,session){
  reviewer<-reactiveVal(who)
  position<-reactiveVal(1L)
  saved<-reactiveVal(events)
  reviewing<-reactiveVal(FALSE)
  complete<-reactive({all(ids %in% names(last_events(saved())))})
  output$complete<-reactive(complete() && !reviewing())
  outputOptions(output,"complete",suspendWhenHidden=FALSE)
  observeEvent(input$review_again,{
    position(1L)
    reviewing(TRUE)
  })
  abstract_saved<-reactiveVal(abstract_events)
  corrected_abstract<-reactive({
    ev<-last_events(abstract_saved())[[current()$review_case_id]]
    if(is.null(ev))val(current()$bibliographic$abstract) else val(ev$abstract)
  })
  output$ready<-reactive(nzchar(reviewer()))
  outputOptions(output,"ready",suspendWhenHidden=FALSE)
  observeEvent(TRUE,{
    if(!nzchar(reviewer()))showModal(modalDialog(
      title="Reviewer identification",textInput("reviewer_name","Your name"),
      easyClose=FALSE,footer=actionButton("identify","Continue")))
  },once=TRUE)
  observeEvent(input$identify,{
    name<-trimws(val(input$reviewer_name))
    if(!nzchar(name))return(showNotification("Reviewer name required",type="error"))
    jsonlite::write_json(list(reviewer=name,created_at_utc=utc()),identityfile,
                         auto_unbox=TRUE,pretty=TRUE)
    reviewer(name);removeModal()
  })
  current<-reactive(cases[[position()]])
  output$reviewer<-renderText(paste("Reviewer:",reviewer()))
  output$progress<-renderText(paste(position(),"of",length(cases),"cases ·",
                                      length(last_events(saved())),"decisions saved"))
  observeEvent(input$prev,position(max(1L,position()-1L)))
  observeEvent(input[['next']],position(min(length(cases),position()+1L)))
  observeEvent(list(position(),saved()),{
    x<-last_events(saved())[[current()$review_case_id]]
    updateRadioButtons(session,"decision",selected=if(is.null(x))character() else x$decision)
    updateTextAreaInput(session,"reason",value=if(is.null(x))"" else x$reason)
  })
  output$bibliography<-renderUI({
    x<-current()$bibliographic
    d<-val(x$doi)
    title<-val(x$title)
    div(class="box",h4(title),
      p(strong("Authors: "),val(x$authors)),p(strong("Year: "),val(x$year)),
      p(strong("Source: "),val(x$journal)),p(strong("Keywords: "),val(x$keywords)),
      if(grepl("^10\\.[0-9]{4,9}/",d))
        tags$a(href=paste0("https://doi.org/",URLencode(d,reserved=FALSE)),
               target="_blank","DOI"),
      tags$span("  "),
      tags$a(href=paste0("https://scholar.google.com/scholar?q=",
                        URLencode(title,reserved=TRUE)),target="_blank","Google Scholar"),
      h5("Abstract"),tags$pre(corrected_abstract()),
      actionButton("edit_abstract","Edit abstract"))
  })
  observeEvent(input$edit_abstract,{
    showModal(modalDialog(title="Edit abstract",size="l",easyClose=FALSE,
      textAreaInput("edited_abstract","Abstract",value=corrected_abstract(),
                    rows=14,width="100%"),
      footer=tagList(modalButton("Cancel"),actionButton("save_abstract","Save abstract"))))
  })
  observeEvent(input$save_abstract,{
    x<-current()
    e<-list(schema="local-w04-abstract-correction-v1",
      review_case_id=x$review_case_id,record_id=x$record_id,
      queue_sha256=queuehash,reviewer=reviewer(),
      original_abstract_sha256=digest::digest(val(x$bibliographic$abstract),
                                      algo="sha256",serialize=FALSE),
      abstract=val(input$edited_abstract),created_at_utc=utc())
    cat(jsonlite::toJSON(e,auto_unbox=TRUE,null="null"),"\n",
        file=abstractfile,append=TRUE,sep="")
    abstract_saved(c(abstract_saved(),list(e)))
    removeModal()
    showNotification("Abstract correction saved separately from source record.",type="message")
  })
  output$model<-renderUI({
    x<-current()
    div(class="box",h4("Model consensus"),
      p(strong("Decision: "),val(x$model$decision)),
      p(strong("Agreement: "),val(x$model$agreement)),
      p(strong("Model: "),val(x$model$model)),
      h5("Model reasons"),tags$pre(val(x$model_reasons)),
      p(strong("Case ID: "),x$review_case_id),
      p(strong("Prompt SHA-256: "),val(x$model$prompt_sha256)))
  })
  observeEvent(input$save,{
    x<-current()
    if(!input$decision %in% c("retain","exclude","uncertain"))
      return(showNotification("Select a decision",type="error"))
    reason<-trimws(val(input$reason))
    if(!nzchar(reason) && !identical(input$decision,"exclude"))
      return(showNotification("Provide a reason",type="error"))
    e<-list(schema="local-w04-review-decision-v1",
      review_case_id=x$review_case_id,record_id=x$record_id,
      queue_sha256=queuehash,reviewer=reviewer(),decision=input$decision,
      reason=reason,record_input_sha256=x$model$screening_input_sha256,
      model_prompt_sha256=x$model$prompt_sha256,created_at_utc=utc())
    cat(jsonlite::toJSON(e,auto_unbox=TRUE,null="null"),"\n",
        file=eventfile,append=TRUE,sep="")
    saved(c(saved(),list(e)))
    if(complete()) reviewing(FALSE)
    showNotification("Decision saved",type="message")
  })
  export_decisions<-function(file) {
      e<-last_events(saved())
      writeLines(vapply(e,function(z)jsonlite::toJSON(z,auto_unbox=TRUE),
                        character(1)),file,useBytes=TRUE)
  }
  output$export<-downloadHandler(filename=function()"w04_human_decisions.jsonl",content=export_decisions)
  output$export_complete<-downloadHandler(filename=function()"w04_human_decisions.jsonl",content=export_decisions)
}
shiny::runApp(shinyApp(ui,server),launch.browser=TRUE)
