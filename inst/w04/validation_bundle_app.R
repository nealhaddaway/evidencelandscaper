# Portable Ostara W04 screening validation app.
# Works only with the immutable bundled queue; writes reviewer files locally.
suppressPackageStartupMessages({
  library(shiny)
  library(jsonlite)
  library(digest)
})
base<-getwd()
queue_file<-file.path(base,"data","review_queue.jsonl")
manifest_file<-file.path(base,"manifest.json")
if(!file.exists(queue_file)||!file.exists(manifest_file))
  stop("Run this app from the unzipped w04_review_bundle folder")
manifest<-jsonlite::fromJSON(manifest_file,simplifyVector=FALSE)
sha<-function(path)digest::digest(file=path,algo="sha256",serialize=FALSE)
if(!identical(manifest$schema,"ostara-w04-colleague-review-v1")||
   !identical(sha(queue_file),manifest$review_queue_sha256))
  stop("Review bundle integrity check failed: queue changed")
readjl<-function(path) {
  if(!file.exists(path))return(list())
  raw<-readLines(path,warn=FALSE,encoding="UTF-8")
  lapply(raw[nzchar(trimws(raw))],
         jsonlite::fromJSON,simplifyVector=FALSE)
}
cases<-readjl(queue_file)
ids<-vapply(cases,function(x)as.character(x$record_id),character(1))
if(!length(ids)||anyDuplicated(ids)||
   length(ids)!=as.integer(manifest$records))
  stop("Review bundle has an invalid record population")
decisions_file<-file.path(base,"decisions.jsonl")
events<-readjl(decisions_file)
if(length(events)&&any(!vapply(events,function(z)
   identical(z$queue_sha256,manifest$review_queue_sha256)&&
   z$record_id%in%ids,logical(1))))
  stop("Previous decisions are not valid for this review queue")
scalar<-function(x) {
  if(is.null(x)||!length(x))return("")
  paste(as.character(unlist(x,use.names=FALSE)),collapse="; ")
}
latest<-function(rows) {
  out<-list()
  for(x in rows)out[[as.character(x$record_id)]]<-x
  out
}
utc<-function()format(Sys.time(),"%Y-%m-%dT%H:%M:%SZ",tz="UTC")
model_decisions<-vapply(cases,function(x)scalar(x$model$decision),
                        character(1))
if(any(!model_decisions%in%c("retain","exclude","uncertain")))
 stop("Unknown model screening category")
ui<-fluidPage(
 tags$head(tags$meta(charset="utf-8"),
   tags$style(HTML("
     body {max-width:1420px; margin:auto; padding-bottom:40px}
     .panel-review {padding:17px; border:1px solid #ddd;
                    border-radius:9px; margin-bottom:14px}
     .abstract {white-space:pre-wrap; overflow-wrap:break-word; line-height:1.55}
     .small-note {color:#666}
     .decision-text {font-weight:600}
     .model-retain {color:#157350}
     .model-exclude {color:#a04f1b}
     .model-uncertain {color:#735eb2}
   "))),
 titlePanel("Ostara | Independent abstract screening validation"),
 p("Complete model screening population. Review retained, excluded and uncertain records.",
   class="small-note"),
 fluidRow(
   column(4, textOutput("run_info")),
   column(4, strong(textOutput("progress"))),
   column(4, textOutput("identity"))
 ),
 fluidRow(
   column(4,selectInput("model_filter","Model decision",
     choices=c("All"="all","Retained"="retain","Excluded"="exclude",
               "Uncertain"="uncertain"))),
   column(4,selectInput("status_filter","Human review status",
     choices=c("All"="all","Not reviewed"="pending","Reviewed"="reviewed"))),
   column(4,textInput("search","Search title or record ID",""))
 ),
 selectInput("record","Select record",choices=NULL,width="100%"),
 fluidRow(
   column(6,actionButton("previous","Previous in filtered list")),
   column(6,actionButton("next_record","Next in filtered list"))
 ),
 tags$hr(),
 fluidRow(
   column(8,
     uiOutput("bibliography"),
     uiOutput("model_info")),
   column(4,div(class="panel-review",
     h4("Independent human decision"),
     radioButtons("decision","Your assessment",
       choices=c("Retain"="retain","Exclude"="exclude",
                 "Uncertain"="uncertain"),selected=character(0)),
     textAreaInput("notes","Reason or evidence (recommended)",
                   rows=6,width="100%"),
     actionButton("save","Save reviewed decision",class="btn-primary"),
     tags$hr(),
     h5("Export to project lead"),
     downloadButton("export_csv","Download complete review CSV"),
     tags$br(),tags$br(),
     downloadButton("export_events","Download audit history JSONL"),
     p("Unsaved records remain blank. All revisions are logged.",
       class="small-note")
   ))
 ),
 tags$hr(),
 p("This validation does NOT modify the W04 consensus or automatically approve",
   "any publication. Use separate copies for separate reviewers.",
   class="small-note")
)
server<-function(input,output,session) {
 reviewer<-reactiveVal("")
 saved<-reactiveVal(events)
 current_reviews<-reactive(latest(saved()))
 filtered<-reactive({
   idx<-seq_along(ids)
   if(!is.null(input$model_filter)&&input$model_filter!="all")
     idx<-idx[model_decisions[idx]==input$model_filter]
   if(!is.null(input$status_filter)&&input$status_filter!="all") {
     present<-ids[idx]%in%names(current_reviews())
     idx<-idx[if(input$status_filter=="reviewed")present else !present]
   }
   q<-trimws(if(is.null(input$search))"" else input$search)
   if(nzchar(q))
     idx<-idx[vapply(idx,function(i)
       grepl(q,paste(ids[[i]],scalar(cases[[i]]$bibliographic$title)),
             collapse=" "),fixed=TRUE,ignore.case=TRUE),logical(1))]
   ids[idx]
 })
 observeEvent(filtered(),{
   choices<-filtered()
   selected<-if(!is.null(input$record)&&input$record%in%choices)
     input$record else if(length(choices))choices[[1L]] else character(0)
   labels<-vapply(choices,function(id){
     i<-match(id,ids)
     title<-scalar(cases[[i]]$bibliographic$title)
     paste0(id," | ",substr(title,1,95))
   },character(1))
   names(choices)<-labels
   updateSelectInput(session,"record",choices=choices,selected=selected)
 },ignoreInit=FALSE)
 current_id<-reactive({
   choices<-filtered()
   if(!length(choices))return(NULL)
   if(!is.null(input$record)&&input$record%in%choices)
     input$record else choices[[1L]]
 })
 current<-reactive({
   id<-current_id()
   if(is.null(id))return(NULL)
   cases[[match(id,ids)]]
 })
 move<-function(offset) {
   visible<-filtered()
   id<-current_id()
   if(is.null(id)||!length(visible))return()
   i<-match(id,visible)
   n<-max(1L,min(length(visible),i+offset))
   updateSelectInput(session,"record",selected=visible[[n]])
 }
 observeEvent(input$previous,move(-1L))
 observeEvent(input$next_record,move(1L))
 observeEvent(TRUE,{
   showModal(modalDialog(
     title="Independent reviewer",
     textInput("reviewer_name","Name (recorded with each decision)"),
     easyClose=FALSE,
     footer=actionButton("set_reviewer","Start reviewing")))
 },once=TRUE)
 observeEvent(input$set_reviewer,{
   name<-trimws(scalar(input$reviewer_name))
   if(!nzchar(name))
     return(showNotification("Reviewer name is required",type="error"))
   reviewer(name)
   removeModal()
 })
 output$run_info<-renderText(
   paste("Records:",length(ids)," | Prompt:",substr(scalar(manifest$prompt_sha256),1,12)))
 output$progress<-renderText({
   counts<-table(model_decisions)
   paste(sum(ids%in%names(current_reviews())),"of",length(ids),
         "reviewed | Visible:",length(filtered()),
         "| Model:",paste(paste(names(counts),counts,sep="="),collapse=", "))
 })
 output$identity<-renderText(paste("Reviewer:",reviewer()))
 observeEvent(list(current_id(),saved()),{
   x<-current_reviews()[[current_id()]]
   updateRadioButtons(session,"decision",
                      selected=if(is.null(x))character(0) else scalar(x$decision))
   updateTextAreaInput(session,"notes",
                       value=if(is.null(x))"" else scalar(x$reason))
 })
 output$bibliography<-renderUI({
   x<-current()
   if(is.null(x))return(div(class="panel-review","No records match the filters."))
   b<-x$bibliographic
   doi<-scalar(b$doi)
   div(class="panel-review",
       h3(scalar(b$title)),
       p(strong("Record ID: "),scalar(x$record_id)),
       p(strong("Authors: "),scalar(b$authors)),
       p(strong("Year: "),scalar(b$year)),
       p(strong("Journal/source: "),scalar(b$journal)),
       p(strong("Keywords: "),scalar(b$keywords)),
       if(grepl("^10\\.[0-9]{4,9}/",doi))
         tags$a(href=paste0("https://doi.org/",URLencode(doi,reserved=FALSE)),
                target="_blank","Open DOI"),
       h4("Abstract"),
       div(class="abstract",if(nzchar(scalar(b$abstract)))
            scalar(b$abstract) else
            "ABSTRACT MISSING: assess cautiously; record uncertainty if warranted."))
 })
 output$model_info<-renderUI({
   x<-current()
   if(is.null(x))return(NULL)
   dec<-scalar(x$model$decision)
   div(class="panel-review",
       h4("Original model assessment (not a human decision)"),
       p(class=paste0("decision-text model-",dec),
         paste("Consensus:",toupper(dec))),
       p(strong("Agreement: "),scalar(x$model$agreement)),
       p(strong("Model: "),scalar(x$model$model)),
       h5("Reasons from model passes"),
       lapply(x$model_reasons,function(z)
         p(strong(paste("Pass",scalar(z$pass),":")),
           paste(scalar(z$decision),"-",scalar(z$reason)))))
 })
 observeEvent(input$save,{
   id<-current_id()
   if(is.null(id)||!nzchar(reviewer()))
     return(showNotification("Select a record and reviewer",type="error"))
   if(is.null(input$decision)||!input$decision%in%
      c("retain","exclude","uncertain"))
     return(showNotification("Choose a human decision",type="error"))
   note<-trimws(scalar(input$notes))
   entry<-list(schema="ostara-w04-validation-decision-v1",
     record_id=id,queue_sha256=manifest$review_queue_sha256,
     reviewer=reviewer(),decision=input$decision,
     reason=note,created_at_utc=utc())
   cat(as.character(jsonlite::toJSON(entry,auto_unbox=TRUE,null="null")),
       "\n",file=decisions_file,append=TRUE,sep="")
   saved(c(saved(),list(entry)))
   showNotification(paste("Decision saved:",id),type="message")
 })
 tabular<-reactive({
   d<-current_reviews()
   data.frame(
     record_id=ids,
     title=vapply(cases,function(x)scalar(x$bibliographic$title),character(1)),
     model_decision=model_decisions,
     model_agreement=vapply(cases,function(x)scalar(x$model$agreement),character(1)),
     reviewer=vapply(ids,function(id)scalar(d[[id]]$reviewer),character(1)),
     human_decision=vapply(ids,function(id)scalar(d[[id]]$decision),character(1)),
     human_reason=vapply(ids,function(id)scalar(d[[id]]$reason),character(1)),
     review_timestamp_utc=vapply(ids,function(id)
       scalar(d[[id]]$created_at_utc),character(1)),
     prompt_sha256=rep(scalar(manifest$prompt_sha256),length(ids)),
     queue_sha256=rep(scalar(manifest$review_queue_sha256),length(ids)),
     stringsAsFactors=FALSE)
 })
 output$export_csv<-downloadHandler(
  filename=function()paste0("ostara_w04_validation_",format(Sys.Date(),"%Y%m%d"),".csv"),
  content=function(file)utils::write.csv(tabular(),file,row.names=FALSE,na=""))
 output$export_events<-downloadHandler(
  filename=function()"ostara_w04_validation_audit.jsonl",
  content=function(file){
    if(file.exists(decisions_file))file.copy(decisions_file,file)
    else writeLines(character(),file)
  })
}
shiny::shinyApp(ui,server)
