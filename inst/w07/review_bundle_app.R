# Self-contained Shiny app for W07 publication-level mechanism validation.
suppressPackageStartupMessages({
 library(shiny)
 library(jsonlite)
 library(digest)
})
root<-getwd()
manifest_file<-file.path(root,"manifest.json")
queue_file<-file.path(root,"data","review_queue.jsonl")
ontology_file<-file.path(root,"data","ontology.csv")
if(!all(file.exists(c(manifest_file,queue_file,ontology_file))))
 stop("Start from the extracted w07_review_bundle directory.")
meta<-jsonlite::fromJSON(manifest_file,simplifyVector=FALSE)
sha<-function(path)digest::digest(file=path,algo="sha256",serialize=FALSE)
if(!identical(meta$schema,"ostara-w07-review-bundle-v1")||
   !identical(sha(queue_file),meta$queue_sha256)||
   !identical(sha(ontology_file),meta$ontology_sha256))
 stop("The review queue or ontology has changed. Use the original ZIP.")
readjl<-function(path) {
 if(!file.exists(path))return(list())
 lines<-readLines(path,warn=FALSE,encoding="UTF-8")
 lapply(lines[nzchar(trimws(lines))],jsonlite::fromJSON,simplifyVector=FALSE)
}
cases<-readjl(queue_file)
ids<-vapply(cases,function(z)as.character(z$record_id),character(1))
if(!length(ids)||anyDuplicated(ids)||length(ids)!=as.integer(meta$records))
 stop("Invalid W07 review queue.")
ontology<-utils::read.csv(ontology_file,stringsAsFactors=FALSE,
                         fileEncoding="UTF-8-BOM",check.names=FALSE)
if(!"mechanism_id"%in%names(ontology))stop("Ontology mechanism_id missing")
codes<-as.character(ontology$mechanism_id)
if(anyDuplicated(codes)||any(!nzchar(codes)))stop("Invalid ontology codes.")
code_row<-setNames(seq_along(codes),codes)
events_path<-file.path(root,"decisions.jsonl")
events<-readjl(events_path)
if(length(events)&&any(!vapply(events,function(x)
 identical(x$queue_sha256,meta$queue_sha256)&&x$record_id%in%ids,logical(1))))
 stop("Existing review events are for a different queue.")
scalar<-function(x)if(is.null(x)||!length(x))"" else
 paste(as.character(unlist(x,use.names=FALSE)),collapse="; ")
latest<-function(entries) {
 out<-list()
 for(e in entries)out[[as.character(e$record_id)]]<-e
 out
}
utc<-function()format(Sys.time(),"%Y-%m-%dT%H:%M:%SZ",tz="UTC")
model_status<-vapply(cases,function(x)scalar(x$coding$coding_status),character(1))
mechanism_labels<-function(x) {
 m<-x$coding$mechanisms
 if(!length(m))return("(none)")
 paste(vapply(m,function(z)scalar(z$mechanism_id),character(1)),collapse=", ")
}
ui<-fluidPage(
 tags$head(tags$meta(charset="utf-8"),tags$style(HTML("
  body{max-width:1450px;margin:auto;padding-bottom:45px}
  .review-box{padding:18px;border:1px solid #d4d4d4;border-radius:9px;margin-bottom:14px}
  .abstract{white-space:pre-wrap;line-height:1.58;overflow-wrap:break-word}
  .muted{color:#6a6a6a}
  .mech{border-left:3px solid #6788a3;padding:10px 15px;margin-bottom:10px;background:#f7f9fb}
  pre{white-space:pre-wrap;overflow-wrap:break-word}
 "))),
 titlePanel("Ostara | Mechanism coding review"),
 p("Review publication-level mechanism assignments from the W07 model. Accept or reject the coding, and record alternatives or comments.",class="muted"),
 fluidRow(
  column(4,textOutput("run")),
  column(4,strong(textOutput("progress"))),
  column(4,textOutput("who"))
 ),
 fluidRow(
  column(4,selectInput("status","Model coding status",
    c("All"="all","Coded"="coded",
      "Insufficient information"="insufficient_information",
      "No supported mechanism"="no_supported_mechanism"))),
  column(4,selectInput("review_status","Client review",
    c("All"="all","Not yet reviewed"="pending",
      "Reviewed"="reviewed","Rejected"="reject"))),
  column(4,textInput("search","Search titles, IDs or mechanism codes",""))
 ),
 selectInput("record","Publication",choices=NULL,width="100%"),
 fluidRow(
  column(6,actionButton("prev","Previous")),
  column(6,actionButton("next_record","Next"))
 ),
 tags$hr(),
 fluidRow(
  column(8,
   uiOutput("publication"),
   uiOutput("mechanisms"),
   uiOutput("ontology_gap")),
  column(4,div(class="review-box",
   h4("Your assessment"),
   radioButtons("decision","Do you accept the model's mechanism coding?",
     choices=c("Accept"="accept","Reject"="reject"),
     selected=character(0)),
   textAreaInput("notes","Comments or reasons (optional)",
     rows=6,width="100%"),
   textAreaInput("alternatives","Different or additional mechanisms (IDs or descriptions)",
     rows=5,width="100%"),
   actionButton("save","Save assessment",class="btn-primary"),
   tags$hr(),
   downloadButton("csv","Export complete review CSV"),
   tags$br(),tags$br(),
   downloadButton("jsonl","Export complete audit JSONL"),
   p("Saved reviews remain in decisions.jsonl in this extracted folder. You can revisit and revise them.",class="muted")
  ))
 ),
 tags$hr(),
 p("Model coding and ontology remain unchanged; client decisions are stored independently.",class="muted")
)
server<-function(input,output,session) {
 reviewer<-reactiveVal("")
 saved<-reactiveVal(events)
 reviewed<-reactive(latest(saved()))
 current_choices<-reactive({
  possible<-seq_along(ids)
  if(!is.null(input$status)&&input$status!="all")
    possible<-possible[model_status[possible]==input$status]
  if(!is.null(input$review_status)&&input$review_status!="all") {
    done<-ids[possible]%in%names(reviewed())
    if(input$review_status=="pending")possible<-possible[!done]
    else if(input$review_status=="reviewed")possible<-possible[done]
    else possible<-possible[vapply(ids[possible],function(id)
       scalar(reviewed()[[id]]$decision)=="reject",logical(1))]
  }
  query<-trimws(if(is.null(input$search))"" else input$search)
  if(nzchar(query))
    possible<-possible[vapply(possible,function(i)
      grepl(query,paste(ids[[i]],scalar(cases[[i]]$bibliographic$title),
                       mechanism_labels(cases[[i]])),
            fixed=TRUE,ignore.case=TRUE),logical(1))]
  ids[possible]
 })
 observeEvent(current_choices(),{
  choices<-current_choices()
  chosen<-if(!is.null(input$record)&&input$record%in%choices)
    input$record else if(length(choices))choices[[1]] else character()
  labels<-vapply(choices,function(id){
    i<-match(id,ids)
    paste0(id," | ",substr(scalar(cases[[i]]$bibliographic$title),1,85))
  },character(1))
  names(choices)<-labels
  updateSelectInput(session,"record",choices=choices,selected=chosen)
 },ignoreInit=FALSE)
 current_id<-reactive({
  choices<-current_choices()
  if(!length(choices))return(NULL)
  if(!is.null(input$record)&&input$record%in%choices)
    input$record else choices[[1]]
 })
 current<-reactive({
  id<-current_id()
  if(is.null(id))return(NULL)
  cases[[match(id,ids)]]
 })
 move<-function(delta){
  choices<-current_choices()
  if(!length(choices)||is.null(current_id()))return()
  pos<-match(current_id(),choices)
  updateSelectInput(session,"record",
     selected=choices[[max(1L,min(length(choices),pos+delta))]])
 }
 observeEvent(input$prev,move(-1L))
 observeEvent(input$next_record,move(1L))
 observeEvent(TRUE,{
  showModal(modalDialog(title="Reviewer identification",
   textInput("reviewer_name","Your name"),
   easyClose=FALSE,
   footer=actionButton("identify","Start review")))
 },once=TRUE)
 observeEvent(input$identify,{
  name<-trimws(scalar(input$reviewer_name))
  if(!nzchar(name))
    return(showNotification("Please enter your name",type="error"))
  reviewer(name)
  removeModal()
 })
 output$who<-renderText(paste("Reviewer:",reviewer()))
 output$run<-renderText(paste("W07:",scalar(meta$source_w07),
                            "|",length(ids),"publications"))
 output$progress<-renderText(paste(length(intersect(ids,names(reviewed()))),
                                   "of",length(ids),"reviewed",
                                   "| Filtered:",length(current_choices())))
 observeEvent(list(current_id(),saved()),{
  id<-current_id()
  if(is.null(id))return()
  entry<-reviewed()[[id]]
  updateRadioButtons(session,"decision",
      selected=if(is.null(entry))character(0) else scalar(entry$decision))
  updateTextAreaInput(session,"notes",
      value=if(is.null(entry))"" else scalar(entry$notes))
  updateTextAreaInput(session,"alternatives",
      value=if(is.null(entry))"" else scalar(entry$alternative_mechanisms))
 })
 output$publication<-renderUI({
  x<-current()
  if(is.null(x))return(div(class="review-box","No publications match the filters."))
  b<-x$bibliographic
  doi<-scalar(b$doi)
  div(class="review-box",
   h3(scalar(b$title)),
   p(strong("Record ID: "),scalar(x$record_id)),
   p(strong("Authors: "),scalar(b$authors)),
   p(strong("Year: "),scalar(b$year)),
   p(strong("Source: "),scalar(b$source)),
   p(strong("Keywords: "),scalar(b$keywords)),
   if(grepl("^10\\.[0-9]{4,9}/",doi))
     tags$a(href=paste0("https://doi.org/",URLencode(doi,reserved=FALSE)),
            target="_blank","Open DOI"),
   h4("Abstract"),
   div(class="abstract",if(nzchar(scalar(b$abstract)))
                        scalar(b$abstract) else "Abstract unavailable"))
 })
 output$mechanisms<-renderUI({
  x<-current()
  if(is.null(x))return(NULL)
  m<-x$coding$mechanisms
  div(class="review-box",
   h4("Model-assigned mechanisms"),
   p(strong("Coding status: "),scalar(x$coding$coding_status)),
   if(!length(m))p("No mechanism IDs assigned."),
   lapply(m,function(z){
    id<-scalar(z$mechanism_id)
    ont<-if(id%in%names(code_row))
      ontology[code_row[[id]],,drop=FALSE] else NULL
    detail<-if(is.null(ont))list() else as.list(ont[1,,drop=TRUE])
    div(class="mech",
      h4(paste(id, if(!is.null(detail$fine_level_mechanism))
                    scalar(detail$fine_level_mechanism) else
                    scalar(detail$mechanism_source_label))),
      if(!is.null(detail$definition))
        p(strong("Ontology definition: "),scalar(detail$definition))
      else if(!is.null(detail$definition_draft))
        p(strong("Ontology definition: "),scalar(detail$definition_draft)),
      if(!is.null(detail$collective_action_type))
        p(strong("Collective-action type: "),scalar(detail$collective_action_type)),
      if(!is.null(detail$high_level_mechanism))
        p(strong("High-level mechanism: "),scalar(detail$high_level_mechanism)),
      p(strong("Model evidence: "),scalar(z$evidence)),
      if(nzchar(scalar(z$evidence_excerpt)))
         p(strong("Text excerpt: "),scalar(z$evidence_excerpt)),
      if(nzchar(scalar(z$implementation_status)))
        p(strong("Implementation: "),scalar(z$implementation_status)),
      if(nzchar(scalar(z$confidence)))
        p(strong("Confidence: "),scalar(z$confidence))
    )
   })
  )
 })
 output$ontology_gap<-renderUI({
  x<-current()
  if(is.null(x))return(NULL)
  z<-x$coding
  div(class="review-box",
   h4("Other coding information"),
   p(strong("Ontology gap: "),if(isTRUE(z$ontology_gap))"Yes" else "No"),
   if(isTRUE(z$ontology_gap))
     tagList(
       p(strong("Candidate: "),scalar(z$proposed_new_mechanism)),
       p(strong("Reason: "),scalar(z$ontology_gap_reason)),
       p(strong("Nearest code: "),scalar(z$closest_existing_mechanism_id))),
   if(nzchar(scalar(z$coding_notes)))
      p(strong("Coding notes: "),scalar(z$coding_notes)),
   if(nzchar(scalar(z$novel_terminology)))
      p(strong("New terminology: "),scalar(z$novel_terminology))
  )
 })
 observeEvent(input$save,{
  id<-current_id()
  if(is.null(id)||!nzchar(reviewer()))
    return(showNotification("Select a reviewer and record",type="error"))
  if(is.null(input$decision)||!input$decision%in%c("accept","reject"))
    return(showNotification("Choose Accept or Reject",type="error"))
  entry<-list(schema="ostara-w07-review-decision-v1",
    record_id=id,queue_sha256=meta$queue_sha256,
    reviewer=reviewer(),decision=input$decision,
    notes=trimws(scalar(input$notes)),
    alternative_mechanisms=trimws(scalar(input$alternatives)),
    created_at_utc=utc())
  cat(as.character(jsonlite::toJSON(entry,auto_unbox=TRUE,null="null")),
      "\n",file=events_path,append=TRUE,sep="")
  saved(c(saved(),list(entry)))
  showNotification("Assessment saved",type="message")
 })
 complete_data<-reactive({
  d<-reviewed()
  data.frame(
    record_id=ids,
    title=vapply(cases,function(x)scalar(x$bibliographic$title),character(1)),
    model_coding_status=model_status,
    model_mechanism_ids=vapply(cases,mechanism_labels,character(1)),
    model_ontology_gap=vapply(cases,function(x)
      isTRUE(x$coding$ontology_gap),logical(1)),
    reviewer=vapply(ids,function(id)scalar(d[[id]]$reviewer),character(1)),
    review_decision=vapply(ids,function(id)scalar(d[[id]]$decision),character(1)),
    notes=vapply(ids,function(id)scalar(d[[id]]$notes),character(1)),
    alternative_mechanisms=vapply(ids,function(id)
      scalar(d[[id]]$alternative_mechanisms),character(1)),
    reviewed_at_utc=vapply(ids,function(id)
      scalar(d[[id]]$created_at_utc),character(1)),
    ontology_sha256=rep(scalar(meta$ontology_sha256),length(ids)),
    prompt_sha256=rep(scalar(meta$prompt_sha256),length(ids)),
    queue_sha256=rep(scalar(meta$queue_sha256),length(ids)),
    stringsAsFactors=FALSE)
 })
 output$csv<-downloadHandler(
  filename=function()paste0("ostara_w07_client_review_",Sys.Date(),".csv"),
  content=function(file)utils::write.csv(complete_data(),file,row.names=FALSE,
                                          na="",fileEncoding="UTF-8"))
 output$jsonl<-downloadHandler(
  filename=function()"ostara_w07_client_review_history.jsonl",
  content=function(file){
    if(file.exists(events_path))file.copy(events_path,file)
    else writeLines(character(),file)
  })
}
shiny::shinyApp(ui,server)
