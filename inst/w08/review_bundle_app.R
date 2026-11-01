# Portable W08 review app. Works from the extracted bundle; no live map required.
suppressPackageStartupMessages({
  library(shiny)
  library(jsonlite)
  library(digest)
})
root<-getwd()
manifest_file<-file.path(root,"manifest.json")
queue_file<-file.path(root,"data","review_queue.jsonl")
prompt_file<-file.path(root,"data","prompt.txt")
if(!all(file.exists(c(manifest_file,queue_file,prompt_file))))
  stop("Run the app from the extracted w08_review_bundle folder.")
sha<-function(x)digest::digest(file=x,algo="sha256",serialize=FALSE)
meta<-jsonlite::fromJSON(manifest_file,simplifyVector=FALSE)
if(!identical(meta$schema,"ostara-w08-colleague-review-v1")||
   !identical(sha(queue_file),meta$queue_sha256)||
   !identical(sha(prompt_file),meta$prompt_sha256))
  stop("The archived review queue or prompt checksum does not match the manifest.")
readjl<-function(p) {
  if(!file.exists(p))return(list())
  raw<-readLines(p,warn=FALSE,encoding="UTF-8")
  lapply(raw[nzchar(trimws(raw))],jsonlite::fromJSON,simplifyVector=FALSE)
}
cases<-readjl(queue_file)
ids<-vapply(cases,function(z)as.character(z$record_id),character(1))
if(!length(ids)||anyDuplicated(ids)||length(ids)!=as.integer(meta$records))
  stop("Review bundle population is invalid.")
sc<-function(x)if(is.null(x)||!length(x))"" else
  paste(as.character(unlist(x,use.names=FALSE)),collapse="; ")
latest<-function(events) {
  result<-list()
  for(x in events)result[[as.character(x$record_id)]]<-x
  result
}
events_file<-file.path(root,"decisions.jsonl")
events<-readjl(events_file)
if(length(events)&&any(!vapply(events,function(x)
  identical(x$queue_sha256,meta$queue_sha256)&&x$record_id%in%ids,logical(1))))
  stop("Saved decisions belong to another review queue.")
utc<-function()format(Sys.time(),"%Y-%m-%dT%H:%M:%SZ",tz="UTC")
statuses<-vapply(cases,function(x)sc(x$detection$case_evidence_status),character(1))
n_candidates<-vapply(cases,function(x)length(x$detection$cases),integer(1))
ui<-fluidPage(
  tags$head(tags$meta(charset="utf-8"),
    tags$style(HTML("
     body {max-width:1450px;margin:auto;padding-bottom:45px}
     .review-box {border:1px solid #d3d8dd;border-radius:9px;padding:17px;margin-bottom:15px}
     .abstract {white-space:pre-wrap;line-height:1.55;overflow-wrap:break-word}
     .muted {color:#606a73}
     .case-block {border-left:3px solid #738ea6;padding:10px 14px;background:#f5f8fa;margin-bottom:12px}
    "))),
  titlePanel("Ostara | Case detection validation"),
  p("Independent review of publication-level case identification and individual case candidates.",
    class="muted"),
  fluidRow(column(4,textOutput("run_info")),
           column(4,strong(textOutput("progress"))),
           column(4,textOutput("identity"))),
  fluidRow(
    column(4,selectInput("model_filter","Model classification",
      choices=c("All"="all","One case"="specific_case",
        "Multiple cases"="multiple_cases",
        "No identifiable case"="no_identifiable_case","Unclear"="unclear"))),
    column(4,selectInput("review_filter","Review status",
      choices=c("All"="all","Unreviewed"="pending",
        "Reviewed"="reviewed","Rejected"="reject"))),
    column(4,textInput("search","Search publication title, ID or case name"))
  ),
  selectInput("record","Publication",choices=NULL,width="100%"),
  fluidRow(column(6,actionButton("prev","Previous")),
           column(6,actionButton("next_record","Next"))),
  tags$hr(),
  fluidRow(
    column(8,
      uiOutput("publication"),
      uiOutput("case_blocks"),
      uiOutput("assessment")),
    column(4,div(class="review-box",
      h4("Publication-level review"),
      radioButtons("decision","Accept the model's case-detection assessment?",
        choices=c("Accept"="accept","Reject"="reject"),
        selected=character(0)),
      textAreaInput("alternative","Missing, different or incorrectly split cases",
        rows=5,width="100%"),
      textAreaInput("notes","Comments and reasons",
        rows=5,width="100%"),
      actionButton("save","Save review",class="btn-primary"),
      tags$hr(),
      downloadButton("export_csv","Export complete CSV"),
      tags$br(),tags$br(),
      downloadButton("export_history","Export full audit JSONL"),
      p("The review is saved locally in decisions.jsonl. Model data remain unchanged.",
        class="muted")
    ))
  ),
  tags$hr(),p("Review is offline. Return both CSV and audit JSONL for reconciliation.",
              class="muted")
)
server<-function(input,output,session) {
  reviewer<-reactiveVal("")
  saved<-reactiveVal(events)
  reviewed<-reactive(latest(saved()))
  choices<-reactive({
    indexes<-seq_along(ids)
    if(!is.null(input$model_filter)&&input$model_filter!="all")
      indexes<-indexes[statuses[indexes]==input$model_filter]
    if(!is.null(input$review_filter)&&input$review_filter!="all") {
      has<-ids[indexes]%in%names(reviewed())
      if(input$review_filter=="pending")indexes<-indexes[!has]
      else if(input$review_filter=="reviewed")indexes<-indexes[has]
      else indexes<-indexes[vapply(ids[indexes],function(id)
        sc(reviewed()[[id]]$decision)=="reject",logical(1))]
    }
    query<-trimws(sc(input$search))
    if(nzchar(query))
      indexes<-indexes[vapply(indexes,function(i) {
        x<-cases[[i]]
        labels<-vapply(x$detection$cases,function(z)sc(z$case_name),character(1))
        grepl(query,paste(x$record_id,sc(x$bibliographic$title),
                          paste(labels,collapse=" ")),fixed=TRUE,ignore.case=TRUE)
      },logical(1))]
    ids[indexes]
  })
  observeEvent(choices(),{
    available<-choices()
    selected<-if(!is.null(input$record)&&input$record%in%available)
      input$record else if(length(available))available[[1]] else character(0)
    labels<-vapply(available,function(id)paste0(id," | ",
      substr(sc(cases[[match(id,ids)]]$bibliographic$title),1,82)),
      character(1))
    names(available)<-labels
    updateSelectInput(session,"record",choices=available,selected=selected)
  },ignoreInit=FALSE)
  current_id<-reactive({
    c<-choices()
    if(!length(c))return(NULL)
    if(!is.null(input$record)&&input$record%in%c)input$record else c[[1]]
  })
  current<-reactive({
    id<-current_id()
    if(is.null(id))return(NULL)
    cases[[match(id,ids)]]
  })
  navigate<-function(delta) {
    c<-choices()
    if(!length(c)||is.null(current_id()))return()
    i<-match(current_id(),c)
    updateSelectInput(session,"record",selected=c[[max(1,min(length(c),i+delta))]])
  }
  observeEvent(input$prev,navigate(-1L))
  observeEvent(input$next_record,navigate(1L))
  observeEvent(TRUE,{
    showModal(modalDialog(title="Reviewer identification",
      textInput("reviewer_name","Your name"),easyClose=FALSE,
      footer=actionButton("identify","Start review")))
  },once=TRUE)
  observeEvent(input$identify,{
    name<-trimws(sc(input$reviewer_name))
    if(!nzchar(name))
      return(showNotification("Enter your name",type="error"))
    reviewer(name)
    removeModal()
  })
  output$run_info<-renderText(paste("W08:",sc(meta$source_w08),
                               "|",length(ids),"publications"))
  output$identity<-renderText(paste("Reviewer:",reviewer()))
  output$progress<-renderText(paste(length(intersect(ids,names(reviewed()))),
         "of",length(ids),"reviewed |",length(choices()),"shown"))
  observeEvent(list(current_id(),saved()),{
    id<-current_id()
    if(is.null(id))return()
    old<-reviewed()[[id]]
    updateRadioButtons(session,"decision",selected=if(is.null(old))
                         character(0) else sc(old$decision))
    updateTextAreaInput(session,"notes",value=if(is.null(old))"" else sc(old$notes))
    updateTextAreaInput(session,"alternative",value=if(is.null(old))""
                        else sc(old$alternative_cases))
  })
  output$publication<-renderUI({
    x<-current()
    if(is.null(x))return(div(class="review-box","No records match these filters."))
    b<-x$bibliographic
    div(class="review-box",
      h3(sc(b$title)),p(strong("Record ID: "),sc(x$record_id)),
      p(strong("Authors: "),sc(b$authors)),
      p(strong("Year: "),sc(b$year)),
      p(strong("Journal/source: "),sc(b$journal)),
      p(strong("DOI: "),sc(b$doi)),
      p(strong("Keywords: "),sc(b$keywords)),
      h4("Abstract"),
      div(class="abstract",if(nzchar(sc(b$abstract)))sc(b$abstract)
          else "Abstract unavailable"))
  })
  output$assessment<-renderUI({
    x<-current()
    if(is.null(x))return(NULL)
    z<-x$detection
    div(class="review-box",
      h4("Model publication assessment"),
      p(strong("Case classification: "),sc(z$case_evidence_status)),
      p(strong("Number of candidates: "),length(z$cases)),
      p(strong("Full-text priority: "),sc(z$fulltext_priority)),
      p(strong("Assessment reason: "),sc(z$assessment_reason)))
  })
  output$case_blocks<-renderUI({
    x<-current()
    if(is.null(x))return(NULL)
    items<-x$detection$cases
    old<-reviewed()[[x$record_id]]
    div(class="review-box",
      h4("Identified case candidates"),
      if(!length(items))p("No distinct candidate cases identified."),
      lapply(seq_along(items),function(i){
        z<-items[[i]]
        prior<-if(!is.null(old)&&length(old$case_decisions)>=i)
                   sc(old$case_decisions[[i]]) else ""
        div(class="case-block",
          h4(paste0("Candidate ",i,": ",
            if(nzchar(sc(z$case_name)))sc(z$case_name) else "(unnamed)")),
          p(strong("Description: "),sc(z$case_description)),
          p(strong("Location: "),sc(z$case_location)),
          p(strong("Commodity/sector: "),sc(z$case_sector)),
          p(strong("Implementation: "),sc(z$implementation_status)),
          p(strong("Exact supporting excerpt: "),sc(z$supporting_evidence)),
          p(strong("Case identification confidence: "),sc(z$confidence)),
          p(strong("Collective-action confidence: "),sc(z$collective_action_confidence)),
          p(strong("Collective-action basis: "),sc(z$collective_action_basis)),
          p(strong("Case type: "),sc(z$case_kind)),
          radioButtons(paste0("case_",i),paste0("Assessment of candidate ",i),
            choices=c("Accept candidate"="accept",
                      "Reject candidate"="reject",
                      "Uncertain / needs full text"="unclear"),
            selected=if(prior%in%c("accept","reject","unclear"))prior
                     else character(0),inline=TRUE)
        )
      }))
  })
  observeEvent(input$save,{
    id<-current_id()
    x<-current()
    if(is.null(id)||!nzchar(reviewer()))
      return(showNotification("Select a record and identify reviewer",type="error"))
    if(is.null(input$decision)||!input$decision%in%c("accept","reject"))
      return(showNotification("Choose publication Accept or Reject",type="error"))
    per_case<-lapply(seq_along(x$detection$cases),function(i){
      decision<-input[[paste0("case_",i)]]
      if(is.null(decision)||!decision%in%c("accept","reject","unclear"))""
      else decision
    })
    # Reject missing candidate-level decisions only if the user has started
    # recording them; publication-level review is valid independently.
    if(any(nzchar(unlist(per_case)))&&!all(nzchar(unlist(per_case))))
      return(showNotification("Complete all candidate-level choices or leave all blank.",
                              type="error"))
    entry<-list(schema="ostara-w08-colleague-decision-v1",
      record_id=id,queue_sha256=meta$queue_sha256,
      reviewer=reviewer(),decision=input$decision,
      case_decisions=per_case,alternative_cases=trimws(sc(input$alternative)),
      notes=trimws(sc(input$notes)),created_at_utc=utc())
    cat(as.character(jsonlite::toJSON(entry,auto_unbox=TRUE,null="null")),
        "\n",file=events_file,append=TRUE,sep="")
    saved(c(saved(),list(entry)))
    showNotification("Review saved",type="message")
  })
  review_df<-reactive({
    d<-reviewed()
    data.frame(record_id=ids,
      title=vapply(cases,function(x)sc(x$bibliographic$title),character(1)),
      model_status=statuses,model_case_count=n_candidates,
      model_fulltext_priority=vapply(cases,function(x)
        sc(x$detection$fulltext_priority),character(1)),
      model_case_descriptions=vapply(cases,function(x)paste(vapply(
        x$detection$cases,function(z)sc(z$case_description),character(1)),
        collapse=" | "),character(1)),
      reviewer=vapply(ids,function(id)sc(d[[id]]$reviewer),character(1)),
      review_decision=vapply(ids,function(id)sc(d[[id]]$decision),character(1)),
      case_decisions=vapply(ids,function(id)sc(d[[id]]$case_decisions),character(1)),
      alternative_cases=vapply(ids,function(id)sc(d[[id]]$alternative_cases),character(1)),
      notes=vapply(ids,function(id)sc(d[[id]]$notes),character(1)),
      reviewed_at_utc=vapply(ids,function(id)sc(d[[id]]$created_at_utc),character(1)),
      queue_sha256=rep(meta$queue_sha256,length(ids)),
      stringsAsFactors=FALSE)
  })
  output$export_csv<-downloadHandler(
    filename=function()paste0("ostara_w08_review_",Sys.Date(),".csv"),
    content=function(file)utils::write.csv(review_df(),file,row.names=FALSE,
                                           na="",fileEncoding="UTF-8"))
  output$export_history<-downloadHandler(
    filename=function()"ostara_w08_review_history.jsonl",
    content=function(file) {
      if(file.exists(events_file))file.copy(events_file,file)
      else writeLines(character(),file)
    })
}
shiny::shinyApp(ui,server)
