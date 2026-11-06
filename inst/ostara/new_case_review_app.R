# OSTARA colleague review: full-text-only case discoveries (portable ZIP)
suppressPackageStartupMessages({
  library(shiny)
  library(jsonlite)
  library(digest)
})
manifest_file<-"manifest.json"
queue_file<-file.path("data","review_queue.jsonl")
if(!all(file.exists(c(manifest_file,queue_file))))
  stop("Open this app from the extracted ostara_new_case_review folder.")
sha<-function(p)digest::digest(file=p,algo="sha256",serialize=FALSE)
meta<-jsonlite::fromJSON(manifest_file,simplifyVector=FALSE)
if(!identical(meta$schema,"ostara-new-cases-review-bundle-v1")||
   !identical(meta$queue_sha256,sha(queue_file)))
  stop("Review queue integrity check failed. Do not edit manifest or data.")
readjl<-function(p) {
  if(!file.exists(p))return(list())
  s<-readLines(p,warn=FALSE,encoding="UTF-8")
  lapply(s[nzchar(trimws(s))],jsonlite::fromJSON,simplifyVector=FALSE)
}
items<-readjl(queue_file)
ids<-vapply(items,function(x)x$record_id,character(1))
if(length(ids)!=as.integer(meta$records)||anyDuplicated(ids))
  stop("Review queue does not match manifest.")
sc<-function(x) {
  if(is.null(x)||!length(x)||all(is.na(x)))return("")
  paste(as.character(unlist(x,use.names=FALSE)),collapse="; ")
}
utc<-function()format(Sys.time(),"%Y-%m-%dT%H:%M:%SZ",tz="UTC")
events_path<-"decisions.jsonl"
events<-readjl(events_path)
if(length(events)&&any(!vapply(events,function(x)
  identical(x$schema,"ostara-new-cases-decision-v1")&&
  identical(x$queue_sha256,meta$queue_sha256)&&x$record_id%in%ids,
  logical(1))))
  stop("Saved review history does not match this ZIP.")
latest<-function(rows) {
  result<-list()
  for(x in rows)result[[x$record_id]]<-x
  result
}
ui<-fluidPage(
  tags$head(tags$meta(charset="utf-8"),
    tags$style(HTML("
      body{max-width:1470px;margin:auto;padding:10px 15px 50px}
      .review-panel{border:1px solid #d7dce2;border-radius:9px;padding:18px;
         margin-bottom:15px;background:white}
      .muted{color:#60717e}
      .abstract{white-space:pre-wrap;line-height:1.6;overflow-wrap:break-word}
      .case{background:#f5f7f9;border-left:3px solid #6c8da6;
         padding:12px 15px;margin:12px 0}
      .quotation{white-space:pre-wrap;padding:10px 12px;background:#fff;
         border:1px solid #e5e8eb}
      .warning{color:#9a4800;font-weight:600}
    "))),
  titlePanel("OSTARA | Additional full-text cases"),
  p("Independent human review of publications first identified as case-positive in full texts.",
    class="muted"),
  fluidRow(
    column(3,selectInput("category","W08 classification",
      choices=c("All"="all","No identifiable case"="no_identifiable_case",
        "Unclear"="unclear","Unvalidated W08"="unvalidated_W08",
        "Missing final W08 decision"="missing_final_W08_classification"))),
    column(3,selectInput("review_filter","Review progress",
      choices=c("All"="all","Unreviewed"="pending",
                "Has case"="has_case",
                "Does not have case"="does_not_have_case"))),
    column(3,textInput("search","Find title or record ID")),
    column(3,div(class="review-panel",
      strong(textOutput("progress")),textOutput("reviewer_status")))
  ),
  selectInput("record","Publication",choices=character(),width="100%"),
  fluidRow(column(6,actionButton("previous","Previous")),
           column(6,actionButton("next_record","Next"))),
  tags$hr(),
  fluidRow(
    column(8,
      uiOutput("publication"),
      uiOutput("candidates")),
    column(4,
      div(class="review-panel",
        h4("Human adjudication"),
        textInput("reviewer","Reviewer name"),
        radioButtons("decision","Does this publication contain at least one eligible collective-action case?",
          choices=c("Has case"="has_case",
                    "Does not have case"="does_not_have_case"),
          selected=character(0)),
        textAreaInput("notes","Review notes or justification",
          rows=5,width="100%"),
        actionButton("save","Save decision",class="btn-primary"),
        textOutput("saved"),
        tags$hr(),
        downloadButton("export_json","Export adjudications JSON"),
        p("Export once reviews are finished; unreviewed publications are omitted.",
          class="muted")
      )
    )
  )
)
server<-function(input,output,session) {
  history<-reactiveVal(events)
  byid<-reactive(latest(history()))
  status<-function(id) {
    e<-byid()[[id]]
    if(is.null(e))"pending" else sc(e$decision)
  }
  available<-reactive({
    keep<-ids
    if(!is.null(input$category)&&input$category!="all")
      keep<-keep[vapply(keep,function(id)identical(
        sc(items[[match(id,ids)]]$w08_status),input$category),logical(1))]
    if(!is.null(input$review_filter)&&input$review_filter!="all")
      keep<-keep[vapply(keep,function(id)
        identical(status(id),input$review_filter),logical(1))]
    term<-tolower(trimws(sc(input$search)))
    if(nzchar(term))keep<-keep[vapply(keep,function(id) {
      x<-items[[match(id,ids)]]
      grepl(term,tolower(paste(id,sc(x$bibliographic$title))),fixed=TRUE)
    },logical(1))]
    keep
  })
  observe({
    opts<-available()
    labels<-vapply(opts,function(id) {
      x<-items[[match(id,ids)]]
      paste0("[",status(id),"] ",substr(sc(x$bibliographic$title),1,110),
             " | ",id)
    },character(1))
    selected<-isolate(input$record)
    if(is.null(selected)||!selected%in%opts)
      selected<-if(length(opts))opts[[1L]] else character()
    updateSelectInput(session,"record",
      choices=setNames(opts,labels),selected=selected)
  })
  current_id<-reactive({
    a<-available()
    if(!length(a))return(NULL)
    if(!is.null(input$record)&&input$record%in%a)input$record else a[[1L]]
  })
  current<-reactive({
    id<-current_id()
    if(is.null(id))return(NULL)
    items[[match(id,ids)]]
  })
  navigate<-function(step) {
    opts<-available()
    id<-current_id()
    if(!length(opts)||is.null(id))return()
    i<-match(id,opts)
    updateSelectInput(session,"record",
      selected=opts[[max(1L,min(length(opts),i+step))]])
  }
  observeEvent(input$previous,navigate(-1L))
  observeEvent(input$next_record,navigate(1L))
  observeEvent(current_id(),{
    id<-current_id()
    if(is.null(id))return()
    prior<-byid()[[id]]
    updateRadioButtons(session,"decision",
      selected=if(is.null(prior))character(0) else sc(prior$decision))
    updateTextAreaInput(session,"notes",
      value=if(is.null(prior))"" else sc(prior$notes))
  })
  output$progress<-renderText(paste(length(names(byid())),"/",
       length(ids)," reviewed"))
  output$reviewer_status<-renderText(paste(length(available()),"shown"))
  output$publication<-renderUI({
    x<-current()
    if(is.null(x))return(div(class="review-panel",
      "No publications match the current filters."))
    b<-x$bibliographic
    div(class="review-panel",
      h3(sc(b$title)),
      p(strong("Record ID: "),sc(x$record_id),
        " | ",strong("Cohort: "),sc(x$cohort)),
      p(strong("Authors: "),sc(b$authors)),
      p(strong("Year: "),sc(b$year)),
      p(strong("Source: "),sc(b$journal)),
      p(strong("DOI: "),sc(b$doi)),
      div(
        if(nzchar(sc(b$doi_url)))
          tags$a(href=sc(b$doi_url),target="_blank",rel="noopener noreferrer",
                 "Open DOI record"),
        "　",
        tags$a(href=sc(b$scholar_url),target="_blank",
               rel="noopener noreferrer","Search title on Google Scholar")
      ),
      p(strong("Keywords: "),sc(b$keywords)),
      h4("Abstract"),
      div(class="abstract",if(nzchar(sc(b$abstract)))sc(b$abstract)
          else "No abstract available in the original W08 bibliographic source."),
      tags$hr(),
      p(strong("Final W08 classification: "),sc(x$w08_status)),
      if(nzchar(sc(x$w08_model_status)))
        p(strong("Original W08 model classification: "),sc(x$w08_model_status)),
      p(strong("Full-text classification: "),sc(x$fulltext_status)),
      p(strong("Full-text assessment reasoning: "),
        sc(x$fulltext_assessment_reason)),
      if(isTRUE(x$evidence_review_required))
        p(class="warning","Some supporting evidence requires verification")
    )
  })
  output$candidates<-renderUI({
    x<-current()
    if(is.null(x))return(NULL)
    div(class="review-panel",
      h4(paste("Full-text candidate cases (",length(x$candidate_cases),")")),
      lapply(seq_along(x$candidate_cases),function(i) {
        z<-x$candidate_cases[[i]]
        div(class="case",
          h4(paste0("Candidate ",i,": ",sc(z$candidate_label))),
          p(strong("Description: "),sc(z$case_description)),
          p(strong("Implementation: "),sc(z$implementation_status)),
          p(strong("Why eligible: "),sc(z$eligibility_reason)),
          p(strong("Evidence location: "),sc(z$source_anchor)),
          div(class="quotation",sc(z$source_quote))
        )
      }),
      if(length(x$evidence_review_flags))
        tags$details(tags$summary("Evidence validation flags"),
          tags$pre(as.character(jsonlite::toJSON(
            x$evidence_review_flags,pretty=TRUE,auto_unbox=TRUE))))
    )
  })
  notice<-reactiveVal("")
  output$saved<-renderText(notice())
  observeEvent(input$save,{
    id<-current_id()
    if(is.null(id))return()
    who<-trimws(sc(input$reviewer))
    if(!nzchar(who))
      return(showNotification("Enter reviewer name first.",type="error"))
    if(is.null(input$decision)||!input$decision%in%
       c("has_case","does_not_have_case"))
      return(showNotification("Choose Has case or Does not have case.",
                              type="error"))
    e<-list(schema="ostara-new-cases-decision-v1",
      record_id=id,decision=input$decision,notes=sc(input$notes),
      reviewer=who,timestamp_utc=utc(),queue_sha256=meta$queue_sha256)
    cat(as.character(jsonlite::toJSON(e,auto_unbox=TRUE,null="null")),
        "\n",file=events_path,append=TRUE,sep="")
    history(readjl(events_path))
    notice(paste("Saved",id,"as",input$decision))
    showNotification("Decision saved.",type="message")
  })
  output$export_json<-downloadHandler(
    filename=function()paste0("ostara_new_cases_adjudications_",
                              Sys.Date(),".json"),
    content=function(file) {
      latest_events<-byid()
      decisions<-unname(latest_events)
      payload<-list(schema="ostara-new-cases-adjudications-v1",
        queue_sha256=meta$queue_sha256,
        decisions=decisions,
        exported_at_utc=utc())
      jsonlite::write_json(payload,file,auto_unbox=TRUE,pretty=TRUE,
                           null="null")
    }
  )
}
shiny::runApp(shiny::shinyApp(ui,server),launch.browser=TRUE)
