#!/usr/bin/env Rscript
# Packaged Shiny human-review app for full-text-only OSTARA case candidates.
args<-commandArgs(trailingOnly=TRUE)
if(length(args)!=2L)stop("Usage: Rscript case_discrepancy_app.R queue.jsonl review_dir")
for(p in c("shiny","jsonlite","digest"))
  if(!requireNamespace(p,quietly=TRUE))
    stop("Install the R package: ",p)
queue_file<-normalizePath(args[[1L]],mustWork=TRUE)
state<-normalizePath(args[[2L]],mustWork=TRUE)
sc<-function(x)if(is.null(x)||!length(x))"" else
  paste(as.character(unlist(x,use.names=FALSE)),collapse="; ")
lines<-readLines(queue_file,warn=FALSE,encoding="UTF-8")
data<-lapply(lines[nzchar(trimws(lines))],jsonlite::fromJSON,
             simplifyVector=FALSE)
ids<-vapply(data,function(x)sc(x$record_id),character(1))
if(!length(ids)||anyDuplicated(ids))stop("Empty or duplicated reviewer queue")
queue_sha<-digest::digest(file=queue_file,algo="sha256",serialize=FALSE)
events_file<-file.path(state,"human_decisions.jsonl")
readjl<-function(path) {
  if(!file.exists(path))return(list())
  z<-readLines(path,warn=FALSE,encoding="UTF-8")
  lapply(z[nzchar(trimws(z))],jsonlite::fromJSON,simplifyVector=FALSE)
}
# Decisions are versioned by publication and mechanism ledger checksum.
record_key<-function(z)paste(sc(z$cohort),sc(z$record_id),
                            sc(z$mechanism_manifest_sha256),sep="|")
keys<-vapply(data,record_key,character(1))
if(anyDuplicated(keys))stop("Repeated review identity")
load_decisions<-function() {
  all<-readjl(events_file)
  out<-list()
  for(e in all) {
    if(!identical(sc(e$schema),"ostara-ft-case-discrepancy-decision-v1"))next
    out[[sc(e$review_key)]]<-e
  }
  out
}
decisions<-shiny::reactiveVal(load_decisions())
current<-shiny::reactiveVal(1L)
font_css<-shiny::HTML("body{font-family:system-ui, sans-serif;background:#f8fafb;}
  .review-panel{background:white;border:1px solid #e1e6ea;border-radius:9px;
    padding:17px;margin-bottom:12px;}
  .evidence{white-space:pre-wrap;background:#f3f5f7;border-left:3px solid #72889b;
    padding:12px;}
  .small-label{font-size:12px;color:#637587;text-transform:uppercase;letter-spacing:.06em;}
  .candidate{border:1px solid #dde4eb;border-radius:8px;margin-top:10px;padding:14px;}
  .flag{color:#9d3f00;font-weight:600;}")
ui<-shiny::fluidPage(
  shiny::tags$head(shiny::tags$style(font_css)),
  shiny::titlePanel("OSTARA | Full-text case discovery review"),
  shiny::fluidRow(
    shiny::column(3,
      shiny::div(class="review-panel",
        shiny::textInput("reviewer","Reviewer",value=""),
        shiny::selectInput("filter","Review status",
                           choices=c("All","Pending","Accepted","Rejected","Unclear")),
        shiny::checkboxInput("flags_only","Evidence flags only",FALSE),
        shiny::textInput("search","Search title or record ID"),
        shiny::uiOutput("summary"),
        shiny::selectInput("record","Publication",choices=character()),
        shiny::actionButton("previous","Previous"),
        shiny::actionButton("next","Next")
      ),
      shiny::div(class="review-panel",
        shiny::h4("Exports"),
        shiny::downloadButton("approved","Approved W09 IDs"),
        shiny::tags$p("Only explicitly accepted publications are exported."),
        shiny::downloadButton("all_decisions","All human decisions")
      )
    ),
    shiny::column(9,
      shiny::uiOutput("publication"),
      shiny::div(class="review-panel",
        shiny::h4("Human adjudication"),
        shiny::radioButtons("decision","Decision",
          choices=c("Pending"="pending",
                    "Accept for case extraction"="accept",
                    "Reject"="reject","Unclear / more review"="unclear"),
          selected="pending",inline=TRUE),
        shiny::textAreaInput("notes","Review notes",rows=3,width="100%"),
        shiny::actionButton("save","Save decision",class="btn-primary"),
        shiny::textOutput("saved_status")
      )
    )
  )
)
server<-function(input,output,session) {
  status_of<-function(i) {
    e<-decisions()[[keys[[i]]]]
    if(is.null(e))"pending" else sc(e$decision)
  }
  options<-shiny::reactive({
    x<-seq_along(data)
    filter_map<-c(Pending="pending",Accepted="accept",
                  Rejected="reject",Unclear="unclear")
    if(!is.null(input$filter)&&input$filter!="All")
      x<-x[vapply(x,function(i)identical(status_of(i),filter_map[[input$filter]]),
                  logical(1))]
    if(isTRUE(input$flags_only))
      x<-x[vapply(x,function(i)
        isTRUE(data[[i]]$fulltext_evidence_review_required),logical(1))]
    needle<-tolower(trimws(input$search))
    if(nzchar(needle))
      x<-x[vapply(x,function(i)
        grepl(needle,tolower(paste(sc(data[[i]]$title),ids[[i]])),
              fixed=TRUE),logical(1))]
    x
  })
  shiny::observe({
    x<-options()
    names<-vapply(x,function(i)paste0(
      "[",if(status_of(i)=="pending")"Pending" else status_of(i),"] ",
      sc(data[[i]]$cohort)," · ",substr(sc(data[[i]]$title),1,70),
      " (",ids[[i]],")"),character(1))
    old<-isolate(input$record)
    chosen<-if(length(x)&&!is.null(old)&&old%in%as.character(x))
      old else if(length(x))as.character(x[[1L]]) else character()
    shiny::updateSelectInput(session,"record",
      choices=setNames(as.character(x),names),selected=chosen)
  })
  selected<-shiny::reactive({
    x<-as.integer(input$record)
    shiny::req(length(x)==1L,!is.na(x),x>=1L,x<=length(data))
    x
  })
  shiny::observeEvent(input$previous,{
    x<-options();k<-match(selected(),x)
    if(!is.na(k)&&k>1L)shiny::updateSelectInput(
      session,"record",selected=as.character(x[[k-1L]]))
  })
  shiny::observeEvent(input$next,{
    x<-options();k<-match(selected(),x)
    if(!is.na(k)&&k<length(x))shiny::updateSelectInput(
      session,"record",selected=as.character(x[[k+1L]]))
  })
  shiny::observeEvent(selected(),{
    e<-decisions()[[keys[[selected()]]]]
    shiny::updateRadioButtons(session,"decision",
      selected=if(is.null(e))"pending" else sc(e$decision))
    shiny::updateTextAreaInput(session,"notes",
      value=if(is.null(e))"" else sc(e$notes))
  })
  output$summary<-shiny::renderUI({
    n<-vapply(seq_along(data),status_of,character(1))
    shiny::tagList(
      shiny::tags$p(shiny::strong(length(data))," full-text-only candidates"),
      shiny::tags$p(sum(n=="accept")," accepted · ",
                    sum(n=="pending")," pending · ",
                    sum(n=="reject")," rejected · ",
                    sum(n=="unclear")," unclear"),
      shiny::tags$p(length(options())," matching filters")
    )
  })
  output$publication<-shiny::renderUI({
    z<-data[[selected()]]
    cs<-z$candidate_cases
    shiny::tagList(
      shiny::div(class="review-panel",
        shiny::tags$div(class="small-label",
          paste(sc(z$cohort),sc(z$record_id),sep=" · ")),
        shiny::h3(sc(z$title)),
        shiny::tags$p(shiny::strong("DOI: "),sc(z$doi)),
        shiny::tags$p(shiny::strong("Abstract W08: "),sc(z$w08_status),
          "  |  ",shiny::strong("Full text: "),sc(z$ft_case_status)),
        shiny::tags$p(shiny::strong("Full-text assessment: "),
          sc(z$assessment_reason)),
        if(isTRUE(z$fulltext_evidence_review_required))
          shiny::tags$p(class="flag","Source quotation/anchor needs verification"),
        shiny::tags$small("Mechanism run: ",sc(z$mechanism_run))
      ),
      shiny::div(class="review-panel",
        shiny::h4(paste("Candidate cases (",length(cs),")")),
        lapply(seq_along(cs),function(j) {
          ca<-cs[[j]]
          shiny::div(class="candidate",
            shiny::h4(paste(j,sc(ca$candidate_label),sep=". ")),
            shiny::tags$p(shiny::strong("Description: "),
              sc(ca$case_description)),
            shiny::tags$p(shiny::strong("Implementation status: "),
              sc(ca$implementation_status)),
            shiny::tags$p(shiny::strong("Eligibility rationale: "),
              sc(ca$eligibility_reason)),
            shiny::tags$p(shiny::strong("Source anchor: "),
              sc(ca$source_anchor)),
            shiny::tags$div(class="evidence",sc(ca$source_quote))
          )
        }),
        if(length(z$evidence_review_flags))
          shiny::tags$details(
            shiny::tags$summary("Evidence review flags"),
            shiny::tags$pre(jsonlite::toJSON(z$evidence_review_flags,
                               pretty=TRUE,auto_unbox=TRUE))
          )
      )
    )
  })
  saved_status<-shiny::reactiveVal("")
  output$saved_status<-shiny::renderText(saved_status())
  shiny::observeEvent(input$save,{
    i<-selected()
    who<-trimws(input$reviewer)
    if(!nzchar(who)) {
      shiny::showNotification("Enter a reviewer name before saving.",
                              type="error")
      return()
    }
    e<-list(schema="ostara-ft-case-discrepancy-decision-v1",
      review_key=keys[[i]],record_id=ids[[i]],
      cohort=sc(data[[i]]$cohort),
      mechanism_ledger_sha256=sc(data[[i]]$mechanism_ledger_sha256),
      decision=input$decision,notes=input$notes,reviewer=who,
      timestamp_utc=format(Sys.time(),"%Y-%m-%dT%H:%M:%SZ",tz="UTC"),
      review_queue_sha256=queue_sha)
    cat(as.character(jsonlite::toJSON(e,auto_unbox=TRUE,null="null")),
        "\n",file=events_file,append=TRUE,sep="")
    decisions(load_decisions())
    saved_status(paste("Saved",e$decision,"for",ids[[i]]))
    shiny::showNotification("Decision saved",type="message")
  })
  export_table<-function(accepted_only=FALSE) {
    current_decisions<-decisions()
    rows<-lapply(seq_along(data),function(i) {
      z<-data[[i]]
      e<-current_decisions[[keys[[i]]]]
      decision<-if(is.null(e))"pending" else sc(e$decision)
      if(accepted_only&&decision!="accept")return(NULL)
      data.frame(
        record_id=ids[[i]],cohort=sc(z$cohort),
        title=sc(z$title),doi=sc(z$doi),
        w08_status=sc(z$w08_status),
        ft_case_status=sc(z$ft_case_status),
        candidate_case_count=as.integer(z$candidate_case_count),
        evidence_review_required=isTRUE(z$fulltext_evidence_review_required),
        human_decision=decision,
        notes=if(is.null(e))"" else sc(e$notes),
        reviewer=if(is.null(e))"" else sc(e$reviewer),
        timestamp_utc=if(is.null(e))"" else sc(e$timestamp_utc),
        mechanism_run=sc(z$mechanism_run),
        mechanism_ledger_sha256=sc(z$mechanism_ledger_sha256),
        source_w09=sc(z$source_w09),
        stringsAsFactors=FALSE
      )
    })
    rows<-Filter(Negate(is.null),rows)
    if(!length(rows))return(data.frame(
      record_id=character(),cohort=character(),human_decision=character()))
    do.call(rbind,rows)
  }
  output$approved<-shiny::downloadHandler(
    filename=function()paste0("ostara-approved-new-cases-",
                              Sys.Date(),".csv"),
    content=function(file)utils::write.csv(
      export_table(TRUE),file,row.names=FALSE,na="")
  )
  output$all_decisions<-shiny::downloadHandler(
    filename=function()paste0("ostara-case-review-all-",
                              Sys.Date(),".csv"),
    content=function(file)utils::write.csv(
      export_table(FALSE),file,row.names=FALSE,na="")
  )
}
shiny::runApp(shiny::shinyApp(ui,server),launch.browser=TRUE)
