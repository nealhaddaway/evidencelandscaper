suppressPackageStartupMessages({library(shiny);library(jsonlite);library(digest)})
a<-commandArgs(trailingOnly=TRUE)
q<-normalizePath(a[[1]],mustWork=TRUE)
state<-a[[2]];dir.create(state,recursive=TRUE,showWarnings=FALSE)
readjl<-function(p){if(!file.exists(p))return(list());l<-readLines(p,warn=FALSE);lapply(l[nzchar(trimws(l))],jsonlite::fromJSON,simplifyVector=FALSE)}
cases<-readjl(q)
if(!length(cases))stop("No missing abstracts in this W04 screening run.")
f<-file.path(state,"abstract_corrections.jsonl")
existing<-readjl(f)
latest<-function(z){m<-list();for(x in z)m[[x$record_id]]<-x;m}
ui<-fluidPage(titlePanel("W04 | Missing abstracts"),
 p("Paste missing abstracts and save. No model calls are made in this editor."),
 textOutput("count"),h4(textOutput("title")),p(textOutput("record")),
 textAreaInput("abstract","Abstract",rows=14,width="100%"),
 actionButton("prev","Previous"),actionButton("save","Save abstract",class="btn-primary"),
 actionButton("next","Next"),verbatimTextOutput("saved"))
server<-function(input,output,session){
 pos<-reactiveVal(1L);ev<-reactiveVal(existing)
 current<-reactive(cases[[pos()]])
 output$count<-renderText(paste(pos(),"of",length(cases),"records"))
 output$title<-renderText(as.character(current()$bibliographic$title))
 output$record<-renderText(as.character(current()$record_id))
 observeEvent(list(pos(),ev()),{
  x<-latest(ev())[[as.character(current()$record_id)]]
  updateTextAreaInput(session,"abstract",value=if(is.null(x))"" else as.character(x$abstract))
 })
 observeEvent(input$prev,pos(max(1L,pos()-1L)))
 observeEvent(input[['next']],pos(min(length(cases),pos()+1L)))
 observeEvent(input$save,{
  x<-current();value<-trimws(input$abstract)
  if(!nzchar(value)){showNotification("Enter an abstract first",type="error");return()}
  event<-list(schema="local-w04-abstract-correction-v1",record_id=x$record_id,
     review_case_id=x$review_case_id,abstract=value,
     queue_sha256=digest::digest(file=q,algo="sha256",serialize=FALSE),
     created_at_utc=format(Sys.time(),"%Y-%m-%dT%H:%M:%SZ",tz="UTC"))
  cat(jsonlite::toJSON(event,auto_unbox=TRUE),"\n",file=f,append=TRUE,sep="")
  ev(c(ev(),list(event)));showNotification("Abstract saved",type="message")
  if(pos()<length(cases))pos(pos()+1L)
 })
 output$saved<-renderText(paste(length(latest(ev())),"abstracts saved. Close when finished."))
}
shiny::runApp(shinyApp(ui,server),launch.browser=TRUE)
