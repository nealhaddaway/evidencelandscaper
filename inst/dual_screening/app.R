library(shiny)
library(googlesheets4)
library(jsonlite)
library(digest)

# Basic project-only login, intentionally configured in source.
accounts <- c(neal="ramonycajal", maria="ramonycajal")
users <- names(accounts)
ss <- Sys.getenv("RYC_SCREENING_SHEET_ID")
credential <- Sys.getenv("LEM_GOOGLE_SERVICE_ACCOUNT_JSON")
if(!nzchar(ss)||!nzchar(credential))
  stop("Configure a dedicated RYC_SCREENING_SHEET_ID and the existing Google service account credential.")
if(!file.exists(credential)){
  parsed <- jsonlite::fromJSON(credential)
  stopifnot(identical(parsed$type,"service_account"))
  keyfile <- tempfile(fileext=".json")
  writeLines(credential,keyfile,useBytes=TRUE)
  Sys.chmod(keyfile,"0600")
  gs4_auth(path=keyfile,cache=FALSE)
  unlink(keyfile)
} else gs4_auth(path=credential,cache=FALSE)

tabs <- c(records="ryc_records",events="ryc_events")
record_cols <- c("batch","record_id","position","title","abstract","authors","year","journal","doi","volume","issue","pages","keywords","source_hash")
event_cols <- c("event_id","batch","record_id","reviewer","kind","decision","notes","utc")
empty <- function(cols)as.data.frame(setNames(rep(list(character()),length(cols)),cols),stringsAsFactors=FALSE)
read_tab <- function(name,cols){
  x <- as.data.frame(read_sheet(ss,sheet=name,col_types="c"),stringsAsFactors=FALSE)
  if(!identical(names(x),cols))stop("Invalid storage schema: ",name)
  x[] <- lapply(x,function(z){z[is.na(z)]<-"";as.character(z)})
  x
}
# Dedicated sheet only; initialise headers once, never replace an existing tab.
for(k in names(tabs)){
  if(!tabs[[k]]%in%sheet_names(ss))
    sheet_write(empty(if(k=="records")record_cols else event_cols),ss=ss,sheet=tabs[[k]])
}
load_records <- function()read_tab(tabs[["records"]],record_cols)
load_events <- function()read_tab(tabs[["events"]],event_cols)
append_event <- function(batch,id,user,kind,decision="",notes=""){
  e <- data.frame(event_id=digest(paste(batch,id,user,kind,decision,format(Sys.time(),"%OS6"),runif(1)),algo="sha256"),
    batch=batch,record_id=id,reviewer=user,kind=kind,decision=decision,notes=notes,
    utc=format(Sys.time(),"%Y-%m-%dT%H:%M:%OS6Z",tz="UTC"),stringsAsFactors=FALSE)
  sheet_append(ss,e,sheet=tabs[["events"]])
  observed <- load_events()
  if(!e$event_id%in%observed$event_id)stop("Save not verified; refresh before retrying.")
  observed
}
parse_ris <- function(path,batch){
  lines <- readLines(path,warn=FALSE,encoding="UTF-8")
  lines <- sub("^\ufeff","",lines)
  out <- list(); rec <- NULL; tag <- NULL
  for(s in lines){
    if(grepl("^[A-Z0-9]{2}  -",s)){
      tag <- substr(s,1,2);v<-trimws(sub("^[A-Z0-9]{2}  -[ ]?","",s))
      if(tag=="TY"){
        if(!is.null(rec))stop("Nested RIS record.")
        rec<-list(TY=v)
      } else if(tag=="ER"){
        if(is.null(rec))stop("Unexpected RIS end.")
        out[[length(out)+1L]]<-rec;rec<-NULL;tag<-NULL
      } else if(!is.null(rec))rec[[tag]]<-c(rec[[tag]],v)
    } else if(!is.null(rec)&&!is.null(tag)&&nzchar(trimws(s))){
      n<-length(rec[[tag]]);rec[[tag]][n]<-paste(rec[[tag]][n],trimws(s))
    }
  }
  if(!is.null(rec)||length(out)!=200L)stop("Expected exactly 200 complete RIS records.")
  val<-function(r,keys,sep="; "){for(k in keys)if(length(r[[k]]))return(paste(r[[k]],collapse=sep));""}
  h<-digest(file=path,algo="sha256",serialize=FALSE)
  d<-do.call(rbind,lapply(seq_along(out),function(i){
    r<-out[[i]]
    data.frame(batch=batch,record_id=val(r,"ID"),position=as.character(i),
      title=val(r,c("TI","T1")),abstract=val(r,c("AB","N2")," "),
      authors=val(r,c("AU","A1")),year=val(r,c("PY","Y1")),
      journal=val(r,c("T2","JO","JF")),doi=val(r,"DO"),
      volume=val(r,"VL"),issue=val(r,"IS"),pages=paste(Filter(nzchar,c(val(r,"SP"),val(r,"EP"))),collapse="-"),
      keywords=val(r,"KW"),source_hash=h,stringsAsFactors=FALSE)
  }))
  if(any(!nzchar(d$record_id))||anyDuplicated(d$record_id))stop("RIS IDs must be present and unique.")
  if(any(nchar(as.matrix(d))>49000L))stop("A field exceeds safe Google Sheets cell length.")
  d
}
# Append-only decisions; a submission freezes each reviewer's latest decisions.
snapshot <- function(r,e,b){
  r<-r[r$batch==b,,drop=FALSE];r<-r[order(as.integer(r$position)),,drop=FALSE]
  e<-e[e$batch==b,,drop=FALSE]
  decision<-matrix("",nrow(r),2,dimnames=list(r$record_id,users))
  notes<-decision
  submitted<-setNames(rep(FALSE,2),users)
  for(u in users){
    x<-e[e$reviewer==u,,drop=FALSE]
    freeze<-which(x$kind=="submit")
    if(length(freeze))x<-x[seq_len(freeze[1L]),,drop=FALSE]
    for(j in which(x$kind=="screen")){
      i<-match(x$record_id[j],r$record_id)
      if(!is.na(i)&&x$decision[j]%in%c("include","exclude")){
        decision[i,u]<-x$decision[j];notes[i,u]<-x$notes[j]
      }
    }
    submitted[u]<-length(freeze)>0L&&nrow(r)>0L&&all(nzchar(decision[,u]))
  }
  unlocked<-all(submitted)
  conflict<-if(unlocked)which(decision[,1]!=decision[,2])else integer()
  final<-rep("",nrow(r));adjudicator<-final;resolution_notes<-final
  if(unlocked){
    agree<-which(decision[,1]==decision[,2]);final[agree]<-decision[agree,1]
    for(j in which(e$kind=="resolve")){
      i<-match(e$record_id[j],r$record_id)
      if(!is.na(i)&&i%in%conflict&&e$reviewer[j]%in%users&&e$decision[j]%in%c("include","exclude")&&!nzchar(final[i])){
        final[i]<-e$decision[j];adjudicator[i]<-e$reviewer[j];resolution_notes[i]<-e$notes[j]
      }
    }
  }
  list(records=r,decision=decision,notes=notes,submitted=submitted,unlocked=unlocked,
    conflict=conflict,final=final,adjudicator=adjudicator,resolution_notes=resolution_notes)
}
stats <- function(s){
  a<-s$decision[,1];b<-s$decision[,2];p<-mean(a==b)
  pe<-sum(vapply(c("include","exclude"),function(k)mean(a==k)*mean(b==k),numeric(1)))
  c(agreement=p,kappa=if(pe==1)NA_real_ else (p-pe)/(1-pe))
}
css <- "
body{background:#f5f7fa;color:#203245;font-family:system-ui,sans-serif}
.container-fluid{max-width:1100px;margin:auto;padding:24px}
.panelbox{background:white;border:1px solid #dce3e9;border-radius:12px;padding:22px;margin:16px 0}
.record-title{font-size:22px;font-weight:650;line-height:1.4}
.abstract{font-size:16px;line-height:1.7;white-space:pre-wrap}
.meta{color:#526578;font-size:14px;margin:12px 0}
.actions{display:flex;gap:12px;flex-wrap:wrap;margin:16px 0}
.actions .btn{min-width:120px;font-weight:600}
.progress{height:10px;margin:8px 0}
.login{max-width:400px;margin:50px auto}
"
ui<-fluidPage(tags$head(tags$style(HTML(css)),tags$script(HTML("
document.addEventListener('keydown',function(e){
 if(e.repeat||e.ctrlKey||e.metaKey||e.altKey||/INPUT|TEXTAREA|SELECT|BUTTON/.test(e.target.tagName)||e.target.isContentEditable)return;
 var ids={i:'include',e:'exclude',ArrowLeft:'previous',ArrowRight:'next'};
 var b=document.getElementById(ids[e.key]);if(b&&!b.disabled&&b.offsetParent!==null){e.preventDefault();b.click();}
});"))),uiOutput("body"))
server<-function(input,output,session){
  user<-reactiveVal(NULL); records<-reactiveVal(NULL);events<-reactiveVal(NULL)
  idx<-reactiveVal(1L);ci<-reactiveVal(1L)
  refresh<-function(){records(load_records());events(load_events())}
  guard<-function(fun)tryCatch(fun(),error=function(e)showNotification(conditionMessage(e),type="error",duration=12))
  output$body<-renderUI({
    if(is.null(user()))return(div(class="panelbox login",h2("Ramón y Cajal"),p("Independent abstract screening"),
      textInput("username","Username"),passwordInput("password","Password"),actionButton("login","Sign in",class="btn-primary")))
    tagList(h2("Ramón y Cajal · Abstract screening"),
      div(class="meta",paste("Signed in as",user()),actionButton("logout","Sign out",class="btn-default btn-sm")),
      uiOutput("batch_picker"),actionButton("refresh","Refresh progress"),
      tabsetPanel(id="section",
        tabPanel("Screening",uiOutput("screen")),
        tabPanel("Conflict resolution",uiOutput("resolve")),
        tabPanel("Batch management",uiOutput("management"))))
  })
  observeEvent(input$login,{
    u<-tolower(trimws(input$username))
    if(u%in%users&&identical(input$password,unname(accounts[u])))guard(function(){refresh();user(u)})
    else showNotification("Incorrect username or password.",type="error")
  })
  observeEvent(input$logout,{user(NULL);records(NULL);events(NULL);idx(1L);ci(1L)})
  observeEvent(input$refresh,{req(user());guard(refresh)})
  output$batch_picker<-renderUI({
    req(user(),records());choices<-sort(unique(records()$batch))
    selectInput("batch","Validation batch",choices=choices,selected=if(isolate(input$batch)%in%choices)isolate(input$batch)else choices[1])
  })
  observeEvent(input$batch,{idx(1L);ci(1L);if(!is.null(user()))guard(refresh)},ignoreInit=TRUE)
  state<-reactive({req(user(),records(),events(),input$batch);snapshot(records(),events(),input$batch)})
  observeEvent(input$section,{if(!is.null(user())&&input$section=="Conflict resolution")guard(refresh)},ignoreInit=TRUE)
  record_card<-function(r){
    doi<-r$doi
    div(class="panelbox",div(class="record-title",if(nzchar(r$title))r$title else "[No title available]"),
      div(class="meta",paste(Filter(nzchar,c(r$authors,r$year,r$journal,paste(r$volume,r$issue,r$pages))),collapse=" · ")),
      if(grepl("^10\\.[0-9]{4,9}/",doi))tags$a(href=paste0("https://doi.org/",doi),target="_blank",rel="noopener noreferrer",doi),
      h4("Abstract"),div(class="abstract",if(nzchar(r$abstract))r$abstract else "[No abstract available — assess the available title and metadata]"),
      if(nzchar(r$keywords))div(class="meta",paste("Keywords:",r$keywords)))
  }
  output$screen<-renderUI({
    s<-state();u<-user();n<-nrow(s$records);req(n>0)
    done<-sum(nzchar(s$decision[,u]));i<-min(idx(),n)
    div(class="panelbox",
      h4(sprintf("%s · %d/%d screened",input$batch,done,n)),
      div(class="progress",div(class="progress-bar",style=sprintf("width:%f%%",100*done/n))),
      p(sprintf("Neal: %d/%d · Maria: %d/%d",sum(nzchar(s$decision[,"neal"])),n,sum(nzchar(s$decision[,"maria"])),n)),
      if(s$submitted[u])p("Your independent decisions are submitted and locked.")else tagList(
        p(sprintf("Record %d of %d · Your decision: %s",i,n,if(nzchar(s$decision[i,u]))s$decision[i,u]else "not screened")),
        record_card(s$records[i,,drop=FALSE]),
        textAreaInput("note","Reviewer notes (optional)",value=s$notes[i,u],rows=2),
        div(class="actions",actionButton("include","Include (I)",class="btn-success"),actionButton("exclude","Exclude (E)",class="btn-danger"),
          actionButton("previous","← Previous"),actionButton("next","Next →"),actionButton("next_pending","Next unscreened")),
        if(done==n)actionButton("submit","Submit and lock independent decisions",class="btn-primary")
      ),
      if(!s$unlocked)p("Other reviewer decisions remain hidden until both reviewers submit this batch."))
  })
  save_screen<-function(d){
    req(user(),input$batch);u<-user();b<-input$batch
    s<-state();i<-min(idx(),nrow(s$records));id<-s$records$record_id[i];note<-input$note
    guard(function(){
      fresh<-snapshot(load_records(),load_events(),b)
      if(fresh$submitted[u]||!id%in%fresh$records$record_id)stop("Review already submitted or record unavailable.")
      events(append_event(b,id,u,"screen",d,if(is.null(note))""else note))
      idx(min(i+1L,nrow(s$records)))
    })
  }
  observeEvent(input$include,save_screen("include"))
  observeEvent(input$exclude,save_screen("exclude"))
  observeEvent(input$previous,{req(user());idx(max(1L,idx()-1L))})
  observeEvent(input$next,{req(user());idx(min(nrow(state()$records),idx()+1L))})
  observeEvent(input$next_pending,{req(user());q<-which(!nzchar(state()$decision[,user()]));if(length(q))idx(q[1])})
  observeEvent(input$submit,{
    req(user(),input$batch)
    showModal(modalDialog(title="Submit independent screening?", "Submission is irreversible. Other decisions become visible only after both reviewers submit.",
      footer=tagList(modalButton("Cancel"),actionButton("confirm_submit","Submit and lock",class="btn-primary"))))
  })
  observeEvent(input$confirm_submit,{
    req(user(),input$batch);removeModal()
    guard(function(){
      fresh<-snapshot(load_records(),load_events(),input$batch);u<-user()
      if(!nrow(fresh$records)||!all(nzchar(fresh$decision[,u]))||fresh$submitted[u])stop("Submission unavailable.")
      events(append_event(input$batch,"",u,"submit"))
    })
  })
  output$resolve<-renderUI({
    s<-state();if(!s$unlocked)return(div(class="panelbox",h4("Conflict resolution is locked"),p("Both reviewers must submit this batch first.")))
    z<-stats(s);q<-s$conflict[!nzchar(s$final[s$conflict])]
    tagList(div(class="panelbox",h4("Independent screening agreement"),
      p(sprintf("Agreement %.1f%% · Cohen's kappa %s · %d conflicts · %d unresolved",100*z["agreement"],format(round(z["kappa"],3)),length(s$conflict),length(q))),
      if(!length(q))p("All decisions resolved.")else {
        i<-q[min(ci(),length(q))]
        tagList(p(sprintf("Unresolved conflict %d of %d",min(ci(),length(q)),length(q))),
          record_card(s$records[i,,drop=FALSE]),
          p(strong("Neal: "),s$decision[i,"neal"]," · ",s$notes[i,"neal"]),
          p(strong("Maria: "),s$decision[i,"maria"]," · ",s$notes[i,"maria"]),
          textAreaInput("resolution_note","Agreed rationale (required)",rows=3),
          div(class="actions",actionButton("resolve_include","Final include",class="btn-success"),
            actionButton("resolve_exclude","Final exclude",class="btn-danger"),
            actionButton("conflict_prev","Previous conflict"),actionButton("conflict_next","Next conflict")))
      }),
      if(user()=="neal"&&!length(q))downloadButton("final_csv","Download final decisions"))
  })
  observeEvent(input$conflict_prev,{req(user());ci(max(1L,ci()-1L))})
  observeEvent(input$conflict_next,{req(user());s<-state();ci(min(sum(!nzchar(s$final[s$conflict])),ci()+1L))})
  resolve<-function(d){
    req(user(),input$batch);s<-state();req(s$unlocked)
    q<-s$conflict[!nzchar(s$final[s$conflict])];req(length(q));i<-q[min(ci(),length(q))]
    id<-s$records$record_id[i];b<-input$batch;u<-user();note<-trimws(input$resolution_note)
    if(!nzchar(note)){showNotification("Enter the agreed rationale first.",type="warning");return()}
    guard(function(){
      fresh<-snapshot(load_records(),load_events(),b);j<-match(id,fresh$records$record_id)
      if(!fresh$unlocked||is.na(j)||!j%in%fresh$conflict||nzchar(fresh$final[j]))stop("Conflict already resolved or unavailable.")
      events(append_event(b,id,u,"resolve",d,note));ci(1L)
    })
  }
  observeEvent(input$resolve_include,resolve("include"))
  observeEvent(input$resolve_exclude,resolve("exclude"))
  output$management<-renderUI({
    req(user(),records(),events())
    bs<-sort(unique(records()$batch))
    table<-if(length(bs))do.call(rbind,lapply(bs,function(b){
      s<-snapshot(records(),events(),b)
      data.frame(Batch=b,Records=nrow(s$records),Neal=sum(nzchar(s$decision[,"neal"])),Maria=sum(nzchar(s$decision[,"maria"])),
        Status=if(!s$unlocked)"Blind screening"else if(all(nzchar(s$final)))"Resolved"else "Adjudication")
    }))else data.frame(Status="No batches uploaded")
    tagList(div(class="panelbox",h4("Batch progress"),renderTable(table)),
      if(user()=="neal")div(class="panelbox",h4("Upload a batch"),selectInput("batch_name","Batch",paste("batch",1:5)),
        fileInput("ris","RIS file",accept=".ris"),actionButton("upload","Upload immutable batch",class="btn-primary")))
  })
  observeEvent(input$upload,{
    req(identical(user(),"neal"),input$ris,input$batch_name)
    guard(function(){
      d<-parse_ris(input$ris$datapath,input$batch_name);old<-load_records()
      if(any(old$batch==input$batch_name)||any(d$record_id%in%old$record_id))stop("Existing batch or duplicate record ID; upload refused.")
      sheet_append(ss,d,sheet=tabs[["records"]])
      refresh()
      got<-records()[records()$batch==input$batch_name,,drop=FALSE]
      if(nrow(got)!=200L||!setequal(got$record_id,d$record_id))stop("Upload not verified.")
      showNotification("Batch uploaded and verified.",type="message")
    })
  })
  output$final_csv<-downloadHandler(filename=function()paste0(input$batch,"_final.csv"),content=function(file){
    req(identical(user(),"neal"),input$batch)
    s<-snapshot(load_records(),load_events(),input$batch)
    req(s$unlocked,nrow(s$records)>0L,all(nzchar(s$final)))
    write.csv(data.frame(batch=input$batch,record_id=s$records$record_id,
      neal=s$decision[,"neal"],maria=s$decision[,"maria"],neal_notes=s$notes[,"neal"],maria_notes=s$notes[,"maria"],
      conflict=seq_len(nrow(s$records))%in%s$conflict,final_decision=s$final,adjudicator=s$adjudicator,
      resolution_notes=s$resolution_notes),file,row.names=FALSE)
  })
}
shinyApp(ui,server)
