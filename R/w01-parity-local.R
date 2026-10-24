# Offline W01 calculation parity trial on real, previously archived W01 signatures.
# Does not alter candidate selection or any production W01 artefacts.
# Run with: evidencelandscaper::trial_w01_qgram_local(map=map,run_name="forestcommodities_sustainability")
#' Compare original and disk-backed W01 q-gram candidates on actual records
#' @param map Existing evidence-map directory.
#' @param run_name Existing named run.
#' @param n Maximum source manifestations to sample, default 500.
#' @param seed Reproducible sample seed.
#' @return Comparison details; stops on mismatch.
#' @export
trial_w01_qgram_local <- function(map="Ostara_collective_action_map",
                                  run_name="forestcommodities_sustainability",
                                  n=500L,seed=20261024L) {
  map<-open_map(map)
  if(!requireNamespace("data.table",quietly=TRUE)||
     !requireNamespace("DBI",quietly=TRUE)||
     !requireNamespace("RSQLite",quietly=TRUE))
    stop("Install local packages data.table, DBI, and RSQLite.")
  home<-file.path(map,"runs","orchestrated",run_name)
  statefile<-file.path(home,"state.json")
  if(!file.exists(statefile))stop("Existing run state missing.")
  state<-jsonlite::fromJSON(statefile,simplifyVector=FALSE)
  prep<-if(is.list(state$w01))state$w01$prepared_manifest else NULL
  if(is.null(prep)||!file.exists(prep))
    stop("W01 preparation manifest unavailable in saved run.")
  root<-dirname(prep)
  candidates<-list.dirs(root,recursive=FALSE,full.names=TRUE)
  candidates<-candidates[grepl("^candidates($|-interrupted-)",basename(candidates))]
  candidates<-candidates[
    file.exists(file.path(candidates,"title_qgram_signatures.csv")) &
    file.exists(file.path(candidates,"normalised_metadata.csv"))]
  if(!length(candidates))stop("No archived title signatures and normalised metadata found.")
  source<-candidates[[which.max(file.info(
    file.path(candidates,"title_qgram_signatures.csv"))$mtime)]]
  sig<-data.table::fread(file.path(source,"title_qgram_signatures.csv"),
                         colClasses=list(integer="idx",character="qgram"))
  meta<-data.table::fread(file.path(source,"normalised_metadata.csv"),
                          select=c("idx","title_norm"),colClasses=list(integer="idx",character="title_norm"))
  ids<-sort(unique(sig$idx))
  n<-as.integer(n)
  if(is.na(n)||n<2L)stop("n must be at least 2.")
  if(length(ids)<2L)stop("Not enough signatures for parity trial.")
  set.seed(seed)
  chosen<-sort(sample(ids,min(n,length(ids))))
  sig<-sig[sig$idx %in% chosen, ]
  lengths<-setNames(nchar(meta$title_norm,type="chars"),as.character(meta$idx))
  message("[W01 parity] ",length(chosen)," real source records; ",nrow(sig),
          " archived rare q-gram signatures.")
  old<-data.table::copy(sig)
  data.table::setkeyv(old,"qgram")
  t_original<-system.time({
    joined<-old[old,allow.cartesian=TRUE,nomatch=0L]
    joined<-joined[joined$idx<joined$i.idx, ]
    # Explicitly name the columns before aggregating to avoid data.table
    # expression scoping ambiguities in the installed package namespace.
    grouped<-data.table::data.table(
      i=as.integer(joined$idx),j=as.integer(joined$i.idx))
    expected<-grouped[,list(shared_rare_qgrams=.N),by=c("i","j")]
    expected<-expected[expected$shared_rare_qgrams>=2L, ]
    ratio<-pmin(lengths[as.character(expected$i)],lengths[as.character(expected$j)]) /
      pmax(lengths[as.character(expected$i)],lengths[as.character(expected$j)])
    expected<-expected[is.finite(ratio)&ratio>=0.75, ]
    old_keys<-sort(paste(expected$i,expected$j,sep="::"))
  })
  dbfile<-tempfile(pattern="w01-parity-",fileext=".sqlite")
  con<-DBI::dbConnect(RSQLite::SQLite(),dbfile)
  on.exit({try(DBI::dbDisconnect(con),silent=TRUE);
           unlink(c(dbfile,paste0(dbfile,c("-wal","-shm"))))},add=TRUE)
  DBI::dbExecute(con,
    "CREATE TABLE overlaps(i INTEGER NOT NULL,j INTEGER NOT NULL,n INTEGER NOT NULL,PRIMARY KEY(i,j)) WITHOUT ROWID")
  insert<-DBI::dbSendStatement(con,
    "INSERT INTO overlaps VALUES(?,?,1) ON CONFLICT(i,j) DO UPDATE SET n=MIN(2,n+1)")
  t_disk<-system.time({
    for(v in split(sig$idx,sig$qgram)) {
      v<-sort(unique(v))
      if(length(v)<2L)next
      for(k in seq_len(length(v)-1L)) {
        i<-v[[k]]
        js<-v[seq.int(k+1L,length(v))]
        ratio<-pmin(lengths[as.character(i)],lengths[as.character(js)]) /
          pmax(lengths[as.character(i)],lengths[as.character(js)])
        js<-js[is.finite(ratio)&ratio>=0.75]
        if(length(js))DBI::dbBind(insert,list(rep.int(i,length(js)),as.integer(js)))
      }
    }
    DBI::dbClearResult(insert)
    actual<-DBI::dbGetQuery(con,
      "SELECT i,j FROM overlaps WHERE n>=2 ORDER BY i,j")
    disk_keys<-sort(paste(actual$i,actual$j,sep="::"))
  })
  identical_results<-identical(old_keys,disk_keys)
  result<-list(source=source,records=length(chosen),signatures=nrow(sig),
               original_pairs=length(old_keys),disk_pairs=length(disk_keys),
               equivalent=identical_results,original_seconds=unname(t_original[["elapsed"]]),
               disk_seconds=unname(t_disk[["elapsed"]]))
  print(result)
  if(!identical_results)stop("STOP: disk W01 does not match original candidate selection.")
  invisible(result)
}
