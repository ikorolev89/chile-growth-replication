# Validate fresh outputs against the recorded numerical audit, not saved images.
source("scripts/load_environment.R")
suppressPackageStartupMessages({library(dplyr)})
root<-normalizePath(getwd());out<-file.path(root,"outputs/tables")
checks<-list()
check<-function(name,observed,expected,keys,columns,tol=1e-7) {
  stopifnot(!anyDuplicated(observed[keys]),!anyDuplicated(expected[keys]))
  joined<-full_join(observed,expected,by=keys,suffix=c(".new",".ref"))
  stopifnot(nrow(joined)==nrow(observed),nrow(joined)==nrow(expected))
  errors<-sapply(columns,function(c) {
    a<-joined[[paste0(c,".new")]];b<-joined[[paste0(c,".ref")]]
    if(any(!is.finite(a))||any(!is.finite(b)))return(Inf)
    max(abs(a-b))
  })
  checks[[name]]<<-data.frame(check=name,rows=nrow(joined),max_abs_difference=max(errors),
                             tolerance=tol,pass=all(errors<=tol))
}
read_audit<-function(kind,filename) {
  paths<-list.files(file.path(root,"verification/rerun",kind),pattern=paste0("^",filename,"$"),recursive=TRUE,full.names=TRUE)
  bind_rows(lapply(paths,function(p) {
    x<-read.csv(p);x[[if(kind=="simulations")"setup" else "spec"]]<-basename(dirname(p));x
  }))
}
if(file.exists(file.path(out,"main_estimates.csv"))) {
  check("main_estimates",read.csv(file.path(out,"main_estimates.csv")),read_audit("main","results.csv"),
        c("spec","dataset","year"),c("actual","SCM","GSCM","BSTSM"))
  check("SCM_weights",read.csv(file.path(out,"Tables_A5_A6_weights.csv")),read_audit("main","weights.csv"),
        c("spec","dataset","country"),"weight")
  check("factor_counts",read.csv(file.path(out,"Table_A4_factors.csv")),read_audit("main","diagnostics.csv"),
        c("spec","dataset"),"factors",0)
}
if(file.exists(file.path(out,"Table_A3_placebo_mse.csv")))
  check("placebo_MSE",read.csv(file.path(out,"Table_A3_placebo_mse.csv")),read_audit("placebo","results.csv"),
        c("spec","dataset","method"),"mse_x1000")
if(file.exists(file.path(out,"Table_A1_simulation_mse.csv"))) {
  check("simulation_MSE",read.csv(file.path(out,"Table_A1_simulation_mse.csv")),read_audit("simulations","mse.csv"),
        c("setup","method","n"),"mse_x1000",1e-6)
  check("simulation_factors",read.csv(file.path(out,"Table_A2_simulation_factors.csv")),read_audit("simulations","factors.csv"),
        c("setup","n"),c("mean","variance"),1e-8)
}
stmfiles<-list.files(file.path(root,"outputs/stm"),pattern="^estimates.csv$",recursive=TRUE,full.names=TRUE)
if(length(stmfiles)) {
  actual<-bind_rows(lapply(stmfiles,function(p){x<-read.csv(p);x$spec<-basename(dirname(p));x}))
  check("STSM_matches_audit",actual,read_audit("stm","results.csv"),c("spec","dataset","year"),c("actual","STSM"),1e-6)
}
if(!length(checks))stop("No fresh outputs found. Run run.R first.")
results<-bind_rows(checks)
print(results)
write.csv(results,file.path(root,"outputs/verification.csv"),row.names=FALSE)
if(any(!results$pass))stop("Some outputs differ from the audited numerical baseline")
cat("All available fresh outputs match the audit baseline.\n")
cat("This does not erase the published Table A6 omission or the STSM/published-curve differences; see REPLICATION_NOTES.md.\n")
