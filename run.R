# Run from the archive root: Rscript --vanilla run.R all
source("scripts/load_environment.R")
args <- commandArgs(trailingOnly=TRUE)
stage <- if(length(args)) args[1] else "all"
stopifnot(stage %in% c("main","placebo","simulations","stm","all"))
needed <- c("dplyr","tidyr","ggplot2","augsynth","fect","CausalImpact","matrixStats")
if(stage %in% c("stm","all")) needed<-c(needed,"tseries","dlm","KFAS","mFilter","numDeriv")
for(p in needed) if(!requireNamespace(p,quietly=TRUE))stop("Missing ",p,". Run Rscript --vanilla scripts/setup.R first.")
if(packageVersion("fect") != "2.0.5")warning(
  "Using fect ",packageVersion("fect"),". For reproduction of the published results, use fect 2.0.5. ",
  "Other versions can change GSCM results substantially; verification against the replication baseline may fail.",
  immediate.=TRUE,call.=FALSE)
suppressPackageStartupMessages(for(p in needed)library(p,character.only=TRUE))
out <- file.path(root,"outputs");dir.create(out,showWarnings=FALSE)
for(d in c("tables","figures","objects","logs"))dir.create(file.path(out,d),showWarnings=FALSE)
main_objects <- list(); estimates<-list(); weights<-list(); diagnostics<-list()
specs<-data.frame(year=rep(c(1973,1988,1988),each=3),
                 pool=rep(c("esc17","esc25","49"),3),
                 period=rep(c("short","short","long"),each=3))
specs$id<-with(specs,paste(year,pool,period,sep="_"))
load_main <- function(i) {
  id<-specs$id[i];e<-new.env(parent=globalenv())
  setwd(file.path(root,"data"));set.seed(123)
  source(file.path(root,"code/main",paste0(id,".R")),local=e)
  setwd(root)
  e
}
export_main <- function(e,i) {
  s<-sub("esc","esc_",specs$pool[i]);yr<-specs$year[i];id<-specs$id[i]
  vals<-wts<-diag<-list()
  for(ds in c("wb","mad")) {
    tag<-if(ds=="mad")"mad_" else ""
    g<-function(n)get(n,e,inherits=FALSE)
    data<-g(paste0("chile_",tag,yr,"_",s));actual<-g(paste0("Y_actual_",tag,yr))
    scm<-g(paste0("chile_syn_",tag,yr,"_",s,"_dm"))
    gs<-g(paste0("out_",tag,s));ci<-g(paste0("impact_",tag,s))
    years<-data$year[data$CountryCode=="CHL"]
    stopifnot(unique(data$CountryCode)[1]=="CHL",abs(sum(scm$weights)-1)<1e-6)
    vals[[ds]]<-data.frame(spec=id,dataset=ds,year=years,actual,
      SCM=as.numeric(g(paste0("Y_synth_",tag,yr,"_",s,"_dm"))),
      GSCM=as.numeric(actual-gs$att),BSTSM=as.numeric(ci$series$point.pred))
    wts[[ds]]<-data.frame(spec=id,dataset=ds,country=rownames(scm$weights),weight=as.numeric(scm$weights))
    diag[[ds]]<-data.frame(spec=id,dataset=ds,donors=length(unique(data$CountryCode))-1,
                          first_year=min(years),last_year=max(years),factors=gs$r.cv)
  }
  list(values=bind_rows(vals),weights=bind_rows(wts),diagnostics=bind_rows(diag))
}
plot_main <- function(values) {
  for(k in 1:3) {
    ids<-specs$id[((k-1)*3+1):(k*3)]
    d<-values |> filter(spec %in% ids) |> mutate(pool=factor(spec,levels=ids),
      dataset=factor(dataset,levels=c("wb","mad"),labels=c("World Bank","Maddison"))) |>
      pivot_longer(c(actual,SCM,GSCM,BSTSM),names_to="method",values_to="log_gdp")
    p<-ggplot(d,aes(year,log_gdp,color=method,linetype=method))+geom_line(linewidth=.5)+
      geom_vline(xintercept=if(k==1)1973 else 1988,linetype="dotted")+
      facet_grid(pool~dataset,scales="free")+theme_bw()+
      scale_color_manual(values=c(actual="black",SCM="red",GSCM="blue",BSTSM="orange"))+
      scale_linetype_manual(values=c(actual="solid",SCM="longdash",GSCM="dotdash",BSTSM="twodash"))+
      labs(x="Year",y="Log real GDP per capita",color=NULL,linetype=NULL)+theme(legend.position="bottom")
    ggsave(file.path(out,"figures",paste0("Figure_",k,".pdf")),p,width=10,height=9)
    ggsave(file.path(out,"figures",paste0("Figure_",k,".png")),p,width=10,height=9,dpi=120)
  }
}
run_logged <- function(label,fn) {
  cat("Running",label,"\n")
  log<-file(file.path(out,"logs",paste0(label,".log")),"wt")
  sink(log);sink(log,type="message")
  on.exit({sink(type="message");sink();close(log);setwd(root)})
  fn()
}
t0<-proc.time()
if(stage %in% c("main","placebo","all")) {
  for(i in seq_len(nrow(specs))) {
    id<-specs$id[i]
    e<-run_logged(paste0("main_",id),function()load_main(i))
    main_objects[[id]]<-e
    exp<-export_main(e,i);estimates[[id]]<-exp$values;weights[[id]]<-exp$weights;diagnostics[[id]]<-exp$diagnostics
    saveRDS(as.list(e),file.path(out,"objects",paste0("main_",id,".rds")))
  }
  write.csv(bind_rows(estimates),file.path(out,"tables/main_estimates.csv"),row.names=FALSE)
  write.csv(bind_rows(weights),file.path(out,"tables/Tables_A5_A6_weights.csv"),row.names=FALSE)
  write.csv(bind_rows(diagnostics),file.path(out,"tables/Table_A4_factors.csv"),row.names=FALSE)
  plot_main(bind_rows(estimates))
  source(file.path(root,"scripts/appendix_figures.R"),local=TRUE)
}
if(stage %in% c("placebo","all")) {
  rows<-list()
  for(i in seq_len(nrow(specs))) {
    id<-specs$id[i];s<-sub("esc","esc_",specs$pool[i]);yr<-specs$year[i]
    e<-new.env(parent=globalenv())
    run_logged(paste0("placebo_",id),function() {
      setwd(file.path(root,"data"));set.seed(123)
      source(file.path(root,"code/placebo",paste0(id,".R")),local=e)
      for(ds in c("wb","mad")) {
        tag<-if(ds=="mad")"mad_" else ""
        T0<-get(paste0("T_0_",tag,s),e);TT<-get(paste0("T_all_",tag,s),e)
        for(m in c("dm","gsynth","CI")) {
          gaps<-get(paste0("gaps_",tag,yr,"_",s,"_",m),e)
          rows[[paste(id,ds,m)]]<<-data.frame(spec=id,dataset=ds,method=m,mse_x1000=1000*mean(gaps[(T0+1):TT,,drop=FALSE]^2))
        }
      }
      saveRDS(as.list(e),file.path(out,"objects",paste0("placebo_",id,".rds")))
      # Plot helpers require both Chile and placebo objects. Recombine explicitly.
      pe<-list2env(as.list(e),parent=main_objects[[id]])
      setwd(file.path(out,"figures"))
      plotfile<-paste0("plots_placebo_",yr,"_",specs$pool[i],if(specs$period[i]=="long")"_1" else "",".R")
      source(file.path(root,"code/placebo",plotfile),local=pe)
    })
  }
  write.csv(bind_rows(rows),file.path(out,"tables/Table_A3_placebo_mse.csv"),row.names=FALSE)
}
if(stage %in% c("simulations","all")) {
  mses<-factors<-list()
  for(id in c("20_const","20_incr","40_const","40_incr")) {
    e<-new.env(parent=globalenv())
    run_logged(paste0("simulation_",id),function()source(file.path(root,"code/simulations",paste0(id,".R")),local=e))
    mses[[id]]<-data.frame(setup=id,method=rep(c("SCM","GSCM","BSTSM"),each=4),n=rep(e$list_n,3),mse_x1000=1000*as.vector(t(e$V)))
    factors[[id]]<-data.frame(setup=id,n=e$list_n,mean=colMeans(e$n_factors),variance=colVars(e$n_factors))
    saveRDS(as.list(e),file.path(out,"objects",paste0("simulation_",id,".rds")))
  }
  write.csv(bind_rows(mses),file.path(out,"tables/Table_A1_simulation_mse.csv"),row.names=FALSE)
  write.csv(bind_rows(factors),file.path(out,"tables/Table_A2_simulation_factors.csv"),row.names=FALSE)
}
if(stage %in% c("stm","all")) {
  for(i in seq_len(nrow(specs))) {
    id<-specs$id[i];e<-new.env(parent=globalenv())
    e$source<-function(file,...)sys.source(file,envir=e)
    wd<-file.path(out,"stm",id);dir.create(wd,recursive=TRUE,showWarnings=FALSE)
    file.copy(list.files(file.path(root,"data"),pattern="\\.csv$",full.names=TRUE),wd,overwrite=TRUE)
    file.copy(file.path(root,"code/stm/R_functions.R"),wd,overwrite=TRUE)
    for(ds in c("wb","mad")) {
      dat<-read.csv(file.path(root,"data",paste0("chile_data_",ds,".csv"))) |>
        select(year,CountryCode,lnGDP_pc) |> pivot_wider(names_from=CountryCode,values_from=lnGDP_pc)
      name<-paste0("chile_",ds);we<-new.env();assign(name,dat,we)
      save(list=name,envir=we,file=file.path(wd,paste0(name,".RData")))
    }
    run_logged(paste0("stm_",id),function() {
      setwd(wd);set.seed(123);pdf("diagnostic_plots.pdf")
      on.exit(dev.off())
      source(file.path(root,"code/stm",paste0(id,".R")),local=e)
      s<-sub("esc","esc_",specs$pool[i]);yr<-specs$year[i]
      result<-bind_rows(lapply(c("wb","mad"),function(ds) {
        tag<-if(ds=="mad")"mad_" else ""
        data.frame(dataset=ds,year=get(if(ds=="wb")"dates" else "dates_mad",e),
          actual=get(paste0("Y_actual_",tag,yr),e),STSM=as.numeric(get(paste0("Y_stm_",tag,s),e)))
      }))
      write.csv(result,"estimates.csv",row.names=FALSE)
    })
  }
}
setwd(root)
writeLines(capture.output(sessionInfo()),file.path(out,"sessionInfo.txt"))
cat("Completed",stage,"in",round((proc.time()-t0)[["elapsed"]],1),"seconds. See outputs/.\n")
