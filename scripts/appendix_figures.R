# A1: donor histories; A2-A5: the corresponding estimated factor loadings.
histogram_specs<-c("1973_esc17_short","1973_49_short","1988_esc17_short","1988_49_short")
for(j in seq_along(histogram_specs)) {
  id<-histogram_specs[j];pool<-strsplit(id,"_")[[1]][2];s<-sub("esc","esc_",pool)
  fit<-get(paste0("out_",s),main_objects[[id]])
  r<-fit$r.cv
  dd<-bind_rows(lapply(seq_len(r),function(k)data.frame(factor=k,loading=fit$lambda.co[,k])))
  treated<-data.frame(factor=seq_len(r),loading=as.numeric(fit$lambda.tr))
  p<-ggplot(dd,aes(loading))+geom_histogram(bins=10,fill="lightblue",color="black")+
    geom_vline(data=treated,aes(xintercept=loading),color="red",linewidth=1)+
    facet_wrap(~factor,scales="free")+theme_bw()+labs(x="Factor loading",y="Count")
  ggsave(file.path(out,"figures",paste0("Figure_A",j+1,".pdf")),p,width=8,height=if(r>2)7 else 4)
}
background<-list()
for(pool in c("esc17","esc25","49")) {
  e<-main_objects[[paste0("1988_",pool,"_long")]];s<-sub("esc","esc_",pool)
  for(ds in c("wb","mad")) {
    tag<-if(ds=="mad")"mad_" else ""
    dat<-get(paste0("chile_",tag,"1988_",s),e) |> mutate(pool=pool,dataset=ds)
    background[[paste(pool,ds)]]<-dat
  }
}
d<-bind_rows(background) |> mutate(pool=factor(pool,levels=c("esc17","esc25","49")),
                                  dataset=factor(dataset,levels=c("wb","mad")))
p<-ggplot(d,aes(year,lnGDP_pc,group=CountryCode))+
  geom_line(color="grey70",linewidth=.25)+
  geom_line(data=filter(d,CountryCode=="CHL"),color="black",linewidth=.7)+
  geom_vline(xintercept=c(1973,1988),linetype="dashed",color="blue")+
  facet_grid(pool~dataset,scales="free")+theme_bw()+labs(x="Year",y="Log real GDP per capita")
ggsave(file.path(out,"figures","Figure_A1.pdf"),p,width=10,height=9)
# A6's original six illustration assets are preserved under reference/figures.
# Their precise generation/save commands were absent from the source folder.
