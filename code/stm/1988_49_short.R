
# install.packages("devtools")
# devtools::install_github("ebenmichael/augsynth")


#library(synthdid)




chile_temp <- read.csv("chile_data_wb.csv", header = TRUE)
chile_mad_temp <- read.csv("chile_data_mad.csv", header = TRUE)

country_list_esc_17 <- read.csv("country_codes_esc_17.csv",header = TRUE)
country_list_esc_25 <- read.csv("country_codes_esc_25.csv",header = TRUE)
country_list_latam_25 <- read.csv("country_codes_latam_25.csv",header = TRUE)
country_list_49 <- read.csv("country_codes_49.csv",header = TRUE)

list_esc_17 <- country_list_esc_17[[2]]
list_esc_25 <- country_list_esc_25[[2]]
list_latam_25 <- country_list_latam_25[[2]]
list_49 <- country_list_49[[2]]


# data preparation

chile <- chile_temp[ which( chile_temp$year >= 1960 & chile_temp$year <= 2014) ,]
chile_mad <- chile_mad_temp[ which( chile_mad_temp$year >= 1951 & chile_mad_temp$year <= 2014) ,]



chile <- chile |> 
  group_by(CountryCode) |>
  filter(!any(is.na(c(lnGDP_pc)))) |>
  ungroup()


chile <- chile %>%
  group_by(CountryCode) %>%
  mutate(CountryID = cur_group_id())





chile_mad <- chile_mad |> 
  group_by(CountryCode) |>
  filter(!any(is.na(c(lnGDP_pc)))) |>
  ungroup()


chile_mad <- chile_mad %>%
  group_by(CountryCode) %>%
  mutate(CountryID = cur_group_id())






# 1988


chile_1988 <- chile[ which( chile$year >= 1975 & chile$year <= 2014) ,]
chile_mad_1988 <- chile_mad[ which( chile_mad$year >= 1975 & chile_mad$year <= 2014) ,]

Y_actual_1988 <- chile_1988$lnGDP_pc[ which(chile_1988$CountryName == "Chile")]
Y_actual_mad_1988 <- chile_mad_1988$lnGDP_pc[ which(chile_mad_1988$CountryName == "Chile")]

# demean WB data

chile_1988 <- chile_1988 |>
  # Calculate mean by group
  group_by(CountryID) |>
  mutate(lnGDP_pc_mean = mean(lnGDP_pc[year < 1988])) |> 
  as.data.frame()

chile_1988 <- chile_1988 |>
  mutate(lnGDP_pc_dm = lnGDP_pc - lnGDP_pc_mean) |> 
  as.data.frame()

# demean mad data

chile_mad_1988 <- chile_mad_1988 |>
  # Calculate mean by group
  group_by(CountryID) |>
  mutate(lnGDP_pc_mean = mean(lnGDP_pc[year < 1988])) |> 
  as.data.frame()

chile_mad_1988 <- chile_mad_1988 |>
  mutate(lnGDP_pc_dm = lnGDP_pc - lnGDP_pc_mean) |> 
  as.data.frame()


# SCM


# Escalante, 17 countries

chile_1988_49 <- chile_1988 |> filter(CountryCode %in% list_49)

chile_1988_49$Treat <- 0
chile_1988_49$Treat[ which(chile_1988_49$CountryName == "Chile" & chile_1988_49$year >= 1988) ] <- 1


T_0_49 <- sum(1-chile_1988_49$Treat[ which(chile_1988_49$CountryName == "Chile" ) ])
T_all_49 <- length(chile_1988_49$Treat[ which(chile_1988_49$CountryName == "Chile" ) ])


chile_syn_1988_49 <- augsynth(lnGDP_pc ~ Treat, CountryName, year, chile_1988_49,
                                  progfunc = "None", scm = T)


chile_syn_1988_49_dm <- augsynth(lnGDP_pc_dm ~ Treat, CountryName, year, chile_1988_49,
                                     progfunc = "None", scm = T)




chile_mad_1988_49 <- chile_mad_1988 |> filter(CountryCode %in% list_49)

chile_mad_1988_49$Treat <- 0
chile_mad_1988_49$Treat[ which(chile_mad_1988_49$CountryName == "Chile" & chile_mad_1988_49$year >= 1988) ] <- 1

T_0_mad_49 <- sum(1-chile_mad_1988_49$Treat[ which(chile_mad_1988_49$CountryName == "Chile" ) ])
T_all_mad_49 <- length(chile_mad_1988_49$Treat[ which(chile_mad_1988_49$CountryName == "Chile" ) ])


chile_syn_mad_1988_49 <- augsynth(lnGDP_pc ~ Treat, CountryName, year, chile_mad_1988_49,
                                      progfunc = "None", scm = T)


chile_syn_mad_1988_49_dm <- augsynth(lnGDP_pc_dm ~ Treat, CountryName, year, chile_mad_1988_49,
                                         progfunc = "None", scm = T)



round(chile_syn_1988_49$weights,3)
round(chile_syn_1988_49_dm$weights,3)


round(chile_syn_mad_1988_49$weights,3)
round(chile_syn_mad_1988_49_dm$weights,3)


Y_synth_1988_49_dm <- chile_1988_49$lnGDP_pc_mean[1] + chile_syn_1988_49_dm$data$synth_data$Y0plot %*% chile_syn_1988_49_dm$weights
Y_synth_mad_1988_49_dm <- chile_mad_1988_49$lnGDP_pc_mean[1] + chile_syn_mad_1988_49_dm$data$synth_data$Y0plot %*% chile_syn_mad_1988_49_dm$weights


# GSynth

out_49 <- fect(lnGDP_pc ~ Treat, data = chile_1988_49, index = c("CountryCode","year"), 
                   method = "gsynth", force = "two-way", CV = TRUE, r = c(0, 5), 
                   se = FALSE, nboots = 1000, vartype = 'parametric', 
                   parallel = FALSE)

out_mad_49 <- fect(lnGDP_pc ~ Treat, data = chile_mad_1988_49, index = c("CountryCode","year"), 
                       method = "gsynth", force = "two-way", CV = TRUE, r = c(0, 5), 
                       se = FALSE, nboots = 1000, vartype = 'parametric', 
                       parallel = FALSE)


Y_gsynth_49 <- Y_actual_1988-out_49$att
Y_gsynth_mad_49 <- Y_actual_mad_1988-out_mad_49$att



# CausalImpact


data_temp_49 <- chile_1988_49 |> dplyr::select(CountryCode,year,lnGDP_pc)

data1_49 <- data_temp_49 %>% 
  pivot_wider(names_from = CountryCode, values_from = lnGDP_pc)

data_49 <- as.matrix(data1_49[,2:ncol(data1_49)])

#matplot(data_49, type = "l")

pre.period_49 <- c(1, T_0_49)
post.period_49 <- c(T_0_49+1, T_all_49)

impact_49 <- CausalImpact(data_49, pre.period_49, post.period_49)

#plot(impact_49, c("original", "pointwise"))



data_temp_mad_49 <- chile_mad_1988_49 |> dplyr::select(CountryCode,year,lnGDP_pc)

data1_mad_49 <- data_temp_mad_49 %>% 
  pivot_wider(names_from = CountryCode, values_from = lnGDP_pc)

data_mad_49 <- as.matrix(data1_mad_49[,2:ncol(data1_mad_49)])

#matplot(data_mad_49, type = "l")

pre.period_mad_49 <- c(1, T_0_mad_49)
post.period_mad_49 <- c(T_0_mad_49+1, T_all_mad_49)

impact_mad_49 <- CausalImpact(data_mad_49, pre.period_mad_49, post.period_mad_49)


Y_impact_49 <- impact_49$series$point.pred
Y_impact_mad_49 <- impact_mad_49$series$point.pred


# STM

load("chile_wb.RData")
load("chile_mad.RData")
source("R_functions.R")

# World Bank

Data <- as.data.frame(chile_wb)

Data <- Data[ which(Data$year >= 1975 & Data$year <= 2014) ,]

Time <- as.integer(Data[,1])

Data <- Data[ , names(Data) %in% list_49]


intervention <- 1988
intm1 <- intervention-1

t.i<-which(Time==intm1)

#list2env(dummies,envir=globalenv())

#********************
#*step 1: choosing the control group:
#*test for cointegration in sample, choosing different starting points as the 1970's are very "turbulent" times
#*visual analysis
#********************

#starting for full sample in t=1970
#only UK is found as control, stepwise increasing t by one year leads to choose 1976 as starting date
chl <- Data$CHL
cointsearch(chl,Data,1,t.i,0.05,4,0.002,log=TRUE)


#potential candidates:
ynames<-c("CHL","MEX","BLZ","COL","BRA")
y<-ts(Data[,match(ynames,colnames(Data))],start=Time[1],f=1)
colour<-c("red","green","orange","cyan")

#visual und descriptive analysis
#original data - pre 1999 mean adjusted
plot.orig.ma(y,colour,t.i,vline=c(intervention),ynames,png.out=F,png.name="Fig1a")

#first difference - mean adjusted
plot.diff.ma(y,colour,t.i,vline=c(intervention),ynames,png.out=F,png.name="Fig1b")



y.1NA<-y
y.1NA[,1]<-NA
y.1NA[1:t.i,1]<-y[1:t.i,1]

#kpss-Tests for stationarity
LT.kpss.test(y,t.i,ynames)
#export(LT.kpss.test(y,t.i,ynames),"table3.xlsx",overwrite=T,row.names=T,col.names=T)


#*******************
#*Step 2: naive model
#*******************
reg<-NULL
fit<-est.fn(y.1NA,reg,t.i)
# model visual output
plot.model.output(fit,colour,"pred.target",vline=intervention,ynames,png.out=F,png.name="Fig2a")

#plot of y1-common trend
stats<-model.stats(fit,y)
plot.gap(stats$gap,ynames,vline=intervention,png.out=F,png.name="Fig2c")


#export(stats$gap.round,"Fig2d.xlsx",row.names=T)

Y_stm_49 <- y[,1] - stats$gap


# Maddison

Data <- as.data.frame(chile_mad)

Data <- Data[ which(Data$year >= 1975 & Data$year <= 2014) ,]

Time <- as.integer(Data[,1])

Data <- Data[ , names(Data) %in% list_49]


intervention <- 1988
intm1 <- intervention-1

t.i<-which(Time==intm1)

#list2env(dummies,envir=globalenv())

#********************
#*step 1: choosing the control group:
#*test for cointegration in sample, choosing different starting points as the 1970's are very "turbulent" times
#*visual analysis
#********************

#starting for full sample in t=1970
#only UK is found as control, stepwise increasing t by one year leads to choose 1976 as starting date
chl <- Data$CHL
cointsearch(chl,Data,1,t.i,0.05,4,0.002,log=TRUE)


#potential candidates:
ynames<-c("CHL","MEX","COL","BRA")
y<-ts(Data[,match(ynames,colnames(Data))],start=Time[1],f=1)
colour<-c("red","green","orange","cyan")

#visual und descriptive analysis
#original data - pre 1999 mean adjusted
plot.orig.ma(y,colour,t.i,vline=c(intervention),ynames,png.out=F,png.name="Fig1a")

#first difference - mean adjusted
plot.diff.ma(y,colour,t.i,vline=c(intervention),ynames,png.out=F,png.name="Fig1b")



y.1NA<-y
y.1NA[,1]<-NA
y.1NA[1:t.i,1]<-y[1:t.i,1]

#kpss-Tests for stationarity
LT.kpss.test(y,t.i,ynames)
#export(LT.kpss.test(y,t.i,ynames),"table3.xlsx",overwrite=T,row.names=T,col.names=T)


#*******************
#*Step 2: naive model
#*******************
reg<-NULL
fit<-est.fn(y.1NA,reg,t.i)
# model visual output
plot.model.output(fit,colour,"pred.target",vline=intervention,ynames,png.out=F,png.name="Fig2a")

#plot of y1-common trend
stats<-model.stats(fit,y)
plot.gap(stats$gap,ynames,vline=intervention,png.out=F,png.name="Fig2c")


#export(stats$gap.round,"Fig2d.xlsx",row.names=T)

Y_stm_mad_49 <- y[,1] - stats$gap



### plots



dates <- unique(chile_1988$year)


plot_data <- data.frame(dates,Y_actual_1988,Y_synth_1988_49_dm,
                        Y_gsynth_49,Y_impact_49,Y_stm_49)


ggplot(plot_data) +
  geom_line(aes(x = dates, y = Y_actual_1988, linetype = "0 actual", colour = "0 actual"),linewidth=.5) + 
  geom_line(aes(x = dates, y = Y_synth_1988_49_dm, linetype = "1 Synth", colour = "1 Synth"),linewidth=.65) + 
  geom_line(aes(x = dates, y = Y_gsynth_49, linetype = "2 GSynth", colour = "2 GSynth"),linewidth=.65) + 
  geom_line(aes(x = dates, y = Y_impact_49, linetype = "3 Impact", colour = "3 Impact"),linewidth=.65) + 
  geom_vline(xintercept = 1988,linetype="dotted",color = "black") +
  #ylim(9.5,10.5) +
  theme_bw() +
  theme(legend.position="right") +
  theme(legend.position="none") +
  scale_linetype_manual(values = c("solid","longdash","dotdash","twodash")) +
  scale_colour_manual(values = c("black","red","blue","orange")) +  labs(x = "year") + labs(y = "ln gdp")


ggsave("chile_wb_1988_49.eps", width = 5, height = 3)


ggplot(plot_data) +
  geom_line(aes(x = dates, y = Y_actual_1988, linetype = "0 actual", colour = "0 actual"),linewidth=.5) + 
  geom_line(aes(x = dates, y = Y_synth_1988_49_dm, linetype = "1 Synth", colour = "1 Synth"),linewidth=.65) + 
  geom_line(aes(x = dates, y = Y_gsynth_49, linetype = "2 GSynth", colour = "2 GSynth"),linewidth=.65) + 
  geom_line(aes(x = dates, y = Y_impact_49, linetype = "3 Impact", colour = "3 Impact"),linewidth=.65) + 
  geom_line(aes(x = dates, y = Y_stm_49, linetype = "4 STM", colour = "4 STM"),linewidth=.65) + 
  geom_vline(xintercept = 1988,linetype="dotted",color = "black") +
  #ylim(9.5,10.5) +
  theme_bw() +
  theme(legend.position="right") +
  theme(legend.position="none") +
  scale_linetype_manual(values = c("solid","longdash","dotdash","twodash","dashed")) +
  scale_colour_manual(values = c("black","red","blue","orange","magenta")) +  labs(x = "year") + labs(y = "ln gdp")


ggsave("chile_wb_1988_49_all.eps", width = 5, height = 3)


dates_mad <- unique(chile_mad_1988$year)


plot_data_mad <- data.frame(dates_mad,Y_actual_mad_1988,Y_synth_mad_1988_49_dm,
                            Y_gsynth_mad_49,Y_impact_mad_49,Y_stm_mad_49)


ggplot(plot_data_mad) +
  geom_line(aes(x = dates_mad, y = Y_actual_mad_1988, linetype = "0 actual", colour = "0 actual"),linewidth=.5) + 
  geom_line(aes(x = dates_mad, y = Y_synth_mad_1988_49_dm, linetype = "1 Synth", colour = "1 Synth"),linewidth=.65) + 
  geom_line(aes(x = dates_mad, y = Y_gsynth_mad_49, linetype = "2 GSynth", colour = "2 GSynth"),linewidth=.65) + 
  geom_line(aes(x = dates_mad, y = Y_impact_mad_49, linetype = "3 Impact", colour = "3 Impact"),linewidth=.65) + 
  geom_vline(xintercept = 1988,linetype="dotted",color = "black") +
  #ylim(9.5,10.5) +
  theme_bw() +
  theme(legend.position="right") +
  theme(legend.position="none") +
  scale_linetype_manual(values = c("solid","longdash","dotdash","twodash")) +
  scale_colour_manual(values = c("black","red","blue","orange")) +  labs(x = "year") + labs(y = "ln gdp")

ggsave("chile_mad_1988_49.eps", width = 5, height = 3)



ggplot(plot_data_mad) +
  geom_line(aes(x = dates_mad, y = Y_actual_mad_1988, linetype = "0 actual", colour = "0 actual"),linewidth=.5) + 
  geom_line(aes(x = dates_mad, y = Y_synth_mad_1988_49_dm, linetype = "1 Synth", colour = "1 Synth"),linewidth=.65) + 
  geom_line(aes(x = dates_mad, y = Y_gsynth_mad_49, linetype = "2 GSynth", colour = "2 GSynth"),linewidth=.65) + 
  geom_line(aes(x = dates_mad, y = Y_impact_mad_49, linetype = "3 Impact", colour = "3 Impact"),linewidth=.65) + 
  geom_line(aes(x = dates_mad, y = Y_stm_mad_49, linetype = "4 STM", colour = "4 STM"),linewidth=.65) + 
  geom_vline(xintercept = 1988,linetype="dotted",color = "black") +
  #ylim(9.5,10.5) +
  theme_bw() +
  theme(legend.position="right") +
  theme(legend.position="none") +
  scale_linetype_manual(values = c("solid","longdash","dotdash","twodash","dashed")) +
  scale_colour_manual(values = c("black","red","blue","orange","magenta")) +  labs(x = "year") + labs(y = "ln gdp")

ggsave("chile_mad_1988_49_all.eps", width = 5, height = 3)
