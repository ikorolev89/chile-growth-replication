
# install.packages("devtools")
# devtools::install_github("ebenmichael/augsynth")


#library(synthdid)




chile_temp <- read.csv("chile_data_wb.csv", header = TRUE)
chile_mad_temp <- read.csv("chile_data_mad.csv", header = TRUE)

country_list_esc_17 <- read.csv("country_codes_esc_17.csv",header = TRUE)
country_list_49 <- read.csv("country_codes_49.csv",header = TRUE)
country_list_latam_25 <- read.csv("country_codes_latam_25.csv",header = TRUE)
country_list_49 <- read.csv("country_codes_49.csv",header = TRUE)

list_esc_17 <- country_list_esc_17[[2]]
list_49 <- country_list_49[[2]]
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


chile_1988 <- chile[ which( chile$year >= 1951 & chile$year <= 2014) ,]
chile_mad_1988 <- chile_mad[ which( chile_mad$year >= 1951 & chile_mad$year <= 2014) ,]

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


