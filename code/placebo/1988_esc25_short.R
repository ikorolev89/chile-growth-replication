
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





chile_mad <- chile_mad |> 
  group_by(CountryCode) |>
  filter(!any(is.na(c(lnGDP_pc)))) |>
  ungroup()






# 1988


chile_1988 <- chile[ which( chile$year >= 1975 & chile$year <= 2014) ,]
chile_mad_1988 <- chile_mad[ which( chile_mad$year >= 1975 & chile_mad$year <= 2014) ,]

# demean WB data

chile_1988 <- chile_1988 |>
  # Calculate mean by group
  group_by(CountryCode) |>
  mutate(lnGDP_pc_mean = mean(lnGDP_pc[year < 1988])) |> 
  as.data.frame()

chile_1988 <- chile_1988 |>
  mutate(lnGDP_pc_dm = lnGDP_pc - lnGDP_pc_mean) |> 
  as.data.frame()

# demean mad data

chile_mad_1988 <- chile_mad_1988 |>
  # Calculate mean by group
  group_by(CountryCode) |>
  mutate(lnGDP_pc_mean = mean(lnGDP_pc[year < 1988])) |> 
  as.data.frame()

chile_mad_1988 <- chile_mad_1988 |>
  mutate(lnGDP_pc_dm = lnGDP_pc - lnGDP_pc_mean) |> 
  as.data.frame()


# SCM


# Escalante, 17 countries

chile_1988_esc_25 <- chile_1988 |> filter(CountryCode %in% list_esc_25)

chile_1988_esc_25 <- chile_1988_esc_25 |> filter(CountryCode != "CHL")

chile_1988_esc_25 <- chile_1988_esc_25 %>%
  group_by(CountryCode) %>%
  mutate(CountryID = cur_group_id()) |> 
  as.data.frame()

n_ctr_esc_25 <- max(chile_1988_esc_25$CountryID)
T_all_esc_25 <- max(chile_1988_esc_25$year)-min(chile_1988_esc_25$year)+1 

T_0_esc_25 <- 1988 - min(chile_1988_esc_25$year)

gaps_1988_esc_25 <- matrix(, nrow = T_all_esc_25, ncol = n_ctr_esc_25)
gaps_1988_esc_25_dm <- matrix(, nrow = T_all_esc_25, ncol = n_ctr_esc_25)

gaps_1988_esc_25_gsynth <- matrix(, nrow = T_all_esc_25, ncol = n_ctr_esc_25)


for (j in 1:n_ctr_esc_25){
  
  
  Y_actual_1988_dm_placebo <- chile_1988_esc_25$lnGDP_pc_dm[ which(chile_1988_esc_25$CountryID == j)]
  
  chile_1988_esc_25$Treat <- 0
  chile_1988_esc_25$Treat[ which(chile_1988_esc_25$CountryID == j & chile_1988_esc_25$year >= 1988) ] <- 1
  
  
  chile_syn_1988_esc_25_placebo <- augsynth(lnGDP_pc ~ Treat, CountryID, year, chile_1988_esc_25,
                                           progfunc = "None", scm = T)
  
  
  chile_syn_1988_esc_25_dm_placebo <- augsynth(lnGDP_pc_dm ~ Treat, CountryID, year, chile_1988_esc_25,
                                              progfunc = "None", scm = T)
  
  Y_synth_1988_esc_25_dm_placebo <- chile_syn_1988_esc_25_dm_placebo$data$synth_data$Y0plot %*% chile_syn_1988_esc_25_dm_placebo$weights
  
  gaps_1988_esc_25_dm[,j] <- Y_actual_1988_dm_placebo - Y_synth_1988_esc_25_dm_placebo
  
  
  out_esc_25_placebo <- fect(lnGDP_pc ~ Treat, data = chile_1988_esc_25, index = c("CountryCode","year"), 
                            method = "gsynth", force = "two-way", CV = TRUE, r = c(0, 5), 
                            se = FALSE, nboots = 1, vartype = 'parametric', 
                            parallel = FALSE)
  
  gaps_1988_esc_25_gsynth[,j] <- out_esc_25_placebo$att
  
  
}



chile_mad_1988_esc_25 <- chile_mad_1988 |> filter(CountryCode %in% list_esc_25)

chile_mad_1988_esc_25 <- chile_mad_1988_esc_25 |> filter(CountryCode != "CHL")

chile_mad_1988_esc_25 <- chile_mad_1988_esc_25 %>%
  group_by(CountryCode) %>%
  mutate(CountryID = cur_group_id()) |> 
  as.data.frame()

n_ctr_esc_25_mad <- max(chile_mad_1988_esc_25$CountryID)
T_all_mad_esc_25 <- max(chile_mad_1988_esc_25$year)-min(chile_mad_1988_esc_25$year)+1 

T_0_mad_esc_25 <- 1988 - min(chile_mad_1988_esc_25$year)

gaps_mad_1988_esc_25 <- matrix(, nrow = T_all_mad_esc_25, ncol = n_ctr_esc_25_mad)
gaps_mad_1988_esc_25_dm <- matrix(, nrow = T_all_mad_esc_25, ncol = n_ctr_esc_25_mad)

gaps_mad_1988_esc_25_gsynth <- matrix(, nrow = T_all_mad_esc_25, ncol = n_ctr_esc_25_mad)


for (j in 1:n_ctr_esc_25_mad){
  
  
  Y_actual_mad_1988_dm_placebo <- chile_mad_1988_esc_25$lnGDP_pc_dm[ which(chile_mad_1988_esc_25$CountryID == j)]
  
  chile_mad_1988_esc_25$Treat <- 0
  chile_mad_1988_esc_25$Treat[ which(chile_mad_1988_esc_25$CountryID == j & chile_mad_1988_esc_25$year >= 1988) ] <- 1
  
  
  chile_syn_mad_1988_esc_25_placebo <- augsynth(lnGDP_pc ~ Treat, CountryID, year, chile_mad_1988_esc_25,
                                               progfunc = "None", scm = T)
  
  
  chile_syn_mad_1988_esc_25_dm_placebo <- augsynth(lnGDP_pc_dm ~ Treat, CountryID, year, chile_mad_1988_esc_25,
                                                  progfunc = "None", scm = T)
  
  Y_synth_mad_1988_esc_25_dm_placebo <- chile_syn_mad_1988_esc_25_dm_placebo$data$synth_data$Y0plot %*% chile_syn_mad_1988_esc_25_dm_placebo$weights
  
  gaps_mad_1988_esc_25_dm[,j] <- Y_actual_mad_1988_dm_placebo - Y_synth_mad_1988_esc_25_dm_placebo
  
  
  out_mad_esc_25_placebo <- fect(lnGDP_pc ~ Treat, data = chile_mad_1988_esc_25, index = c("CountryCode","year"), 
                                method = "gsynth", force = "two-way", CV = TRUE, r = c(0, 5), 
                                se = FALSE, nboots = 1, vartype = 'parametric', 
                                parallel = FALSE)
  
  gaps_mad_1988_esc_25_gsynth[,j] <- out_mad_esc_25_placebo$att
  
}




# CausalImpact


data_temp_esc_25 <- chile_1988_esc_25 |> dplyr::select(CountryCode,year,lnGDP_pc)

data1_esc_25 <- data_temp_esc_25 %>% 
  pivot_wider(names_from = CountryCode, values_from = lnGDP_pc)

data_esc_25 <- as.matrix(data1_esc_25[,2:ncol(data1_esc_25)])

list <- colnames(data_esc_25)

#matplot(data_esc_25, type = "l")

pre.period_esc_25 <- c(1, T_0_esc_25)
post.period_esc_25 <- c(T_0_esc_25+1, T_all_esc_25)

gaps_1988_esc_25_CI <- matrix(, nrow = T_all_esc_25, ncol = n_ctr_esc_25)


for (j in 1:n_ctr_esc_25){
  
  l = list[j]
  
  data_esc_25_placebo <- data_esc_25 |> as.data.frame() |>
    relocate(l) |> as.matrix()
  
  
  impact_esc_25_placebo <- CausalImpact(data_esc_25_placebo, pre.period_esc_25, post.period_esc_25)
  
  gaps_1988_esc_25_CI[,j] <- impact_esc_25_placebo$series$point.effect
  
}

#plot(impact_esc_25, c("original", "pointwise"))



data_temp_mad_esc_25 <- chile_mad_1988_esc_25 |> dplyr::select(CountryCode,year,lnGDP_pc)

data1_mad_esc_25 <- data_temp_mad_esc_25 %>% 
  pivot_wider(names_from = CountryCode, values_from = lnGDP_pc)

data_mad_esc_25 <- as.matrix(data1_mad_esc_25[,2:ncol(data1_mad_esc_25)])

list_mad <- colnames(data_mad_esc_25)

#matplot(data_esc_25, type = "l")

pre.period_mad_esc_25 <- c(1, T_0_mad_esc_25)
post.period_mad_esc_25 <- c(T_0_mad_esc_25+1, T_all_mad_esc_25)

gaps_mad_1988_esc_25_CI <- matrix(, nrow = T_all_mad_esc_25, ncol = n_ctr_esc_25_mad)

for (j in 1:n_ctr_esc_25_mad){
  
  l = list_mad[j]
  
  data_mad_esc_25_placebo <- data_mad_esc_25 |> as.data.frame() |>
    relocate(l) |> as.matrix()
  
  
  impact_mad_esc_25_placebo <- CausalImpact(data_mad_esc_25_placebo, pre.period_mad_esc_25, post.period_mad_esc_25)
  
  gaps_mad_1988_esc_25_CI[,j] <- impact_mad_esc_25_placebo$series$point.effect
  
  
}
