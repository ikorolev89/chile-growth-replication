
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

chile_1988_49 <- chile_1988 |> filter(CountryCode %in% list_49)

chile_1988_49 <- chile_1988_49 |> filter(CountryCode != "CHL")

chile_1988_49 <- chile_1988_49 %>%
  group_by(CountryCode) %>%
  mutate(CountryID = cur_group_id()) |> 
  as.data.frame()

n_ctr_49 <- max(chile_1988_49$CountryID)
T_all_49 <- max(chile_1988_49$year)-min(chile_1988_49$year)+1 

T_0_49 <- 1988 - min(chile_1988_49$year)

gaps_1988_49 <- matrix(, nrow = T_all_49, ncol = n_ctr_49)
gaps_1988_49_dm <- matrix(, nrow = T_all_49, ncol = n_ctr_49)

gaps_1988_49_gsynth <- matrix(, nrow = T_all_49, ncol = n_ctr_49)


for (j in 1:n_ctr_49){


Y_actual_1988_dm_placebo <- chile_1988_49$lnGDP_pc_dm[ which(chile_1988_49$CountryID == j)]

chile_1988_49$Treat <- 0
chile_1988_49$Treat[ which(chile_1988_49$CountryID == j & chile_1988_49$year >= 1988) ] <- 1


chile_syn_1988_49_placebo <- augsynth(lnGDP_pc ~ Treat, CountryID, year, chile_1988_49,
                              progfunc = "None", scm = T)


chile_syn_1988_49_dm_placebo <- augsynth(lnGDP_pc_dm ~ Treat, CountryID, year, chile_1988_49,
                                 progfunc = "None", scm = T)

Y_synth_1988_49_dm_placebo <- chile_syn_1988_49_dm_placebo$data$synth_data$Y0plot %*% chile_syn_1988_49_dm_placebo$weights

gaps_1988_49_dm[,j] <- Y_actual_1988_dm_placebo - Y_synth_1988_49_dm_placebo


out_49_placebo <- fect(lnGDP_pc ~ Treat, data = chile_1988_49, index = c("CountryCode","year"), 
               method = "gsynth", force = "two-way", CV = TRUE, r = c(0, 5), 
               se = FALSE, nboots = 1, vartype = 'parametric', 
               parallel = FALSE)

gaps_1988_49_gsynth[,j] <- out_49_placebo$att


}



chile_mad_1988_49 <- chile_mad_1988 |> filter(CountryCode %in% list_49)

chile_mad_1988_49 <- chile_mad_1988_49 |> filter(CountryCode != "CHL")

chile_mad_1988_49 <- chile_mad_1988_49 %>%
  group_by(CountryCode) %>%
  mutate(CountryID = cur_group_id()) |> 
  as.data.frame()

n_ctr_49_mad <- max(chile_mad_1988_49$CountryID)
T_all_mad_49 <- max(chile_mad_1988_49$year)-min(chile_mad_1988_49$year)+1 

T_0_mad_49 <- 1988 - min(chile_mad_1988_49$year)

gaps_mad_1988_49 <- matrix(, nrow = T_all_mad_49, ncol = n_ctr_49_mad)
gaps_mad_1988_49_dm <- matrix(, nrow = T_all_mad_49, ncol = n_ctr_49_mad)

gaps_mad_1988_49_gsynth <- matrix(, nrow = T_all_mad_49, ncol = n_ctr_49_mad)


for (j in 1:n_ctr_49_mad){
  
  
  Y_actual_mad_1988_dm_placebo <- chile_mad_1988_49$lnGDP_pc_dm[ which(chile_mad_1988_49$CountryID == j)]
  
  chile_mad_1988_49$Treat <- 0
  chile_mad_1988_49$Treat[ which(chile_mad_1988_49$CountryID == j & chile_mad_1988_49$year >= 1988) ] <- 1
  
  
  chile_syn_mad_1988_49_placebo <- augsynth(lnGDP_pc ~ Treat, CountryID, year, chile_mad_1988_49,
                                    progfunc = "None", scm = T)
  
  
  chile_syn_mad_1988_49_dm_placebo <- augsynth(lnGDP_pc_dm ~ Treat, CountryID, year, chile_mad_1988_49,
                                       progfunc = "None", scm = T)
  
  Y_synth_mad_1988_49_dm_placebo <- chile_syn_mad_1988_49_dm_placebo$data$synth_data$Y0plot %*% chile_syn_mad_1988_49_dm_placebo$weights
  
  gaps_mad_1988_49_dm[,j] <- Y_actual_mad_1988_dm_placebo - Y_synth_mad_1988_49_dm_placebo
  
  
  out_mad_49_placebo <- fect(lnGDP_pc ~ Treat, data = chile_mad_1988_49, index = c("CountryCode","year"), 
                 method = "gsynth", force = "two-way", CV = TRUE, r = c(0, 5), 
                 se = FALSE, nboots = 1, vartype = 'parametric', 
                 parallel = FALSE)
  
  gaps_mad_1988_49_gsynth[,j] <- out_mad_49_placebo$att
  
}
  



# CausalImpact


data_temp_49 <- chile_1988_49 |> dplyr::select(CountryCode,year,lnGDP_pc)

data1_49 <- data_temp_49 %>% 
  pivot_wider(names_from = CountryCode, values_from = lnGDP_pc)

data_49 <- as.matrix(data1_49[,2:ncol(data1_49)])

list <- colnames(data_49)

#matplot(data_49, type = "l")

pre.period_49 <- c(1, T_0_49)
post.period_49 <- c(T_0_49+1, T_all_49)

gaps_1988_49_CI <- matrix(, nrow = T_all_49, ncol = n_ctr_49)


for (j in 1:n_ctr_49){
  
  l = list[j]

  data_49_placebo <- data_49 |> as.data.frame() |>
  relocate(l) |> as.matrix()


  impact_49_placebo <- CausalImpact(data_49_placebo, pre.period_49, post.period_49)

  gaps_1988_49_CI[,j] <- impact_49_placebo$series$point.effect

}

#plot(impact_49, c("original", "pointwise"))



data_temp_mad_49 <- chile_mad_1988_49 |> dplyr::select(CountryCode,year,lnGDP_pc)

data1_mad_49 <- data_temp_mad_49 %>% 
  pivot_wider(names_from = CountryCode, values_from = lnGDP_pc)

data_mad_49 <- as.matrix(data1_mad_49[,2:ncol(data1_mad_49)])

list_mad <- colnames(data_mad_49)

#matplot(data_49, type = "l")

pre.period_mad_49 <- c(1, T_0_mad_49)
post.period_mad_49 <- c(T_0_mad_49+1, T_all_mad_49)

gaps_mad_1988_49_CI <- matrix(, nrow = T_all_mad_49, ncol = n_ctr_49_mad)

for (j in 1:n_ctr_49_mad){
  
  l = list_mad[j]
  
  data_mad_49_placebo <- data_mad_49 |> as.data.frame() |>
    relocate(l) |> as.matrix()
  
  
  impact_mad_49_placebo <- CausalImpact(data_mad_49_placebo, pre.period_mad_49, post.period_mad_49)
  
  gaps_mad_1988_49_CI[,j] <- impact_mad_49_placebo$series$point.effect
  
  
}
