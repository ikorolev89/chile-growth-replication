set.seed(123)


M <- 250

MSE_scm <- matrix(, nrow = M, 4)
MSE_gsynth <- matrix(, nrow = M, 4)
MSE_CI <- matrix(, nrow = M, 4)

n_factors <- matrix(, nrow = M, 4)

factor_overlap <- array(NA, dim = c(M, 4, 5))

list_n <- c(10, 25, 50, 100)

for (j in 1:M){
  
  for (l in 1:4){  
    
    ### 1. Simulate panel data ###
    N  <- list_n[l]   # total units
    T  <- 20    # total periods
    T0 <- 10    # treatment start
    r  <- 3     # number of latent factors
    true_effect <- 1
    treated_unit <- 1
    
    unit_id = rep(1:N, each = T)
    time = rep(1:T, times = N)
    
    sigma <- 0.1
    
    ### 1. Simulate Interactive Fixed Effects ###
    # Factor loadings (N x r)
    #sigma_lambda <- sqrt(N)/sqrt(40)
    #sigma_lambda <- N/50
    sigma_lambda <- 0.5
    Lambda <- matrix(rnorm(N * r, mean = 1, sd = sigma_lambda), N, r)
    
    # Common factors (T x r)
    #sigma_fe <- sqrt(N)/sqrt(20)
    #sigma_fe <- N/25
    sigma_fe <- 1
    unit_fe <- rnorm(N, mean = 7, sd = sigma_fe)  # unit_id fixed effects
    
    Fmat <- matrix(NA, T, r)
    Fmat[,1] <- 0.75*seq(0, .035*(T-1), length.out = T)                  # smooth trend
    Fmat[,2] <- 0.2*sin(seq(0, 4*pi, length.out = T))            # seasonal factor
    Fmat[,3] <- 0.2*cos(seq(0, 4*pi, length.out = T))
    
    # Noise
    
    
    eps <- matrix(rnorm(N * T, mean = 0, sd = sigma), N, T)
    
    # Outcome without treatment
    y <- unit_fe + Lambda %*% t(Fmat) + eps
    
    # Add treatment effect to treated unit_id after T0
    y[treated_unit, (T0+1):T] <- y[treated_unit, (T0+1):T] + true_effect
    
    
    data_sim <- data.frame(
      unit_id,
      time,
      y    = as.vector(t(y))
    )
    
    data_sim$Treat <- 0
    data_sim$Treat[unit_id == treated_unit & time > T0] <- 1
    
    
    data_sim <- data_sim |>
      # Calculate mean by group
      group_by(unit_id) |>
      mutate(y_mean = mean(y[time <= T0])) |> 
      as.data.frame()
    
    data_sim <- data_sim |>
      mutate(y_dm = y - y_mean) |> 
      as.data.frame()
    
    #plot(range(data_sim$time),c(4,15), ylab="ln gdp", xlab="year", main="", type="n")
    #for (j in 2:N) lines(data_sim$time[unit_id == j], data_sim$y[unit_id == j],col="darkgrey",lwd=0.75,lty=1)
    #lines(data_sim$time[unit_id == 1], data_sim$y[unit_id == 1],col="black",lwd=4)
    
    
    syn <- augsynth(y ~ Treat, unit_id, time, data_sim,
                    progfunc = "None", scm = T)
    
    
    syn_dm <- augsynth(y_dm ~ Treat, unit_id, time, data_sim,
                       progfunc = "None", scm = T)
    
    effect_scm <- (data_sim$y_dm[unit_id == treated_unit]-syn_dm$data$synth_data$Y0plot %*% syn_dm$weights)[(T0+1):T]
    errors_scm <- effect_scm - true_effect
    MSE_scm[j,l] <- mean(errors_scm^2)
    
    
    # GSynth
    
    
    out <- fect(y ~ Treat, data = data_sim, index = c("unit_id","time"), 
                method = "gsynth", force = "two-way", CV = TRUE, r = c(0, 5), 
                se = FALSE, nboots = 1000, vartype = 'parametric', 
                parallel = FALSE)
    
    effect_gsynth <- out$att[(T0+1):T]
    errors_gsynth <- effect_gsynth - true_effect
    MSE_gsynth[j,l] <- mean(errors_gsynth^2)
    
    n_factors[j,l] <- out$r.cv
    
    if (out$r.cv > 0){
      for (r_temp in 1:out$r.cv){
        factor_overlap[j,l,r_temp] <- mean(out$lambda.tr[r_temp] < out$lambda.co[,r_temp])
      }
    }
    
    # Causal Impact
    
    data_sim1 <- data_sim
    
    data_sim1$unit_id <- chartr("123456789", "ABCDEFGHI", data_sim1$unit_id)
    
    data_temp <- data_sim1 |> dplyr::select(unit_id,time,y)
    
    data1 <- data_temp %>% 
      pivot_wider(names_from = unit_id, values_from = y)
    
    data <- as.matrix(data1[,2:ncol(data1)])
    
    #matplot(data, type = "l")
    
    pre.period <- c(1, T0)
    post.period <- c(T0+1, T)
    
    impact <- CausalImpact(data, pre.period, post.period)
    
    effect_CI <- impact$series$point.effect[(T0+1):T]
    errors_CI <- effect_CI - true_effect
    MSE_CI[j,l] <- mean(errors_CI^2)
    
  }
}



colMeans(MSE_scm)
colMeans(MSE_gsynth)
colMeans(MSE_CI)


V <- rbind(colMeans(MSE_scm),colMeans(MSE_gsynth),colMeans(MSE_CI))

