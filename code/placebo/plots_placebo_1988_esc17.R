# WB

gap_1988_esc_17_dm <- Y_actual_1988 - Y_synth_1988_esc_17_dm 

gaps_1988_esc_17_dm

year <- unique(chile_1988$year)

setEPS()
postscript("placebo_1988_esc17_scm.eps", height = 4.5, width = 7.5)

plot(range(year),c(-2,2), ylab="ln gdp gap", xlab="year", main="", type="n")
for (j in 1:n_ctr_esc_17) lines(year, gaps_1988_esc_17_dm[,j] ,col="darkgrey",lwd=0.75,lty=1)
lines(year, gap_1988_esc_17_dm,col="black",lwd=4)
abline(v=1988, col="blue",lty=2)


dev.off()

colMeans(gaps_1988_esc_17_dm[(T_0_esc_17+1):T_all_esc_17,]^2)
mean(gaps_1988_esc_17_dm[(T_0_esc_17+1):T_all_esc_17,]^2)


gap_1988_esc_17_gsynth <- out_esc_17$att

gaps_1988_esc_17_gsynth

year <- unique(chile_1988$year)

setEPS()
postscript("placebo_1988_esc17_gsynth.eps", height = 4.5, width = 7.5)

plot(range(year),c(-2,2), ylab="ln gdp gap", xlab="year", main="", type="n")
for (j in 1:n_ctr_esc_17) lines(year, gaps_1988_esc_17_gsynth[,j] ,col="darkgrey",lwd=0.75,lty=1)
lines(year, gap_1988_esc_17_gsynth,col="black",lwd=4)
abline(v=1988, col="blue",lty=2)

dev.off()

colMeans(gaps_1988_esc_17_gsynth[(T_0_esc_17+1):T_all_esc_17,]^2)
mean(gaps_1988_esc_17_gsynth[(T_0_esc_17+1):T_all_esc_17,]^2)



gap_1988_esc_17_CI <-  impact_esc_17$series$point.effect

gaps_1988_esc_17_CI

year <- unique(chile_1988$year)


setEPS()
postscript("placebo_1988_esc17_CI.eps", height = 4.5, width = 7.5)

plot(range(year),c(-2,2), ylab="ln gdp gap", xlab="year", main="", type="n")
for (j in 1:n_ctr_esc_17) lines(year, gaps_1988_esc_17_CI[,j] ,col="darkgrey",lwd=0.75,lty=1)
lines(year, gap_1988_esc_17_CI,col="black",lwd=4)
abline(v=1988, col="blue",lty=2)

dev.off()

colMeans(gaps_1988_esc_17_CI[(T_0_esc_17+1):T_all_esc_17,]^2)
mean(gaps_1988_esc_17_CI[(T_0_esc_17+1):T_all_esc_17,]^2)


# Maddison



gap_mad_1988_esc_17_dm <- Y_actual_mad_1988 - Y_synth_mad_1988_esc_17_dm 

gaps_mad_1988_esc_17_dm

year <- unique(chile_mad_1988$year)


setEPS()
postscript("placebo_1988_esc17_mad_scm.eps", height = 4.5, width = 7.5)

plot(range(year),c(-2,2), ylab="ln gdp gap", xlab="year", main="", type="n")
for (j in 1:n_ctr_esc_17_mad) lines(year, gaps_mad_1988_esc_17_dm[,j] ,col="darkgrey",lwd=0.75,lty=1)
lines(year, gap_mad_1988_esc_17_dm,col="black",lwd=4)
abline(v=1988, col="blue",lty=2)

dev.off()

colMeans(gaps_mad_1988_esc_17_dm[(T_0_mad_esc_17+1):T_all_mad_esc_17,]^2)
mean(gaps_mad_1988_esc_17_dm[(T_0_mad_esc_17+1):T_all_mad_esc_17,]^2)



gap_mad_1988_esc_17_gsynth <- out_mad_esc_17$att

gaps_mad_1988_esc_17_gsynth

year <- unique(chile_mad_1988$year)



setEPS()
postscript("placebo_1988_esc17_mad_gsynth.eps", height = 4.5, width = 7.5)

plot(range(year),c(-2,2), ylab="ln gdp gap", xlab="year", main="", type="n")
for (j in 1:n_ctr_esc_17_mad) lines(year, gaps_mad_1988_esc_17_gsynth[,j] ,col="darkgrey",lwd=0.75,lty=1)
lines(year, gap_mad_1988_esc_17_gsynth,col="black",lwd=4)
abline(v=1988, col="blue",lty=2)

dev.off()

colMeans(gaps_mad_1988_esc_17_gsynth[(T_0_mad_esc_17+1):T_all_mad_esc_17,]^2)
mean(gaps_mad_1988_esc_17_gsynth[(T_0_mad_esc_17+1):T_all_mad_esc_17,]^2)


gap_mad_1988_esc_17_CI <-  impact_mad_esc_17$series$point.effect

gaps_mad_1988_esc_17_CI

year <- unique(chile_mad_1988$year)



setEPS()
postscript("placebo_1988_esc17_mad_CI.eps", height = 4.5, width = 7.5)

plot(range(year),c(-2,2), ylab="ln gdp gap", xlab="year", main="", type="n")
for (j in 1:n_ctr_esc_17_mad) lines(year, gaps_mad_1988_esc_17_CI[,j] ,col="darkgrey",lwd=0.75,lty=1)
lines(year, gap_mad_1988_esc_17_CI,col="black",lwd=4)
abline(v=1988, col="blue",lty=2)

dev.off()

colMeans(gaps_mad_1988_esc_17_CI[(T_0_mad_esc_17+1):T_all_mad_esc_17,]^2)
mean(gaps_mad_1988_esc_17_CI[(T_0_mad_esc_17+1):T_all_mad_esc_17,]^2)
