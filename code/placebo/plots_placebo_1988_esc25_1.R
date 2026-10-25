# WB

gap_1988_esc_25_dm <- Y_actual_1988 - Y_synth_1988_esc_25_dm 

gaps_1988_esc_25_dm

year <- unique(chile_1988$year)

setEPS()
postscript("placebo_1988_esc25_1_scm.eps", height = 4.5, width = 7.5)

plot(range(year),c(-2,2), ylab="ln gdp gap", xlab="year", main="", type="n")
for (j in 1:n_ctr_esc_25) lines(year, gaps_1988_esc_25_dm[,j] ,col="darkgrey",lwd=0.75,lty=1)
lines(year, gap_1988_esc_25_dm,col="black",lwd=4)
abline(v=1988, col="blue",lty=2)

dev.off()

colMeans(gaps_1988_esc_25_dm[(T_0_esc_25+1):T_all_esc_25,]^2)
mean(gaps_1988_esc_25_dm[(T_0_esc_25+1):T_all_esc_25,]^2)


gap_1988_esc_25_gsynth <- out_esc_25$att

gaps_1988_esc_25_gsynth

year <- unique(chile_1988$year)

setEPS()
postscript("placebo_1988_esc25_1_gsynth.eps", height = 4.5, width = 7.5)

plot(range(year),c(-2,2), ylab="ln gdp gap", xlab="year", main="", type="n")
for (j in 1:n_ctr_esc_25) lines(year, gaps_1988_esc_25_gsynth[,j] ,col="darkgrey",lwd=0.75,lty=1)
lines(year, gap_1988_esc_25_gsynth,col="black",lwd=4)
abline(v=1988, col="blue",lty=2)

dev.off()

colMeans(gaps_1988_esc_25_gsynth[(T_0_esc_25+1):T_all_esc_25,]^2)
mean(gaps_1988_esc_25_gsynth[(T_0_esc_25+1):T_all_esc_25,]^2)



gap_1988_esc_25_CI <-  impact_esc_25$series$point.effect

gaps_1988_esc_25_CI

year <- unique(chile_1988$year)


setEPS()
postscript("placebo_1988_esc25_1_CI.eps", height = 4.5, width = 7.5)

plot(range(year),c(-2,2), ylab="ln gdp gap", xlab="year", main="", type="n")
for (j in 1:n_ctr_esc_25) lines(year, gaps_1988_esc_25_CI[,j] ,col="darkgrey",lwd=0.75,lty=1)
lines(year, gap_1988_esc_25_CI,col="black",lwd=4)
abline(v=1988, col="blue",lty=2)

dev.off()

colMeans(gaps_1988_esc_25_CI[(T_0_esc_25+1):T_all_esc_25,]^2)
mean(gaps_1988_esc_25_CI[(T_0_esc_25+1):T_all_esc_25,]^2)


# Maddison



gap_mad_1988_esc_25_dm <- Y_actual_mad_1988 - Y_synth_mad_1988_esc_25_dm 

gaps_mad_1988_esc_25_dm

year <- unique(chile_mad_1988$year)


setEPS()
postscript("placebo_1988_esc25_1_mad_scm.eps", height = 4.5, width = 7.5)

plot(range(year),c(-2,2), ylab="ln gdp gap", xlab="year", main="", type="n")
for (j in 1:n_ctr_esc_25_mad) lines(year, gaps_mad_1988_esc_25_dm[,j] ,col="darkgrey",lwd=0.75,lty=1)
lines(year, gap_mad_1988_esc_25_dm,col="black",lwd=4)
abline(v=1988, col="blue",lty=2)

dev.off()

colMeans(gaps_mad_1988_esc_25_dm[(T_0_mad_esc_25+1):T_all_mad_esc_25,]^2)
mean(gaps_mad_1988_esc_25_dm[(T_0_mad_esc_25+1):T_all_mad_esc_25,]^2)



gap_mad_1988_esc_25_gsynth <- out_mad_esc_25$att

gaps_mad_1988_esc_25_gsynth

year <- unique(chile_mad_1988$year)



setEPS()
postscript("placebo_1988_esc25_1_mad_gsynth.eps", height = 4.5, width = 7.5)

plot(range(year),c(-2,2), ylab="ln gdp gap", xlab="year", main="", type="n")
for (j in 1:n_ctr_esc_25_mad) lines(year, gaps_mad_1988_esc_25_gsynth[,j] ,col="darkgrey",lwd=0.75,lty=1)
lines(year, gap_mad_1988_esc_25_gsynth,col="black",lwd=4)
abline(v=1988, col="blue",lty=2)

dev.off()

colMeans(gaps_mad_1988_esc_25_gsynth[(T_0_mad_esc_25+1):T_all_mad_esc_25,]^2)
mean(gaps_mad_1988_esc_25_gsynth[(T_0_mad_esc_25+1):T_all_mad_esc_25,]^2)


gap_mad_1988_esc_25_CI <-  impact_mad_esc_25$series$point.effect

gaps_mad_1988_esc_25_CI

year <- unique(chile_mad_1988$year)



setEPS()
postscript("placebo_1988_esc25_1_mad_CI.eps", height = 4.5, width = 7.5)

plot(range(year),c(-2,2), ylab="ln gdp gap", xlab="year", main="", type="n")
for (j in 1:n_ctr_esc_25_mad) lines(year, gaps_mad_1988_esc_25_CI[,j] ,col="darkgrey",lwd=0.75,lty=1)
lines(year, gap_mad_1988_esc_25_CI,col="black",lwd=4)
abline(v=1988, col="blue",lty=2)

dev.off()

colMeans(gaps_mad_1988_esc_25_CI[(T_0_mad_esc_25+1):T_all_mad_esc_25,]^2)
mean(gaps_mad_1988_esc_25_CI[(T_0_mad_esc_25+1):T_all_mad_esc_25,]^2)
