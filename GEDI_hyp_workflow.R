# GEDI_hypsometer workflow


# when fill in table will only need to divide by how many rows (63) with no n/a
rmsetest <- sqrt(sum((allheight$zq95 - allheight$rh95)^2, na.rm = TRUE)/length(which(!is.na(allheight$rh95))))
rmse_loggedtest <- sqrt(sum((allheight$zq95[which(allheight$Degrdtn == 'logged')] - allheight$rh95[which(allheight$Degrdtn == 'logged')])^2, na.rm = TRUE)/length(which(!is.na(allheight$rh95[which(allheight$Degrdtn == 'logged')]))))
rmse_burnedtest <- sqrt(sum((allheight$zq95[which(allheight$Degrdtn == 'burned')] - allheight$rh95[which(allheight$Degrdtn == 'burned')])^2, na.rm = TRUE)/length(which(!is.na(allheight$rh95[which(allheight$Degrdtn == 'burned')]))))

rmse
rmsetest
rmse_log
rmse_loggedtest
rmse_burn
rmse_burnedtest


modtest <- lm(allheight$zq95 ~ allheight$rh95)
modlogtest <- lm(allheight$zq95[which(allheight$Degrdtn == 'logged')] ~ allheight$rh95[which(allheight$Degrdtn == 'logged')])
modburntest <- lm(allheight$zq95[which(allheight$Degrdtn == 'burned')] ~ allheight$rh95[which(allheight$Degrdtn == 'burned')])

mod
modtest
modlog
modlogtest
modburn
modburntest


biastest <- sum((allheight$zq95 - allheight$rh95), na.rm = TRUE)/length(which(!is.na(allheight$rh95)))
bias_logtest <- sum(allheight$zq95[allheight$Degrdtn == 'logged'] - allheight$rh95[allheight$Degrdtn == 'logged'], na.rm = TRUE)/length(which(!is.na(allheight$rh95[allheight$Degrdtn == 'logged'])))
bias_burntest <- sum(allheight$zq95[allheight$Degrdtn == 'burned'] - allheight$rh95[allheight$Degrdtn == 'burned'], na.rm = TRUE)/length(which(!is.na(allheight$rh95[allheight$Degrdtn == 'burned'])))

bias
biastest
bias_log
bias_logtest
bias_burn
bias_burntest


mean_hyp_height <- sum(mean(allheight$zq95))
rbiastest <- sum(bias/mean_hyp_height)*100
rbias_logtest <- sum(bias_log/mean_hyp_height)*100
rbias_burntest <- sum(bias_burn/mean_hyp_height)*100

rbias
rbiastest