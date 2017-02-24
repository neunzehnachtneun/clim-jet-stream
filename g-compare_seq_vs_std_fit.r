######################################################################
## FEHLERGRÖẞEN
## MSE
## RMSE
######################################################################
##

residuals.cheb <- u.monmean - model.u
#residuals.cheb.seq <- u.monmean - model.u.seq
mse <- sum(residuals.cheb ** 2) / length(residuals.cheb)
#mse.seq <- sum(residuals.cheb.seq **2) / length(residuals.cheb.seq)
rmse <- sqrt(sum(residuals.cheb ** 2) / length(residuals.cheb))
#rmse.seq <- sqrt(sum(residuals.cheb.seq **2) / length(residuals.cheb.seq))

## rmse.seq = 0.4079846  ## mse.seq = 0.1664514
## rmse     = 0.2911683  ## mse     = 0.08477901