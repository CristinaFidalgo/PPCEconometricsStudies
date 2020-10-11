#------------------------------------------------------------------------------#
#                       Causality and forecast analysis                        #
#------------------------------------------------------------------------------#

## Granger Causality

# Does GERMANY granger causes other economies? If so, is the accuracy of VECM with 4 economies better than VECM with 3 economies only?
# vars package
VAR.est.2 <- VAR(rates.series.data.TFM, p=2, type="const")
causality(VAR.est.2, cause="rate.g",vcov.=vcovHC(VAR.est.2))$Granger #robust HC variance-covariance matrix for the Granger test

## Forecast
# vars package
#VAR.urca.2 <- vec2var(VECM.urca.2)
#VAR.pred.urca <- predict(VAR.urca.2,n.ahead=6,ci=0.95)
#fanchart(VAR.pred.urca)
#plot(VAR.pred.urca)

# tsDyn package
## VECM model with 3 EC economies only
data_in_24_3          <- head(rates.series.data.TFM.2, -24)
data_out_24_3         <- tail(rates.series.data.TFM.2, 24)
lagorder_24_3         <- rank.select(data_in_24_3, lag.max=12)
mod_VECM_tsDyn_24_3   <- VECM(data_in_24_3, lag=1)
pred_VECM_tsDyn_24_3  <- predict(mod_VECM_tsDyn_24_3, n.ahead = 24)
accuracy_24_3         <- accuracy_stat(pred_VECM_tsDyn_24_3, true=data_out_24_3)

data_in_12_3          <- head(rates.series.data.TFM.2, -12)
data_out_12_3         <- tail(rates.series.data.TFM.2, 12)
lagorder_12_3         <- rank.select(data_in_12_3, lag.max=12)
mod_VECM_tsDyn_12_3   <- VECM(data_in_12_3, lag=1)
pred_VECM_tsDyn_12_3  <- predict(mod_VECM_tsDyn_12_3, n.ahead = 12)
accuracy_12_3         <- accuracy_stat(pred_VECM_tsDyn_12_3, true=data_out_12_3)

data_in_6_3          <- head(rates.series.data.TFM.2, -6)
data_out_6_3         <- tail(rates.series.data.TFM.2, 6)
lagorder_6_3         <- rank.select(data_in_6_3, lag.max=12)
mod_VECM_tsDyn_6_3   <- VECM(data_in_6_3, lag=1)
pred_VECM_tsDyn_6_3  <- predict(mod_VECM_tsDyn_6_3, n.ahead = 6)
accuracy_6_3         <- accuracy_stat(pred_VECM_tsDyn_6_3, true=data_out_6_3)

## VECM model with 4 EC economies
data_in_24_4          <- head(rates.series.data.TFM, -24)
data_out_24_4         <- tail(rates.series.data.TFM, 24)
lagorder_24_4         <- rank.select(data_in_24_4, lag.max=12)
mod_VECM_tsDyn_24_4   <- VECM(data_in_24_4, lag=1)
pred_VECM_tsDyn_24_4  <- predict(mod_VECM_tsDyn_24_4, n.ahead = 24)
accuracy_24_4         <- accuracy_stat(pred_VECM_tsDyn_24_4, true=data_out_24_4)

data_in_12_4          <- head(rates.series.data.TFM, -12)
data_out_12_4         <- tail(rates.series.data.TFM, 12)
lagorder_12_4         <- rank.select(data_in_12_4, lag.max=12)
mod_VECM_tsDyn_12_4   <- VECM(data_in_12_4, lag=1)
pred_VECM_tsDyn_12_4  <- predict(mod_VECM_tsDyn_12_4, n.ahead = 12)
accuracy_12_4         <- accuracy_stat(pred_VECM_tsDyn_12_4, true=data_out_12_4)

data_in_6_4          <- head(rates.series.data.TFM, -6)
data_out_6_4         <- tail(rates.series.data.TFM, 6)
lagorder_6_4         <- rank.select(data_in_6_4, lag.max=12)
mod_VECM_tsDyn_6_4   <- VECM(data_in_6_4, lag=1)
pred_VECM_tsDyn_6_4  <- predict(mod_VECM_tsDyn_6_4, n.ahead = 6)
accuracy_6_4         <- accuracy_stat(pred_VECM_tsDyn_6_4, true=data_out_6_4)

# forecast package
## Random walk - naive model
data_in_24_RW_h          <- head(rate.h, -24)
data_out_24_RW_h         <- tail(rate.h, 24)
pred_RW_24_h             <- rwf(data_in_24_RW_h, h=24, drift=TRUE)
accuracy_24_h            <- accuracy(pred_RW_24_h,data_out_24_RW_h)

data_in_24_RW_p          <- head(rate.p, -24)
data_out_24_RW_p         <- tail(rate.p, 24)
pred_RW_24_p             <- rwf(data_in_24_RW_p, h=24, drift=TRUE)
accuracy_24_p            <- accuracy(pred_RW_24_p,data_out_24_RW_p)

data_in_24_RW_cz          <- head(rate.cz, -24)
data_out_24_RW_cz         <- tail(rate.cz, 24)
pred_RW_24_cz             <- rwf(data_in_24_RW_cz, h=24, drift=TRUE)
accuracy_24_cz            <- accuracy(pred_RW_24_cz,data_out_24_RW_cz)

data_in_12_RW_h          <- head(rate.h, -12)
data_out_12_RW_h         <- tail(rate.h, 12)
pred_RW_12_h             <- rwf(data_in_12_RW_h, h=12, drift=TRUE)
accuracy_12_h            <- accuracy(pred_RW_12_h,data_out_12_RW_h)

data_in_12_RW_p          <- head(rate.p, -12)
data_out_12_RW_p         <- tail(rate.p, 12)
pred_RW_12_p             <- rwf(data_in_12_RW_p, h=12, drift=TRUE)
accuracy_12_p            <- accuracy(pred_RW_12_p,data_out_12_RW_p)

data_in_12_RW_cz          <- head(rate.cz, -12)
data_out_12_RW_cz         <- tail(rate.cz, 12)
pred_RW_12_cz             <- rwf(data_in_12_RW_cz, h=12, drift=TRUE)
accuracy_12_cz            <- accuracy(pred_RW_12_cz,data_out_12_RW_cz)

data_in_6_RW_h          <- head(rate.h, -6)
data_out_6_RW_h         <- tail(rate.h, 6)
pred_RW_6_h             <- rwf(data_in_6_RW_h, h=6, drift=TRUE)
accuracy_6_h            <- accuracy(pred_RW_6_h,data_out_6_RW_h)

data_in_6_RW_p          <- head(rate.p, -6)
data_out_6_RW_p         <- tail(rate.p, 6)
pred_RW_6_p             <- rwf(data_in_6_RW_p, h=6, drift=TRUE)
accuracy_6_p            <- accuracy(pred_RW_6_p,data_out_6_RW_p)

data_in_6_RW_cz          <- head(rate.cz, -6)
data_out_6_RW_cz         <- tail(rate.cz, 6)
pred_RW_6_cz             <- rwf(data_in_6_RW_cz, h=6, drift=TRUE)
accuracy_6_cz            <- accuracy(pred_RW_6_cz,data_out_6_RW_cz)
