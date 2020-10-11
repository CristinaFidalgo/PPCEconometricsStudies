#------------------------------------------------------------------------------#
#                            Univariate analysis                               #
#------------------------------------------------------------------------------#

# Plots
plot.b  <- ggplot(rates.series.data, aes(x=date, y=rate.b))+geom_line()+xlab("")+ylab("")+ggtitle("Bulgária")+theme(plot.title = element_text(hjust = 0.5)) + scale_x_date(breaks = c(as.Date("1993-1-1"), as.Date("2000-1-1"), as.Date("2007-1-1"), as.Date("2014-1-1"), as.Date("2019-01-31")), date_labels = "%Y", limits = c(as.Date("1993-1-1"), as.Date("2019-12-31")))
plot.cr <- ggplot(rates.series.data, aes(x=date, y=rate.cr))+geom_line()+xlab("")+ylab("")+ggtitle("Croácia")+theme(plot.title = element_text(hjust = 0.5)) + scale_x_date(breaks = c(as.Date("1993-1-1"), as.Date("2000-1-1"), as.Date("2007-1-1"), as.Date("2014-1-1"), as.Date("2019-01-31")), date_labels = "%Y", limits = c(as.Date("1993-1-1"), as.Date("2019-12-31")))
plot.h  <- ggplot(rates.series.data, aes(x=date, y=rate.h))+geom_line()+xlab("")+ylab("")+ggtitle("Hungria")+theme(plot.title = element_text(hjust = 0.5)) + scale_x_date(breaks = c(as.Date("1993-1-1"), as.Date("2000-1-1"), as.Date("2007-1-1"), as.Date("2014-1-1"), as.Date("2019-01-31")), date_labels = "%Y", limits = c(as.Date("1993-1-1"), as.Date("2019-12-31")))
plot.p  <- ggplot(rates.series.data, aes(x=date, y=rate.p))+geom_line()+xlab("")+ylab("")+ggtitle("Polónia")+theme(plot.title = element_text(hjust = 0.5)) + scale_x_date(breaks = c(as.Date("1993-1-1"), as.Date("2000-1-1"), as.Date("2007-1-1"), as.Date("2014-1-1"), as.Date("2019-01-31")), date_labels = "%Y", limits = c(as.Date("1993-1-1"), as.Date("2019-12-31")))
plot.cz <- ggplot(rates.series.data, aes(x=date, y=rate.cz))+geom_line()+xlab("")+ylab("")+ggtitle("República Checa")+theme(plot.title = element_text(hjust = 0.5)) + scale_x_date(breaks = c(as.Date("1993-1-1"), as.Date("2000-1-1"), as.Date("2007-1-1"), as.Date("2014-1-1"), as.Date("2019-01-31")), date_labels = "%Y", limits = c(as.Date("1993-1-1"), as.Date("2019-12-31")))
plot.r  <- ggplot(rates.series.data, aes(x=date, y=rate.r))+geom_line()+xlab("")+ylab("")+ggtitle("Roménia")+theme(plot.title = element_text(hjust = 0.5)) + scale_x_date(breaks = c(as.Date("1993-1-1"), as.Date("2000-1-1"), as.Date("2007-1-1"), as.Date("2014-1-1"), as.Date("2019-01-31")), date_labels = "%Y", limits = c(as.Date("1993-1-1"), as.Date("2019-12-31")))
plot.g <- ggplot(rates.series.data, aes(x=date, y=rate.g))+geom_line()+xlab("")+ylab("")+ggtitle("Alemanha")+theme(plot.title = element_text(hjust = 0.5)) + scale_x_date(breaks = c(as.Date("1993-1-1"), as.Date("2000-1-1"), as.Date("2007-1-1"), as.Date("2014-1-1"), as.Date("2019-01-31")), date_labels = "%Y", limits = c(as.Date("1993-1-1"), as.Date("2019-12-31")))
plot.sl  <- ggplot(rates.series.data, aes(x=date, y=rate.sl))+geom_line()+xlab("")+ylab("")+ggtitle("Eslováquia")+theme(plot.title = element_text(hjust = 0.5)) + scale_x_date(breaks = c(as.Date("1993-1-1"), as.Date("2000-1-1"), as.Date("2007-1-1"), as.Date("2014-1-1"), as.Date("2019-01-31")), date_labels = "%Y", limits = c(as.Date("1993-1-1"), as.Date("2019-12-31")))

grid.arrange(plot.g, plot.h, plot.p, plot.cz, nrow = 2)

# Autocorrelation Functions (ACF)

par(mfrow=c(2,2))

acf.b  <- acf(rate.b, plot = FALSE)
acf.cr <- acf(rate.cr, plot = FALSE)
acf.cz <- acf(rate.cz, plot = FALSE)
acf.p  <- acf(rate.p, plot = FALSE)
acf.h  <- acf(rate.h, plot = FALSE)
acf.r  <- acf(rate.r, plot = FALSE)
acf.g  <- acf(rate.g, plot = FALSE)

plot(acf.g, main = "Alemanha", xlab = "Desfasamento", ylab = "Autocorrelação")
plot(acf.h, main = "Hungria", xlab = "Desfasamento", ylab = "Autocorrelação")
plot(acf.p, main = "Polónia", xlab = "Desfasamento", ylab = "Autocorrelação")
plot(acf.cz, main = "República Checa", xlab = "Desfasamento", ylab = "Autocorrelação")

acf.b.d  <- acf(rate.b.d, plot = FALSE)
acf.cr.d <- acf(rate.cr.d, plot = FALSE)
acf.cz.d <- acf(rate.cz.d, plot = FALSE)
acf.p.d  <- acf(rate.p.d, plot = FALSE)
acf.h.d  <- acf(rate.h.d, plot = FALSE)
acf.r.d  <- acf(rate.r.d, plot = FALSE)
acf.g.d  <- acf(rate.g.d, plot = FALSE)

plot(acf.g.d, main = "Alemanha", xlab = "Desfasamento", ylab = "Autocorrelação")
plot(acf.h.d, main = "Hungria", xlab = "Desfasamento", ylab = "Autocorrelação")
plot(acf.p.d, main = "Polónia", xlab = "Desfasamento", ylab = "Autocorrelação")
plot(acf.cz.d, main = "República Checa", xlab = "Desfasamento", ylab = "Autocorrelação")

par(mfrow=c(1,1)) # reset

# Correlation matrix
corr.matrix.2 <- rcorr(as.matrix(rates.series.data.TFM))

# Summary statistics
sumstats <- summary(rates.series.data.TFM)

# Unit Root Tests - Dickey-Fuller
# Comment 1: type ="drift" considers only drift in test equation while type="trend" considers both drift and trend
# Comment 2: number of lags selected following l12 (=16) rule on Lopes (2015, pg.98) - taken from Schwert (1989)
# Comment 3: second and third test-stats values and phis critical values correspond to tests of join significance of all parameters in test equation
# Comment 4: critical values are taken from Dickey and Fuller (1981) and Hamilton (1994).
# Help resource: Enders, Applied Econometric Time Series 3e, 2010, p. 206-209

adf.cz.A <- summary(ur.df(rate.cz, type="drift", lags=16, selectlags = "AIC"))
adf.p.A  <- summary(ur.df(rate.p, type="drift", lags=16, selectlags = "AIC"))
adf.h.A  <- summary(ur.df(rate.h, type="drift", lags=16, selectlags = "AIC"))
adf.g.A  <- summary(ur.df(rate.g, type="drift", lags=16, selectlags = "AIC"))

# Auxiliary regressions without drift; as per previous results, drift does not seem to be statistically significant.
adf.p.A.NC  <- summary(ur.df(rate.p, type="none", lags=16, selectlags = "AIC"))
adf.h.A.NC  <- summary(ur.df(rate.h, type="none", lags=16, selectlags = "AIC"))

# Test to first differences: if series in level are first order integrated, first differences must be stationary.

adf.cz.d <- summary(ur.df(rate.cz.d, type="drift", lags=16, selectlags = "AIC"))
adf.p.d  <- summary(ur.df(rate.p.d, type="drift", lags=16, selectlags = "AIC"))
adf.h.d  <- summary(ur.df(rate.h.d, type="drift", lags=16, selectlags = "AIC"))
adf.g.d  <- summary(ur.df(rate.g.d, type="drift", lags=16, selectlags = "AIC"))

adf.cz.d.NC  <- summary(ur.df(rate.cz.d, type="none", lags=16, selectlags = "AIC"))
adf.p.d.NC  <- summary(ur.df(rate.p.d, type="none", lags=16, selectlags = "AIC"))
adf.h.d.NC  <- summary(ur.df(rate.h.d, type="none", lags=16, selectlags = "AIC"))
adf.g.d.NC  <- summary(ur.df(rate.g.d, type="none", lags=16, selectlags = "AIC"))

# Unit Root tests - DF-GLS
# Comment 1: Due to not so well known properties of DF test (lack of power in small samples), DF-GLS tests are conducted under a confirmatory approach.

dfgls.cz  <- summary(ur.ers(rate.cz, type="DF-GLS", model="constant", lag.max = 16))
dfgls.p   <- summary(ur.ers(rate.p, type="DF-GLS", model="constant", lag.max = 16))
dfgls.h   <- summary(ur.ers(rate.h, type="DF-GLS", model="constant", lag.max = 16))
dfgls.g   <- summary(ur.ers(rate.g, type="DF-GLS", model="constant", lag.max = 16))

