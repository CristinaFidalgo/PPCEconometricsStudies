#------------------------------------------------------------------------------#
#                          Multivariate analysis                               #
#------------------------------------------------------------------------------#

##      Specification of a VECM model with 4 economies - Germany + EC         ##

#------------------------------------------------------------------------------#

# First step: lags/order selection 
# Comment 1: maximum lag defined as 12, given data frequency
VARselect(rates.series.data.TFM, lag.max = 12, type = "const")$selection

# Second step: VAR estimation
VAR.est <- VAR(rates.series.data.TFM, p=2, type = "const")
summary(VAR.est)

# Third step: VAR specification (diagnostic tests with respect to the residuals)
## 1st test: test for serial autocorrelation, Q test of Portemanteau adjusted, Breusch-Godfrey LM test and Edgerton and Shukur test
port.test <- serial.test(VAR.est, lags.pt = 16, type = "PT.adjusted")
bg.test   <- serial.test(VAR.est, lags.bg = 5, type = "BG")
es.test   <- serial.test(VAR.est, type = "ES")
## Additional: graphical analysis of each equation residuals and squared residuals properties
plot(port.test, names="rate.g", main.resid = 'Resíduos da equação da economia da Alemanha', main.hist = 'Histograma e Distribuição Empírica', main.acf = 'FAC dos resíduos', main.pacf = 'FACP dos resíduos', main.acf2 = 'FAC do quadrado dos resíduos', main.pacf2 = 'FACP do quadrado dos resíduos', ylim.hist = c(0, 40))
plot(port.test, names="rate.h", main.resid = 'Resíduos da equação da economia da Hungria', main.hist = 'Histograma e Distribuição Empírica', main.acf = 'FAC dos resíduos', main.pacf = 'FACP dos resíduos', main.acf2 = 'FAC do quadrado dos resíduos', main.pacf2 = 'FACP do quadrado dos resíduos', ylim.hist = c(0, 40))
plot(port.test, names="rate.p", main.resid = 'Resíduos da equação da economia da Polónia', main.hist = 'Histograma e Distribuição Empírica', main.acf = 'FAC dos resíduos', main.pacf = 'FACP dos resíduos', main.acf2 = 'FAC do quadrado dos resíduos', main.pacf2 = 'FACP do quadrado dos resíduos', ylim.hist = c(0, 40))
plot(port.test, names="rate.cz", main.resid = 'Resíduos da equação da economia da República Checa', main.hist = 'Histograma e Distribuição Empírica', main.acf = 'FAC dos resíduos', main.pacf = 'FACP dos resíduos', main.acf2 = 'FAC do quadrado dos resíduos', main.pacf2 = 'FACP do quadrado dos resíduos', ylim.hist = c(0, 40))
## 2nd test: test for conditional hetereoskedasticity, ARCH-LM test
heter.test <- arch.test(VAR.est, lags.multi = 5, multivariate.only = TRUE)
## 3rd test: test for normality, Jarque-Bera
norm.test  <- normality.test(VAR.est, multivariate.only = TRUE)
## 4th test: test for stability, OLS-CUSUM plot
plot(stability(VAR.est), nc = 2) 

# Fourth step: Johansen-Procedure
## 1st test: Maximum Eeigenvalue test
eigen.test <- summary(ca.jo(rates.series.data.TFM, K = 2, type = "eigen", ecdet = "none", spec = "transitory"))
## 2nd test: Trace test
trace.test <- summary(ca.jo(rates.series.data.TFM, K = 2, type = "trace", ecdet = "none", spec = "transitory"))
## Attention: Osterwald-Lenum [1992] critical values are used
## Comment 1: both tests suggest that r=3

# Fifth step: estimation of the VEC model
## 1st option: Using urca package, cajorls - returns ECT coefficients by default and normalized OR non-normalized matrixes
VECM.urca   <- ca.jo(rates.series.data.TFM, K = 2, type = "trace", ecdet = "none", spec = "transitory")
coefPI(VECM.urca,r=3,normalize=FALSE) # PI matrix coefficients
coefA(VECM.urca,r=3,normalize=FALSE)  # alpha matrix coefficients
coefB(VECM.urca,r=3,normalize=FALSE)  # beta matrix coefficients
## 2nd option: Using urca package, cajorls - returns ECT coefficients by default and normalized matrixes
VECM.rls  <- cajorls(VECM.urca,r=3) # without statistical significance
VECM.rls.2  <- summary((cajorls(VECM.urca,r=3))$rlm) # with statistical significance
coefPI(VECM.rls) # PI matrix coefficients
coefA(VECM.rls)  # alpha matrix coefficients
coefB(VECM.rls)  # beta matrix coefficients
coefB(VECM.rls)  # beta matrix coefficients

# Sixth step: Modeling the long-run: the structure of beta matrix
# Comment 1: beta' matrix has order r*n, where r is the alfa*beta' matrix rank and n is the nr of variables; 
#             so, given that we have 4 variables and r=3 as per VECM fuction, beta' matrix has order 4*3.
H1            <- ca.jo(rates.series.data.TFM, K = 2, type = "trace", ecdet = "none", spec = "transitory")
H0A            <- matrix(c(1,1,1,-1,0,0,0,-1,0,0,0,-1), nrow = 4, ncol = 3, byrow = TRUE)
restrict.test.A <- summary(blrtest(z = H1, H = H0A, r = 3))
