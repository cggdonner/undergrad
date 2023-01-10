// GOOGLE PROBLEM
generate dateinteger = [_n]
tsset dateinteger
generate returngoogle = ((pricegoogle-l1.pricegoogle)/l1.pricegoogle)*100
// regression
regress returngoogle d.pricetnx d.SMB d.HML, robust
// ARIMA
arima returngoogle d.pricetnx d.SMB d.HML, arima (1,0,0)
// ARCH
arch returngoogle d.pricetnx d.SMB d.HML, arch(1/1)
// newey west robust
newey returngoogle d.pricetnx d.SMB d.HML, lag(1)
// time series graph
tsline returngoogle
// regression for price
regress pricegoogle d.pricetnx d.SMB d.HML, robust