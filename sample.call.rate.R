##########	 Calculate Sample Call Rate for 450k data	##########  

# Provides a call rate for each sample (column) by calculating what proportion of CpGs (rows) are not NA
# Ideally call rate is >97% but >95% is also passable



sample.call.rate <- function(beta){
	rate <- colSums(!is.na(beta))/nrow(beta)
	return(rate)
}


