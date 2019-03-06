##########    Calculate molecular correlation within twin pairs   ##########

# can calculate using either all available beta values, 65 SNP probe beta or genotypes 
# NB. different regions of the methylome have different heritability 



twin.correlation <- function(beta){
	cor <- cor(beta)
	cor[lower.tri(cor, diag=TRUE)] = NA  		  	# set self vs self to NA
	cor2 <- as.data.frame(as.table(cor))
	cor2=na.omit(cor2) 					# remove self vs self
	twin.correlation=cor2[order(-abs(cor2$Freq)),] 		# order by correlation
	return(twin.correlation)
}



