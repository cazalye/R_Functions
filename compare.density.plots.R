##########  Combined Density Plot ##########

# Creates a combined figure for data normalised with 4 different methods

  
require(minfi)  

compare.density.plots <- function(data){
	png(file="densityPlots.png", 
	width = 10*300, 
	height = 5*300, 
	res=300, 
	pointsize = 5.90)

	par(mfrow=c(2,2)) 	# if you have more than 4 methods, change accordingly

	for(i in 1:length(data)){
		densityPlot(get(data[i]), main=data[i], xlab="Beta Values", legend = TRUE)
	}

dev.off()	
}

