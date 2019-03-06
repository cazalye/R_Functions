###########  	 Control Probe PCA & Heatmap 	 ###########

# This function takes beta values of control probes and performs 3 steps: PCA, correlation with resultant first 10 PCs & covariates, heatmap (output as .png)
 
# data = character string of beta value data object(s) (objects must be in R workspace)
# covariates = df of biological &/or technical covariates of interest
# Can be run over multiple datasets, for example to compare normalisation methods. Creates separate image file for each dataset 




heatmap.ctrl.probes <- function(data, covariates){

require(gplots)

	for(i in 1:length(data)){
	png(file=paste(data[i], "_heatmap.png", sep=""), 	
		width = 10*300, 
		height = 5*300, 
		res=300, 
		pointsize = 5.90)	

	prcomp <- prcomp(t(na.omit(get(data[i]))))
	PCs <- data.frame(prcomp$x[,1:10])
	PCs$barcode <- row.names(PCs)
	
	cor.matrix <- merge(covariates, PCs)
	print(dim(cor.matrix))  
	cor <- na.omit(cor(x=cor.matrix[,c(2:10)],  
		y=cor.matrix[,11:20], 
	       	use="pairwise.complete.obs", 
		method="pearson")) 

	heatmap.2(cor,
 		cellnote = cor,  				# use same data for cell labels		
 		main = paste("Correlation between PCs 1-10 of", data[i], "all probes & 
			Technical + Biological Covariates", sep=" "), 	
 		notecol="black",      				# font colour of cell labels
  		density.info="none",  				# turn off density plot inside color legend
  		trace="none",         				# turn off trace lines inside the heat map
		margins =c(11,9),     				# widen margins around plot, default (5,5)
  		col=colorRampPalette(c("red",
			"yellow","green"))(n = 299),       
  		breaks=c(seq(-1,0,length=100),   		# enable color transition at specified limits
               		 seq(0.01,0.8,length=100),	
               		 seq(0.81,1,length=100)),    	
 		dendrogram="none",				# don't draw a dendrogram	
		Colv="NA",					# turn off col clustering,instead order by PCs
		keysize=.5)            				# make the key smaller
dev.off()
}}



# heatmap.ctrl.probes(data=data, covariates=covariates)
