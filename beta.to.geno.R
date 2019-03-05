##########   Convert betas from 65 SNP probes to genotypes: 0, 1=hets, 2   ##########   



beta.to.geno <- function(snp.betas, centers=c(0.2,0.5,0.8)) {		# centers for low, mid, high methylation (eg. TT, CT, CC)
	geno <- t(apply(snp.betas, 1, function(geno) {			# 1 indicates perform function on rows
        	kmeans(geno, centers=centers)$cluster - 1		# -1 makes cluster 1=0, cluster2=1, cluster3=2
    	}))
}






# adapted from:

beta.to.geno.old <- function(snp.betas, centers=c(0.2,0.5,0.8)) {	
	geno <- t(apply(snp.betas, 1, function(geno) {			
        	tryCatch(kmeans(geno, centers=centers)$cluster - 1,
           	error=function(e) {
			cluster <- rep(1,ncol(snp.betas))
                    	cluster[which(geno < min(centers))] <- 0
                     	cluster[which(geno > max(centers))] <- 2
                    	cluster
                 })
    	}))
    	dimnames(geno) <- dimnames(snp.betas)
    	return(geno)
}


snps_new <- beta.to.geno(snp.betas)
snps_old <-  beta.to.geno.old(snp.betas)
identical(snps_new, snps_old) # TRUE