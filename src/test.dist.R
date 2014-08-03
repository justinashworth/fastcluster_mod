library(fastcluster)
rr = matrix(c(1,1,2,NA,3,2,2,2,2,3),2,byrow=T)
rownames(rr) = c('row1','row2')
colnames(rr) = c('a','b','c','d','e')
cc = 1-cor(t(rr),method='pearson',use='pairwise.complete.obs')
cc[is.na(cc)] = 2
dd1=as.dist(cc)
dd2=fastcluster_correlation_distance(t(rr))

cat(str(dd1))
cat(str(dd2))
