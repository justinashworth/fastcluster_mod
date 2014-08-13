#  fastcluster: Fast hierarchical clustering routines for R and Python
#
#  Copyright © 2011 Daniel Müllner
#  <http://danifold.net>

hclust <- function(d, method="complete", members=NULL)
{
  # Hierarchical clustering, on raw input data.
  METHODS <- c("single", "complete", "average", "mcquitty", "ward", "centroid", "median")
  method <- pmatch(method, METHODS)
  if (is.na(method))
    stop("Invalid clustering method.")
  if (method == -1)
    stop("Ambiguous clustering method.")
  dendrogram <- c( .Call(fastcluster, attr(d, "Size"), method, d, members),
    list(
      labels = attr(d, "Labels")
      ,method = METHODS[method]
      ,call = match.call()
      ,dist.method = attr(d, "method")
    )
  )
  class(dendrogram) <- "hclust"
  return (dendrogram)
}

hclust.vector <- function(X, method='single', members=NULL, metric='euclidean', p=NULL)
{
  # Hierarchical clustering, on vector data.
  METHODS <- c("single", "ward", "centroid", "median")
  methodidx <- pmatch(method, METHODS)
  if (is.na(methodidx))
    stop(paste("Invalid clustering method '", method, "' for vector data.", sep=''))
  if (methodidx == -1)
    stop("Ambiguous clustering method.")

  METRICS <- c("euclidean", "maximum", "manhattan", "canberra", "binary",
               "minkowski")
  metric = pmatch(metric, METRICS)
  if (is.na(metric))
    stop("Invalid metric.")
  if (metric == -1)
    stop("Ambiguous metric.")

  if (methodidx!=1 && metric!=1)
    stop("The Euclidean methods 'ward', 'centroid' and 'median' require the 'euclidean' metric.")

  X <- as.matrix(X)

  dendrogram <- c( .Call(fastcluster_vector, methodidx, metric, X, members, p),
    list(
      labels = dimnames(X)[[1L]]
      ,method = METHODS[methodidx]
      ,call = match.call()
      ,dist.method = METRICS[metric]
    )
  )
  class(dendrogram) <- "hclust"
  return (dendrogram)
}

fastcluster_correlation_distance <- function(mat,type=1)
{
	if(!is.matrix(mat)) stop('Not a matrix, it is: ',class(mat),' with nrow ',nrow(mat),' ncol ',ncol(mat),' and length ',length(mat))
  dd = .Call(fastcluster_correlation_distances, mat, nrow(mat), ncol(mat), as.integer(type))
	class(dd) = 'dist'
	attr(dd,'Size') = ncol(mat)
	attr(dd,'Labels') = colnames(mat)
	attr(dd,'Diag') = F
	attr(dd,'Upper') = F
	attr(dd,'call') = 'fastcluster_correlation_distance'
	return(dd)
}

