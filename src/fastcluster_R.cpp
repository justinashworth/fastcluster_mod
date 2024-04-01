/*
  fastcluster: Fast hierarchical clustering routines for R and Python

  Copyright © 2011 Daniel Müllner
  <http://danifold.net>
*/
#if __GNUC__ > 4 || (__GNUC__ == 4 && (__GNUC_MINOR__ >= 6))
#define HAVE_DIAGNOSTIC 1
#endif

#if HAVE_DIAGNOSTIC
#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wredundant-decls"
#pragma GCC diagnostic ignored "-Wpadded"
#endif
#include <Rdefines.h>
#include <R_ext/Rdynload.h>
#include <Rmath.h> // for R_pow
#if HAVE_DIAGNOSTIC
#pragma GCC diagnostic pop
#endif

#define fc_isnan(X) ((X)!=(X))
// There is ISNAN but it is so much slower on my x86_64 system with GCC!

#include <cstddef> // for std::ptrdiff_t
#include <limits> // for std::numeric_limits<...>::infinity()
#include <algorithm> // for std::stable_sort
#include <stdexcept> // for std::runtime_error
#include <string> // for std::string
#include <list>
#include <vector>
//#include <sstream> // can't use sstream because Rinternal.h macro length() clashes with another
#include <new> // for std::bad_alloc
#include <exception> // for std::exception

// ja for rand()
#include <stdlib.h>
#include <time.h>

#include "fastcluster.cpp"

/* Since the public interface is given by the Python respectively R interface,
 * we do not want other symbols than the interface initalization routines to be
 * visible in the shared object file. The "visibility" switch is a GCC concept.
 * Hiding symbols keeps the relocation table small and decreases startup time.
 * See http://gcc.gnu.org/wiki/Visibility
 */
#if HAVE_VISIBILITY
#pragma GCC visibility push(hidden)
#endif

/*
  Helper function: order the nodes so that they can be displayed nicely
  in a dendrogram.

  This is used for the 'order' field in the R output.
*/

#if HAVE_DIAGNOSTIC
#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wpadded"
#endif
struct pos_node {
  t_index pos;
  int node;
};
#if HAVE_DIAGNOSTIC
#pragma GCC diagnostic pop
#endif

void order_nodes(const int N, const int * const merge, const t_index * const node_size, int * const order) {
  /* Parameters:
     N         : number of data points
     merge     : (N-1)×2 array which specifies the node indices which are
                 merged in each step of the clustering procedure.
                 Negative entries -1...-N point to singleton nodes, while
                 positive entries 1...(N-1) point to nodes which are themselves
                 parents of other nodes.
     node_size : array of node sizes - makes it easier
     order     : output array of size N

     Runtime: Θ(N)
  */
  auto_array_ptr<pos_node> queue(N/2);

  int parent;
  int child;
  t_index pos = 0;

  queue[0].pos = 0;
  queue[0].node = N-2;
  t_index idx = 1;

  do {
    --idx;
    pos = queue[idx].pos;
    parent = queue[idx].node;

    // First child
    child = merge[parent];
    if (child<0) { // singleton node, write this into the 'order' array.
      order[pos] = -child;
      ++pos;
    }
    else { /* compound node: put it on top of the queue and decompose it
              in a later iteration. */
      queue[idx].pos = pos;
      queue[idx].node = child-1; // convert index-1 based to index-0 based
      ++idx;
      pos += node_size[child-1];
    }
    // Second child
    child = merge[parent+N-1];
    if (child<0) {
      order[pos] = -child;
    }
    else {
      queue[idx].pos = pos;
      queue[idx].node = child-1;
      ++idx;
    }
  } while (idx>0);
}

#define size_(r_) ( ((r_<N) ? 1 : node_size[r_-N]) )

template <const bool sorted>
void generate_R_dendrogram(int * const merge, double * const height, int * const order, cluster_result & Z2, const int N) {
  // The array "nodes" is a union-find data structure for the cluster
  // identites (only needed for unsorted cluster_result input).
  union_find nodes(sorted ? 0 : N);
  if (!sorted) {
    std::stable_sort(Z2[0], Z2[N-1]);
  }

  t_index node1, node2;
  auto_array_ptr<t_index> node_size(N-1);

  for (t_index i=0; i<N-1; ++i) {
    // Get two data points whose clusters are merged in step i.
    // Find the cluster identifiers for these points.
    if (sorted) {
      node1 = Z2[i]->node1;
      node2 = Z2[i]->node2;
    }
    else {
      node1 = nodes.Find(Z2[i]->node1);
      node2 = nodes.Find(Z2[i]->node2);
      // Merge the nodes in the union-find data structure by making them
      // children of a new node.
      nodes.Union(node1, node2);
    }
    // Sort the nodes in the output array.
    if (node1>node2) {
      t_index tmp = node1;
      node1 = node2;
      node2 = tmp;
    }
    /* Conversion between labeling conventions.
       Input:  singleton nodes 0,...,N-1
               compound nodes  N,...,2N-2
       Output: singleton nodes -1,...,-N
               compound nodes  1,...,N
    */
    merge[i]     = (node1<N) ? -static_cast<int>(node1)-1
                              : static_cast<int>(node1)-N+1;
    merge[i+N-1] = (node2<N) ? -static_cast<int>(node2)-1
                              : static_cast<int>(node2)-N+1;
    height[i] = Z2[i]->dist;
    node_size[i] = size_(node1) + size_(node2);
  }

  order_nodes(N, merge, node_size, order);
}

/*
  R interface code
*/

enum {
  METRIC_R_EUCLIDEAN = 0,
  METRIC_R_MAXIMUM   = 1,
  METRIC_R_MANHATTAN = 2,
  METRIC_R_CANBERRA  = 3,
  METRIC_R_BINARY    = 4,
  METRIC_R_MINKOWSKI = 5
};

#if HAVE_DIAGNOSTIC
#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wpadded"
#endif
class R_dissimilarity {
private:
  t_float * Xa;
  std::ptrdiff_t dim; // std::ptrdiff_t saves many statis_cast<> in products
  t_float * members;
  void (cluster_result::*postprocessfn) (const t_float) const;
  t_float postprocessarg;

  t_float (R_dissimilarity::*distfn) (const t_index, const t_index) const;
  auto_array_ptr<t_index> row_repr;
  int N;

  // no default constructor
  R_dissimilarity();
  // noncopyable
  R_dissimilarity(R_dissimilarity const &);
  R_dissimilarity & operator=(R_dissimilarity const &);

public:
  // Ignore warning about uninitialized member variables. I know what I am
  // doing here, and some member variables are only used for certain metrics.
#if HAVE_DIAGNOSTIC
#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Weffc++"
#endif
  R_dissimilarity (t_float * const X_,
                   const int N_,
                   const int dim_,
                   t_float * const members_,
                   const unsigned char method,
                   const unsigned char metric,
                   const t_float p,
                   bool make_row_repr)
    : Xa(X_),
      dim(dim_),
      members(members_),
      postprocessfn(NULL),
      postprocessarg(p),
      N(N_)
  {
    switch (method) {
    case METHOD_VECTOR_SINGLE:
      switch (metric) {
      case METRIC_R_EUCLIDEAN:
        distfn = &R_dissimilarity::sqeuclidean<false>;
        postprocessfn = &cluster_result::sqrt;
        break;
      case METRIC_R_MAXIMUM:
        distfn = &R_dissimilarity::maximum;
        break;
      case METRIC_R_MANHATTAN:
        distfn = &R_dissimilarity::manhattan;
        break;
      case METRIC_R_CANBERRA:
        distfn = &R_dissimilarity::canberra;
        break;
      case METRIC_R_BINARY:
        distfn = &R_dissimilarity::dist_binary;
        break;
      case METRIC_R_MINKOWSKI:
        distfn = &R_dissimilarity::minkowski;
        postprocessfn = &cluster_result::power;
        break;
      default:
        throw std::runtime_error(std::string("Invalid method."));
      }
      break;

    case METHOD_VECTOR_WARD:
      postprocessfn = &cluster_result::sqrtdouble;
      break;

    default:
      postprocessfn = &cluster_result::sqrt;
    }

    if (make_row_repr) {
      row_repr.init(2*N-1);
      for (t_index i=0; i<N; ++i) {
        row_repr[i] = i;
      }
    }

  }
#if HAVE_DIAGNOSTIC
#pragma GCC diagnostic pop
#endif

  inline t_float operator () (const t_index i, const t_index j) const {
    return (this->*distfn)(i,j);
  }

  inline t_float X (const t_index i, const t_index j) const {
    // "C-style" array alignment
    return Xa[i*dim+j];
  }

  inline t_float * Xptr(const t_index i, const t_index j) const {
    // "C-style" array alignment
    return Xa+i*dim+j;
  }

  void merge(const t_index i, const t_index j, const t_index newnode) const {
    merge_inplace(row_repr[i], row_repr[j]);
    row_repr[newnode] = row_repr[j];
  }

  void merge_inplace(const t_index i, const t_index j) const {
    for(t_index k=0; k<dim; ++k) {
      *(Xptr(j,k)) = (X(i,k)*members[i] + X(j,k)*members[j]) /
        (members[i]+members[j]);
    }
    members[j] += members[i];
  }

  void merge_weighted(const t_index i, const t_index j, const t_index newnode) const {
    merge_inplace_weighted(row_repr[i], row_repr[j]);
    row_repr[newnode] = row_repr[j];
  }

  void merge_inplace_weighted(const t_index i, const t_index j) const {
    t_float * const Pi = Xa+i*dim;
    t_float * const Pj = Xa+j*dim;
    for(t_index k=0; k<dim; ++k) {
      Pj[k] = (Pi[k]+Pj[k])*.5;
    }
  }

  void postprocess(cluster_result & Z2) const {
    if (postprocessfn!=NULL) {
        (Z2.*postprocessfn)(postprocessarg);
    }
  }

  double ward(t_index const i1, t_index const i2) const {
    return sqeuclidean<true>(i1,i2)*members[i1]*members[i2]/    \
      (members[i1]+members[i2]);
  }

  inline double ward_initial(t_index const i1, t_index const i2) const {
    /* In the R interface, ward_initial is the same as ward. Only the Python
       interface has two different functions here. */
    return ward(i1,i2);
  }

  // This method must not produce NaN if the input is non-NaN.
  inline static t_float ward_initial_conversion(const t_float min) {
    // identity
    return min;
  }

  double ward_extended(t_index i1, t_index i2) const {
    return ward(row_repr[i1], row_repr[i2]);
  }

  /*
    The following definitions and methods have been taken directly from
    the R source file

      /src/library/stats/src/distance.c

    in the R release 2.13.0. The code has only been adapted very slightly.
    (Unfortunately, the methods cannot be called directly in the R libraries
    since the functions are declared "static" in the above file.)

    Note to maintainers: If the code in distance.c changes in future R releases
    compared to 2.13.0, please update the definitions here, if necessary.
  */

  // translation of variable names
  #define nc dim
  #define nr N
  #define x Xa
  #define p postprocessarg

  // The code from distance.c starts here

  #define both_FINITE(a,b) (R_FINITE(a) && R_FINITE(b))
  #ifdef R_160_and_older
  #define both_non_NA both_FINITE
  #else
  #define both_non_NA(a,b) (!ISNAN(a) && !ISNAN(b))
  #endif

  /* We need two variants of the Euclidean metric: one that does not check
     for a NaN result, which is used for the initial distances, and one which
     does, for the updated distances during the clustering procedure.
  */
  // still public
  template <const bool check_NaN>
  double sqeuclidean(t_index const i1, t_index const i2) const {
    double dev, dist;
    int count, j;

    count = 0;
    dist = 0;
    double * p1 = x+i1*nc;
    double * p2 = x+i2*nc;
    for(j = 0 ; j < nc ; ++j) {
      if(both_non_NA(*p1, *p2)) {
        dev = (*p1 - *p2);
        if(!ISNAN(dev)) {
          dist += dev * dev;
          ++count;
        }
      }
      ++p1;
      ++p2;
    }
    if(count == 0) return NA_REAL;
    if(count != nc) dist /= (static_cast<double>(count)/static_cast<double>(nc));
    //return sqrt(dist);
    // we take the square root later
    if (check_NaN) {
#if HAVE_DIAGNOSTIC
#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wfloat-equal"
#endif
      if (fc_isnan(dist))
        throw(nan_error());
#if HAVE_DIAGNOSTIC
#pragma GCC diagnostic pop
#endif
    }
    return dist;
  }

  inline double sqeuclidean_extended(t_index const i1, t_index const i2) const {
    return sqeuclidean<true>(row_repr[i1], row_repr[i2]);
  }

private:
  double maximum(t_index i1, t_index i2) const {
    double dev, dist;
    int count, j;

    count = 0;
    dist = -DBL_MAX;
    double * p1 = x+i1*nc;
    double * p2 = x+i2*nc;
    for(j = 0 ; j < nc ; ++j) {
      if(both_non_NA(*p1, *p2)) {
        dev = fabs(*p1 - *p2);
        if(!ISNAN(dev)) {
          if(dev > dist)
            dist = dev;
          ++count;
        }
      }
      ++p1;
      ++p2;
    }
    if(count == 0) return NA_REAL;
    return dist;
  }

  double manhattan(t_index i1, t_index i2) const {
    double dev, dist;
    int count, j;

    count = 0;
    dist = 0;
    double * p1 = x+i1*nc;
    double * p2 = x+i2*nc;
    for(j = 0 ; j < nc ; ++j) {
      if(both_non_NA(*p1, *p2)) {
        dev = fabs(*p1 - *p2);
        if(!ISNAN(dev)) {
          dist += dev;
          ++count;
        }
      }
      ++p1;
      ++p2;
    }
    if(count == 0) return NA_REAL;
    if(count != nc) dist /= (static_cast<double>(count)/static_cast<double>(nc));
    return dist;
  }

  double canberra(t_index i1, t_index i2) const {
    double dev, dist, sum, diff;
    int count, j;

    count = 0;
    dist = 0;
    double * p1 = x+i1*nc;
    double * p2 = x+i2*nc;
    for(j = 0 ; j < nc ; ++j) {
      if(both_non_NA(*p1, *p2)) {
        sum = fabs(*p1 + *p2);
        diff = fabs(*p1 - *p2);
        if (sum > DBL_MIN || diff > DBL_MIN) {
          dev = diff/sum;
#if HAVE_DIAGNOSTIC
#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wfloat-equal"
#endif
          if(!ISNAN(dev) ||
             (!R_FINITE(diff) && diff == sum &&
              /* use Inf = lim x -> oo */ (dev = 1.))) {
            dist += dev;
            ++count;
          }
#if HAVE_DIAGNOSTIC
#pragma GCC diagnostic pop
#endif
        }
      }
      ++p1;
      ++p2;
    }
    if(count == 0) return NA_REAL;
    if(count != nc) dist /= (static_cast<double>(count)/static_cast<double>(nc));
    return dist;
  }

  double dist_binary(t_index i1, t_index i2) const {
    int total, count, dist;
    int j;

    total = 0;
    count = 0;
    dist = 0;
    double * p1 = x+i1*nc;
    double * p2 = x+i2*nc;
    for(j = 0 ; j < nc ; ++j) {
      if(both_non_NA(*p1, *p2)) {
        if(!both_FINITE(*p1, *p2)) {
          //          warning(_("treating non-finite values as NA"));
        }
        else {
#if HAVE_DIAGNOSTIC
#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wfloat-equal"
#endif
          if(*p1 || *p2) {
            ++count;
            if( ! (*p1 && *p2) ) {
              ++dist;
            }
          }
#if HAVE_DIAGNOSTIC
#pragma GCC diagnostic pop
#endif
          ++total;
        }
      }
      ++p1;
      ++p2;
    }

    if(total == 0) return NA_REAL;
    if(count == 0) return 0;
    return static_cast<double>(dist) / static_cast<double>(count);
  }

  double minkowski(t_index i1, t_index i2) const {
    double dev, dist;
    int count, j;

    count= 0;
    dist = 0;
    double * p1 = x+i1*nc;
    double * p2 = x+i2*nc;
    for(j = 0 ; j < nc ; ++j) {
      if(both_non_NA(*p1, *p2)) {
        dev = (*p1 - *p2);
        if(!ISNAN(dev)) {
          dist += R_pow(fabs(dev), p);
          ++count;
        }
      }
      ++p1;
      ++p2;
    }
    if(count == 0) return NA_REAL;
    if(count != nc) dist /= (static_cast<double>(count)/static_cast<double>(nc));
    //return R_pow(dist, 1.0/p);
    // raise to the (1/p)-th power later
    return dist;
  }

};
#if HAVE_DIAGNOSTIC
#pragma GCC diagnostic pop
#endif

void
pearson_distances_pairwise_complete_obs(
	double * const d,
	const double * const matrix,
	int const nrow,
	int const ncol
){
	std::ptrdiff_t p(0);
	t_float EX(0), EY(0), EXX(0), EYY(0), EXY(0), x(0), y(0);
	for(int col1(0), end(ncol); col1<(end-1); ++col1){
		for(int col2(col1+1); col2<end; ++col2){
			// Pearson correlation distance
			EX=0, EY=0, EXX=0, EYY=0, EXY=0, x=0, y=0;
			unsigned npairs(0);
			for(int row(0); row<nrow; ++row){
				// R indexes its arrays BY COLUMN
				x = matrix[col1*nrow+row];
				y = matrix[col2*nrow+row];
				if(ISNA(x) || ISNA(y)) continue;
				++npairs;
				EX += x;
				EY += y;
				EXX += x*x;
				EYY += y*y;
				EXY += x*y;
			}
			if(npairs<1) d[p++] = 2.0;
			else d[p++] = 1.0 - (EXY - EX*EY/npairs) / sqrt( (EXX - EX*EX/npairs)*(EYY - EY*EY/npairs) );
		}
	}
}

// variant: means by /nrow rather than /npairs when dealing with missing data
// deviates from R's pairwise.complete.obs, but highly similar.
// good results in context of particular biological validations,
// but formal correctness undetermined
void
pearson_distances_pairwise_complete_obs_variant(
	double * const d,
	const double * const matrix,
	int const nrow,
	int const ncol
){
	std::ptrdiff_t p(0);
	t_float EX(0), EY(0), EXX(0), EYY(0), EXY(0), x(0), y(0);
	for(int col1(0), end(ncol); col1<(end-1); ++col1){
		for(int col2(col1+1); col2<end; ++col2){
			// Pearson correlation distance
			EX=0, EY=0, EXX=0, EYY=0, EXY=0, x=0, y=0;
			unsigned npairs(0);
			for(int row(0); row<nrow; ++row){
				// R indexes its arrays BY COLUMN
				x = matrix[col1*nrow+row];
				y = matrix[col2*nrow+row];
				if(ISNA(x) || ISNA(y)) continue;
				++npairs;
				EX += x;
				EY += y;
				EXX += x*x;
				EYY += y*y;
				EXY += x*y;
			}
			if(npairs<1) d[p++] = 2.0;
			else d[p++] = 1.0 - (EXY - EX*EY/nrow) / sqrt( (EXX - EX*EX/nrow)*(EYY - EY*EY/nrow) );
		}
	}
}

struct Rank{
	double x;
	double y;
	double xrank;
	double yrank;
};

bool sortbyx(Rank const & a, Rank const & b){return a.x < b.x;}
bool sortbyy(Rank const & a, Rank const & b){return a.y < b.y;}

typedef std::vector<Rank> Ranks;

// numerically equivalent but much, much faster than R's version [cor(,method='spearman',use='pairwise.complete.obs')]
// code has been profiled: slow steps are sort(x) [35%], sort(y) [35%], and ranks.push_back() [15%]
// may be possible to speed this up using an unordered list (std::list?) with a faster/lighter-weight sorting method (i.e. std::list.sort())
// and/or some scary/clever/efficient use of pointers that results in less STL creation/allocation/destruction overhead
void
spearman_distances_pairwise_complete_obs(
	double * const d,
	const double * const matrix,
	int const nrow,
	int const ncol
){
	std::ptrdiff_t p(0);
	t_float x(0), y(0);
	for(int col1(0), end(ncol); col1<(end-1); ++col1){
		for(int col2(col1+1); col2<end; ++col2){
			Ranks ranks;
			unsigned nranks(0);
			for(int row(0); row<nrow; ++row){
				// R indexes its arrays BY COLUMN
				x = matrix[col1*nrow+row];
				y = matrix[col2*nrow+row];
				if(ISNA(x) || ISNA(y)) continue;
				++nranks;
				Rank rank;
				rank.x = x;
				rank.y = y;
				rank.xrank = -1;
				rank.yrank = -1;
				ranks.push_back(rank);
			}

			if(nranks<1){
				d[p++] = 2.0;
				continue;
			}

			int lastval(0), numtied(1);

			// some code repetition here,  could be refactored?
			std::sort(ranks.begin(), ranks.end(), sortbyy);
			for(size_t i(0); i<nranks; ++i){
				t_float y(ranks[i].y);
				// first value
				// ranks are 1-indexed (though this shouldn't matter?)
				if(i==0){ranks[i].yrank = i+1; lastval = y; continue;}
				// non-first value: can be a tie, a tiebreaker, or a non-tiebreaker; and also could be the last value
				// value is a tie
				if(y==lastval){ranks[i-1].yrank = -1; ++numtied;}
				// last value is a tie

				if(i+1==nranks && y==lastval){
					t_float rank(0);
					for(size_t ip(i-numtied+1); ip<=i; ++ip) rank += ip+1.0;
					rank /= numtied;
					for(size_t ip(i-numtied+1); ip<=i; ++ip) ranks[ip].yrank = rank;

				// value is not a tie
				} else if (y!=lastval) {
					ranks[i].yrank = i+1;
					// value succeeds a tie: process the previous ties
					if(numtied>1){
						t_float rank(0);
						for(size_t ip(i-numtied); ip<i; ++ip) rank += ip+1.0;
						rank /= numtied;
						for(size_t ip(i-numtied); ip<i; ++ip) ranks[ip].yrank = rank;
					}
					numtied = 1;
					lastval = y;
				}
			}

			lastval = 0; numtied=1;
			std::sort(ranks.begin(), ranks.end(), sortbyx);
			for(size_t i(0); i<nranks; ++i){
				t_float x(ranks[i].x);
				// first value
				// ranks are 1-indexed (though this shouldn't matter?)
				if(i==0){ranks[i].xrank = i+1; lastval = x; continue;}
				// non-first value: can be a tie, a tiebreaker, or a non-tiebreaker; and also could be the last value
				// value is a tie
				if(x==lastval){ranks[i-1].xrank = -1; ++numtied;}
				// last value is a tie

				if(i+1==nranks && x==lastval){
					t_float rank(0);
					for(size_t ip(i-numtied+1); ip<=i; ++ip) rank += ip+1.0;
					rank /= numtied;
					for(size_t ip(i-numtied+1); ip<=i; ++ip) ranks[ip].xrank = rank;

				// value is not a tie
				} else if (x!=lastval) {
					ranks[i].xrank = i+1;
					// value succeeds a tie: process the previous ties
					if(numtied>1){
						t_float rank(0);
						for(size_t ip(i-numtied); ip<i; ++ip) rank += ip+1.0;
						rank /= numtied;
						for(size_t ip(i-numtied); ip<i; ++ip) ranks[ip].xrank = rank;
					}
					numtied = 1;
					lastval = x;
				}
			}

/*
			std::string output = "ranks are:\n";
			for(size_t i(0); i<ranks.size(); ++i)
				output += " i " + std::to_string(i)
								+ " x " + std::to_string(ranks[i].x)
								+ " y " + std::to_string(ranks[i].y)
								+ " xrank " + std::to_string(ranks[i].xrank)
								+ " yrank " + std::to_string(ranks[i].yrank)
								+ "\n";
			Rf_error(output.c_str());
*/

			// Pearson distance for the ranks (this is the standard, as in R)
			t_float EX=0, EY=0, EXX=0, EYY=0, EXY=0;
			for(size_t i(0); i<ranks.size(); ++i){
				EX += ranks[i].xrank;
				EY += ranks[i].yrank;
				EXX += ranks[i].xrank * ranks[i].xrank;
				EYY += ranks[i].yrank * ranks[i].yrank;
				EXY += ranks[i].xrank * ranks[i].yrank;
			}

			d[p++] = 1.0 - (EXY - EX*EY/nranks) / sqrt( (EXX - EX*EX/nranks)*(EYY - EY*EY/nranks) );

		}
	}
}

extern "C" {

	// for obtaining a fast empirical distribution of mean values for randomly sampled 'clusters'
	SEXP emp_means(SEXP matrix_, SEXP nrow_, SEXP const cols_, SEXP nsample_, SEXP niter_){
		SEXP means = NULL;
		try{
			srand(time(NULL));

			PROTECT(nrow_ = AS_INTEGER(nrow_));
			int const nrow = *INTEGER_POINTER(nrow_);
			UNPROTECT(1);

			PROTECT(cols_);
			int * const cols = INTEGER_POINTER(cols_);
			int const ncol = LENGTH(cols_);

			PROTECT(nsample_ = AS_INTEGER(nsample_));
			int const nsample = *INTEGER_POINTER(nsample_);
			UNPROTECT(1);

			PROTECT(niter_ = AS_INTEGER(niter_));
			int const niter = *INTEGER_POINTER(niter_);
			UNPROTECT(1);

      PROTECT(matrix_ = AS_NUMERIC(matrix_));
      const double * const matrix = NUMERIC_POINTER(matrix_);

      PROTECT(means = NEW_NUMERIC(niter));
      double * const meansp = NUMERIC_POINTER(means);
			t_float val(0), sum(0);
			int row(0), i(0), j(0);

			for(int iter(0); iter<niter; ++iter){
				// compute mean over nsample rows for column indices cols
				// R matrices are filled BY COLUMN
				sum=0;
				for(i=0; i<nsample; ++i){
					row = rand() % nrow;
					for(j=0; j<ncol; ++j){
						// R is 1-indexed
						val = matrix[row+nrow*(cols[j]-1)];
						if(ISNA(val)) continue;
						sum += val;
					}
				}
				meansp[iter] = sum/nsample/ncol;
			}
			UNPROTECT(1); // matrix_
			UNPROTECT(1); // cols_

			UNPROTECT(1); // means
		}
    catch (const std::bad_alloc&) {
      Rf_error( "Memory overflow.");
    }
    catch(const std::exception& e){
      Rf_error( e.what() );
    }
    catch(const nan_error&){
      Rf_error("NaN dissimilarity value.");
    }
    catch(...){
      Rf_error( "C++ exception (unknown reason)." );
    }

		return means;
	}

	// for obtaining a fast empirical distribution of mean differences between two sets of columns for randomly sampled 'clusters'
	SEXP emp_diffs(SEXP matrix_, SEXP nrow_, SEXP const colsA_, SEXP const colsB_, SEXP nsample_, SEXP niter_){
		SEXP diffs = NULL;
		try{
			srand(time(NULL));

			PROTECT(nrow_ = AS_INTEGER(nrow_));
			int const nrow = *INTEGER_POINTER(nrow_);
			UNPROTECT(1);

			PROTECT(colsA_);
			int * const colsA = INTEGER_POINTER(colsA_);
			int const ncolA = LENGTH(colsA_);

			PROTECT(colsB_);
			int * const colsB = INTEGER_POINTER(colsB_);
			int const ncolB = LENGTH(colsB_);

			PROTECT(nsample_ = AS_INTEGER(nsample_));
			int const nsample = *INTEGER_POINTER(nsample_);
			UNPROTECT(1);

			PROTECT(niter_ = AS_INTEGER(niter_));
			int const niter = *INTEGER_POINTER(niter_);
			UNPROTECT(1);

      PROTECT(matrix_ = AS_NUMERIC(matrix_));
      const double * const matrix = NUMERIC_POINTER(matrix_);

      PROTECT(diffs = NEW_NUMERIC(niter));
      double * const diffsp = NUMERIC_POINTER(diffs);
			t_float val(0), diff(0), sumA(0), sumB(0);
			int row(0), i(0), j(0);

			for(int iter(0); iter<niter; ++iter){
				// compute mean over nsample rows for column indices colsA
				// R matrices are filled BY COLUMN
				diff=0;
				for(i=0; i<nsample; ++i){
					row = rand() % nrow;
					sumA=0; sumB=0;
					for(j=0; j<ncolA; ++j){
						// R is 1-indexed
						val = matrix[row+nrow*(colsA[j]-1)];
						if(ISNA(val)) continue;
						sumA += val;
					}
					for(j=0; j<ncolB; ++j){
						val = matrix[row+nrow*(colsB[j]-1)];
						if(ISNA(val)) continue;
						sumB += val;
					}
					diff += sumB/ncolB - sumA/ncolA;
				}
				diffsp[iter] = diff/nsample;
			}
			UNPROTECT(1); // matrix_
			UNPROTECT(1); // colsA_
			UNPROTECT(1); // colsB_

			UNPROTECT(1); // diffs
		}
    catch (const std::bad_alloc&) {
      Rf_error( "Memory overflow.");
    }
    catch(const std::exception& e){
      Rf_error( e.what() );
    }
    catch(const nan_error&){
      Rf_error("NaN dissimilarity value.");
    }
    catch(...){
      Rf_error( "C++ exception (unknown reason)." );
    }

		return diffs;
	}

	// for obtaining a fast empirical distribution of mean differences between two sets of columns for randomly sampled 'clusters'
	SEXP emp_diffs_meanratio(SEXP matrix_, SEXP nrow_, SEXP const colsA_, SEXP const colsB_, SEXP nsample_, SEXP niter_){
		SEXP diffs = NULL;
		try{
			srand(time(NULL));

			PROTECT(nrow_ = AS_INTEGER(nrow_));
			int const nrow = *INTEGER_POINTER(nrow_);
			UNPROTECT(1);

			PROTECT(colsA_);
			int * const colsA = INTEGER_POINTER(colsA_);
			int const ncolA = LENGTH(colsA_);

			PROTECT(colsB_);
			int * const colsB = INTEGER_POINTER(colsB_);
			int const ncolB = LENGTH(colsB_);

			PROTECT(nsample_ = AS_INTEGER(nsample_));
			int const nsample = *INTEGER_POINTER(nsample_);
			UNPROTECT(1);

			PROTECT(niter_ = AS_INTEGER(niter_));
			int const niter = *INTEGER_POINTER(niter_);
			UNPROTECT(1);

      PROTECT(matrix_ = AS_NUMERIC(matrix_));
      const double * const matrix = NUMERIC_POINTER(matrix_);

      PROTECT(diffs = NEW_NUMERIC(niter));
      double * const diffsp = NUMERIC_POINTER(diffs);
			t_float val(0), sumratio(0), sumA(0), sumB(0);
			int row(0), i(0), j(0);

			//niter is the size of the background distribution being calculated
			for(int iter(0); iter<niter; ++iter){
				// compute mean over nsample rows for column indices colsA and colsB
				// R matrices are filled BY COLUMN
				// each i is a single row, nsample is the number of random row/elements/genes sampled
				sumratio=0;
				for(i=0; i<nsample; ++i){
					row = rand() % nrow;
					sumA=0; sumB=0;
					for(j=0; j<ncolA; ++j){
						// R is 1-indexed, so subtract 1 to get correct index in the values array
						val = matrix[row+nrow*(colsA[j]-1)];
						if(ISNA(val)) continue;
						// summing all values over colsA ('condition A') for all sampled rows
						sumA += val;
					}
					for(j=0; j<ncolB; ++j){
						val = matrix[row+nrow*(colsB[j]-1)];
						if(ISNA(val)) continue;
						// summing all values over colsB ('condition B') for all sampled rows
						sumB += val;
					}
					// rearranged fraction since multiplication is considered faster than division
					sumratio += (sumB*ncolA)/(sumA*ncolB);
				}
				// this is the mean intra-row ratio between colsA and colsB over nsample rows
				diffsp[iter] = sumratio / nsample;
			}
			UNPROTECT(1); // matrix_
			UNPROTECT(1); // colsA_
			UNPROTECT(1); // colsB_

			UNPROTECT(1); // diffs
		}
    catch (const std::bad_alloc&) {
      Rf_error( "Memory overflow.");
    }
    catch(const std::exception& e){
      Rf_error( e.what() );
    }
    catch(const nan_error&){
      Rf_error("NaN dissimilarity value.");
    }
    catch(...){
      Rf_error( "C++ exception (unknown reason)." );
    }

		return diffs;
	}

  SEXP fastcluster(SEXP const N_, SEXP const method_, SEXP D_, SEXP members_) {
    SEXP r = NULL; // return value

    try{
      /*
        Input checks
      */
      // Parameter N: number of data points
      PROTECT(N_);
      if (!IS_INTEGER(N_) || LENGTH(N_)!=1)
        Rf_error("'N' must be a single integer.");
      const int N = *INTEGER_POINTER(N_);
      if (N<2)
        Rf_error("N must be at least 2.");
      const std::ptrdiff_t NN = static_cast<std::ptrdiff_t>(N)*(N-1)/2;
      UNPROTECT(1); // N_

      // Parameter method: dissimilarity index update method
      PROTECT(method_);
      if (!IS_INTEGER(method_) || LENGTH(method_)!=1)
        Rf_error("'method' must be a single integer.");
      const int method = *INTEGER_POINTER(method_) - 1; // index-0 based;
      if (method<METHOD_METR_SINGLE || method>METHOD_METR_MEDIAN) {
        Rf_error("Invalid method index.");
      }
      UNPROTECT(1); // method_

      // Parameter members: number of members in each node
      auto_array_ptr<t_float> members;
      if (method==METHOD_METR_AVERAGE ||
          method==METHOD_METR_WARD ||
          method==METHOD_METR_CENTROID) {
        members.init(N);
        if (Rf_isNull(members_)) {
          for (t_index i=0; i<N; ++i) members[i] = 1;
        }
        else {
          PROTECT(members_ = AS_NUMERIC(members_));
          if (LENGTH(members_)!=N)
            Rf_error("'members' must have length N.");
          const t_float * const m = NUMERIC_POINTER(members_);
          for (t_index i=0; i<N; ++i) members[i] = m[i];
          UNPROTECT(1); // members
        }
      }

      // Parameter D_: dissimilarity matrix
      PROTECT(D_ = AS_NUMERIC(D_));
      if (LENGTH(D_)!=NN)
        Rf_error("'D' must have length (N \\choose 2).");
      const double * const D = NUMERIC_POINTER(D_);
      // Make a working copy of the dissimilarity array
      // for all methods except "single".
      auto_array_ptr<double> D__;
      if (method!=METHOD_METR_SINGLE) {
        D__.init(NN);
        for (std::ptrdiff_t i=0; i<NN; ++i)
          D__[i] = D[i];
      }
      UNPROTECT(1); // D_

      /*
        Clustering step
      */
      cluster_result Z2(N-1);
      switch (method) {
      case METHOD_METR_SINGLE:
        MST_linkage_core(N, D, Z2);
        break;
      case METHOD_METR_COMPLETE:
        NN_chain_core<METHOD_METR_COMPLETE, t_float>(N, D__, NULL, Z2);
        break;
      case METHOD_METR_AVERAGE:
        NN_chain_core<METHOD_METR_AVERAGE, t_float>(N, D__, members, Z2);
        break;
      case METHOD_METR_WEIGHTED:
        NN_chain_core<METHOD_METR_WEIGHTED, t_float>(N, D__, NULL, Z2);
        break;
      case METHOD_METR_WARD:
        NN_chain_core<METHOD_METR_WARD, t_float>(N, D__, members, Z2);
        break;
      case METHOD_METR_CENTROID:
        generic_linkage<METHOD_METR_CENTROID, t_float>(N, D__, members, Z2);
        break;
      case METHOD_METR_MEDIAN:
        generic_linkage<METHOD_METR_MEDIAN, t_float>(N, D__, NULL, Z2);
        break;
      default:
        throw std::runtime_error(std::string("Invalid method."));
      }

      D__.free();     // Free the memory now
      members.free(); // (not strictly necessary).

      SEXP m; // return field "merge"
      PROTECT(m = NEW_INTEGER(2*(N-1)));
      int * const merge = INTEGER_POINTER(m);

      SEXP dim_m; // Specify that m is an (N-1)×2 matrix
      PROTECT(dim_m = NEW_INTEGER(2));
      INTEGER(dim_m)[0] = N-1;
      INTEGER(dim_m)[1] = 2;
      SET_DIM(m, dim_m);

      SEXP h; // return field "height"
      PROTECT(h = NEW_NUMERIC(N-1));
      double * const height = NUMERIC_POINTER(h);

      SEXP o; // return fiels "order'
      PROTECT(o = NEW_INTEGER(N));
      int * const order = INTEGER_POINTER(o);

      if (method==METHOD_METR_CENTROID ||
          method==METHOD_METR_MEDIAN)
        generate_R_dendrogram<true>(merge, height, order, Z2, N);
      else
        generate_R_dendrogram<false>(merge, height, order, Z2, N);

      SEXP n; // names
      PROTECT(n = NEW_CHARACTER(3));
      SET_STRING_ELT(n, 0, COPY_TO_USER_STRING("merge"));
      SET_STRING_ELT(n, 1, COPY_TO_USER_STRING("height"));
      SET_STRING_ELT(n, 2, COPY_TO_USER_STRING("order"));

      PROTECT(r = NEW_LIST(3)); // field names in the output list
      SET_ELEMENT(r, 0, m);
      SET_ELEMENT(r, 1, h);
      SET_ELEMENT(r, 2, o);
      SET_NAMES(r, n);

      UNPROTECT(6); // m, dim_m, h, o, r, n
    } // try
    catch (const std::bad_alloc&) {
      Rf_error( "Memory overflow.");
    }
    catch(const std::exception& e){
      Rf_error( e.what() );
    }
    catch(const nan_error&){
      Rf_error("NaN dissimilarity value.");
    }
    #ifdef FE_INVALID
    catch(const fenv_error&){
      Rf_error( "NaN dissimilarity value in intermediate results.");
    }
    #endif
    catch(...){
      Rf_error( "C++ exception (unknown reason)." );
    }

    return r;
  }

  SEXP fastcluster_vector(SEXP const method_,
                          SEXP const metric_,
                          SEXP X_,
                          SEXP members_,
                          SEXP p_) {
    SEXP r = NULL; // return value
    try{

      /*
        Input checks
      */

      // Parameter method: dissimilarity index update method
      PROTECT(method_);
      if (!IS_INTEGER(method_) || LENGTH(method_)!=1)
        Rf_error("'method' must be a single integer.");
      int method = *INTEGER_POINTER(method_) - 1; // index-0 based;
      if (method<METHOD_VECTOR_SINGLE || method>METHOD_VECTOR_MEDIAN) {
        Rf_error("Invalid method index.");
      }
      UNPROTECT(1); // method_

      // Parameter metric
      PROTECT(metric_);
      if (!IS_INTEGER(metric_) || LENGTH(metric_)!=1)
        Rf_error("'metric' must be a single integer.");
      int metric = *INTEGER_POINTER(metric_) - 1; // index-0 based;
      if (metric<0 || metric>5 ||
          (method!=METHOD_VECTOR_SINGLE && metric!=0) ) {
        Rf_error("Invalid metric index.");
      }
      UNPROTECT(1); // metric_

      // data array
      PROTECT(X_ = AS_NUMERIC(X_));
      SEXP dims_ = PROTECT( Rf_getAttrib( X_, R_DimSymbol ) ) ;
      if( dims_ == R_NilValue || LENGTH(dims_) != 2 ) {
        Rf_error( "Argument is not a matrix.");
      }
      const int * const dims = INTEGER(dims_);
      const int N = dims[0];
      const int dim = dims[1];
      if (N<2)
        Rf_error("There must be at least two data points.");
      // Make a working copy of the dissimilarity array
      // for all methods except "single".
      double * X__ = NUMERIC_POINTER(X_);
      // Copy the input array and change it from Fortran-contiguous  style
      // to C-contiguous style
      // (Waste of memory for 'single'; the other methods need a copy
      auto_array_ptr<double> X(LENGTH(X_));
      for (std::ptrdiff_t i=0; i<N; ++i)
        for (std::ptrdiff_t j=0; j<dim; ++j)
          X[i*dim+j] = X__[i+j*N];

      UNPROTECT(2); // dims_, X_

      // Parameter members: number of members in each node
      auto_array_ptr<t_float> members;
      if (method==METHOD_VECTOR_WARD ||
          method==METHOD_VECTOR_CENTROID) {
        members.init(N);
        if (Rf_isNull(members_)) {
          for (t_index i=0; i<N; ++i) members[i] = 1;
        }
        else {
          PROTECT(members_ = AS_NUMERIC(members_));
          if (LENGTH(members_)!=N)
            Rf_error("The length of 'members' must be the same as the number of data points.");
          const t_float * const m = NUMERIC_POINTER(members_);
          for (t_index i=0; i<N; ++i) members[i] = m[i];
          UNPROTECT(1); // members
        }
      }

      // Parameter p
      PROTECT(p_);
      double p = 0;
      if (metric==METRIC_R_MINKOWSKI) {
        if (!IS_NUMERIC(p_) || LENGTH(p_)!=1)
          Rf_error("'p' must be a single floating point number.");
        p = *NUMERIC_POINTER(p_);
      }
      else {
        if (p_ != R_NilValue) {
          Rf_error("No metric except 'minkowski' allows a 'p' parameter.");
        }
      }
      UNPROTECT(1); // p_

      /* The generic_linkage_vector_alternative algorithm uses labels
         N,N+1,... for the new nodes, so we need a table which node is
         stored in which row.

         Instructions: Set this variable to true for all methods which
         use the generic_linkage_vector_alternative algorithm below.
      */
      bool make_row_repr =
        (method==METHOD_VECTOR_CENTROID || method==METHOD_VECTOR_MEDIAN);

      R_dissimilarity dist(X, N, dim, members,
                           static_cast<unsigned char>(method),
                           static_cast<unsigned char>(metric),
                           p,
                           make_row_repr);
      cluster_result Z2(N-1);

      /*
        Clustering step
      */
      switch (method) {
      case METHOD_VECTOR_SINGLE:
        MST_linkage_core_vector(N, dist, Z2);
        break;

      case METHOD_VECTOR_WARD:
        generic_linkage_vector<METHOD_METR_WARD>(N, dist, Z2);
        break;

      case METHOD_VECTOR_CENTROID:
        generic_linkage_vector_alternative<METHOD_METR_CENTROID>(N, dist, Z2);
        break;

      case METHOD_VECTOR_MEDIAN:
        generic_linkage_vector_alternative<METHOD_METR_MEDIAN>(N, dist, Z2);
        break;

      default:
        throw std::runtime_error(std::string("Invalid method."));
      }

      X.free();     // Free the memory now
      members.free(); // (not strictly necessary).

      dist.postprocess(Z2);

      SEXP m; // return field "merge"
      PROTECT(m = NEW_INTEGER(2*(N-1)));
      int * const merge = INTEGER_POINTER(m);

      SEXP dim_m; // Specify that m is an (N-1)×2 matrix
      PROTECT(dim_m = NEW_INTEGER(2));
      INTEGER(dim_m)[0] = N-1;
      INTEGER(dim_m)[1] = 2;
      SET_DIM(m, dim_m);

      SEXP h; // return field "height"
      PROTECT(h = NEW_NUMERIC(N-1));
      double * const height = NUMERIC_POINTER(h);

      SEXP o; // return fiels "order'
      PROTECT(o = NEW_INTEGER(N));
      int * const order = INTEGER_POINTER(o);

      if (method==METHOD_VECTOR_SINGLE)
        generate_R_dendrogram<false>(merge, height, order, Z2, N);
      else
        generate_R_dendrogram<true>(merge, height, order, Z2, N);

      SEXP n; // names
      PROTECT(n = NEW_CHARACTER(3));
      SET_STRING_ELT(n, 0, COPY_TO_USER_STRING("merge"));
      SET_STRING_ELT(n, 1, COPY_TO_USER_STRING("height"));
      SET_STRING_ELT(n, 2, COPY_TO_USER_STRING("order"));

      PROTECT(r = NEW_LIST(3)); // field names in the output list
      SET_ELEMENT(r, 0, m);
      SET_ELEMENT(r, 1, h);
      SET_ELEMENT(r, 2, o);
      SET_NAMES(r, n);

      UNPROTECT(6); // m, dim_m, h, o, r, n
    } // try
    catch (const std::bad_alloc&) {
      Rf_error( "Memory overflow.");
    }
    catch(const std::exception& e){
      Rf_error( e.what() );
    }
    catch(const nan_error&){
      Rf_error("NaN dissimilarity value.");
    }
    catch(...){
      Rf_error( "C++ exception (unknown reason)." );
    }

    return r;
  }

  SEXP fastcluster_correlation_distances(SEXP matrix_, SEXP const nrow_, SEXP const ncol_, SEXP const type_) {
    SEXP distance = NULL; // return value

    try{
			PROTECT(type_);
			const int type = *INTEGER_POINTER(type_);
			UNPROTECT(1); // type_

      PROTECT(nrow_);
      if (!IS_INTEGER(nrow_) || LENGTH(nrow_)!=1) Rf_error("'nrow' must be a single integer.");
      const int nrow = *INTEGER_POINTER(nrow_);
      UNPROTECT(1); // nrow_
      if (nrow<2) Rf_error("nrow must be at least 2.");

      PROTECT(ncol_);
      if (!IS_INTEGER(ncol_) || LENGTH(ncol_)!=1) Rf_error("'ncol' must be a single integer.");
      const int ncol = *INTEGER_POINTER(ncol_);
      UNPROTECT(1); // ncol_
      if (ncol<2) Rf_error("ncol must be at least 2.");

      const std::ptrdiff_t N = static_cast<std::ptrdiff_t>(nrow*ncol);

      PROTECT(matrix_ = AS_NUMERIC(matrix_));
      if (LENGTH(matrix_)!=N) Rf_error("Improperly specified matrix dimensions.");
      const double * const matrix = NUMERIC_POINTER(matrix_);

			// R defaults to by-column comparisons
			const std::ptrdiff_t dsize = static_cast<std::ptrdiff_t>((ncol)*(ncol-1)/2);

      PROTECT(distance = NEW_NUMERIC(dsize));
      double * const d = NUMERIC_POINTER(distance);

			if(type==2) pearson_distances_pairwise_complete_obs_variant(d, matrix, nrow, ncol);
			else if(type==3) spearman_distances_pairwise_complete_obs(d, matrix, nrow, ncol);
			else pearson_distances_pairwise_complete_obs(d, matrix, nrow, ncol);

      UNPROTECT(2); // matrix_ and distance

    } // try
    catch (const std::bad_alloc&) {
      Rf_error( "Memory overflow.");
    }
    catch(const std::exception& e){
      Rf_error( e.what() );
    }
    catch(const nan_error&){
      Rf_error("NaN dissimilarity value.");
    }
    catch(...){
      Rf_error( "C++ exception (unknown reason)." );
    }

    return distance;
  }

#if HAVE_VISIBILITY
#pragma GCC visibility push(default)
#endif
  void R_init_fastcluster(DllInfo * const info)
  {
    R_CallMethodDef callMethods[]  = {
      {"fastcluster", (DL_FUNC) &fastcluster, 4},
			{"emp_means", (DL_FUNC) &emp_means, 5},
			{"emp_diffs", (DL_FUNC) &emp_diffs, 6},
			{"emp_diffs_meanratio", (DL_FUNC) &emp_diffs_meanratio, 6},
      {"fastcluster_correlation_distances", (DL_FUNC) &fastcluster_correlation_distances, 4},
      {"fastcluster_vector", (DL_FUNC) &fastcluster_vector, 5},
      {NULL, NULL, 0}
    };
    R_registerRoutines(info, NULL, callMethods, NULL, NULL);
  }
#if HAVE_VISIBILITY
#pragma GCC visibility pop
#endif

} // extern "C"

#if HAVE_VISIBILITY
#pragma GCC visibility pop
#endif
