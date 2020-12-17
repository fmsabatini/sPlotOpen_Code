// To enable the functionality provided by Armadillo's various macros,

// simply include them before you include the RcppArmadillo headers.

#define ARMA_NO_DEBUG

// [[Rcpp::depends(RcppArmadillo, BH, bigmemory)]]

// [[Rcpp::plugins(cpp11)]]

// [[Rcpp::plugins(openmp)]]

#include <RcppArmadillo.h>

#include <bigmemory/BigMatrix.h>

#include <Rcpp.h> 

#include <omp.h>


using namespace Rcpp;

using namespace arma;

template <typename InputIterator1, typename InputIterator2>

inline double  BrayElement(InputIterator1 begin1, InputIterator1 end1, InputIterator2 begin2){
	
	// value to return
	
	double rval = 0;
	
	double sumPi = 0;
	
	double sumPj = 0;
	
	double aComp = 0;
	
	// set iterators to beginning of ranges

	InputIterator1 it1 = begin1;
	
	InputIterator2 it2 = begin2;

	// for each input item
	
	while (it1 != end1) {
		
		// take the value and increment the iterator
		
		double d1 = *it1++;
		
		double d2 = *it2++;
		
		sumPi += d1;
		
		sumPj += d2;
		
		aComp += std::min(d1,d2);
		
	}
			
	double bComp = sumPi - aComp;
			
	double cComp = sumPj - aComp;
			
	double minBC = 0.0;
			 
	if (bComp < cComp) { 
			 
		minBC = bComp;
			 
	}
			 
	else {
			 
		minBC = cComp; 
			 
	}
			 
rval = minBC / (aComp + minBC);

return rval;	
}


 void BrayDistance(const arma::Mat<double>& mat, arma::Mat<double> rmat){
	 
	int nRows = mat.n_rows;
  
    omp_set_num_threads(10);
	
	#pragma omp parallel for  
	
    for (int i = 0; i < nRows; i++) {
         
			for (int j = 0; j < nRows; j++) {
            
				arma::Row<double> row1 = mat.row(i);
            
				arma::Row<double> row2 = mat.row(j);
			
				rmat(i,j) = BrayElement(row1.begin(), row1.end(), row2.begin()); 
				
			}
			
	}
	
rmat =   rmat.t();	

}
	
// [[Rcpp::export]]	

void bray_distance_OpenMP(SEXP mat, SEXP rmat) {
 
	XPtr<BigMatrix> xpMat(mat);

	XPtr<BigMatrix> xpOutMat(rmat);
	
   // create the worker
    BrayDistance(arma::Mat<double>((double *)xpMat->matrix(), xpMat->nrow(), xpMat->ncol(), false),
							arma::Mat<double>((double *)xpOutMat->matrix(), xpOutMat->nrow(), xpOutMat->ncol(), false));

}
