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


inline double  JacElement(InputIterator1 begin1, InputIterator1 end1, InputIterator2 begin2){
	
	// value to return
	
	double rval = 0;
	
	int compA = 0;
	
	int compB = 0;
	
	int compC = 0;
	
	// set iterators to beginning of ranges

	InputIterator1 it1 = begin1;
	
	InputIterator2 it2 = begin2;

	// for each input item
	
	while (it1 != end1) {
	
	// take the value and increment the iterator
		
		double d1 = *it1++;
		
		double d2 = *it2++;
	
		if (d1 == 1 && d2 ==1)  { 
		
			compA += 1; 
		
		} 
		
		if (d1 > d2)  { 
		
			compB += 1; 
		
		} 
		
		if (d1 < d2)  { 
		
			compC += 1; 
		
		} 
		
	}
	
	if (compB < compC) {
	
		double numerator = 2 * compB;
	
		double denominator = compA + numerator;
	
		rval = numerator / denominator; 
		
	}
	
	else {
		
		double numerator = 2 * compC;
	
		double denominator = compA + numerator;
	
		rval = numerator / denominator; 
		
	}

return rval;

}

void BetaJtu (const arma::Mat<int>& mat, arma::Mat<double> rmat){
	 
	int nRows = mat.n_rows;
  
    omp_set_num_threads(10);
	
	#pragma omp parallel for  
	
    for (int i = 0; i < nRows; i++) {
         
			for (int j = 0; j < nRows; j++) {
            
				arma::Row<int> row1 = mat.row(i);
            
				arma::Row<int> row2 = mat.row(j);
			
				rmat(i,j) = JacElement(row1.begin(), row1.end(), row2.begin()); 
				
			}
			
	}
	
}


// [[Rcpp::export]]	

void betaJtu(SEXP mat, SEXP rmat) {
 
	XPtr<BigMatrix> xpMat(mat);

	XPtr<BigMatrix> xpOutMat(rmat);
	
   // create the worker
    BetaJtu(arma::Mat<int>((int *)xpMat->matrix(), xpMat->nrow(), xpMat->ncol(), false),
			
			arma::Mat<double>((double *)xpOutMat->matrix(), xpOutMat->nrow(), xpOutMat->ncol(), false));

}
