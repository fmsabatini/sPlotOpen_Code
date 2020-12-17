// To enable the functionality provided by Armadillo's various macros,

// simply include them before you include the RcppArmadillo headers.

#define ARMA_NO_DEBUG

// [[Rcpp::depends(RcppArmadillo, BH, bigmemory,RcppParallel)]]

// [[Rcpp::plugins(cpp11)]]

#include <RcppArmadillo.h>

#include <bigmemory/BigMatrix.h>

#include <Rcpp.h> 

#include <RcppParallel.h>

using namespace Rcpp;

using namespace arma;

using namespace RcppParallel;


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

struct JtuDistance : public Worker {
   
   // input matrix to read from
   
   const RMatrix<int> mat;
   
   // output matrix to write to
   
   RMatrix<double> rmat;
   
   // initialize from Rcpp input and output matrixes (the RMatrix class
   
   // can be automatically converted to from the Rcpp matrix type)
   
   JtuDistance(const IntegerMatrix mat, NumericMatrix rmat)
      : mat(mat), rmat(rmat) {}
   
   // function call operator that work for the specified range (begin/end)
   
   void operator()(std::size_t begin, std::size_t end) {
      
	    for (std::size_t i = begin; i < end; i++) {
         
			for (std::size_t j = 0; j < i; j++) {
            
            // rows we will operate on
            
				RMatrix<int>::Row row1 = mat.row(i);
            
				RMatrix<int>::Row row2 = mat.row(j);
			
				rmat(j,i) = JacElement(row1.begin(), row1.end(), row2.begin()); 
				
				rmat(i,j) = rmat(j,i);
				
			}
			
		}
	
	}
	
};
			
// [[Rcpp::export]]	

NumericMatrix betaJtu_RcppParallel(IntegerMatrix mat) {
  
   // allocate the matrix we will return
   NumericMatrix rmat(mat.nrow(), mat.nrow());

   // create the worker
   JtuDistance JtuDistance(mat, rmat);
     
   // call it with parallelFor
   parallelFor(0, mat.nrow(), JtuDistance);

   	
   return rmat;
}