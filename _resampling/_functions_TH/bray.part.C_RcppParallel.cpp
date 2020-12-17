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

struct BrayDistance : public Worker {
   
   // input matrix to read from
   
   const RMatrix<double> mat;
   
   // output matrix to write to
   
   RMatrix<double> rmat;
   
   // initialize from Rcpp input and output matrixes (the RMatrix class
   
   // can be automatically converted to from the Rcpp matrix type)
   
   BrayDistance(const NumericMatrix mat, NumericMatrix rmat)
      : mat(mat), rmat(rmat) {}
   
   // function call operator that work for the specified range (begin/end)
   
   void operator()(std::size_t begin, std::size_t end) {
      
	    for (std::size_t i = begin; i < end; i++) {
         
			for (std::size_t j = 0; j < i; j++) {
            
            // rows we will operate on
            
				RMatrix<double>::Row row1 = mat.row(i);
            
				RMatrix<double>::Row row2 = mat.row(j);
			
				rmat(j,i) = BrayElement(row1.begin(), row1.end(), row2.begin()); 
				
				rmat(i,j) = rmat(j,i);
				
			}
			
		}
	
	}
	
};
			
// [[Rcpp::export]]	

NumericMatrix bray_distance_RcppParallel(NumericMatrix mat) {
  
   // allocate the matrix we will return
   NumericMatrix rmat(mat.nrow(), mat.nrow());

   // create the worker
   BrayDistance BrayDistance(mat, rmat);
     
   // call it with parallelFor
   parallelFor(0, mat.nrow(), BrayDistance);

   	
   return rmat;
}












