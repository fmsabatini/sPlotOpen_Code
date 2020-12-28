// To enable the functionality provided by Armadillo's various macros,

// simply include them before you include the RcppArmadillo headers.

#define ARMA_NO_DEBUG

// [[Rcpp::depends(RcppArmadillo, BH, bigmemory)]]

// [[Rcpp::plugins(cpp11)]]

#include <RcppArmadillo.h>

#include <RcppArmadilloExtensions/sample.h>

#include <bigmemory/BigMatrix.h>

#include <Rcpp.h> 

using namespace Rcpp;

using namespace arma;

//seqInt function 

IntegerVector seqInt(double x, double y, double by) {

	IntegerVector anOut(1);

	// compute sequence
	
	double min_by = 1.e-8;
	
	if (by < min_by) min_by = by/100;
	
	double i = x + by;
	
	anOut(0) = x;
	
	while(i/min_by < y/min_by + 1) { 
	
	anOut.push_back(i);
	
	i += by;
 
	}

	return anOut;

 }

// stl_sort

 NumericVector stl_sort(NumericVector x) {
   
   NumericVector y = clone(x);
   
   std::sort(y.begin(), y.end());
   
   return y;
}

// stl_sort_int

 IntegerVector stl_sort_int( IntegerVector x) {
   
   IntegerVector y = clone(x);
   
   std::sort(y.begin(), y.end());
   
   return y;
}

// HCR function 

IntegerVector Hcr(arma::Mat<double> dMat , int nout , int nsampl) {
	
	int  nplots = dMat.n_rows; 
	
	NumericVector meand( nsampl, 0.0 ); 
	
	NumericVector vard( nsampl, 0.0 ); 
	
	IntegerMatrix sel( nout, nsampl ); 
		
	arma::Mat<double> selMat( nout, nout );
	
	IntegerVector plotID = seqInt( 0 , nplots -1 , 1 );
	
	for (int i = 0 ; i < nsampl ; i++ ) {
		
		IntegerVector sampleID = Rcpp::RcppArmadillo::sample(plotID, nout, false , NumericVector::create());	
		
		sel( _ , i) = sampleID;
		
		arma::uvec permuID = as<arma::uvec>(sampleID);
		
		selMat = dMat.submat( permuID , permuID );
				
		double sum = 0;
		
		int n = 0;

		for (int j = 0 ; j < nout ; j++){
			
			for (int k = 0 ; k < nout ; k++){
			
				if (j > k ) {
					
					sum += selMat(j,k); 
					
					n += 1 ; 
					
				}	
			
			}
		}
		
		double tmpMean = sum/n ; 
		 
		double sce = 0;
		
		for (int j = 0 ; j < nout; j++){
			
			for (int k = 0 ; k < nout; k++){
			
				if (j > k ) {
					
					sce += pow(selMat(j,k) - tmpMean, 2.0);
					
				}	
			
			}
		}
		
		double tmpVar = sce / (n - 1) ; 
		
		meand[i] = tmpMean;
		
		vard[i] = tmpVar; 	
		
	}
	
	NumericVector sortdemean = stl_sort(-meand);
	
	NumericVector sortdevar = stl_sort(vard);
	
	IntegerVector rankdecmean = match(-meand, sortdemean); 
	
	IntegerVector rankdevar = match(vard, sortdevar); 
	
	IntegerVector sumVarMean = rankdecmean + rankdevar;
	
	IntegerVector sorVarMean = stl_sort_int(sumVarMean); 
	
	IntegerVector rankFinal = match(sumVarMean, sorVarMean); 
	
	int selecSample = which_min(rankFinal); 
	
	IntegerVector res = sel(_, selecSample); 
	
	return res;
		
} 

// [[Rcpp::export]]

IntegerVector HcrCPP(SEXP pInBigMat, int nout , int nsampl) {

	// First we tell Rcpp that the object we've been given is an external pointer.
 
	XPtr<BigMatrix> xpMat(pInBigMat);
  
	IntegerVector res = Hcr(arma::Mat<double>((double *)xpMat->matrix(), xpMat->nrow(), xpMat->ncol(), false), nout , nsampl);
	
	return wrap(res);
}