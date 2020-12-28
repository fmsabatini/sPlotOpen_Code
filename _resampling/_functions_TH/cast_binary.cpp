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

IntegerVector whichEq( IntegerVector x, int value) {

    int nx = x.size();
    std::vector<int> y;
    y.reserve(nx);

    for(int i = 0; i < nx; i++) {
        if (x[i] == value) y.push_back(i);
    }

    return wrap(y);
}


// [[Rcpp::export]]

IntegerMatrix castC(IntegerVector iD, IntegerVector sp , NumericVector cov){
	
	int sizeI = iD.size(); 
	
	IntegerVector plotNb = unique(iD);
	
	IntegerVector spNb = unique(sp);
	
	int rowSize = plotNb.size(); 
	
	int colSize= spNb.size() ;
	
	IntegerMatrix comMat(rowSize, colSize +1); 
	
	comMat(_ , 0) = plotNb;
	
	for (int i = 0; i < sizeI ; i++) {
		
	IntegerVector rowI = whichEq (plotNb,iD[i]);
	
	int plotID = rowI[0]; 
	
	int species = sp[i];
	
	comMat(plotID, species) = 1;
	
	}
		
	return comMat;
		
}
