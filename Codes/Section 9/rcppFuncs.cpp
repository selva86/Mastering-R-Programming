#include <Rcpp.h>
using namespace Rcpp;


// [[Rcpp::export]]
NumericVector timesTwo(NumericVector x) {
  return x * 2;
}


// You can include R code blocks in C++ files processed with sourceCpp
// (useful for testing and development). The R code will be automatically 
// run after the compilation.
//

/*** R
# timesTwo(42)
*/

// Handle Multiple Datatypes -----------------------------------------------------------------------
//[[Rcpp::export]]
SEXP timesTwoNew(SEXP x) {
  switch(TYPEOF(x)){
  
  case REALSXP: { // Input is numeric
    NumericVector holding = as<NumericVector>(x) * 2;
    return wrap(holding);
  }
    
  case INTSXP: { // Input is integer
    IntegerVector holding = as<IntegerVector>(x) * 2;
    return wrap(holding);
  }
    
  case STRSXP: { // Input is character
    CharacterVector holding(2);  // declare output and its size
    holding[0] = as<CharacterVector>(x)[0];  // convert to character vector
    holding[1] = as<CharacterVector>(x)[0];
    return wrap(holding);
  }
    
  default: {
    Rcout << TYPEOF(x) << std::endl;  // print the number corresponding to SEXTYPE to R console.
    stop("Only numbers and characters are supported");
  }
  }
}
// https://cran.r-project.org/doc/manuals/R-ints.html#SEXPTYPEs


// Subset a Numeric Vector ------------------------------------------------------------------------
//[[Rcpp::export]]
Rcpp::NumericVector vecsub(Rcpp::NumericVector x, int start, int end) {
  NumericVector out;
  for(int i=(start-1); i<end; i++){
    out.push_back(x[i]);
  }
  return out;
}


// Create a Data.Frame from Character Vectors with columns as factors -----------------------------
// [[Rcpp::export]]
DataFrame makeDataFrame(CharacterVector x, CharacterVector y){
  return DataFrame::create(Named("a")= x, Named("b")= y);
}


// Create a Data.Frame from Numeric Vectors with columns as characters. -----------------------------
// [[Rcpp::export]]
DataFrame makeDataFrameChar(NumericVector x, 
                            NumericVector y, 
                            bool to_char=true){
  if(to_char == true){
    CharacterVector a = as<CharacterVector>(x);
    CharacterVector b = as<CharacterVector>(y);
    return DataFrame::create(Named("a")= a, Named("b")= b, 
                             Named("stringsAsFactors") = false);
  }
  
  return DataFrame::create(Named("a")= x, Named("b")= y, 
                           Named("stringsAsFactors") = false);
}


// Convert a list to a Data.Frame ------------------------------------------------------------
// [[Rcpp::export]]
List makeListToDf(List L){
  List out_df = clone(L);
  
  int len = out_df.length();
  Rcout << "Num items in input List: " << len << std::endl;  // print number of items in list
  
  GenericVector sample_row = out_df(0);
  
  // check if all items in 'a' are of same size.
  for(int i=0; i< len; i++){
    GenericVector v = out_df(i);
    if(v.size() != sample_row.size()){
      stop("Some items in list is not of same length.");
    }
  }
  
  // create and assign rownames of output dataframe.
  StringVector row_names(sample_row.length());
  for (int i = 0; i < sample_row.length(); ++i) {
    char name[5];
    sprintf(&(name[0]), "%d", i);
    row_names(i) = name;
  }
  out_df.attr("row.names") = row_names;
  
  out_df.attr("class") = "data.frame";
  return out_df;
}

// Get a row from a data.frame ----------------------------------------------------------------------
// [[Rcpp::export]]
CharacterVector getRowFromDf(DataFrame df, int n){
  int ncols = df.size();  // number of columns in df
  CharacterVector out(ncols);  // output vector with length 'ncol'
  
  // for loop to get values in nth column.
  for(int i=0; i<ncols; i++){  //counting starts from 0 in c++
    CharacterVector df_column = df[i];
    out[i] = df_column[n];
  }
  return out;
}


// Get a row from a data.frame with rowname ----------------------------------------------------------
// [[Rcpp::export]]
CharacterVector getRowFromDfWithRowname(DataFrame df, int n){
  int ncols = df.size();  // number of columns in df
  CharacterVector out;  // output vector with length 'ncol'
  
  CharacterVector rnames = df.attr("row.names");  // get row names.
  
  // assign first item in 'out' as the rowname
  out.push_back(rnames[n]);
  
  // for loop to get values in nth column.
  for(int i=0; i<ncols; i++){  //counting starts from 0 in c++
    CharacterVector df_column = df[i];
    out.push_back(df_column[n]);
  }
  return out;
}


// Benchmark Problem --------------------------------------------------------------
// [[Rcpp::export]]
CharacterVector myFunc(DataFrame x) {
  NumericVector col1 = as<NumericVector>(x["col1"]);
  NumericVector col2 = as<NumericVector>(x["col2"]);
  NumericVector col3 = as<NumericVector>(x["col3"]);
  NumericVector col4 = as<NumericVector>(x["col4"]);
  int n = col1.size();
  CharacterVector out(n);
  for (int i=0; i<n; i++) {
    double tempOut = col1[i] + col2[i] + col3[i] + col4[i];
    if (tempOut > 4){
      out[i] = "greater_than_4";
    } else {
      out[i] = "lesser_than_4";
    }
  }
  return out;
}


// Answer ----------------------------------------------------------------------
// [[Rcpp::export]]
NumericMatrix getRowsFromMat(NumericMatrix mat, NumericVector n){
  int nc=mat.ncol();                // get number of columns
  NumericMatrix out(n.size(), nc);  // declare output matrix
  n = n - 1;                        // because positioning in Rcpp begins from 0
  
  NumericVector::iterator rnum;     // loop iterator
  int rownum_out = 0;               // row number of output matrix
  for(rnum=n.begin(); rnum<n.end(); ++rnum){     // iterate through rows
    Rcout << "rnum: "<< *rnum << std::endl;      // print current row number to console.  (optional)
    for(int cnum=0; cnum<nc; ++cnum){            // iteratate through columns
      out(rownum_out, cnum) = mat(*rnum, cnum);  // assign value in output matrix. *rnum gets the value in the address.
      Rcout << "  cnum: "<< cnum;   // print current column number to console (optional)
    }
    rownum_out++;                   // increment rownumber of output mat.
    Rcout << std::endl;             // end of line (optional)
  }
  return out;
}
