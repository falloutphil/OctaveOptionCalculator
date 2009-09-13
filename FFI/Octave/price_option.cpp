#include <iostream>

#include <octave/oct.h>
#include <octave/dMatrix.h>

#include "CInterface.h"

using namespace std;

DEFUN_DLD(price_option, args, , "Option Pricer")
{
  if (args.length() != 11)
  {
    cout<<"see : help price_option";
    return octave_value();
  }

  const Matrix matUnderl = args(0).matrix_value();
  const double strk      = args(1).double_value();
  const double vol       = args(2).double_value();
  const Matrix matExpy   = args(3).matrix_value();
  const double ir        = args(4).double_value();
  const int    ts        = args(5).int_value();
  const int    sims      = args(6).int_value();
  // string_value() returns by value so need a
  // a string placeholder to return a char* to (see below).
  const string putCallStr = args(7).string_value();
  const string rngStr     = args(8).string_value();
  const string normStr    = args(9).string_value();
  const string instrStr   = args(10).string_value();
  
  const dim_vector& resDim = matUnderl.dims();

  if ( resDim  == matExpy.dims() )
  {
    Matrix matResult(resDim);
    // const_cast is naughty but safe here - Haskell won't mutate
    double* const arrayOfUnderlyings = const_cast<double* const>(matUnderl.fortran_vec());
    double* const arrayOfExpiries    = const_cast<double* const>(matExpy.fortran_vec());
    
    // Call into our C interface
    // We go Octave -> C++ -> C -> Haskell
    const double* const arrayOfResults  = pricerInterface( arrayOfUnderlyings,
			  	  	                   matUnderl.nelem(),
							   strk,
							   vol,
							   arrayOfExpiries,
							   ir,
							   ts,
							   sims,
							   const_cast<char* const>(putCallStr.c_str()),
							   const_cast<char* const>(rngStr.c_str()),
							   const_cast<char* const>(normStr.c_str()),
							   const_cast<char* const>(instrStr.c_str()) );
     
    
    // Need to copy results into matrix
    for(unsigned int col=0; col < resDim(1); ++col)
    {
      for(unsigned int row=0; row < resDim(0); ++row)
      {
	matResult(row,col) = arrayOfResults[(col*resDim(0))+row];
      }
    }
    
    // Clean up Haskell allocated memory
    delete[] arrayOfResults;

    return octave_value(matResult);
  }
  else
  {
    cout << "\nStock and Expiry matrices must be the same size";
    cout << "\nStock: (" << resDim(0) << "," << resDim(1) << ")";
    cout << "\nExiry: (" << matExpy.dims()(0) << "," << matExpy.dims()(1) << ")";
  }

  return octave_value();
}
