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
  // Some strange shit is going down here.  What
  // we want here is a char* to the strings, but
  // if we do this as a one liner all sorts of
  // strange behaviour occurs.  For whatever reason
  // storing in a string and delaying cast till later
  // seems to work (see below)!
  const string putCallStr = args(7).string_value();
  const string rngStr     = args(8).string_value();
  const string normStr    = args(9).string_value();
  const string instrStr   = args(10).string_value();
  
  const dim_vector& resDim = matUnderl.dims();

  if ( resDim  == matExpy.dims() )
  {
    Matrix matResult(resDim);
    // Const cast is naughty but safe - haskell won't mutate
    double* const arrayOfUnderlyings = const_cast<double*>(matUnderl.fortran_vec());
    double* const arrayOfExpiries    = const_cast<double*>(matExpy.fortran_vec());
    
    // Your memory use on the return is shakey! The temp is destroyed before
    // you use it - you're just lucky that this works and it is doomed to fail
    // at some point!
    // Two options as I see it.  Either pass in the result vector as a parameter
    // then we can handle destruction here.  Or if we could try to use a smart
    // pointer to kill itself once no longer referenced.  First option is better
    // as the C interface is written in C not C++, so not sure how you could
    // create a smart pointer.  You'd have to write it in C++ and expose a C
    // interface to it.  Could probably just use auto_ptr as we shouldn't copy
    // the data - init it for doubles to avoid using templates in C.
    const double* const arrayOfResults  = pricerInterface( arrayOfUnderlyings,
			  	  	                   matUnderl.nelem(),
							   strk,
							   vol,
							   arrayOfExpiries,
							   ir,
							   ts,
							   sims,
							   const_cast<char*>(putCallStr.c_str()),
							   const_cast<char*>(rngStr.c_str()),
							   const_cast<char*>(normStr.c_str()),
							   const_cast<char*>(instrStr.c_str()) );
     
    
    // Need to copy results into matrix
    for(unsigned int col=0; col < resDim(1); ++col)
    {
      for(unsigned int row=0; row < resDim(0); ++row)
      {
	matResult(row,col) = arrayOfResults[(col*resDim(0))+row];
      }
    }

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
