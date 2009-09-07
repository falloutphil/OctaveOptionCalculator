#include <iostream>
#include <octave/oct.h>
#include <octave/dMatrix.h>

#include "CInterface.h"

using namespace std;

DEFUN_DLD(price_option, args, , "Option Pricer")
{
  if (args.length() != 10)
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
  // Not very safe, but we know Haskell can't
  // mutate, so this will be safe if we're careful.
  // Decided to use const_cast here as it is
  // on-balance safer than using a C-style cast in the
  // C-code.  However it does mean our variable
  // is potentially exposed for the next few lines.
  // Of course the pointer is kept const, we won't
  // be changing this.
  char* const  rngStr   = const_cast<char*>(args(7).string_value().c_str());
  char* const  normStr  = const_cast<char*>(args(8).string_value().c_str());
  char* const  instrStr = const_cast<char*>(args(9).string_value().c_str());
  
  const dim_vector& resDim = matUnderl.dims();

  if ( resDim  == matExpy.dims() )
  {
    Matrix matResult(resDim);
    // For every row in the matrix
    for(unsigned int row=0;row < resDim(0); ++row)
    {
      for(unsigned int col=0; col < resDim(1); ++col)
      {
	// Looping this isn't efficient, ideally
	// we'd take vectors into Haskell and treat
	// them all at once - valuing each different
	// stock and recording values at relevant
	// expiries.  Still for now it will do...
	matResult(row,col) = pricerInterface( matUnderl(row,col),
					      strk,
					      vol,
					      matExpy(row,col),
					      ir,
					      ts,
					      sims,
					      rngStr,
					      normStr,
					      instrStr );
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
