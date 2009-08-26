#include <iostream>
#include <octave/oct.h>

#include "MyInterface.h"


DEFUN_DLD(OctaveOptionTest, args, , "Haskell Option Pricer")
{

  if (args.length() < 6)
  {
    std::cout<<"see  : help priceOption";
    return octave_value();
  }

  
  double strk = args(0).double_value();
  double vol  = args(1).double_value();
  double expy = args(2).double_value();
  double ir   = args(3).double_value();
  int    ts   = args(4).int_value();
  int    sims = args(5).int_value();

  double result = pricerInterface(strk,vol,expy,ir,ts,sims);
  
  return octave_value();

}
