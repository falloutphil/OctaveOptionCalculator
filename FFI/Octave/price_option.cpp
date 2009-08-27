#include <iostream>
#include <octave/oct.h>

#include "CInterface.h"

DEFUN_DLD(price_option, args, , "Option Pricer")
{
  if (args.length() < 6)
  {
    std::cout<<"see : help price_option";
    return octave_value();
  }

  double strk = args(0).double_value();
  double vol  = args(1).double_value();
  double expy = args(2).double_value();
  double ir   = args(3).double_value();
  int    ts   = args(4).int_value();
  int    sims = args(5).int_value();
  std::cout << "\nstrk " << strk
            << "\nvol  " << vol
            << "\nexpy " << expy
            << "\nir   " << ir
            << "\nts   " << ts
            << "\nsims " << sims;

  double answer = pricerInterface( strk,
				   vol,
			           expy,
			           ir,
			           ts,
			           sims );
  return octave_value(answer);
}
