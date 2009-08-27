#include <iostream>
#include <octave/oct.h>

#include "CInterface.h"

DEFUN_DLD(hs_exit, args, , "Haskell Exit")
{
  if (args.length() > 0)
  {
    std::cout<<"see : help hs_exit";
    return octave_value();
  }

  exit_CInterface();
 
  return octave_value();
}
