#include <iostream>
#include <octave/oct.h>

#include "CInterface.h"

DEFUN_DLD(hs_init, args, , "Haskell Init")
{
  if (args.length() > 0)
  {
    std::cout<<"see : help hs_init";
    return octave_value();
  }

  init_CInterface();
 
  return octave_value();
}


