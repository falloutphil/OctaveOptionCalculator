
#include "CInterface.h"
#include "OptionInterface_stub.h"

void init_CInterface(void)
{
  static char * program_name = "Haskell-Octave Interface";
  static char * terminator = (char *) 0;
  int argc;
  char **argv[2];
  argc = 1;
  argv [0] = &program_name;
  argv [1] = &terminator;
  hs_init(&argc, argv);
}


void exit_CInterface(void)
{
   hs_exit();
}


double* pricerInterface( double* underl,
			 size_t  underlSize,
		         double  strk,
		         double  vol,
		         double* expy,
		         double  ir,
		         int     ts,
		         int     sims,
                         char*   rngStr,
                         char*   normalStr,
		         char*   instrStr )
{
  return priceOption( underl, underlSize, strk, vol, expy, ir, ts, sims, 
                      rngStr, normalStr, instrStr );
}



