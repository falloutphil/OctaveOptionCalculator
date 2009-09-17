#include <stdio.h>

#include "CInterface.h"
#include "OptionInterface_stub.h"

#define NUM_OF_INSTANCES 999
#define INSTANCE_STR_LEN 28

// In Haskell if we hs_init() twice
// it seems to use the first instance
// anyway.  This isn't a bug *here*
// this will return unique strings
// for each program.
void init_CInterface(void)
{
  // Instance counter
  static unsigned int  id = 0;
  // Array of Instance names 
  static char          instanceNames[NUM_OF_INSTANCES][INSTANCE_STR_LEN];
  // Only produce 999 instance names - after that use this instead
  static char          overflowName[] = "BIG";
  // NULL char used to terminate
  static char*         terminator = (char*) 0;
  // One 'command line' argument
  static int           argc = 1;
  // Pointer to start of instance name
  //   - note this MUST be static because
  //     argv dictates we pass a pointer to
  //     this rather than the *value* pointer
  static char          *pInstanceString;
  // Command line argument strings
  //   - not static because this is passed
  //     by value
  char                 **argv[2];
  // Loop counter
  unsigned int         instanceNumber;

  // If we run out of unique instance ids, handle it
  pInstanceString = id >= (INSTANCE_STR_LEN-1) ?
                          &overflowName[0] :
                          &instanceNames[id][0];
  
  // if 0 then initialise our array, either way increment instance
  if (!id++)
  {
    for( instanceNumber = 0;
	 instanceNumber < NUM_OF_INSTANCES;
	 ++instanceNumber )
    {
      sprintf(instanceNames[instanceNumber], "Haskell-Octave Instance %u", (instanceNumber+1));
    }
  }
  
  // Setup 'program name' and terminate array
  argv[0] = &pInstanceString;
  argv[1] = &terminator;
 
  // Pass 'program' and args to Haskell initialiser 
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
			 char*   putCallStr,
                         char*   rngStr,
                         char*   normalStr,
		         char*   instrStr )
{
  return priceOption( underl, underlSize, strk, vol, expy, ir, ts, sims, 
                      putCallStr, rngStr, normalStr, instrStr );
}



