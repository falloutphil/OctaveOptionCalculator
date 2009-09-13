#ifndef OCTAVECINTERFACE
#define OCTAVECINTERFACE

#include <stddef.h>

#ifdef __cplusplus
extern "C" {
#endif
  
void init_CInterface(void);
void exit_CInterface(void);

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
			 char*   instrStr );

#ifdef __cplusplus
}
#endif

#endif
