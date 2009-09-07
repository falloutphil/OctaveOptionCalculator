#ifndef OCTAVECINTERFACE
#define OCTAVECINTERFACE

#ifdef __cplusplus
extern "C" {
#endif
  
void init_CInterface(void);
void exit_CInterface(void);

  double pricerInterface( double underl,
		          double strk,
			  double vol,
			  double expy,
			  double ir,
			  int    ts,
			  int    sims,
			  char*  rngStr,
			  char*  normalStr,
			  char*  instrStr );

#ifdef __cplusplus
}
#endif

#endif
