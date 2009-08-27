#ifndef OCTAVECINTERFACE
#define OCTAVECINTERFACE

#ifdef __cplusplus
extern "C" {
#endif
  
void init_CInterface(void);
void exit_CInterface(void);

double pricerInterface( double strk,
			double vol,
			double expy,
			double ir,
			int    ts,
			int    sims );

#ifdef __cplusplus
}
#endif

#endif
