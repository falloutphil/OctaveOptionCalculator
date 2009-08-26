#include "MyInterface.h"
#include "OptionInterface_stub.h"

double pricerInterface( double strk,
		      double vol,
		      double expy,
		      double ir,
		      int    ts,
		      int    sims )
{
  return priceOption( strk, vol, expy, ir, ts, sims);
}
