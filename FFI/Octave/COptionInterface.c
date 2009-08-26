
#include "COptionInterface.h"
#include "OptionInterface_stub.h"

HsBool COptionInterface_init(void)
{
  static int argc = 1;
  static char* program_name = "Option Interface";
  static char* terminator = (char *) 0;
  static char** argv[2];
  argv[0] = &program_name;
  argv[1] = &terminator;

  hs_init(&argc, &argv);

  hs_add_root(__stginit_OptionInterface);

  return HS_BOOL_TRUE:
}


void COptionInterface_end(void)
{
  hs_exit();
}


double pricerInterface( double strk,
		      double vol,
		      double expy,
		      double ir,
		      int    ts,
		      int    sims )
{
  return priceOption( strk, vol, expy, ir, ts, sims);
}
