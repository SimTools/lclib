typedef void (*SubPtr_t) ();

void setloc_(int *loc, SubPtr_t fname)
{
  *loc = (int)fname;
}

void cdbcsex_(char *cmdnam, int *loc, int len)
{
  SubPtr_t fp = (SubPtr_t)*loc;
  dbcsex_(cmdnam, fp, len);
}

void cfexsub_(int *loc)
{
  SubPtr_t fp = (SubPtr_t)*loc;
  fexsub_(fp);
}

void cfexprn_(int *loc, int *level, int *dbg)
{
  SubPtr_t fp = (SubPtr_t)*loc;
  fexsub_(fp, level, dbg);
}

void cfexevt_(int *loc, int *irec, int *level, int *dbg, int *iret)
{
  SubPtr_t fp = (SubPtr_t)*loc;
  fexevt_(fp, irec, level, dbg, iret);
}
