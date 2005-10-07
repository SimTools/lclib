extern
struct SPPARM_t {
  void (*nadspf)();
  int  mxtry;
} spparm_;

void springwrapper_(int *iret)
{
   spring_(spparm_.nadspf, spparm_.mxtry, iret);
}

typedef void (*SubPtr_t) ();

void addfunc_(SubPtr_t fun)
{
   spparm_.nadspf = fun;
}
