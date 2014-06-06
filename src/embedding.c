#include <stdio.h>
#include <math.h>
#include <R.h>
#include <Rinternals.h>
#include <Rmath.h>
#include <julia.h>
#include "Julia_R.h"
#define pkgdebug

SEXP initJulia(SEXP julia_home,SEXP DisableGC)
{
  if (jl_is_initialized())
    return R_NilValue;

  char *s = CHAR(STRING_ELT(julia_home, 0));
  if (strlen(s) == 0)
    jl_init(NULL);
  else
    jl_init(s);

  JL_SET_STACK_BASE;
  if (jl_exception_occurred())
  {
    error("Julia not initialized");
  }
  if (LOGICAL(DisableGC))
    jl_gc_disable();
  return R_NilValue;
}

//Convert Julia Type To R
SEXP Julia_R(jl_value_t* Var)
{  
  SEXP ans;
  JL_GC_PUSH1(&Var);
  ans=R_NilValue;
  //Array To Vector
  if (jl_is_array(Var))
  {
   if (jl_array_ndims(Var)==1)
   {
     Julia_R_1D(Var,ans);
   }
   else  
   {
     /* code */
   }   
 }
//Value to Vector
 Julia_R_Basic_Element(Var,ans);
 JL_GC_POP();
 return ans;
}
//Convert R Type To Julia
SEXP R_Julia(SEXP Var,SEXP VarNam)
{
  int n;
  jl_value_t* ret;
  char *VarName = CHAR(STRING_ELT(VarNam, 0));
  if (isVector(Var))
   R_Julia_Vector(Var,ret,VarName);
 return R_NilValue;
}

SEXP jl_void_eval(SEXP cmd)
{
  char *s = CHAR(STRING_ELT(cmd, 0));
  jl_value_t* ret = jl_eval_string(s);
  return R_NilValue;
}

SEXP jl_eval(SEXP cmd)
{
  char *s = CHAR(STRING_ELT(cmd, 0));
  jl_value_t* ret= jl_eval_string(s);
  return Julia_R(ret);
}

