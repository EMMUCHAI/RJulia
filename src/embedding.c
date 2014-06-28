/*
Copyright (C) 2014 by Yu Gong
*/
#include <stdio.h>
#include <math.h>
#include <R.h>
#include <Rinternals.h>
#include <Rmath.h>
#include <julia.h>
#include <Rdefines.h>
#include "Julia_R.h"
#include "R_Julia.h"

#define pkgdebug
#define JL_SYSTEM_IMAGE_PATH "../lib/julia/sys.ji"
extern DLLEXPORT char *julia_home;
char *image_file = NULL;
char system_image[256] = JL_SYSTEM_IMAGE_PATH;

int true_main(int argc, char *argv[])
{
    if (jl_base_module != NULL) {
        jl_array_t *args = (jl_array_t*)jl_get_global(jl_base_module, jl_symbol("ARGS"));
        if (args == NULL) {
            args = jl_alloc_cell_1d(0);
            jl_set_const(jl_base_module, jl_symbol("ARGS"), (jl_value_t*)args);
        }
        jl_array_grow_end(args, argc);
        int i;
        for (i=0; i < argc; i++) {
            jl_value_t *s = (jl_value_t*)jl_cstr_to_string(argv[i]);
            s->type = (jl_value_t*)jl_utf8_string_type;
           // Rprintf("%d %s\n",i,argv[i]);
            jl_arrayset(args, s, i);
       }
    }
    jl_function_t *start_client =
        (jl_function_t*)jl_get_global(jl_base_module, jl_symbol("_startnoterm"));

    if (start_client) {
        jl_apply(start_client, NULL, 0);
    }
    return 0;
}

static int jlrunning=0;

static int DataArrayFrameInited=0;
SEXP Julia_LoadDataArrayFrame()
{
  jl_eval_string("using DataArrays,DataFrames");
  DataArrayFrameInited=1;
  if (jl_exception_occurred()){
    jl_show(jl_stderr_obj(), jl_exception_occurred());
    Rprintf("\n");
   DataArrayFrameInited=0; 
   jl_exception_clear();
  } 
  return R_NilValue; 
}

SEXP Julia_DataArrayFrameInited()
{
  SEXP ans;
  PROTECT(ans = allocVector(LGLSXP, 1));
  LOGICAL(ans)[0]=DataArrayFrameInited;
  UNPROTECT(1);
  return ans; 
}


SEXP Julia_is_running()
{
  SEXP ans;
  PROTECT(ans = allocVector(LGLSXP, 1));
  LOGICAL(ans)[0]=jlrunning;
  UNPROTECT(1);
  return ans; 
}

SEXP initJulia(SEXP julia_home1,SEXP DisableGC)
{
  if (jl_is_initialized())
    return R_NilValue;
  julia_home =(char*) CHAR(STRING_ELT(julia_home1, 0));
  int argc1=0;
  char **argv1=NULL;

  /*char worker[10];
  snprintf(worker,10,"%d",INTEGER(Workers)[0]);
  argv1[1]=worker;*/

  char realimage[1024];
  if (julia_home[strlen(julia_home)-1]=='/'
    ||julia_home[strlen(julia_home)-1]=='\\')
   snprintf(realimage,1024,"%s%s",julia_home,system_image); 
  else  
   snprintf(realimage,1024,"%s/%s",julia_home,system_image);
  
  libsupport_init();
  julia_init(realimage);
  julia_trampoline(argc1, argv1, true_main);
  jlrunning=1;

  if (jl_exception_occurred()){
    jlrunning=0;
    jl_show(jl_stderr_obj(), jl_exception_occurred());
    Rprintf("\n");
    jl_exception_clear();
  }
  if (LOGICAL(DisableGC))
    jl_gc_disable();
  Rprintf("Julia Init success\n");
  return R_NilValue;
}

//Convert Julia Type To R
SEXP Julia_R(jl_value_t* Var)
{  
  SEXP ans;
  JL_GC_PUSH1(&Var);
  ans=R_NilValue;
  //Array To Vector
  if (jl_is_null(Var)||jl_is_nothing(Var))
    return ans;
  if (jl_is_array(Var))
  {
   ans=Julia_R_MD(Var); 
  }
  else if (strcmp(jl_typeof_str(Var),"DataArray")==0||
           strcmp(jl_typeof_str(Var),"PoolDataArray")==0||
           strcmp(jl_typeof_str(Var),"DataVector")==0||
           strcmp(jl_typeof_str(Var),"DataMatrix")==0||
           strcmp(jl_typeof_str(Var),"DataFrame")==0||
           strcmp(jl_typeof_str(Var),"NAtype")==0)
   {
    //try to load DataArrays DataFrames package
    if (!DataArrayFrameInited) Julia_LoadDataArrayFrame();
    if (!DataArrayFrameInited)
    {
     error("DataArrays and DataFrames can't be load,please check this\n");
     return R_NilValue;
    }
    if(strcmp(jl_typeof_str(Var),"NAtype")==0||
       strcmp(jl_typeof_str(Var),"DataFrame")==0||
       strcmp(jl_typeof_str(Var),"PoolDataArray")==0)
     { 
      if(strcmp(jl_typeof_str(Var),"NAtype")==0)
        ans=Julia_R_Scalar_NA(Var); 
      else if (strcmp(jl_typeof_str(Var),"PoolDataArray")==0)
       {
        //ans=Julia_R_MD_NA_PoolArray(Var);
       }
      else 
        ans=Julia_R_MD_NA_DataFrame(Var);
     } 
    else 
     ans=Julia_R_MD_NA(Var);
   } 
  else if (jl_is_tuple(Var))
   {
     PROTECT(ans=allocVector(VECSXP,jl_tuple_len(Var)));
     for(int i=0;i<jl_tuple_len(Var);i++)
     {
      SET_ELEMENT(ans,i,Julia_R(jl_tupleref(Var,i)));
     }
     UNPROTECT(1);
   } 
  else 
  { 
    ans=Julia_R_Scalar(Var); 
  }
 JL_GC_POP();
 return ans;
}

//Convert R Type To Julia,which not contain NA
SEXP R_Julia(SEXP Var,SEXP VarNam)
{
  const char *VarName = CHAR(STRING_ELT(VarNam, 0));
  R_Julia_MD(Var,VarName);
  return R_NilValue;
}
//Convert R Type To Julia,which contain NA
SEXP R_Julia_NA(SEXP Var,SEXP VarNam)
{
  const char *VarName = CHAR(STRING_ELT(VarNam, 0));
  R_Julia_MD_NA(Var,VarName);
  return R_NilValue;
}
//Convert R data frame To Julia
SEXP R_Julia_NA_DataFrame(SEXP Var,SEXP VarNam)
{
  const char *VarName = CHAR(STRING_ELT(VarNam, 0));
  R_Julia_MD_NA_DataFrame(Var,VarName);
  return R_NilValue;
}
//eval but not return val
SEXP jl_void_eval(SEXP cmd)
{
  const char *s = CHAR(STRING_ELT(cmd, 0));
  jl_eval_string((char*)s);
  if (jl_exception_occurred())
   { 
   jl_show(jl_stderr_obj(), jl_exception_occurred());
   Rprintf("\n");
   jl_exception_clear();
   }
  return R_NilValue;
}

//eval julia script and retrun
SEXP jl_eval(SEXP cmd)
{
  const char *s = CHAR(STRING_ELT(cmd, 0));
  jl_value_t* ret= jl_eval_string((char*)s);
  if (jl_exception_occurred()){
    jl_show(jl_stderr_obj(), jl_exception_occurred());
    Rprintf("\n");
    jl_exception_clear();
   return R_NilValue;
  }
  return Julia_R(ret);
}
