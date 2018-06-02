#include <R.h>
#include <Rinternals.h>
#include <R_ext/Rdynload.h>
#include "ursa.h"


static R_NativePrimitiveArgType readBsqBandInteger_t[] = {
   STRSXP, INTSXP, INTSXP, INTSXP, INTSXP, INTSXP, INTSXP
};

static R_NativePrimitiveArgType isNear_t[] = {
   REALSXP, REALSXP, INTSXP, INTSXP, INTSXP
};

static R_NativePrimitiveArgType bilinear_t[] = {
   REALSXP, INTSXP, INTSXP, INTSXP, REALSXP
};

static R_NativePrimitiveArgType makemap4_t[] = {
   REALSXP, REALSXP, INTSXP, REALSXP, REALSXP, INTSXP, REALSXP
};

static R_NativePrimitiveArgType writeBilBandDouble_t[] = {
   STRSXP, REALSXP, INTSXP, INTSXP, INTSXP, INTSXP, INTSXP
};

static R_NativePrimitiveArgType readBilLineDouble_t[] = {
   STRSXP, INTSXP, INTSXP, INTSXP, INTSXP, INTSXP, REALSXP
};

static R_NativePrimitiveArgType focalGaussian_t[] = {
   REALSXP, INTSXP, REALSXP, REALSXP, REALSXP, REALSXP, REALSXP, INTSXP, INTSXP, INTSXP, REALSXP
};

static R_NativePrimitiveArgType variability4_t[] = {
   REALSXP, REALSXP, INTSXP, REALSXP, REALSXP
};

static R_NativePrimitiveArgType progressBar_t[] = {
   INTSXP, INTSXP, STRSXP
};

static R_NativePrimitiveArgType focalOsisaf_t[] = {
   REALSXP, INTSXP, REALSXP, REALSXP, REALSXP, REALSXP, REALSXP, INTSXP, INTSXP, INTSXP, REALSXP
};

static R_NativePrimitiveArgType reclassify_t[] = {
   REALSXP, INTSXP, REALSXP, REALSXP, INTSXP, INTSXP
};

static R_NativePrimitiveArgType optimalDatatypeInt_t[] = {
   INTSXP, INTSXP, INTSXP
};

static R_NativePrimitiveArgType readBilLineInteger_t[] = {
   STRSXP, INTSXP, INTSXP, INTSXP, INTSXP, INTSXP, INTSXP
};

static R_NativePrimitiveArgType internalMargin_t[] = {
   REALSXP, INTSXP, INTSXP, INTSXP
};

static R_NativePrimitiveArgType table_log_t[] = {
   INTSXP, INTSXP, INTSXP
};

static R_NativePrimitiveArgType groupSummary_t[] = {
   REALSXP, INTSXP, REALSXP, REALSXP, INTSXP, REALSXP
};

static R_NativePrimitiveArgType aggregate_t[] = {
   REALSXP, INTSXP, INTSXP, REALSXP, INTSXP, REALSXP
};

static R_NativePrimitiveArgType focalExtrem_t[] = {
   REALSXP, INTSXP, REALSXP, INTSXP, INTSXP, REALSXP, INTSXP, INTSXP, INTSXP, REALSXP
};

static R_NativePrimitiveArgType focal4_t[] = {
   REALSXP, REALSXP, INTSXP, INTSXP, INTSXP, REALSXP, INTSXP, INTSXP, REALSXP
};

static R_NativePrimitiveArgType conTest_t[] = {
   INTSXP, INTSXP
};

static R_NativePrimitiveArgType makeField_t[] = {
   REALSXP, INTSXP, INTSXP
};

static R_NativePrimitiveArgType optimalDatatypeDouble_t[] = {
   REALSXP, INTSXP, INTSXP
};

static R_NativePrimitiveArgType readBilLineDouble2_t[] = {
   STRSXP, INTSXP, INTSXP, INTSXP, INTSXP, INTSXP, REALSXP
};

static R_NativePrimitiveArgType dist2dist_t[] = {
   REALSXP, REALSXP, REALSXP, REALSXP, INTSXP, INTSXP, INTSXP, INTSXP, REALSXP, INTSXP
};

static R_NativePrimitiveArgType expand_t[] = {
   REALSXP, INTSXP, INTSXP, INTSXP, REALSXP
};

static R_NativePrimitiveArgType focalSobelG_t[] = {
   REALSXP, INTSXP, REALSXP, REALSXP, REALSXP, REALSXP, REALSXP, INTSXP, INTSXP, INTSXP, REALSXP
};

static R_NativePrimitiveArgType focalHires_t[] = {
   REALSXP, INTSXP, REALSXP, REALSXP, REALSXP, REALSXP, REALSXP, INTSXP, INTSXP, INTSXP, REALSXP
};

static R_NativePrimitiveArgType focalSobel_t[] = {
   REALSXP, INTSXP, REALSXP, REALSXP, REALSXP, REALSXP, REALSXP, INTSXP, INTSXP, INTSXP, REALSXP
};

static R_NativePrimitiveArgType focalLoG_t[] = {
   REALSXP, INTSXP, REALSXP, REALSXP, REALSXP, REALSXP, REALSXP, INTSXP, INTSXP, INTSXP, REALSXP
};

static R_NativePrimitiveArgType focalMean_t[] = {
   REALSXP, REALSXP, INTSXP, REALSXP, REALSXP, INTSXP, INTSXP, INTSXP, REALSXP
};

static R_NativePrimitiveArgType readBsqLineInteger_t[] = {
   STRSXP, INTSXP, INTSXP, INTSXP, INTSXP, INTSXP, INTSXP
};

static R_NativePrimitiveArgType focalMeanWithNA_t[] = {
   REALSXP, INTSXP, REALSXP, REALSXP, INTSXP, INTSXP, REALSXP
};

static R_NativePrimitiveArgType writeBilBandInteger_t[] = {
   STRSXP, INTSXP, INTSXP, INTSXP, INTSXP, INTSXP, INTSXP
};

static R_NativePrimitiveArgType readBsqBandDouble_t[] = {
   STRSXP, INTSXP, INTSXP, INTSXP, INTSXP, INTSXP, REALSXP
};

static R_NativePrimitiveArgType readBilBandInteger_t[] = {
   STRSXP, INTSXP, INTSXP, INTSXP, INTSXP, INTSXP, INTSXP
};

static R_NativePrimitiveArgType interp4_t[] = {
   REALSXP, INTSXP, INTSXP, REALSXP, REALSXP
};

static R_NativePrimitiveArgType readBsqLineDouble_t[] = {
   STRSXP, INTSXP, INTSXP, INTSXP, INTSXP, INTSXP, REALSXP
};

static R_NativePrimitiveArgType readBilBandDouble_t[] = {
   STRSXP, INTSXP, INTSXP, INTSXP, INTSXP, INTSXP, REALSXP
};

static R_NativePrimitiveArgType focalCorrel_t[] = {
   REALSXP, INTSXP, REALSXP, REALSXP, REALSXP, REALSXP, REALSXP, INTSXP, INTSXP, INTSXP, REALSXP
};

static R_NativePrimitiveArgType focalCommon_t[] = {
   REALSXP, INTSXP, REALSXP, REALSXP, INTSXP, REALSXP, INTSXP, INTSXP, INTSXP, REALSXP
};

static R_NativePrimitiveArgType resampl4_t[] = {
   REALSXP, REALSXP, INTSXP, INTSXP, REALSXP, REALSXP, REALSXP, REALSXP, INTSXP, REALSXP
};

static R_NativePrimitiveArgType areaIncrement_t[] = {
   REALSXP, INTSXP, REALSXP, REALSXP
};

static R_NativePrimitiveArgType ffocal4_t[] = {
   REALSXP, INTSXP, REALSXP, INTSXP, INTSXP, REALSXP, INTSXP, INTSXP, REALSXP
};

static R_NativePrimitiveArgType readBilLineInteger2_t[] = {
   STRSXP, INTSXP, INTSXP, INTSXP, INTSXP, INTSXP, INTSXP
};

static R_NativePrimitiveArgType timefilt4_t[] = {
   REALSXP, INTSXP, INTSXP, REALSXP, REALSXP
};

static R_NativePrimitiveArgType focalLaplacian_t[] = {
   REALSXP, INTSXP, REALSXP, REALSXP, REALSXP, REALSXP, REALSXP, INTSXP, INTSXP, INTSXP, REALSXP
};

static R_NativePrimitiveArgType rasterize_t[] = {
   REALSXP, INTSXP, REALSXP, REALSXP, REALSXP, REALSXP, REALSXP, INTSXP, INTSXP
};

static R_NativePrimitiveArgType focalMedian_t[] = {
   REALSXP, REALSXP, INTSXP, INTSXP, INTSXP, INTSXP, REALSXP, INTSXP, REALSXP
};

static const R_CMethodDef CEntries[] = {
   {"readBsqBandInteger", (DL_FUNC) &readBsqBandInteger, 7, readBsqBandInteger_t},
   {"isNear", (DL_FUNC) &isNear, 5, isNear_t},
   {"bilinear", (DL_FUNC) &bilinear, 5, bilinear_t},
   {"makemap4", (DL_FUNC) &makemap4, 7, makemap4_t},
   {"writeBilBandDouble", (DL_FUNC) &writeBilBandDouble, 7, writeBilBandDouble_t},
   {"readBilLineDouble", (DL_FUNC) &readBilLineDouble, 7, readBilLineDouble_t},
   {"focalGaussian", (DL_FUNC) &focalGaussian, 11, focalGaussian_t},
   {"variability4", (DL_FUNC) &variability4, 5, variability4_t},
   {"progressBar", (DL_FUNC) &progressBar, 3, progressBar_t},
   {"focalOsisaf", (DL_FUNC) &focalOsisaf, 11, focalOsisaf_t},
   {"reclassify", (DL_FUNC) &reclassify, 6, reclassify_t},
   {"optimalDatatypeInt", (DL_FUNC) &optimalDatatypeInt, 3, optimalDatatypeInt_t},
   {"readBilLineInteger", (DL_FUNC) &readBilLineInteger, 7, readBilLineInteger_t},
   {"internalMargin", (DL_FUNC) &internalMargin, 4, internalMargin_t},
   {"table_log", (DL_FUNC) &table_log, 3, table_log_t},
   {"groupSummary", (DL_FUNC) &groupSummary, 6, groupSummary_t},
   {"aggregate", (DL_FUNC) &aggregate, 6, aggregate_t},
   {"focalExtrem", (DL_FUNC) &focalExtrem, 10, focalExtrem_t},
   {"focal4", (DL_FUNC) &focal4, 9, focal4_t},
   {"conTest", (DL_FUNC) &conTest, 2, conTest_t},
   {"makeField", (DL_FUNC) &makeField, 3, makeField_t},
   {"optimalDatatypeDouble", (DL_FUNC) &optimalDatatypeDouble, 3, optimalDatatypeDouble_t},
   {"readBilLineDouble2", (DL_FUNC) &readBilLineDouble2, 7, readBilLineDouble2_t},
   {"dist2dist", (DL_FUNC) &dist2dist, 10, dist2dist_t},
   {"expand", (DL_FUNC) &expand, 5, expand_t},
   {"focalSobelG", (DL_FUNC) &focalSobelG, 11, focalSobelG_t},
   {"focalHires", (DL_FUNC) &focalHires, 11, focalHires_t},
   {"focalSobel", (DL_FUNC) &focalSobel, 11, focalSobel_t},
   {"focalLoG", (DL_FUNC) &focalLoG, 11, focalLoG_t},
   {"focalMean", (DL_FUNC) &focalMean, 9, focalMean_t},
   {"readBsqLineInteger", (DL_FUNC) &readBsqLineInteger, 7, readBsqLineInteger_t},
   {"focalMeanWithNA", (DL_FUNC) &focalMeanWithNA, 7, focalMeanWithNA_t},
   {"writeBilBandInteger", (DL_FUNC) &writeBilBandInteger, 7, writeBilBandInteger_t},
   {"readBsqBandDouble", (DL_FUNC) &readBsqBandDouble, 7, readBsqBandDouble_t},
   {"readBilBandInteger", (DL_FUNC) &readBilBandInteger, 7, readBilBandInteger_t},
   {"interp4", (DL_FUNC) &interp4, 5, interp4_t},
   {"readBsqLineDouble", (DL_FUNC) &readBsqLineDouble, 7, readBsqLineDouble_t},
   {"readBilBandDouble", (DL_FUNC) &readBilBandDouble, 7, readBilBandDouble_t},
   {"focalCorrel", (DL_FUNC) &focalCorrel, 11, focalCorrel_t},
   {"focalCommon", (DL_FUNC) &focalCommon, 10, focalCommon_t},
   {"resampl4", (DL_FUNC) &resampl4, 10, resampl4_t},
   {"areaIncrement", (DL_FUNC) &areaIncrement, 4, areaIncrement_t},
   {"ffocal4", (DL_FUNC) &ffocal4, 9, ffocal4_t},
   {"readBilLineInteger2", (DL_FUNC) &readBilLineInteger2, 7, readBilLineInteger2_t},
   {"timefilt4", (DL_FUNC) &timefilt4, 5, timefilt4_t},
   {"focalLaplacian", (DL_FUNC) &focalLaplacian, 11, focalLaplacian_t},
   {"rasterize", (DL_FUNC) &rasterize, 9, rasterize_t},
   {"focalMedian", (DL_FUNC) &focalMedian, 9, focalMedian_t},
   {NULL, NULL, 0, NULL}
};

void R_init_ursa(DllInfo *dll)
{
   R_registerRoutines(dll, CEntries, NULL, NULL, NULL);
   R_useDynamicSymbols(dll, FALSE);
   R_forceSymbols(dll, TRUE);
}
