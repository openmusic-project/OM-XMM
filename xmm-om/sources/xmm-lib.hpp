//
//  xmm-lib.hpp
//  xmm-lib
//
//  Created by Paul Best on 12/03/2018.
//

#ifndef xmm_lib_h
#define xmm_lib_h


extern "C" void* initDataset(int numcolumns);

extern "C" void* initXMM();

extern "C" int fillDataset(void* descptr, int sample_num, void* sample_sizes, void* labls, void* dataset);

extern "C" int trainXMM(void* dataset, void* model);

extern "C" float runXMM(void* descptr, int sample_size, int columnum, void* model, bool reset, void* outptr);

extern "C" int save_model_JSON(char* pathptr, void* model);

extern "C" void free_model(void* model, void* dataset);

extern "C" int importJson(char* pathptr, void* modelptr, void* lablptr);

#endif /* xmm_lib_h */

