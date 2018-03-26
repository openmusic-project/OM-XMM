//
//  xmm-lib.hpp
//  xmm-lib
//
//  Created by Paul Best on 12/03/2018.
//

#ifndef xmm_lib_h
#define xmm_lib_h



extern "C" int trainXMM(void* descrptr, int sample_num, void* sample_sizes, void* labls, void* model);

extern "C" int runXMM(void* descptr, int sample_size, void* model);

extern "C" void* initXMM();

extern "C" int save_model_JSON(char* pathptr, void* model);

extern "C" void free_model(void* model);


#endif /* xmm_lib_h */

