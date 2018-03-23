//
//  xmm-lib.hpp
//  xmm-lib
//
//  Created by Paul Best on 12/03/2018.
//

#ifndef xmm_lib_h
#define xmm_lib_h



extern "C" char opennXMM ();

extern "C" int trainXMM(void* descrptr, int sample_num, void* sample_sizes, void* labls, void* model);

extern "C" int runXMM(void* descptr, int sample_size, void* model);

extern "C" void* initXMM();


#endif /* xmm_lib_h */

