//
//  xmm-lib.hpp
//  xmm-lib
//
//  Created by Paul Best on 12/03/2018.
//

#ifndef xmm_lib_h
#define xmm_lib_h



extern "C" char opennXMM ();

extern "C" int trainXMM(void* ptr, int sample_num, void* sample_sizes, void* labels);


#endif /* xmm_lib_h */

