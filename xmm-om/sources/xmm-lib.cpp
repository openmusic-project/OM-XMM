//
//  xmm-lib.cpp
//  xmm
//
//  Created by Paul Best on 12/03/2018.
//


#include "include-xmm.h"
#include "stdlib.h"
#include "xmm-lib.hpp"
//#include <xmmTrainingSet.hpp>
#include <iostream>
#include <sstream>
#include <string>
static const int DESC_NUM=9;



std::string toString(char c){
    std::stringstream ss;
    std::string target;
    ss << c;
    ss >> target;
    return target;
}


void* initXMM(){
    static xmm::HierarchicalHMM mhhmm = new xmm::HierarchicalHMM(false);
    return &mhhmm;
}

int trainXMM(void* descptr, int sample_num, void* sample_sizes, void* labls, void* model){
    
    const float*** descr = static_cast<const float***>(descptr);
    const int* sizes = static_cast<const int*>(sample_sizes);
    const char* labels = static_cast<char*>(labls);
    xmm::HierarchicalHMM* mhhmm = static_cast<xmm::HierarchicalHMM*>(model);
    xmm::TrainingSet *mdataset = new xmm::TrainingSet(xmm::MemoryMode::OwnMemory, xmm::Multimodality::Unimodal);
    
    const std::vector<std::string> descr_names {"Total Energy","Fundamental Frequency","Spectral Centroid","Loudness","Sharpness","Spread","Harmonic Energy","Inharmonicity", "Noisiness"};
    mdataset->column_names.resize(DESC_NUM);
    mdataset->column_names.set(descr_names);
    mdataset->dimension=9;
    
    
    
    try{
        //For each sample
        for(int j=0; j<sample_num; j++){
            
            //Build Phrase
            mdataset->addPhrase(j, toString(labels[j]));
            mdataset->getPhrase(j)->column_names.resize(DESC_NUM);
            mdataset->getPhrase(j)->column_names =descr_names;
            mdataset->getPhrase(j)->dimension =DESC_NUM;
            for(int it =0; it < sizes[j]; it++){
                std::vector<float> observation = *new std::vector<float>(DESC_NUM);
                //Add each sound descriptor to the phrase
                for(int i =0; i<DESC_NUM; i++){
                    observation[i] = descr[j][i][it];
                }
                mdataset->getPhrase(j)->record(observation);
            }
        }
        
        //Train the model
        mhhmm->train(mdataset);
        
        
        //Counter for each label : in one sample, how many times each label has been recognized
        std::map<std::string, int> results;
        
        //Print labels
        std::cout<<"Labels : "<<std::endl;
        for(std::set<std::string>::iterator it = mdataset->labels().begin(); it !=  mdataset->labels().end(); it++){
            std::cout << *it ;
            results.insert(std::pair<std::string, int>(*it,0));
        }
        std::cout<<"\n";
        
        std::cout<<"Num of obs : "<<mdataset->size()<<std::endl;
        
        ////////////////////
        /////// TEST ///////
        ////////////////////
        float accuracy = 0;
        char predicted = ' ';
        for(int j=0; j<sample_num;j++){
            mhhmm->reset();
            for(int k=0; k<sizes[j];k++){
                std::vector<float> observation = *new std::vector<float>(DESC_NUM);
                for(int i =0; i<DESC_NUM; i++){
                    observation[i] = descr[j][i][k];
                }
                mhhmm->filter(observation);
            }
            predicted=mhhmm->results.likeliest[0];
            std::cout<<"Predicted: "<<predicted<<", Actual: "<<labels[j]<<std::endl;
            if(predicted==labels[j]){
                accuracy++;
            }
        }
        accuracy = accuracy/sample_num;
        std::cout<<"Accuracy :"<<accuracy;
        

    }catch ( const std::exception & Exp )
    {
        std::cerr << "\nErreur : " << Exp.what() << ".\n";
    }
    
    // FREE MEMORY !!!
    return int('Y');
}



int runXMM(void* descptr, int sample_size, void* model){
    xmm::HierarchicalHMM* mhhmm = static_cast<xmm::HierarchicalHMM*>(model);
    const float** descr = static_cast<const float**>(descptr);
    
    mhhmm->reset();
    for(int k=0; k<sample_size;k++){
        std::vector<float> observation = *new std::vector<float>(DESC_NUM);

        for(int i =0; i<9; i++){
            observation[i] = descr[i][k];
        }
        mhhmm->filter(observation);
    }
    return mhhmm->results.likeliest[0];
}


int save_model_JSON(char* pathptr, void* model){
    xmm::HierarchicalHMM* mhhmm = static_cast<xmm::HierarchicalHMM*>(model);
    const char* path = static_cast<const char*>(pathptr);
    
    std::ofstream file_id;
    file_id.open(path);
    Json::FastWriter writer;
    file_id << writer.write(mhhmm->toJson());
    file_id.close();
    return 'Y';
}
           
           


           
           
           
           
           
           
           
           

