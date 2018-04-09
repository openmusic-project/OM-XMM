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

std::string toString(char c){
    std::stringstream ss;
    std::string target;
    ss << c;
    ss >> target;
    return target;
}


void* initXMM(){
    static xmm::HierarchicalHMM *mhhmm = new xmm::HierarchicalHMM(false);
    return mhhmm;
}


void* initDataset(int numcolumns){
    static xmm::TrainingSet* mdataset = new xmm::TrainingSet(xmm::MemoryMode::OwnMemory, xmm::Multimodality::Unimodal);
    try{
        mdataset->dimension=numcolumns;
        mdataset->column_names.resize(numcolumns); //initdata segmentation violation here
        const std::vector<std::string> vec(numcolumns, "col");
        mdataset->column_names= vec;
    }catch ( const std::exception & Exp )
    {
        std::cerr << "\nErreur initDataset : " << Exp.what() << ".\n";
    }
    return mdataset;
}



int fillDataset(void* descptr, int sample_num, void* sample_sizes, void* labls, void* dataset){
    //init variables from pointers
    const float*** descr = static_cast<const float***>(descptr);
    const int* sizes = static_cast<const int*>(sample_sizes);
    const char** labels = static_cast<const char**>(labls);
    xmm::TrainingSet* mdataset = static_cast<xmm::TrainingSet*>(dataset);
    std::vector<float> *observation = new std::vector<float>(mdataset->dimension.get());
    try{
        mdataset->empty();
        //For each sample
        for(int j=0; j<sample_num; j++){
            //Build Phrase
            mdataset->addPhrase(j, labels[j]);
            mdataset->getPhrase(j)->column_names.resize(mdataset->column_names.size());
            mdataset->getPhrase(j)->column_names = mdataset->column_names.get();
            mdataset->getPhrase(j)->dimension =mdataset->column_names.size();
            for(int it =0; it < sizes[j]; it++){
                //Add each sound descriptor to the phrase
                for(int i =0; i<mdataset->dimension.get(); i++){
                    observation->at(i) = descr[j][i][it];
                }
                mdataset->getPhrase(j)->record(*observation);
            }
        }
        for(auto &label : mdataset->labels()){
            std::cout<<label<<std::endl;
        }
    }catch ( const std::exception & Exp )
    {
        std::cerr << "\nErreur fillDataset : " << Exp.what() << ".\n";
    }
    delete observation;
    return int('Y');
}



int trainXMM(void* dataset, void* model){
    try{
        xmm::HierarchicalHMM *mhhmm = static_cast<xmm::HierarchicalHMM*>(model);
        xmm::TrainingSet *mdataset = static_cast<xmm::TrainingSet*>(dataset);
        mhhmm->train(mdataset);
    }catch ( const std::exception & Exp )
    {
        std::cerr << "\nErreur train : " << Exp.what() << ".\n";
    }
    return int('Y');
}



int runXMM(void* descptr, int sample_size, int columnnum, void* model, bool reset){
    if(!model){
        std::cout<<"Model Pointer is null !";
    }
    xmm::HierarchicalHMM *mhhmm = static_cast<xmm::HierarchicalHMM*>(model);
    const float** descr = static_cast<const float**>(descptr);
    std::vector<float> *observation = new std::vector<float>(columnnum);
    try{
        if(reset){
            mhhmm->reset();
        }
        for(int k=0; k<sample_size;k++){
            for(int i =0; i<columnnum; i++){
                observation->at(i) = descr[i][k];
            }
            mhhmm->filter(*observation);
        }
    }catch ( const std::exception & Exp )
    {
        std::cerr << "\nErreur run : " << Exp.what() << ".\n";
    }
    delete observation;
    return mhhmm->results.likeliest[0];
}


int save_model_JSON(char* pathptr, void* model){
    xmm::HierarchicalHMM* mhhmm = static_cast<xmm::HierarchicalHMM*>(model);
    try{
        const char* path = static_cast<const char*>(pathptr);
        std::ofstream file_id(path);
        file_id << mhhmm->xmm::HierarchicalHMM::toJson().toStyledString().c_str();
        file_id.close();
    }catch(const std::exception & Exp ){
        std::cerr << "\nErreur export : " << Exp.what() << ".\n";
    }
    return 'Y';
}

int importJson(char* pathptr, void* modelptr, void* lablptr){
    const char* path = static_cast<const char*>(pathptr);
    xmm::HierarchicalHMM *mhhmm = static_cast<xmm::HierarchicalHMM*>(modelptr);
    //char** labls=(char**)malloc((mhhmm->models.size()+1)*__SIZEOF_POINTER__);
    char** labls=static_cast<char**>(lablptr);
    try{
        std::ifstream file(path);
        if(!file.good()){
            throw "file doesn't exist";
            return 0;
        }
        Json::Value json;
        Json::Reader reader;
        std::string str((std::istreambuf_iterator<char>(file)), std::istreambuf_iterator<char>());
        
        //Load model from json file
        if (reader.parse(str, json)) {
            mhhmm->xmm::HierarchicalHMM::fromJson(json);
        }else{throw std::runtime_error("unable to parse json value");}
        
        //Prepare labels list for OM
        int j, i =0;
        for(auto &model : mhhmm->models){
            j=0;
            labls[i]=(char*)malloc((model.first.size()+1)*sizeof(char));
            for(auto lettre : model.first){
                labls[i][j]= lettre ;
                j++;
            }
            labls[i][model.first.size()]='0';
            i++;
        }
        labls[i]=(char*)malloc(sizeof(char));
        labls[i][0]='0';
    }catch(const std::exception & Exp )
    {
        std::cerr << "\nErreur import : " << Exp.what() << ".\n";
    }
    return int(mhhmm->models.size());
}

void free_model(void* model, void* dataset){
    try{
        if(dataset){
            static_cast<xmm::TrainingSet*>(dataset)->clear();
            free (dataset);
            std::cout<<"dataset freed"<<std::endl;
        }
        if(model){
            static_cast<xmm::HierarchicalHMM*>(model)->clear();
            free(model);
            std::cout<<"model freed"<<std::endl;
        }
    }catch(const std::exception & Exp )
    {
        std::cerr << "\nErreur free : " << Exp.what() << ".\n";
    }
}
           
           
           
           

