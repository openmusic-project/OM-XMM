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

class omTrainingListener{
public:
    void onEvent(xmm::TrainingEvent const& e) {
        std::cout<<e.label<<std::endl;
    }
};

void* initXMM(float relat, float abs, int statenum){
    xmm::HierarchicalHMM *mhhmm = new xmm::HierarchicalHMM(false);
    mhhmm->configuration.relative_regularization.set(relat);
    mhhmm->configuration.absolute_regularization.set(abs);
    mhhmm->configuration.states.set(statenum);
    mhhmm->configuration.multithreading = xmm::MultithreadingMode::Sequential;
    return mhhmm;
}


void* initDataset(int numcolumns){
    xmm::TrainingSet* mdataset = new xmm::TrainingSet(xmm::MemoryMode::OwnMemory, xmm::Multimodality::Unimodal);
    try{
        mdataset->clear();
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

int add_Phrase(void* descptr, int sample_size, void* labelptr, void* dataset){
    const float** descr = static_cast<const float**>(descptr);
    xmm::TrainingSet* mdataset = static_cast<xmm::TrainingSet*>(dataset);
    const char* label = static_cast<const char*>(labelptr);
    std::vector<float> *observation = new std::vector<float>(mdataset->dimension.get());
    try{
        int index = mdataset->size();
        mdataset->addPhrase(index ,label);
        mdataset->getPhrase(index)->column_names.resize(mdataset->dimension.get());
        mdataset->getPhrase(index)->column_names = mdataset->column_names.get();
        mdataset->getPhrase(index)->dimension = mdataset->dimension.get();
        
        for(int it =0; it < sample_size; it++){
            //Add each sound descriptor to the phrase
            for(int i =0; i<mdataset->dimension.get(); i++){
                observation->at(i) = descr[i][it];
            }
            mdataset->getPhrase(index)->record(*observation);
        }
    }catch(const std::exception &Exp){
        std::cerr<<"\n Erreur addPhrase : " <<Exp.what()<<std::endl;
    }
    return 'Y';
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
    }catch ( const std::exception & Exp )
    {
        std::cerr << "\nErreur fillDataset : " << Exp.what() << ".\n";
    }
    delete observation;
    return int('Y');
}

int getclassAvrg(void* dataset, void* labl, void* out){
    const char* label = static_cast<const char*>(labl);
    xmm::TrainingSet *mdataset = static_cast<xmm::TrainingSet*>(dataset)->getPhrasesOfClass(label);
    
    //Get min size of all phrase of the class
    int minsize = mdataset->begin()->second->size();
    for(auto phrase = mdataset->begin(); phrase!=mdataset->end(); phrase++){
        if(phrase->second->size()<minsize){
            minsize= phrase->second->size();
        }
    }
    float temp=0;
    float deltasize=0;
    float** means = static_cast<float**>(out);
    for(int i=0; i<mdataset->dimension.get(); i++){
        means[i]=(float*)malloc(minsize*sizeof(float));
        for(int j=0; j<minsize; j++){
            temp=0;
            for(auto phrase = mdataset->begin(); phrase!=mdataset->end(); phrase++){
                deltasize=phrase->second->size()-minsize;
                temp += phrase->second->getValue(j+round(deltasize/2),i);
            }
            means[i][j]=temp/minsize;
        }
    }
    return minsize;
}



int trainXMM(void* dataset, void* model){
    try{
        xmm::HierarchicalHMM *mhhmm = static_cast<xmm::HierarchicalHMM*>(model);
        xmm::TrainingSet *mdataset = static_cast<xmm::TrainingSet*>(dataset);
        
        for(auto model= mhhmm->models.begin(); model !=mhhmm->models.end(); model++){
            std::cout<<"hey";
            std::cout<<model->second.states.size()<<std::endl;
            std::cout<<model->second.states[0].parameters.gaussians.get()<<std::endl;
        }
        //print info
        std::cout<<"Training with "<<mdataset->dimension.get()<<" columns"<<std::endl
        <<mhhmm->configuration.states.get()<<" states"<<std::endl
        <<"regularization "<<mhhmm->configuration.relative_regularization.get()<<" "<<mhhmm->configuration.absolute_regularization.get()<<std::endl;
       
        mhhmm->train(mdataset);
        omTrainingListener* list = new omTrainingListener();
        mhhmm->training_events.addListener(list, &omTrainingListener::onEvent);
        
    }catch ( const std::exception & Exp )
    {
        std::cerr << "\nErreur train : " << Exp.what() << ".\n";
    }
    return int('Y');
}



float runXMM(void* descptr, int sample_size, int columnnum, void* model, int reset, void* outptr){
    if(!model){
        std::cout<<"Model Pointer is null !";
    }
    xmm::HierarchicalHMM *mhhmm = static_cast<xmm::HierarchicalHMM*>(model);
    const float** descr = static_cast<const float**>(descptr);
    std::vector<float> *observation = new std::vector<float>(columnnum);
    char* out = static_cast<char*>(outptr);
    double max1 =-1;
    try{
        if((reset!=0) || (mhhmm->results.instant_likelihoods.size()==0)){
            mhhmm->reset();
        }
        for(int k=0; k<sample_size;k++){
            for(int i =0; i<columnnum; i++){
                observation->at(i) = descr[i][k];
            }
            mhhmm->filter(*observation);
            
            //std::cout<<mhhmm->results.likeliest<<std::endl;
        }
   //     std::cout<<mhhmm->models.at(mhhmm->results.likeliest).results.progress<<std::endl;
        
//        //Get three likeliest labels
//        double max2 =-1, max3 = -1;
//        std::string label1, label2, label3;
//        int i=0;
//        for(auto it = mhhmm->models.begin(); it != mhhmm->models.end(); ++it, ++i){
//            if(mhhmm->results.smoothed_normalized_likelihoods[i]>max1){
//                max1=mhhmm->results.smoothed_normalized_likelihoods[i];
//                label1=it->first;
//            }
//        }
//        for(auto it = mhhmm->models.begin(); it != mhhmm->models.end(); ++it, ++i){
//            if(mhhmm->results.smoothed_normalized_likelihoods[i]>max2 && it->first != label1){
//                max2=mhhmm->results.smoothed_normalized_likelihoods[i];
//                label2=it->first;
//            }
//        }
//        for(auto it = mhhmm->models.begin(); it != mhhmm->models.end(); ++it, ++i){
//            if(mhhmm->results.smoothed_normalized_likelihoods[i]>max3 && it->first != label1 && it->first != label2){
//                max3=mhhmm->results.smoothed_normalized_likelihoods[i];
//                label3=it->first;
//            }
//        }
//        for(auto likel : mhhmm->results.smoothed_normalized_likelihoods){
//            std::cout<<likel<<" ";
//        }
        
//        std::cout<<std::endl<<"1. "<<label1<<" "<<max1<<std::endl<<"2. "<<label2<<" "<<max2<<std::endl<<"3. "<<label3<<" "<<max3<<std::endl;
       // std::cout<<mhhmm->results.likeliest;
        strcpy(out, (mhhmm->results.likeliest+"0").c_str());
    }catch ( const std::exception & Exp )
    {
        std::cerr << "\nErreur run : " << Exp.what() << ".\n";
    }
    delete observation;
    return float(max1);//mhhmm->results.smoothed_normalized_likelihoods.at(std::distance(mhhmm->models.begin(),mhhmm->models.find(mhhmm->results.likeliest)));
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
    char* path = static_cast<char*>(pathptr);
    xmm::HierarchicalHMM *mhhmm = static_cast<xmm::HierarchicalHMM*>(modelptr);
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
        }else{
            std::cout<<"unable to parse json value";
            return 0;
        }
        
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



