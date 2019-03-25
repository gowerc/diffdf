#include <Rcpp.h>
using namespace Rcpp;

// Takes two data frames, sorted by each column in turn, and following that
//order, so eg DF(A,B) with DF[order(A,B)] then returns an index
//saying where the matches are in both DFs

// [[Rcpp::export]]
List find_matches(DataFrame DS1, DataFrame DS2,
                  CharacterVector classtype, int KN) {
    int n1;
    int n2;
    std::map<int, CharacterVector> mapchar;
    std::map<int, NumericVector> mapnum;
    std::map<int, LogicalVector> maplog;
    int seq1 = 1;
    int seq2 = 1;
    int d1 = 0;
    int d2 = 0;
    int flag =  0;
    int inc1 = 0;
    int inc2 = 0;
    int j  = 0;
    List out(2);
    
    // Need to initialise the objects
    // We know how many columns there are and their types because
    // the user provided them. We loop over column number, and map
    // each column to its appropriate type, indexing on j=i*2
    // to allow room for ds1 and ds2
    
    for(int i = 0; i < KN; ++i) {
        j = i *2;
        if (classtype[i] == "character"){
            mapchar.insert(std::make_pair(j,
                                          as<CharacterVector>(DS1[i])));
            n1 = mapchar[j].size();
            mapchar.insert(std::make_pair(j+1,
                                          as<CharacterVector>(DS2[i])));
            n2 = mapchar[j+1].size();
        } else  if (classtype[i] == "numeric"){
            mapnum.insert(std::make_pair(j,
                                         as<NumericVector>(DS1[i])));
            n1 = mapnum[j].size();
            mapnum.insert(std::make_pair(j+1,
                                         as<NumericVector>(DS2[i])));
            n2 = mapnum[j+1].size() ;
        } else if  (classtype[i] == "logical"){
            maplog.insert(std::make_pair(j,
                                         as<LogicalVector>(DS1[i])));
            n1 = maplog[j].size();
            maplog.insert(std::make_pair(j+1,
                                         as<LogicalVector>(DS2[i]))) ;
            n2 = maplog[j+1].size() ;
            
        }
    }
    
    
    CharacterVector c1(n1);
    CharacterVector c2(n2);
    NumericVector N1(n1);
    NumericVector N2(n1);
    LogicalVector l1(n1);
    LogicalVector l2(n2);
    NumericVector index1(n1+n2);
    NumericVector index2(n1+n2);
    
    // loop over data frames, moving along the rows
    // if a match is found, record that and move along both
    // if a mismatch, only move down a row in the lowest df
    // eg if ds1 value = 3 and ds2 value = 2, go down a row
    //in ds2 and remain in place in ds1
    //This iterates through and finds all matching
    // note that this requires sorting to be succesful!!
    
    while((d1 < n1) & (d2 < n2)){
        
        // flag records if a mismatch was found, inc1 and inc2
        //tells us how much we should increase d1 and d22
        flag = 0;
        inc1 = 1;
        inc2 = 1;
        
        // we need to search through the keys columns, iterating over
        // 1:KN as before, looking for matches
        for (int i =0; i < (KN*2); i+=2){
            if ((mapchar.count(i)>0) & (flag == 0)){
                c1 = mapchar[i];
                c2 = mapchar[i+1];
                if (c1[d1] != c2[d2]){
                    flag =1;
                    if (c1[d1] > c2[d2]){
                        inc1 = 0 ;
                    }else{
                        inc2 = 0;
                    }
                    
                }
            }
            if ((mapnum.count(i)>0) & (flag == 0)){
                N1 = mapnum[i];
                N2 = mapnum[i+1];
                
                if (N1[d1] != N2[d2]){
                    flag =1;
                    if (N1[d1] > N2[d2]){
                        inc1 = 0 ;
                    }else{
                        inc2 = 0;
                    }
                    
                }
            }
            if ((maplog.count(i)>0) & (flag == 0)){
                l1 = maplog[i];
                l2 = maplog[i+1];
                if (l1[d1] != l2[d2]){
                    flag =1;
                    if (l1[d1] > l2[d2]){
                        inc1 = 0 ;
                    }else{
                        inc2 = 0;
                    }
                    
                }
            }
        }
        if (flag == 1){
            index1[d1] = 0;
            index2[d2] = 0;
        }else{
            index1[d1] = d1 +1;
            index2[d2] = d2 +1;
        }
        if (d1 <n1){
        d1 = d1 + inc1;
        }
        if (d2 < n2){
        d2 = d2 + inc2;
        }
    }
    
    out[0] = index1[index1>0];
    out[1] = index2[index2>0];
    
    
    
    
    return out;
    
    
}


 
