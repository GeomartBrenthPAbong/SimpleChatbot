-module(refresher).
-compile(export_all).




levenshtein(FString,SString) ->
  levenshteinH(FString,string:len(FString),SString,string:len(SString)).
 
levenshteinH(_,0,_,SSLen) -> SSLen; 
levenshteinH(_,FSLen,_,0) -> FSLen;
levenshteinH([FC|NFString]=FString,FSLen,[SC|NSString]=SString,SSLen) ->
   lists:min([levenshteinH(NFString,FSLen-1,SString,SSLen)+1,levenshteinH(FString,FSLen,NSString,SSLen-1)+1,levenshteinH(NFString,FSLen-1,NSString,SSLen-1)+compareLastChar(FC,SC)]).
   
   
compareLastChar(C,C) -> 0;
compareLastChar(_,_) -> 1. 

