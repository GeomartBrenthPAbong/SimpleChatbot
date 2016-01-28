-module(memory).
-export([connect/0,database/2]).

connect() ->
   spawn(memory,database,[loadKeywords("keywords.txt"),loadResponses("responses.txt")]).
   
database(Keywords,Responses) ->
   receive
       {getResponse,SenderPID,UserInput,PrevUserInput,PrevBotResponse} ->
	        case getBotResponse(UserInput,PrevUserInput,PrevBotResponse,Keywords,Responses) of
			   {cont,Response} -> 
			             SenderPID ! {cont,Response},
                         database(Keywords,Responses);						 
			   {exit,Response} ->
			             SenderPID ! {exit,Response}
			end
   end.
      
getBotResponse(UserInput,UserInput,PrevBotResponse,Keywords,Responses)  ->
   {cont,handleUserInput(Keywords,Responses,"USER_INPUT_REPETITION**",PrevBotResponse)};
getBotResponse([],_,PrevBotResponse,Keywords,Responses) -> 
   {cont,handleUserInput(Keywords,Responses,"NULL_INPUT**",PrevBotResponse)};
getBotResponse(UserInput,_,PrevBotResponse,Keywords,Responses) when UserInput == "BYE" orelse UserInput == "GOODBYE" -> 
   {exit,handleUserInput(Keywords,Responses,"GOODBYE",PrevBotResponse)};
getBotResponse(UserInput,_,PrevBotResponse,Keywords,Responses) -> 
   {cont,handleUserInput(Keywords,Responses,UserInput,PrevBotResponse)}.

handleUserInput([],Responses,_,PrevBotResponse) ->
   pickRandomlyFromList(getListOfResponsesAtLine("0",Responses),4,PrevBotResponse);   
handleUserInput([[CurrentKeyword|IDplusMaxN]|_],Responses,CurrentKeyword,PrevBotResponse) ->
   [LineNumber|[MaxN]] = IDplusMaxN,
   {MaxNum,_} = string:to_integer(MaxN),
   pickRandomlyFromList(getListOfResponsesAtLine(LineNumber,Responses),MaxNum,PrevBotResponse);
handleUserInput([_|TheRest],Responses,Keyword,PrevBotResponse) ->
   handleUserInput(TheRest,Responses,Keyword,PrevBotResponse).   
   
getListOfResponsesAtLine(LineNumber,[[LineNumber|ListOfResponses]|_]) -> ListOfResponses;
getListOfResponsesAtLine(LineNumber,[_|TheRest]) ->
   getListOfResponsesAtLine(LineNumber,TheRest).

pickRandomlyFromList(List,MaxN,PrevBotResponse) ->
   case string:equal(Response = pickRandomlyFromListH(List,generateRandomNumber(MaxN),1),PrevBotResponse) of
       true ->
	        pickRandomlyFromList(List,MaxN,PrevBotResponse);
		  _ ->
            Response
	end.

pickRandomlyFromListH([Response|_],RandomNumber,Counter) when Counter == RandomNumber ->
   Response;
pickRandomlyFromListH([_|TheRest],RandomNumber,Counter) ->
   pickRandomlyFromListH(TheRest,RandomNumber,Counter+1).

generateRandomNumber(MaxN) ->
	 {A,B,C} = now(), 
     random:seed(A,B,C),
     random:uniform(MaxN).   	
   
loadKeywords(FileName) ->
   loadKeywordsH(getFileContents(FileName),[]).
   
loadKeywordsH([],Keywords) -> Keywords;
loadKeywordsH([CurrentKeywordData|TheRest],Keywords) ->
   loadKeywordsH(TheRest,[string:tokens(CurrentKeywordData,"|")|Keywords]).

loadResponses(FileName) -> 
   loadResponsesH(getFileContents(FileName),[]).
  
loadResponsesH([],Responses) -> Responses;
loadResponsesH([CurrentResponses|TheRest],Responses) ->
   [ID,Resp] = string:tokens(CurrentResponses,"~"),
   loadResponsesH(TheRest,[[ID|string:tokens(Resp,"|")]|Responses]).
   
getFileContents(FileName) ->
   case file:open(FileName,[read]) of 
      {ok,IODevice} ->
	          getFileContentsH([],IODevice);
	  {error,Reason} ->
	          {error,Reason}
   end.
   
getFileContentsH(Contents,IODevice) ->
   case io:get_line(IODevice,"") of
      eof -> file:close(IODevice),Contents;
	  Line -> 
           [C] = string:tokens(Line,"\n"),	  
	       getFileContentsH([C|Contents],IODevice)
   end.

