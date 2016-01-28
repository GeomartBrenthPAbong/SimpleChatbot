-module(chatbot).
-compile(export_all).

newBot() ->
   register(botMemory,memory:connect()),
   spawn(chatbot,chatbot,["",""]). 
   
chatbot(BotPrevResponse,UserPrevResponse) ->
   receive
      {msg,SenderPID,Message} ->
	          botMemory ! {getResponse,self(),Message,UserPrevResponse,BotPrevResponse},
		      receive
			        {cont,Response} -> 
					       SenderPID ! {cont,Response},
						   chatbot(Response,Message);
				    {exit,Response} ->
					       SenderPID ! {exit,Response}
			  end
   end.
  
     