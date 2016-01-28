-module(client).
-export([startConversation/0]).

startConversation() ->
   register(chatbot,chatbot:newBot()),
   talkToBot().

talkToBot() ->
   case string:tokens(io:get_line("Your message >> "),"\n") of
       [] ->
	          chatbot ! {msg,self(),[]};
	   [Message] -> 
              chatbot ! {msg,self(),string:to_upper(Message)}
   end,
   receive
      {cont,Response} -> 
	                    io:fwrite("bogoBOT >> ~s~n",[Response]),
						talkToBot();
	  {exit,Response} -> io:fwrite("bogoBOT >> ~s~n",[Response])
   end.  