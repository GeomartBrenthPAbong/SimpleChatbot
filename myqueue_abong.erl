%%----------------------------------
%% File: myqueue_abong.erl
%% Name: Geomart Brenth Abong
%% ID No.: 2008-0886
%% Section: CSX/CSY
%% Description: This program implements the Queue data structure
%%
%% Created: September 9 , 2013
%%----------------------------------

-module(myqueue_abong).
-export([new/0,is_empty/1,front/1,enqueue/2,dequeue/1,size/1,delete/1,queue/1]).

%% creates a process for the queue function and returns its process id
new() -> spawn(myqueue_abong,queue,[[]]).

%% returns true if the queue is empty otherwise, return false
is_empty(PID) -> 
			 case is_process_alive(PID) of
			    true ->
					     PID ! {self(),is_empty},
					     receive
						    Value -> Value
					     end;
			    false ->
				        {error,invalid_queue_process_id}
			 end.

%% returns the front data of the queue
front(PID)	-> 
             case is_process_alive(PID) of
			     true -> 
						 case is_empty(PID) of
							  false ->
								   PID ! {self(),front},	
								   receive
									  Value -> {ok,Value}
									end;
							  true ->
									{error,queue_is_empty}
						 end;
				 false ->
				         {error,invalid_queue_process_id}
			 end.

%% adds a new element to the queue
enqueue(Element,PID) ->			 
             case is_process_alive(PID) of
			     true -> 
					     PID ! {self(),enqueue,Element},		
					     receive
						    Value -> Value
						 end;
				 false ->
				         {error,invalid_queue_process_id}
			 end.
			 
%% deletes an element(front element) from the queue 
dequeue(PID) ->			 
             case is_process_alive(PID) of
			     true -> 
						 case is_empty(PID) of
							  false ->
								   PID ! {self(),dequeue},		
								   receive
									  Value -> Value
									end;
							  true ->
									{error,queue_is_empty}
						 end;
				 false ->
				         {error,invalid_queue_process_id}
			 end.	

%% returns the size of the queue 
size(PID) ->			 
             case is_process_alive(PID) of
			     true -> 
						 PID ! {self(),size},		
						 receive
							  Value -> Value
					     end;
				 false ->
				         {error,invalid_queue_process_id}
			 end.	

%% stops the current process 
delete(PID) -> 
             case is_process_alive(PID) of
			     true -> 
						 PID ! {self(),delete},		
						 receive
							  Value -> Value
					     end;
				 false ->
				         {error,invalid_queue_process_id}
			 end.	

%% responsible for processing the requests			 
queue(Queue) ->
             receive
				 {Sender,is_empty} ->
					 if 
						Queue == [] -> Sender ! true;
						true -> Sender ! false
					 end,
					 queue(Queue);
				 {Sender,front} ->
					 [Element|_] = lists:reverse(Queue),
					 Sender ! Element,
					 queue(Queue);
				 {Sender,enqueue,Element} ->
					 Sender ! ok,
					 queue([Element|Queue]);
				 {Sender,dequeue} ->
					 Sender ! ok,
					 [_|NewQueue] = lists:reverse(Queue),
					 queue(lists:reverse(NewQueue));
				 {Sender,size} ->
				     Sender ! getListSize(0,Queue),
					 queue(Queue);
				 {Sender,delete} ->
					 Sender ! ok				 
			 end.

%% returns the size of the queue 
getListSize(Size,[]) -> Size;			 
getListSize(Size,[_|T]) -> getListSize(Size+1,T). 


%% Sample Run

%% 1> Q = myqueue:new().
%% <0.39.0>
%% 2> myqueue:is_empty(Q).
%% true
%% 3> myqueue:front(Q).
%% {error,queue_is_empty}
%% 4> myqueue:dequeue(Q).
%% {error,queue_is_empty}
%% 5> myqueue:enqueue(a,Q).
%% ok
%% 6> myqueue:enqueue(2,Q).
%% ok
%% 7> myqueue:size(Q).
%% 2
%% 8> myqueue:is_empty(Q).
%% false
%% 9> myqueue:front(Q).
%% {ok,a}
%% 10> myqueue:dequeue(Q).
%% ok
%% 11> myqueue:front(Q).
%% {ok,2}
%% 12> myqueue:dequeue(Q).
%% ok
%% 13> myqueue:is_empty(Q).
%% true
%% 14> myqueue:size(Q).
%% 0
%% 15> myqueue:delete(Q). 
%% ok
%% 16> myqueue:enqueue(2,Q). 
%% {error,invalid_queue_process_id}
%% 17> myqueue:front(Q).
%% {error,invalid_queue_process_id}
%% 18> myqueue:is_empty(Q).
%% {error,invalid_queue_process_id}
%% 19> myqueue:dequeue(Q).
%% {error,invalid_queue_process_id}
%% 20> myqueue:size(Q).
%% {error,invalid_queue_process_id}