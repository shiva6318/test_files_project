%%% @author Manas Parganiha
%%% @copyright (C) 2015, Manas Parganiha <manas@lynkit.in>
%%% @doc
%%%    Transecur is an open source GPS tracking system for various GPS tracking devices.
%%%
%%%    Copyright (C) 2015, Manas Parganiha <manas@lynkit.in>.
%%%
%%%    This file is part of Transecur.
%%%
%%%    Transecur is free software: you can redistribute it and/or  modify
%%%    it under the terms of the GNU Affero General Public License, version 3,
%%%    as published by the Free Software Foundation.
%%%
%%%    Transecur is distributed in the hope that it will be useful,
%%%    but WITHOUT ANY WARRANTY; without even the implied warranty of
%%%    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
%%%    GNU Affero General Public License for more details.
%%%
%%%    You should have received a copy of the GNU Affero General Public License
%%%    along with this program.  If not, see <http://www.gnu.org/licenses/>.
%%% @end
%%%-------------------------------------------------------------------
-module(ht_aeshhd1_protocol).
-author("Manas Parganiha <manas@lynkit.in>").
-behaviour(ranch_protocol).
-behaviour(gen_server).

-include("ht_hardware.hrl").
-include("ht_records.hrl").

-ifdef(TEST).
-compile(export_all).
-endif. % EXPORT_ALL

%% API
-export([start_link/4]).
%%-export([decode_test/0]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-define(TIMEOUT, infinity).

-define(SERVER, ?MODULE).
-define(SOCKET_OPTS, [{reuseaddr, true},{buffer, 65536},{recbuf, 16#FFFFF},{active, once}, {packet, raw}]).
%-define(SOCKET_OPTS, [{active, once}, {packet, line}]).

%%-define(SOCKET_OPTS, [{active, once}, {packet, line}]).
%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @end
%%--------------------------------------------------------------------
-spec(start_link(Ref :: any(), Socket :: any(), Transport :: any(), Opts :: any()) ->
             {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link(Ref, Socket, Transport, Opts) ->
    {ok, proc_lib:spawn_link(?MODULE, init, [{Ref, Socket, Transport, Opts}])}.

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
-spec(init(Args :: term()) ->
             {ok, State :: #state{}} | {ok, State :: #state{}, timeout() | hibernate} |
             {stop, Reason :: term()} | ignore).
init({Ref, Socket, Transport, Opts}) ->
    ok = ranch:accept_ack(Ref),
    ok = Transport:setopts(Socket, ?SOCKET_OPTS),
    Protocol = proplists:get_value(protocol, Opts),
  State = set_timeout(#state{timer = undefined}),
  UserName = "admin",
  Password = "Lynkit@W39",
  Url = "http://13.235.99.225:5984",
  Options = [{basic_auth, {UserName, Password}}],
  %couchbeam:start(),
  Server = couchbeam:server_connection(Url,Options),
  gen_server:enter_loop(?MODULE, [],#state{socket = Socket, transport = Transport, protocol = Protocol,db = Server}).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @end
%%--------------------------------------------------------------------
-spec(handle_call(Request :: term(), From :: {pid(), Tag :: term()},
                  State :: #state{}) ->
             {reply, Reply :: term(), NewState :: #state{}} |
             {reply, Reply :: term(), NewState :: #state{}, timeout() | hibernate} |
             {noreply, NewState :: #state{}} |
             {noreply, NewState :: #state{}, timeout() | hibernate} |
             {stop, Reason :: term(), Reply :: term(), NewState :: #state{}} |
             {stop, Reason :: term(), NewState :: #state{}}).
handle_call(_Request, _From, State) ->
    {reply, ok, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @end
%%--------------------------------------------------------------------
-spec(handle_cast(Request :: term(), State :: #state{}) ->
             {noreply, NewState :: #state{}} |
             {noreply, NewState :: #state{}, timeout() | hibernate} |
             {stop, Reason :: term(), NewState :: #state{}}).
handle_cast(_Request, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
-spec(handle_info(Info :: timeout() | term(), State :: #state{}) ->
             {noreply, NewState :: #state{}} |
             {noreply, NewState :: #state{}, timeout() | hibernate} |
             {stop, Reason :: term(), NewState :: #state{}}).
handle_info({command, Command}, State) ->
    do_execute_command(State, Command);
handle_info({tcp, _, <<67, 78, 88, 78, 0, 0, 0, 1, 0, 0, 4, 0, 27, 0, 0, 0, 77, 10>>}, State = #state{socket = Socket, transport = Transport}) ->
    Transport:setopts(Socket, ?SOCKET_OPTS),
    {noreply, State, ?TIMEOUT};

handle_info({tcp, Socket, Data}, State=#state{socket=Socket, transport=Transport}) ->
  %Transport:setopts(Socket, [{active, once},{reuseaddr, true}]),
  State0 = set_timeout(State),
 %ht_logger:info("HHD RAW SIZE ~w DATA: ~s HRX: ~s", [byte_size(Data),Data,parse_hex(Data)]),
  ht_logger:info_debug("HHD AES RAW DATA: ~w DATA: ~s HRX: ~s",[byte_size(Data), Data,parse_hex(Data)]),
 % ht_storage_couchdb_hhd:add_raw(Data),
%   Data1 = check_pattern(Data),
%   case do_process_frame(State0,Data1) of
%    {noreply, NewState, ?TIMEOUT} ->
%  Transport:setopts(Socket, ?SOCKET_OPTS),
%      {noreply, NewState};
%    {stop,normal,NewState} ->
%  Transport:setopts(Socket, ?SOCKET_OPTS),
%      {noreply, NewState};
%    _ ->
%  Transport:setopts(Socket, ?SOCKET_OPTS),
%      {noreply, State}
%  end;
 A = binary:split(Data,<<126>>,[global]),
 {_,State1,_} = send_multiple(State0,A),
 Transport:setopts(Socket, ?SOCKET_OPTS),
 {noreply, State1, ?TIMEOUT};
handle_info({device_cmd,CommandBin}, State = #state{socket = Socket, transport = Transport}) ->
  State0 = set_timeout(State),
  P1 = check_packet(CommandBin),
  Transport:send(Socket, P1),
  {noreply, State0, ?TIMEOUT};

handle_info({tcp_closed, _Socket}, State) ->
  {stop, normal, State};

handle_info({tcp_error, _Socket, etimedout}, State) ->
 ht_logger:info("AESHHD1 ETIMEOUT ERROR"),
  {stop, normal, State};
handle_info({tcp_error, _Socket, Reason}, State) ->
  {stop, Reason, State};

handle_info(timeout, State) ->
  ht_logger:info("TIMEOUT: 2", [ ]),
  {stop, normal, State};

handle_info({timeout, TimerRef, Reason}, State) ->
%  ht_logger:info("TIMEOUT: 3 ~s", [Reason]),
  {stop, normal, State};
     % timeout(State, Reason);

handle_info(_Info, State) ->
  {noreply, State}.
send_multiple(State,[]) -> {noreply,State,?TIMEOUT};
send_multiple(State,[H|T]) when byte_size(H) >= 15 ->
   Data = check_pattern(H),
   %do_process_frame(State,Data1),

%  em_logger:info("HHD G500 RAW DATA SIZE : ~w HRX: ~s",[byte_size(Data1),parse_hex(Data1)]),
 {_, State0, _} =  case do_process_frame(State,<<16#7E,Data/binary,16#7E>>) of
    {noreply, NewState, ?TIMEOUT} ->
      {noreply, NewState, ?TIMEOUT};
    {stop,normal,NewState} ->
      {noreply, NewState, ?TIMEOUT};
    _ ->
      {noreply, State, ?TIMEOUT}
  end,
   send_multiple(State0,T);
send_multiple(State,[H|T]) ->
  %em_logger:info("NOT SPLIT HHD G500 RAW DATA SIZE : ~w HRX: ~s",[byte_size(H),parse_hex(H)]),
 send_multiple(State,T).
%%%===================================================================
%%% Internal functions
%%%===================================================================

%% We set request_timeout when there are no active streams,
%% and idle_timeout otherwise.
set_timeout(State=#state{timer = Timer}) ->
       State0 = cancel_timeout(State),
       Name = idle_timeout,
       Timeout = 1000000,
       TimerRef = erlang:start_timer(Timeout, self(), Name),
       State0#state{timer=TimerRef}.

cancel_timeout(State=#state{timer=TimerRef}) ->
        ok = case TimerRef of
                undefined -> ok;
                _ -> erlang:cancel_timer(TimerRef, [{async, true}, {info, false}])
        end,
        State#state{timer=undefined}.

-spec timeout(_, _) -> no_return().
timeout(State, idle_timeout) ->
       terminate(shutdown,State).


 
do_process_frame( State = #state{socket = Socket, transport = Transport, assetId = 0, user = ""},<<16#7E, MsgId:2/binary, MsgBodyProp:2/binary, TerminalId:6/binary, MsgSerialNo:2/binary,_/binary>> = Data) when byte_size(Data) >= 15 ->
%handle_info({tcp, Socket, <<16#7E,_:2/binary,MsgBodyProp:2/binary,TerminalId:6/binary,_/binary>> = Data}, State = #state{socket = Socket, transport = Transport}) ->
  % Data1 = check_pattern(Data),
   <<_:1,ConnProp:1,_:4,Length:10>> = MsgBodyProp, 
%   <<Length:16>> = <<0:6,Len:10>>,
      case ht_storage_mongodb:get_asset_by_imei(parse_imei(TerminalId)) of
        {error, _Reason} ->
          ht_hhd_helper_lynktrac:do_send_response(State,TerminalId,MsgId,MsgSerialNo),
          ht_logger:info("[AESHHD1 packet] unit: ip = '~s' unknown device with imei = '~s'", [ht_inet:resolve(Socket), parse_imei(TerminalId)]),
          {stop, normal, State};
        {ok, Object} ->
          em_proc:registry(Object#asset.id, self()),
          %ht_logger:info("HHD LENGTH: ~w", [Length]),
          FrameSize = 15 + Length,
           case byte_size(Data) >= FrameSize of
            true ->
             %<<FrameData:FrameSize/binary, NewData/binary>> = Data,
             %do_process_data(State,Length,FrameData);
             do_process_frame(State#state{assetId = Object#asset.id,user = Object#asset.user},Data);
            false ->
             ht_logger:info("HHD Packet Reject size incorrect:~p", [parse_imei(TerminalId)])
           end
      end;

do_process_frame(State = #state{assetId = AssetId,socket = Socket, transport = Transport, db = Db},<<16#7E, MsgId:2/binary, MsgBodyProp:2/binary, TerminalId:6/binary, MsgSerialNo:2/binary,Rest/binary>>  = Data) when byte_size(Data) >= 15  ->
%   Data1 = check_pattern(Data),
       CommandModel = #command{deviceId = AssetId,type = <<"set_port_number">>,attributes = #{<<"port">> => <<"7032">>}},
       %em_manager_commands:execute(CommandModel), 
       %timer:send_after(1000,{command, CommandModel}),
   PID = case em_proc:get(AssetId) of
    undefined ->
       em_proc:registry(AssetId,self()),
       self();
       Pid -> Pid
    end,

   case PID /= self() of
     true ->
       ht_logger:info("HHD PID NOTTTTTT equal: ~w", [AssetId]),
       exit(PID,kill),
       em_proc:registry(AssetId,self());
     _ -> ok
    %   ht_logger:info("HHD PID equal: ~w", [AssetId])
   end,


   <<Encrypt:1,ConnProp:1,_:4,Length:10>> = MsgBodyProp, 
   %<<Length:16>> = <<0:6,Len:10>>,
          %ht_logger:info("HHD LENGTH: ~w", [Length]),
    case Encrypt of
       0 -> 
             ht_logger:info("HHD Packet Reject Encryption WRONG PORT:~p", [parse_imei(TerminalId)]);
       1 ->
         EncryptData = binary:part(Rest, {0, byte_size(Rest) - 2}), 
         DecryptData = list_to_binary(ht_aes:aes256ecb(EncryptData,0)),
         try ht_storage_couchdb_hhd:add_raw_data(Db,<<16#7E, MsgId:2/binary, MsgBodyProp:2/binary, TerminalId:6/binary, MsgSerialNo:2/binary,DecryptData/binary>>) catch Error:Reason -> {Error, Reason} end,
%         ht_logger:info_debug("HHD DECRYPT DATA LENGTH: ~w  ~s", [byte_size( DecryptData),parse_hex( DecryptData)]),
         FrameSize = 13 + Length,
         Size = byte_size(DecryptData),
         case (Size + 13)  >= FrameSize of
            true ->
             <<FrameData:FrameSize/binary, NewData/binary>> = <<16#7E, MsgId:2/binary, MsgBodyProp:2/binary, TerminalId:6/binary, MsgSerialNo:2/binary,DecryptData:Size/binary>>,
             ht_hhd_helper_lynktrac:do_process_data(State,Length-6,FrameData);
%             do_process_frame(State,NewData);
            false ->
             ht_logger:info("HHD AES Packet Reject size incorrect:~p", [parse_imei(TerminalId)])
           end
       end;

do_process_frame(State, <<>>) ->
   % Transport:setopts(Socket, ?SOCKET_OPTS),
    {noreply, State, ?TIMEOUT};
do_process_frame(State = #state{assetId = Asset_id}, Data)  ->
   % Transport:setopts(Socket, ?SOCKET_OPTS),
     DATA = parse_hex(Data),
     case bin_to_int(DATA) of
      0 -> ok;
      _ -> 
       ht_logger:info("HHD Packet Reject:~p", [Asset_id])
     end,  
    {noreply, State, ?TIMEOUT};
do_process_frame(State = #state{assetId = Asset_id}, Data) ->
    ht_logger:info("HHD Packet Reject:~p", [Asset_id]),
    %Transport:setopts(Socket, ?SOCKET_OPTS),
    {noreply, State, ?TIMEOUT}.   

bin_to_int(Bin) ->
    N = binary_to_list(Bin),
    case string:to_integer(N) of
        {error,no_integer} -> list_to_binary(N);
        {F,_Rest} -> F
    end.
%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
-spec(terminate(Reason :: (normal | shutdown | {shutdown, term()} | term()),
                State :: #state{}) -> term()).
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
-spec(code_change(OldVsn :: term() | {down, term()}, State :: #state{},
                  Extra :: term()) ->
             {ok, NewState :: #state{}} | {error, Reason :: term()}).
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
%% remove device command code %% shiva code
%%
do_execute_command(State = #state{transport = Transport, socket = Socket}, Command ) ->
      %ht_logger:info("CMD Received: ~p", [Command]), 
  case  ht_hhd_cmd_aes:do_encode_command(Command) of
    {ok, CommandBin} ->
      ht_logger:info("CMD SENT: ~s", [parse_hex(CommandBin)]),
      Transport:send(Socket,CommandBin),
      {noreply, State, ?TIMEOUT};
    {error, Reason} ->
      ht_logger:info("Error: ~s", [Reason]),
      {noreply, State, ?TIMEOUT}
  end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%
%%%%   P45 related Internal function
%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
parse_hex(Bin) ->
  list_to_binary(lists:flatten([io_lib:format("~2.16.0B", [X]) || X <- binary_to_list(Bin)])).


parse_imei(Bin) ->
  fix_imei(list_to_binary(lists:flatten([io_lib:format("~2.16.0B", [X]) || X <- binary_to_list(Bin)]))).

fix_imei(Bin) when byte_size(Bin) > 11 ->
  binary:part(Bin, {byte_size(Bin) - 11, 11});
fix_imei(Bin) -> Bin.

check_pattern(Data) ->
  A = binary:replace(Data,<<125,2>>,<<126>>,[global]),
  binary:replace(A,<<125,1>>,<<125>>,[global]).

check_packet(Data) ->
  A = binary:replace(Data,<<125>>,<<125,1>>,[global]),
  binary:replace(A,<<126>>,<<125,2>>,[global]).


hexstr_to_list([X,Y|T]) ->
    [int(X)*16 + int(Y) | hexstr_to_list(T)];
hexstr_to_list([]) ->
    [].

int(C) when $0 =< C, C =< $9 ->
    C - $0;
int(C) when $A =< C, C =< $F ->
    C - $A + 10;
int(C) when $a =< C, C =< $f ->
    C - $a + 10.


