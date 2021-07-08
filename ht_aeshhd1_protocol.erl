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
  gen_server:enter_loop(?MODULE, [], #state{socket = Socket, transport = Transport, protocol = Protocol,db = Server}).

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

handle_info({tcp_error, _Socket, Reason}, State) ->
  {stop, Reason, State};

handle_info(timeout, State) ->
  %ht_logger:info("TIMEOUT: 2", [ ]),
  {stop, normal, State};

handle_info({timeout, TimerRef, Reason}, State) ->
%  ht_logger:info("TIMEOUT: 3 ~s", [Reason]),
  {stop, normal, State};
     % timeout(State, Reason);

handle_info(_Info, State) ->
  {noreply, State}.
send_multiple(State,[]) -> {noreply,State,?TIMEOUT};
send_multiple(State,[H|T]) when byte_size(H) >= 15 ->
   Data1 = check_pattern(<<16#7E,H/binary,16#7E>>),
   %do_process_frame(State,Data1),

%  em_logger:info("HHD G500 RAW DATA SIZE : ~w HRX: ~s",[byte_size(Data1),parse_hex(Data1)]),
 {_, State0, _} =  case do_process_frame(State,Data1) of
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
          do_send_response(State,TerminalId,MsgId,MsgSerialNo),
          ht_logger:info("[packet] unit: ip = '~s' unknown device with imei = '~s'", [ht_inet:resolve(Socket), parse_imei(TerminalId)]),
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
             do_process_data(State,Length-6,FrameData);
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
do_encode_command(#command{deviceId = DeviceId,type = ?CMD_SETTIME,attributes = #{?KEY_WAKEUP := HitFrequency}}) when HitFrequency >= 10 ->
    Id = integer_to_list(DeviceId),
    TerminalId = list_to_binary(hexstr_to_list("0"++Id)),
    Body = <<16#03,16#02,16#02,16#00,HitFrequency,16#06,16#02,16#00,16#3C,16#08,16#02,16#00,16#C3>>,
    Header = <<16#03,16#10,16#80,16#13,TerminalId/binary,16#00,16#0A>>,
    EncryptBody = get_encryption(Body),
    Crc = ht_checksum:crc(<<Header/binary,EncryptBody/binary>>),

  Packet = <<
    Header/binary,
    EncryptBody/binary,
    Crc:8/unsigned-integer
  >>,
  P1 = check_packet(Packet), 
  {ok,<<16#7E,P1/binary,16#7E>>};
do_encode_command(#command{deviceId = DeviceId,type = ?CMD_TIMERESET}) ->
    Id = integer_to_list(DeviceId),
    TerminalId = list_to_binary(hexstr_to_list("0"++Id)),
    ht_logger:info_debug("Terminal id: ~s", [parse_hex(TerminalId)]),
    A = calendar:system_time_to_rfc3339(erlang:system_time(second), [{time_designator, $-},{offset, "+08:00"}]),
    B = binary:part(list_to_binary(A),{0,byte_size(list_to_binary(A))-6}),
    C = binary:replace(B,[<<":">>,<<"-">>],<<"">>,[global]),
  %  D = list_to_binary(hexstr_to_list(binary_to_list(C))),
    Body = <<16#01,16#15,16#0E,C/binary>>,
    Header = <<16#03,16#10,16#80,16#17,TerminalId/binary,16#00,16#0C>>,
    EncryptBody = get_encryption(Body),
    Crc = ht_checksum:crc(<<Header/binary,EncryptBody/binary>>),

  Packet = <<
    Header/binary,
    EncryptBody/binary,
    Crc:8/unsigned-integer
  >>,
  P1 = check_packet(Packet),
  {ok,<<16#7E,P1/binary,16#7E>>};
do_encode_command(#command{deviceId = DeviceId,type = ?CMD_SETTIME,attributes = #{?KEY_WAKEUP := HitFrequency}})  ->
    Id = integer_to_list(DeviceId),
    TerminalId = list_to_binary(hexstr_to_list("0"++Id)),
    Hb = HitFrequency * 60,
    HB = Hb + 60,
    Body = <<16#03,16#02,16#02,16#00,16#00,16#06,16#02,Hb:16/unsigned-integer,16#08,16#02,HB:16/unsigned-integer>>,
    Header = <<16#03,16#10,16#80,16#13,TerminalId/binary,16#00,16#0A>>,
    EncryptBody = get_encryption(Body),
    Crc = ht_checksum:crc(<<Header/binary,EncryptBody/binary>>),

  Packet = <<
    Header/binary,
    EncryptBody/binary,
    Crc:8/unsigned-integer
  >>,

  P1 = check_packet(Packet), 

  {ok,<<16#7E,P1/binary,16#7E>>};
do_encode_command(#command{deviceId = DeviceId,type = ?CMD_SLEEP})  ->
    Id = integer_to_list(DeviceId),
    TerminalId = list_to_binary(hexstr_to_list("0"++Id)),
    Body = <<16#01,16#27,16#02,16#27,16#0F>>,
    Header = <<16#03,16#10,16#80,16#0B,TerminalId/binary,16#00,16#0A>>,
    EncryptBody = get_encryption(Body),
    Crc = ht_checksum:crc(<<Header/binary,EncryptBody/binary>>),

  Packet = <<
    Header/binary,
    EncryptBody/binary,
    Crc:8/unsigned-integer
  >>,

  P1 = check_packet(Packet), 

  {ok,<<16#7E,P1/binary,16#7E>>};
do_encode_command(#command{deviceId = DeviceId,type = <<"clean_card_random">>})  ->
    Id = integer_to_list(DeviceId),
    TerminalId = list_to_binary(hexstr_to_list("0"++Id)),
    Body = <<16#01,16#23,16#01,16#04>>,
    Header = <<16#03,16#10,16#80,16#0A,TerminalId/binary,16#00,16#0A>>,
    EncryptBody = get_encryption(Body),
    Crc = ht_checksum:crc(<<Header/binary,EncryptBody/binary>>),

  Packet = <<
    Header/binary,
    EncryptBody/binary,
    Crc:8/unsigned-integer
  >>,

  P1 = check_packet(Packet),

  {ok,<<16#7E,P1/binary,16#7E>>};
do_encode_command(#command{deviceId = DeviceId,type = ?CMD_UNLOCK,attributes = #{?KEY_PASSWORD := Password}}) ->
    Id = integer_to_list(DeviceId),
    TerminalId = list_to_binary(hexstr_to_list("0"++Id)),
    ht_logger:info_debug("Terminal id: ~s", [parse_hex(TerminalId)]),
    Body = <<16#01,16#24,16#01,16#00>>,
    Header = <<16#03,16#10,16#80,16#0A,TerminalId/binary,16#00,16#0C>>,
    EncryptBody = get_encryption(Body),
    Crc = ht_checksum:crc(<<Header/binary,EncryptBody/binary>>),

  Packet = <<
    Header/binary,
    EncryptBody/binary,
    Crc:8/unsigned-integer
  >>,
  P1 = check_packet(Packet),
  {ok,<<16#7E,P1/binary,16#7E>>};
do_encode_command(#command{deviceId = DeviceId,type = ?CMD_LOCK,attributes = #{?KEY_PASSWORD := Password}}) ->
    Id = integer_to_list(DeviceId),
    TerminalId = list_to_binary(hexstr_to_list("0"++Id)),
    ht_logger:info_debug("Terminal id: ~s", [parse_hex(TerminalId)]),
    Body = <<16#01,16#24,16#01,16#01>>,
    Header = <<16#03,16#10,16#80,16#0A,TerminalId/binary,16#00,16#0C>>,
    EncryptBody = get_encryption(Body),
    Crc = ht_checksum:crc(<<Header/binary,EncryptBody/binary>>),

  Packet = <<
    Header/binary,
    EncryptBody/binary,
    Crc:8/unsigned-integer
  >>,
  P1 = check_packet(Packet), 
  {ok,<<16#7E,P1/binary,16#7E>>};
do_encode_command(#command{deviceId = DeviceId,type = ?CMD_RESET}) ->
    Id = integer_to_list(DeviceId),
    TerminalId = list_to_binary(hexstr_to_list("0"++Id)),
    ht_logger:info_debug("Terminal id: ~s", [parse_hex(TerminalId)]),
    Body = <<16#01,16#23,16#01,16#02>>,
    Header = <<16#03,16#10,16#80,16#0A,TerminalId/binary,16#00,16#0C>>,
    EncryptBody = get_encryption(Body),
    Crc = ht_checksum:crc(<<Header/binary,EncryptBody/binary>>),

  Packet = <<
    Header/binary,
    EncryptBody/binary,
    Crc:8/unsigned-integer
  >>,
  P1 = check_packet(Packet),
  {ok,<<16#7E,P1/binary,16#7E>>};
do_encode_command(#command{deviceId = DeviceId,type = ?TYPE_SET_TIMEZONE,attributes = #{?KEY_TIMEZONE := Timezone}}) ->
    Id = integer_to_list(DeviceId),
    TerminalId = list_to_binary(hexstr_to_list("0"++Id)),
    ht_logger:info_debug("Terminal id: ~s", [parse_hex(TerminalId)]),
    Tz = list_to_binary(integer_to_list(Timezone)),
    Body = <<16#01,16#37,16#01,16#16>>,
    Header = <<16#03,16#10,16#80,16#0A,TerminalId/binary,16#00,16#0C>>,
    EncryptBody = get_encryption(Body),
    Crc = ht_checksum:crc(<<Header/binary,EncryptBody/binary>>),

  Packet = <<
    Header/binary,
    EncryptBody/binary,
    Crc:8/unsigned-integer
  >>,

  P1 = check_packet(Packet), 
  {ok,<<16#7E,P1/binary,16#7E>>};
do_encode_command(#command{deviceId = DeviceId,type = <<"clean_card_random">>})  ->
    Id = integer_to_list(DeviceId),
    TerminalId = list_to_binary(hexstr_to_list("0"++Id)),
    Body = <<16#01,16#23,16#01,16#01>>,
    Header = <<16#03,16#10,16#80,16#0A,TerminalId/binary,16#00,16#0A>>,
    EncryptBody = get_encryption(Body),
    Crc = ht_checksum:crc(<<Header/binary,EncryptBody/binary>>),

  Packet = <<
    Header/binary,
    EncryptBody/binary,
    Crc:8/unsigned-integer
  >>,

  P1 = check_packet(Packet),

  {ok,<<16#7E,P1/binary,16#7E>>};
do_encode_command(#command{deviceId = DeviceId,type = <<"get_mcu_version">>}) ->
    Id = integer_to_list(DeviceId),
    TerminalId = list_to_binary(hexstr_to_list("0"++Id)),
    ht_logger:info("Terminal id: ~s ~p", [parse_hex(TerminalId)]),
    Body = <<16#01,16#3A>>,
    BB = byte_size(Body) + 6,
    Header = <<16#03,16#12,16#80,BB:8/unsigned-integer,TerminalId/binary,16#00,16#0D>>,
    ht_logger:info("HEADER: ~s BODY: ~s ~p", [parse_hex(Header), parse_hex(Body)]),
    EncryptBody = get_encryption(Body),
    Crc = ht_checksum:crc(<<Header/binary,EncryptBody/binary>>),

  Packet = <<
    Header/binary,
    EncryptBody/binary,
    Crc:8/unsigned-integer
  >>,
  P1 = check_packet(Packet),
  {ok,<<16#7E,P1/binary,16#7E>>};
do_encode_command(#command{deviceId = DeviceId,type = <<"get_bt_version">>}) ->
    Id = integer_to_list(DeviceId),
    TerminalId = list_to_binary(hexstr_to_list("0"++Id)),
    ht_logger:info("Terminal id: ~s ~p", [parse_hex(TerminalId)]),
    Body = <<16#01,16#3B>>,
    BB = byte_size(Body) + 6,
    Header = <<16#03,16#12,16#80,BB:8/unsigned-integer,TerminalId/binary,16#00,16#0D>>,
    ht_logger:info("HEADER: ~s BODY: ~s ~p", [parse_hex(Header), parse_hex(Body)]),
    EncryptBody = get_encryption(Body),
    Crc = ht_checksum:crc(<<Header/binary,EncryptBody/binary>>),

  Packet = <<
    Header/binary,
    EncryptBody/binary,
    Crc:8/unsigned-integer
  >>,
  P1 = check_packet(Packet),
  {ok,<<16#7E,P1/binary,16#7E>>};
do_encode_command(#command{deviceId = DeviceId,type = <<"add_card_random">>,attributes = #{?CARD_NUMBER := Ver,<<"card_index">> := Index}} = Command) ->
    Id = integer_to_list(DeviceId),
    TerminalId = list_to_binary(hexstr_to_list("0"++Id)),
%    ht_logger:info("Terminal id: ~s CARD NO : ~s~p", [parse_hex(TerminalId), Ver]),
    Len = byte_size(Ver),
    case Len > 269 of
      true ->
       [Version,Rest] = binary:split(Ver,<<",">>,[{scope,{269,2}}]),
       Card_orig =  binary:split(Version,<<",">>,[global]),
       Length = length(Card_orig),
       CommandModel = Command#command{attributes = #{?CARD_NUMBER => Rest,<<"card_index">> => Index+1}},
       %em_manager_commands:execute(CommandModel), 
       timer:send_after(6000,{command, CommandModel}),
       create_card_packet(Length,Card_orig,TerminalId,Index);
     false ->
       Card_orig =  binary:split(Ver,<<",">>,[global]),
       Length = length(Card_orig),
       create_card_packet(Length,Card_orig,TerminalId,Index)
    end;
do_encode_command(#command{deviceId = DeviceId,type = <<"get_card_random">>,attributes = #{?CARD_NUMBER := Version}}) ->
    Id = integer_to_list(DeviceId),
    TerminalId = list_to_binary(hexstr_to_list("0"++Id)),
    ht_logger:info("Terminal id: ~s CARD NO: ~s~p", [parse_hex(TerminalId), Version]),
    Card_list =  binary:split(Version,<<",">>,[global]),
    Length = length(Card_list),
    Card_cmd = create_list(Length,Card_list),

    Body = Card_cmd,
    BB = byte_size(Body) + 6,
    Header = <<16#03,16#16,1:1,0:1,0:1,0:1,0:1,0:1,BB:10,TerminalId/binary,16#00,16#0D>>,
    ht_logger:info("HEADER: ~s BODY: ~s ~p", [parse_hex(Header), parse_hex(Body)]),
    EncryptBody = get_encryption(Body),
    Crc = ht_checksum:crc(<<Header/binary,EncryptBody/binary>>),

  Packet = <<
    Header/binary,
    EncryptBody/binary,
    Crc:8/unsigned-integer
  >>,
  P1 = check_packet(Packet),
  {ok,<<16#7E,P1/binary,16#7E>>};
do_encode_command(#command{deviceId = DeviceId,type = ?CMD_ADD_CARD,attributes = #{?CARD_NUMBER := Version}}) ->
    Id = integer_to_list(DeviceId),
    TerminalId = list_to_binary(hexstr_to_list("0"++Id)),
    ht_logger:info_debug("Terminal id: ~s", [parse_hex(TerminalId)]),
    VV = byte_size(Version),
    Size = integer_to_binary(VV),
    SS = byte_size(Size),

    Body = <<16#33,16#2A,16#4D,16#30,16#36,16#2C,Size:SS/binary,16#2C,Version:VV/binary,16#23>>,
    BB = byte_size(Body) + 6,
    Header = <<16#89,16#00,16#80,BB:8/unsigned-integer,TerminalId/binary,16#00,16#0D>>,
    EncryptBody = get_encryption(Body),
    Crc = ht_checksum:crc(<<Header/binary,EncryptBody/binary>>),

  Packet = <<
    Header/binary,
    EncryptBody/binary,
    Crc:8/unsigned-integer
  >>,
  P1 = check_packet(Packet),
  {ok,<<16#7E,P1/binary,16#7E>>};


do_encode_command(#command{deviceId = DeviceId,type = ?CMD_READ_CARD}) ->
    Id = integer_to_list(DeviceId),
    TerminalId = list_to_binary(hexstr_to_list("0"++Id)),
    ht_logger:info_debug("Terminal id: ~s", [parse_hex(TerminalId)]),
    Body = <<16#33,16#2A,16#4D,16#30,16#37,16#2C,16#30,16#30,16#2C,16#23>>,
    BB = byte_size(Body) + 6,
    Header = <<16#89,16#00,16#80,BB:8/unsigned-integer,TerminalId/binary,16#00,16#0D>>,
    ht_logger:info_debug("CARD READ: ~s  ~s", [parse_hex(Body),parse_hex(Header)]),
    EncryptBody = get_encryption(Body),
    Crc = ht_checksum:crc(<<Header/binary,EncryptBody/binary>>),

  Packet = <<
    Header/binary,
    EncryptBody/binary,
    Crc:8/unsigned-integer
  >>,
  P1 = check_packet(Packet),
  {ok,<<16#7E,P1/binary,16#7E>>};

do_encode_command(#command{deviceId = DeviceId,type = ?CMD_BT_PASSWORD,attributes = #{?PASSWORD := Password}}) when byte_size(Password) == 8  ->
    Id = integer_to_list(DeviceId),
    TerminalId = list_to_binary(hexstr_to_list("0"++Id)),
    ht_logger:info_debug("Terminal id: ~s", [parse_hex(TerminalId)]),
    Body = <<16#01,16#36,16#08,Password/binary>>,
    Header = <<16#03,16#10,16#80,16#10,TerminalId/binary,16#00,16#0C>>,
    EncryptBody = get_encryption(Body),
    Crc = ht_checksum:crc(<<Header/binary,EncryptBody/binary>>),

  Packet = <<
    Header/binary,
    EncryptBody/binary,
    Crc:8/unsigned-integer
  >>,
  P1 = check_packet(Packet), 
  {ok,<<16#7E,P1/binary,16#7E>>};

do_encode_command(#command{deviceId = DeviceId,type = ?CMD_GETALL}) ->
    Id = integer_to_list(DeviceId),
    TerminalId = list_to_binary(hexstr_to_list("0"++Id)),
    ht_logger:info_debug("Terminal id: ~s", [parse_hex(TerminalId)]),
    Body = <<16#24,16#01,16#02,16#03,16#04,16#05,16#06,16#07,16#08,16#09,16#0A,16#0B,16#0C,16#0D,16#0E,16#0F,16#10,16#11,16#12,16#13,16#14,16#15,16#16,16#23,16#24,16#25,16#26,16#27,16#28,16#29,16#33,16#35,16#36,16#37,16#30,16#31,16#32>>,
    Header = <<16#03,16#12,16#80,16#2B,TerminalId/binary,16#00,16#0D>>,
    EncryptBody = get_encryption(Body),
    Crc = ht_checksum:crc(<<Header/binary,EncryptBody/binary>>),

  Packet = <<
    Header/binary,
    EncryptBody/binary,
    Crc:8/unsigned-integer
  >>,

  P1 = check_packet(Packet), 
  {ok,<<16#7E,P1/binary,16#7E>>};
do_encode_command(#command{deviceId = DeviceId,type = ?CMD_UPDATE,attributes = #{?VERSION := Version}}) ->
    Id = integer_to_list(DeviceId),
    TerminalId = list_to_binary(hexstr_to_list("0"++Id)),
    ht_logger:info("Terminal id: ~s", [parse_hex(TerminalId)]),
    VV = byte_size(Version),
    Size = integer_to_binary(VV),
    SS = byte_size(Size),
    ht_storage_redis:update_single_field(DeviceId,fwUpdateChecked,true),
    ht_storage_redis:update_single_field(DeviceId,fwUpdateTime,ht_helper_time:timestamp()),
    Body = <<16#33,16#2A,16#44,16#30,16#31,16#2C,Size:SS/binary,16#2C,Version:VV/binary,16#23>>,
    BB = byte_size(Body) + 6,
    Header = <<16#89,16#00,16#00,BB:8/unsigned-integer,TerminalId/binary,16#00,16#0D>>,
    EncryptBody = get_encryption(Body),
    Crc = ht_checksum:crc(<<Header/binary,EncryptBody/binary>>),

  Packet = <<
    Header/binary,
    EncryptBody/binary,
    Crc:8/unsigned-integer
  >>,
  P1 = check_packet(Packet),
  {ok,<<16#7E,P1/binary,16#7E>>};
do_encode_command(#command{deviceId = DeviceId,type = ?CMD_VERSION,attributes = #{}}) ->
    Id = integer_to_list(DeviceId),
    TerminalId = list_to_binary(hexstr_to_list("0"++Id)),
    ht_logger:info("Terminal id: ~s", [parse_hex(TerminalId)]),
    ht_storage_redis:update_single_field(DeviceId,fwVersionChecked,true),
    ht_storage_redis:update_single_field(DeviceId,fwVersionTime,ht_helper_time:timestamp()),
    Body = <<16#33,16#2A,16#44,16#30,16#33,16#2C,16#00,16#23>>,
    BB = byte_size(Body) + 6,
    Header = <<16#89,16#00,16#00,BB:8/unsigned-integer,TerminalId/binary,16#00,16#0D>>,
    EncryptBody = get_encryption(Body),
    Crc = ht_checksum:crc(<<Header/binary,EncryptBody/binary>>),

  Packet = <<
    Header/binary,
    EncryptBody/binary,
    Crc:8/unsigned-integer
  >>,
  P1 = check_packet(Packet),
  {ok,<<16#7E,P1/binary,16#7E>>};
do_encode_command(#command{deviceId = DeviceId,type = ?CMD_OTA_CONT,attributes = #{?VERSION := Version}}) ->
    Id = integer_to_list(DeviceId),
    case check_fwStatus(ht_storage_redis:get_single_field(DeviceId,fwUpdateStatus)) of
    {ok,<<"end">>} -> ok;
    {ok,<<"ongoing">>} ->
      TerminalId = list_to_binary(hexstr_to_list("0"++Id)),
      ht_logger:info("Terminal id: ~s", [parse_hex(TerminalId)]),
      {ok,Total}= ht_storage_redis:get_single_field(DeviceId,fwUpdateTotal),
      {ok,Current} = ht_storage_redis:get_single_field(DeviceId,fwUpdateCurrent),
      {ok,Path} = ht_storage_redis:get_single_field(DeviceId,fwPath),
      %ht_storage_redis:update_single_field(DeviceId,fwCrc,ht_checksum:crc16_ccitt(Data)),
      {ok,CRC} = ht_storage_redis:get_single_field(DeviceId,fwCrc),
      {ok,File} = file:open(Path,[read]),
      {ok,Data1} = file:pread(File,binary_to_integer(Current)*512,512),
      ht_logger:info("FW DATA : ~p ~n", [Data1]),
      P1 = fw_update(TerminalId,binary_to_integer(Total),list_to_binary(Data1),binary_to_integer(Current)+1,CRC)
    end;
do_encode_command(#command{deviceId = DeviceId,type = ?CMD_OTA,attributes = #{data := Data,version := Version}}) ->
    Id = integer_to_list(DeviceId),
    case check_fwStatus(ht_storage_redis:get_single_field(DeviceId,fwUpdateStatus)) of
    {ok,<<"end">>} ->
       ht_storage_redis:update_single_field(DeviceId,fwUpdateStatus,"ongoing"),
       TerminalId = list_to_binary(hexstr_to_list("0"++Id)),
       case filelib:is_file("/opt/lynktrac/bin/"++binary_to_list(Version)++"/firmware.bin") of
        true -> ok;
        false ->
         file:make_dir("/opt/lynktrac/bin/"++binary_to_list(Version)),
         {ok, File} = file:open("/opt/lynktrac/bin/"++binary_to_list(Version)++"/firmware.bin",[write]),
         file:write(File,Data),
         file:close(File)
       end,
       ht_logger:info("Terminal id: ~s", [parse_hex(TerminalId)]),

       ht_storage_redis:update_single_field(DeviceId,fwUpdateReq,true),
       ht_storage_redis:update_single_field(DeviceId,fwUpdateTime,ht_helper_time:timestamp()),
       Size = byte_size(Data),
       Reminder = Size rem 512,
       TotalBinPacket = case Reminder of
        0 ->
           Size div 512;
        _ ->
          (Size div 512) + 1
      end,
      ht_storage_redis:update_single_field(DeviceId,fwUpdateTotal,TotalBinPacket),
      ht_storage_redis:update_single_field(DeviceId,fwUpdateCurrent,0),
      ht_storage_redis:update_single_field(DeviceId,fwPath,"/opt/lynktrac/bin/"++binary_to_list(Version)++"/firmware.bin"),
      ht_storage_redis:update_single_field(DeviceId,fwCrc,ht_checksum:crc16_ccitt(Data)),
      <<FirstPacket:512/binary,_/binary>> = Data,
      P1 = fw_update(TerminalId,TotalBinPacket,FirstPacket,1,0);
    {ok,<<"ongoing">>} ->
      TerminalId = list_to_binary(hexstr_to_list("0"++Id)),
      ht_logger:info("Terminal id: ~s", [parse_hex(TerminalId)]),
      {ok,Total}= ht_storage_redis:get_single_field(DeviceId,fwUpdateTotal),
      {ok,Current} = ht_storage_redis:get_single_field(DeviceId,fwUpdateCurrent),
      {ok,Path} = ht_storage_redis:get_single_field(DeviceId,fwPath),
      %ht_storage_redis:update_single_field(DeviceId,fwCrc,ht_checksum:crc16_ccitt(Data)),
      {ok,CRC} = ht_storage_redis:get_single_field(DeviceId,fwCrc),
      {ok,File} = file:open(Path,[read]),
      {ok,Data1} = file:pread(File,binary_to_integer(Current)*512,512),
      ht_logger:info("FW DATA : ~p ~n", [Data1]),
      P1 = fw_update(TerminalId,binary_to_integer(Total),list_to_binary(Data1),binary_to_integer(Current)+1,CRC)
    end;
do_encode_command(#command{} = Cmd) ->
  ht_logger:info_debug("Unknown Command: ~s", [Cmd]),
  {error, unknown_command}.
create_card_packet(Length,Card_list,TerminalId,Index) ->
    Card_cmd = create_array(Length,Card_list,Index),
    Body = <<Length:16,Length:16,Card_cmd/binary>>,
    BB = byte_size(Body) + 6,
    Msg_id = Index+1,
    Header = <<16#03,16#14,1:1,0:1,0:1,0:1,0:1,0:1,BB:10,TerminalId/binary,Msg_id:16/unsigned-integer>>,
%    ht_logger:info("HEADER: ~s BODY: ~s ~p", [parse_hex(Header), parse_hex(Body)]),
    EncryptBody = get_encryption(Body),
    Crc = ht_checksum:crc(<<Header/binary,EncryptBody/binary>>),

    Packet = <<
       Header/binary,
       EncryptBody/binary,
       Crc:8/unsigned-integer
      >>,
    P1 = check_packet(Packet),
    {ok,<<16#7E,P1/binary,16#7E>>}.

fw_update(TerminalId,TotalBinPacket,FirstPacket,CurrentPacket,CRC) when TotalBinPacket =:= CurrentPacket ->
   % ht_logger:info("FW DATA : ~p ~n", [FirstPacket]),
    TotalPackage = byte_size(FirstPacket) + 4,
    FinalPacket = <<TotalBinPacket:16/unsigned-integer,CurrentPacket:16/unsigned-integer,FirstPacket/binary>>,
    SS = integer_to_binary(TotalPackage + 2),
    SL = byte_size(SS),
   % ht_logger:info("FW DATA : ~p  ~p ~p ~p ~p ~n", [SL,SS,FinalPacket,CRC,TotalPackage]),
    PacketCrc = binary_to_integer(CRC),
    Body = <<16#33,16#2A,16#44,16#30,16#32,16#2C,SS:SL/binary,16#2C,FinalPacket:TotalPackage/binary,PacketCrc:16/unsigned-integer,16#23>>,
    BB = byte_size(Body) + 6,
    Header = <<16#89,16#00,1:1,0:1,0:1,0:1,0:1,0:1,BB:10,TerminalId/binary,16#00,16#0D>>,
    EncryptBody = get_encryption(Body),
    Crc = ht_checksum:crc(<<Header/binary,EncryptBody/binary>>),
    Packet = <<
      Header/binary,
      EncryptBody/binary,
      Crc:8/unsigned-integer
    >>,
    P1 = check_packet(Packet),
    ht_logger:info_debug("FW LAST PACKET : ~p ~n", [parse_hex(P1)]),
    {ok,<<16#7E,P1/binary,16#7E>>};
fw_update(TerminalId,TotalBinPacket,FirstPacket,1,_) ->
    TotalPackage = 512 + 4,
    FinalPacket = <<TotalBinPacket:16/unsigned-integer,1:16/unsigned-integer,FirstPacket:512/binary>>,
    SS = integer_to_binary(TotalPackage),
    SL = byte_size(SS),
    Body = <<16#33,16#2A,16#44,16#30,16#32,16#2C,SS:SL/binary,16#2C,FinalPacket:516/binary,16#23>>,
    BB = byte_size(Body) + 6,
    Header = <<16#89,16#00,1:1,0:1,0:1,0:1,0:1,0:1,BB:10,TerminalId/binary,16#00,16#0D>>,
    EncryptBody = get_encryption(Body),
    Crc = ht_checksum:crc(<<Header/binary,EncryptBody/binary>>),
    Packet = <<
      Header/binary,
      EncryptBody/binary,
      Crc:8/unsigned-integer
    >>,
    P1 = check_packet(Packet),
    {ok,<<16#7E,P1/binary,16#7E>>};
fw_update(TerminalId,TotalBinPacket,FirstPacket,CurrentPacket,_) ->
    TotalPackage = 512 + 4,
    FinalPacket = <<TotalBinPacket:16/unsigned-integer,CurrentPacket:16/unsigned-integer,FirstPacket:512/binary>>,
    SS = integer_to_binary(TotalPackage),
    SL = byte_size(SS),
    Body = <<16#33,16#2A,16#44,16#30,16#32,16#2C,SS:SL/binary,16#2C,FinalPacket:516/binary,16#23>>,
    BB = byte_size(Body),
    Header = <<16#89,16#00,1:1,0:1,0:1,0:1,0:1,0:1,BB:10,TerminalId/binary,16#00,16#0D>>,
    EncryptBody = get_encryption(Body),
    Crc = ht_checksum:crc(<<Header/binary,EncryptBody/binary>>),
    Packet = <<
      Header/binary,
      EncryptBody/binary,
      Crc:8/unsigned-integer
    >>,
    P1 = check_packet(Packet),
    {ok,<<16#7E,P1/binary,16#7E>>}.


create_list(Length,Card) ->
   create_list(Length,Card,<<"">>).
create_list(0,Card,Final) -> Final;
create_list(Length,Card,Final) ->
    Len = Length -1,
    TerminalId = binary_to_integer(lists:nth(Length,Card)),
create_list(Length - 1,Card,<<TerminalId:16,Final/binary>>).

create_array(Length,Card,Index) ->
   create_array(Length,Card,Index,<<"">>).
create_array(0,Card,Index,Final) -> Final;
create_array(Length,Card,Index,Final) ->
    Len = 30*Index + Length - 1,
    TerminalId = list_to_binary(hexstr_to_list(binary_to_list(lists:nth(Length,Card)))),
create_array(Length - 1,Card,Index,<<Len:16,TerminalId:4/binary,0,0,0,0,0,0,0,0,0,0,0,0,Final/binary>>).
do_execute_command(State = #state{transport = Transport, socket = Socket}, Command ) ->
      %ht_logger:info("CMD Received: ~p", [Command]),
  case ht_hhd_cmd_aes:do_encode_command(Command) of
    {ok, CommandBin} ->
      ht_logger:info("CMD SENT: ~s", [parse_hex(CommandBin)]),
      Transport:send(Socket,CommandBin),
      {noreply, State, ?TIMEOUT};
    {error, Reason} ->
      ht_logger:info("Error: ~s", [Reason]),
      {noreply, State, ?TIMEOUT}
  end.
do_process_data(State = #state{socket = Socket,assetId=AssetId,protocol=Protocol,user=User,transport=Transport},Length, Data)  ->
<<16#7E, MsgId:2/binary, MsgBodyProp:2/binary, TerminalId:6/binary, MsgSerialNo:2/binary,_:6/binary,MsgBody:Length/binary,_/binary>> = Data,
    
  case MsgId of
  <<16#02,16#10>> ->
      do_send_response(State,TerminalId,MsgId,MsgSerialNo),
      check_bulk_data(State,MsgBody,#track{});
  <<16#02,16#00>> ->
    <<AlertId:4/binary,Status:4/binary,Latitude:4/binary,Longitude:4/binary,Altitude:16/unsigned-integer,Speed:16/unsigned-integer,_:16/unsigned-integer,Year:1/binary,Month:1/binary,Day:1/binary,Hour:1/binary,Minute:1/binary,Second:1/binary,Rest/binary>> = MsgBody,
   do_send_response(State,TerminalId,MsgId,MsgSerialNo),
    Track = #track{
       asset_id = AssetId,
       device_name = atom_to_binary(Protocol, utf8),
       socket_ip = ht_inet:resolve(Socket),
        packet_time = parse_date(Year,Month,Day,Hour,Minute,Second),
        latitude = parse_latitude(Latitude,Status),
        longitude = parse_longitude(Longitude,Status),
        altitude = Altitude,
        speed = Speed/10,
        packet_type = live,
        sim_slot = parse_sim_status(AlertId),
        hall = parse_hall_status(AlertId),
        travel_swt = parse_travel_status(AlertId),
        valid = parse_status(Status),
        charge_status = parse_charging(Status),
        accuracy = parse_accuracy(AlertId),
        satellite_count = parse_satellite(Status) 
      },
%  ht_logger:info("save message => unit: ip = '~s' id = '~w' imei position: ~w", [ht_inet:resolve(Socket), AssetId, Track]),

   case Track#track.packet_time == {error,badarg} of
      false ->

   UpdatedTrack = decode(State,Track,Rest),
%      ht_logger:info("save LIVE DATA => unit: ip = '~s' id = '~w' position: ~w", [ht_inet:resolve(Socket), AssetId, Track]),
      case check(UpdatedTrack#track.latitude,UpdatedTrack#track.longitude) of
        valid ->
%          ht_logger:info("VALID => unit: ip = '~s' id = '~w' position: ~w", [ht_inet:resolve(Socket), AssetId, Track]),
          case ht_storage_redis:get_previous_packet_time(AssetId) of
           {packet,[OldPacket]} ->
	      CurrentTime = em_helper_time:timestamp() + 30,
              case check_packet_state(UpdatedTrack#track.packet_time,OldPacket,CurrentTime) of
                live_data ->
                        UT = ht_hhd_helper_lynktrac:hhd_event_adder(State,UpdatedTrack),

%                  ht_logger:info("LIVE DATA => unit: ip = '~s' id = '~w' position: ~w", [ht_inet:resolve(Socket), AssetId, Track]),
                  %RRR = ht_storage_mongodb:check_order_by_assetid(AssetId),
                   %ht_logger:info("LIVE DATA => unit: ip = '~s' id = '~w' position: ~w", [ AssetId, RRR]), 
                  %case RRR of
                  case ht_storage_mongodb:check_order_by_assetid(AssetId) of
                    {ok, {neworder,<<"mapped">>,GeoFencesCol,Requested,Route}} ->
                      initiate_alert_data(AssetId),
                      ht_storage_redis:update_single_field(AssetId,mileage,float_to_binary(UT#track.gps,[{decimals,2}])),
                      ht_hhd_helper_lynktrac:trip_live_n_historical_data(State,<<"mapped">>,GeoFencesCol,UT#track{requested = Requested},AssetId,Requested,Route);
                      %initiate_card_alert(State,AssetId);
                    {ok, {neworder, Status1, GeoFencesCol,Requested,Route}}  ->
%                  ht_logger:info("LIVE DATA INSIDE GEOLOCATION=> unit: ip = '~s' id = '~w'", [ht_inet:resolve(Socket), AssetId]),
                      ht_hhd_helper_lynktrac:trip_live_n_historical_data(State,Status1,GeoFencesCol,UT#track{requested = Requested},AssetId,Requested,Route);
                    Error ->
%                  ht_logger:info("LIVE DATA INSIDE ERROR=> unit: ip = '~s' id = '~w'", [Error, AssetId]),
                      trip_unmapped_data(UT#track{requested = null},AssetId,User),
                      terminate_alert_data(AssetId)
                  end;

                historical_data ->
                  case ht_storage_mongodb:check_order_by_assetid(AssetId) of
                    {ok, {neworder,<<"completed">>,GeoFencesCol,Requested,_}} ->
                      trip_historical_data(GeoFencesCol,UpdatedTrack#track{requested = Requested,packet_type = history,battery_voltage = float_to_binary(bat_per(binary_to_integer(UpdatedTrack#track.battery_voltage)),[{decimals,2}])},User); 
                    {ok, {neworder,_Status,GeoFencesCol,Requested,_}} ->
                      trip_historical_data(GeoFencesCol,UpdatedTrack#track{requested = Requested,packet_type = history,battery_voltage = float_to_binary(bat_per(binary_to_integer(UpdatedTrack#track.battery_voltage)),[{decimals,2}])},User); 
                   _Error ->
                      trip_unmapped_historical_data(UpdatedTrack#track{requested = null,packet_type = history,battery_voltage = float_to_binary(bat_per(binary_to_integer(UpdatedTrack#track.battery_voltage)),[{decimals,2}])},AssetId,User),
                      terminate_alert_data(AssetId)
                  end;
               future_data ->
                 {ok,Packet} = do_encode_command(#command{deviceId = AssetId,type = ?CMD_TIMERESET}),
                 Transport:send(Socket,Packet),
                 ht_logger:info("Future Packets: ~s", [AssetId])
              end;
            {error,Error} ->
              ht_logger:info("Error in Redis Read: ~s", [Error])
          end;
        invalid -> ok
       end;
      true -> 
             ht_logger:info("HHD AES Packet DATE incorrect:~p", [parse_imei(TerminalId)])
      end;
 % em_worker:start(AssetId, Track),
 % do_send_response(State,TerminalId,MsgId,MsgSerialNo);
  <<16#00,16#02>> ->
  do_send_response(State,TerminalId,MsgId,MsgSerialNo);
  <<16#03,16#11>> ->
  ht_logger:info("GOT RESPONSE: SIZE ~w DATA: ~s", [byte_size(MsgBody),parse_hex(MsgBody)]),
   <<ResId:2/binary,Count:8/unsigned-integer,Rest1/binary>> = MsgBody,
   ht_hhd_cmd_aes:cmd_check_response(State,Count,Rest1,AssetId); %% shiva code
  <<16#03,16#15>> ->
  ht_logger:info("GOT RESPONSE: SIZE ~w DATA: ~s", [byte_size(MsgBody),parse_hex(MsgBody)]),
   <<ResId:2/binary,Count:8/unsigned-integer,Rest1/binary>> = MsgBody,
   ht_hhd_cmd_aes:cmd_check_response(State,Count,Rest1,AssetId); %%shiva code
  <<16#03,16#13>> ->
  ht_logger:info("GOT RESPONSE: SIZE ~w DATA: ~s", [byte_size(MsgBody),parse_hex(MsgBody)]),
   <<ResId:2/binary,Count:8/unsigned-integer,Rest2/binary>> = MsgBody,
   ht_hhd_cmd_aes:query_check_response(State,Count,Rest2,AssetId); %% shiva code
   _ ->
    ht_logger:info("HHD AES1 WRONG MSG ID RESPONSE: ID ~p", [parse_imei(TerminalId)])
  end,
  {noreply, State, ?TIMEOUT};
do_process_data( State,Length,Data) ->
  ht_logger:info("UNKNOWN SIZE: ~w DATA: ~w DATA: ~s", [byte_size(Data), Data, Data]),
  {noreply, State, ?TIMEOUT}.

check_bulk_data(State,<<"">>,_) -> ok;
check_bulk_data(State,<<Len:8/unsigned-integer,Rest/binary>>,Track) ->
 case byte_size(Rest) >=  Len of
   true ->
    <<FrameData:Len/binary, NewData/binary>> = Rest,
    NewTrack = parse_bulk_data(State,FrameData,Track),
    check_bulk_data(State,NewData,NewTrack); 
   false ->
    ht_logger:info("HHD BULK REJECT SIZE INCURRECT: DATA: ~s", [parse_hex(Rest)])
  end.
  

parse_bulk_data(State = #state{socket = Socket,assetId=AssetId,protocol=Protocol,user=User,transport=Transport},<<AlertId:4/binary,Status:4/binary,Latitude:4/binary,Longitude:4/binary,Altitude:16/unsigned-integer,Speed:16/unsigned-integer,_:16/unsigned-integer,Year:1/binary,Month:1/binary,Day:1/binary,Hour:1/binary,Minute:1/binary,Second:1/binary,Rest/binary>> = Data,NewTrack) ->
    Track = NewTrack#track{
       asset_id = AssetId,
       device_name = atom_to_binary(Protocol, utf8),
       socket_ip = ht_inet:resolve(Socket),
        packet_time = parse_date(Year,Month,Day,Hour,Minute,Second),
        latitude = parse_latitude(Latitude,Status),
        longitude = parse_longitude(Longitude,Status),
        altitude = Altitude,
        speed = Speed/10,
        packet_type = live,
        sim_slot = parse_sim_status(AlertId),
        hall = parse_hall_status(AlertId),
        travel_swt = parse_travel_status(AlertId),
        valid = parse_status(Status),
        charge_status = parse_charging(Status),
        accuracy = parse_accuracy(AlertId),
        satellite_count = parse_satellite(Status)

      },

  UpdatedTrack = decode(State,Track,Rest),
   case UpdatedTrack#track.packet_time == {error,badarg} of
      false ->


 % ht_logger:info("BULK Frist save message => unit: ip = '~s' id = '~w' imei position: ~w", [ht_inet:resolve(Socket), AssetId, Track]),
      case check(UpdatedTrack#track.latitude,UpdatedTrack#track.longitude) of
        valid ->
          case ht_storage_redis:get_previous_packet_time(AssetId) of
           {packet,[OldPacket]} ->
	      CurrentTime = em_helper_time:timestamp() + 30,
              case check_packet_state(UpdatedTrack#track.packet_time,OldPacket,CurrentTime) of

              live_data ->
                    UT = ht_hhd_helper_lynktrac:hhd_event_adder(State,UpdatedTrack),

                  %ht_logger:info("BULK FIRST LIVE DATA => unit: ip = '~s' id = '~w' position: ~w", [ht_inet:resolve(Socket), AssetId, Track]),
                  %RRR = ht_storage_mongodb:check_order_by_assetid(AssetId),
                  % ht_logger:info("LIVE DATA => unit: ip = '~s' id = '~w' position: ~w", [ AssetId, RRR]),                          
                  %case RRR of
                  case ht_storage_mongodb:check_order_by_assetid(AssetId) of
                    {ok, {neworder,<<"mapped">>,GeoFencesCol,Requested,Route}} ->
                      initiate_alert_data(AssetId),
                      ht_storage_redis:update_single_field(AssetId,mileage,float_to_binary(UT#track.gps,[{decimals,2}])),
                      ht_hhd_helper_lynktrac:trip_live_n_historical_data(State,<<"mapped">>,GeoFencesCol,UT#track{requested = Requested},AssetId,Requested,Route);
                      %initiate_card_alert(State,AssetId);
                    {ok, {neworder, Status1, GeoFencesCol,Requested,Route}}  ->
                    %ht_logger:info("BULK FIRST LIVE DATA INSIDE GEOLOCATION=> unit: ip = '~s' id = '~w'", [ht_inet:resolve(Socket), AssetId]),
                      ht_hhd_helper_lynktrac:trip_live_n_historical_data(State,Status1,GeoFencesCol,UT#track{requested = Requested},AssetId,Requested,Route);
                    Error ->
                  %ht_logger:info("BULK FIRST LIVE DATA INSIDE ERROR=> unit: ip = '~s' id = '~w'", [Error, AssetId]),
                      trip_unmapped_data(UT#track{requested = null},AssetId,User),
                      terminate_alert_data(AssetId)
                  end;

                historical_data ->
                  case ht_storage_mongodb:check_order_by_assetid(AssetId) of
                    {ok, {neworder,<<"completed">>,GeoFencesCol,Requested,_}} -> 
                      trip_historical_data(GeoFencesCol,UpdatedTrack#track{requested = Requested,packet_type = history,battery_voltage = float_to_binary(bat_per(binary_to_integer(UpdatedTrack#track.battery_voltage)),[{decimals,2}])},User); 
                    {ok, {neworder,_Status,GeoFencesCol,Requested,_}} ->
                      trip_historical_data(GeoFencesCol,UpdatedTrack#track{requested = Requested,packet_type = history,battery_voltage = float_to_binary(bat_per(binary_to_integer(UpdatedTrack#track.battery_voltage)),[{decimals,2}])},User); 

                    _Error ->
                      trip_unmapped_historical_data(UpdatedTrack#track{requested = null,packet_type = history,battery_voltage = float_to_binary(bat_per(binary_to_integer(UpdatedTrack#track.battery_voltage)),[{decimals,2}])},AssetId,User),
                      terminate_alert_data(AssetId)
                  end;
               future_data ->
                 {ok,Packet} = do_encode_command(#command{deviceId = AssetId,type = ?CMD_TIMERESET}),
                 Transport:send(Socket,Packet),
                 ht_logger:info("Future Packets: ~s", [AssetId])
              end;
            {error,Error} ->
              ht_logger:info("Error in Redis Read: ~s", [Error])
          end;
        invalid -> ok
      end;
     true -> 
             ht_logger:info("HHD AES Packet DATE incorrect:~p", [AssetId])
     end, 
  UpdatedTrack;
parse_bulk_data(State = #state{socket = Socket,assetId=AssetId,protocol=Protocol,user=User,transport=Transport},<<AlertId:4/binary,Status:4/binary,Latitude:4/binary,Longitude:4/binary,Altitude:16/unsigned-integer,Speed:16/unsigned-integer,_:16/unsigned-integer,Year:1/binary,Month:1/binary,Day:1/binary,Hour:1/binary,Minute:1/binary,Second:1/binary>> = Data,NewTrack) ->
 UpdatedTrack = NewTrack#track{
       asset_id = AssetId,
       device_name = atom_to_binary(Protocol, utf8),
       socket_ip = ht_inet:resolve(Socket),
        packet_time = parse_date(Year,Month,Day,Hour,Minute,Second),
        latitude = parse_latitude(Latitude,Status),
        longitude = parse_longitude(Longitude,Status),
        altitude = Altitude,
        speed = Speed/10,
        packet_type = live,
        sim_slot = parse_sim_status(AlertId),
        hall = parse_hall_status(AlertId),
        travel_swt = parse_travel_status(AlertId),
        valid = parse_status(Status),
        charge_status = parse_charging(Status),
        accuracy = parse_accuracy(AlertId),
        satellite_count = parse_satellite(Status)
      },
  %ht_logger:info("bulk save message => unit: ip = '~s' id = '~w' imei position: ~w", [ht_inet:resolve(Socket), AssetId, UpdatedTrack]),
   case UpdatedTrack#track.packet_time == {error,badarg} of
      false ->

      %ht_logger:info("save LIVE DATA => unit: ip = '~s' id = '~w' position: ~w", [ht_inet:resolve(Socket), AssetId, Track]),
      case check(UpdatedTrack#track.latitude,UpdatedTrack#track.longitude) of
        valid ->
        % ht_logger:info("BULK VALID => unit: ip = '~s' id = '~w' position: ~w", [ht_inet:resolve(Socket), AssetId, UpdatedTrack]),
          case ht_storage_redis:get_previous_packet_time(AssetId) of
           {packet,[OldPacket]} ->
	      CurrentTime = em_helper_time:timestamp() + 30,
              case check_packet_state(UpdatedTrack#track.packet_time,OldPacket,CurrentTime) of

              live_data ->
                    UT = ht_hhd_helper_lynktrac:hhd_event_adder(State,UpdatedTrack),

                  case ht_storage_mongodb:check_order_by_assetid(AssetId) of
                    {ok, {neworder,<<"mapped">>,GeoFencesCol,Requested,Route}} ->
                      initiate_alert_data(AssetId),
                      ht_storage_redis:update_single_field(AssetId,mileage,float_to_binary(UT#track.gps,[{decimals,2}])),
                      ht_hhd_helper_lynktrac:trip_live_n_historical_data(State,<<"mapped">>,GeoFencesCol,UT#track{requested = Requested},AssetId,Requested,Route);
                      %initiate_card_alert(State,AssetId);
                    {ok, {neworder, Status1, GeoFencesCol,Requested,Route}}  ->
                      %ht_logger:info("BULK LIVE DATA INSIDE GEOLOCATION=> unit: ip = '~s' id = '~w'", [ht_inet:resolve(Socket), AssetId]),
                      ht_hhd_helper_lynktrac:trip_live_n_historical_data(State,Status1,GeoFencesCol,UT#track{requested = Requested},AssetId,Requested,Route);
                    Error ->
                      %ht_logger:info("BULK LIVE DATA INSIDE ERROR=> unit: ip = '~s' id = '~w'", [Error, AssetId]),
                      trip_unmapped_data(UT#track{requested = null},AssetId,User),
                      terminate_alert_data(AssetId)
                  end;

                historical_data ->
                  case ht_storage_mongodb:check_order_by_assetid(AssetId) of
                    {ok, {neworder,<<"completed">>,GeoFencesCol,Requested,_}} -> 
                      trip_historical_data(GeoFencesCol,UpdatedTrack#track{requested = Requested,packet_type = history,battery_voltage = float_to_binary(bat_per(binary_to_integer(UpdatedTrack#track.battery_voltage)),[{decimals,2}])},User); 
                    {ok, {neworder,_Status,GeoFencesCol,Requested,_}} ->
                      trip_historical_data(GeoFencesCol,UpdatedTrack#track{requested = Requested,packet_type = history,battery_voltage = float_to_binary(bat_per(binary_to_integer(UpdatedTrack#track.battery_voltage)),[{decimals,2}])},User); 

                    _Error ->
                      trip_unmapped_historical_data(UpdatedTrack#track{requested = null,packet_type = history,battery_voltage = float_to_binary(bat_per(binary_to_integer(UpdatedTrack#track.battery_voltage)),[{decimals,2}])},AssetId,User),
                      terminate_alert_data(AssetId)
                  end;
               future_data ->
                 {ok,Packet} = do_encode_command(#command{deviceId = AssetId,type = ?CMD_TIMERESET}),
                 Transport:send(Socket,Packet),
                 ht_logger:info("Future Packets: ~s", [AssetId])
              end;
            {error,Error} ->
             ht_logger:info("Error in Redis Read: ~s", [Error])
          end;
        invalid -> ok
      end;
     true -> 
       ht_logger:info("HHD AES Packet DATE incorrect:~p,  ~p", [AssetId,parse_hex(Data)])
     end,
      UpdatedTrack;
parse_bulk_data(_,_,_) ->ok.

%%%===================================================================
%%% Internal functions - Alert
%%%===================================================================
check_response(_State,0,_Rest,_AssetId) -> ok;
 
check_response(State,Count, <<Cmd:1/binary,Response:1/binary,Res/binary>>, AssetId) ->
  case Cmd of
   <<16#24>> ->
      case Response of
        <<16#00>> ->
          ht_storage_redis:update_single_field(AssetId,lock_resend,0),
          ht_storage_redis:update_single_field(AssetId,course,"WEB"), 
          ht_storage_mongodb:update_unlock_status(AssetId,Response);
         _ ->
         case ht_storage_redis:get_single_field(AssetId,lock_resend) of
          {ok,<<"0">>} ->
          ht_storage_redis:update_single_field(AssetId,lock_resend,1), 
          send_lock_cmd(State);
          {ok,<<"1">>} ->
          ht_storage_redis:update_single_field(AssetId,lock_resend,2),
          send_lock_cmd(State);
          {ok,<<"2">>} ->
          ht_storage_redis:update_single_field(AssetId,lock_resend,3),
          send_lock_cmd(State);
          _ ->
          ht_storage_redis:update_single_field(AssetId,lock_resend,0)
          end
        end;  
    _ -> ok
   end,
  check_response(State,Count-1,Res,AssetId).

initiate_alert_data(AssetId) ->
  Key = list_to_atom("alert_" ++ integer_to_list(AssetId)),
  case ht_storage_redis:check_alert_info(Key) of
    no_data_found ->
      ht_storage_redis:create_alert_info(Key, #alert_info{});
    true ->
      already_initiated
  end.

terminate_alert_data(AssetId) ->
  Key = list_to_atom("alert_" ++ integer_to_list(AssetId)),
  ht_storage_redis:delete_alert_info(Key).

%%%===================================================================

%check_packet_state(NewPacket,OldPacket) when NewPacket >= OldPacket -> live_data;
%check_packet_state(NewPacket,OldPacket) when NewPacket < OldPacket -> historical_data.
check_packet_state(NewPacket,_,CurrentTime) when NewPacket >= CurrentTime -> future_data;
check_packet_state(NewPacket,OldPacket,CurrentTime) when NewPacket >= OldPacket -> live_data;
check_packet_state(_,_,_)  -> historical_data.

%%%===================================================================
%%% Internal functions - Idle Checker
%%%===================================================================
check_idle_case(Speed,AssetId,TrackModel) when Speed < 3.0 ->
  case ht_storage_redis:get_idle_data(AssetId) of
    {idle_state,[Latitude,Longitude,IdleTime,PacketTime]} ->
      check_distance(TrackModel,IdleTime,PacketTime,Latitude,Longitude);
    {moving_state,[Latitude,Longitude,MovingTime,PacketTime]} ->
      TrackModel#track{idle_state = true,idle_time = (TrackModel#track.packet_time - PacketTime)}
  end;

check_idle_case(0,AssetId,TrackModel) ->
  case ht_storage_redis:get_idle_data(AssetId) of
    {idle_state,[Latitude,Longitude,IdleTime,PacketTime]} ->
      check_distance(TrackModel,IdleTime,PacketTime,Latitude,Longitude);
    {moving_state,[Latitude,Longitude,MovingTime,PacketTime]} ->
      TrackModel#track{idle_state = true,idle_time = (TrackModel#track.packet_time - PacketTime)}
  end;
check_idle_case(Speed,AssetId,TrackModel) ->
  case ht_storage_redis:get_idle_data(AssetId) of
    {idle_state,[Latitude,Longitude,MovingTime,PacketTime]} ->
 %     case TrackModel#track.latitude == Latitude andalso TrackModel#track.longitude == Longitude of
 %        true ->
 %          TrackModel#track{idle_state = true,speed = 0, idle_time = MovingTime + (TrackModel#track.packet_time - PacketTime)};
 %        false ->
           TrackModel#track{idle_state = false,idle_time = (TrackModel#track.packet_time - PacketTime)};
 %        end;
    {moving_state,[Latitude,Longitude,MovingTime,PacketTime]} ->
 %     case TrackModel#track.latitude == Latitude andalso TrackModel#track.longitude == Longitude of
 %        true ->
 %          TrackModel#track{idle_state = true,speed = 0, idle_time = TrackModel#track.packet_time - PacketTime};
 %        false ->
           TrackModel#track{idle_state = false,idle_time = (TrackModel#track.packet_time - PacketTime) + MovingTime}
 %        end
  end.


%%%===================================================================
%%% Internal functions - Checking Distance Between Idle State
%%%===================================================================

check_distance(Track = #track{latitude = Latitude1, longitude = Longitude1, packet_time = PacketTime2},
    IdleTime,PacketTime1,Latitude2,Longitude2) ->
  Deg2rad = fun(Deg) -> math:pi()*Deg/180 end,
  [RLng1, RLat1, RLng2, RLat2] = [Deg2rad(Deg) || Deg <- [Longitude1, Latitude1, Longitude2, Latitude2]],
  DLon = RLng2 - RLng1,
  DLat = RLat2 - RLat1,
  A = math:pow(math:sin(DLat/2), 2) + math:cos(RLat1) * math:cos(RLat2) * math:pow(math:sin(DLon/2), 2),
  C = 2 * math:asin(math:sqrt(A)),
  Km = 6372.8 * C,
  case validate_distance(Km) of
    new_idle ->
      Track#track{idle_state = true,idle_time = 0};
    same_idle ->
      [NewIdleTime] = [(PacketTime2 - PacketTime1) + IdleTime],
      Track#track{latitude = Latitude2,longitude = Longitude2,speed = 0,
        packet_time = PacketTime2,idle_state = true,idle_time = NewIdleTime}
  end.

validate_distance(Distance) when  Distance > 0.033 -> new_idle;
validate_distance(Distance) when  Distance =< 0.033 -> same_idle.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%
%%%%   P45 related Internal function
%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
trip_live_n_historical_data(State = #state{user = User},<<"mapped">>,GeoFencesCol,UpdatedTrack = #track{lock_status = unlocked,unlock_report = RFID},AssetId,false) ->
  case ht_storage_mongodb:check_geo_by_id(GeoFencesCol, UpdatedTrack, User) of
    {ok, {fence_combo, <<"source">>, GeoName}} ->
      NewTrackDetails = UpdatedTrack#track{geo_type = <<"source">>,geo_name = GeoName,alarm_report = <<"Device Tampered">>},
      %ht_logger:info("In Transit unlock RFID number: ~p~n",[RFID]),
      case ht_storage_mongodb:check_rfid(RFID) of
        {ok, {user, FName}} ->
          %ht_logger:info("In Transit User Name:~p~n",[FName]),
          ht_storage_mongodb:update_order(AssetId,FName,false_mapped_unlock,NewTrackDetails#track.packet_time,RFID),
          ht_storage_mongodb:create_track_history(NewTrackDetails),
          ht_storage_redis:create_live_data(AssetId, NewTrackDetails),
          %set_time_interval(State,<<"30">>,<<"1440">>),
          ht_device_alert_handler:initiate(hhd,AssetId,NewTrackDetails);
        {error,<<"Not find item">>} ->
          ht_storage_mongodb:update_order(AssetId,<<"unknown">>,false_mapped_unlock,NewTrackDetails#track.packet_time,RFID),
          ht_storage_mongodb:create_track_history(NewTrackDetails),
          ht_storage_redis:create_live_data(AssetId, NewTrackDetails),
          %set_time_interval(State,<<"30">>,<<"1440">>),
          ht_device_alert_handler:initiate(hhd,AssetId,NewTrackDetails)
      end;
    {error,<<"Not find item">>} ->
      NewTrackDetails = UpdatedTrack#track{geo_type = atom_to_binary(nil, utf8),geo_name = atom_to_binary(nil, utf8),alarm_type = atom_to_binary(out_of_destination, utf8),alarm_report = <<"Device Tampered">>},
      case ht_storage_mongodb:check_rfid(RFID) of
      {ok, {user, FName}} ->
      ht_storage_mongodb:update_order(AssetId,mapped,NewTrackDetails#track.packet_time),
      ht_storage_mongodb:update_order(AssetId,FName,false_outside,NewTrackDetails#track.packet_time,RFID),
      ht_storage_mongodb:create_track_history(NewTrackDetails),
      ht_storage_redis:create_live_data(AssetId, NewTrackDetails),
      ht_device_alert_handler:initiate(hhd,AssetId,NewTrackDetails);
      {error,<<"Not find item">>} ->
      ht_storage_mongodb:update_order(AssetId,mapped,NewTrackDetails#track.packet_time),
      ht_storage_mongodb:update_order(AssetId,<<"unknown">>,false_outside,NewTrackDetails#track.packet_time,RFID),
      ht_storage_mongodb:create_track_history(NewTrackDetails),
      ht_storage_redis:create_live_data(AssetId, NewTrackDetails),
      ht_device_alert_handler:initiate(hhd,AssetId,NewTrackDetails)
      end;
    Error ->
      ht_logger:info("NoMatch Scenario at unlocked Mapped: ~s~n",[Error])
  end;

trip_live_n_historical_data(State = #state{user = User},<<"mapped">>,GeoFencesCol,UpdatedTrack = #track{lock_status = unlocked,unlock_report = RFID},AssetId,true) ->
  case ht_storage_mongodb:check_geo_by_id(GeoFencesCol, UpdatedTrack, User) of
    {ok, {fence_combo, <<"source">>, GeoName}} ->
      NewTrackDetails = UpdatedTrack#track{geo_type = <<"source">>,geo_name = GeoName,alarm_report = <<"Device Tampered">>},
      %ht_logger:info("In Transit unlock RFID number: ~p~n",[RFID]),
      case ht_storage_mongodb:check_rfid(RFID) of
        {ok, {user, FName}} ->
          %ht_logger:info("In Transit User Name:~p~n",[FName]),
          ht_storage_mongodb:update_order(AssetId,FName,mapped_unlock,NewTrackDetails#track.packet_time,RFID),
          ht_storage_mongodb:create_track_history(NewTrackDetails),
          ht_storage_redis:create_live_data(AssetId, NewTrackDetails),
          %set_time_interval(State,<<"30">>,<<"1440">>),
          ht_device_alert_handler:initiate(hhd,AssetId,NewTrackDetails);
        {error,<<"Not find item">>} ->
          ht_storage_mongodb:update_order(AssetId,<<"unknown">>,mapped_unlock,NewTrackDetails#track.packet_time,RFID),
          ht_storage_mongodb:create_track_history(NewTrackDetails),
          ht_storage_redis:create_live_data(AssetId, NewTrackDetails),
          %set_time_interval(State,<<"30">>,<<"1440">>),
          ht_device_alert_handler:initiate(hhd,AssetId,NewTrackDetails)
      end;
    {error,<<"Not find item">>} ->
      NewTrackDetails = UpdatedTrack#track{geo_type = atom_to_binary(nil, utf8),geo_name = atom_to_binary(nil, utf8),alarm_report = <<"Device Tampered">>},
      case ht_storage_mongodb:check_rfid(RFID) of
      {ok, {user, FName}} ->
      ht_storage_mongodb:update_order(AssetId,FName,outside,NewTrackDetails#track.packet_time,RFID),
      ht_storage_mongodb:create_track_history(NewTrackDetails),
      ht_storage_redis:create_live_data(AssetId, NewTrackDetails),
      ht_device_alert_handler:initiate(hhd,AssetId,NewTrackDetails);
      {error,<<"Not find item">>} ->
      ht_storage_mongodb:update_order(AssetId,<<"unknown">>,outside,NewTrackDetails#track.packet_time,RFID),
      ht_storage_mongodb:create_track_history(NewTrackDetails),
      ht_storage_redis:create_live_data(AssetId, NewTrackDetails),
      ht_device_alert_handler:initiate(hhd,AssetId,NewTrackDetails)
      end;
    Error ->
      ht_logger:info("NoMatch Scenario at unlocked Mapped: ~s~n",[Error])
  end;


trip_live_n_historical_data(State = #state{user = User},<<"mapped">>,GeoFencesCol,UpdatedTrack = #track{lock_status = locked},AssetId,_Requested) ->
  case ht_storage_mongodb:check_geo_by_id(GeoFencesCol, UpdatedTrack, User) of
    {ok, {fence_combo, <<"source">>, GeoName}} ->
      NewTrackDetails = UpdatedTrack#track{geo_type = <<"source">>,geo_name = GeoName},
      %ht_logger:info("In Side Trip live and history => id = '~w' position: ~w", [ AssetId, NewTrackDetails]),
      ht_storage_mongodb:update_order(AssetId,device_locked,NewTrackDetails#track.packet_time),
      ht_storage_mongodb:create_track_history(NewTrackDetails),
      ht_storage_redis:create_live_data(AssetId, NewTrackDetails),
      %initiate_time_alert(State,AssetId,<<"15">>),
      %set_time_interval(State,<<"600">>,<<"120">>),
      ht_device_alert_handler:initiate(hhd,AssetId,NewTrackDetails);
    {ok, {fence_combo,GeoType, GeoName}} ->
      NewTrackDetails = UpdatedTrack#track{geo_type = GeoType,geo_name = GeoName},
      %ht_logger:info("In Side Trip live and history => id = '~w' position: ~w", [ AssetId, NewTrackDetails]),
      ht_storage_mongodb:create_track_history(NewTrackDetails),
      ht_storage_redis:create_live_data(AssetId, NewTrackDetails),
      %initiate_time_alert(State,AssetId,<<"15">>),
      %set_time_interval(State,<<"600">>,<<"120">>),
      ht_device_alert_handler:initiate(hhd,AssetId,NewTrackDetails);
    {error,<<"Not find item">>} ->
      NewTrackDetails = UpdatedTrack#track{geo_type = atom_to_binary(nil, utf8),geo_name = atom_to_binary(nil, utf8)},
      ht_storage_mongodb:update_order(AssetId,mapped,NewTrackDetails#track.packet_time),
      ht_storage_mongodb:create_track_history(NewTrackDetails),
      ht_storage_redis:create_live_data(AssetId, NewTrackDetails),
      ht_device_alert_handler:initiate(hhd,AssetId,NewTrackDetails);
    Error ->
      ht_logger:info("NoMatch Scenario at locked mapped : ~s~n",[Error])
  end;

trip_live_n_historical_data(State = #state{user = User},<<"mapped">>,GeoFencesCol,UpdatedTrack,AssetId,_Requested) ->
  case ht_storage_mongodb:check_geo_by_id(GeoFencesCol, UpdatedTrack, User) of
    {ok, {fence_combo, <<"source">>, GeoName}} ->
      NewTrackDetails = UpdatedTrack#track{geo_type = <<"source">>,geo_name = GeoName},
      ht_storage_mongodb:create_track_history(NewTrackDetails),
      ht_storage_redis:create_live_data(AssetId, NewTrackDetails),
      %initiate_time_alert(State,AssetId,<<"15">>),
      %set_time_interval(State,<<"600">>,<<"120">>),
      ht_device_alert_handler:initiate(hhd,AssetId,NewTrackDetails);
    {error,<<"Not find item">>} ->
      NewTrackDetails = UpdatedTrack#track{geo_type = atom_to_binary(nil, utf8),geo_name = atom_to_binary(nil, utf8)},
      %ht_logger:info("Out Side Trip live and history => id = '~w' position: ~w", [ AssetId, NewTrackDetails]),
      ht_storage_mongodb:update_order(AssetId,mapped,NewTrackDetails#track.packet_time),
      ht_storage_mongodb:create_track_history(NewTrackDetails),
      ht_storage_redis:create_live_data(AssetId, NewTrackDetails),
      ht_device_alert_handler:initiate(hhd,AssetId,NewTrackDetails);
    Error ->
      ht_logger:info("NoMatch Scenario at rest of mapped : ~s~n",[Error])
  end;
trip_live_n_historical_data(State = #state{user = User},<<"in_transit">>,GeoFencesCol,UpdatedTrack = #track{alarm_type = tampered},AssetId,_) ->
  case ht_storage_mongodb:check_geo_by_id(GeoFencesCol, UpdatedTrack, User) of
    {ok, {fence_combo, <<"destination">>, GeoName}} ->
      NewTrackDetails = UpdatedTrack#track{geo_type = <<"destination">>,geo_name = GeoName,alarm_report = <<"Device Tampered">>},
      ht_storage_mongodb:update_order(AssetId,wirecut),
      ht_storage_mongodb:create_track_history(NewTrackDetails),
      ht_storage_redis:create_live_data(AssetId, NewTrackDetails),
      ht_device_alert_handler:initiate(hhd,AssetId,NewTrackDetails);
    {ok, {fence_combo, <<"source">>, GeoName}} ->
      NewTrackDetails = UpdatedTrack#track{geo_type = <<"source">>,geo_name = GeoName,alarm_report = <<"Device Tampered">>},
      ht_storage_mongodb:update_order(AssetId,wirecut),
      ht_storage_mongodb:create_track_history(NewTrackDetails),
      ht_storage_redis:create_live_data(AssetId, NewTrackDetails),
      ht_device_alert_handler:initiate(hhd,AssetId,NewTrackDetails);
    {error,<<"Not find item">>} ->
      NewTrackDetails = UpdatedTrack#track{geo_type = atom_to_binary(nil, utf8),geo_name = atom_to_binary(nil, utf8),alarm_type = atom_to_binary(out_of_destination, utf8),alarm_report = <<"Device Tampered">>},
      ht_storage_mongodb:update_order(AssetId,wirecut),
      ht_storage_mongodb:create_track_history(NewTrackDetails),
      ht_storage_redis:create_live_data(AssetId, NewTrackDetails),
      ht_device_alert_handler:initiate(hhd,AssetId,NewTrackDetails)
   end;

trip_live_n_historical_data(State = #state{user = User},<<"in_transit">>,GeoFencesCol,UpdatedTrack = #track{lock_status = unlocked,unlock_report = RFID},AssetId,false) ->
  case ht_storage_mongodb:check_geo_by_id(GeoFencesCol, UpdatedTrack, User) of
    {ok, {fence_combo, <<"destination">>, GeoName}} ->
      NewTrackDetails = UpdatedTrack#track{geo_type = <<"destination">>,geo_name = GeoName,alarm_report = <<"Device Tampered">>},
      %ht_logger:info("In Transit unlock RFID number: ~p~n",[RFID]),
      case ht_storage_mongodb:check_rfid(RFID) of
        {ok, {user, FName}} ->
          %ht_logger:info("In Transit User Name:~p~n",[FName]),
          ht_storage_mongodb:update_order(AssetId,in_transit,NewTrackDetails#track.packet_time),
          ht_storage_mongodb:update_order(AssetId,FName,false_outside,NewTrackDetails#track.packet_time,RFID),
          %ht_storage_mongodb:update_order(AssetId,mapped_unlock,NewTrackDetails#track.packet_time),
          ht_storage_mongodb:create_track_history(NewTrackDetails),
          ht_storage_redis:create_live_data(AssetId, NewTrackDetails),
          %set_time_interval(State,<<"30">>,<<"1440">>),
          ht_device_alert_handler:initiate(hhd,AssetId,NewTrackDetails);
        {error,<<"Not find item">>} ->
          ht_storage_mongodb:update_order(AssetId,in_transit,NewTrackDetails#track.packet_time),
          ht_storage_mongodb:update_order(AssetId,<<"unknown">>,false_outside,NewTrackDetails#track.packet_time,RFID),
          %ht_storage_mongodb:update_order(AssetId,in_transit,NewTrackDetails#track.packet_time),
          %ht_storage_mongodb:update_order(AssetId,<<"unknown">>,reached,NewTrackDetails#track.packet_time,RFID),
          ht_storage_mongodb:create_track_history(NewTrackDetails),
          ht_storage_redis:create_live_data(AssetId, NewTrackDetails),
          %set_time_interval(State,<<"30">>,<<"1440">>),
          ht_device_alert_handler:initiate(hhd,AssetId,NewTrackDetails)
      end;

    {ok, {fence_combo, <<"source">>, GeoName}} ->
      NewTrackDetails = UpdatedTrack#track{geo_type = <<"source">>,geo_name = GeoName,alarm_report = <<"Device Tampered">>},
      case ht_storage_mongodb:check_rfid(RFID) of
      {ok, {user, FName}} ->
      ht_storage_mongodb:update_order(AssetId,FName,false_mapped_unlock,NewTrackDetails#track.packet_time,RFID),
      ht_storage_mongodb:create_track_history(NewTrackDetails),
      ht_storage_redis:create_live_data(AssetId, NewTrackDetails),
      ht_device_alert_handler:initiate(hhd,AssetId,NewTrackDetails);
      {error,<<"Not find item">>} ->
      ht_storage_mongodb:update_order(AssetId,<<"unknown">>,false_mapped_unlock,NewTrackDetails#track.packet_time,RFID),
      ht_storage_mongodb:create_track_history(NewTrackDetails),
      ht_storage_redis:create_live_data(AssetId, NewTrackDetails),
      ht_device_alert_handler:initiate(hhd,AssetId,NewTrackDetails)
      end;
    {ok, {fence_combo, <<"viapoints">>, GeoName}} ->
      NewTrackDetails = UpdatedTrack#track{geo_type = <<"viapoints">>,geo_name = GeoName,alarm_report = <<"Device Tampered">>},
      case ht_storage_mongodb:check_rfid(RFID) of
      {ok, {user, FName}} ->
      %ht_storage_mongodb:update_order(AssetId,FName,false_mapped_unlock,NewTrackDetails#track.packet_time,RFID),
      ht_storage_mongodb:create_track_history(NewTrackDetails),
      ht_storage_redis:create_live_data(AssetId, NewTrackDetails),
      ht_device_alert_handler:initiate(hhd,AssetId,NewTrackDetails);
      {error,<<"Not find item">>} ->
      %ht_storage_mongodb:update_order(AssetId,<<"unknown">>,false_mapped_unlock,NewTrackDetails#track.packet_time,RFID),
      ht_storage_mongodb:create_track_history(NewTrackDetails),
      ht_storage_redis:create_live_data(AssetId, NewTrackDetails),
      ht_device_alert_handler:initiate(hhd,AssetId,NewTrackDetails)
      end;
    {error,<<"Not find item">>} ->
      NewTrackDetails = UpdatedTrack#track{geo_type = atom_to_binary(nil, utf8),geo_name = atom_to_binary(nil, utf8),alarm_type = atom_to_binary(out_of_destination, utf8),alarm_report = <<"Device Tampered">>},
      case ht_storage_mongodb:check_rfid(RFID) of
      {ok, {user, FName}} ->
      ht_storage_mongodb:update_order(AssetId,FName,false_outside,NewTrackDetails#track.packet_time,RFID),
      ht_storage_mongodb:create_track_history(NewTrackDetails),
      ht_storage_redis:create_live_data(AssetId, NewTrackDetails),
      ht_device_alert_handler:initiate(hhd,AssetId,NewTrackDetails);
      {error,<<"Not find item">>} ->
      ht_storage_mongodb:update_order(AssetId,<<"unknown">>,false_outside,NewTrackDetails#track.packet_time,RFID),
      ht_storage_mongodb:create_track_history(NewTrackDetails),
      ht_storage_redis:create_live_data(AssetId, NewTrackDetails),
      ht_device_alert_handler:initiate(hhd,AssetId,NewTrackDetails)
      end;
    Error ->
      ht_logger:info("NoMatch Scenario at unlocked in transit : ~s~n",[Error])
  end;

trip_live_n_historical_data(State = #state{user = User},<<"in_transit">>,GeoFencesCol,UpdatedTrack = #track{lock_status = unlocked,unlock_report = RFID},AssetId,true) ->
  case ht_storage_mongodb:check_geo_by_id(GeoFencesCol, UpdatedTrack, User) of
    {ok, {fence_combo, <<"destination">>, GeoName}} ->
      NewTrackDetails = UpdatedTrack#track{geo_type = <<"destination">>,geo_name = GeoName},
      %ht_logger:info("In Transit unlock RFID number: ~p~n",[RFID]),
      case ht_storage_mongodb:check_rfid(RFID) of
        {ok, {user, FName}} ->
          %ht_logger:info("In Transit User Name:~p~n",[FName]),
          ht_storage_mongodb:update_order(AssetId,in_transit,NewTrackDetails#track.packet_time),
          ht_storage_mongodb:update_order(AssetId,FName,reached,NewTrackDetails#track.packet_time,RFID),
          ht_storage_mongodb:create_track_history(NewTrackDetails),
          ht_storage_redis:create_live_data(AssetId, NewTrackDetails),
          %set_time_interval(State,<<"30">>,<<"1440">>),
          ht_device_alert_handler:initiate(hhd,AssetId,NewTrackDetails);
        {error,<<"Not find item">>} ->
          ht_storage_mongodb:update_order(AssetId,in_transit,NewTrackDetails#track.packet_time),
          ht_storage_mongodb:update_order(AssetId,<<"unknown">>,reached,NewTrackDetails#track.packet_time,RFID),
          ht_storage_mongodb:create_track_history(NewTrackDetails),
          ht_storage_redis:create_live_data(AssetId, NewTrackDetails),
          %set_time_interval(State,<<"30">>,<<"1440">>),
          ht_device_alert_handler:initiate(hhd,AssetId,NewTrackDetails)
      end;

    {ok, {fence_combo, <<"source">>, GeoName}} ->
      NewTrackDetails = UpdatedTrack#track{geo_type = <<"source">>,geo_name = GeoName,alarm_report = <<"Device Tampered">>},
      case ht_storage_mongodb:check_rfid(RFID) of
      {ok, {user, FName}} ->
      ht_storage_mongodb:update_order(AssetId,FName,mapped_unlock,NewTrackDetails#track.packet_time,RFID),
      ht_storage_mongodb:create_track_history(NewTrackDetails),
      ht_storage_redis:create_live_data(AssetId, NewTrackDetails),
      ht_device_alert_handler:initiate(hhd,AssetId,NewTrackDetails);
      {error,<<"Not find item">>} ->
        ht_storage_mongodb:update_order(AssetId,<<"unknown">>,mapped_unlock,NewTrackDetails#track.packet_time,RFID),
        ht_storage_mongodb:create_track_history(NewTrackDetails),
        ht_storage_redis:create_live_data(AssetId, NewTrackDetails),
        ht_device_alert_handler:initiate(hhd,AssetId,NewTrackDetails)
      end;
    {ok, {fence_combo, <<"viapoints">>, GeoName}} ->
      NewTrackDetails = UpdatedTrack#track{geo_type = <<"viapoints">>,geo_name = GeoName},
      case ht_storage_mongodb:check_rfid(RFID) of
      {ok, {user, FName}} ->
      %ht_storage_mongodb:update_order(AssetId,FName,mapped_unlock,NewTrackDetails#track.packet_time,RFID),
        ht_storage_mongodb:create_track_history(NewTrackDetails),
        ht_storage_redis:create_live_data(AssetId, NewTrackDetails),
        ht_device_alert_handler:initiate(hhd,AssetId,NewTrackDetails);
      {error,<<"Not find item">>} ->
      %ht_storage_mongodb:update_order(AssetId,<<"unknown">>,mapped_unlock,NewTrackDetails#track.packet_time,RFID),
        ht_storage_mongodb:create_track_history(NewTrackDetails),
        ht_storage_redis:create_live_data(AssetId, NewTrackDetails),
        ht_device_alert_handler:initiate(hhd,AssetId,NewTrackDetails)
      end;
    {error,<<"Not find item">>} ->
      NewTrackDetails = UpdatedTrack#track{geo_type = atom_to_binary(nil, utf8),geo_name = atom_to_binary(nil, utf8),alarm_type = atom_to_binary(out_of_destination, utf8),alarm_report = <<"Device Tampered">>},
      case ht_storage_mongodb:check_rfid(RFID) of
      {ok, {user, FName}} ->
      ht_storage_mongodb:update_order(AssetId,FName,outside,NewTrackDetails#track.packet_time,RFID),
      ht_storage_mongodb:create_track_history(NewTrackDetails),
      ht_storage_redis:create_live_data(AssetId, NewTrackDetails),
      ht_device_alert_handler:initiate(hhd,AssetId,NewTrackDetails);
      {error,<<"Not find item">>} ->
      ht_storage_mongodb:update_order(AssetId,<<"unknown">>,outside,NewTrackDetails#track.packet_time,RFID),
      ht_storage_mongodb:create_track_history(NewTrackDetails),
      ht_storage_redis:create_live_data(AssetId, NewTrackDetails),
      ht_device_alert_handler:initiate(hhd,AssetId,NewTrackDetails)
      end;
    Error ->
      ht_logger:info("NoMatch Scenario at unlocked in transit : ~s~n",[Error])
  end;

trip_live_n_historical_data(State = #state{user = User},<<"in_transit">>,GeoFencesCol,UpdatedTrack,AssetId,_Requested) ->
      %ht_logger:info("In Transit without unlock: ~p~n",[AssetId]),
  case ht_storage_mongodb:check_geo_by_id(GeoFencesCol, UpdatedTrack, User) of
    {ok, {fence_combo, <<"destination">>, GeoName}} ->
      NewTrackDetails = UpdatedTrack#track{geo_type = <<"destination">>,geo_name = GeoName},
      %ht_logger:info("In Transit without unlock inside geocheck ~p~n",[AssetId]),
      ht_storage_mongodb:update_order(AssetId,in_transit,NewTrackDetails#track.packet_time),
      ht_storage_mongodb:create_track_history(NewTrackDetails),
      ht_storage_redis:create_live_data(AssetId, NewTrackDetails),
      ht_device_alert_handler:initiate(hhd,AssetId,NewTrackDetails);
    {ok, {fence_combo, <<"forbidden">>, GeoName}} ->
      NewTrackDetails = UpdatedTrack#track{geo_type = <<"forbidden">>,geo_name = GeoName},
      ht_storage_mongodb:update_order(AssetId,forbidden,NewTrackDetails#track.packet_time),
      ht_storage_mongodb:create_track_history(NewTrackDetails),
      ht_storage_redis:create_live_data(AssetId, NewTrackDetails),
      ht_device_alert_handler:initiate(hhd,AssetId,NewTrackDetails);
    {ok, {fence_combo, <<"source">>, GeoName}} ->
      NewTrackDetails = UpdatedTrack#track{geo_type = <<"source">>,geo_name = GeoName},
      %ht_storage_mongodb:update_order(AssetId,initiate,NewTrackDetails#track.packet_time),
      ht_storage_mongodb:create_track_history(NewTrackDetails),
      ht_storage_redis:create_live_data(AssetId, NewTrackDetails),
      ht_device_alert_handler:initiate(hhd,AssetId, NewTrackDetails);
    {ok, {fence_combo,GeoType, GeoName}} ->
      NewTrackDetails = UpdatedTrack#track{geo_type = GeoType,geo_name = GeoName},
      %ht_storage_mongodb:update_order(AssetId,initiate,NewTrackDetails#track.packet_time),
      ht_storage_mongodb:create_track_history(NewTrackDetails),
      ht_storage_redis:create_live_data(AssetId, NewTrackDetails),
      ht_device_alert_handler:initiate(hhd,AssetId, NewTrackDetails);
    {error,<<"Not find item">>} ->
      NewTrackDetails = UpdatedTrack#track{geo_type = atom_to_binary(nil, utf8),geo_name = atom_to_binary(nil, utf8)},
      %ht_logger:info("In Transit without unlock outside geofence: ~p~n",[AssetId]),
      ht_storage_mongodb:create_track_history(NewTrackDetails),
      ht_storage_redis:create_live_data(AssetId, NewTrackDetails);
    %  ht_device_alert_handler:initiate(hhd,AssetId,NewTrackDetails);
    Error ->
      ht_logger:info("NoMatch Scenario at rest of in transit : ~s~n",[Error])
  end;

trip_live_n_historical_data(State = #state{user = User},<<"reached">>,GeoFencesCol,UpdatedTrack = #track{lock_status = locked} ,AssetId,_Requested) ->
  case ht_storage_mongodb:check_geo_by_id(GeoFencesCol, UpdatedTrack, User) of
    {ok, {fence_combo, <<"destination">>, GeoName}} ->
      NewTrackDetails = UpdatedTrack#track{geo_type = <<"destination">>,geo_name = GeoName},
      ht_storage_mongodb:create_track_history(NewTrackDetails),
      ht_storage_redis:create_live_data(AssetId, NewTrackDetails),
      ht_device_alert_handler:initiate(hhd,AssetId,NewTrackDetails);
    {ok, {fence_combo, <<"source">>, GeoName}} ->
      NewTrackDetails = UpdatedTrack#track{geo_type = <<"source">>,geo_name = GeoName},
      ht_storage_mongodb:create_track_history(NewTrackDetails),
      ht_storage_redis:create_live_data(AssetId, NewTrackDetails),
      ht_device_alert_handler:initiate(hhd,AssetId,NewTrackDetails);
    {error,<<"Not find item">>} ->
      NewTrackDetails = UpdatedTrack#track{geo_type = atom_to_binary(nil, utf8), geo_name = atom_to_binary(nil, utf8)},
      %event_type = atom_to_binary(out_of_destination, utf8)},
      ht_storage_mongodb:create_track_history(NewTrackDetails),
      ht_storage_redis:create_live_data(AssetId, NewTrackDetails),
      ht_device_alert_handler:initiate(hhd,AssetId,NewTrackDetails);
    Error ->
      ht_logger:info("NoMatch Scenario at locked reached : ~s~n",[Error])
  end;

trip_live_n_historical_data(State = #state{user = User},<<"reached">>,GeoFencesCol,UpdatedTrack = #track{lock_status = unlocked,
  unlock_report = RFID} ,AssetId,false) ->
  case ht_storage_mongodb:check_geo_by_id(GeoFencesCol, UpdatedTrack, User) of
    {ok, {fence_combo, <<"source">>, GeoName}} ->
      NewTrackDetails = UpdatedTrack#track{geo_type = <<"source">>, geo_name = GeoName,
        alarm_type = atom_to_binary(out_of_destination, utf8),alarm_report = <<"Device Tampered">>},
      ht_logger:info_debug("NEW HHD Reached unlock RFID number: ~p ~p ~n",[RFID,AssetId]),
      case ht_storage_mongodb:check_rfid(RFID) of
        {ok, {user, FName}} ->
          ht_storage_mongodb:update_order(AssetId,FName,false_outside,NewTrackDetails#track.packet_time,RFID),
      %ht_storage_mongodb:update_order(AssetId,FName,outside,NewTrackDetails#track.packet_time,RFID),
          ht_storage_mongodb:create_track_history(NewTrackDetails),
          ht_storage_redis:create_live_data(AssetId, NewTrackDetails),
          ht_device_alert_handler:initiate(hhd,AssetId,NewTrackDetails);
        {error,<<"Not find item">>} ->
          ht_logger:info_debug("Check RFID Failed"),
          ht_storage_mongodb:update_order(AssetId,<<"unknown">>,false_outside,NewTrackDetails#track.packet_time,RFID),
          %ht_storage_mongodb:update_order(AssetId,<<"unknown">>,outside,NewTrackDetails#track.packet_time,RFID),
          ht_storage_mongodb:create_track_history(NewTrackDetails),
          ht_storage_redis:create_live_data(AssetId, NewTrackDetails),
          %set_time_interval(State,<<"600">>,<<"1440">>),
          ht_device_alert_handler:initiate(hhd,AssetId,NewTrackDetails)
        end;
    {ok, {fence_combo, <<"destination">>, GeoName}} ->
      NewTrackDetails = UpdatedTrack#track{geo_type = <<"destination">>,geo_name = GeoName,alarm_report = <<"Device Tampered">>},
      ht_logger:info_debug("AES1 Reached unlock RFID number: ~p ~p~n",[RFID,AssetId]),
      case ht_storage_mongodb:check_rfid(RFID) of
        {ok, {user, FName}} ->
          ht_logger:info_debug("Reached User Name:~p~n",[FName]),
          ht_storage_mongodb:update_order(AssetId,FName,false_outside,NewTrackDetails#track.packet_time,RFID),
          %ht_storage_mongodb:update_order(AssetId,FName,reached,NewTrackDetails#track.packet_time,RFID),
          ht_storage_mongodb:create_track_history(NewTrackDetails),
          ht_storage_redis:create_live_data(AssetId, NewTrackDetails),
          %set_time_interval(State,<<"600">>,<<"1440">>),
          ht_device_alert_handler:initiate(hhd,AssetId,NewTrackDetails);
        {error,<<"Not find item">>} ->
          ht_logger:info_debug("Check RFID Failed"),
          ht_storage_mongodb:update_order(AssetId,<<"unknown">>,false_outside,NewTrackDetails#track.packet_time,RFID),
          %ht_storage_mongodb:update_order(AssetId,<<"unknown">>,reached,NewTrackDetails#track.packet_time,RFID),
          ht_storage_mongodb:create_track_history(NewTrackDetails),
          ht_storage_redis:create_live_data(AssetId, NewTrackDetails),
          %set_time_interval(State,<<"600">>,<<"1440">>),
          ht_device_alert_handler:initiate(hhd,AssetId,NewTrackDetails)
      end;
    {error,<<"Not find item">>} ->
      NewTrackDetails = UpdatedTrack#track{geo_type = atom_to_binary(nil, utf8), geo_name = atom_to_binary(nil, utf8),
        alarm_type = atom_to_binary(out_of_destination, utf8),alarm_report = <<"Device Tampered">>},
      ht_logger:info_debug("AES1 Reached unlock RFID number: ~p ~p~n",[RFID,AssetId]),
      case ht_storage_mongodb:check_rfid(RFID) of
        {ok, {user, FName}} ->
      ht_storage_mongodb:update_order(AssetId,FName,false_outside,NewTrackDetails#track.packet_time,RFID),
      %ht_storage_mongodb:update_order(AssetId,FName,outside,NewTrackDetails#track.packet_time,RFID),
      ht_storage_mongodb:create_track_history(NewTrackDetails),
      ht_storage_redis:create_live_data(AssetId, NewTrackDetails),
      ht_device_alert_handler:initiate(hhd,AssetId,NewTrackDetails);
        {error,<<"Not find item">>} ->
          ht_logger:info_debug("Check RFID Failed"),
          ht_storage_mongodb:update_order(AssetId,<<"unknown">>,false_outside,NewTrackDetails#track.packet_time,RFID),
          %ht_storage_mongodb:update_order(AssetId,<<"unknown">>,outside,NewTrackDetails#track.packet_time,RFID),
          ht_storage_mongodb:create_track_history(NewTrackDetails),
          ht_storage_redis:create_live_data(AssetId, NewTrackDetails),
          %set_time_interval(State,<<"600">>,<<"1440">>),
          ht_device_alert_handler:initiate(hhd,AssetId,NewTrackDetails)
        end;
    Error ->
      ht_logger:info("NoMatch Scenario at unlocked reached : ~s~n",[Error])
  end;

trip_live_n_historical_data(State = #state{user = User},<<"reached">>,GeoFencesCol,UpdatedTrack = #track{lock_status = unlocked,
  unlock_report = RFID} ,AssetId,true) ->
  case ht_storage_mongodb:check_geo_by_id(GeoFencesCol, UpdatedTrack, User) of
    {ok, {fence_combo, <<"source">>, GeoName}} ->
      NewTrackDetails = UpdatedTrack#track{geo_type = <<"source">>, geo_name = GeoName,
        alarm_type = atom_to_binary(out_of_destination, utf8),alarm_report = <<"Device Tampered">>},
      ht_logger:info_debug("NEW HHD Reached unlock RFID number: ~p ~p~n",[RFID,AssetId]),
      case ht_storage_mongodb:check_rfid(RFID) of
        {ok, {user, FName}} ->
          ht_storage_mongodb:update_order(AssetId,FName,outside,NewTrackDetails#track.packet_time,RFID),
          ht_storage_mongodb:create_track_history(NewTrackDetails),
          ht_storage_redis:create_live_data(AssetId, NewTrackDetails),
          ht_device_alert_handler:initiate(hhd,AssetId,NewTrackDetails);
        {error,<<"Not find item">>} ->
          ht_logger:info_debug("Check RFID Failed"),
          ht_storage_mongodb:update_order(AssetId,<<"unknown">>,outside,NewTrackDetails#track.packet_time,RFID),
          ht_storage_mongodb:create_track_history(NewTrackDetails),
          ht_storage_redis:create_live_data(AssetId, NewTrackDetails),
          %set_time_interval(State,<<"600">>,<<"1440">>),
          ht_device_alert_handler:initiate(hhd,AssetId,NewTrackDetails)
       end;
    {ok, {fence_combo, <<"destination">>, GeoName}} ->
      NewTrackDetails = UpdatedTrack#track{geo_type = <<"destination">>,geo_name = GeoName},
      ht_logger:info_debug("AES1 Reached unlock RFID number: ~p ~p~n",[RFID,AssetId]),
      case ht_storage_mongodb:check_rfid(RFID) of
        {ok, {user, FName}} ->
          ht_logger:info_debug("Reached User Name:~p~n",[FName]),
          ht_storage_mongodb:update_order(AssetId,FName,reached,NewTrackDetails#track.packet_time,RFID),
          ht_storage_mongodb:create_track_history(NewTrackDetails),
          ht_storage_redis:create_live_data(AssetId, NewTrackDetails),
          %set_time_interval(State,<<"600">>,<<"1440">>),
          ht_device_alert_handler:initiate(hhd,AssetId,NewTrackDetails);
        {error,<<"Not find item">>} ->
          ht_logger:info_debug("Check RFID Failed"),
          ht_storage_mongodb:update_order(AssetId,<<"unknown">>,reached,NewTrackDetails#track.packet_time,RFID),
          ht_storage_mongodb:create_track_history(NewTrackDetails),
          ht_storage_redis:create_live_data(AssetId, NewTrackDetails),
          %set_time_interval(State,<<"600">>,<<"1440">>),
          ht_device_alert_handler:initiate(hhd,AssetId,NewTrackDetails)
      end;
    {error,<<"Not find item">>} ->
      NewTrackDetails = UpdatedTrack#track{geo_type = atom_to_binary(nil, utf8), geo_name = atom_to_binary(nil, utf8),
        alarm_type = atom_to_binary(out_of_destination, utf8),alarm_report = <<"Device Tampered">>},
      ht_logger:info_debug("AES1 Reached unlock RFID number: ~p ~p~n",[RFID,AssetId]),
      case ht_storage_mongodb:check_rfid(RFID) of
        {ok, {user, FName}} ->
      ht_storage_mongodb:update_order(AssetId,FName,outside,NewTrackDetails#track.packet_time,RFID),
      ht_storage_mongodb:create_track_history(NewTrackDetails),
      ht_storage_redis:create_live_data(AssetId, NewTrackDetails),
      ht_device_alert_handler:initiate(hhd,AssetId,NewTrackDetails);
        {error,<<"Not find item">>} ->
          ht_logger:info_debug("Check RFID Failed"),
          ht_storage_mongodb:update_order(AssetId,<<"unknown">>,outside,NewTrackDetails#track.packet_time,RFID),
          ht_storage_mongodb:create_track_history(NewTrackDetails),
          ht_storage_redis:create_live_data(AssetId, NewTrackDetails),
          %set_time_interval(State,<<"600">>,<<"1440">>),
          ht_device_alert_handler:initiate(hhd,AssetId,NewTrackDetails)
        end;
    Error ->
      ht_logger:info("NoMatch Scenario at unlocked reached : ~s~n",[Error])
  end;


 trip_live_n_historical_data(State = #state{user = User},<<"completed">>,_GeoFencesCol,Track,AssetId,Requested) ->
  trip_unmapped_data(Track,AssetId, User).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


trip_historical_data(GeoFencesCol,Track, User) ->
 case ht_storage_mongodb:check_geo_by_id(GeoFencesCol, Track, User) of
    {ok, {fence_combo, GeoType, GeoName}} ->
      NewTrackDetails = Track#track{geo_type = GeoType,geo_name = GeoName},
      ht_storage_mongodb:create_track_history(NewTrackDetails);
    {error,<<"Not find item">>} ->
      NewTrackDetails = Track#track{geo_type = atom_to_binary(nil, utf8),geo_name = atom_to_binary(nil, utf8)},
      NewTrackDetails1 =  ht_hhd_helper_lynktrac:validate_out_packets_in_geofence( NewTrackDetails#track.asset_id, GeoFencesCol, NewTrackDetails, User),
      ht_storage_mongodb:create_track_history(NewTrackDetails1);
    Error ->
      ht_logger:info("Completed NoMatch Scenario: ~s~n",[Error])
  end.

trip_unmapped_data(Track,AssetId, User) ->
  case ht_storage_mongodb:check_geo_by_id(<<"geohamaratrucks">>, Track, User) of
    {ok, {fence_combo, GeoType, GeoName}} ->
      NewTrackDetails = Track#track{geo_type = GeoType,geo_name = GeoName},
      ht_storage_mongodb:create_track_history(NewTrackDetails),
      ht_storage_redis:create_live_data(AssetId, NewTrackDetails);
    {error,<<"Not find item">>} ->
      NewTrackDetails = Track#track{geo_type = atom_to_binary(nil, utf8),geo_name = atom_to_binary(nil, utf8)},
      ht_storage_mongodb:create_track_history(NewTrackDetails),
      ht_storage_redis:create_live_data(AssetId, NewTrackDetails);
    Error ->
      ht_logger:info("NoMatch Scenario: ~s~n",[Error])
  end.

trip_unmapped_historical_data(Track,AssetId, User) ->
  case ht_storage_mongodb:check_geo_by_id(<<"geohamaratrucks">>, Track, User) of
    {ok, {fence_combo, GeoType, GeoName}} ->
      NewTrackDetails = Track#track{geo_type = GeoType,geo_name = GeoName},
      ht_storage_mongodb:create_track_history(NewTrackDetails);
    {error,<<"Not find item">>} ->
      NewTrackDetails = Track#track{geo_type = atom_to_binary(nil, utf8),geo_name = atom_to_binary(nil, utf8)},
      ht_storage_mongodb:create_track_history(NewTrackDetails);
    Error ->
      ht_logger:info("NoMatch Scenario: ~s~n",[Error])
  end.

send_lock_cmd(#state{transport = Transport, socket = Socket,assetId = DeviceId}) ->
    Id = integer_to_list(DeviceId),
    TerminalId = list_to_binary(hexstr_to_list("0"++Id)),
    Body = <<16#01,16#24,16#01,16#01>>,
    Header = <<16#03,16#10,16#80,16#0A,TerminalId/binary,16#00,16#0C>>,
    EncryptBody = get_encryption(Body),
    Crc = ht_checksum:crc(<<Header/binary,EncryptBody/binary>>),

  Packet = <<
    Header/binary,
    EncryptBody/binary,
    Crc:8/unsigned-integer
  >>,
  P1 = check_packet(Packet),
  ht_logger:info("Terminal id for LOCKED: ~s, ~s", [parse_hex(TerminalId),parse_hex(P1)]),
  Transport:send(Socket, <<16#7E,P1/binary,16#7E>>).


do_send_response(#state{transport = Transport, socket = Socket},TerminalId,MsgId,MsgSerialNo) ->
    
    Body = <<MsgSerialNo/binary,MsgId/binary,16#00>>,
    Header = <<16#80,16#01,16#10,16#0B,TerminalId/binary,16#00,16#0A>>,
    EncryptBody = get_encryption(Body),
    Crc = ht_checksum:crc(<<Header/binary,EncryptBody/binary>>),
     
  Packet = <<
    Header/binary,
    EncryptBody/binary,
    Crc:8/unsigned-integer
  >>,
  P1 = check_packet(Packet), 
  Transport:send(Socket, <<16#7E,P1/binary,16#7E>>);
do_send_response(_,_,_,_) ->
  ht_logger:info("NO RESPONSE PACKET").

get_encryption(Body) ->
    A = calendar:system_time_to_rfc3339(erlang:system_time(second), [{time_designator, $-},{offset, "+08:00"}]),
    B = binary:part(list_to_binary(A),{2,byte_size(list_to_binary(A))-8}),
    C = binary:replace(B,[<<":">>,<<"-">>],<<"">>,[global]),
    D = list_to_binary(hexstr_to_list(binary_to_list(C))),

    NewBody = <<D:6/binary,Body/binary>>,
    list_to_binary(ht_aes:aes256ecb(NewBody,1)).

parse_accuracy( <<Acc:16/unsigned-integer,_/binary>>) ->
    Acc.

parse_satellite(<<_:20,Sat:6,_:6>>) ->
 <<SAT:16>> =  <<0:10,Sat:6>>,
     integer_to_binary(SAT).
parse_latitude(Val, EW) ->
   C = binary:decode_unsigned(Val),
   E = C /1000000,
   <<_:29/bits,X:1/bits,_:2/bits>> = EW,
   
   case X of
    <<1:1>> ->
      -1 * E;
    <<0:1>> ->
      E
   end.

parse_longitude(Val, NS) ->
   C = binary:decode_unsigned(Val),
   E = C /1000000,
   <<_:28/bits,X:1/bits,_:3/bits>> = NS,
   case X of
    <<1:1>> ->
      -1 * E;
    <<0:1>> ->
      E
   end.

parse_hex(Bin) ->
  list_to_binary(lists:flatten([io_lib:format("~2.16.0B", [X]) || X <- binary_to_list(Bin)])).


parse_imei(Bin) ->
  fix_imei(list_to_binary(lists:flatten([io_lib:format("~2.16.0B", [X]) || X <- binary_to_list(Bin)]))).

fix_imei(Bin) when byte_size(Bin) > 11 ->
  binary:part(Bin, {byte_size(Bin) - 11, 11});
fix_imei(Bin) -> Bin.

parse_date(Year, Month, Day, Hour, Minute, Second) ->
  try Date = {
    {
      list_to_integer(binary_to_list(parse_hex(Year))) + 2000,
      list_to_integer(binary_to_list(parse_hex(Month))),
      list_to_integer(binary_to_list(parse_hex(Day)))
    },
    {
      list_to_integer(binary_to_list(parse_hex(Hour))),
      list_to_integer(binary_to_list(parse_hex(Minute))),
      list_to_integer(binary_to_list(parse_hex(Second)))
    }
  },
  ht_helper_time:chinatime_to_utc(Date)
 catch
Error:Reason -> {Error, Reason} end.

check_bin(Bin) ->
   case Bin of
    <<0:1>> ->
      0;
    <<1:1>> ->
      1
   end.

parse_status(<<_:30/bits,X:1/bits,_:1/bits>>) ->
  case X of
    <<0:1>> ->
      false;
    <<1:1>> ->
      true
   end.

parse_charging(<<_:26/bits,X:2/bits,_:4/bits>>) ->
  case X of
    <<0:2>> ->
      false;
    <<1:2>> ->
      true;
    _ ->
      false
   end.

parse_hall_status(<<_:16/bits,Bat:1/bits,_:15/bits>>) ->
  case Bat of
    <<0:1>> ->
      0;
    <<1:1>> ->
      1
   end.

parse_travel_status(<<_:18/bits,Bat:1/bits,_:13/bits>>) ->
  case Bat of
    <<0:1>> ->
      0;
    <<1:1>> ->
      1
   end.

parse_sim_status(<<_:20/bits,Bat:2/bits,_/bits>>) ->
  case Bat of
    <<0:2>> ->
      0;
    <<1:2>> ->
      1;
    <<2:2>> ->
      2;
    _ ->
      0
   end.

check_pattern(Data) ->
  A = binary:replace(Data,<<125,2>>,<<126>>,[global]),
  binary:replace(A,<<125,1>>,<<125>>,[global]).

%check_pattern(Data) ->
%  A = case re:run(Data,<<125,1>>) of
%     nomatch ->
%        Data;
%     {match,_} ->
%       re:replace(Data,<<125,1>>,<<"XXXX">>,[global,{return,binary}])
%     end,
%  
%  B =  case re:run(A,<<125,2>>) of
%     nomatch ->
%        A;
%     {match,_} ->
%        re:replace(A,<<125,2>>,<<126>>,[global,{return,binary}])
%     end,
%  case re:run(B,<<"XXXX">>) of
%     nomatch ->
%        B;
%     {match,_} ->
%        re:replace(B,<<"XXXX">>,<<125>>,[global,{return,binary}])
%     end.
%
check_packet(Data) ->
  A = binary:replace(Data,<<125>>,<<125,1>>,[global]),
  binary:replace(A,<<126>>,<<125,2>>,[global]).

%check_packet(Data) ->
%  A =  case re:run(Data,<<16#7D>>) of
%     nomatch ->
%       Data;
%     {match,_} ->
%       re:replace(Data,<<16#7D>>,<<125,1>>,[global,{return,binary}])
%     end,
%
%    case re:run(A,<<16#7E>>) of
%     nomatch ->
%        A;
%     {match,_} ->
%        re:replace(A,<<16#7E>>,<<125,2>>,[global,{return,binary}])
%     end.

decode(State,Track,<<16#39,AddMsgLen:8/unsigned-integer,AddMsg:AddMsgLen/binary,Rest/binary>>) ->
  decode(State,Track,Rest);
decode(State,Track,<<16#33,AddMsgLen:8/unsigned-integer,AddMsg:AddMsgLen/binary,Rest/binary>>) ->
  UpdatedTrack =  decode_addmsg_33(State,Track,AddMsg),
  decode(State,UpdatedTrack,Rest);
decode(State,Track,<<16#35,AddMsgLen:8/unsigned-integer,AddMsg:AddMsgLen/binary,Rest/binary>>) ->
  UpdatedTrack =  decode_addmsg_35(Track,AddMsg),
  decode(State,UpdatedTrack,Rest);
decode(State=#state{assetId = DeviceId,socket=Socket,transport=Transport},Track,<<42,Cmd:3/binary,44,Length:2/binary,16#2C,Result/binary>>) ->
  case Cmd of 
   <<"D01">> ->
      case Result of
       <<"1#">> ->
         ht_storage_redis:update_single_field(DeviceId,fwUpdateStatus,"end"),
         ht_storage_redis:update_single_field(DeviceId,fwVersionAllow,true),
         ht_logger:info("FW VERSION CHECK RESULT: ~p~n", [Result]);
       _ ->
         ht_storage_redis:update_single_field(DeviceId,fwVersionAllow,false),
         ht_logger:info("FW VERSION CHECK RESULT: ~p~n", [Result])
     end; 
   <<"D02">> ->
      ht_logger:info("D02 CMD RESULT : ~p ~p ~p~n", [Length,Cmd,Result]),
      <<Current:3/binary,Res:1/binary,_/binary>> = Result,
       case Res of 
        <<"1">> ->
              {ok,Total} = ht_storage_redis:get_single_field(DeviceId,fwUpdateTotal),
              case Current of 
                 <<"E51">> ->
                   ht_logger:info("ERROR WHILE UPGRADING: ~p ~p~n", [Cmd,Result]);
                  _ ->
                   case binary_to_integer(Current) =:= binary_to_integer(Total) of
                     false ->
                      ht_storage_redis:update_single_field(DeviceId,fwUpdateCurrent,binary_to_integer(Current)), 
                      Id = integer_to_list(DeviceId),
                      TerminalId = list_to_binary(hexstr_to_list("0"++Id)),
                      {ok,Path} = ht_storage_redis:get_single_field(DeviceId,fwPath),
                      {ok,CRC} = ht_storage_redis:get_single_field(DeviceId,fwCrc),
                      {ok,File} = file:open(Path,[read]),
                      {ok,Data} = file:pread(File,(binary_to_integer(Current))*512,512),
                 %     ht_logger:info("FW DATA : ~p ~n", [Data]),
                      {ok,FwPacket} =  ht_hhd_cmd_aes:fw_update(TerminalId,binary_to_integer(Total),list_to_binary(Data),binary_to_integer(Current)+1,CRC),
                      Transport:send(Socket,FwPacket);
                     true ->
                      ht_storage_redis:update_single_field(DeviceId,fwUpdateStatus,"end")
                   end
              end;  
         _ ->
              Id = integer_to_list(DeviceId),
              TerminalId = list_to_binary(hexstr_to_list("0"++Id)),
              {ok,Total}= ht_storage_redis:get_single_field(DeviceId,fwUpdateTotal),
              {ok,Path} = ht_storage_redis:get_single_field(DeviceId,fwPath),
              {ok,CRC} = ht_storage_redis:get_single_field(DeviceId,fwCrc),
              {ok,Current1} = ht_storage_redis:get_single_field(DeviceId,fwUpdateCurrent),
              {ok,File} = file:open(Path,[read]),
              {ok,Data} = file:pread(File,(binary_to_integer(Current1))*512,512),
           %   ht_logger:info("FW DATA : ~p ~n", [Data]),
              {ok,FwPacket} =  ht_hhd_cmd_aes:fw_update(TerminalId,binary_to_integer(Total),list_to_binary(Data),binary_to_integer(Current1)+1,CRC),
              Transport:send(Socket,FwPacket)
       end;    
  <<"M07">> ->

      Key = [cmd_receive,"GET_RFID_CARD",cmd_receive_time,ht_helper_time:timestamp(),conSetRfidCard,Result],
      ht_storage_redis:mupdate_config_info_hhd(DeviceId,Key),		  
      ht_logger:info("M07 CMD RESULT : ~p ~p ~p~n", [Length,Cmd,Result]);
   _ ->
      ht_logger:info("UNKNOWN CMD RESULT: ~p ~p~n", [Cmd,Result])
  end,
 
  case ht_storage_redis:get_hhd_data(Track#track.asset_id) of
    {hhd,LockStatus,RFID,Alarm_bit,Remain} ->
     <<_:2/binary,Battery:3/binary,_/binary>> = Alarm_bit,
     Track#track{
       alarm_bit = Alarm_bit,
       lock_status = binary_to_atom(LockStatus,utf8),
       unlock_report = RFID,
       battery_voltage = Battery,
       remaining_batt = Remain, 
       packet_type = command
      };
    _ -> Track
 end;   


decode(_,Track,<<"">>) -> Track;
decode(_,Track,_) -> 
  case ht_storage_redis:get_hhd_data(Track#track.asset_id) of
    {hhd,LockStatus,RFID,Alarm_bit,Remain} ->
     <<_:2/binary,Battery:3/binary,_/binary>> = Alarm_bit,
     Track#track{
       alarm_bit = Alarm_bit,
       lock_status = binary_to_atom(LockStatus,utf8),
       unlock_report = RFID,
       battery_voltage = Battery,
       remaining_batt = Remain,
       packet_type = command
      };
    _ -> Track
 end.

decode_addmsg_33(State,Track,<<42,77,48,48,44,Add/binary>>) ->
   [Length,RestPack] = string:split(Add, ","),
   Alarm_bit = binary:part(RestPack,{0,7}),
   <<LockStatus:1/binary,SealStatus:1/binary,Battery:3/binary,TearStatus:1/binary,ReportReason:1/binary>> = Alarm_bit,        
   Rest = binary:part(RestPack,{7,(byte_size(RestPack)-7)}),
   [RFID,SubLock,ID] = case string:split(Rest,"&", all) of
       [R,S,I] -> [R,S,I];
       [R,S]  -> [R,S,0];
       [R] -> [R,0,0]
    end,

  {AlarmReport,AlarmType} = check_alarms(Track#track.asset_id,[
    {<<"Steel String Tampered">>,TearStatus}],[],[]),

   Track#track{
       alarm_bit = Alarm_bit,
       lock_status = check_lock(LockStatus),
       unlock_report = RFID,
       battery_voltage = Battery,
       alarm_report = AlarmReport,
       alarm_type = AlarmType,
       remaining_batt = get_remaining_batt(binary_to_integer(Battery))
  };
decode_addmsg_33(State,Track,<<16#2A,16#4D,16#30,16#36,16#2C,Add/binary>>) ->
 <<Size:2/binary,",",Result:1/binary,_/binary>> = Add,    
  ht_logger:info("M06 CARD ADD RESULT:~p ~p ", [Track#track.asset_id,Add]),
  case Result of
    <<"1">> ->
      case ht_storage_redis:get_single_config_info_hhd(Track#track.asset_id,reqSetRfidCard) of
      {ok,Value2} ->
           Key = [cmd_receive,"SET_RFID_CARD",cmd_receive_time,ht_helper_time:timestamp(),conSetRfidCard,Value2],
           ht_storage_redis:mupdate_config_info_hhd(Track#track.asset_id,Key);
       _ -> ok
      end;
     _ -> ok
   end,

  ht_storage_redis:update_single_field(Track#track.asset_id,card_update,true),
  case ht_storage_redis:get_hhd_data(Track#track.asset_id) of
    {hhd,LockStatus,RFID,Alarm_bit,Remain} ->
     <<_:2/binary,Battery:3/binary,_/binary>> = Alarm_bit,
     Track#track{
       alarm_bit = Alarm_bit,
       lock_status = binary_to_atom(LockStatus,utf8),
       unlock_report = RFID,
       battery_voltage = Battery,
       remaining_batt = Remain,
       packet_type = command
      };
    _ -> Track
 end;
decode_addmsg_33(State,Track,<<16#2A,16#4D,16#30,16#37,16#2C,Size:2/binary,",",Add/binary>>) ->
  ht_logger:info("M07 CARD READ RESULT:~p ~p ", [Track#track.asset_id,Add]),
  Data = binary:part(Add, {0, byte_size(Add) - 1}),
  Key = [cmd_receive,"GET_RFID_CARD",cmd_receive_time,ht_helper_time:timestamp(),conSetRfidCard,Data],
  ht_storage_redis:mupdate_config_info_hhd(Track#track.asset_id,Key),
  case ht_storage_redis:get_hhd_data(Track#track.asset_id) of
    {hhd,LockStatus,RFID,Alarm_bit,Remain} ->
     <<_:2/binary,Battery:3/binary,_/binary>> = Alarm_bit,
     Track#track{
       alarm_bit = Alarm_bit,
       lock_status = binary_to_atom(LockStatus,utf8),
       unlock_report = RFID,
       battery_voltage = Battery,
       remaining_batt = Remain,
       packet_type = command
      };
    _ -> Track
 end;
decode_addmsg_33(_,Track,_) ->
 case ht_storage_redis:get_hhd_data(Track#track.asset_id) of
    {hhd,LockStatus,RFID,Alarm_bit,Remain} ->
     <<_:2/binary,Battery:3/binary,_/binary>> = Alarm_bit,
     Track#track{
       alarm_bit = Alarm_bit,
       lock_status = binary_to_atom(LockStatus,utf8),
       unlock_report = RFID,
       battery_voltage = Battery,
       remaining_batt = Remain,
       packet_type = command
      };
    _ -> Track
 end.



decode_addmsg_35(Track,<<MMC:2/binary,Data/binary>>) ->
   
   UpdatedTrack = loop_35(Track#track{mcc = parse_hex(MMC)},Data),
   UpdatedTrack.

loop_35(Track,<<RXL:1/binary,MNC:2/binary,CELLID:2/binary,LAC:2/binary,Rest/binary>>) ->
     UpdatedTrack =  Track#track{
         lac = parse_hex(LAC),
         mnc = parse_hex(MNC),
         cellid = parse_hex(CELLID),
         rxl = parse_hex(RXL)
     },
   loop_35(UpdatedTrack,Rest);
loop_35(Track,<<"">>) -> Track. 


check_alarms(_AssetId,[],[],[]) ->
  {<<"NA">>,<<"NA">>};

check_alarms(AssetId,[],<<"Device Tampered">> = AlarmReport,AlarmType) ->
 % ht_storage_mongodb:update_order(AssetId,wirecut),
  {AlarmReport,AlarmType};

check_alarms(_AssetId,[{Alarm,AlarmVal}|OtherTypes],AlarmReport,AlarmType) ->
  case check_tear(AlarmVal) of
    false ->
      check_alarms(_AssetId,OtherTypes,AlarmReport,AlarmType);
    true ->
      check_alarms(_AssetId,OtherTypes,<<"Device Tampered">>,AlarmType ++ [Alarm])
  end.


bat_per(Vol) when Vol >= 420 ->
   100.0;
bat_per(Vol) when Vol < 420, Vol >= 406 ->
   90 + (10 * ( Vol - 406 ) / (420 - 406));
bat_per(Vol) when Vol < 406, Vol >= 398 ->
   80 + (10 * ( Vol - 398 ) / ( 406 - 398));
bat_per(Vol) when Vol < 398, Vol >= 392 ->
   70 + (10 * ( Vol - 392 ) / ( 398 - 392 ));
bat_per(Vol) when Vol < 392, Vol >= 387 ->
   60 + (10 * ( Vol - 387 ) / ( 392 - 387 ));
bat_per(Vol) when Vol < 387, Vol >= 382 ->
   50 + (10 * ( Vol - 382) / ( 387 - 382 ));
bat_per(Vol) when Vol < 382, Vol >= 379 ->
   40 + (10 * ( Vol - 379) / ( 382 - 379));
bat_per(Vol) when Vol < 379, Vol >= 377 ->
   30 + (10 * ( Vol - 377 ) / ( 379 - 377));
bat_per(Vol) when Vol < 377, Vol >= 374 ->
   20 + (10 * ( Vol - 374 ) / ( 377 - 374 ));
bat_per(Vol) when Vol < 374, Vol >= 368 ->
   10 + (10 * ( Vol - 368 ) / ( 374 - 368));
bat_per(Vol) when Vol < 368, Vol >= 345 ->
   5 + (5 * ( Vol - 345 ) / ( 368 - 345 ));
bat_per(Vol) when Vol < 345, Vol >= 300 ->
       5 * ( Vol - 300 ) / ( 345 - 300);
bat_per(Vol) when Vol =< 300 ->
    0.0;
bat_per(_) ->
    0.0.
 

check_lock(Bin) ->
   case Bin of
    <<"1">> ->
      locked;
    <<"0">> ->
      unlocked
   end. 

check_tear(Bin) ->
    case Bin of
    <<"1">> ->
      true;
    <<"0">> ->
      false
   end.


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

%check(Latitude,Longitude) -> valid;
%check(_Latitude,_Longitude) -> invalid.
check(Latitude,Longitude) when Latitude =< 34.55765,Longitude =< 96.12882,Latitude > 7.01934,Longitude > 68.29478 -> valid;
check(_Latitude,_Longitude) -> invalid.

%%%===================================================================
%%% Internal functions - Initiation of Cards
%%%===================================================================

initiate_time_alert(State,AssetId,HitFrequency) ->
  case ht_storage_redis:get_time_info(AssetId) of
    false ->
      set_time_interval(State,AssetId,HitFrequency),
      ht_storage_redis:update_time_info(AssetId,true);
    true ->
      already_initiated;
    {error,_Error} ->
      ht_storage_redis:add_time_info(AssetId),
      set_time_interval(State,AssetId,HitFrequency),
      ht_storage_redis:update_time_info(AssetId,true)
  end.

%%%===================================================================

%%%===================================================================
%%% Internal function - Time Interval
%%%===================================================================

set_time_interval(State=#state{socket=Socket, transport=Transport},DeviceId,HitFrequency) ->
    Id = integer_to_list(DeviceId),
    TerminalId = list_to_binary(hexstr_to_list("0"++Id)),
    Fq = binary_to_list(HitFrequency),
    Hf = list_to_binary(hexstr_to_list(Fq)),
    Body = <<16#01,16#02,16#02,16#00,Hf:1/binary>>,
    Header = <<16#03,16#10,16#00,16#05,TerminalId/binary,16#00,16#0A>>,
    Crc = ht_checksum:crc(<<Header/binary,Body/binary>>),

  Packet = <<
    Header/binary,
    Body/binary,
    Crc:8/unsigned-integer
  >>,
  ht_logger:info_debug("SET TIME PACKET: ~w", [Packet]),
  P1 = check_packet(Packet), 
  Transport:send(Socket,<<16#7E,P1/binary,16#7E>>).
check_wirecut(Track, Bit) when Bit =:= <<"2">> ->
ht_storage_mongodb:update_order(Track#track.asset_id,wirecut);    
check_wirecut(_,_) -> ok.

%update_report_reason(Track,Bit) when Bit =:= <<"E">> ->
%ht_storage_mongodb:update_report_reason(Track#track.asset_id,"BT");
%update_report_reason(Track,Bit) when Bit =:= <<"G">> ->
%ht_storage_mongodb:update_report_reason(Track#track.asset_id,"WEB");
%update_report_reason(Track,Bit) when Bit =:= <<"C">> ->
%ht_storage_mongodb:update_report_reason(Track#track.asset_id,"RFID");
%update_report_reason(_,_)  -> ok.


update_report_reason(Track,Bit) when Bit =:= <<"E">> ->
ht_storage_mongodb:update_report_reason(Track#track.asset_id,"BT"),
Track#track{course = <<"BT">>};
update_report_reason(Track,Bit) when Bit =:= <<"G">> ->
ht_storage_mongodb:update_report_reason(Track#track.asset_id,"WEB"),
Track#track{course = <<"WEB">>};
update_report_reason(Track,Bit) when Bit =:= <<"C">> ->
ht_storage_mongodb:update_report_reason(Track#track.asset_id,"RFID"),
Track#track{course = <<"RFID">>};
update_report_reason(Track,Bit) when Bit =:= <<"7">> ->
ht_storage_mongodb:update_report_reason(Track#track.asset_id,"SMS"),
Track#track{course = <<"SMS">>};
update_report_reason(Track,_)  -> Track.


check_speed(Track = #track{latitude = Latitude1, longitude = Longitude1, packet_time = PacketTime2},PacketTime1,Latitude2,Longitude2) when PacketTime2 /= PacketTime1 ->
  Deg2rad = fun(Deg) -> math:pi()*Deg/180 end,
  [RLng1, RLat1, RLng2, RLat2] = [Deg2rad(Deg) || Deg <- [Longitude1,Latitude1,Longitude2,Latitude2]],
  DLon = RLng2 - RLng1,
  DLat = RLat2 - RLat1,
  A = math:pow(math:sin(DLat/2), 2) + math:cos(RLat1) * math:cos(RLat2) * math:pow(math:sin(DLon/2), 2),
  C = 2 * math:asin(math:sqrt(A)),
  Km = 6372.8 * C,
 % ht_logger:info("CHECK SPEED: ~w ~p ~p ", [Km,PacketTime2,PacketTime1]),
  Speed = (Km * 3600)/abs(PacketTime2 - PacketTime1),
  case Speed > 180.0 of
       true -> true;
       false -> false
  end;
check_speed(_,_,_,_) -> false.

check_batt(Asset_id,Battery) ->
   case ht_storage_redis:get_single_field(Asset_id,batt3) of
    {error,undefined} ->
       ht_storage_redis:update_single_field(Asset_id,batt3,Battery),
       {ok,float_to_binary(bat_per(Battery),[{decimals,2}]),integer_to_binary(get_remaining_batt(Battery))};
    {ok,Batt3} ->
       case ht_storage_redis:get_single_field(Asset_id,batt2) of
          {error,undefined} ->
             ht_storage_redis:update_single_field(Asset_id,batt2,Battery),
             {ok,float_to_binary(bat_per(Battery),[{decimals,2}]),integer_to_binary(get_remaining_batt(Battery))};
          {ok,Batt2} ->
             case ht_storage_redis:get_single_field(Asset_id,batt1) of
               {error,undefined} ->
                  ht_storage_redis:update_single_field(Asset_id,batt1,Battery),
                  {ok,float_to_binary(bat_per(Battery),[{decimals,2}]),integer_to_binary(get_remaining_batt(Battery))};
               {ok,Batt1} ->
                  ht_storage_redis:update_single_field(Asset_id,batt3,Batt2),
                  ht_storage_redis:update_single_field(Asset_id,batt2,Batt1),
                  ht_storage_redis:update_single_field(Asset_id,batt1,Battery),
                  %ht_logger:info("CHECK BATT CHECKING: ~w ~p ~p ~p ~p", [Asset_id,Battery, Batt3,Batt2,Batt1]),
                  case bat_check([binary_to_integer(Batt3),binary_to_integer(Batt2),binary_to_integer(Batt1)],Battery,0) of
                    true ->
                   %    ht_logger:info("CHECK BATT TRUE: ~w ~p", [Asset_id,Battery]),
                      {ok,float_to_binary(bat_per(Battery),[{decimals,2}]),integer_to_binary(get_remaining_batt(Battery))};                     
                    false ->
                      case ht_storage_redis:get_single_field(Asset_id,battery_voltage) of 
                       {ok,BATT} ->
                         case ht_storage_redis:get_single_field(Asset_id,remaining_batt) of
                           {ok,<<"">>} ->
                             {ok,float_to_binary(bin_to_num(BATT),[{decimals,2}]),integer_to_binary(get_remaining_batt(Battery))};
                           {ok,Remain} ->
                             {ok,float_to_binary(bin_to_num(BATT),[{decimals,2}]),bin_to_numi(Remain)};
                           {error,undefined} ->
                             {ok,float_to_binary(bin_to_num(BATT),[{decimals,2}]),integer_to_binary(get_remaining_batt(Battery))} 
                         end;
                       {error,undefined} ->
                         {ok,float_to_binary(bat_per(Battery),[{decimals,2}]),integer_to_binary(get_remaining_batt(Battery))} 
                      end 
                  end   
             end
        end
   end.

bat_check([],_Batt,Count) when Count > 2 ->
   true;
bat_check([],_Batt,Count) ->
   false;
bat_check([BattH|BattT],Batt,Count) when Batt =:= BattH  ->
   bat_check(BattT,Batt,Count+1);
bat_check([BattH|BattT],Batt,Count) -> 
   bat_check(BattT,Batt,Count).
    
bin_to_num(Bin) ->
    N = binary_to_list(Bin),
    case string:to_float(N) of
        {error,no_float} -> list_to_integer(N)/1;
        {F,_Rest} -> F
    end.   

bin_to_numi(Bin) ->
    N = binary_to_list(Bin),
    case string:to_float(N) of
        {error,no_float} -> list_to_integer(N);
        {F,_Rest} -> F
    end.

check_fwStatus(Status) ->
  case Status of
    {error,undefined} ->
       {ok,<<"end">>};
    {ok,<<"end">>} ->
       {ok,<<"end">>};
    _ ->
       {ok,<<"ongoing">>} 
    end.     

get_remaining_batt(Batt) when Batt > 415 ->
  206;
get_remaining_batt(Batt) when Batt < 350 ->
  0;
get_remaining_batt(Batt) ->
  {ok,{_,Hours}} = ht_manager_firmware:get_by_id(Batt), 
  Hours. 


hhd_event_adder(State,UpdatedTrack) ->
   AssetId = UpdatedTrack#track.asset_id,
   Alarm_bit = UpdatedTrack#track.alarm_bit,
   Speed = UpdatedTrack#track.speed,
   <<Current_lock:2/binary,Battery:3/binary,TearStatus:1/binary,ReportReason:1/binary>> = Alarm_bit,

{ok,VehicleNo} = case  ht_storage_redis:get_single_field(UpdatedTrack#track.asset_id,vehicleNo) of
                    {ok,VV} -> {ok,VV};
                    {error,undefined} -> {ok,<<"">>}
                    end,

{ok,Previous_Alarm_bit} = case  ht_storage_redis:get_single_field(UpdatedTrack#track.asset_id,alarm_bit) of
                    {ok,LLL} -> {ok,LLL};
                    {error,undefined} -> {ok,Alarm_bit}
                    end,
{ok,Old_Alarm_bit} = case  ht_storage_redis:get_single_field(UpdatedTrack#track.asset_id,old_alarm_bit) of
                    {ok,DDD} -> {ok,DDD};
                    {error,undefined} -> {ok,Alarm_bit}
                    end,

  ht_storage_redis:update_single_field(UpdatedTrack#track.asset_id,old_alarm_bit,Previous_Alarm_bit),
   <<Previous_lock:2/binary,_:3/binary,Pre_Tear:1/binary,_/binary>> = Previous_Alarm_bit, 
         case Previous_lock of
           <<"00">> ->
              case Current_lock of
               <<"10">> ->
                  ht_storage_redis:update_single_field(AssetId,lock_resend,0),
                  send_lock_cmd(State);
               _ -> ok
              end;
            _ -> ok 
           end,   

      Track1 = UpdatedTrack#track{vehicleNo = VehicleNo}, 

%                    UpdatedTrack2 = case Track1#track.valid of
%                               true ->  Track1;
%                               false ->
%                                 ht_logger:info("Packet Tower ~w",[AssetId]),
           UpdatedTrack2 =  case ht_storage_redis:get_geo_data(AssetId) of
                            {ok, [LatitudeA,LongitudeA,PacketTime]} ->
                               case check_speed(Track1,PacketTime,LatitudeA,LongitudeA) of
                                 true ->  
                                   Track1#track{latitude = LatitudeA,longitude = LongitudeA};
                                 false -> Track1
                               end; 
                            {error,_} ->
                               ht_logger:info("No Previous Packet Tower ~w",[AssetId]),
                               Track1
                            end,
                %        end,

       UpdatedTrack3 =   case ht_storage_redis:get_lock_data(AssetId) of
            {lock, <<"locked">>,Course} ->
              case UpdatedTrack2#track.lock_status of
                     locked ->
                       UpdatedTrack2#track{course = Course};
                     unlocked ->
                       UpdatedTrack2#track{event_type = lockStsChange,course = Course}         
                    end;
            {lock, <<"unlocked">>,Course} ->
              case UpdatedTrack2#track.lock_status of
                     locked ->
                       UpdatedTrack2#track{event_type = lockStsChange,course = Course};         
                     unlocked ->
                       UpdatedTrack2#track{course = Course}
                  end
            end,     
          UpdatedTrack1 = check_idle_case(Speed/10,AssetId,UpdatedTrack3),
          UT2 =
             case Pre_Tear of
               <<"0">> ->
                 case TearStatus of
                   <<"1">> ->
                    check_wirecut(UpdatedTrack1,Previous_Alarm_bit,Old_Alarm_bit);
                   _  ->
                  UpdatedTrack1
                 end;
               <<"1">> ->
                  case TearStatus of
                    <<"1">> ->
                      check_wirecut1(UpdatedTrack1,Previous_Alarm_bit,Old_Alarm_bit);
                     _ ->
                       UpdatedTrack1
                   end
            end,
%               check_wirecut(UpdatedTrack1,binary:part(UpdatedTrack1#track.alarm_bit,{7,-1})),
              UT1 = update_report_reason(UT2,binary:part(UT2#track.alarm_bit,{7,-1})),
          {ok,BT,RB} = check_batt(UT1#track.asset_id,binary_to_integer(UT1#track.battery_voltage)),
           UT1#track{battery_voltage = BT, remaining_batt = RB}.

check_wirecut1(Track,Pre_AB,_)  ->
   <<Pre_lock:2/binary,_/binary>> = Pre_AB,
   <<Cur_lock:2/binary,_/binary>> = Track#track.alarm_bit,
   Track1 = case Cur_lock of
      <<"01">> ->
          case Pre_lock of
             <<"00">> -> Track#track{lock_status = unlocked};
             <<"10">> -> Track#track{lock_status = unlocked,event_type = tampered,alarm_type = tampered };
             <<"11">> -> Track#track{lock_status = unlocked,event_type = tampered,alarm_type = tampered };
             <<"01">> -> Track#track{lock_status = unlocked}
          end;
      _ ->
        Track
      end.
check_wirecut(Track,Pre_AB,Old_AB)  ->
   <<Pre_lock:2/binary,_:3/binary,Pre_Tear:1/binary,_/binary>> = Pre_AB,
   <<Old_lock:2/binary,_:3/binary,Old_Tear:1/binary,_/binary>> = Old_AB,
   <<Cur_lock:2/binary,_:3/binary,Cur_Tear:1/binary,Reason:1/binary>> = Track#track.alarm_bit,
   Track1 = case Pre_lock of
      <<"01">> ->
          case Old_lock of
             <<"00">> -> Track#track{lock_status = unlocked,event_type = backCoverOpen };
             <<"10">> -> Track#track{lock_status = unlocked,event_type = tampered,alarm_type = tampered };
             <<"11">> -> Track#track{lock_status = unlocked,event_type = tampered,alarm_type = tampered };
             <<"01">> -> Track#track{lock_status = unlocked,event_type = backCoverOpen }
          end;
       <<"11">> ->
          case Cur_lock of
              <<"01">> ->
                    Track#track{lock_status = unlocked,event_type = tampered,alarm_type = tampered};
              <<"00">> ->
                  case Reason of
                   <<"0">> ->
                     Track#track{lock_status = unlocked,event_type = tampered,alarm_type = tampered};
                    <<"2">> ->
                     Track#track{lock_status = unlocked,event_type = tampered,alarm_type = tampered};
                    _ ->
                     Track#track{event_type = backCoverOpen }
                  end;
               _ ->
              Track#track{event_type = backCoverOpen }
           end;
      _ ->
        Track#track{event_type = backCoverOpen}
      end,

     Track2 = case Track#track.event_type =/= <<"NA">> of
       true ->
         case Track1#track.event_type  of
           backCoverOpen ->
             Track1#track{event_type = Track#track.event_type};
           _ ->
            Track1
         end;
      false ->
        Track1
     end.
