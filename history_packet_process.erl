
-module( history_packet_process).
-behaviour(gen_server).


-export([ start_link/2, init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).


-record( state, { topology,
		  device_id
		}).

-define( NEXT_WAKEUP_INTERVAL, 5000).


%%=====================================================================

-spec( start_link() -> {ok, Pid :: pid()} | ignore | {error, Error :: term()} ).

start_link(Topology, Device_id) ->
	gen_server:start_link({local,?MODULE}, ?MODULE, [Topology, Device_id], []).


%% ====================================================================
%% Behavioural functions
%% ====================================================================

%% init/1
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/gen_server.html#Module:init-1">gen_server:init/1</a>
%%
%% Function set the gen_server State variable used by periodic monitoring of psu and
%% buzzer alarms.
%%
-spec init(Args :: term()) -> Result when
	Result :: {ok, State}
			| {ok, State, Timeout}
			| {ok, State, hibernate}
			| {stop, Reason :: term()}
			| ignore,
	State :: term(),
	Timeout :: non_neg_integer() | infinity.
%% ====================================================================
init([Topology, Device_id]) ->

	self() ! tick,

	{ ok, #state { topology = Topology, device_id = Device_id } }.


%% handle_call/3
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/gen_server.html#Module:handle_call-3">gen_server:handle_call/3</a>
-spec handle_call(Request :: term(), From :: {pid(), Tag :: term()}, State :: term()) -> Result when
	Result :: {reply, Reply, NewState}
			| {reply, Reply, NewState, Timeout}
			| {reply, Reply, NewState, hibernate}
			| {noreply, NewState}
			| {noreply, NewState, Timeout}
			| {noreply, NewState, hibernate}
			| {stop, Reason, Reply, NewState}
			| {stop, Reason, NewState},
	Reply :: term(),
	NewState :: term(),
	Timeout :: non_neg_integer() | infinity,
	Reason :: term().
%% ====================================================================
handle_call(_Request, _From, State) ->
	
	Reply = ok,
	{reply, Reply, State}.


%% handle_cast/2
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/gen_server.html#Module:handle_cast-2">gen_server:handle_cast/2</a>
-spec handle_cast(Request :: term(), State :: term()) -> Result when
	Result :: {noreply, NewState}
			| {noreply, NewState, Timeout}
			| {noreply, NewState, hibernate}
			| {stop, Reason :: term(), NewState},
	NewState :: term(),
	Timeout :: non_neg_integer() | infinity.
%% ====================================================================
handle_cast(_Msg, State) ->
	{noreply, State}.


%% handle_info/2
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/gen_server.html#Module:handle_info-2">gen_server:handle_info/2</a>
%%
%%
-spec handle_info(Info :: timeout | term(), State :: term()) -> Result when
	Result :: {noreply, NewState}
			| {noreply, NewState, Timeout}
			| {noreply, NewState, hibernate}
			| {stop, Reason :: term(), NewState},
	NewState :: term(),
	Timeout :: non_neg_integer() | infinity.
%% ====================================================================
handle_info(tick, State) ->

	erlang:send_after( ?NEXT_WAVEUP_INTERVAL, self(), tick ),

	history_packet_recovery( State#state.topology, State#state.device_id),

	{noreply, State};

handle_info(_Info, State) ->
	{noreply, State}.


%% terminate/2
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/gen_server.html#Module:terminate-2">gen_server:terminate/2</a>
-spec terminate(Reason, State :: term()) -> Any :: term() when
	Reason :: normal
			| shutdown
			| {shutdown, term()}
			| term().
%% ====================================================================
terminate(_Reason, _State) ->
	ok.


%% code_change/3
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/gen_server.html#Module:code_change-3">gen_server:code_change/3</a>
-spec code_change(OldVsn, State :: term(), Extra :: term()) -> Result when
	Result :: {ok, NewState :: term()} | {error, Reason :: term()},
	OldVsn :: Vsn | {down, Vsn},
	Vsn :: term().
%% ====================================================================
code_change(_OldVsn, State, _Extra) ->
	{ok, State}.



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

history_packet_recovery( Topology, Device_id ) ->

	{PacketTime, Device_name} = get_history_packet_time_and_device_name( Topology, Device_id),

	Map = get_two_live_packet_details( Topology, Device_id, PacketTime),

   	Record = convert_record(Map),
  	FirstLive = lists:nth(1,Record),
  	LivePacketTime = FirstLive#track.packet_time,

	Cur = get_history_packets( Topology, Device_id, LivePacketTime),

	erlang:apply( erlang:binary_to_atom(Device_name), recovery_history_packet, [[ Topology, lists:nth(1, Record), lists:nth(2, Record), Cur]]).

convert_record(Map) ->
convert_record(Map,[]).
convert_record([],Track) -> Track;
convert_record([H|T],Track) ->
   NT = from_map(H),
   convert_record(T,Track++[NT]).


get_two_live_packets_detals( Topology, Device_id, PacketTime) ->
	
	 Key = #{<<"$query">> => #{<<"asset_id">> => Device_id,<<"packet_type">> => <<"live">>,<<"packet_time">> => #{<<"$lt">> => PacketTime}},<<"$orderby">> => #{<<"packet_time">> => -1}},

	Projector = #{projector => #{<<"fixtime">> => true,<<"created">> => true,<<"speed">> => true,<<"idle_time">> => true,<<"latitude">> => true,<<"longitude">> => true,<<"battery_voltage">> => true,<<"course">> => true,<<"packet_type">> => true,<<"packet_time">> => true,<<"alarm_bit">> => true,<<"lock_status">> => true}},
  	{ok, Cur} = mc_worker_api:find(Topology,<<"track">>, Key, Projector),
  	Map = mc_cursor:take(Cur,2),
  	mc_cursor:close(Cur),
	Map.


get_history_packet_time_and_device_name( Topology, Device_id) ->

	Key = #{<<"$query">> => #{<<"asset_id">> => Device_id, <<"packet_type">> => <<"history">>,<<"packet_time">> => #{<<"$gte">> => new Date().getTime() - (5*60*1000)}},<<"$orderby">> => #{<<"packet_time">> => -1}},
  	Projector = #{projector => #{<<"_id">> => false,<<"packet_time">> => true, <<"device_name">> => true}},
  	{ok, Cur} = mc_worker_api:find(Topology,<<"track">>, Key,Projector),
  	[#{<<"packet_time">>:= PacketTime, <<"device_name">> := DeviceName}] = mc_cursor:take(Cur,1),
  	mc_cursor:close(Cur),
	{PacketTime, DeviceName}.

get_history_packets( Topology, Device_id, PacketTime) ->
	Key = #{<<"$query">> => #{<<"asset_id">> => Device_id,<<"packet_type">> => <<"history">>,<<"packet_time">> => #{<<"$gte">> => PacketTime}},<<"$orderby">> => #{<<"packet_time">> => 1}},
  	{ok, Cur} = mc_worker_api:find(Topology,<<"track">>, Key),
	Cur.

from_map(Map) ->
  #track{
    latitude = maps:get(<<"latitude">>, Map, 0.0),
    longitude = maps:get(<<"longitude">>, Map, 0.0),
    course = maps:get(<<"course">>, Map, 0.0),
    speed = maps:get(<<"speed">>, Map, 0.0),
    ignition_status = maps:get(<<"ignition_status">>, Map, 0.0),
    altitude = maps:get(<<"altitude">>, Map, 0.0),
    mcc = maps:get(<<"mcc">>, Map, <<"">>),
    lac = maps:get(<<"lac">>, Map, <<"">>),
    gsm = maps:get(<<"gsm">>, Map, <<"">>),
    gps = maps:get(<<"gps">>, Map, <<"">>),
    battery_voltage = maps:get(<<"battery_voltage">>, Map, <<"">>),
    satellite_count = maps:get(<<"satellite_count">>, Map, <<"">>),
    packet_type = maps:get(<<"packet_type">>, Map, <<"">>),
    device_name = maps:get(<<"device_name">>, Map, <<"">>),
    geo_type = maps:get( <<"geo_type">>, Map, <<"">>),
    geo_name = maps:get( <<"geo_name">>, Map, <<"">>),
    idle_state = maps:get( <<"idle_state">>, Map, <<"">>),
    idle_time = maps:get( <<"idle_time">>, Map, 0),
    event_type = maps:get( <<"event_type">>, Map, <<"">>),
    fixTime = timestamp_to_seconds(maps:get(<<"fixTime">>, Map, null)), 
    created = timestamp_to_seconds(maps:get(<<"created">>, Map, null)),
    packet_time = timestamp_to_seconds(maps:get(<<"packet_time">>, Map, null)),
    socket_ip = maps:get(<<"socket_ip">>, Map, <<"">>),
    socket_port = maps:get(<<"socket_port">>, Map, <<"">>),
    asset_id =  maps:get(<<"asset_id">>, Map, <<"">>),
    lock_status = maps:get(<<"lock_status">>,Map, <<"">>),
    unlock_report = maps:get(<<"unlock_report">>,Map, 0),
    alarm_bit = maps:get(<<"alarm_bit">>,Map, <<"">>),
    low_battery_alert = maps:get(<<"low_battery_alert">>,Map, false),
    alarm_report = maps:get(<<"alarm_report">>,Map, <<"">>),
    alarm_type = maps:get(<<"alarm_type">>,Map, <<"">>),
    status_report = maps:get(<<"status_report">>,Map, <<"">>),
    status_type = maps:get(<<"status_type">>,Map, <<"">>)}.


