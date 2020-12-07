-module(history_packet).

-include("ht_hardware.hrl").
-include("ht_records.hrl").

-export([check_and_correct_history_packets/0]).

-spec( check_and_correct_history_packets() -> ok ).

check_and_correct_history_packets() ->

    application:ensure_all_started (mongodb),
    application:start(bson), 
   
    {ok, Topology} = mc_worker_api:connect([{host, "15.206.204.73"},
        {port, 27017},
        {database, <<"test_lynktrac">>},
        {login,"testLynktrac" },
        {password, "*~lynkTracTest@LynkItW39#"},
        {w_mode, safe}
	]),

	case get_the_device_info_from_database( Topology ) of
		{true, Device_info} ->
			io:format("in main func deivce id ~p~n",[Device_info]),
			Fun = fun(Device_id) ->
				rpc:call(node(), history_packet_process, start,[Topology, Device_id])
			      end,
			lists:foreach(Fun, Device_info);
		Error ->
			io:format("mongo db error : ~p~n",[Error])
	end,

	ok.

-spec( get_the_device_info_from_database( term() ) -> list() | ignore | {error, Error :: term()} ).

get_the_device_info_from_database( Topology ) ->

 % Key = #{<<"packet_time">> => #{<<"$gte">> => new Date().getTime()-(5*60*1000)}},
  Seconds = (calendar:datetime_to_gregorian_seconds(calendar:universal_time()) - (5*60*1000)),
  %{ok, Device_info} = mc_worker_api:distinct(Topology,<<"track">>, <<"asset_id">>,Key).  
  Time =  bson:secs_to_unixtime(Seconds),
  io:format("Time from bson ~p~n",[Time]),
  {true,DeviceIds}  = mc_worker_api:command( Topology, #{<<"distinct">> => <<"track">>, <<"key">> => <<"asset_id">>, <<"query">> => #{ <<"created">> => #{ <<"$gte">> => bson:secs_to_unixtime(Seconds)}} }),
  io:format("device id map from db :~p~n",[DeviceIds]),
  {true, maps:get(<<"values">>,DeviceIds)}.
