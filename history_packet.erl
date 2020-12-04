-module(history_packet).


-export([check_and_correct_history_packets/0]).


check_and_correct_history_packets() ->

    application:ensure_all_started (mongodb),
    
    {ok, Topology} = mc_worker_api:connect([{host, "15.206.204.73"},
        {port, 27017},
        {database, <<"test_lynktrac">>},
        {login,"testLynktrac" },
        {password, "*~lynkTracTest@LynkItW39#"},
        {w_mode, safe}
	]),

	case get_the_device_info_from_database( Topology ) of
		{ok, Device_info} ->
			Fun = fun(Device_id) ->
				rpc:call(node(), history_packet_process, start,[Topology, Device_id])
			      end,
			lists:foreach(Fun, Device_info);
		Error ->
			io:format("mongo db error : ~p~n",[Error])
	end,

	ok.

get_the_device_info_from_database( Topology ) ->

  Key = #{<<"packet_time">> => #{<<"$gte">> => new Date().getTime()-(5*60*1000)}},
  %{ok, Device_info} = mc_worker_api:distinct(Topology,<<"track">>, <<"asset_id">>,Key).  
  {true, DeviceIds } =mc_worker_api:command({"distinct" : "track", "key" : "asset_id", "query" : { "packet_time" : {"$gte" : new Date().getTime()-(5*60*1000)}} }),
  DeviceIds.
