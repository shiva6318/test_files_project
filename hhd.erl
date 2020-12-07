-module(hhd).

-export([recovery_history_packet/4]).
-include("ht_hardware.hrl").
-include("ht_records.hrl").

-spec( recovery_history_packet( term(), term(), term(), term()) -> {ok, Pid :: pid()} | ignore | {error, Error :: term()} ).

recovery_history_packet(Topology,LiveLatest,LiveBefore,Cur) ->

  case mc_cursor:next(Cur) of
	  {} ->
		mc_cursor:close(Cur);
	  error ->
		mc_cursor:close(Cur);
	 FirstHistory -> 
  		HistoryRecord = history_packet_process:from_map(FirstHistory),
  		UpdatedHistoryRecord = hhd_history_packet_recovery(LiveLatest,LiveBefore,HistoryRecord),
  		update_db(Topology,UpdatedHistoryRecord,HistoryRecord),
  		recovery_history_packet(Topology,HistoryRecord,LiveLatest,Cur)
   end.

hhd_history_packet_recovery(LiveLatest,LiveBefore,Track) ->
   AssetId = Track#track.asset_id,
   Alarm_bit = Track#track.alarm_bit,
   Speed = Track#track.speed,
   <<_Current_lock:2/binary,Battery:3/binary,TearStatus:1/binary,ReportReason:1/binary>> = Alarm_bit,
   <<_Previous_lock:2/binary,Prev_Batt:3,_/binary>> = LiveLatest#track.alarm_bit,
   <<_Before_lock:2/binary,Before_Batt:3,_/binary>> = LiveBefore#track.alarm_bit,

   UpdatedTrack2 = case Track#track.valid of
     true ->  Track;
     false ->
        case check_speed(Track,LiveLatest#track.packet_time,LiveLatest#track.latitude,LiveLatest#track.longitude) of
          true ->
            Track#track{latitude = LiveLatest#track.latitude,longitude = LiveLatest#track.longitude};
          false -> Track
        end
      end,

   UpdatedTrack3 =   case {lock,LiveLatest#track.lock_status,LiveLatest#track.course} of
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
    UpdatedTrack4 = check_wirecut(UpdatedTrack1,TearStatus),
    UT1 = update_report_reason(UpdatedTrack4,ReportReason),
    {ok,BT,RB} = check_batt(Battery,Prev_Batt,Before_Batt),
    UT1#track{battery_voltage = BT, remaining_batt = RB}.

check_speed(#track{latitude = Latitude1, longitude = Longitude1, packet_time = PacketTime2},PacketTime1,Latitude2,Longitude2) when PacketTime2 /= PacketTime1 ->
  Deg2rad = fun(Deg) -> math:pi()*Deg/180 end,
  [RLng1, RLat1, RLng2, RLat2] = [Deg2rad(Deg) || Deg <- [Longitude1,Latitude1,Longitude2,Latitude2]],
  DLon = RLng2 - RLng1,
  DLat = RLat2 - RLat1,
  A = math:pow(math:sin(DLat/2), 2) + math:cos(RLat1) * math:cos(RLat2) * math:pow(math:sin(DLon/2), 2),
  C = 2 * math:asin(math:sqrt(A)),
  Km = 6372.8 * C,
  Speed = (Km * 3600)/abs(PacketTime2 - PacketTime1),
  case Speed > 140.0 of
       true -> true;
       false -> false
  end;
check_speed(_,_,_,_) -> false.

check_idle_case(0.0,AssetId,TrackModel) ->
  case ht_storage_redis:get_idle_data(AssetId) of
    {idle_state,[Latitude,Longitude,IdleTime,PacketTime]} ->
      check_distance(TrackModel,IdleTime,PacketTime,Latitude,Longitude);
    {moving_state,[_Latitude,_Longitude,_MovingTime,PacketTime]} ->
      TrackModel#track{idle_state = true,idle_time = (TrackModel#track.packet_time - PacketTime)}
  end;

check_idle_case(0,AssetId,TrackModel) ->
  case ht_storage_redis:get_idle_data(AssetId) of
    {idle_state,[Latitude,Longitude,IdleTime,PacketTime]} ->
      check_distance(TrackModel,IdleTime,PacketTime,Latitude,Longitude);
    {moving_state,[_Latitude,_Longitude,_MovingTime,PacketTime]} ->
      TrackModel#track{idle_state = true,idle_time = (TrackModel#track.packet_time - PacketTime)}
  end;
check_idle_case(_Speed,AssetId,TrackModel) ->
  case ht_storage_redis:get_idle_data(AssetId) of
    {idle_state,[_Latitude,_Longitude,_MovingTime,PacketTime]} ->
           TrackModel#track{idle_state = false,idle_time = (TrackModel#track.packet_time - PacketTime)};
    {moving_state,[_Latitude,_Longitude,MovingTime,PacketTime]} ->
           TrackModel#track{idle_state = false,idle_time = (TrackModel#track.packet_time - PacketTime) + MovingTime}
  end.


check_wirecut(Track, Bit) when Bit =:= <<"2">> ->
	ht_storage_mongodb:update_order(Track#track.asset_id,wirecut),
  	Track;
check_wirecut( Track, _) ->
      Track.

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

check_batt(Battery,Batt2,Batt1) ->
  case bat_check([binary_to_integer(Batt2),binary_to_integer(Batt1)],binary_to_integer(Battery),0) of
    true ->
      {ok,float_to_binary(bat_per(binary_to_integer(Battery)),[{decimals,2}]),integer_to_binary(get_remaining_batt(binary_to_integer(Battery)))};
    false ->
      {ok,float_to_binary(bat_per(binary_to_integer(Batt1)),[{decimals,2}]),integer_to_binary(get_remaining_batt(binary_to_integer(Battery)))}
  end.

bat_check([],_Batt,Count) when Count >= 2 ->
   true;
bat_check([],_Batt,_Count) ->
   false;
bat_check([BattH|BattT],Batt,Count) when Batt =:= BattH  ->
   bat_check(BattT,Batt,Count+1);
bat_check([_BattH|BattT],Batt,Count) ->
   bat_check(BattT,Batt,Count).

get_remaining_batt(Batt) when Batt > 417 ->
  206;
get_remaining_batt(Batt) when Batt < 341 ->
  0;
get_remaining_batt(Batt) ->
  {ok,{_,Hours}} = ht_manager_firmware:get_by_id(Batt),
  Hours.


bat_per(Vol) when Vol >= 420 ->
   100.0;
bat_per(Vol) when Vol < 420, Vol >= 406 ->
   90 + (10 * ( Vol - 406 ) / (420 - 406));
bat_per(Vol) when Vol < 406, Vol >= 398 ->
   80 + (10 * ( Vol - 398 ) / ( 406 - 398));
bat_per(Vol) when Vol < 398, Vol >= 392 ->
   70 + (10 * ( Vol - 392 ) / ( 398 - 392 ));
bat_per(Vol) when Vol < 392, Vol >= 387 ->
   60 + (10 * ( Vol - 387 ) / ( 392 - 387 ));
bat_per(Vol) when Vol < 387, Vol >= 382 ->
   50 + (10 * ( Vol - 382) / ( 387 - 382 ));
bat_per(Vol) when Vol < 382, Vol >= 379 ->
   40 + (10 * ( Vol - 379) / ( 382 - 379));
bat_per(Vol) when Vol < 379, Vol >= 377 ->
   30 + (10 * ( Vol - 377 ) / ( 379 - 377));
bat_per(Vol) when Vol < 377, Vol >= 374 ->
   20 + (10 * ( Vol - 374 ) / ( 377 - 374 ));
bat_per(Vol) when Vol < 374, Vol >= 368 ->
   10 + (10 * ( Vol - 368 ) / ( 374 - 368));
bat_per(Vol) when Vol < 368, Vol >= 345 ->
   5 + (5 * ( Vol - 345 ) / ( 368 - 345 ));
bat_per(Vol) when Vol < 345, Vol >= 300 ->
       5 * ( Vol - 300 ) / ( 345 - 300);
bat_per(Vol) when Vol =< 300 ->
    0.0;
bat_per(_) ->
    0.0.

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
      Track#track{latitude = Latitude2,longitude = Longitude2,speed = 0,idle_state = true,idle_time = NewIdleTime}
  end.

validate_distance(Distance) when  Distance > 0.033 -> new_idle;
validate_distance(Distance) when  Distance =< 0.033 -> same_idle.


timestamp_to_seconds(Timestamp) ->
    bson:unixtime_to_secs(Timestamp).


update_db(Topology,UpdateTrack,ExistingTrack) ->
  List = compare_record(UpdateTrack,ExistingTrack),
  Map = maps:from_list(List),
  mongo_api:update(Topology,<<"track">>,#{<<"tripStatus">> => true}, #{<<"$set">> => Map}, #{}).

compare_record(UpdateTrack,ExistingTrack) ->
  FieldList = record_info(fields, track),
  [_RecordName|UpdatedList] = tuple_to_list(UpdateTrack),
  [_RecordName|ExistingList] = tuple_to_list(ExistingTrack),
  compare_list(FieldList,UpdatedList,ExistingList,[]).
compare_list([],[],[],DiffList) -> DiffList;
compare_list([FH|FT],[UH|UT],[EH|ET],DiffList) ->
	New = case UH == EH of
		true -> [];
 		false -> [{FH,UH}]
 	     end, 
	compare_list(FT,UT,ET,New++DiffList).
        


        
