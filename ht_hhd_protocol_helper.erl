%%%-------------------------------------------------------------------
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

-module(ht_hhd_protocol_helper).
-author("Manas Parganiha <manas@lynkit.in>").

-include("ht_hardware.hrl").
-include("ht_records.hrl").

%% API
-export([
        hhd_event_adder/2
]).

-spec hhd_event_adder(State :: #state{},UpdatedTrack :: #track{}) -> #track{}.
hhd_event_adder(State,UpdatedTrack) ->
   AssetId = UpdatedTrack#track.asset_id,
   Alarm_bit = UpdatedTrack#track.alarm_bit,
   Speed = UpdatedTrack#track.speed,
  <<Current_lock:2/binary,_/binary>> = Alarm_bit,
   <<LockStatus:1/binary,SealStatus:1/binary,Battery:3/binary,TearStatus:1/binary,ReportReason:1/binary>> = Alarm_bit,

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
                  send_lock_cmd(State);
               _ -> ok
              end;
            _ -> ok 
           end,   
 
      Track1 = case Previous_lock of
           <<"11">> ->
              case Current_lock of
               <<"10">> ->
                 UpdatedTrack#track{event_type = lockDisengaged,lock_status = lockDisengaged};      
               _ -> 
                 UpdatedTrack
              end;
            _ -> 
              UpdatedTrack
           end,



                 %  UpdatedTrack2 = case Track1#track.valid of
                 %              true ->  Track1;
                 %              false ->
                 %                ht_logger:info("Packet Tower ~w",[AssetId]),
         UpdatedTrack2 =  case ht_storage_redis:get_geo_data(AssetId) of
               {ok, [LatitudeA,LongitudeA,PacketTime]} ->
                  case check_speed(UpdatedTrack,PacketTime,LatitudeA,LongitudeA) of
                    true ->  
                  UpdatedTrack#track{latitude = LatitudeA,longitude = LongitudeA};
                    false -> UpdatedTrack
                  end; 
               {error,_} ->
                  ht_logger:info("No Previous Packet Tower ~w",[AssetId]),
                  UpdatedTrack
 %               end
           end,
              Lock_course_status  =  ht_storage_redis:get_lock_data(AssetId),
              {lock,Pre_lock_status,_} = Lock_course_status, 
              UpdatedTrack4 = case Current_lock of
                     <<"01">> ->
                         UpdatedTrack2#track{lock_status = Pre_lock_status};
                       _ ->
                         UpdatedTrack2
               end,  
               UpdatedTrack3 = case Lock_course_status of
                    {lock, <<"lockDisengaged">>,Course} ->
                      case UpdatedTrack4#track.lock_status of
                             lockDisengaged ->
                               UpdatedTrack4#track{course = Course,lock_status = lockDisengaged};
                             locked ->
                               UpdatedTrack4#track{course = Course,lock_status = locked};
                             unlocked ->
                               UpdatedTrack4#track{event_type = lockStsChange,course = Course};
                             <<"lockDisengaged">> ->
                               UpdatedTrack4#track{course = Course,lock_status = lockDisengaged};
                             <<"locked">> ->
                               UpdatedTrack4#track{course = Course,lock_status = locked};
                             <<"unlocked">> ->
                               UpdatedTrack4#track{event_type = lockStsChange,course = Course}
                      end;
                    {lock, <<"locked">>,Course} ->
                      case UpdatedTrack4#track.lock_status of
                             lockDisengaged -> 
                               UpdatedTrack4#track{course = Course,lock_status = lockDisengaged};
                             locked ->
                               UpdatedTrack4#track{course = Course};
                             unlocked ->
                               UpdatedTrack4#track{event_type = lockStsChange,course = Course};
                             <<"lockDisengaged">> -> 
                               UpdatedTrack4#track{course = Course,lock_status = lockDisengaged};
                             <<"locked">> ->
                               UpdatedTrack4#track{course = Course};
                             <<"unlocked">> ->
                               UpdatedTrack4#track{event_type = lockStsChange,course = Course}      
                            end;
                    {lock, <<"unlocked">>,Course} ->
                      case UpdatedTrack4#track.lock_status of
                             lockDisengaged ->
                               UpdatedTrack4#track{course = Course,lock_status = lockDisengaged};
                             locked ->
                               UpdatedTrack4#track{event_type = lockStsChange,course = Course};
                             unlocked ->
                               UpdatedTrack4#track{course = Course};
                             <<"lockDisengaged">> ->
                               UpdatedTrack4#track{course = Course,lock_status = lockDisengaged};
                             <<"locked">> ->
                               UpdatedTrack4#track{event_type = lockStsChange,course = Course};
                             <<"unlocked">> ->
                               UpdatedTrack4#track{course = Course}
                          end
                    end,     
                  UpdatedTrack1 = check_idle_case(Speed,AssetId,UpdatedTrack3),
                     
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
                   UT1 = update_report_reason(UT2,binary:part(UT2#track.alarm_bit,{7,-1})),
                  {ok,BT,RB} = check_batt(UT1#track.asset_id,binary_to_integer(UT1#track.battery_voltage)),
                   UT1#track{battery_voltage = BT, remaining_batt = RB}.


send_lock_cmd(#state{transport = Transport, socket = Socket,assetId = DeviceId}) ->
    Id = integer_to_list(DeviceId),
    TerminalId = list_to_binary(hexstr_to_list("0"++Id)),
    Body = <<16#01,16#24,16#01,16#01>>,
    Header = <<16#03,16#10,16#00,16#04,TerminalId/binary,16#00,16#0C>>,
    Crc = ht_checksum:crc(<<Header/binary,Body/binary>>),
  Packet = <<
    Header/binary,
    Body/binary,
    Crc:8/unsigned-integer
  >>,
  P1 = check_packet(Packet),
  ht_logger:info("Terminal id for LOCKED: ~s, ~s", [parse_hex(TerminalId),parse_hex(P1)]),
  Transport:send(Socket, <<16#7E,P1/binary,16#7E>>).

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
  case Speed > 140.0 of
       true -> true;
       false -> false
  end;
check_speed(_,_,_,_) -> false.

check_idle_case(0.0,AssetId,TrackModel) ->
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
           TrackModel#track{idle_state = false,idle_time = (TrackModel#track.packet_time - PacketTime)};
    {moving_state,[Latitude,Longitude,MovingTime,PacketTime]} ->
           TrackModel#track{idle_state = false,idle_time = (TrackModel#track.packet_time - PacketTime) + MovingTime}
  end.
check_wirecut1(Track,Pre_AB,_)  ->
   <<Pre_lock:2/binary,_/binary>> = Pre_AB,
   <<Cur_lock:2/binary,_/binary>> = Track#track.alarm_bit,
   Track1 = case Cur_lock of
      <<"01">> ->
          case Pre_lock of
             <<"00">> -> Track#track{lock_status = unlocked};
             <<"10">> -> Track#track{lock_status = unlocked,event_type = tampered };  
             <<"11">> -> Track#track{lock_status = unlocked,event_type = tampered };  
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
             <<"10">> -> Track#track{lock_status = unlocked,event_type = tampered };  
             <<"11">> -> Track#track{lock_status = unlocked,event_type = tampered };  
             <<"01">> -> Track#track{lock_status = unlocked,event_type = backCoverOpen }
          end;
       <<"11">> ->
          case Cur_lock of
              <<"01">> -> 
                    Track#track{lock_status = unlocked,event_type = tampered};
              <<"00">> ->
                  case Reason of
                   <<"0">> ->
                     Track#track{lock_status = unlocked,event_type = tampered};
                    <<"2">> ->
                     Track#track{lock_status = unlocked,event_type = tampered};
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
                  case bat_check([binary_to_integer(Batt3),binary_to_integer(Batt2),binary_to_integer(Batt1)],Battery,0) of
                    true ->
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

check_pattern(Data) ->
  A = case re:run(Data,<<125,1>>) of
     nomatch ->
        Data;
     {match,_} ->
       re:replace(Data,<<125,1>>,<<"XXXX">>,[global,{return,binary}])
     end,
  
  B =  case re:run(A,<<125,2>>) of
     nomatch ->
        A;
     {match,_} ->
        re:replace(A,<<125,2>>,<<126>>,[global,{return,binary}])
     end,
  case re:run(B,<<"XXXX">>) of
     nomatch ->
        B;
     {match,_} ->
        re:replace(B,<<"XXXX">>,<<125>>,[global,{return,binary}])
     end.


check_packet(Data) ->
  A =  case re:run(Data,<<16#7D>>) of
     nomatch ->
       Data;
     {match,_} ->
       re:replace(Data,<<16#7D>>,<<125,1>>,[global,{return,binary}])
     end,

    case re:run(A,<<16#7E>>) of
     nomatch ->
        A;
     {match,_} ->
        re:replace(A,<<16#7E>>,<<125,2>>,[global,{return,binary}])
     end.

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
 
get_remaining_batt(Batt) when Batt > 415 ->
  142;
get_remaining_batt(Batt) when Batt < 350 ->
  0;
get_remaining_batt(Batt) ->
  {ok,{_,Hours}} = ht_manager_firmware:get_by_id(Batt),
  Hours.


parse_hex(Bin) ->
  list_to_binary(lists:flatten([io_lib:format("~2.16.0B", [X]) || X <- binary_to_list(Bin)])).
