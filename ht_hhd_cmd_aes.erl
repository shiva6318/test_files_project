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
-module(ht_hhd_cmd_aes).
-author("Manas Parganiha <manas@lynkit.in>").

-include("ht_hardware.hrl").
-include("ht_records.hrl").

-export([fw_update/5,do_encode_command/1,cmd_check_response/4,query_check_response/4]).

cmd_check_response(_State,0,_Rest,_AssetId) -> ok;
cmd_check_response(State,1,<<16#39,Res1:1/binary>>,AssetId) when Res1 == <<16#00>> ->
    case ht_storage_redis:get_single_config_info_hhd(AssetId,reqBtAesPassword) of
      {ok,Value2} ->
           Key = [cmd_receive,"SET_BT_AES_PASSWORD",cmd_receive_time,ht_helper_time:timestamp(),conBtAesPassword,Value2],
           ht_storage_redis:mupdate_config_info_hhd(AssetId,Key);
       _ -> ok
    end;
cmd_check_response(State,1,<<16#39,_/binary>>,AssetId) -> ok;
cmd_check_response(State,1,<<16#27,Res1:1/binary>>,AssetId) when Res1 == <<16#00>> ->
    case ht_storage_redis:get_single_config_info_hhd(AssetId,reqShutdownTime) of
      {ok,Value2} ->
           Key = [cmd_receive,"SET_SHUTDOWN_TIME",cmd_receive_time,ht_helper_time:timestamp(),conShutdownTime,Value2],
           ht_storage_redis:mupdate_config_info_hhd(AssetId,Key);
       _ -> ok
    end;
cmd_check_response(State,1,<<16#27,_/binary>>,AssetId) -> ok;
cmd_check_response(State,1,<<16#24,Res1:1/binary>>,AssetId) when Res1 == <<16#00>> ->
    case ht_storage_redis:get_single_config_info_hhd(AssetId,cmd_send) of
      {ok,Value2} ->
           Key = [cmd_receive,Value2,cmd_receive_time,ht_helper_time:timestamp()],
           ht_storage_redis:mupdate_config_info_hhd(AssetId,Key);
       _ -> ok
    end;
cmd_check_response(State,1,<<16#24,_/binary>>,AssetId) -> ok;
cmd_check_response(State,1,<<16#23,Res1:1/binary>>,AssetId) when Res1 == <<16#00>> ->
    case ht_storage_redis:get_single_config_info_hhd(AssetId,cmd_send) of
      {ok,Value2} ->
           Key = [cmd_receive,Value2,cmd_receive_time,ht_helper_time:timestamp()],
           ht_storage_redis:mupdate_config_info_hhd(AssetId,Key);
       _ -> ok
    end;
cmd_check_response(State,1,<<16#23,_/binary>>,AssetId) -> ok;
cmd_check_response(State,1,<<16#13,Res1:1/binary>>,AssetId) when Res1 == <<16#00>> ->
    case ht_storage_redis:get_single_config_info_hhd(AssetId,reqSmsPassword) of
      {ok,Value2} ->
           Key = [cmd_receive,"SET_SMS_PASSWORD",cmd_receive_time,ht_helper_time:timestamp(),conSmsPassword,Value2],
           ht_storage_redis:mupdate_config_info_hhd(AssetId,Key);
       _ -> ok
    end;
cmd_check_response(State,1,<<16#13,_/binary>>,AssetId) -> ok;
cmd_check_response(State,1,<<16#15,Res1:1/binary>>,AssetId) when Res1 == <<16#00>> ->
    case ht_storage_redis:get_single_config_info_hhd(AssetId,reqSetTimerReset) of
      {ok,Value2} ->
           Key = [cmd_receive,"SET_TIMER_RESET",cmd_receive_time,ht_helper_time:timestamp(),conSetTimerReset,Value2],
           ht_storage_redis:mupdate_config_info_hhd(AssetId,Key);
       _ -> ok
    end;
cmd_check_response(State,1,<<16#15,_/binary>>,AssetId) -> ok;
cmd_check_response(State,1,<<16#0C,Res1:1/binary>>,AssetId) when Res1 == <<16#00>> ->
    case ht_storage_redis:get_single_config_info_hhd(AssetId,reqSim2ApnName) of
      {ok,Value2} ->
           Key = [cmd_receive,"SET_SIM2_APN_NAME",cmd_receive_time,ht_helper_time:timestamp(),conSim2ApnName,Value2],
           ht_storage_redis:mupdate_config_info_hhd(AssetId,Key);
       _ -> ok
    end;
cmd_check_response(State,1,<<16#0C,_/binary>>,AssetId) -> ok;
cmd_check_response(State,1,<<16#09,Res1:1/binary>>,AssetId) when Res1 == <<16#00>> ->
    case ht_storage_redis:get_single_config_info_hhd(AssetId,reqSim1ApnName) of
      {ok,Value2} ->
           Key = [cmd_receive,"SET_SIM1_APN_NAME",cmd_receive_time,ht_helper_time:timestamp(),conSim1ApnName,Value2],
           ht_storage_redis:mupdate_config_info_hhd(AssetId,Key);
       _ -> ok
    end;
cmd_check_response(State,1,<<16#09,_/binary>>,AssetId) -> ok;
cmd_check_response(State,1,<<16#05,Res1:1/binary>>,AssetId) when Res1 == <<16#00>> ->
    case ht_storage_redis:get_single_config_info_hhd(AssetId,reqServerPort) of
      {ok,Value2} ->
           Key = [cmd_receive,"SET_SERVER_PORT",cmd_receive_time,ht_helper_time:timestamp(),conServerPort,Value2],
           ht_storage_redis:mupdate_config_info_hhd(AssetId,Key);
       _ -> ok
    end;
cmd_check_response(State,1,<<16#05,_/binary>>,AssetId) -> ok;
cmd_check_response(State,1,<<16#03,Res1:1/binary>>,AssetId) when Res1 == <<16#00>> ->
    case ht_storage_redis:get_single_config_info_hhd(AssetId,reqServerIp) of
      {ok,Value2} ->
           Key = [cmd_receive,"SET_SERVER_IP",cmd_receive_time,ht_helper_time:timestamp(),conServerIp,Value2],
           ht_storage_redis:mupdate_config_info_hhd(AssetId,Key);
       _ -> ok
    end;
cmd_check_response(State,1,<<16#03,_/binary>>,AssetId) -> ok;
cmd_check_response(State,3,<<16#02,Res1:1/binary,16#06,Res2:1/binary,16#08,Res3:1/binary>>,AssetId) when Res1 == <<16#00>>, Res2 == <<16#00>>,Res3 == <<16#00>>  ->
   case ht_storage_redis:get_single_config_info_hhd(AssetId,reqSleepTime) of
    {ok,Value1} ->
        case ht_storage_redis:get_single_config_info_hhd(AssetId,reqWakeupTime) of
          {ok,Value2} ->
               Key = [cmd_receive,"SET_PACKET_INTERVAL",cmd_receive_time,ht_helper_time:timestamp(),conSleepTime,Value1,conWakeupTime,Value2],
               ht_storage_redis:mupdate_config_info_hhd(AssetId,Key);
           _ -> ok
        end;
     _ -> ok
   end;
cmd_check_response(State,3,<<16#02,_:1/binary,16#06,_:1/binary,16#08,_/binary>>,AssetId) -> ok;   
cmd_check_response(State,Count,Rest,AssetId) -> 
  <<Cmd:1/binary,Response:1/binary,Res/binary>> = Rest, 
  cmd_check_response(State,Count-1,Res,AssetId).

query_check_response(_State,0,_Rest,_AssetId) -> ok;
query_check_response(State,1, <<16#37,Len:8/unsigned-integer,Res/binary>>, AssetId) ->
  Zone =  case Res of
     <<16#00>> ->  <<"UTC-12">>;
     <<16#01>> -> <<"UTC-11">>;
     <<16#02>> ->  <<"UTC-10">>;
     <<16#03>> -> <<"UTC-09">>;
     <<16#04>> -> <<"UTC-08">>;
     <<16#05>> -> <<"UTC-07">>;
     <<16#06>> -> <<"UTC-06">>;
     <<16#07>> -> <<"UTC-05">>;
     <<16#08>> -> <<"UTC-04:30">>;
     <<16#09>> -> <<"UTC-04">>;
     <<16#0A>> -> <<"UTC-03:30">>;
     <<16#0B>> -> <<"UTC-03">>;
     <<16#0C>> -> <<"UTC-02">>;
     <<16#0D>> -> <<"UTC-01">>;
     <<16#0E>> -> <<"UTC-00">>;
     <<16#0F>> -> <<"UTC+01">>;
     <<16#10>> -> <<"UTC+02">>;
     <<16#11>> -> <<"UTC+03">>;
     <<16#12>> -> <<"UTC+03:30">>;
     <<16#13>> -> <<"UTC+04">>;
     <<16#14>> -> <<"UTC+04:30">>;
     <<16#15>> -> <<"UTC+05">>;
     <<16#16>> -> <<"UTC+05:30">>;
     <<16#17>> -> <<"UTC+05:45">>;
     <<16#18>> -> <<"UTC+06">>;
     <<16#19>> -> <<"UTC+06:30">>;
     <<16#1A>> -> <<"UTC+07">>;
     <<16#1B>> -> <<"UTC+08">>;
     <<16#1C>> -> <<"UTC+09">>;
     <<16#1D>> -> <<"UTC+09:30">>;
     <<16#1E>> -> <<"UTC+10">>;
     <<16#1F>> -> <<"UTC+11">>;
     <<16#20>> -> <<"UTC+12">>;
     <<16#21>> -> <<"UTC+13">>
    end,
   Key = [cmd_receive,"GET_TIMEZONE",cmd_receive_time,ht_helper_time:timestamp(),conSetTimezone,Zone],
   ht_storage_redis:mupdate_config_info_hhd(AssetId,Key);
query_check_response(State,1, <<16#27,Len:8/unsigned-integer,Res:16/unsigned-integer>>, AssetId) ->
   Key = [cmd_receive,"GET_SHUTDOWN_TIME",cmd_receive_time,ht_helper_time:timestamp(),conShutdownTime,Res],
   ht_storage_redis:mupdate_config_info_hhd(AssetId,Key);
query_check_response(State,1, <<16#13,Len:8/unsigned-integer,Res/binary>>, AssetId) ->
   Key = [cmd_receive,"GET_SMS_PASSWORD",cmd_receive_time,ht_helper_time:timestamp(),conSmsPassword,Res],
   ht_storage_redis:mupdate_config_info_hhd(AssetId,Key);
query_check_response(State,1, <<16#15,Len:8/unsigned-integer,Res/binary>>, AssetId) ->
   Key = [cmd_receive,"GET_TIMER_RESET",cmd_receive_time,ht_helper_time:timestamp(),conSetTimerReset,Res],
   ht_storage_redis:mupdate_config_info_hhd(AssetId,Key);
query_check_response(State,1, <<16#0C,Len:8/unsigned-integer,Res/binary>>, AssetId) ->
   Key = [cmd_receive,"GET_SIM2_APN_NAME",cmd_receive_time,ht_helper_time:timestamp(),conSim2ApnName,Res],
   ht_storage_redis:mupdate_config_info_hhd(AssetId,Key);
query_check_response(State,1, <<16#09,Len:8/unsigned-integer,Res/binary>>, AssetId) ->
   Key = [cmd_receive,"GET_SIM1_APN_NAME",cmd_receive_time,ht_helper_time:timestamp(),conSim1ApnName,Res],
   ht_storage_redis:mupdate_config_info_hhd(AssetId,Key);
query_check_response(State,1, <<16#05,Len:8/unsigned-integer,Res/binary>>, AssetId) ->
   Key = [cmd_receive,"GET_SERVER_PORT",cmd_receive_time,ht_helper_time:timestamp(),conServerPort,Res],
   ht_storage_redis:mupdate_config_info_hhd(AssetId,Key);
query_check_response(State,1, <<16#03,Len:8/unsigned-integer,Res/binary>>, AssetId) ->
   Key = [cmd_receive,"GET_SERVER_IP",cmd_receive_time,ht_helper_time:timestamp(),conServerIp,Res],
   ht_storage_redis:mupdate_config_info_hhd(AssetId,Key);
query_check_response(State,3, <<16#02,16#02,Res/binary>>, AssetId) ->
  <<LongWake:16/unsigned-integer,16#06,16#02,ShortWake:16/unsigned-integer,16#08,16#02,Sleep:16/unsigned-integer>> = Res,
  Value = case LongWake of
            0  ->
              ShortWake;
            Wake ->
              Wake*60
          end,
   Key = [cmd_receive,"GET_PACKET_INTERVAL",cmd_receive_time,ht_helper_time:timestamp(),conSleepTime,Sleep,conWakeupTime,Value],
   ht_storage_redis:mupdate_config_info_hhd(AssetId,Key);

query_check_response(State,1, <<16#3A,Len:8/unsigned-integer,Res/binary>>, AssetId) -> 
   Length = Len-3,
   <<A:Length/binary,Res1/binary>> = Res,
   Key = [cmd_receive,"get_mcu_version",cmd_receive_time,ht_helper_time:timestamp(),conMcuVersion,parse_hex(Res1)],
   ht_storage_redis:mupdate_config_info_hhd(AssetId,Key);

query_check_response(State,1, <<16#3B,Len:8/unsigned-integer,Res/binary>>, AssetId) ->
  Length = Len-3,
  <<A:Length/binary,Res1/binary>> = Res,  
   Key = [cmd_receive,"get_bt_version",cmd_receive_time,ht_helper_time:timestamp(),conBtVersion,parse_hex(Res1)],
   ht_storage_redis:mupdate_config_info_hhd(AssetId,Key);

query_check_response(State,Count, <<Cmd:1/binary,Length:1/binary,Res/binary>>, AssetId) ->
	<<Respose:Length/binary,Rest/binary>> = Res,
  query_check_response(State,Count-1,Rest,AssetId).


do_encode_command(#command{deviceId = DeviceId,type = <<"GET_RFID_CARD">>}) ->
  Key = [cmd_send,"GET_RFID_CARD",cmd_send_time,ht_helper_time:timestamp()],
  ht_storage_redis:mupdate_config_info_hhd(DeviceId,Key),
  Id = integer_to_list(DeviceId),
  TerminalId = list_to_binary(hexstr_to_list("0"++Id)),
  ht_logger:info("Terminal id: ~s", [parse_hex(TerminalId)]),
  Body = <<16#33,16#2A,16#4D,16#30,16#37,16#2C,16#30,16#30,16#2C,16#23>>,
  BB = byte_size(Body) + 6,
  Header = <<16#89,16#00,16#80,BB:8/unsigned-integer,TerminalId/binary,16#00,16#0D>>,
  ht_logger:info("CARD READ: ~s  ~s", [parse_hex(Body),parse_hex(Header)]),
  EncryptBody = get_encryption(Body),
  Crc = ht_checksum:crc(<<Header/binary,EncryptBody/binary>>),

  Packet = <<
    Header/binary,
    EncryptBody/binary,
    Crc:8/unsigned-integer
  >>,
  P1 = check_packet(Packet),
  {ok,<<16#7E,P1/binary,16#7E>>};
do_encode_command(#command{deviceId = DeviceId,type = <<"SET_RFID_CARD">>,attributes = #{<<"RFID_CARD">> := RFID_CARD}}) ->
  Key = [cmd_send,"SET_RFID_CARD",cmd_send_time,ht_helper_time:timestamp(),reqSetRfidCard,RFID_CARD],
  ht_storage_redis:mupdate_config_info_hhd(DeviceId,Key),

    Id = integer_to_list(DeviceId),
    TerminalId = list_to_binary(hexstr_to_list("0"++Id)),
    ht_logger:info("Terminal id: ~s", [parse_hex(TerminalId)]),
    VV = byte_size(RFID_CARD),
    Size = integer_to_binary(VV),
    SS = byte_size(Size),

    Body = <<16#33,16#2A,16#4D,16#30,16#36,16#2C,Size:SS/binary,16#2C,RFID_CARD:VV/binary,16#23>>,
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
do_encode_command(#command{deviceId = DeviceId,type = <<"SET_BT_AES_PASSWORD">>,attributes = #{<<"BT_AES_PASSWORD">> := SMS_PASSWORD}}) ->
  Key = [cmd_send,"SET_BT_AES_PASSWORD",cmd_send_time,ht_helper_time:timestamp(),reqBtAesPassword,SMS_PASSWORD],
  ht_storage_redis:mupdate_config_info_hhd(DeviceId,Key),

    Id = integer_to_list(DeviceId),
    TerminalId = list_to_binary(hexstr_to_list("0"++Id)),
    Size = byte_size(SMS_PASSWORD),
    BB = 16+6+2,
    Body = <<16#01,16#39,16#10,SMS_PASSWORD/binary>>,
    ht_logger:info("Body : ~s", [parse_hex(Body)]),
    Header = <<16#03,16#10,1:1,0:1,0:1,0:1,0:1,0:1,BB:10,TerminalId/binary,16#00,16#0D>>,
    EncryptBody = get_encryption(Body),
    Crc = ht_checksum:crc(<<Header/binary,EncryptBody/binary>>),

  Packet = <<
    Header/binary,
    EncryptBody/binary,
    Crc:8/unsigned-integer
  >>,
  P1 = check_packet(Packet),
  {ok,<<16#7E,P1/binary,16#7E>>};
do_encode_command(#command{deviceId = DeviceId,type = <<"GET_TIMEZONE">>})  ->
  Key = [cmd_send,"GET_TIMEZONE",cmd_send_time,ht_helper_time:timestamp()],
  ht_storage_redis:mupdate_config_info_hhd(DeviceId,Key),
  Id = integer_to_list(DeviceId),
  TerminalId = list_to_binary(hexstr_to_list("0"++Id)),
  ht_logger:info("Terminal id: ~s", [parse_hex(TerminalId)]),
  Body = <<16#01,16#37>>,
  Header = <<16#03,16#12,16#80,16#08,TerminalId/binary,16#00,16#0C>>,
  EncryptBody = get_encryption(Body),
  Crc = ht_checksum:crc(<<Header/binary,EncryptBody/binary>>),

  Packet = <<
    Header/binary,
    EncryptBody/binary,
    Crc:8/unsigned-integer
  >>,
  P1 = check_packet(Packet),
  {ok,<<16#7E,P1/binary,16#7E>>};
do_encode_command(#command{deviceId = DeviceId,type = <<"SET_TIMEZONE">>,attributes = #{<<"TIMEZONE">> := TIMEZONE}}) ->
  Key = [cmd_send,"SET_TIMEZONE",cmd_send_time,ht_helper_time:timestamp(),reqSetTimezone,TIMEZONE],
  ht_storage_redis:mupdate_config_info_hhd(DeviceId,Key),

    Id = integer_to_list(DeviceId),
    TerminalId = list_to_binary(hexstr_to_list("0"++Id)),
    Zone = case TIMEZONE of
      <<"UTC-12">> -> <<16#00>>;
      <<"UTC-11">> -> <<16#01>>;
      <<"UTC-10">> -> <<16#02>>;
      <<"UTC-09">> -> <<16#03>>;
      <<"UTC-08">> -> <<16#04>>;
      <<"UTC-07">> -> <<16#05>>;
      <<"UTC-06">> -> <<16#06>>;
      <<"UTC-05">> -> <<16#07>>;
      <<"UTC-04:30">> -> <<16#08>>;
      <<"UTC-04">> -> <<16#09>>;
      <<"UTC-03:30">> -> <<16#0A>>;
      <<"UTC-03">> -> <<16#0B>>;
      <<"UTC-02">> -> <<16#0C>>;
      <<"UTC-01">> -> <<16#0D>>;
      <<"UTC-00">> -> <<16#0E>>;
      <<"UTC+01">> -> <<16#0F>>;
      <<"UTC+02">> -> <<16#10>>;
      <<"UTC+03">> -> <<16#11>>;
      <<"UTC+03:30">> -> <<16#12>>;
      <<"UTC+04">> -> <<16#13>>;
      <<"UTC+04:30">> -> <<16#14>>;
      <<"UTC+05">> -> <<16#15>>;
      <<"UTC+05:30">> -> <<16#16>>;
      <<"UTC+05:45">> -> <<16#17>>;
      <<"UTC+06">> -> <<16#18>>;
      <<"UTC+06:30">> -> <<16#19>>;
      <<"UTC+07">> -> <<16#1A>>;
      <<"UTC+08">> -> <<16#1B>>;
      <<"UTC+09">> -> <<16#1C>>;
      <<"UTC+09:30">> -> <<16#1D>>;
      <<"UTC+10">> -> <<16#1E>>;
      <<"UTC+11">> -> <<16#1F>>;
      <<"UTC+12">> -> <<16#20>>;
      <<"UTC+13">> -> <<16#21>>
    end,

    Body = <<16#01,16#37,16#01,Zone:1/binary>>,
    ht_logger:info("Body : ~s", [parse_hex(Body)]),
    Header = <<16#03,16#10,16#0A,TerminalId/binary,16#00,16#0D>>,
    EncryptBody = get_encryption(Body),
    Crc = ht_checksum:crc(<<Header/binary,EncryptBody/binary>>),

  Packet = <<
    Header/binary,
    EncryptBody/binary,
    Crc:8/unsigned-integer
  >>,
  P1 = check_packet(Packet),
  {ok,<<16#7E,P1/binary,16#7E>>};
do_encode_command(#command{deviceId = DeviceId,type = <<"GET_SHUTDOWN_TIME">>})  ->
  Key = [cmd_send,"GET_SHUTDOWN_TIME",cmd_send_time,ht_helper_time:timestamp()],
  ht_storage_redis:mupdate_config_info_hhd(DeviceId,Key),
  Id = integer_to_list(DeviceId),
  TerminalId = list_to_binary(hexstr_to_list("0"++Id)),
  ht_logger:info("Terminal id: ~s", [parse_hex(TerminalId)]),
  Body = <<16#01,16#27>>,
  Header = <<16#03,16#12,16#80,16#08,TerminalId/binary,16#00,16#0C>>,
  EncryptBody = get_encryption(Body),
  Crc = ht_checksum:crc(<<Header/binary,EncryptBody/binary>>),

  Packet = <<
    Header/binary,
    EncryptBody/binary,
    Crc:8/unsigned-integer
  >>,
  P1 = check_packet(Packet),
  {ok,<<16#7E,P1/binary,16#7E>>};
do_encode_command(#command{deviceId = DeviceId,type = <<"SET_SHUTDOWN_TIME">>,attributes = #{<<"SHUTDOWN_TIME">> := SHUTDOWN_TIME}}) ->
  Key = [cmd_send,"SET_SHUTDOWN_TIME",cmd_send_time,ht_helper_time:timestamp(),reqShutdownTime,SHUTDOWN_TIME],
  ht_storage_redis:mupdate_config_info_hhd(DeviceId,Key),

    Id = integer_to_list(DeviceId),
    TerminalId = list_to_binary(hexstr_to_list("0"++Id)),
    Body = <<16#01,16#27,16#02,SHUTDOWN_TIME:16/unsigned-integer>>,
    ht_logger:info("Body : ~s", [parse_hex(Body)]),
    Header = <<16#03,16#10,16#0B,TerminalId/binary,16#00,16#0D>>,
    EncryptBody = get_encryption(Body),
    Crc = ht_checksum:crc(<<Header/binary,EncryptBody/binary>>),

  Packet = <<
    Header/binary,
    EncryptBody/binary,
    Crc:8/unsigned-integer
  >>,
  P1 = check_packet(Packet),
  {ok,<<16#7E,P1/binary,16#7E>>};
do_encode_command(#command{deviceId = DeviceId,type = <<"SET_UNLOCK">>})  ->
  Key = [cmd_send,"SET_UNLOCK",cmd_send_time,ht_helper_time:timestamp()],
  ht_storage_redis:mupdate_config_info_hhd(DeviceId,Key),
  Id = integer_to_list(DeviceId),
  TerminalId = list_to_binary(hexstr_to_list("0"++Id)),
  ht_logger:info("Terminal id: ~s", [parse_hex(TerminalId)]),
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
do_encode_command(#command{deviceId = DeviceId,type = <<"SET_LOCK">>})  ->
  Key = [cmd_send,"SET_LOCK",cmd_send_time,ht_helper_time:timestamp()],
  ht_storage_redis:mupdate_config_info_hhd(DeviceId,Key),
  Id = integer_to_list(DeviceId),
  TerminalId = list_to_binary(hexstr_to_list("0"++Id)),
  ht_logger:info("Terminal id: ~s", [parse_hex(TerminalId)]),
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
do_encode_command(#command{deviceId = DeviceId,type = <<"CLEAR_HISTORY_DATA">>})  ->
  Key = [cmd_send,"CLEAR_HISTORY_DATA",cmd_send_time,ht_helper_time:timestamp()],
  ht_storage_redis:mupdate_config_info_hhd(DeviceId,Key),
  Id = integer_to_list(DeviceId),
  TerminalId = list_to_binary(hexstr_to_list("0"++Id)),
  ht_logger:info("Terminal id: ~s", [parse_hex(TerminalId)]),
  Body = <<16#01,16#23,16#01,16#04>>,
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
do_encode_command(#command{deviceId = DeviceId,type = <<"SET_DEVICE_RESTART">>})  ->
  Key = [cmd_send,"SET_DEVICE_RESTART",cmd_send_time,ht_helper_time:timestamp()],
  ht_storage_redis:mupdate_config_info_hhd(DeviceId,Key),
  Id = integer_to_list(DeviceId),
  TerminalId = list_to_binary(hexstr_to_list("0"++Id)),
  ht_logger:info("Terminal id: ~s", [parse_hex(TerminalId)]),
  Body = <<16#01,16#23,16#01,16#03>>,
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
do_encode_command(#command{deviceId = DeviceId,type = <<"SET_FACTORY_RESET">>})  ->
  Key = [cmd_send,"SET_FACTORY_RESET",cmd_send_time,ht_helper_time:timestamp()],
  ht_storage_redis:mupdate_config_info_hhd(DeviceId,Key),
  Id = integer_to_list(DeviceId),
  TerminalId = list_to_binary(hexstr_to_list("0"++Id)),
  ht_logger:info("Terminal id: ~s", [parse_hex(TerminalId)]),
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
do_encode_command(#command{deviceId = DeviceId,type = <<"CLEAR_CARD_SPECIAL">>})  ->
  Key = [cmd_send,"CLEAR_CARD_SPECIAL",cmd_send_time,ht_helper_time:timestamp()],
  ht_storage_redis:mupdate_config_info_hhd(DeviceId,Key),
  Id = integer_to_list(DeviceId),
  TerminalId = list_to_binary(hexstr_to_list("0"++Id)),
  ht_logger:info("Terminal id: ~s", [parse_hex(TerminalId)]),
  Body = <<16#01,16#23,16#01,16#01>>,
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
do_encode_command(#command{deviceId = DeviceId,type = <<"GET_SMS_PASSWORD">>})  ->
  Key = [cmd_send,"GET_SMS_PASSWORD",cmd_send_time,ht_helper_time:timestamp()],
  ht_storage_redis:mupdate_config_info_hhd(DeviceId,Key),
  Id = integer_to_list(DeviceId),
  TerminalId = list_to_binary(hexstr_to_list("0"++Id)),
  ht_logger:info("Terminal id: ~s", [parse_hex(TerminalId)]),
  Body = <<16#01,16#13>>,
  Header = <<16#03,16#12,16#80,16#08,TerminalId/binary,16#00,16#0C>>,
  EncryptBody = get_encryption(Body),
  Crc = ht_checksum:crc(<<Header/binary,EncryptBody/binary>>),

  Packet = <<
    Header/binary,
    EncryptBody/binary,
    Crc:8/unsigned-integer
  >>,
  P1 = check_packet(Packet),
  {ok,<<16#7E,P1/binary,16#7E>>};
do_encode_command(#command{deviceId = DeviceId,type = <<"SET_SMS_PASSWORD">>,attributes = #{<<"SMS_PASSWORD">> := SMS_PASSWORD}}) ->
  Key = [cmd_send,"SET_SMS_PASSWORD",cmd_send_time,ht_helper_time:timestamp(),reqSmsPassword,SMS_PASSWORD],
  ht_storage_redis:mupdate_config_info_hhd(DeviceId,Key),

    Id = integer_to_list(DeviceId),
    TerminalId = list_to_binary(hexstr_to_list("0"++Id)),
    Size = byte_size(SMS_PASSWORD),
    BB = Size+6+2,
    Body = <<16#01,16#13,Size:8/unsigned-integer,SMS_PASSWORD/binary>>,
    ht_logger:info("Body : ~s", [parse_hex(Body)]),
    Header = <<16#03,16#10,1:1,0:1,0:1,0:1,0:1,0:1,BB:10,TerminalId/binary,16#00,16#0D>>,
    EncryptBody = get_encryption(Body),
    Crc = ht_checksum:crc(<<Header/binary,EncryptBody/binary>>),

  Packet = <<
    Header/binary,
    EncryptBody/binary,
    Crc:8/unsigned-integer
  >>,
  P1 = check_packet(Packet),
  {ok,<<16#7E,P1/binary,16#7E>>};
do_encode_command(#command{deviceId = DeviceId,type = <<"GET_TIMER_RESET">>})  ->
  Key = [cmd_send,"GET_TIMER_RESET",cmd_send_time,ht_helper_time:timestamp()],
  ht_storage_redis:mupdate_config_info_hhd(DeviceId,Key),

  Id = integer_to_list(DeviceId),
  TerminalId = list_to_binary(hexstr_to_list("0"++Id)),
  ht_logger:info("Terminal id: ~s", [parse_hex(TerminalId)]),
  Body = <<16#01,16#15>>,
  Header = <<16#03,16#12,16#80,16#08,TerminalId/binary,16#00,16#0C>>,
  EncryptBody = get_encryption(Body),
  Crc = ht_checksum:crc(<<Header/binary,EncryptBody/binary>>),
  Packet = <<
    Header/binary,
    EncryptBody/binary,
    Crc:8/unsigned-integer
  >>,
  P1 = check_packet(Packet),
  {ok,<<16#7E,P1/binary,16#7E>>};

do_encode_command(#command{deviceId = DeviceId,type = <<"SET_TIMER_RESET">>}) ->

  Id = integer_to_list(DeviceId),
  TerminalId = list_to_binary(hexstr_to_list("0"++Id)),
  ht_logger:info("Terminal id: ~s", [parse_hex(TerminalId)]),
  A = calendar:system_time_to_rfc3339(erlang:system_time(second), [{time_designator, $-},{offset, "+08:00"}]),
  B = binary:part(list_to_binary(A),{0,byte_size(list_to_binary(A))-6}),
  C = binary:replace(B,[<<":">>,<<"-">>],<<"">>,[global]),
  Key = [cmd_send,"SET_TIMER_RESET",cmd_send_time,ht_helper_time:timestamp(),reqSetTimerReset,C],
  ht_storage_redis:mupdate_config_info_hhd(DeviceId,Key),
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
do_encode_command(#command{deviceId = DeviceId,type = <<"GET_SIM2_APN_NAME">>})  ->
  Key = [cmd_send,"GET_SIM2_APN_NAME",cmd_send_time,ht_helper_time:timestamp()],
  ht_storage_redis:mupdate_config_info_hhd(DeviceId,Key),
  Id = integer_to_list(DeviceId),
  TerminalId = list_to_binary(hexstr_to_list("0"++Id)),
  ht_logger:info("Terminal id: ~s", [parse_hex(TerminalId)]),
  Body = <<16#01,16#0C>>,
  Header = <<16#03,16#12,16#80,16#08,TerminalId/binary,16#00,16#0C>>,
  EncryptBody = get_encryption(Body),
  Crc = ht_checksum:crc(<<Header/binary,EncryptBody/binary>>),

  Packet = <<
    Header/binary,
    EncryptBody/binary,
    Crc:8/unsigned-integer
  >>,
  P1 = check_packet(Packet),
  {ok,<<16#7E,P1/binary,16#7E>>};
do_encode_command(#command{deviceId = DeviceId,type = <<"SET_SIM2_APN_NAME">>,attributes = #{<<"APN_NAME">> := APN_NAME}}) ->
  Key = [cmd_send,"SET_SIM2_APN_NAME",cmd_send_time,ht_helper_time:timestamp(),reqSim2ApnName,APN_NAME],
  ht_storage_redis:mupdate_config_info_hhd(DeviceId,Key),

    Id = integer_to_list(DeviceId),
    TerminalId = list_to_binary(hexstr_to_list("0"++Id)),
    Size = byte_size(APN_NAME),
    BB = Size+6+2,
    Body = <<16#01,16#0C,Size:8/unsigned-integer,APN_NAME/binary>>,
    ht_logger:info("Body : ~s", [parse_hex(Body)]),
    Header = <<16#03,16#10,1:1,0:1,0:1,0:1,0:1,0:1,BB:10,TerminalId/binary,16#00,16#0D>>,
    EncryptBody = get_encryption(Body),
    Crc = ht_checksum:crc(<<Header/binary,EncryptBody/binary>>),

  Packet = <<
    Header/binary,
    EncryptBody/binary,
    Crc:8/unsigned-integer
  >>,
  P1 = check_packet(Packet),
  {ok,<<16#7E,P1/binary,16#7E>>};
do_encode_command(#command{deviceId = DeviceId,type = <<"GET_SIM1_APN_NAME">>})  ->
  Key = [cmd_send,"GET_SIM1_APN_NAME",cmd_send_time,ht_helper_time:timestamp()],
  ht_storage_redis:mupdate_config_info_hhd(DeviceId,Key),
  Id = integer_to_list(DeviceId),
  TerminalId = list_to_binary(hexstr_to_list("0"++Id)),
  ht_logger:info("Terminal id: ~s", [parse_hex(TerminalId)]),
  Body = <<16#01,16#09>>,
  Header = <<16#03,16#12,16#80,16#08,TerminalId/binary,16#00,16#0C>>,
  EncryptBody = get_encryption(Body),
  Crc = ht_checksum:crc(<<Header/binary,EncryptBody/binary>>),

  Packet = <<
    Header/binary,
    EncryptBody/binary,
    Crc:8/unsigned-integer
  >>,
  P1 = check_packet(Packet),
  {ok,<<16#7E,P1/binary,16#7E>>};
do_encode_command(#command{deviceId = DeviceId,type = <<"SET_SIM1_APN_NAME">>,attributes = #{<<"APN_NAME">> := APN_NAME}}) ->
  Key = [cmd_send,"SET_SIM1_APN_NAME",cmd_send_time,ht_helper_time:timestamp(),reqSim1ApnName,APN_NAME],
  ht_storage_redis:mupdate_config_info_hhd(DeviceId,Key),

    Id = integer_to_list(DeviceId),
    TerminalId = list_to_binary(hexstr_to_list("0"++Id)),
    Size = byte_size(APN_NAME),
    BB = Size+6+2,
    Body = <<16#01,16#09,Size:8/unsigned-integer,APN_NAME/binary>>,
    ht_logger:info("Body : ~s", [parse_hex(Body)]),
    Header = <<16#03,16#10,1:1,0:1,0:1,0:1,0:1,0:1,BB:10,TerminalId/binary,16#00,16#0D>>,
    EncryptBody = get_encryption(Body),
    Crc = ht_checksum:crc(<<Header/binary,EncryptBody/binary>>),

  Packet = <<
    Header/binary,
    EncryptBody/binary,
    Crc:8/unsigned-integer
  >>,
  P1 = check_packet(Packet),
  {ok,<<16#7E,P1/binary,16#7E>>};
do_encode_command(#command{deviceId = DeviceId,type = <<"GET_SERVER_PORT">>})  ->
  Key = [cmd_send,"GET_SERVER_PORT",cmd_send_time,ht_helper_time:timestamp()],
  ht_storage_redis:mupdate_config_info_hhd(DeviceId,Key),
  Id = integer_to_list(DeviceId),
  TerminalId = list_to_binary(hexstr_to_list("0"++Id)),
  ht_logger:info("Terminal id: ~s", [parse_hex(TerminalId)]),
  Body = <<16#01,16#05>>,
  Header = <<16#03,16#12,16#80,16#08,TerminalId/binary,16#00,16#0C>>,
  EncryptBody = get_encryption(Body),
  Crc = ht_checksum:crc(<<Header/binary,EncryptBody/binary>>),

  Packet = <<
    Header/binary,
    EncryptBody/binary,
    Crc:8/unsigned-integer
  >>,
  P1 = check_packet(Packet),
  {ok,<<16#7E,P1/binary,16#7E>>};
do_encode_command(#command{deviceId = DeviceId,type = <<"SET_SERVER_PORT">>,attributes = #{<<"SERVER_PORT">> := PORT}}) ->
  Key = [cmd_send,"SET_SERVER_PORT",cmd_send_time,ht_helper_time:timestamp(),reqServerPort,PORT],
  ht_storage_redis:mupdate_config_info_hhd(DeviceId,Key),

    Id = integer_to_list(DeviceId),
    TerminalId = list_to_binary(hexstr_to_list("0"++Id)),
    Size = byte_size(PORT),
    BB = Size+6+2,
    Body = <<16#01,16#05,Size:8/unsigned-integer,PORT/binary>>,
    ht_logger:info("Body : ~s", [parse_hex(Body)]),
    Header = <<16#03,16#10,1:1,0:1,0:1,0:1,0:1,0:1,BB:10,TerminalId/binary,16#00,16#0D>>,
    EncryptBody = get_encryption(Body),
    Crc = ht_checksum:crc(<<Header/binary,EncryptBody/binary>>),

  Packet = <<
    Header/binary,
    EncryptBody/binary,
    Crc:8/unsigned-integer
  >>,
  P1 = check_packet(Packet),
  {ok,<<16#7E,P1/binary,16#7E>>};
do_encode_command(#command{deviceId = DeviceId,type = <<"GET_SERVER_IP">>})  ->
  Key = [cmd_send,"GET_SERVER_IP",cmd_send_time,ht_helper_time:timestamp()],
  ht_storage_redis:mupdate_config_info_hhd(DeviceId,Key),
  Id = integer_to_list(DeviceId),
  TerminalId = list_to_binary(hexstr_to_list("0"++Id)),
  ht_logger:info("Terminal id: ~s", [parse_hex(TerminalId)]),
  Body = <<16#01,16#03>>,
  Header = <<16#03,16#12,16#80,16#08,TerminalId/binary,16#00,16#0C>>,
  EncryptBody = get_encryption(Body),
  Crc = ht_checksum:crc(<<Header/binary,EncryptBody/binary>>),

  Packet = <<
    Header/binary,
    EncryptBody/binary,
    Crc:8/unsigned-integer
  >>,
  P1 = check_packet(Packet),
  {ok,<<16#7E,P1/binary,16#7E>>};
do_encode_command(#command{deviceId = DeviceId,type = <<"SET_SERVER_IP">>,attributes = #{<<"SERVER_IP">> := IP}}) ->
  Key = [cmd_send,"SET_SERVER_IP",cmd_send_time,ht_helper_time:timestamp(),reqServerIp,IP],
  ht_storage_redis:mupdate_config_info_hhd(DeviceId,Key),

    Id = integer_to_list(DeviceId),
    TerminalId = list_to_binary(hexstr_to_list("0"++Id)),
    Size = byte_size(IP),
    BB = Size+6+2,
    Body = <<16#01,16#03,Size:8/unsigned-integer,IP/binary>>,
    ht_logger:info("Body : ~s", [parse_hex(Body)]),
    Header = <<16#03,16#10,1:1,0:1,0:1,0:1,0:1,0:1,BB:10,TerminalId/binary,16#00,16#0D>>,
    EncryptBody = get_encryption(Body),
    Crc = ht_checksum:crc(<<Header/binary,EncryptBody/binary>>),

  Packet = <<
    Header/binary,
    EncryptBody/binary,
    Crc:8/unsigned-integer
  >>,
  P1 = check_packet(Packet),
  {ok,<<16#7E,P1/binary,16#7E>>};
do_encode_command(#command{deviceId = DeviceId,type = <<"GET_PACKET_INTERVAL">>})  ->
  Key = [cmd_send,"GET_PACKET_INTERVAL",cmd_send_time,ht_helper_time:timestamp()],
  ht_storage_redis:mupdate_config_info_hhd(DeviceId,Key),
  Id = integer_to_list(DeviceId),
  TerminalId = list_to_binary(hexstr_to_list("0"++Id)),
  ht_logger:info("Terminal id: ~s", [parse_hex(TerminalId)]),
  Body = <<16#03,16#02,16#06,16#08>>,
  Header = <<16#03,16#12,16#80,16#0A,TerminalId/binary,16#00,16#0C>>,
  EncryptBody = get_encryption(Body),
  Crc = ht_checksum:crc(<<Header/binary,EncryptBody/binary>>),

  Packet = <<
    Header/binary,
    EncryptBody/binary,
    Crc:8/unsigned-integer
  >>,
  P1 = check_packet(Packet),
  {ok,<<16#7E,P1/binary,16#7E>>};
do_encode_command(#command{deviceId = DeviceId,type = <<"SET_PACKET_INTERVAL">>,attributes = #{<<"PACKET_INTERVAL">> := HitFrequency}}) when HitFrequency >= 10  ->
  Key = [cmd_send,"SET_PACKET_INTERVAL",cmd_send_time,ht_helper_time:timestamp(),reqSleepTime,HitFrequency*60,reqWakeupTime,195],
  ht_storage_redis:mupdate_config_info_hhd(DeviceId,Key),

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
do_encode_command(#command{deviceId = DeviceId,type = <<"SET_PACKET_INTERVAL">>,attributes = #{<<"PACKET_INTERVAL">> := HitFrequency}})  ->
  Key = [cmd_send,"SET_PACKET_INTERVAL",cmd_send_time,ht_helper_time:timestamp(),reqSleepTime,HitFrequency*60,reqWakeupTime,HitFrequency*60+60],
  ht_storage_redis:mupdate_config_info_hhd(DeviceId,Key),
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

do_encode_command(#command{deviceId = DeviceId,type = ?CMD_SET_RAW,attributes = #{?CMD_RAW := HitFrequency}})  ->
    Id = integer_to_list(DeviceId),
    TerminalId = list_to_binary(hexstr_to_list("0"++Id)),
    Body = list_to_binary(hexstr_to_list(binary_to_list(HitFrequency))),
    BB = byte_size(Body)+6,
    Header = <<16#03,16#10,1:1,0:1,0:1,0:1,0:1,0:1,BB:10,TerminalId/binary,16#00,16#0D>>,
    EncryptBody = get_encryption(Body),
%    Header = <<16#03,16#10,16#00,16#04,TerminalId/binary,16#00,16#0A>>,
    Crc = ht_checksum:crc(<<Header/binary,EncryptBody/binary>>),
    Packet = <<
    Header/binary,
    EncryptBody/binary,
    Crc:8/unsigned-integer
    >>,

   P1 = check_packet(Packet),
   {ok,<<16#7E,P1/binary,16#7E>>};
do_encode_command(#command{deviceId = DeviceId,type = ?CMD_GET_RAW,attributes = #{?CMD_RAW := HitFrequency}})  ->
    Id = integer_to_list(DeviceId),
    TerminalId = list_to_binary(hexstr_to_list("0"++Id)),
    Body = list_to_binary(hexstr_to_list(binary_to_list(HitFrequency))),
    BB = byte_size(Body)+6,
    Header = <<16#03,16#12,1:1,0:1,0:1,0:1,0:1,0:1,BB:10,TerminalId/binary,16#00,16#0D>>,
%    Header = <<16#03,16#12,16#00,16#04,TerminalId/binary,16#00,16#0A>>,
    EncryptBody = get_encryption(Body),
    Crc = ht_checksum:crc(<<Header/binary,EncryptBody/binary>>),
    Packet = <<
    Header/binary,
    EncryptBody/binary,
    Crc:8/unsigned-integer
    >>,
   P1 = check_packet(Packet),
   {ok,<<16#7E,P1/binary,16#7E>>};
do_encode_command(#command{deviceId = DeviceId,type = ?CMD_TIMERGET}) ->
  Id = integer_to_list(DeviceId),
  TerminalId = list_to_binary(hexstr_to_list("0"++Id)),
  ht_logger:info("Terminal id: ~s", [parse_hex(TerminalId)]),
  Body = <<16#01,16#15>>,
  Header = <<16#03,16#12,16#80,16#08,TerminalId/binary,16#00,16#0C>>,
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
  ht_logger:info("Terminal id: ~s", [parse_hex(TerminalId)]),
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
do_encode_command(#command{deviceId = DeviceId,type = ?CMD_SETTIME,attributes = #{?KEY_WAKEUP := HitFrequency}}) when HitFrequency >= 10 ->
    ht_logger:info("wakeup:~p",[HitFrequency]),
    Key = [cmd_send,"SET_PACKET_INTERVAL",cmd_send_time,ht_helper_time:timestamp(),reqSleepTime,HitFrequency*60,reqWakeupTime,195], %% shiva code
    ht_storage_redis:mupdate_config_info_hhd(DeviceId,Key),

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
do_encode_command(#command{deviceId = DeviceId,type = ?CMD_SETTIME,attributes = #{?KEY_WAKEUP := HitFrequency}})  ->
    Key = [cmd_send,"SET_PACKET_INTERVAL",cmd_send_time,ht_helper_time:timestamp(),reqSleepTime,HitFrequency*60,reqWakeupTime,HitFrequency*60+60],
    ht_storage_redis:mupdate_config_info_hhd(DeviceId,Key),%% shiva code
     ht_logger:info("wakeup:~p",[HitFrequency]),
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
do_encode_command(#command{deviceId = DeviceId,type = ?CMD_CLEAN_HISTORY})  ->
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
    ht_logger:info("Terminal id: ~s", [parse_hex(TerminalId)]),
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
    ht_logger:info("Terminal id: ~s", [parse_hex(TerminalId)]),
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
    ht_logger:info("Terminal id: ~s", [parse_hex(TerminalId)]),
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
    ht_logger:info("Terminal id: ~s", [parse_hex(TerminalId)]),
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
%do_encode_command(#command{deviceId = DeviceId,type = ?TYPE_SET_TIMEZONE,attributes = #{?KEY_TIMEZONE := Timezone}}) -> %% duplicate/copy of above function
%    Id = integer_to_list(DeviceId),
%    TerminalId = list_to_binary(hexstr_to_list("0"++Id)),
%    ht_logger:info("Terminal id: ~s", [parse_hex(TerminalId)]),
%    Tz = list_to_binary(integer_to_list(Timezone)),
%    Body = <<16#01,16#37,16#01,16#16>>,
%    Header = <<16#03,16#10,16#80,16#0A,TerminalId/binary,16#00,16#0C>>,
%    EncryptBody = get_encryption(Body),
%    Crc = ht_checksum:crc(<<Header/binary,EncryptBody/binary>>),

%  Packet = <<
%    Header/binary,
%    EncryptBody/binary,
%    Crc:8/unsigned-integer
%  >>,

%  P1 = check_packet(Packet),
%  {ok,<<16#7E,P1/binary,16#7E>>};
do_encode_command(#command{deviceId = DeviceId,type = ?CMD_CLEAN_CARD})  ->
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
do_encode_command(#command{deviceId = DeviceId,type = ?CMD_ADD_CARD_LMT,attributes = #{?CARD_NUMBER := Version}}) ->
    Id = integer_to_list(DeviceId),
    TerminalId = list_to_binary(hexstr_to_list("0"++Id)),
    ht_logger:info("Terminal id: ~s ~p", [parse_hex(TerminalId), Version]),
    Card_list =  binary:split(Version,<<",">>,[global]),
    Length = length(Card_list),
    Card_cmd = create_array(Length,Card_list),

    Body = <<Length:16,Length:16,Card_cmd/binary>>,
    BB = byte_size(Body) + 6,
    Header = <<16#03,16#14,16#80,BB:8/unsigned-integer,TerminalId/binary,16#00,16#0D>>,
    ht_logger:info("BODY: ~s ~p", [parse_hex(TerminalId), parse_hex(Body)]),
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
    ht_logger:info("Terminal id: ~s", [parse_hex(TerminalId)]),
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

do_encode_command(#command{deviceId = DeviceId,type = ?CMD_BT_PASSWORD,attributes = #{?PASSWORD := Password}}) when byte_size(Password) == 8  ->
    Id = integer_to_list(DeviceId),
    TerminalId = list_to_binary(hexstr_to_list("0"++Id)),
    ht_logger:info("Terminal id: ~s", [parse_hex(TerminalId)]),
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
    ht_logger:info("Terminal id: ~s", [parse_hex(TerminalId)]),
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

%% shivacode start
do_encode_command(#command{deviceId = DeviceId,type = <<"clean_card_random">>})  ->
  Key = [cmd_send,"clean_card_random",cmd_send_time,ht_helper_time:timestamp()],
   ht_storage_redis:mupdate_config_info_hhd(DeviceId,Key),

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

%do_encode_command(#command{deviceId = DeviceId,type = <<"clean_card_random">>})  -> %% copy of above with different data packet
%    Id = integer_to_list(DeviceId),
%    TerminalId = list_to_binary(hexstr_to_list("0"++Id)),
%    Body = <<16#01,16#23,16#01,16#01>>,
%    Header = <<16#03,16#10,16#80,16#0A,TerminalId/binary,16#00,16#0A>>,
%    EncryptBody = get_encryption(Body),
%    Crc = ht_checksum:crc(<<Header/binary,EncryptBody/binary>>),

%  Packet = <<
%    Header/binary,
%    EncryptBody/binary,
%    Crc:8/unsigned-integer
%  >>,

%  P1 = check_packet(Packet),

%  {ok,<<16#7E,P1/binary,16#7E>>};


do_encode_command(#command{deviceId = DeviceId,type = <<"get_mcu_version">>}) ->

    Key = [cmd_send,"get_mcu_version",cmd_send_time,ht_helper_time:timestamp()],
    ht_storage_redis:mupdate_config_info_hhd(DeviceId,Key),

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
    Key = [cmd_send,"get_bt_version",cmd_send_time,ht_helper_time:timestamp()],
    ht_storage_redis:mupdate_config_info_hhd(DeviceId,Key),

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
    ht_logger:info("Terminal id: ~p CARD NO : ~p", [parse_hex(TerminalId), Ver]),
    Len = byte_size(Ver),
    case Len > 269 of
      true ->
      
       ReqCard = string:join(["reqCardBatch",erlang:integer_to_list(Index+1)],"_"),
       Key = [cmd_send,"add_card_random",cmd_send_time,ht_helper_time:timestamp(), ReqCard, Ver ],
       ht_storage_redis:mupdate_config_info_hhd(DeviceId,Key),
       [Version,Rest] = binary:split(Ver,<<",">>,[{scope,{269,2}}]),
       Card_orig =  binary:split(Version,<<",">>,[global]),
       Length = length(Card_orig),
       CommandModel = Command#command{attributes = #{?CARD_NUMBER => Rest,<<"card_index">> => Index+1}},
       %em_manager_commands:execute(CommandModel),
       timer:send_after(6000,{command, CommandModel}),
       create_card_packet(Length,Card_orig,TerminalId,Index);
     false ->
       ReqCard = string:join(["reqCardBatch",erlang:integer_to_list(Index+1)],"_"),
       Key = [cmd_send,"add_card_random",cmd_send_time,ht_helper_time:timestamp(), ReqCard, Ver ],
       ht_storage_redis:mupdate_config_info_hhd(DeviceId,Key),
       Card_orig =  binary:split(Ver,<<",">>,[global]),
       Length = length(Card_orig),
       create_card_packet(Length,Card_orig,TerminalId,Index)
    end;


do_encode_command(#command{deviceId = DeviceId,type = <<"get_card_random">>,attributes = #{?CARD_NUMBER := Version}}) ->
    Key = [cmd_send,"get_card_random",cmd_send_time,ht_helper_time:timestamp(), reqCardNumber, Version],
    ht_storage_redis:mupdate_config_info_hhd(DeviceId,Key),

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
  ht_logger:info("Unknown Command: ~s", [Cmd]),
  {error, unknown_command}.

create_card_packet(Length,Card_list,TerminalId,Index) ->
   ht_logger:info("Length:~p,Card_list:~p,TerminalId:~p,Index:~p",[Length,Card_list,TerminalId,Index]),
    Card_cmd = create_array(Length,Card_list),
    Body = <<Length:16,Length:16,Card_cmd/binary>>,
    ht_logger:info("Body:~p",[Body]),
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
    {ok,<<16#7E,P1/binary,16#7E>>}
    
    .
fw_update(TerminalId,TotalBinPacket,FirstPacket,CurrentPacket,CRC) when TotalBinPacket =:= CurrentPacket ->
   % em_logger:info("FW DATA : ~p ~n", [FirstPacket]),
    TotalPackage = byte_size(FirstPacket) + 4,
    FinalPacket = <<TotalBinPacket:16/unsigned-integer,CurrentPacket:16/unsigned-integer,FirstPacket/binary>>,
    SS = integer_to_binary(TotalPackage + 2),
    SL = byte_size(SS),
   % ht_logger:info("FW DATA : ~p  ~p ~p ~p ~p ~n", [SL,SS,FinalPacket,CRC,TotalPackage]),
    PacketCrc = binary_to_integer(CRC),
    Body = <<16#33,16#2A,16#44,16#30,16#32,16#2C,SS:SL/binary,16#2C,FinalPacket:TotalPackage/binary,PacketCrc:16/unsigned-integer,16#23>>,
    BB = byte_size(Body),
    Header = <<16#89,16#00,1:1,0:1,0:1,0:1,0:1,0:1,BB:10,TerminalId/binary,16#00,16#0D>>,
    Crc = ht_checksum:crc(<<Header/binary,Body/binary>>),
    Packet = <<
      Header/binary,
      Body/binary,
      Crc:8/unsigned-integer
    >>,
    P1 = check_packet(Packet),
    ht_logger:info("FW LAST PACKET : ~p ~n", [parse_hex(P1)]),
    {ok,<<16#7E,P1/binary,16#7E>>};
fw_update(TerminalId,TotalBinPacket,FirstPacket,1,_) ->
    TotalPackage = 512 + 4,
    FinalPacket = <<TotalBinPacket:16/unsigned-integer,1:16/unsigned-integer,FirstPacket:512/binary>>,
    SS = integer_to_binary(TotalPackage),
    SL = byte_size(SS),
    Body = <<16#33,16#2A,16#44,16#30,16#32,16#2C,SS:SL/binary,16#2C,FinalPacket:516/binary,16#23>>,
    BB = byte_size(Body),
    Header = <<16#89,16#00,1:1,0:1,0:1,0:1,0:1,0:1,BB:10,TerminalId/binary,16#00,16#0D>>,
    Crc = ht_checksum:crc(<<Header/binary,Body/binary>>),
    Packet = <<
      Header/binary,
      Body/binary,
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
    Crc = ht_checksum:crc(<<Header/binary,Body/binary>>),
    Packet = <<
      Header/binary,
      Body/binary,
      Crc:8/unsigned-integer
    >>,
    P1 = check_packet(Packet),
    {ok,<<16#7E,P1/binary,16#7E>>}.
get_encryption(Body) ->
    A = calendar:system_time_to_rfc3339(erlang:system_time(second), [{time_designator, $-},{offset, "+08:00"}]),
    B = binary:part(list_to_binary(A),{2,byte_size(list_to_binary(A))-8}),
    C = binary:replace(B,[<<":">>,<<"-">>],<<"">>,[global]),
    D = list_to_binary(hexstr_to_list(binary_to_list(C))),

    NewBody = <<D:6/binary,Body/binary>>,
    list_to_binary(ht_aes:aes256ecb(NewBody,1)).

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

parse_hex(Bin) ->
  list_to_binary(lists:flatten([io_lib:format("~2.16.0B", [X]) || X <- binary_to_list(Bin)])).

check_fwStatus(Status) ->
  case Status of
    {error,undefined} ->
       {ok,<<"end">>};
    {ok,<<"end">>} ->
       {ok,<<"end">>};
    _ ->
       {ok,<<"ongoing">>}
    end.


create_list(Length,Card) ->
   create_list(Length,Card,<<"">>).
create_list(0,Card,Final) -> Final;
create_list(Length,Card,Final) ->
    Len = Length -1,
    TerminalId = binary_to_integer(lists:nth(Length,Card)),
create_list(Length - 1,Card,<<TerminalId:16,Final/binary>>).


create_array(Length,Card) ->
   create_array(Length,Card,<<"">>).

create_array(0,Card,Final) -> Final;
create_array(Length,Card,Final) ->
    Len = Length -1,
    TerminalId = list_to_binary(hexstr_to_list(binary_to_list(lists:nth(Length,Card)))),
create_array(Length - 1,Card,<<Len:16,TerminalId:4/binary,0,0,0,0,0,0,0,0,0,0,0,0,Final/binary>>).
