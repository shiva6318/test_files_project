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
-ifndef(RECORDS_HRL).
-define(RECORDS_HRL, true).

-record(server, {
  id = 0 :: integer(),
  registration = true :: boolean(),
  readonly = false :: boolean(),
  map = <<"osm">> :: string(),
  bingKey = <<"">> :: string(),
  mapUrl = <<"">> :: string(),
  language = <<"en">> :: string(),
  distanceUnit  = <<"km">> :: string(),
  speedUnit = <<"kmh">> :: string(),
  latitude = 0 :: string(),
  longitude = 0 :: string(),
  zoom = 0 :: string()
}).

-record(user, {
  userName = <<"">> :: string()
}).

-record(firmware, {
  id = 0 :: integer(),
  data = <<"">> :: string() | atom(),
  totalSize = 0 :: integer(),
  currentSize = 0 :: integer(),
  crc = <<"">> :: string() | atom()
}).

-record(position, {
  id = 0 :: integer(),
  type = <<"">> :: string(),
  protocol = <<"">> :: string(),
  serverTime = 0 :: integer(),  %% seconds
  deviceTime = 0 :: integer(),  %% seconds
  fixTime = 0 :: integer(),  %% seconds
  deviceId = 0 :: integer(),
  outdated = false :: boolean(),
  valid = false :: boolean(),
  latitude = 0.0 :: float(),
  longitude = 0.0 :: float(),
  altitude = 0.0 :: float(),
  speed = 0.0 :: float(),
  course = 0.0 :: float(),
  address = <<"">> :: string(),
  attributes =#{} :: map()
}).

-record(order, {
  status =  <<"">> :: string(),
  geofenceCombo = <<"">> :: string()
}).

-record(trip, {
  tripid = <<"">> :: string(),
  status =  <<"">> :: string(),
  shipmentId = <<"">> :: string()
}).

-record(neworder, {
  status =  <<"">> :: string(),
  geofenceCombo = <<"">> :: string(),
  requested = <<"">> :: string(),
  route = <<"">> :: string()
}).

-record(rfid, {
  status =  <<"">> :: string(),
  lcs = <<"">> :: string(),
  cardUserType = <<"">> :: string() 
}).

-record(completed_order, {
  status =  <<"">> :: string(),
  geofenceCombo = <<"">> :: string()
}).

-record(fence_combo, {
  geo_type =  <<"">> :: string(),
  geo_name = <<"">> :: string()
}).

-record(route_combo, {
  route_id =  <<"">> :: string(),
  route_name = <<"">> :: string()
}).
 
-record(geo_combo, {
  geo_id =  <<"">> :: string(),
  geo_name = <<"">> :: string()
}).

-record(track, {
  latitude = 0.0 :: float(),
  longitude = 0.0 :: float(),
  course = <<"">> :: string(),
  vehicleNo = <<"">> :: string(),
  speed = 0.0 :: float(),
  ignition_status = <<"">> :: string(),
  valid = <<"">> :: string(),
  altitude = 0.0 :: float(),
  mcc = <<"">> :: string(),
  lac = <<"">> :: string(),
  mnc = <<"">> :: string(),
  cellid = <<"">> :: string(),
  rxl = <<"">> :: string(),
  gsm = <<"">> :: string(),
  sim_slot = <<"">> :: string(),
  deviation = <<"">> :: string(),
  route_name = <<"">> :: string(),
  hall  = <<"">> :: string(),
  travel_swt  = <<"">> :: string(),
  gps = 0.0 :: float(),
  battery_voltage = <<"">> :: string(),
  external_voltage = <<"">> :: string(),
  satellite_count =  <<"">> :: string(),
  packet_type = <<"">> :: string(),
  fixTime = 0 :: integer(),  %% seconds
  created = 0 :: integer(), 
  packet_time = 0 :: integer(),
  socket_ip = <<"">> :: string(),
  socket_port = <<"">> :: string(),
  asset_id = <<"">> :: string(),
  device_name = <<"">> :: string(),
  geo_type = <<"">> ::string(),
  geo_name = <<"">> ::string(),
  idle_state = false :: boolean(),
  idle_time =  0 :: integer(),
  event_type = <<"NA">> :: string(),
  arrival_status = false :: boolean(),
  charge_status = <<"">> ::string(),
  lock_status = <<"">> ::string(),
  unlock_report = <<"">> ::string(),
%  unlock_report = 0 ::integer(),
  accuracy =  0 :: integer(),
  alarm_bit = <<"">> ::string(),
  low_battery_alert = false :: boolean(),
  remaining_batt = <<"">> :: string(),
  alarm_report = <<"NA">> ::string(),
  alarm_type = <<"NA">> ::string(),

  status_report = <<"NA">> ::string(),
  status_type = <<"NA">> ::string()
}).

-record(alert_info, {
  alert_initiation_status = true :: boolean(),
  low_battery_alert = false :: boolean(),
  critical_battery_alert = false :: boolean(),
  geo_source_alert = false :: boolean(),
  geo_in_transit_alert = false :: boolean(),
  geo_destination_alert = false :: boolean(),
  geo_forbidden_alert = false :: boolean(),
  idle_state_alert = false :: boolean(),
  alarm_report_alert = false :: boolean(),
  status_report_alert = false :: boolean(),
  device_lock_alert = false :: boolean(),
  history_speed_flag = false :: boolean(),
  card_info = false :: boolean(),
  time_info = false :: boolean(),
  card_count = 0 :: integer()
}).

-record (live_info, {
  id = 0 :: integer(), 
  latitude = 0.0 :: float(),
  longitude = 0.0 :: float(),
  course = 0.0 :: float(),
  speed = 0.0 :: float(),
  ignition_status = <<"">> :: string(),
  altitude = 0.0 :: float(),
  rfid = <<"">> :: string(),
  mcc = <<"">> :: string(),
  lac = <<"">> :: string(),
  gsm = <<"">> :: string(),
  gps = <<"">> :: string(),
  battery_voltage = <<"">> :: string(),
  external_battery_voltage = <<"">> :: string(),
  satellite_count =  <<"">> :: string(),
  packet_type = <<"">> :: string(),
  fixTime = 0 :: integer(),  %% seconds
  created = 0 :: integer(), 
  packet_time = 0 :: integer(), 
  address = <<"">> :: string(), 
  geocode_id = 0 :: integer(), 
  lock_stat = <<"">> :: string(), 
  power_connected = <<"">> :: string(), 
  socket_ip = <<"">> :: string(),
  socket_port = <<"">> :: string(),
  device_ip = <<"">> :: string(),
  device_port = <<"">> :: string(),
  asset_id = 0 :: integer(),
  device_name = <<"">> :: string(), 
  protocol = <<"">> :: string(),
  evnt_type = <<"">> :: string(),
  evnt_status = <<"">> :: string(),
  lock_status = false :: boolean(),  
  in_geo_alarm = false :: boolean(),
  ex_geo_alarm = false :: boolean(),
  lock_cut_alarm = false :: boolean(),
  vibra_alarm = false :: boolean(),
  wire_status = false :: boolean(),
  lock_alarm = false :: boolean(),
  unauth_swipe_alarm = false :: boolean(),
  low_bat_alarm = false :: boolean(),
  back_cov_alarm = false :: boolean(),
  back_cov_status = false :: boolean(),
  lock_fault_alarm = false :: boolean(),
  wrng_pass_alarm = false :: boolean()
 }).

-record(asset, {
  id = 0 :: integer(),
  name = <<"">> :: string(),
  user = <<"">> :: string(),
  license_number = <<"">> :: string(),
  imei = <<"">> :: string(),
  imsi = <<"">> :: string(),
  sim_number = <<"">> :: string(),
  battery_voltage = <<"">> :: string(),
  external_battery_voltage = <<"">> :: string(),
  satellite_count =  <<"">> :: string(),
  packet_type = <<"">> :: string(),
  icon = <<"">> :: string(),
  timezone = <<"">> :: string(),
  status = <<"">> :: string(),
  probe = <<"">> :: string(),
  created = 0 :: integer(), 
  modified = 0 :: integer(),
  joined = 0 :: integer(),
  packet_time = 0 :: integer(), 
  device_id = 0 :: integer(),
  vehicle_information_id = 0 :: integer(),
  driver_id = 0 :: integer(),
  camera_connected = <<"">> :: string(),
  rfid_connected = <<"">> :: string(),
  allowed_sms = 0 :: integer(),
  account_id = 0 :: integer(),
  partner_account_id = 0 :: integer()
}).

%%  -record(alert, {
%%    id = 0 :: integer(),
%%    asset_id = 0 :: integer(),
%%    operator = <<"">> :: string(),
%%    code  = <<"">> :: string(),
%%    value = 0 :: integer(),
%%    mode  = <<"">> :: string(),  %% SMS, EMAIL, GCM
%%    email = <<"">> :: string(),
%%    phone =  0 :: integer(),
%%    gcm = #{} :: map(),
%%    dnd_mode  = <<"">> :: string(),
%%    dnd_start_time = 0 :: integer(),
%%    dnd_stop_time = 0 :: integer(),
%%    status = <<"">> :: string(),
%%    created = 0 :: integer(),
%%    user_id = 0 :: integer(),
%%    account_id =  0 :: integer()
%%  }).
%%
%%  -record(alert_asset, {
%%    id = 0 :: integer(),
%%    asset_id = 0 :: integer(),
%%    alert_id = 0 :: integer()
%%  }).

%%web commands hhd   shiva code
-record(config_info_hhd, {
  cmd_send_time = <<"">> :: string(),
  cmd_receive_time = <<"">> :: string(),
  cmd_send = <<"">> :: string(),
  cmd_receive = <<"">> :: string(),
  rfid = <<"">> ::string(),
  fwVersion = <<"">> ::string(),
  sim1_ip = <<"">> ::string(),
  sim2_ip = <<"">> ::string(),
  sim1_port = <<"">> ::string(),
  sim2_port = <<"">> ::string(),
  apn1 = <<"">> :: string(),
  apn2 = <<"">> :: string(),
  sleepTime = <<"">> :: string(),  %% seconds
  wakeupTime = <<"">> :: string()  %% seconds
}).

-record(last_alert, {
  id = 0 :: integer(),
  asset_id = 0 :: integer(),
  status = 0 :: integer()
}).

-record(command, {
  deviceId = 0 :: integer(),
  type = <<"">> :: string(),
  attributes = #{} :: map()
}).

-record(device_permission, {
  id = 0 :: integer(),
  userId = 0 :: integer(),
  deviceId = 0 :: integer()
}).

-record(event, {
  devices = [] :: list(),
  positions = [] :: list()
}).

-record(address, {
  postcode :: string(),
  country :: string(),
  state :: string(),
  district :: string(),
  settlement :: string(),
  suburb :: string(),
  street :: string(),
  house :: string()
}).

-record(statistics, {
  node :: atom(),
  usersCounter :: integer(),
  devicesCounter :: integer()
}).

%%  Device
-define(STATUS_UNKNOWN, <<"unknown">>).
-define(STATUS_ONLINE, <<"online">>).
-define(STATUS_OFFLINE, <<"offline">>).

%% Words separated by dashes (word-second-third)
-define(KEY_ORIGINAL, <<"raw">>).
-define(KEY_INDEX, <<"index">>).
-define(KEY_HDOP, <<"hdop">>).
-define(KEY_VDOP, <<"vdop">>).
-define(KEY_PDOP, <<"pdop">>).
-define(KEY_SATELLITES, <<"sat">>). %% in use
-define(KEY_SATELLITES_VISIBLE, <<"satVisible">>).
-define(KEY_RSSI, <<"rssi">>).
-define(KEY_GPS, <<"gps">>).
-define(KEY_EVENT, <<"event">>).
-define(KEY_ALARM, <<"alarm">>).
-define(KEY_STATUS, <<"status">>).
-define(KEY_ODOMETER, <<"odometer">>). %% meters
-define(KEY_TRIP_ODOMETER, <<"tripOdometer">>).
-define(KEY_HOURS, <<"hours">>).
-define(KEY_INPUT, <<"input">>).
-define(KEY_OUTPUT, <<"output">>).
-define(KEY_POWER, <<"power">>).
-define(KEY_BATTERY, <<"battery">>).
-define(KEY_FUEL, <<"fuel">>).
-define(KEY_FUEL_CONSUMPTION, <<"fuelConsumption">>).
-define(KEY_RFID, <<"rfid">>).
-define(KEY_VERSION_FW, <<"versionFw">>).
-define(KEY_VERSION_HW, <<"versionHw">>).
-define(KEY_TYPE, <<"type">>).
-define(KEY_IGNITION, <<"ignition">>).
-define(KEY_FLAGS, <<"flags">>).
-define(KEY_CHARGE, <<"charge">>).
-define(KEY_IP, <<"ip">>).
-define(KEY_ARCHIVE, <<"archive">>).
-define(KEY_DISTANCE, <<"distance">>). %% meters
-define(KEY_TOTAL_DISTANCE, <<"totalDistance">>).
-define(KEY_RPM, <<"rpm">>).
-define(KEY_VIN, <<"vin">>).
-define(KEY_APPROXIMATE, <<"approximate">>).
-define(KEY_THROTTLE, <<"throttle">>).
-define(KEY_MOTION, <<"motion">>).
-define(KEY_ARMED, <<"armed">>).
-define(KEY_ACCURACY, <<"accuracy">>).
-define(KEY_GEOFENCE, <<"geofence">>).
-define(KEY_ACCELERATION, <<"acceleration">>).
-define(KEY_DEVICE_TEMP, <<"deviceTemp">>).

-define(KEY_DTCS, <<"dtcs">>).
-define(KEY_OBD_SPEED, <<"obdSpeed">>).
-define(KEY_OBD_ODOMETER, <<"obdOdometer">>).

-define(KEY_RESULT, <<"result">>).

%% Starts with 1 not 0
-define(PREFIX_TEMP, <<"temp">>).
-define(PREFIX_ADC, <<"adc">>).
-define(PREFIX_IO, <<"io">>).
-define(PREFIX_COUNT, <<"count">>).


%% COMMANT TYPES AND ATTR FIELDS

-define(TYPE_CUSTOM, <<"custom">>).
-define(TYPE_IDENTIFICATION, <<"deviceIdentification">>).
-define(TYPE_POSITION_SINGLE, <<"positionSingle">>).
-define(TYPE_POSITION_PERIODIC, <<"positionPeriodic">>).
-define(TYPE_UNLOCK_DEVICE, <<"unlockDevice">>).
-define(TYPE_POSITION_STOP, <<"positionStop">>).
-define(TYPE_ENGINE_STOP, <<"engineStop">>).
-define(TYPE_ENGINE_RESUME, <<"engineResume">>).
-define(TYPE_ALARM_ARM, <<"alarmArm">>).
-define(TYPE_ALARM_DISARM, <<"alarmDisarm">>).
-define(TYPE_SET_TIMEZONE, <<"setTimezone">>).
-define(TYPE_REQUEST_PHOTO, <<"requestPhoto">>).
-define(TYPE_REBOOT_DEVICE, <<"rebootDevice">>).
-define(TYPE_SEND_SMS, <<"sendSms">>).
-define(TYPE_SEND_USSD, <<"sendUssd">>).
-define(TYPE_SOS_NUMBER, <<"sosNumber">>).
-define(TYPE_SILENCE_TIME, <<"silenceTime">>).
-define(TYPE_SET_PHONEBOOK, <<"setPhonebook">>).
-define(TYPE_VOICE_MESSAGE, <<"voiceMessage">>).
-define(TYPE_OUTPUT_CONTROL, <<"outputControl">>).
-define(TYPE_VOICE_MONITORING, <<"voiceMonitoring">>).
-define(TYPE_SET_AGPS, <<"setAgps">>).
-define(TYPE_SET_INDICATOR, <<"setIndicator">>).
-define(TYPE_CONFIGURATION, <<"configuration">>).
-define(TYPE_GET_VERSION, <<"getVersion">>).
-define(TYPE_FIRMWARE_UPDATE, <<"firmwareUpdate">>).
-define(TYPE_SET_CONNECTION, <<"setConnection">>).
-define(TYPE_SET_ODOMETER, <<"setOdometer">>).

-define(TYPE_MODE_POWER_SAVING, <<"modePowerSaving">>).
-define(TYPE_MODE_DEEP_SLEEP, <<"modeDeepSleep">>).

-define(KEY_UNIQUE_ID, <<"uniqueId">>).
-define(KEY_FREQUENCY, <<"frequency">>).
-define(KEY_TIMEZONE, <<"timezone">>).
-define(KEY_DEVICE_PASSWORD, <<"devicePassword">>).
-define(KEY_RADIUS, <<"radius">>).
-define(KEY_MESSAGE, <<"message">>).
-define(KEY_ENABLE, <<"enable">>).
-define(KEY_DATA, <<"data">>).
%%-define(KEY_INDEX, <<"index">>).
-define(KEY_PHONE, <<"phone">>).
-define(KEY_SERVER, <<"server">>).
-define(KEY_PORT, <<"port">>).
-define(FREQUENCY, <<"frequency">>).
-define(WAKE_UP, <<"wakeUp">>).

-define(ALARM_GENERAL, <<"general">>).
-define(ALARM_SOS, <<"sos">>).
-define(ALARM_VIBRATION, <<"vibration">>).
-define(ALARM_MOVEMENT, <<"movement">>).
-define(ALARM_LOW_SPEED, <<"lowspeed">>).
-define(ALARM_OVERSPEED, <<"overspeed">>).
-define(ALARM_FALL_DOWN, <<"fallDown">>).
-define(ALARM_LOW_POWER, <<"lowPower">>).
-define(ALARM_LOW_BATTERY, <<"lowBattery">>).
-define(ALARM_FAULT, <<"fault">>).
-define(ALARM_POWER_OFF, <<"powerOff">>).
-define(ALARM_POWER_ON, <<"powerOn">>).
-define(ALARM_DOOR, <<"door">>).
-define(ALARM_GEOFENCE, <<"geofence">>).
-define(ALARM_GEOFENCE_ENTER, <<"geofenceEnter">>).
-define(ALARM_GEOFENCE_EXIT, <<"geofenceExit">>).
-define(ALARM_GPS_ANTENNA_CUT, <<"gpsAntennaCut">>).
-define(ALARM_ACCIDENT, <<"accident">>).
-define(ALARM_TOW, <<"tow">>).
-define(ALARM_ACCELERATION, <<"hardAcceleration">>).
-define(ALARM_BREAKING, <<"hardBreaking">>).
-define(ALARM_FATIGUE_DRIVING, <<"fatigueDriving">>).
-define(ALARM_POWER_CUT, <<"powerCut">>).
-define(ALARM_JAMMING, <<"jamming">>).
-define(ALARM_TEMPERATURE, <<"temperature">>).
-define(ALARM_PARKING, <<"parking">>).
-define(ALARM_SHOCK, <<"shock">>).
-define(ALARM_BONNET, <<"bonnet">>).
-define(ALARM_FOOT_BRAKE, <<"footBrake">>).
-define(ALARM_OIL_LEAK, <<"oilLeak">>).
-define(ALARM_TAMPERING, <<"tampering">>).


%% JT701 COMMANDS
-define(TYPE_FIRMWARE, <<"firmware">>).
-define(TYPE_LOCATION, <<"location">>).
-define(TYPE_SIM1_IP1, <<"sim1IP">>).
-define(TYPE_SIM2_IP2, <<"sim2IP">>).
-define(TYPE_VIP_NUMBER, <<"vipNumber">>).
-define(TYPE_RESTORE, <<"restoreDevice">>).
-define(TYPE_IMEI, <<"imeiDevice">>).
-define(TYPE_RESTART, <<"restartDevice">>).
-define(TYPE_TIME_SERVICE, <<"timeService">>).
-define(TYPE_SLEEP, <<"sleepDevice">>).

-define(TYPE_WORK_TIME, <<"workingTime">>).
-define(TYPE_RFID_CARD, <<"rfidCard">>).
-define(TYPE_CHANGE_PASSWORD, <<"changePassword">>).
-define(TYPE_CLEAR_WHITE_LIST, <<"clearWhiteList">>).
-define(TYPE_POWER_SWITCH, <<"powerSwitch">>).


-define(FUNCTION, <<"function">>).
-define(IP_ADDRESS, <<"ipAddress">>).
-define(PORT_VALUE, <<"port">>).
-define(DEVICEID, <<"deviceid">>).
-define(APN, <<"apn">>).
-define(APN_USER, <<"apnUser">>).
-define(APN_PSWRD, <<"apnPassword">>).

-define(COUNT, <<"count">>).
-define(PHONE_NUMBER, <<"phoneNumber">>).

-define(WORKING_TIME, <<"workTime">>).

-define(ACTION, <<"action">>).
-define(RFID_TAGS, <<"rfidTags">>).
-define(OLD_PASSWORD, <<"oldPassword">>).
-define(NEW_PASSWORD, <<"newPassword">>).

%% JT701 COMMANDS
-define(CMD_SETTIME,<<"P041">>).%et Time Interval</option>
-define(CMD_GETTIME,<<"P040">>).%Get Time Interval</option>
-define(CMD_GETSTATUS,<<"getstatus">>). %                                               <option value="P061">Set APN SIM1</option>
-define(CMD_GETPARAM,<<"getparam">>). %                                               <option value="P061">Set APN SIM1</option>
-define(CMD_SETAPN,<<"setapn">>). %                                               <option value="P061">Set APN SIM1</option>
-define(CMD_GETAPN,<<"getapn">>). %                                               <option value="P061">Set APN SIM1</option>
-define(CMD_SETIP,<<"setip">>). %                                               <option value="P061">Set APN SIM1</option>
-define(CMD_GETIP,<<"getip">>). %                                               <option value="P061">Set APN SIM1</option>
-define(CMD_SETAPN1,<<"P061">>). %                                               <option value="P061">Set APN SIM1</option>
-define(CMD_GETAPN1,<<"P060">>).%                                                <option value="P060">Get APN SIM1</option>
-define(CMD_SETAPN2,<<"P063">>).%                                                <option value="P063">Set APN SIM2</option>
-define(CMD_GETAPN2,<<"P062">>).%                                                <option value="P062">Get APN SIM3</option>
-define(CMD_SETVIP,<<"P111">>).%                                                <option value="P111">Set VIP Number</option>
-define(CMD_GETVIP,<<"P110">>).%                                                <option value="P110">Get VIP Number</option>
-define(CMD_RESTART,<<"P15">>).%                                                <option value="P15">Device Restart</option>
-define(CMD_SETCURTIME,<<"P22">>).%                                                <option value="P22">Set Time</option>
-define(CMD_SETSMSWAKE,<<"P231">>).%                                                <option value="P231">Set SMS Wake</option>
-define(CMD_GETSMSWAKE,<<"P230">>).%                                                 <option value="P230">Get SMS Wake</option>
-define(CMD_DEVICESLEEP,<<"P32">>).%                                                 <option value="P32">Make Device sleep</option>
-define(CMD_SETVIBRA,<<"P361">>).%                                                 <option value="P361">Set Vibration value</option>
-define(CMD_GETVIBRA,<<"P360">>).  %                                               <option value="P360">Get Vibration value</option>
-define(CMD_SETMOVING,<<"P371">>).%                                                 <option value="P371">Set Moving Detection</option>
-define(CMD_GETMOVING,<<"P370">>).%                                                 <option value="P370">Get Moving Detection</option>
-define(CMD_SETALM,<<"P401">>).%                                                 <option value="P401">Set Alarm Switch</option>
-define(CMD_GETALM,<<"P400">>).%                                                 <option value="P400">Get Alarm Switch</option>
-define(CMD_SETRFID,<<"P411">>).%                                                 <option value="P411">Set RFID Cards</option>
-define(CMD_GETRFID,<<"P410">>). %                                                <option value="P410">Get RFID Cards</option>
-define(CMD_REMOVERFID,<<"P412">>). %                                                <option value="P410">Get RFID Cards</option>
-define(CMD_UNLOCK,<<"P43">>).%                                                 <option value="P43">Unlock</option>
-define(CMD_LOCK,<<"P431">>).%    
-define(CMD_FWVER,<<"P01">>).%                                                 <option value="P43">Unlock</option>
-define(CMD_LOCATION,<<"P02">>).%                                                 <option value="P43">Unlock</option>
-define(CMD_GETALL,<<"GETALL">>).%                                                 <option value="P43">Unlock</option>
-define(CMD_SLEEP,<<"UNLOCKWAKEUP">>).%                                                 <option value="P43">Unlock</option>
-define(CMD_OTA_CONT,<<"ota_cont">>).
-define(CMD_OTA,<<"ota">>).%                                                 <option value="P43">Unlock</option>
-define(CMD_RESET,<<"reset">>).%                                                 <option value="P43">Unlock</option>
-define(CMD_VERSION,<<"check_version">>).
-define(CMD_UPDATE,<<"check_update">>).
-define(VERSION,<<"version">>). 
-define(CMD_RESTART1,<<"restart">>).
-define(KEY_WAKEUP, <<"p04wakeup">>).
-define(KEY_SLEEP, <<"p04sleep">>).
-define(CMD_CHK_FW, <<"D03">>).
-define(CMD_TIMERESET,<<"timereset">>).
-define(KEY_IP_ADDRESS, <<"p06ipAddress">>).
-define(KEY_PORT_VALUE, <<"p06port">>).
-define(KEY_APN, <<"p06apn">>).
-define(KEY_APN_USER, <<"p06apnUser">>).
-define(KEY_APN_PSWRD, <<"p06apnPassword">>).
-define(CMD_READ_CARD, <<"read_card">>).
-define(CMD_ADD_CARD, <<"add_card">>).
-define(CMD_ADD_CARD_LMT, <<"add_card_rule">>).
-define(CMD_CLEAN_CARD, <<"clean_card">>).
-define(CARD_NUMBER, <<"card_number">>).
-define(KEY_VIP_NUMBER, <<"p11vipNumber">>).
-define(KEY_VIP_COUNT, <<"p11vipCount">>).

-define(KEY_VIBRA, <<"p36value">>).
-define(KEY_MOVING, <<"p37value">>).

-define(KEY_WIRECUT,<<"p40wirecutAlarm">>).
-define(KEY_UNAUTH,<<"P40unauthAlarm">>).
-define(KEY_UNLOCK,<<"P40unlockAlarm">>).
-define(KEY_WRONGCMD,<<"P40wrongCmdAlarm">>).
-define(KEY_VIBRAALM,<<"P40vibrationAlarm">>).
-define(KEY_ENTERGEO,<<"P40enterGeoAlarm">>).
-define(KEY_EXITGEO,<<"P40exitGeoAlarm">>).
-define(KEY_LOWBAT,<<"P40lowBatAlarm">>).
-define(KEY_BACKCAP,<<"P40backCapAlarm">>).
-define(KEY_MOTORFULT,<<"P40motorFalutAlarm">>).
-define(CMD_BT_PASSWORD,<<"BT_PSWD_CNG">>).
-define(PASSWORD,<<"PASSWORD">>).
-define(CMD_CLEAN_HISTORY,<<"CACHE_CLEAN">>).
-define(KEY_CARD_LIST, <<"p41cardList">>).
-define(KEY_CARD_COUNT, <<"p41cardCount">>).
-define(KEY_CARD_PASS, <<"p41password">>).
-define(KEY_CARD_GROUP, <<"p41rfidgroup">>).

-define(KEY_PASSWORD, <<"p43password">>).

%% shiva code start web commands

-define(CMD_SET_RAW,<<"CMD_SET_RAW">>).
-define(CMD_RAW,<<"CMD_RAW">>).
-define(CMD_TIMERGET,<<"timerget">>).
-define(CMD_GET_RAW,<<"CMD_GET_RAW">>).
%% shiva code end

-endif. % RECORDS_HRL




