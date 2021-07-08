%%%-------------------------------------------------------------------
%%% @author sunnyrichards
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 16. Mar 2018 5:56 PM
%%%-------------------------------------------------------------------
-module(ht_storage_redis).
-author("sunnyrichards").

-behaviour(gen_server).

-include("ht_records.hrl").

%% API
-export([start_link/2]).

%% gen_server callbacks
-export([init/1,
  handle_call/3,
  handle_cast/2,
  handle_info/2,
  terminate/2,
  code_change/3]).

-export([create_live_data/2,create_alert_info/2]).
-export([check_alert_info/1,delete_alert_info/1]).

-export([get_idle_data/1,get_previous_packet_time/1]).
-export([get_geo_data/1,get_geo_reject_data/1,get_full_data/1]).
-export([get_lock_data/1]).
-export([get_hhd_data/1]).
-export([get_valid_data/1]).

-export([get_low_battery_info/1,get_critical_battery_info/1]).
-export([get_geo_source_info/1,get_geo_in_transit_info/1]).
-export([get_geo_forbidden_info/1,get_geo_destination_info/1,update_single_field/3,get_single_field/2]).
-export([get_idle_state_info/1,get_lock_state_info/1]).
-export([get_history_speed_flag/1,get_card_info/1]).

-export([update_critical_battery_info/2,update_low_battery_info/2]).
-export([update_geo_source_info/2,update_geo_in_transit_info/2]).
-export([update_geo_destination_info/2,update_geo_forbidden_info/2]).
-export([update_idle_state_info/2,update_lock_state_info/2]).
-export([update_history_speed_flag/2,update_card_info/2]).

-export([add_card_info/1,add_time_info/1, update_time_info/2, get_time_info/1]).
-export([add_card_count/1, update_card_count/2, get_card_count/1]).

-export([update_geofence_count/2, get_geofence_count/1]). %% shiva added for geofence count
-export([update_geofence_out_data/2, get_geofence_out_data/1]). %% shiva added for geofence out data
-export([get_geo_type_and_geo_name/1]).

-export([create_config_info_hhd/2,get_single_config_info_hhd/2]). %% webcommands shiva code start
-export([update_config_info_hhd/3,get_config_info_hhd/1]).
-export([mupdate_config_info_hhd/2]).  %% shiva code end

-define(SERVER, ?MODULE).

-record(state, {connection :: pid()}).

%%%===================================================================
%%% API
%%%===================================================================

-spec(create_live_data(Key:: any(), Rec :: #track{}) -> {ok, #track{}} | {error, string() | [string()]}).
create_live_data(Key, Rec) ->
  gen_server:call(?SERVER, {create_live_data, Key, Rec}).

-spec(create_alert_info(Key:: any(), Rec :: #alert_info{}) -> {ok, #track{}} | {error, string() | [string()]}).
create_alert_info(Key, Rec) ->
  gen_server:call(?SERVER, {create_alert_info, Key, Rec}).

-spec(check_alert_info(Key:: any()) -> {ok, #track{}} | {error, string() | [string()]}).
check_alert_info(Key) ->
  gen_server:call(?SERVER, {check_alert_info, Key}).

-spec(delete_alert_info(Key:: any()) -> {ok, #track{}} | {error, string() | [string()]}).
delete_alert_info(Key) ->
  gen_server:call(?SERVER, {delete_alert_info, Key}).

-spec(get_idle_data(Key:: any()) -> {ok, #track{}} | {error, string() | [string()]}).
get_idle_data(Key) ->
  gen_server:call(?SERVER, {get_idle_data, Key}).

-spec(get_geo_data(Key:: any()) -> {ok, #track{}} | {error, string() | [string()]}).
get_geo_data(Key) ->
  gen_server:call(?SERVER, {get_geo_data, Key}).

-spec(get_geo_reject_data(Key:: any()) -> {ok, #track{}} | {error, string() | [string()]}).
get_geo_reject_data(Key) ->
  gen_server:call(?SERVER, {get_geo_reject_data, Key}).

-spec(get_full_data(Key:: any()) -> {ok, #track{}} | {error, string() | [string()]}).
get_full_data(Key) ->
  gen_server:call(?SERVER, {get_full_data, Key}).

-spec(get_lock_data(Key:: any()) -> {ok, #track{}} | {error, string() | [string()]}).
get_lock_data(Key) ->
  gen_server:call(?SERVER, {get_lock_data, Key}).

-spec(get_hhd_data(Key:: any()) -> {ok, #track{}} | {error, string() | [string()]}).
get_hhd_data(Key) ->
  gen_server:call(?SERVER, {get_hhd_data, Key}).


-spec(get_valid_data(Key:: any()) -> {ok, #track{}} | {error, string() | [string()]}).
get_valid_data(Key) ->
  gen_server:call(?SERVER, {get_valid_data, Key}).

-spec(get_previous_packet_time(Key:: any()) -> {ok, #track{}} | {error, string() | [string()]}).
get_previous_packet_time(Key) ->
  gen_server:call(?SERVER, {get_previous_packet_time, Key}).

-spec(get_low_battery_info(Key:: any()) -> {ok, #track{}} | {error, string() | [string()]}).
get_low_battery_info(AssetId) ->
  Key = list_to_atom("alert_" ++ integer_to_list(AssetId)),
  gen_server:call(?SERVER, {get_low_battery_info, Key}).

-spec(update_low_battery_info(Key:: any(), Value :: boolean()) -> success).
update_low_battery_info(AssetId, Value) ->
  Key = list_to_atom("alert_" ++ integer_to_list(AssetId)),
  gen_server:call(?SERVER, {update_low_battery_info, Key, Value}).


-spec(update_single_field(Key:: any(),Field:: string(),Value:: string()) -> {ok, #track{}} | {error, string() | [string()]}).
update_single_field(Key,Field,Value) ->
  gen_server:call(?SERVER, {update_single_field, Key, Field,Value}).


-spec(get_single_field(Key:: any(),Field:: string()) -> {ok, #track{}} | {error, string() | [string()]}).
get_single_field(Key,Field) ->
  gen_server:call(?SERVER, {get_single_field, Key,Field}).


-spec(get_critical_battery_info(Key:: any()) -> {ok, #track{}} | {error, string() | [string()]}).
get_critical_battery_info(AssetId) ->
  Key = list_to_atom("alert_" ++ integer_to_list(AssetId)),
  gen_server:call(?SERVER, {get_critical_battery_info, Key}).

-spec(update_critical_battery_info(Key:: any(), Value :: boolean()) -> success).
update_critical_battery_info(AssetId, Value) ->
  Key = list_to_atom("alert_" ++ integer_to_list(AssetId)),
  gen_server:call(?SERVER, {update_critical_battery_info, Key, Value}).

-spec(get_geo_source_info(Key:: any()) -> {ok, #track{}} | {error, string() | [string()]}).
get_geo_source_info(AssetId) ->
  Key = list_to_atom("alert_" ++ integer_to_list(AssetId)),
  gen_server:call(?SERVER, {get_geo_source_info, Key}).

-spec(update_geo_source_info(Key:: any(), Value :: boolean()) -> success).
update_geo_source_info(AssetId, Value) ->
  Key = list_to_atom("alert_" ++ integer_to_list(AssetId)),
  gen_server:call(?SERVER, {update_geo_source_info, Key, Value}).

-spec(get_geo_in_transit_info(Key:: any()) -> {ok, #track{}} | {error, string() | [string()]}).
get_geo_in_transit_info(AssetId) ->
  Key = list_to_atom("alert_" ++ integer_to_list(AssetId)),
  gen_server:call(?SERVER, {get_geo_in_transit_info, Key}).

-spec(update_geo_in_transit_info(Key:: any(), Value :: boolean()) -> success).
update_geo_in_transit_info(AssetId, Value) ->
  Key = list_to_atom("alert_" ++ integer_to_list(AssetId)),
  gen_server:call(?SERVER, {update_geo_in_transit_info, Key, Value}).

-spec(get_geo_destination_info(Key:: any()) -> {ok, #track{}} | {error, string() | [string()]}).
get_geo_destination_info(AssetId) ->
  Key = list_to_atom("alert_" ++ integer_to_list(AssetId)),
  gen_server:call(?SERVER, {get_geo_destination_info, Key}).

-spec(update_geo_destination_info(Key:: any(), Value :: boolean()) -> success).
update_geo_destination_info(AssetId, Value) ->
  Key = list_to_atom("alert_" ++ integer_to_list(AssetId)),
  gen_server:call(?SERVER, {update_geo_destination_info, Key, Value}).

-spec(get_geo_forbidden_info(Key:: any()) -> {ok, #track{}} | {error, string() | [string()]}).
get_geo_forbidden_info(AssetId) ->
  Key = list_to_atom("alert_" ++ integer_to_list(AssetId)),
  gen_server:call(?SERVER, {get_geo_forbidden_info, Key}).

-spec(update_geo_forbidden_info(Key:: any(), Value :: boolean()) -> success).
update_geo_forbidden_info(AssetId, Value) ->
  Key = list_to_atom("alert_" ++ integer_to_list(AssetId)),
  gen_server:call(?SERVER, {update_geo_forbidden_info, Key, Value}).

-spec(get_idle_state_info(Key:: any()) -> {ok, #track{}} | {error, string() | [string()]}).
get_idle_state_info(AssetId) ->
  Key = list_to_atom("alert_" ++ integer_to_list(AssetId)),
  gen_server:call(?SERVER, {get_idle_state_info, Key}).

-spec(update_idle_state_info(Key:: any(), Value :: boolean()) -> success).
update_idle_state_info(AssetId, Value) ->
  Key = list_to_atom("alert_" ++ integer_to_list(AssetId)),
  gen_server:call(?SERVER, {update_idle_state_info, Key, Value}).

-spec(get_lock_state_info(Key:: any()) -> {ok, #track{}} | {error, string() | [string()]}).
get_lock_state_info(AssetId) ->
  Key = list_to_atom("alert_" ++ integer_to_list(AssetId)),
  gen_server:call(?SERVER, {get_lock_state_info, Key}).

-spec(update_lock_state_info(Key:: any(), Value :: boolean()) -> success).
update_lock_state_info(AssetId, Value) ->
  Key = list_to_atom("alert_" ++ integer_to_list(AssetId)),
  gen_server:call(?SERVER, {update_lock_state_info, Key, Value}).

-spec(get_history_speed_flag(Key:: any()) -> {ok, #track{}} | {error, string() | [string()]}).
get_history_speed_flag(AssetId) ->
  Key = list_to_atom("alert_" ++ integer_to_list(AssetId)),
  gen_server:call(?SERVER, {get_history_speed_flag, Key}).

-spec(update_history_speed_flag(Key:: any(), Value :: boolean()) -> success).
update_history_speed_flag(AssetId, Value) ->
  Key = list_to_atom("alert_" ++ integer_to_list(AssetId)),
  gen_server:call(?SERVER, {update_history_speed_flag, Key, Value}).

-spec(get_card_info(Key:: any()) -> {ok, #track{}} | {error, string() | [string()]}).
get_card_info(AssetId) ->
  Key = list_to_atom("alert_" ++ integer_to_list(AssetId)),
  gen_server:call(?SERVER, {get_card_info, Key}).

-spec(get_time_info(Key:: any()) -> {ok, #track{}} | {error, string() | [string()]}).
get_time_info(AssetId) ->
  Key = list_to_atom("alert_" ++ integer_to_list(AssetId)),
  gen_server:call(?SERVER, {get_time_info, Key}).

-spec(update_card_info(Key:: any(), Value :: boolean()) -> success).
update_card_info(AssetId, Value) ->
  Key = list_to_atom("alert_" ++ integer_to_list(AssetId)),
  gen_server:call(?SERVER, {update_card_info, Key, Value}).

-spec(update_time_info(Key:: any(), Value :: boolean()) -> success).
update_time_info(AssetId, Value) ->
  Key = list_to_atom("alert_" ++ integer_to_list(AssetId)),
  gen_server:call(?SERVER, {update_time_info, Key, Value}).

-spec(add_card_info(Key:: any()) -> success).
add_card_info(AssetId) ->
  Key = list_to_atom("alert_" ++ integer_to_list(AssetId)),
  gen_server:call(?SERVER, {add_card_info, Key}).

-spec(add_time_info(Key:: any()) -> success).
add_time_info(AssetId) ->
  Key = list_to_atom("alert_" ++ integer_to_list(AssetId)),
  gen_server:call(?SERVER, {add_time_info, Key}).

-spec(add_card_count(Key:: any()) -> success).
add_card_count(AssetId) ->
  Key = list_to_atom("alert_" ++ integer_to_list(AssetId)),
  gen_server:call(?SERVER, {add_card_count, Key}).

-spec(update_card_count(Key:: any(), Value :: boolean()) -> success).
update_card_count(AssetId, Value) ->
  Key = list_to_atom("alert_" ++ integer_to_list(AssetId)),
  gen_server:call(?SERVER, {update_card_count, Key, Value}).

-spec(get_card_count(Key:: any()) -> {ok, Value :: integer()} | {error, string() | [string()]}).
get_card_count(AssetId) ->
  Key = list_to_atom("alert_" ++ integer_to_list(AssetId)),
  gen_server:call(?SERVER, {get_card_count, Key}).

%%%%%%%%%%%%%%% shiva code start %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec(update_geofence_count(Key:: any(), Value :: integer()) -> success).
update_geofence_count(AssetId, Value) ->
  Key = list_to_atom("alert_" ++ integer_to_list(AssetId)),
  gen_server:call(?SERVER, {update_geofence_count, Key, Value}).

-spec(get_geofence_count(Key:: any()) -> {ok, Value :: integer()} | {error, string() | [string()]}).
get_geofence_count(AssetId) ->
  Key = list_to_atom("alert_" ++ integer_to_list(AssetId)),
  gen_server:call(?SERVER, {get_geofence_count, Key}).

-spec(update_geofence_out_data(Key:: any(), Value :: list()) -> success).
update_geofence_out_data(AssetId, Value) ->
  Key = list_to_atom("alert_" ++ integer_to_list(AssetId)),
  gen_server:call(?SERVER, {update_geofence_out_data, Key, Value}).

-spec(get_geofence_out_data(Key:: any()) -> {ok, Value :: list()} | {error, string() | [string()]}).
get_geofence_out_data(AssetId) ->
  Key = list_to_atom("alert_" ++ integer_to_list(AssetId)),
  gen_server:call(?SERVER, {get_geofence_out_data, Key}).

-spec(get_geo_type_and_geo_name(Key:: any()) -> {ok, Value :: list()} | {error, string() | [string()]}).
get_geo_type_and_geo_name(AssetId) ->
  Key = list_to_atom("alert_" ++ integer_to_list(AssetId)),
  gen_server:call(?SERVER, {get_geo_type_and_geo_geo_name, Key}).


%% web commands hhd 
-spec(create_config_info_hhd(Key:: any(),Rec :: #config_info_hhd{}) -> {ok, #config_info_hhd{}} | {error, string() | [string()]}).
create_config_info_hhd(AssetId, Rec) ->
  Key = list_to_atom("config_" ++ integer_to_list(AssetId)),
  gen_server:call(?SERVER, {create_config_info_hhd, Key, Rec}).

-spec(get_config_info_hhd(Key:: any()) -> {ok, #config_info_hhd{}} | {error, string() | [string()]}).
get_config_info_hhd(AssetId) ->
  Key = list_to_atom("config_" ++ integer_to_list(AssetId)),
  gen_server:call(?SERVER, {get_config_info_hhd, Key}).

-spec(update_config_info_hhd(Key:: any(),Field:: string(),Value:: string()) -> {ok, #config_info_hhd{}} | {error, string() | [string()]}).
update_config_info_hhd(AssetId,Field,Value) ->
  Key = list_to_atom("config_" ++ integer_to_list(AssetId)),
  gen_server:call(?SERVER, {update_config_info_hhd, Key,Field,Value}).

-spec(get_single_config_info_hhd(Key:: any(),Field:: string()) -> {ok, #config_info_hhd{}} | {error, string() | [string()]}).
get_single_config_info_hhd(AssetId,Field) ->
  Key = list_to_atom("config_" ++ integer_to_list(AssetId)),
  gen_server:call(?SERVER, {get_single_config_info_hhd, Key,Field}).

-spec(mupdate_config_info_hhd(AssetId:: any(),KeyValuePairs :: string()) -> {ok, #config_info_hhd{}} | {error, string() | [string()]}).
mupdate_config_info_hhd(AssetId,KeyValuePairs) ->
  Key = list_to_atom("config_" ++ integer_to_list(AssetId)),
  gen_server:call(?SERVER, {mupdate_config_info_hhd, Key,KeyValuePairs}).


%%%%%%%%%%%%%%%%% shiva code end %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @end
%%--------------------------------------------------------------------
-spec(start_link(DBName :: atom(), WorkerOptions :: list()) ->
  {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link(redis, WorkerOptions) ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [WorkerOptions], []).

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
init([[Host,Port]] = WorkerOptions) ->
  ht_logger:info("Redis DB connect '~p'", [WorkerOptions]),
  {ok, Connection} = eredis:start_link(Host,Port),
  State = #state{connection = Connection},
  {ok, State}.

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

handle_call({get_pid}, _From, State) ->
  {reply, State#state.connection, State};

handle_call({create_live_data, Key, Rec}, _From, State) ->
  {reply, do_create_live_data(State,Key, Rec), State};

handle_call({create_alert_info, Key, Rec}, _From, State) ->
  {reply, do_create_alert_info(State,Key, Rec), State};

handle_call({check_alert_info, Key}, _From, State) ->
  {reply, do_check_alert_info(State,Key), State};

handle_call({delete_alert_info, Key}, _From, State) ->
  {reply, do_delete_alert_info(State,Key), State};

handle_call({get_idle_data, Key}, _From, State) ->
  {reply, get_idle_data(State,Key), State};

handle_call({get_geo_data, Key}, _From, State) ->
  {reply, get_geo_data(State,Key), State};

handle_call({get_geo_reject_data, Key}, _From, State) ->
  {reply, get_geo_reject_data(State,Key), State};

handle_call({get_full_data, Key}, _From, State) ->
  {reply, get_full_data(State,Key), State};

handle_call({get_lock_data, Key}, _From, State) ->
  {reply, get_lock_data(State,Key), State};

handle_call({get_hhd_data, Key}, _From, State) ->
  {reply, get_hhd_data(State,Key), State};

handle_call({get_valid_data, Key}, _From, State) ->
  {reply, get_valid_data(State,Key), State};

handle_call({get_previous_packet_time, Key}, _From, State) ->
  {reply, get_previous_packet_time(State,Key), State};

handle_call({get_low_battery_info, Key}, _From, State) ->
  {reply, get_low_battery_info(State,Key), State};

handle_call({update_low_battery_info, Key, Value}, _From, State) ->
  {reply, update_low_battery_info(State,Key,Value), State};

handle_call({get_critical_battery_info, Key}, _From, State) ->
  {reply, get_critical_battery_info(State,Key), State};

handle_call({update_critical_battery_info, Key, Value}, _From, State) ->
  {reply, update_critical_battery_info(State,Key,Value), State};

handle_call({get_geo_source_info, Key}, _From, State) ->
  {reply, get_geo_source_info(State,Key), State};

handle_call({update_geo_source_info, Key, Value}, _From, State) ->
  {reply, update_geo_source_info(State,Key,Value), State};

handle_call({update_single_field, Key,Field,Value}, _From, State) ->
  {reply, do_update_single_field(State,Key,Field,Value), State};

handle_call({get_single_field, Key,Field}, _From, State) ->
  {reply, do_get_single_field(State,Key,Field), State};

handle_call({get_geo_in_transit_info, Key}, _From, State) ->
  {reply,get_geo_in_transit_info(State,Key), State};

handle_call({update_geo_in_transit_info, Key, Value}, _From, State) ->
  {reply, update_geo_in_transit_info(State,Key,Value), State};

handle_call({get_geo_destination_info, Key}, _From, State) ->
  {reply, get_geo_destination_info(State,Key), State};

handle_call({update_geo_destination_info, Key, Value}, _From, State) ->
  {reply, update_geo_destination_info(State,Key,Value), State};

handle_call({get_geo_forbidden_info, Key}, _From, State) ->
  {reply, get_geo_forbidden_info(State,Key), State};

handle_call({update_geo_forbidden_info, Key, Value}, _From, State) ->
  {reply, update_geo_forbidden_info(State,Key,Value), State};

handle_call({get_idle_state_info, Key}, _From, State) ->
  {reply, get_idle_state_info(State,Key), State};

handle_call({update_idle_state_info, Key, Value}, _From, State) ->
  {reply, update_idle_state_info(State,Key,Value), State};

handle_call({get_lock_state_info, Key}, _From, State) ->
  {reply, get_lock_state_info(State,Key), State};

handle_call({update_lock_state_info, Key, Value}, _From, State) ->
  {reply, update_lock_state_info(State,Key,Value), State};

handle_call({get_history_speed_flag, Key}, _From, State) ->
  {reply, get_history_speed_flag(State,Key), State};

handle_call({update_history_speed_flag, Key, Value}, _From, State) ->
  {reply, update_history_speed_flag(State,Key,Value), State};

handle_call({get_card_info, Key}, _From, State) ->
  {reply, get_card_info(State,Key), State};

handle_call({get_time_info, Key}, _From, State) ->
  {reply, get_time_info(State,Key), State};

handle_call({update_card_info, Key, Value}, _From, State) ->
  {reply, update_card_info(State,Key,Value), State};

handle_call({update_time_info, Key, Value}, _From, State) ->
  {reply, update_time_info(State,Key,Value), State};

handle_call({add_card_info, Key}, _From, State) ->
  {reply, add_card_info(State,Key), State};

handle_call({add_time_info, Key}, _From, State) ->
  {reply, add_time_info(State,Key), State};

handle_call({add_card_count, Key}, _From, State) ->
  {reply, add_card_count(State,Key), State};

handle_call({update_card_count, Key, Value}, _From, State) ->
  {reply, update_card_count(State,Key,Value), State};

handle_call({get_card_count, Key}, _From, State) ->
  {reply, get_card_count(State,Key), State};

%%%%%%%%%%% shiva code start %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% 
handle_call({update_geofence_count, Key, Value}, _From, State) ->
  {reply, update_geofence_count(State,Key,Value), State};

handle_call({get_geofence_count, Key}, _From, State) ->
  {reply, get_geofence_count(State,Key), State};

handle_call({update_geofence_out_data, Key, Value}, _From, State) ->
  {reply, update_geofence_out_data(State,Key,Value), State};

handle_call({get_geofence_out_data, Key}, _From, State) ->
  {reply, get_geofence_out_data(State,Key), State};

handle_call({get_geo_type_and_geo_name, Key}, _From, State) ->
  {reply, get_geo_type_and_geo_name(State,Key), State};


%% web hhd comands
handle_call({create_config_info_hhd, Key, Rec}, _From, State) ->
  {reply, do_create_config_info_hhd(State,Key, Rec), State};

handle_call({get_config_info_hhd, Key}, _From, State) ->
  {reply, do_get_config_info_hhd(State,Key), State};

handle_call({update_config_info_hhd, Key,Field,Value}, _From, State) ->
  {reply, do_update_config_info_hhd(State,Key,Field,Value), State};

handle_call({get_single_config_info_hhd, Key,Field}, _From, State) ->
  {reply, do_get_single_config_info_hhd(State,Key,Field), State};

handle_call({mupdate_config_info_hhd, Key,KeyValuePairs}, _From, State) ->
  {reply, do_mupdate_config_info_hhd(State,Key,KeyValuePairs), State};

%%%%%%%%%s shiva code end %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

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

handle_info(_Info, State) ->
  {noreply, State}.

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
%%% Functions
%%%===================================================================

do_create_live_data(#state{connection = Pid},Key, Rec) ->
  ServerTime = ht_helper_time:timestamp(),
  DeviceTime = Rec#track.packet_time,
%  if
%      DeviceTime - 30  > ServerTime  ->
%        ht_logger:info("FUTURE DATA: Server Time ~w Device Time: ~w Device ID: ~w~n",[ServerTime, DeviceTime, Rec#track.asset_id]); 
%      DeviceTime < ServerTime  - 120 ->
%        ht_logger:info("HISTORY DATA: Server Time ~w Device Time: ~w Device ID: ~w~n",[ServerTime, DeviceTime, Rec#track.asset_id]); 
%      true ->
        %ht_logger:info("LIVE DATA: Server Time ~w Device Time: ~w Device ID: ~w~n",[ServerTime, DeviceTime, Rec#track.asset_id]) 
   %end, 
  Ignition = Rec#track.ignition_status,
  Speed = Rec#track.speed,
  NewRec = Rec#track{
    created = ServerTime,
    fixTime = fix_time(ServerTime, DeviceTime),
    ignition_status = get_vehicle_status (Ignition,Speed)},
  FieldList = record_info(fields, track),
  [_RecordName|ValueList] = tuple_to_list(NewRec),
  ValuePairs = make_pairs(FieldList,ValueList,[]),
  eredis:q(Pid, ["HMSET", Key | ValuePairs]).
 % end.

do_create_alert_info(#state{connection = Pid},Key, Rec) ->
  FieldList = record_info(fields, alert_info),
  [_RecordName|ValueList] = tuple_to_list(Rec),
  ValuePairs = make_pairs(FieldList,ValueList,[]),
  eredis:q(Pid, ["HMSET", Key | ValuePairs]).

do_check_alert_info(#state{connection = Pid},Key) ->
  case eredis:q(Pid, ["HGET", Key, alert_initiation_status]) of
    {ok,undefined} ->
      no_data_found;
    {ok,AlertStatus}   ->
      binary_to_atom(AlertStatus,utf8)
  end.

do_delete_alert_info(#state{connection = Pid},Key) ->
  case eredis:q(Pid, ["DEL", Key]) of
    {ok,<<"1">>} ->
      deleted;
    {ok,<<"0">>}   ->
      already_deleted
  end.

get_idle_data(#state{connection = Pid},Key) ->
  case eredis:q(Pid, ["HMGET", Key | [latitude,longitude,idle_time,packet_time,idle_state]]) of
    {ok,[Latitude,Longitude,MovingTime,PacketTime,<<"false">>]} ->
      {moving_state,
        [binary_to_float(Latitude),binary_to_float(Longitude),binary_to_integer(MovingTime),binary_to_integer(PacketTime)]};
    {ok,[Latitude,Longitude,IdleTime,PacketTime,<<"true">>]} ->
      {idle_state,
        [binary_to_float(Latitude),binary_to_float(Longitude),binary_to_integer(IdleTime),binary_to_integer(PacketTime)]};
    _ ->
      {idle_state,
        [0.0,0.0,0,0]}
  end.

do_update_single_field(#state{connection = Pid},Key,Field,Value) ->
  eredis:q(Pid, ["HSET", Key, Field, Value]).

do_get_single_field(#state{connection = Pid},Key,Field) ->
  case eredis:q(Pid, ["HGET", Key, Field]) of
    {ok,undefined} ->
      {error,undefined};
    {ok,AlertStatus} ->
      {ok,AlertStatus};
    Error ->
      {error,Error} 
  end.


get_geo_data(#state{connection = Pid},Key) ->
  case eredis:q(Pid, ["HMGET", Key | [latitude,longitude,packet_time]]) of
    {ok,[undefined,undefined,undefined]} ->
      {ok, [0.0,0.0,0]};
    {ok,[Latitude,Longitude,PacketTime]} ->
      {ok, [binary_to_float(Latitude),binary_to_float(Longitude),binary_to_integer(PacketTime)]};
    Error ->
      {error,Error}
  end.


get_geo_reject_data(#state{connection = Pid},Key) ->
  case eredis:q(Pid, ["HMGET", Key | [latitude,longitude,reject_packet_time,packet_time]]) of
    {ok,[undefined,undefined,undefined,undefined]} ->
      {error,undefined};
    {ok,[Latitude,Longitude,undefined,PacketTime]} ->
      {ok, [binary_to_float(Latitude),binary_to_float(Longitude),binary_to_integer(PacketTime)]};
    {ok,[Latitude,Longitude,RPacketTime,_]} ->
      {ok, [binary_to_float(Latitude),binary_to_float(Longitude),binary_to_integer(RPacketTime)]};
    Error ->
      {error,Error}
  end.


get_hhd_data(#state{connection = Pid},Key) ->
  case eredis:q(Pid, ["HMGET", Key| [lock_status,unlock_report,alarm_bit,remaining_batt]])  of
     {ok,undefined} ->
      {hhd,""};
    {ok,[Lock_status,Unlock_report,Battery_voltage,Remain]} ->
      {hhd, Lock_status,Unlock_report,Battery_voltage,Remain};
    Error ->
      {error,Error}
  end.

get_valid_data(#state{connection = Pid},Key) ->
  case eredis:q(Pid, ["HMGET", Key|[valid,validNo]])  of
    {ok,[undefined,undefined]} ->
      {error,undefined};
    {ok,[Valid,undefined]} ->
      {valid,Valid,<<"0">>};
    {ok,[Valid,ValidNo]} ->
      {valid, Valid,ValidNo};
    Error ->
      {error,Error}
  end.

get_lock_data(#state{connection = Pid},Key) ->
  case eredis:q(Pid, ["HMGET", Key| [lock_status,course]])  of
     {ok,[undefined,undefined]} ->
      {lock,<<"locked">>,<<"WEB">>};
    {ok,[Lock_status,Course]} ->
      {lock, Lock_status,Course};
    Error ->
      {error,Error}
  end.

get_previous_packet_time(#state{connection = Pid},Key) ->
  case eredis:q(Pid, ["HGET", Key, packet_time]) of
    {ok,undefined} ->
      {packet,[0]};
    {ok,PacketTime} ->
      {packet,[binary_to_integer(PacketTime)]};
    Error ->
      {error,Error}
  end.

get_full_data(#state{connection = Pid},Key) ->
  {ok, List} = eredis:q(Pid, ["HGETALL", Key]),
  List1 = make_unpairs(List,[]),
  list_to_tuple([track|[proplists:get_value(X, List1) || X <- record_info(fields, track)]]).

make_unpairs([],Result) -> Result;
make_unpairs([H1,H2|Tail],Result) ->
    make_unpairs(Tail,[{list_to_atom(binary_to_list(H1)),H2}] ++ Result).

get_low_battery_info(#state{connection = Pid},Key) ->
  case eredis:q(Pid, ["HGET", Key, low_battery_alert]) of
    {ok,undefined} ->
      {error,alert_not_initiated};
    {ok,LowBatteryAlert} ->
      binary_to_atom(LowBatteryAlert,utf8);
    Error ->
      {error,Error}
  end.

update_low_battery_info(#state{connection = Pid},Key,Value) ->
  eredis:q(Pid, ["HSET", Key, low_battery_alert, Value]).

get_critical_battery_info(#state{connection = Pid},Key) ->
  case eredis:q(Pid, ["HGET", Key, critical_battery_alert]) of
    {ok,undefined} ->
      {error,alert_not_initiated};
    {ok,CriticalBatteryAlert} ->
      binary_to_atom(CriticalBatteryAlert,utf8);
    Error ->
      {error,Error}
  end.

update_critical_battery_info(#state{connection = Pid},Key,Value) ->
  eredis:q(Pid, ["HSET", Key, critical_battery_alert, Value]).

get_geo_source_info(#state{connection = Pid},Key) ->
  case eredis:q(Pid, ["HGET", Key, geo_source_alert]) of
    {ok,undefined} ->
      {error,alert_not_initiated};
    {ok,GeoSourceAlert} ->
      binary_to_atom(GeoSourceAlert,utf8);
    Error ->
      {error,Error}
  end.

update_geo_source_info(#state{connection = Pid},Key,Value) ->
  eredis:q(Pid, ["HSET", Key, geo_source_alert, Value]).

get_geo_in_transit_info(#state{connection = Pid},Key) ->
  case eredis:q(Pid, ["HGET", Key, geo_in_transit_alert]) of
    {ok,undefined} ->
      {error,alert_not_initiated};
    {ok,GeoInTransitAlert} ->
      binary_to_atom(GeoInTransitAlert,utf8);
    Error ->
      {error,Error}
  end.

update_geo_in_transit_info(#state{connection = Pid},Key,Value) ->
  eredis:q(Pid, ["HSET", Key, geo_in_transit_alert, Value]).

get_geo_destination_info(#state{connection = Pid},Key) ->
  case eredis:q(Pid, ["HGET", Key, geo_destination_alert]) of
    {ok,undefined} ->
      {error,alert_not_initiated};
    {ok,GeoDestinationAlert} ->
      binary_to_atom(GeoDestinationAlert,utf8);
    Error ->
      {error,Error}
  end.

update_geo_destination_info(#state{connection = Pid},Key,Value) ->
  eredis:q(Pid, ["HSET", Key, geo_destination_alert, Value]).

get_geo_forbidden_info(#state{connection = Pid},Key) ->
  case eredis:q(Pid, ["HGET", Key, geo_forbidden_alert]) of
    {ok,undefined} ->
      {error,alert_not_initiated};
    {ok,GeoForbiddenAlert} ->
      binary_to_atom(GeoForbiddenAlert,utf8);
    Error ->
      {error,Error}
  end.

update_geo_forbidden_info(#state{connection = Pid},Key,Value) ->
  eredis:q(Pid, ["HSET", Key, geo_forbidden_alert, Value]).

get_idle_state_info(#state{connection = Pid},Key) ->
  case eredis:q(Pid, ["HGET", Key, idle_state_alert]) of
    {ok,undefined} ->
      {error,alert_not_initiated};
    {ok,IdleStateAlert} ->
      binary_to_atom(IdleStateAlert,utf8);
    Error ->
      {error,Error}
  end.

update_idle_state_info(#state{connection = Pid},Key,Value) ->
  eredis:q(Pid, ["HSET", Key, idle_state_alert, Value]).

get_lock_state_info(#state{connection = Pid},Key) ->
  case eredis:q(Pid, ["HGET", Key, device_lock_alert]) of
    {ok,undefined} ->
      {error,alert_not_initiated};
    {ok,IdleStateAlert} ->
      binary_to_atom(IdleStateAlert,utf8);
    Error ->
      {error,Error}
  end.

update_lock_state_info(#state{connection = Pid},Key,Value) ->
  eredis:q(Pid, ["HSET", Key, device_lock_alert, Value]).

get_history_speed_flag(#state{connection = Pid},Key) ->
  case eredis:q(Pid, ["HGET", Key, history_speed_flag]) of
    {ok,undefined} ->
      {error,alert_not_initiated};
    {ok,IdleStateAlert} ->
      binary_to_atom(IdleStateAlert,utf8);
    Error ->
      {error,Error}
  end.

update_history_speed_flag(#state{connection = Pid},Key,Value) ->
  eredis:q(Pid, ["HSET", Key, history_speed_flag, Value]).

get_card_info(#state{connection = Pid},Key) ->
  case eredis:q(Pid, ["HGET", Key, card_info]) of
    {ok,undefined} ->
      {error,alert_not_initiated};
    {ok,IdleStateAlert} ->
      binary_to_atom(IdleStateAlert,utf8);
    Error ->
      {error,Error}
  end.

get_time_info(#state{connection = Pid},Key) ->
  case eredis:q(Pid, ["HGET", Key, time_info]) of
    {ok,undefined} ->
      {error,alert_not_initiated};
    {ok,IdleStateAlert} ->
      binary_to_atom(IdleStateAlert,utf8);
    Error ->
      {error,Error}
  end.

update_card_info(#state{connection = Pid},Key,Value) ->
  eredis:q(Pid, ["HSET", Key, card_info, Value]).

update_time_info(#state{connection = Pid},Key,Value) ->
  eredis:q(Pid, ["HSET", Key, time_info, Value]).

add_card_info(#state{connection = Pid},Key) ->
  case eredis:q(Pid, ["HMSET", Key | [card_info,false]]) of
    {ok,<<"OK">>} ->
      success;
    _ ->
      add_card_info(#state{connection = Pid},Key)
  end.

add_time_info(#state{connection = Pid},Key) ->
  case eredis:q(Pid, ["HMSET", Key | [time_info,false]]) of
    {ok,<<"OK">>} ->
      success;
    _ ->
      add_time_info(#state{connection = Pid},Key)
  end.


add_card_count(#state{connection = Pid},Key) ->
  case eredis:q(Pid, ["HMSET", Key | [card_count,false]]) of
    {ok,<<"OK">>} ->
      success;
    _ ->
      add_card_count(#state{connection = Pid},Key)
  end.

update_card_count(#state{connection = Pid},Key,Value) ->
  eredis:q(Pid, ["HSET", Key, card_count, Value]).

get_card_count(#state{connection = Pid},Key) ->
  case eredis:q(Pid, ["HGET", Key, card_count]) of
    {ok,undefined} ->
      {error,alert_not_initiated};
    {ok,IdleStateAlert} ->
      binary_to_integer(IdleStateAlert);
    Error ->
      {error,Error}
  end.

%%%%%%%%%%%%%%%% shiva code start %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
update_geofence_count(#state{connection = Pid},Key,Value) ->
  eredis:q(Pid, ["HSET", Key, geofence_count, Value]).

get_geofence_count(#state{connection = Pid},Key) ->
  case eredis:q(Pid, ["HGET", Key, geofence_count]) of
    {ok,undefined} ->
      { geofence_count, 0};
    {ok, GeoFenceCount} ->
      { geofence_count, erlang:list_to_integer(GeoFenceCount)};
    Error ->
      {error,Error}
  end.

update_geofence_out_data(#state{connection = Pid},Key,ValueList) ->
	FieldList = [geofence_out_latitude,geofence_out_longitude],
	ValuePairs = make_pairs(FieldList,ValueList,[]),
	eredis:q(Pid, ["HMSET", Key | ValuePairs]).

get_geofence_out_data(#state{connection = Pid},Key) ->
  case eredis:q(Pid, ["HMGET", Key | [geofence_out_latitude,geofence_out_longitude]]) of
    {ok,[undefined,undefined]} ->
      {ok, [0.0,0.0]};
    {ok,[Latitude,Longitude]} ->
      {ok, [binary_to_float(Latitude),binary_to_float(Longitude)]};
    Error ->
      {error,Error}
  end.

get_geo_type_and_geo_name(#state{connection = Pid},Key) ->
  case eredis:q(Pid, ["HMGET", Key| [geo_type,geo_name]])  of
     {ok,[undefined,undefined]} ->
      {ok, "",""};
    {ok,[Geo_type,Geo_name]} ->
      {ok, [Geo_type,Geo_name]};
    Error ->
      {error,Error}
  end.

%% web commands config hhd
%%
do_create_config_info_hhd(#state{connection = Pid},Key, Rec) ->
  FieldList = record_info(fields, config_info_hhd),
  [_RecordName|ValueList] = tuple_to_list(Rec),
  ValuePairs = make_pairs(FieldList,ValueList,[]),
  eredis:q(Pid, ["HMSET", Key | ValuePairs]).

do_get_single_config_info_hhd(#state{connection = Pid},Key,Field) ->
  case eredis:q(Pid, ["HGET", Key, Field]) of
    {ok,<<"undefined">>} ->
      {ok,undefined};
    {ok,undefined} ->
      {ok,undefined};
    {ok,AlertStatus} ->
      {ok,AlertStatus};
    Error ->
      {error,Error}
  end.

do_get_config_info_hhd(#state{connection = Pid},Key) ->
  {ok, List} = eredis:q(Pid, ["HGETALL", Key]),
  List1 = make_unpairs(List,[]),
  list_to_tuple([config_info_hhd|[proplists:get_value(X, List1) || X <- record_info(fields, config_info_hhd)]]).

do_update_config_info_hhd(#state{connection = Pid},Key,Field,Value) ->
  eredis:q(Pid, ["HSET", Key, Field, Value]).

do_mupdate_config_info_hhd(#state{connection = Pid},Key,KeyValuePairs) ->
  eredis:q(Pid, ["HMSET", Key | KeyValuePairs]).

%%%%%%%%%%%%%%% shiva code end %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%===================================================================
%%% Internal functions - Make DB Key/Value
%%%===================================================================

make_pairs([],[],Result) -> Result;

make_pairs([Field|FList],[Value|VList],Acc) when is_float(Value) ->
  make_pairs(FList,VList,Acc ++ [Field,float_to_list(Value)]);

make_pairs([Field|FList],[Value|VList],Acc) ->
  make_pairs(FList,VList,Acc ++ [Field,Value]).

%%%===================================================================
%%% Internal functions - DB Operations
%%%===================================================================

fix_time(ServerTime, DeviceTime) when ServerTime < DeviceTime ->
  ServerTime;
fix_time(_, DeviceTime) ->
  DeviceTime.

get_vehicle_status(Ignition,_Speed) when Ignition == 0 ->
  <<"STOP">>;
get_vehicle_status(Ignition,Speed) when (Ignition == 1) and (Speed == 0) ->
  <<"IDLE">>;
get_vehicle_status(Ignition,Speed) when (Ignition == 1) and (Speed > 0) ->
  <<"MOTION">>;
get_vehicle_status(_, Speed) when Speed == 0 ->
  <<"STOP">>;
get_vehicle_status(_, Speed) when Speed > 0 ->
  <<"MOTION">>.

