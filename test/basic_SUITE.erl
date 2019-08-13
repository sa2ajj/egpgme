-module(basic_SUITE).
-include_lib("common_test/include/ct.hrl").

-export([
    all/0,
    init_per_testcase/2,
    end_per_testcase/2
]).

-export([
    t_version/1,
    t_data/1,
    t_engine_info/1,
    run_keylist/1,
    run_export/1,
    run_import/1,
    run_sign/1,
    run_verify/1
]).

all() -> [
    t_version,
    t_data,
    t_engine_info,
    run_keylist,
    run_export,
    run_import,
    run_sign,
    run_verify
].

init_per_testcase(_, Config) ->
    Dir = ?config(priv_dir, Config),
    os:putenv("GNUPGHOME", Dir),
    io:format("PRIV DIR=~p~n", [ ?config(priv_dir, Config) ]),
    io:format("SEARCH DIR=~p~n", [Dir]),
    Config_2  = lists:keyreplace(priv_dir, 1, Config, {priv_dir,"./priv"}),
    io:format("Config_2 = ~p~n", [Config_2]),
    Config_2.

end_per_testcase(_, _Config) ->
    ok.

run_export(_Config) ->
    {ok, Context} = egpgme:context(),
    Keys = collect_keys(Context, egpgme:keylist_first(Context, []), []),
    io:format("Found keys: ~p~n", [Keys]),
    Data = egpgme:data(),
    egpgme:context_set_armor(Context, true),
    Mode = 1,   % Checkout how to set the right mode
    ok = egpgme:export_keys(Context, Keys, Mode, Data),
    io:format("Result: ~p~n", [egpgme:data_to_binary(Data)]),
    ok.

collect_keys(_Context, '$no_more_keys', Acc) ->
    lists:reverse(Acc);
collect_keys(Context, {ok, Key}, Acc) ->
    collect_keys(Context, egpgme:keylist_next(Context), [Key | Acc]);
collect_keys(_, {error, _}=Error, Acc) ->
    io:format("Collected so far: ~p~n", [Acc]),
    error(Error).

run_import(_Config) ->
    ok.

run_keylist(_Config) ->
    ok.

run_sign(_Config) ->
    ok.

run_verify(_Config) ->
    ok.

t_data(_Config) ->
    ok.

t_engine_info(_Config) ->
    ok.

t_version(_Config) ->
    ok.
