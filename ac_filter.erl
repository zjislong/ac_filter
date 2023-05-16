%%%-------------------------------------------------------------------
%%% @author zhengjia
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 11. 四月 2018 04:54
%%%-------------------------------------------------------------------
-module(ac_filter).

-author("zhengjia").

%% API
-export([
    build/1,
    filter/1,
    check/1,
    replace/1,
    insert/2,
    get_childs/2
]).

build(Keys) ->
    Trie = build_trie(Keys, #{}),
    build_fail(Trie).

%%检查字符串是否包含指定字符：返回包含的指定字符数组
filter(String) ->
    Trie = get(trie),
    filter(String, [], Trie, [], []).

%%检查字符串是否包含非法字符：返回true表示通过，返回false表示没通过
check([]) ->
    true;
check([Char | String]) ->
    Trie = get(trie),
    check(String, [Char], Trie, []).

replace(String) ->
    Trie = get(trie),
    replace(String, [], Trie, [], []).

%% ====================================================================
%% Internal functions
%% ====================================================================
build_trie([], Trie) ->
    put(trie, Trie),
    Trie;
build_trie([Key | Keys], Trie) ->
    Trie1 = insert(Key, Trie),
    build_trie(Keys, Trie1).

build_fail(Trie) ->
    Keys0 = maps:keys(Trie),
    Keys = [[Key] || Key <- Keys0],
    build_fail(Keys, Trie).

build_fail([], _) ->
    ok;
build_fail([Key | Keys], Trie) ->
    FailKey = get_fail(Key),
    {ok, Childs} = get_childs(Key, Trie),
    F = fun
        (-1) ->
            ok;
        (Child) ->
            case build_fail(Child, FailKey, Trie) of
                [] -> ok;
                ChildFailKey -> put({fail, Key ++ [Child]}, ChildFailKey)
            end
    end,
    lists:foreach(F, Childs),
    build_fail(Keys ++ [Key ++ [Char] || Char <- Childs, Char =/= -1], Trie).

build_fail(Child, [], Trie) ->
    TempKey = [Child],
    case get_childs(TempKey, Trie) of
        {ok, _} ->
            TempKey;
        false ->
            []
    end;
build_fail(Child, FailKey, Trie) ->
    TempKey = FailKey ++ [Child],
    case get_childs(TempKey, Trie) of
        {ok, _} ->
            TempKey;
        false ->
            FailKey1 = get_fail(FailKey),
            build_fail(Child, FailKey1, Trie)
    end.

filter([], _, _, _, Filter) ->
    Filter;
filter([Char | String], Head, Trie, [], Filter) ->
    case status(Head, Trie) of
        0 ->
            Head1 = get_fail(Head),
            filter([Char | String], Head1, Trie, [], Filter);
        1 ->
            filter(String, Head ++ [Char], Trie, [], Filter);
        2 ->
            filter(String, Head ++ [Char], Trie, Head, Filter)
    end;
filter([Char | String], Head, Trie, Success, Filter) ->
    case status(Head, Trie) of
        0 ->
            filter(String, [Char], Trie, [], [Success | Filter]);
        1 ->
            filter(String, Head ++ [Char], Trie, Success, Filter);
        2 ->
            filter(String, Head ++ [Char], Trie, Head, Filter)
    end.

replace([], _, _, _, Replaced) ->
    lists:reverse(Replaced);
replace([Char | String], Head, Trie, [], Replaced) ->
    case status(Head, Trie) of
        0 ->
            Head1 = get_fail(Head),
            {_, Replaced1} = lists:foldl(
                fun(H, {S, Acc}) ->
                    case S of
                        0 -> {S, Acc};
                        _ -> {S - 1, [H | Acc]}
                    end
                end,
                {length(Head) - length(Head1), Replaced},
                Head
            ),
            replace([Char | String], Head1, Trie, [], Replaced1);
        1 ->
            replace(String, Head ++ [Char], Trie, [], Replaced);
        2 ->
            replace(String, Head ++ [Char], Trie, Head, Replaced)
    end;
replace([Char | String], Head, Trie, Success, Replaced) ->
    case status(Head, Trie) of
        0 ->
            {_, Replaced1} = lists:foldl(
                fun(H, {S, Acc}) ->
                    case S of
                        [] -> {S, [H | Acc]};
                        [_ | S1] -> {S1, [42 | Acc]}
                    end
                end,
                {Success, Replaced},
                Head
            ),
            replace(String, [Char], Trie, [], Replaced1);
        1 ->
            replace(String, Head ++ [Char], Trie, Success, Replaced);
        2 ->
            replace(String, Head ++ [Char], Trie, Head, Replaced)
    end.

check([], _, _, _) ->
    true;
check([Char | String], Head, Trie, []) ->
    case status(Head, Trie) of
        0 ->
            Head1 = get_fail(Head),
            check([Char | String], Head1, Trie, []);
        1 ->
            check(String, Head ++ [Char], Trie, []);
        2 ->
            check(String, Head ++ [Char], Trie, Head)
    end;
check([Char | String], Head, Trie, Success) ->
    case status(Head, Trie) of
        0 ->
            false;
        1 ->
            check(String, Head ++ [Char], Trie, Success);
        2 ->
            check(String, Head ++ [Char], Trie, Head)
    end.

insert([], Trie) ->
    Trie;
insert([Char], Childs) ->
    CharChilds = maps:get(Char, Childs, #{}),
    Childs#{Char => CharChilds#{-1 => ok}};
insert([Char | RChar], Childs) ->
    CharChilds = maps:get(Char, Childs, #{}),
    Childs#{Char => insert(RChar, CharChilds)}.

get_childs([], Childs) ->
    {ok, maps:keys(Childs)};
get_childs([Char | RChar], Childs) ->
    case maps:get(Char, Childs, undefined) of
        undefined -> false;
        CharChilds -> get_childs(RChar, CharChilds)
    end.

get_fail(Key) ->
    case get({fail, Key}) of
        undefined -> [];
        FailKey -> FailKey
    end.

status(Key, Trie) ->
    case get_childs(Key, Trie) of
        {ok, Childs} ->
            case lists:member(-1, Childs) of
                true ->
                    2;
                false ->
                    1
            end;
        false ->
            0
    end.
