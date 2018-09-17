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
-export([build_ac/2, build_ac_file/2, data_ac/1, filter/2, check/2, replace/2, try_goto/3]).

build_ac(Name, Keys) ->
    spawn(?MODULE, build_ac_file, [Name, Keys]).

build_ac_file(Name, Keys) ->
    build_ac_goto_output(Keys),
    data_ac(Name).

%%检查字符串是否包含指定字符：返回包含的指定字符数组
filter(Mod, String) ->
    filter(Mod, String, []).

%%检查字符串是否包含非法字符：返回ok表示通过，返回error表示没通过
check(_, []) ->
    ok;
check(Mod, [Char | String]) ->
    case try_goto(Mod, [Char | String], "") of
        fail ->
            check(Mod, String);
        {ok, _, _} ->
            error
    end.

replace(Mod, String) ->
    replace(Mod, String, "").

%% ====================================================================
%% Internal functions
%% ====================================================================
data_ac(Name0) ->
    Name = atom_to_list(Name0),
    FileName = "./src/filter/" ++ Name ++ ".erl",
    file:delete(FileName),
    io:format("start write file:~p~n", [Name]),
    write_file(FileName, "-module("++Name++").\n-export([goto/2,output/2]).\n"),
    Goto = [{Head, Char, Key} || {{goto, {Head, Char}}, Key} <- get()],
    Output = [{Head, Char, Key} || {{output, {Head, Char}}, Key} <- get()],
    Total = length(Goto) + length(Output),
    GotoC = lists:foldl(fun({Head, Char, Key}, C) ->
                                write_file(FileName, "\ngoto(\""++Head++"\","++integer_to_list(Char)++")-> \""++Key++"\";"),
                                erase({goto, {Head, Char}}),
                                io:format("write progress:~p/~p~n", [C, Total]),
                                C + 1
                        end, 1, Goto),
    write_file(FileName, "\ngoto(_,_)-> fail."),
    lists:foldl(fun({Head, Char, Key}, C) ->
                        write_file(FileName, "\noutput(\""++Head++"\", "++integer_to_list(Char)++")-> \""++Key++"\";"),
                        erase({output, {Head, Char}}),
                        io:format("write progress:~p/~p~n", [C, Total]),
                        C + 1
                end, GotoC, Output),
    write_file(FileName, "\noutput(_,_)-> fail."),
    io:format("write file:~p finished!~n", [Name]).

build_ac_goto_output([]) ->
    ok;
build_ac_goto_output([Key | Keys]) ->
    build_ac_goto_output1(Key, ""),
    build_ac_goto_output(Keys).

build_ac_goto_output1([], _) ->
    ok;
build_ac_goto_output1([Char], Head) ->
    Key = Head ++ [Char],
    put({goto, {Head, Char}}, Key),
    put({output, {Head, Char}}, Key),
    ok;
build_ac_goto_output1([Char|RChar], Head) ->
    Key = Head ++ [Char],
    put({goto, {Head, Char}}, Key),
    build_ac_goto_output1(RChar, Key).

filter(_, [], Acc) ->
    Acc;
filter(Mod, [Char | String], Acc) ->
    case try_goto(Mod, [Char | String], "") of
        fail ->
            filter(Mod, String, Acc);
        {ok, Output, NewString} ->
            filter(Mod, NewString, [Output | Acc])
    end.

replace(_, [], Acc) ->
    Acc;
replace(Mod, [Char | String], Acc) ->
    case try_goto(Mod, [Char | String], "") of
        fail ->
            replace(Mod, String, Acc ++ [Char]);
        {ok, Output, NewString} ->
            replace(Mod, NewString, Acc ++ [$* || _ <- Output])
    end.

try_goto(_, [], _) ->
    fail;
try_goto(Mod, [Char | String], Head) ->
    case Mod:goto(Head, Char) of
        fail ->
            case Mod:output(Head, Char) of
                fail ->
                    fail;
                NewHead ->
                    {ok, NewHead, String}
            end;
        NewHead ->
            case try_goto(Mod, String, NewHead) of
                fail ->
                    case Mod:output(Head, Char) of
                        fail ->
                            fail;
                        NewHead ->
                            {ok, NewHead, String}
                    end;
                Res ->
                    Res
            end
    end.

write_file(FileName, Data) ->
    file:write_file(FileName, unicode:characters_to_binary(Data), [append]).