-module(riaknostic_opts).

-export([parse/1]).

parse(Args) ->
    parse(Args, []).

parse([], Options) ->
    Fun = fun(Args) -> lists:reverse(Args) end,
    update(args, Options, [], Fun);

parse(["-dir", Dir|Args], Options) ->
    Options1 = update(dirs, Options, [], fun(Dirs) -> [Dir|Dirs] end),
    parse(Args, Options1);

parse(["-bitcask_threshold", Size|Args], Options) ->
    Options1 = update(bitcask_threshold, Options, [], fun(_) -> list_to_integer(Size) end),
    parse(Args, Options1);

parse([Arg|Args], Options) ->
    Fun = fun(Others) -> [Arg|Others] end,
    Options1 = update(args, Options, [], Fun),
    parse(Args, Options1).

update(Key, Proplist, Default, Fun) ->
    Value = proplists:get_value(Key, Proplist, Default),
    Value1 = Fun(Value),
    Proplist1 = proplists:delete(Key, Proplist),
    [{Key, Value1}|Proplist1].

