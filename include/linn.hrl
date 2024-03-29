-author("amoein").

-record(option, {
    pool_id :: atom(),
    handler_module :: atom(),
    handler_func :: atom(),
    handler_func_arity :: tuple(),
    process_count :: integer()
}).

-type option() :: #option{}.

-ifdef(TEST).

-define(LOG_ERROR(Format, Args), ct:print(default, 10, Format, Args)).
-define(LOG_INFO(Format, Args), ct:print(default, 30, Format, Args)).
-define(LOG_DEBUG(Format, Args), ct:print(default, 50, Format, Args)).

-else.

-define(LOG_ERROR(Format, Args), logger:log(error, Format, Args)).
-define(LOG_INFO(Format, Args), logger:log(info, Format, Args)).
-define(LOG_DEBUG(Format, Args), logger:log(debug, Format, Args)).

-endif.
