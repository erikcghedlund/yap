%%%-------------------------------------------------------------------
%% @doc yap public API
%% @end
%%%-------------------------------------------------------------------

-module(yap_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    yap_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
