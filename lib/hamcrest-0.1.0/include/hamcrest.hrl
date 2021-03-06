%% -----------------------------------------------------------------------------
%%
%% Hamcrest Erlang.
%%
%% Copyright (c) 2010 Tim Watson (watson.timothy@gmail.com)
%%
%% Permission is hereby granted, free of charge, to any person obtaining a copy
%% of this software and associated documentation files (the "Software"), to deal
%% in the Software without restriction, including without limitation the rights
%% to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
%% copies of the Software, and to permit persons to whom the Software is
%% furnished to do so, subject to the following conditions:
%%
%% The above copyright notice and this permission notice shall be included in
%% all copies or substantial portions of the Software.
%%
%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
%% IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
%% FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
%% AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
%% LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
%% OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
%% THE SOFTWARE.
%% -----------------------------------------------------------------------------
%% @author Tim Watson <watson.timothy@gmail.com>
%% @copyright 2010 Tim Watson.
%% -----------------------------------------------------------------------------

-include_lib("hamcrest/include/hamcrest_internal.hrl").

-ifdef(EUNIT_HRL).
%% TODO: notify hamcrest that we are using eunit!?
-endif.

-ifndef(assertThat).
-define(assertThat(Value, MatchSpec),
    ((fun () ->
        case (hamcrest:assert_that(Value, MatchSpec)) of
            true -> true;
            __V -> erlang:error({assertion_failed,
                      [{module, ?MODULE},
                       {line, ?LINE},
                       {expected, true},
                       {value, case __V of false -> __V;
                           _ -> {not_a_boolean,__V}
                           end}]})
        end
      end)())).
-define(_assertThat(Value, MatchSpec),
fun() ->
    case (hamcrest:assert_that(Value, MatchSpec)) of
        true -> true;
        __V -> erlang:error({assertion_failed,
                  [{module, ?MODULE},
                   {line, ?LINE},
                   {expected, true},
                   {value, case __V of false -> __V;
                       _ -> {not_a_boolean,__V}
                       end}]})
    end
end).
-define(assertThat(Value, MatchSpec, After),
    ((fun () ->
        case (hamcrest:assert_that(Value, MatchSpec, After)) of
        true -> true;
        __V -> erlang:error({assertion_failed,
                      [{module, ?MODULE},
                       {line, ?LINE},
                       {expected, true},
                       {value, case __V of false -> __V;
                           _ -> {not_a_boolean,__V}
                           end}]})
        end
      end)())).
-endif.

-import(hamcrest, [assert_that/2, assert_that/3, match/2, match/3]).
-import(hamcrest_matchers, [module_info/1, module_info/0, check_isempty/1, isempty/0, check_member/2, contains_member/1, has_same_contents_as/1, has_length/1, isdead/0, isalive/0, reverse_match_mfa/4, reverse_match_mfa/3, match_mfa/4, match_mfa/3, matches_regex/1, ends_with/1, starts_with/1, contains_string/1, less_than_or_equal_to/1, less_than/1, greater_than_or_equal_to/1, greater_than/1, is_not/1, is_false/0, is_true/0, is/1, exactly_equal_to/1, equal_to/1, all_of/1, any_of/1, foreach/1, anything/0, will_fail/2, will_fail/0]).
