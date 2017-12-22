%% Copyright (c) 2017 Guilherme Andrade <locus.lib@gandrade.net>
%%
%% Permission is hereby granted, free of charge, to any person obtaining a
%% copy  of this software and associated documentation files (the "Software"),
%% to deal in the Software without restriction, including without limitation
%% the rights to use, copy, modify, merge, publish, distribute, sublicense,
%% and/or sell copies of the Software, and to permit persons to whom the
%% Software is furnished to do so, subject to the following conditions:
%%
%% The above copyright notice and this permission notice shall be included in
%% all copies or substantial portions of the Software.
%%
%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
%% IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
%% FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
%% AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
%% LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
%% FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
%% DEALINGS IN THE SOFTWARE.
%%
%% locus is an independent project and has not been authorized, sponsored,
%% or otherwise approved by MaxMind.

%-define(log_debug(Fmt, Args),     ?___log(debug, Fmt, Args)).
%-define(log_info(Fmt, Args),      ?___log(info, Fmt, Args)).
%-define(log_notice(Fmt, Args),    ?___log(notice, Fmt, Args)).
%-define(log_warning(Fmt, Args),   ?___log(warning, Fmt, Args)).
%-define(log_error(Fmt, Args),     ?___log(error, Fmt, Args)).
%-define(log_critical(Fmt, Args),  ?___log(critical, Fmt, Args)).
%-define(log_alert(Fmt, Args),     ?___log(alert, Fmt, Args)).
%-define(log_emergency(Fmt, Args), ?___log(emergency, Fmt, Args)).

%-define(___log(Level, Fmt, Args),
%        (locus_logger:should_log((Level)) andalso lager:(Level)((Fmt), (Args)))).

-define(log_info(Fmt, Args),      ?___log(info, info_msg, Fmt, Args)).
-define(log_warning(Fmt, Args),   ?___log(warning, warning_msg, Fmt, Args)).
-define(log_error(Fmt, Args),     ?___log(error, error_msg, Fmt, Args)).

-define(___log(Level, Fun, Fmt, Args),
        (locus_logger:should_log((Level)) andalso error_logger:(Fun)(("[locus] " ++ (Fmt) ++ "~n"), (Args)))).
