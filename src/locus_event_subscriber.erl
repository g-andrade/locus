%% Copyright (c) 2017-2018 Guilherme Andrade
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

%% @doc
%% Callbacks:
%% <ul>
%% <li>`report(DatabaseId :: atom(), Event :: event()) -> ok'</li>
%% </ul>

-module(locus_event_subscriber).

%% ------------------------------------------------------------------
%% Callback Definitions
%% ------------------------------------------------------------------

-callback report(DatabaseId, Event) -> ok
        when DatabaseId :: atom(),
             Event :: event().

-ignore_xref({behaviour_info,1}).

%% ------------------------------------------------------------------
%% Type Definitions
%% ------------------------------------------------------------------

-type event() ::
        locus_http_loader:event() |
        locus_filesystem_loader:event().
-export_type([event/0]).
