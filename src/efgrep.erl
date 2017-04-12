%%#!/usr/bin/env escript

-module(efgrep).
-export([main/1]).

-define(BUFSIZ, 1024).

usage() ->
	io:format("usage: efgrep [-ln][-m max] string [file ...]~n"),
	io:format("-l\t\tlist files with matching lines~n"),
	io:format("-m max\t\tmax. number of pattern match errors~n"),
	io:format("-n\t\tnumbers the output lines~n"),
	halt(2).

main(Args) ->
	case egetopt:parse(Args, [
		{ $l, flag, list_file_match },
		{ $m, param, max_mismatches },
		{ $n, flag, line_numbers }
	]) of
	{ok, _Options, []} ->
		usage();
	{ok, Options, [Pattern | Files]} ->
		process(Options, list_to_binary(Pattern), Files);
	{error, Reason, Opt} ->
		io:format("~s -~c~n", [Reason, Opt]),
		usage()
	end.

process(Opts, Pattern, []) ->
	io:setopts(standard_io, [binary]),
	fgrep("-", standard_io, Pattern, Opts);
process(Opts, Pattern, Files) ->
	process_files(Opts, Pattern, Files).

process_files(_Opts, _Pattern, []) ->
	ok;
process_files(Opts, Pattern, [File | Rest]) ->
	try
		Fp = open_file(File),
		fgrep(File, Fp, Pattern, Opts),
		file:close(Fp)
	catch
		throw:{error, Reason} ->
			io:format(standard_error, "efgrep: ~s: ~s~n", [File, str:error(Reason)]),
			halt(1)
	end,
	process_files(Opts, Pattern, Rest).

open_file("-") ->
        io:setopts(standard_io, [binary]),
	standard_io;
open_file(File) ->
	case file:open(File, [read, binary, {read_ahead, ?BUFSIZ}]) of
	{ok, Fp} ->
		Fp;
	Error ->
		throw(Error)
	end.

fgrep(File, Fp, Pattern, Opts) ->
	MaxErr = list_to_integer(proplists:get_value(max_mismatches, Opts, "0")),
	{_, _, DeltaMap} = sunday:init(Pattern, MaxErr),
	fgrep(Fp, Pattern, MaxErr, DeltaMap, which_putline(Opts, File), 1).
fgrep(Fp, Pattern, MaxErr, DeltaMap, Putline, Lineno) ->
	case file:read_line(Fp) of
	eof ->
		eof;
	{ok, Line} ->
		case sunday:search(Line, {Pattern, MaxErr, DeltaMap}) of
		-1 ->
			ok;
		_Index ->
			Putline(Line, Lineno)
		end,
		fgrep(Fp, Pattern, MaxErr, DeltaMap, Putline, Lineno+1);
	Error ->
		throw(Error)
	end.

which_putline(Opts, File) ->
	Putline = case proplists:get_value(line_numbers, Opts, false) of
	true ->
		fun (Line, Lineno) ->
			io:format("~B:", [Lineno]),
			file:write(standard_io, Line)
		end;
	false ->
		fun (Line, _Lineno) ->
			file:write(standard_io, Line)
		end
	end,
	case proplists:get_value(list_file_match, Opts, false) of
	true ->
		fun (Line, Lineno) ->
			io:format("~s:", [File]),
			Putline(Line, Lineno)
		end;
	false ->
		Putline
	end.
