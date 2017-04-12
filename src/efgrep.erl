%%#!/usr/bin/env escript

-module(efgrep).
-export([main/1]).

-define(BUFSIZ, (16*1024)).

usage() ->
	io:format("usage: efgrep [-Fln][-k max] string [file ...]~n"),
	io:format("-F\t\tframe the found string in square brackets~n"),
	io:format("-l\t\tlist files with a matching line ~n"),
	io:format("-k max\t\tmax. number of pattern mismatches~n"),
	io:format("-n\t\tnumber the output lines~n"),
	halt(2).

main(Args) ->
	case egetopt:parse(Args, [
		{ $F, flag, frame_match },
		{ $l, flag, list_file_match },
		{ $k, param, max_mismatches },
		{ $n, flag, line_numbers }
	]) of
	{ok, _Options, []} ->
		usage();
	{ok, Options, [Pattern | Files]} ->
		Opts = [{one_file, length(Files) == 1} | Options],
		process(Opts, list_to_binary(Pattern), Files);
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
		Continue = case sunday:search(Line, {Pattern, MaxErr, DeltaMap}) of
		-1 ->
			ok;
		Index ->
			Putline(Line, Lineno, Pattern, Index)
		end,
		case Continue of
		ok ->
			fgrep(Fp, Pattern, MaxErr, DeltaMap, Putline, Lineno+1);
		Other ->
			Other
		end;
	Error ->
		throw(Error)
	end.

which_putline(Opts, File) ->
	Frame = case proplists:get_value(frame_match, Opts, false) of
	true ->
		fun (Line, _Lineno, Pattern, Index) ->
			PatLen = byte_size(Pattern),
			<<Lead:Index/bytes, Matched:PatLen/bytes, Trail/binary>> = Line,
			file:write(standard_io, <<Lead/binary, $[, Matched/binary, $], Trail/binary>>)
		end;
	false ->
		fun (Line, _Lineno, _Pattern, _Index) ->
			file:write(standard_io, Line)
		end
	end,

	Putline = case proplists:get_value(line_numbers, Opts, false) of
	true ->
		fun (Line, Lineno, Pattern, Index) ->
			io:format("~B:", [Lineno]),
			Frame(Line, Lineno, Pattern, Index)
		end;
	false ->
		fun (Line, Lineno, Pattern, Index) ->
			Frame(Line, Lineno, Pattern, Index)
		end
	end,

	Putname = case proplists:get_value(one_file, Opts, false) of
	false ->
		fun (Line, Lineno, Pattern, Index) ->
			io:format("~s:", [File]),
			Putline(Line, Lineno, Pattern, Index)
		end;
	true ->
		Putline
	end,

	case proplists:get_value(list_file_match, Opts, false) of
	true ->
		fun (_Line, _Lineno, _Pattern, _Index) ->
			io:format("~s~n", [File]),
			nextfile
		end;
	false ->
		Putname
	end.
