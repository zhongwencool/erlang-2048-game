SHELL := /bin/bash
.PHONY: all mmake build clean
all:mmake build
mmake:
	erl -pa ../ebin/  -noinput \
	-eval "case make:files([\"../deps/mmake.erl\"], [debug_info, {outdir, \"../ebin\"}]) of error -> halt(1); _ -> halt(0) end"

build:
	erl -pa ../ebin/ -noinput -eval "case mmake:all(16,[{d, 'dummmmmy'}]) of up_to_date -> halt(0); error -> halt(1) end."
clean:
	@echo "clean ebin/*.beam"
	(rm -rf ../ebin/*.beam)
