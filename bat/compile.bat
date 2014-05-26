@echo off
erlc -o ../ebin/ ../deps/mmake.erl
erl -pa ../ebin/ -noinput -eval "case mmake:all(16,[{d, 'dummmmmy'}]) of up_to_date -> halt(0); error -> halt(1) end."
pause