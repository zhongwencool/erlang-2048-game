@echo off
erl -name player1@127.0.0.1 -pa "..\\ebin\\" -setcookie game2048 -run game2048