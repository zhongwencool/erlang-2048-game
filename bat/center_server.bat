@echo off
erl -name game2048_center_server@127.0.0.1 -pa "..\\ebin\\" -setcookie game2048 -run game2048_center
