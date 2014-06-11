**0 . prework**

    Windows intall erlang and set erlang PATH.

**1. config**

- change your center name in game2048.hrl, format as : game2048_center@IP
- for example: game2048_center@127.0.0.1 
	
		change ./bat/center_server.bat node name       : game2048_center@IP
	

**2. compile**

		
		double click ./bat/compile.bat
     
**3.  set center_server up**
     	
	double click ./bat/center_server.bat
  
**4. run game2048 client**

	double click ./bat/player1.bat
**5. Tip**
	
-   5.1 press down 
   
    	|  Left:A | Right:D | Up:W  | Down:S  |

- 	5.2 Save game : File -> Save game
- 	5.3 play game with other : online -> signup (set a new name) -> click "<-" button right to begin

- 	5.4 chat with other : write msg in blew dialogue, print Enter to send msg.

-------------------------------------------------------------------------
If you just want to play alone, you can :
	
	erl -name test -pa "./ebin/"
    >application:start(game2048).
    >appliccation:stop(game2048).

---------------------------------------------------------------------------
Find work flow in  **[Wiki](https://github.com/zhongwencool/2048/wiki)**

 Html Doc generate by Erlang Edoc :**[Document](https://github.com/zhongwencool/2048/tree/master/doc)**
![gamepic](http://zhongwencool.qiniudn.com/erlang2048.png)