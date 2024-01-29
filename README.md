# Battleship-Haskell
This project was developed as part of my Bachelor's Degree. Despite being rudimentary and in a not so common language (Haskell), I consider it might be useful. Detailed comments on code are in spanish. However, I hope the following instructions let it be clearful:

Game: Battleship

Initialize the game:
To start playing, you need to call the play function. Once called, it will ask you if you want to load an existing game. If you type the letter "c", you will have the option to enter the name of the game you want to load. If not, by pressing enter (or typing any other letter), you start a new game. Now it will ask for the size of the board (between 5 and 15), and once indicated, it will generate the ships on the boards and show both, where you make shots and where your ships (B) are located.

During the game:
In each turn, the state of your board will appear, with the position of your ships, and the moves that the machine is making. Ships are indicated on the screen with "B", water with ".", hits to a ship with "X", and misses with "O". When you make a shot, you will indicate with the first coordinate how many spaces to the right you move and with the second how many spaces down. Both coordinates start from 1 and are entered without parentheses (if you enter 1 5, you will be shooting at the bottom left corner). Every time you make a shot, you will have the option to save and exit the game by pressing the letter "g". If you do, you will be prompted to name the game. If not, press enter (or type any other letter) to continue the game. Every time the machine sinks one of your ships, the message "¡Ciao barco!" will appear. On the other hand, every time you sink one of the machine's ships, the number of squares left to sink the rest of the machine's ships will appear. As a "turn separator" (a turn begins when the player indicates a coordinate to shoot), the message appears: ---------------------------------------------------------¡TURNO DE LOS DISPAROS!---------------------------------------------------------

End of the game:
Whether you win or lose, a message indicating it will appear, and you will immediately exit the game.
