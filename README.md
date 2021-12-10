# Haskell-Battleship-Game
Implements two-player game by guessing where 3 ships are located on a 4x8 grid and providing feedback on the accuracy of guesses.

Given target of three ship locations on grid, program makes an initial guess of
where the ships are located and enumerates all possible targets. Feedback on 
the accuracy of the guess is returned in the form of a triple:
    ( Number of correct guess locations
    , Number of guess locations exactly one space away from a target location,
    , Number of guess locations exactly two spaces away from a target location
    )
The program uses this feedback to eliminate targets from the list of possible 
targets, and calculates the expected number of remaining possible targets for 
each potential guess. The guess with the smallest expected number of remaining 
possible targets is taken as the next guess. The program repeats until the 
guess matches the target exactly. 

To run this program in Grok, load the files and run the game loop using the
following ghci commands,
> :l Main
> main
