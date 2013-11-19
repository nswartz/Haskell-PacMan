Haskell-PacMan
==============
Primitive PacMan map parser coded in Haskell

To run the program:

-> ghc --make .\csce322a2p*.hs
-> ./csce322a2p*.hs args

where args is a correctly formatted map (included in tests).

I assumed that all IO was handled correctly, as the code for that was provided.

I also assumed based on the requirements document, that if a ghost could not move the shortest 
distance to PacMan, it would choose the next shortest as long as that movement did not increase 
it's distance from PacMan. The logic stops there, so ghosts will not go around obstacles
if one lies directly (in the same row or column) between them and PacMan.

Outside resources:

http://learnyouahaskell.com/
http://stackoverflow.com/
