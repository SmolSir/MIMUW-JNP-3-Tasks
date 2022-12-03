# Sokoban!
Main project for the MIMUW-JNP-3 Haskell course (winter 2022/23) about recreating the [Sokoban](https://en.wikipedia.org/wiki/Sokoban) game in Haskell, starting from scratch.

## Play the game

### Full version
Visit the [Codeworld](https://code.world/haskell#) webpage, copy & paste the `codeworld/Sokoban.hs` file from `main` branch into the editor and press `Run`.

### Console version
With the [Haskell compiler](https://www.haskell.org/ghcup/) installed, just run the `console/Sokoban.hs` file from `main` branch.

## Contribute new mazes
Feel free to fork this repository and add the new level(s) either as a separate file, or as modification of appropriate, existing files. The mazes will undergo correctness tests before merging, but it would be great if you ensured that the maze is indeed possible to complete. Also try to keep the level difficulty in the `mazes` list on a linear scale, so that it ramps up throughout the game.

## Bug reports
If you happened to find any bugs in either of the game versions, please create an Issue for it with a description of the problem you run into.
