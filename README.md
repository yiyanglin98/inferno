# inferno
A text-based adventure game built using Clojure. This is the final project for CS296-25 Fall 2019 at UIUC.
## How to run
Please run the following command
```
lein run
```
## Game guide
If you want to move
```
n/north/go north, and the four canonical directions (synonyms are supported).
```
If you want to quit the game
```
quit
```
Show the objects in the room.
```
look/examine
```
Show the description of something
```
look/examine [name of the thing]
```
If you want to take/drop something
```
take/drop [name of the thing]
```
Show the inventory
```
i/inventory
```
When there is slime/demon in the room, you can choose to attack it.
```
attack
```
When possible, you can buy food
```
buy
```
You can eat food when you have any, to +hp
```
eat
```
