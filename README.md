# elm-spangler
This is an Elm implementation based on [Spangler](https://github.com/smurp/spangler_js/)

The app generates images based on a set of input parameters which are displayed at the bottom of the screen.
Use Left/Right to move between the input boxes and Up/Down to increment/decrement.

Inputs:
1) The number of points to start with. 3 = Triangle, 5 = Pentagon/Pentagram
2) The distance between points that are connected with a line. Usually 1 is good but for a Pentagram you would use 2. You can try other values
3) and after: The number of times to repeat the previous item.

## Controls
* Left/Right: Navigate between the boxes
* Up/Down: Increment/Decrement. Setting the last segment to 0 will remove it. Increment the empty segment will add a new one
* J/K/L: Animate Forward/Backward. K will stop the animation. Only works on the repeats, not on the first two

## Todo
- [x] Add coloring
- [x] Add rotation
- [x] Add timing controls
- [ ] Add export
- [ ] Add onion skinning

[Here is an example](https://tilmans.github.io/elm-spangler/#2,5,3,6,11)
