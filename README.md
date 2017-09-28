
# DobadoBots

[![Travis Status](https://travis-ci.org/NinjaTrappeur/DobadoBots.svg?branch=master)](https://travis-ci.org/NinjaTrappeur/DobadoBots)

## Video

[![Live Video](http://img.youtube.com/vi/Lb8V3ujVHMc/0.jpg)](http://www.youtube.com/watch?v=Lb8V3ujVHMc "Live Video")

Click on the above image to play a video showing Dobadobots.

## Abstract

Dobadobots is a video-game where you program your own robot AI. The goal of the game is to move the robot to the objective (the orange square).

You can program the robot using a custom programming language documented in the **Language** section of this readme file.

## Build

When the development will be done, pre-built versions of dobadobots will be available for download. The game is available for Windows > 7, Linux and MacOS.

In order to build the game, you will need:

- GHC (Haskell2010 compatible)
- Stack
- SDL2 (> 2.0.5)
- SDL2-TTF (> 2.0.14)

In order to build the game, just use
```
  stack build
```

In order to launch unit tests, use
```
  stack test
```

In order to launch the game, use
```
  stack exec DobadoBots-exe
```

## Language

The language consists off three majors concepts:

1. Conditional structures.
2. Robot actions.
3. Robot sensors.

In this section, we will dive in each of these concepts.

### Conditional structures

As it name states, a conditional structure aims to express a condition. We express robots behavior using this structure as a base building block.

Let's have a look at an example:

```
IF laserDistance > 30
  moveForward
ELSE
  IF laserScan = obstacle
    turnRight
  ELSE
    moveForward
```

Here, you are basically doing two checks:

1. If the distance returned by the front laser of the robot is more than 30, it moves forward.
2. If not, it will check the nature of the object detected by the front laser. If it is an obstacle, it will turn to the right, otherwise, it moves forward.

A more general view of the conditional structure would be like this.

```
IF [Sensor]
  [Action or nested conditional]
ELSE
  [Action or nested conditional]
```

### Sensor

The robots can scan the environment using its front laser ray. This laser scans what is fronting it and gathers two kinds of information:

- **laserDistance**:
  - Checks the distance between the robot and the nearest fronting obstacle.
  - Can be compared to any positive number using <, > or =
  - Shape: ```
              laserDistance {<,>,=} 23
           ```
- **laserScan**:
  - Checks what kind of obstacle in fronting the robot. 
  - Shape: ```
              laserScan = {obstacle, wall, objective}
           ```
- **objectiveDistance**:
  - Checks the distance between the robot and the objective.
  - Shape: ```
              objectiveDistance {<,>,=} 23
           ```

### Actions 

You can move the robot using action instructions. There are 4 of them.

- **moveForward**: ... as it states, moves the robot forward.
- **turnRight**: ... as it states, rotates the robot to the right.
- **turnLeft**: ... as it states, rotates the robot to the left.
- **faceObjective**: rotates the robot to make it face the objective.
