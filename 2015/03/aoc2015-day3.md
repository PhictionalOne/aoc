---
jupyter:
  jupytext:
    formats: ipynb,md
    text_representation:
      extension: .md
      format_name: markdown
      format_version: '1.3'
      jupytext_version: 1.11.3
  kernelspec:
    display_name: R
    language: R
    name: ir
---

## Day 3: Perfectly Spherical Houses in a Vacuum
### Part 1
Santa is delivering presents to an infinite two-dimensional grid of houses.

He begins by delivering a present to the house at his starting location, and then an elf at the North Pole calls him via radio and tells him where to move next. Moves are always exactly one house to the north (`^`), south (`v`), east (`>`), or west (`<`). After each move, he delivers another present to the house at his new location.

However, the elf back at the north pole has had a little too much eggnog, and so his directions are a little off, and Santa ends up visiting some houses more than once.

For example:
- `>` delivers presents to `2` houses: one at the starting location, and one to the east.
- `^>v<` delivers presents to `4` houses in a square, including twice to the house at his starting/ending location.
- `^v^v^v^v^v` delivers a bunch of presents to some very lucky children at only `2` houses.


#### 0. load directions

```R
txt <- scan('puzzle.aoc', what='character', sep='\n', fileEncoding='utf8')
vec <- strsplit(txt, '')[[1]]
```

### Task 1: How many houses receive at least one present?


#### 1. Setup variables & count homes

```R
# Setup variables
x <- 0; y <- 0; xs <- c(x); ys <- c(y)
visited <- 1

# iterate over directional input
for (direction in vec) {
    if (direction == '>') { x <- x + 1 }
    if (direction == '<') { x <- x - 1 }
    if (direction == 'v') { y <- y + 1 }
    if (direction == '^') { y <- y - 1 }
    
    xs <- append(xs, x); ys <- append(ys, y)   
}
```

#### 2. calculate dimension of deduplicated maxtrix

```R
mat <- unique(cbind(xs,ys))
dim(mat)[1]
```

### Part 2
The next year, to speed up the process, Santa creates a robot version of himself, Robo-Santa, to deliver presents with him.

Santa and Robo-Santa start at the same location (delivering two presents to the same starting house), then take turns moving based on instructions from the elf, who is eggnoggedly reading from the same script as the previous year.

For example:

- `^v` delivers presents to 3 houses, because Santa goes north, and then Robo-Santa goes south.
- `^>v<` now delivers presents to 3 houses, and Santa and Robo-Santa end up back where they started.
- `^v^v^v^v^v` now delivers presents to 11 houses, with Santa going one direction and Robo-Santa going the other.


### Task 2: This year, how many houses receive at least one present?


#### 3. Setup variables & count homes

```R
# Setup variables
santa_x <- 0; santa_y <- 0 
robot_x <- 0; robot_y <- 0
xs <- c(santa_x); ys <- c(santa_y)
is_santa <- TRUE # TRUE := Santa, FALSE := Robot
visited <- 1

# iterate over directional input
for (direction in vec) {
    if (is_santa) {
        if (direction == '>') { santa_x <- santa_x + 1 }
        if (direction == '<') { santa_x <- santa_x - 1 }
        if (direction == 'v') { santa_y <- santa_y + 1 }
        if (direction == '^') { santa_y <- santa_y - 1 }
    
        xs <- append(xs, santa_x); ys <- append(ys, santa_y)
        is_santa <- !is_santa
    } else {
        if (direction == '>') { robot_x <- robot_x + 1 }
        if (direction == '<') { robot_x <- robot_x - 1 }
        if (direction == 'v') { robot_y <- robot_y + 1 }
        if (direction == '^') { robot_y <- robot_y - 1 }
    
        xs <- append(xs, robot_x); ys <- append(ys, robot_y)
        is_santa <- !is_santa
    }
}
```

#### 4. dimension of deduplicated matrix

```R
mat <- unique(cbind(xs,ys))
dim(mat)[1]
```
