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
    display_name: Python 3 (ipykernel)
    language: python
    name: python3
---

```python
import hashlib
```

<!-- #region -->
## [Day 4: The Ideal Stocking Stuffer](https://adventofcode.com/2015/day/4)


### Part 1
Santa needs help mining some AdventCoins (very similar to bitcoins) to use as gifts for all the economically forward-thinking little girls and boys.

To do this, he needs to find MD5 hashes which, in hexadecimal, start with at least five zeroes. The input to the MD5 hash is some secret key (your puzzle input, given below) followed by a number in decimal. To mine AdventCoins, you must find Santa the lowest positive number (no leading zeroes: `1`, `2`, `3`, ...) that produces such a hash.

For example:
- If your secret key is `abcdef`, the answer is `609043`, because the MD5 hash of `abcdef609043` starts with five zeroes (`000001dbbfa...`), and it is the lowest such number to do so.
- If your secret key is `pqrstuv`, the lowest number it combines with to make an MD5 hash starting with five zeroes is `1048970`; that is, the MD5 hash of `pqrstuv1048970` looks like `000006136ef....`
<!-- #endregion -->

#### 0. Set Puzzle Input

```python
PUZZLE_INPUT = "ckczppom"
```

#### 1. Calculate Hash

```python
salt = 0

while True:
    testhash = PUZZLE_INPUT + str(salt)
    advent_coin = hashlib.md5(testhash.encode()).hexdigest()
    if str(advent_coin).startswith('00000'):
        break
    salt += 1

advent_coin
```

#### 2. Print salt

```python
salt
```

### Part 2
### Task 2: Now find one that starts with six zeroes.


#### 3. Calculate Hash

```python
salt = 0

while True:
    testhash = PUZZLE_INPUT + str(salt)
    advent_coin = hashlib.md5(testhash.encode()).hexdigest()
    if str(advent_coin).startswith('000000'):
        break
    salt += 1

advent_coin
```

#### 4. Print salt

```python
salt
```
