# hrolldice

## Name

hrolldice - a Haskell program that rolls virtual dice, loosely based on `rolldice` by Stevie Strickland.

## Synopsis

    hrolldice dice_string dice_string ...

## Description

As an exercise in writing Haskell programs, I wrote my own version of [rolldice](http://packages.debian.org/search?keywords=rolldice) by Stevie Strickland. It is not an exact clone, but for the majority of dice rolls it is a drop-in replacement.

`dice_string`s specify the number of dice, what kind of dice, how many to drop, etc. For example:

 * `d6`: roll a six-sided die.
 * `4d6`: roll four six-sided die and sum the result.
 * `2x4d6`: same as 4d6, but perform it twice and show each result separately.
 * `4d6s1`: same as 4d6 but drop the lowest number rolled.
 * `d6*2+1`: same as d6 but multiply it by 2 and then add 1
 * `d6+1*2`: same as d6 but add 1 and then multiply the result by 2.

In other words, the "modifier functions" at the end are processed in order from left to right. `+1*2-3` can be thought of as `(((result + 1) * 2) - 3)`. This behaviour is different than `rolldice`, which will only handle *one* multiplier and *one* offset (it takes the last occurrence of each without saying so) and has a specified order of operations (multiplication, then addition/subtraction).

### Example usage:

Roll four six-sided dice four times, dropping the lowest number. Repeat six times.

    $ hrolldice 6x4d6s1

    13 11 10 16 7 8

This is the typical way to generate "ability scores" in RPGs like D&D.

## Prior Art

* [rolldice](http://packages.debian.org/search?keywords=rolldice), as mentioned above.
* [python-rolldice](https://launchpad.net/python-rolldice), a python clone of `rolldice`.
* [game-probability](http://hackage.haskell.org/package/game-probability/) seems like a useful Haskell library that I would have used had I not been doing this as an exercise.

## TODO

* Better error messages on incorrect dice strings.
* Tests for modifier functions.
* Better API.

## Author

Tavish Armstrong <tavisharmstrong@gmail.com>


