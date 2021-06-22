# UNIX grep
Implementation of the UNIX grep utility function for searching plain-text data 
sets for lines that match a regular expression.
## Installation
Clone the current repository to your machine
```
git clone git@github.com:Azamat-tech/grep.git
```
## Running the program
Make sure that the Haskell platform is installed on your machine. Then go the
cloned directory and run in the terminal the following:
```
runghc .\src\myGrep.hs 'orange|apple' text.txt
```
`runghc` - the command that will run the haskell file

`.\src\myGrep.hs` - the main file that contains the implementation of grep

`'orange|apple'` - the first argument of the program. Pattern that needs to be searched

`text.txt` - the text where the pattern should be found. 

## Sample Usage I (text file)

```
$ runghc .\src\myGrep.hs one .\files\1
(line 1) at index 1 'one': one two three one
(line 1) at index 15 'one': one two three one
(line 2) at index 15 'one': four five six one
```

## Sample Usage II (standard input)

```
$ runghc .\src\myGrep.hs one
two three four one
(line 1) at index 16 'one': two three four one
apple orange banana
exam school project
onemillion
(line 5) at index 1 'one': onemillion
...
```