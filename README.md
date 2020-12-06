# Advent of code 2020

## download

```shell
SESSION_COOKIE="enter your session cookie here"

DAY=6; curl "https://adventofcode.com/2020/day/$DAY/input" \
  -H "cookie: session=$SESSION_COOKIE" \
  --compressed > "src/main/resources/day$DAY.txt"
```

you can use [direnv](https://direnv.net/) to make your life easier (sets the env var whenever you `cd` into this folder)

```shell
echo 'EXPORT SESSION_COOKIE="enter your session cookie here"' >> .envrc
```

## run

### with default input for the day

Inputfile has to be located here: `src/main/resource/{DAY}.txt`

The resource name will be taken from the package name of the main class, so make sure you have it match

`sbt runMain day6.part1`

### with overridden inputfile

If you put the inputfile somewhere else, provide it as an argument

`sbt runMain day6.part1 src/main/resources/day6-example.txt`
