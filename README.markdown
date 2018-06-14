# Fuzzy-Finder

## Requirements

- Roswell


## Usage

```sh
./fuzzy-finder.ros
```

- text
target string
- pattern
pattern string
- distance
Levenshtein distance

### Example 

#### Roswell script

```sh
./fuzzy-finder.ros "acbacbaca" "acbaca" 2
```

- distance

Levenshtein distance

- index

the last index of the matched string

#### REPL

```sh
ros run

* (ql:quickload :fuzzy-finder)

To load "fuzzy-finder":
  Load 1 ASDF system:
    fuzzy-finder
; Loading "fuzzy-finder"

(:FUZZY-FINDER)


* (fuzzy-finder:fuzzy-finder "acbacbaca" "acbaca" 2)

((2 . 3) (1 . 4) (2 . 4) (1 . 5) (2 . 5) (1 . 6) (2 . 6) (1 . 7) (2 . 7)
 (0 . 8) (1 . 8) (2 . 8))


* (fuzzy-finder:levenshtein-distance "acbacbaca" "acbaca")

3


* (fuzzy-finder:shift-and "acbacbaca" "acbaca")

(8)


* (fuzzy-finder:shift-or "acbacbaca" "acbaca")

(8)
```


## Installation

1. Clone this repository into wherever below the path `ql:\*local-project-directories\*`, e.g. `~/.roswell/local-projects`

```sh
git clone https://github.com/henninltn/fuzzy-finder.git ~/.roswell/local-projects/github.com/henninltn/
```

2. Register local projects with Quicklisp

```sh
ros run

* (ql:register-local-projects)

Nil
```

then, confirm the file `~/.roswell/local-projects/system-index.txt`

```sh
cat ~/.roswell/local-projects/system-index.txt

github.com/henninltn/fuzzy-finder/fuzzy-finder.asd
github.com/henninltn/fuzzy-finder/fuzzy-finder-test.asd
```

3. Load this project with Quicklisp and run fuzzy-finder

view [Usage](#usage)


## Author

* hennin (hennin.ltn@gmail.com)


## Copyright

Copyright (c) 2018 hennin (hennin.ltn@gmail.com)


## License

Licensed under the MIT License.
