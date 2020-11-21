# JSON Parser

This project uses json.org for design specification and implements a JSON parser in Haskell. EBNF diagrams and a basic [report](/report/report.pdf) can be found within.


## Table of contents
* [Setup](#setup)
* [Usage](#usage)
* [License](#license)



## Setup

Git clone the project and build the binaries 

```bash
ghc json-parser.hs
```

## Usage
Pass a local json filename as a parameter to the program.

```bash
./json-parser input.json
```

The output will show pairs, lexemes, file data and final result


## License
[MIT](https://choosealicense.com/licenses/mit/)