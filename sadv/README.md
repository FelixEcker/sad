# sadv
sadv(iewer) uses these Units for displaying a sad in console or converting it to HTML.

## Usage
`sadv <file> [parameters] [:section]` <br>

By default, the File will be Formatted using ANSI-escapes and output to STDOUT. <br>

**Parameters** <br>

| Name          | Parameters | Description                         |
| ------------- | ---------- | ----------------------------------- |
| -pm / --meta  |            | Prints the documents Meta-Data      |
| -l  / --lines |            | Print linenumbers alongside the converted output |
| --sections    |            | List all sections within a document in a tree-format |

The last parameter may be an absolute path to a specific section, the path
seperator is a colon (`:`).

## Installation
### AUR <br>
sadv is part of the sadsuite AUR package.

### Source
```bash
git clone https://github.com/FelixEcker/sad.git
cd sad
sh install.sh
```

