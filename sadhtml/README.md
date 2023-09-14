# sadhtml
sadhtml is a tool to convert sad files to html documents.

## Usage
`sadhtml <file> [parameters] [:section]` <br>

By default, the generated html will be saved at the same location with the extension
`.sad.html`<br>

**Parameters** <br>

| Name          | Parameters | Description                 |
| ------------- | ---------- | --------------------------- |
| -o  / --out   | file       | Specify the Output file     |
| -s  / --style | file       | Specify the Stylesheet file |

The last parameter may be an absolute path to a specific section, the path
seperator is a colon (`:`).

## Installation
```bash
git clone https://github.com/FelixEcker/sad.git
cd sad/sadhtml
sh build.sh
sudo mv out/sadhtml /usr/bin/sadhtml
```

