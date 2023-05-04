# SAD
*(S)imple (A)nsi (D)ocument*

## About
SAD is a document-format, designed to be displayed in ANSI compatible terminals through a viewer
program like this. Formatting is handled through `mode-switches` in plain text.

This Repository has Pascal-Units for parsing a sad file, you may use these Units in your projects
as long as you follow the license specified in the LICENSE file and in the source.

## sadv
sadv(iewer) uses these Units for displaying a sad in console or converting it to HTML.

### Rewrite
A rewrite of the parsing unit and sav is currently being done on the fpc_rewrite branch,
the goals of this rewrite are:
1. Cleaner Code, more maintainable code
2. Better usability as a library
3. Move away from OOP to Procedural

### Usage
`sadv <file> [parameters] [:section]` <br>

By default, the File will be Formatted using ANSI-escapes and output to STDOUT. <br>

**Arguments** <br>

| Name         | Parameters | Description                         |
| ------------ | ---------- | ----------------------------------- |
| -pm / --meta |            | Prints the documents Meta-Data      |
| -x           |            | Convert to HTML                     |
| --style      | path       | Specify a CSS for the HTML export   |
| -vw          |            | Enable Verbose mode for HTML export |

### Installation
**AUR** <br>
You can find sadv on the AUR [here](https://aur.archlinux.org/packages/sadv). <br>

**From Source**
```bash
git clone https://github.com/FelixEcker/sad.git
cd sad
sh install.sh
```

## Features and Limitations
**Features** <br>
* Supports Formatting using Standard ANSI codes (see `Styles and Colors` in `Example` section)
* Sections and Sub-Sections
* Can also Store some meta-data tags

**Limitations** <br>
* *Only* ANSI codes can be used for formatting
* *Only* Text is supported
* Syntax can be limiting or annoying

## Example
```text
{$meta date} 02-11-2022
{$meta author} FelixEcker
{$preserve-mode style}
{$start}
{$title} Test Note

{$begin-section} section-name
{$head} This is a Section of Text
Just some test text. This should be {$style bold} bold {$reset}
This should be {$style italic} italic {$reset-all}
{$color yellow} and this should {$color blue back} be some color {$reset-all}
{$end-section}

{$begin-section} section2-name
{$head} Another Section
{$begin-section} section3-nme
{$sub-head} Subsection of Section above

Lorum ipsum, im a subsection!
{$end-section}

and im not in the subsection anymore
{$end-section}
```

## Switches
### Header
* meta `<name> <content (single-word)>`
    * Sets some meta information
* preserve-mode `style/color`
    * Sets whether to preserve the current style or color when the `reset` switch is executed

### In Text
* title
	* Sets the overall title of the note
* begin-section `<section-name>`
	* Starts a new section in the file with given header
* end-section
	* Closes the current section
* head
    * Format rest of line as header. Has to come first in line
* sub-head
    * Format rest of line as sub-header. Has to come first in line
* style `<style name>`
	* Sets the Text-Style
* color `<color name> <(optional) back/fore>`
	* Sets the Text-Color to the appropriate ansi color
* reset
	* Resets all formatting to default (regards preserve-mode)
* reset-all
    * Resets all formatting to default (disregards preverse-mode)

