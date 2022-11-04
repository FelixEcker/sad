# SNF
*(S)imple (N)ote (F)ormat*

## About
SNF is a document-format, designed to be displayed in ANSI compatible terminals through a viewer
program like this. Formatting is handled through `mode-switches` in plain text.

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
{$comment Default Style, White Foreground, Default background}
{$default default white default}
{$meta date} 02-11-2022
{$meta author} FelixEcker
{$start}
{$title} Test Note

{$begin-section}
{$head} This is a Section of Text
Just some test text. This should be {$style bold} bold {$style default}
This should be {$style italic} italic {$style default}
{$color yellow} and this should {$color blue back} be some color {$reset}
{$end-section}

{$begin-section}
{$head} Another Section
{$begin-section}
{$sub-head} Subsection of Section above

Lorum ipsum, im a subsection!
{$end-section}

and im not in the subsection anymore
{$end-section}
```

## Commands
* title
	* Sets the overall title of the note
* begin-section `<section-header>`
	* Starts a new section in the file with given header
* end-section
	* Closes the current section
* default `<default style> <default color> <default color (back>`
	* Defines the defaults that `reset` reverts to
* style `<style name>`
	* Sets the Text-Style
* color `<color name> <(optional) back/fore>`
	* Sets the Text-Color to the appropriate ansi color
* reset
	* Resets all formatting to default
