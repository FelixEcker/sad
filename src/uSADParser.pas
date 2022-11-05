{$mode objfpc}
unit uSADParser;

{  uSADParser.pas ; Pascal Parser for Simple-Ansi-Documents  }
{                                                            }
{ Author: Felix Eckert                                       }
{ Written for sadv (SAD-Viewer), licensed under the 3-Clause }
{ BSD License. See below.                                    }

(* Copyright (c) 2022, Felix Eckert                                               *)
(*                                                                                *)
(* Redistribution and use in source and binary forms, with or without             *)
(* modification, are permitted provided that the following conditions are met:    *)
(*                                                                                *)
(* 1. Redistributions of source code must retain the above copyright notice, this *)
(*    list of conditions and the following disclaimer.                            *)
(*                                                                                *)
(* 2. Redistributions in binary form must reproduce the above copyright notice,   *)
(*    this list of conditions and the following disclaimer in the documentation   *)
(*    and/or other materials provided with the distribution.                      *)
(*                                                                                *)
(* 3. Neither the name of the copyright holder nor the names of its               *)
(*    contributors may be used to endorse or promote products derived from        *)
(*    this software without specific prior written permission.                    *)
(*                                                                                *)
(* THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"    *)
(* AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE      *)
(* IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE *)
(* DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE   *)
(* FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL     *)
(* DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR     *)
(* SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER     *)
(* CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,  *)
(* OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE  *)
(* OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.           *)

interface
  uses StrUtils, SysUtils, Types;

  type
    EMalformedDocumentException = Class(Exception);
    ENoSuchSectionException = Class(Exception);
    TSADMeta = record
      head_style: String;
      head_color: String;
      sub_head_style: String;
      sub_head_color: String;

      author, date: String;
    end;
    TSADParser = class
      private
        FFile: TextFile;
        FOpen: Boolean;
        FFilePath: String;

        FPreviousSections: TStringDynArray;
        FParseSection: String;
        FCurrentSection: String;

        FLineNumber: Integer;
        FLine: String;

        FMetaData: TSADMeta;

        function IsEOF: Boolean;
        function StyleToStr(const AStr: String): String;
        function StyleToInt(const AStr: String): Integer;
        function ColorStrToInt(const AStr: String; const AIsBack: Boolean): Integer;
      public
        constructor Create;
        destructor Free;

        procedure SetFile(const Value: String);
        procedure Open;
        procedure CloseFile;
        function NextLine: String;
        function ParseFile: TStringDynArray;

        function FinishedRequiredSection: Boolean;
        property Path: String read FFilePath write SetFile;
        property Opened: Boolean read FOpen;
        property Section: String read FParseSection write FParseSection;
        property ReachedEOF: Boolean read IsEOF;
        property MetaData: TSADMeta read FMetaData;
    end;

    const
      { Text Styles }
      tsResetAll     = 0;
      tsBold         = 1;  // bright
      tsDim          = 2;
      tsItalic       = 3;
      tsUnderline    = 4;
      tsBlink        = 5;
      tsOverline     = 6;
      tsInvert       = 7;
      tsHidden       = 8;
      tsStrike       = 9;
      tsFraktur      = 20;
      tsNoBold       = 21;
      tsNoDim        = 22;
      tsNoItalic     = 0;  // 23; not working?
      tsNoUnderline  = 24;
      tsNoBlink      = 25;
      tsNoOverline   = 26;
      tsNoInvert     = 27;
      tsNoHidden     = 28;
      tsNoStrike     = 29;
      { Foreground Colors }
      fcBlack        = 30;
      fcRed          = 31;
      fcGreen        = 32;
      fcBrown        = 33;
      fcBlue         = 34;
      fcMagenta      = 35;
      fcCyan         = 36;
      fcLightGray    = 37;
      fcRichColors   = 38;  // requires additional parameter(s)
      fcDefault      = 39;
      fcDarkGray     = 90;
      fcLightRed     = 91;
      fcLightGreen   = 92;
      fcYellow       = 93;
      fcLightBlue    = 94;
      fcLightMagenta = 95;
      fcLightCyan    = 96;
      fcWhite        = 97;
      { Background Colors }
      bcBlack        = 40;
      bcRed          = 41;
      bcGreen        = 42;
      bcBrown        = 43;
      bcBlue         = 44;
      bcMagenta      = 45;
      bcCyan         = 46;
      bcLightGray    = 47;
      bcRichColors   = 48;  // requires additional parameter(s)
      bcDefault      = 49;
      bcDarkGray     = 100;
      bcLightRed     = 101;
      bcLightGreen   = 102;
      bcYellow       = 103;
      bcLightBlue    = 104;
      bcLightMagenta = 105;
      bcLightCyan    = 106;
      bcWhite        = 107;

implementation
  { TSADParser }

  constructor TSADParser.Create;
  begin
    FOpen := False;
    FParseSection := '.';
    FMetaData.author := 'N/A';
    FMetaData.date := 'N/A';
    FMetaData.head_style := #27'[1m'#27'[4m';
    FMetaData.head_color := #27'[39m';;
    FMetaData.sub_head_style := #27'[4m';
    FMetaData.sub_head_color := #27'[39m';
  end;

  destructor TSADParser.Free;
  begin
    Close(FFile);
  end;

  procedure TSADParser.SetFile(const Value: String);
  begin
    FFilePath := Value;
    Assign(FFile, FFilePath);
  end;

  procedure TSADParser.CloseFile;
  begin
    Close(FFile);
    FOpen := False;
  end;

  { Open the set file and read the heading data. Also reads forward to the section specfied in
    FParseSection.  }
  procedure TSADParser.Open;
  var
    split: TStringDynArray;
    i: Integer;
  begin
    reset(FFile);
    FLineNumber := 0;

    { Parse Until title is hit }
    FLine := '';
    FCurrentSection := '.';

    while (FLine <> '{$start}') do
    begin
      readln(FFile, FLine);
      FLineNumber := FLineNumber + 1;

      split := SplitString(FLine, ' ');
      case split[0] of
        '{$meta': begin
          case split[1] of
            'author}': FMetaData.author := split[2];
            'date}': FMetaData.date := split[2];
          end;
        end;
        '{$comment': continue;
      end;
    end;

    // read ahead to wanted section
    while (FCurrentSection <> FParseSection) do
    begin 
      readln(FFile, FLine);
      FLineNumber := FLineNumber + 1;
      split := SplitString(FLine, ' ');

      for i := 0 to Length(split)-1 do
      begin
        if (split[i] = '{$begin-section}') then
        begin
          SetLength(FPreviousSections, Length(FPreviousSections)+1);
          FPreviousSections[Length(FPreviousSections)-1] := FCurrentSection;

          if (Length(split)-1 < i+1) then
            raise EMalformedDocumentException.Create('Missing title name for section at line: '+IntToStr(FLineNumber));
          FCurrentSection := split[i+1];
        end;
      end;

      if eof(FFile) then
        raise ENoSuchSectionException.Create('Can'' t find section '+FParseSection);
    end;

    FOpen := True;
  end;

  { Parse the next line from the currently opened file }
  function TSADParser.NextLine: String;
  var
    i, skipWords: Integer;
    isBack, addReset: Boolean;
    lsplit: TStringDynArray;
  begin
    // TODO: Implement Parsing
    readln(FFile, FLine);
    FLineNumber := FLineNumber + 1;

    result := '';
    lsplit := SplitString(FLine, ' ');
    skipWords := 0;
    addReset := False;
    for i := 0 to Length(lsplit)-1 do
    begin
      if (skipWords > 0) then
      begin
        skipWords := skipWords - 1;
        continue;
      end;
      if (StartsStr('{$', lsplit[i])) then // Modeswitch
      begin
        case lsplit[i] of
          { Sectioning }
          '{$begin-section}': begin
            SetLength(FPreviousSections, Length(FPreviousSections)+1);
            FPreviousSections[Length(FPreviousSections)-1] := FCurrentSection;

            if (Length(lsplit)-1 < i+1) then
              raise EMalformedDocumentException.Create('Missing title name for section at line: '+IntToStr(FLineNumber));
            FCurrentSection := lsplit[i+1];
            exit(NextLine()); 
          end;
          '{$end-section}': begin 
            FCurrentSection := FPreviousSections[Length(FPreviousSections)-1];
            FPreviousSections := Copy(FPreviousSections, 1, Length(FPreviousSections)-1);
            exit(NextLine()+sLineBreak); 
          end;

          '{$title}': begin
            addReset := True;
            result := result + MetaData.head_style;
          end;

          '{$head}': begin
            addReset := True;
            result := result + FMetaData.head_style;
          end;

          '{$sub-head}': begin
            addReset := True;
            result := result + FMetaData.sub_head_color + FMetaData.sub_head_style;
          end;

          { Styling }
          // Text-Color
          '{$color': begin
            if (Length(lsplit)-1 < i+1) then
              raise EMalformedDocumentException.Create('Missing color value for color-switch at line: '+IntToStr(FLineNumber));

            skipWords := 1;
            isBack := not EndsStr('}', lsplit[i+1]);
            if isBack then skipWords := skipWords + 1
            else
              lsplit[i+1] := Copy(lsplit[i+1], 1, Length(lsplit[i+1])-1);

            result := result + #27'['+IntToStr(ColorStrToInt(
                lsplit[i+1], isBack))+'m'
          end;

          // ANSI Reset All
          '{$reset}':
            result := Copy(result, 1, Length(result)-1) + Format(#27'[%dm', [tsResetAll]) + ' ';

          // Text-Styling
          '{$style': begin
            if (Length(lsplit)-1 < i+1) then
              raise EMalformedDocumentException.Create('Missing style value for style-switch at line: '+IntToStr(FLineNumber));

            skipWords := 1;

            result := result + StyleToStr(Copy(lsplit[i+1], 1, Length(lsplit[i+1])-1));
          end;
        end;
      end else if (StartsStr('{{$', lsplit[i])) then
        result := result + Copy(lsplit[i], 2, Length(lsplit[i])) + ' '
      else
        result := result + lsplit[i] + ' ';
    end;

    result := Copy(result, 1, Length(result)-1); // remove trailing space
    if addReset then result := result + #27'['+IntToStr(tsResetAll)+'m';
  end;

  { Parses the entire file into an array of Ansi-Formatted Strings. }
  function TSADParser.ParseFile: TStringDynArray;
  var
    out: TStringDynArray;
  begin
    SetLength(out, 0);

    while not IsEof() and not FinishedRequiredSection() do
    begin
      SetLength(out, Length(out)+1);
      out[Length(out)-1] := NextLine;
    end;

    result := out;
  end;

  { Check if the Section specified in FParseSection has finished reading }
  function TSADParser.FinishedRequiredSection: Boolean;
  var
    sect: String;
  begin
    if (FCurrentSection = FParseSection) then exit(False);
    for sect in FPreviousSections do
      if (FParseSection = sect) then
        exit(False);

    exit(True);
  end;

  { private }

  { Checks if EOF is reached }
  function TSADParser.IsEOF: Boolean;
  begin
    result := eof(FFile);
  end;

  { Get the Integer value for the given style }
  function TSADParser.StyleToInt(const AStr: String): Integer;
  begin
    case AStr of
      'fraktur':     result := tsFraktur;
      'bold':        result := tsBold;
      'dim':         result := tsDim;
      'italic':      result := tsItalic;
      'underline':   result := tsUnderline;
      'blink':       result := tsBlink;
      'overline':    result := tsOverline;
      'invert':      result := tsInvert;
      'hidden':      result := tsHidden;
      'reset':       result := tsResetAll;
      'strike':      result := tsStrike;
      'nobold':      result := tsNoBold;
      'nodim':       result := tsNoDim;
      'noitalic':    result := tsNoItalic;
      'nounderline': result := tsNoUnderline;
      'noblink':     result := tsNoBlink;
      'nooverline':  result := tsNoOverline;
      'noinvert':    result := tsNoInvert;
      'nohidden':    result := tsNoHidden;
      'nostrike':    result := tsNoStrike;
    else
      result := tsResetAll;
    end;
  end;

  function TSADParser.StyleToStr(const AStr: String): String;
  var
    curf, outstr: String;
    split: TStringDynArray;
    tc: Integer;
  begin
    split := SplitString(AStr, ';');
    // writeln(IntToStr(FLineNumber), '  ', 'AStr = ', AStr, ' ; Len = ', IntToStr(Length(split)), ' ; 1st = ', split[0]); // DEBUG
    outstr := '';
    for curf in split do
      outstr := outstr + Format(#27'[%dm', [StyleToInt(curf)]);

    result := outstr;
  end;

  { Get the Integer value for the given color }
  function TSADParser.ColorStrToInt(const AStr: String; const AIsBack: Boolean): Integer;
  begin
    case AStr of
      'default':      begin if AIsBack then result := bcDefault      else result := fcDefault;      end;
      'black':        begin if AIsBack then result := bcBlack        else result := fcBlack;        end;
      'red':          begin if AIsBack then result := bcRed          else result := fcRed;          end;
      'green':        begin if AIsBack then result := bcGreen        else result := fcGreen;        end;
      'brown':        begin if AIsBack then result := bcBrown        else result := fcBrown;        end;
      'blue':         begin if AIsBack then result := bcBlue         else result := fcBlue;         end;
      'magenta':      begin if AIsBack then result := bcMagenta      else result := fcMagenta;      end;
      'cyan':         begin if AIsBack then result := bcCyan         else result := fcCyan;         end;
      'lightgray':    begin if AIsBack then result := bcLightGray    else result := fcLightGray;    end;
      'richcolors':   begin if AIsBack then result := bcRichColors   else result := fcRichColors;   end;
      'reset':        result := tsResetAll;
      'darkgray':     begin if AIsBack then result := bcDarkGray     else result := fcDarkGray;     end;
      'lightred':     begin if AIsBack then result := bcLightRed     else result := fcLightRed;     end;
      'lighgreen':    begin if AIsBack then result := bcLightGreen   else result := fcLightGreen;   end;
      'yellow':       begin if AIsBack then result := bcYellow       else result := fcYellow;       end;
      'lightblue':    begin if AIsBack then result := bcLightBlue    else result := fcLightBlue;    end;
      'lightmagenta': begin if AIsBack then result := bcLightMagenta else result := fcLightMagenta; end;
      'lightcyan':    begin if AIsBack then result := bcLightCyan    else result := fcLightCyan;    end;
      'white':        begin if AIsBack then result := bcWhite        else result := fcWhite;        end;
    else
      result := tsResetAll;
    end;
  end;
end.
