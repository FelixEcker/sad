{$mode objfpc}
program sadv;

{         sadv - Simple Ansi Document Viewer          }
{                                                     }
{ Author: Felix Eckert                                }
{ Licensed under the 3-Clause BSD License. See below. }

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


uses SysUtils, Types, uSADParser, uSADHTMLParser, dos;

var
  i, lastparam: Integer;
  path, stylepath, _line, required_section, cparam: String;
  print_meta, print_lines, as_html, verbose_writing: Boolean;
  parser: TSADParser;
  outfile: TextFile;
  content: TStringDynArray;
begin
  if (ParamCount() = 0) then
  begin
    writeln('SAD Command Line Viewer ; 1.2.0 by Felix Eckert');
    writeln('Usage: sadv [file] <parameters> <:section>');
    writeln;
    writeln('Parameters: ');
    writeln('-pm, --meta          Print Meta-Information');
    writeln('-l,  --lines         Print Line-Numbers');
    writeln('-x,  --html          Output as HTML');
    writeln('  ,  --style <path>  Set the HTML Style Sheet');
    writeln('-vw                  Verbose Writing');
    writeln;
    halt;
  end;

  path := ParamStr(1);
  
  if not FileExists(path) then
  begin
    writeln('Input File not found: ', path);
    halt;
  end;

  required_section := '.';
  print_meta := False;
  print_lines := False;
  as_html := False;
  verbose_writing := False;
  for i := 2 to ParamCount() do
  begin
    cparam := ParamStr(i);
    if cparam[1] = ':' then
    begin
      required_section := Copy(cparam, 2, Length(cparam));
      break;
    end;

    if (cparam = '-pm') or (cparam = '--meta') then
      print_meta := True
    else if (cparam = '-l') or (cparam = '--lines') then
      print_lines := True
    else if (cparam = '-x') or (cparam = '--html') then
      as_html := True
    else if (cparam = '--style') then
      stylepath := ParamStr(i)
    else if (cparam = '-vw') then
      verbose_writing := True;
  end;

  try
    if not as_html then
    begin
      parser := TSADParser.Create;
      parser.Path := path;
      parser.Section := required_section;
      parser.Open;

      if print_meta then
      begin
        writeln('Author: ', parser.MetaData.author);
        writeln('Date: ', parser.MetaData.date);
        writeln;
      end;
    
      i := 0;
      content := parser.ParseFile;
      for _line in content do
      begin
        i := i + 1;
        if print_lines then write(Format('%.3d  ', [i]));
        writeln(_line);
      end;
    end else
    begin
      parser := TSADHTMLParser.Create;
      parser.Path := path;
      parser.Section := required_section;
      parser.Open;

      if (stylepath = '') then
      begin
        writeln('No stylesheet set, using default');
        stylepath := GetEnv('HOME')+'/.config/sadv/default.css';
      end;

      if not FileExists(stylepath) then
      begin
        writeln('Stylesheet not found: ', stylepath);
        halt;
      end;

      // Verbose writing was originally debug but i decided to keep it
      TSADHTMLParser(parser).WriteHtml(path+'.html', stylepath, verbose_writing);
    end;
  except
    on e: EMalformedDocumentException do
    begin
      writeln('Input File is Malformed: ', e.Message);
    end;
    on e: ENoSuchSectionException do
    begin
      writeln(e.Message);
    end;
  end;
end.
