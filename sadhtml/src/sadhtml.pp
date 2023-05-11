{$mode fpc}
program sadhtml;

{  sadhtml - Simple Ansi Document to HTML converter  }
{                                                    }
{ Author: Marie Eckert                               }
{ Licensed under the 3-Clause BSD License. See below }

{ TODO: Potentially refactor the reused code from sadv into a shared unit }

(* Copyright (c) 2022, Marie Eckert                                           *)
(*                                                                            *)
(* Redistribution and use in source and binary forms, with or without         *)
(* modification, are permitted provided that the following conditions are met:*)
(*                                                                            *)
(* 1. Redistributions of source code must retain the above copyright notice,  *)
(* this list of conditions and the following disclaimer.                      *)
(*                                                                            *)
(* 2. Redistributions in binary form must reproduce the above copyright       *)
(*    notice, this list of conditions and the following disclaimer in the     *)
(*    documentation   and/or other materials provided with the distribution.  *)
(*                                                                            *)
(* 3. Neither the name of the copyright holder nor the names of its           *)
(*    contributors may be used to endorse or promote products derived from    *)
(*    this software without specific prior written permission.                *)
(*                                                                            *)
(* THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS        *)
(* "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED  *)
(* TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR *)
(* PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR          *)
(* CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,      *)
(* EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,        *)
(* PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS;*)
(* OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,   *)
(* WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR    *)
(* OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF     *)
(* ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.                                 *)

{$H+}

uses SysUtils, uSADParser, uSADHTML;

const
  DEFAULT_STYLESHEET = '$XDG_CONFIG_HOME/sadhtml/default.css';
  VERSION = '1.0.0';
var
  i: Integer;
  path, style_path, required_section, cparam: String;
  converted: String;
  print_meta: Boolean;
  doc: TSADocument;
  meta: TMetaData;
begin
  if (ParamCount() = 0) then
  begin
    writeln('SAD to HTML converter ; ', VERSION, ' by Marie Eckert');
    writeln('Usage: sadhtml [file] <parameters> <:section>');
    writeln;
    writeln('Parameters: ');
    writeln('-m,  --meta          Include Meta-Information');
    writeln('-s,  --style <file>  Override default Stylesheet');
    writeln;
    halt;
  end;

  path := ParamStr(1);

  if not FileExists(path) then
  begin
    writeln('Input File not found: ', path);
    halt;
  end;

  style_path := DEFAULT_STYLESHEET;
  required_section := '.';
  print_meta := False;
  for i := 2 to ParamCount() do
  begin
    cparam := ParamStr(i);
    if cparam[1] = ':' then
    begin
      required_section := Copy(cparam, 2, Length(cparam));
      break;
    end;

    if (cparam = '-m') or (cparam = '--meta') then
      print_meta := True
    else if (cparam = '-s') or (cparam = '--style') then
    begin
      if ParamCount() <= i+1 then
      begin
        writeln(cparam, ' requires one additional parameter: path to file');
        exit;
      end;

      style_path := ParamStr(i+1);
    end;
  end;

  if not FileExists(style_path) then
  begin
    writeln('Stylesheet file not found: ', style_path);
    exit;
  end;

  Assign(doc.doc_file, path);
  ReSet(doc.doc_file);
  if not ParseStructure(doc) then
  begin
    writeln('ParseStructure failed on line ', doc.line_number);
    writeln('--> ', parse_error);
    halt;
  end;

{$IFDEF DEBUG}
  DebugPrintDocument(doc);
{$ENDIF}

  {converted := HTMLParseSection();}
end.
