{$mode fpc}
program sadv;

{         sadv - Simple Ansi Document Viewer          }
{                                                     }
{ Author: Felix Eckert                                }
{ Licensed under the 3-Clause BSD License. See below. }

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

uses SysUtils, Types, StrUtils, uPathResolve, uSADParser, dos;

function FindSection(const ADocument: TSADocument;
                     const AName: String): TSection;

  (* Local function for recursive section search *)
  function _FindSection(const ASec: TSection; const ASecName: String): TSection;
  var
    sec: TSection;
  begin
    _FindSection := ASec;
    for sec in ASec.children do
    begin
      if sec.name = ASecName then
      begin
        _FindSection := sec;
        exit;
      end;
    end;

    for sec in ASec.children do
      _FindSection := _FindSection(sec, ASecName);
  end;
begin

  FindSection := ADocument.root_section;
  if AName = '.' then exit;

  FindSection := _FindSection(ADocument.root_section, AName);
end;

const
  VERSION = '1.3.0';
var
  i: Integer;
  path, required_section, cparam: String;
  converted: String;
  converted_lines: TStringDynArray;
  print_meta, print_lines: Boolean;
  doc: TSADocument;
  meta: TMetaData;
begin
  if (ParamCount() = 0) then
  begin
    writeln('SAD Command Line Viewer ; ', VERSION, ' by Felix Eckert');
    writeln('Usage: sadv [file] <parameters> <:section>');
    writeln;
    writeln('Parameters: ');
    writeln('-pm, --meta          Print Meta-Information');
    writeln('-l,  --lines         Print Line-Numbers');
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
      print_lines := True;
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

  converted := ParseSection(FindSection(doc, required_section), True);

  if print_meta then
    for meta in doc.meta_data do
      writeln('meta-data: ', meta.name, ': ', meta.content);

  if not print_lines then
  begin
    writeln(converted);
    halt;
  end;

  converted_lines := SplitString(converted, sLineBreak);
  for i := 0 to Length(converted_lines) - 1 do
    writeln(Format('%.3d %s', [i+1, converted_lines[i]]));
end.
