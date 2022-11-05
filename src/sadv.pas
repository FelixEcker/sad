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


uses SysUtils, uSADParser;

var
  i, lastparam: Integer;
  path, _line, required_section, cparam: String;
  print_meta: Boolean;
  parser: TSADParser;
begin
  if (ParamCount() = 0) then
  begin
    writeln('SAD Command Line Viewer ; 1.0.0 by Felix Eckert');
    writeln('Usage: sadv [file] <parameters> <:section>');
    writeln;
    writeln('Parameters: ');
    writeln('-pm, --meta   Print Meta-Information');
    writeln;
  end;

  path := ParamStr(1);
  required_section := '.';
  for i := 2 to ParamCount() do
  begin
    cparam := ParamStr(i);
    if cparam[1] = ':' then
    begin
      required_section := Copy(cparam, 2, Length(cparam));
      break;
    end;

    if (cparam = '-pm') or (cparam = '--meta') then
      print_meta := True;
  end;

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

  while not parser.ReachedEOF and not parser.FinishedRequiredSection() do
  begin
    writeln(parser.nextline());
  end;
end.
