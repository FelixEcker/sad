{$mode fpc}
unit uSADHTML;

{ uSADHTML.pp ; SAD to HTML parser/converter }
{ Author: Marie Eckert                       }

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

interface
  uses SysUtils, StrUtils, Types, uSADParser;

  function HTMLParseSection(const ASection: TSection;
                            const ADoChildren: Boolean): String;
implementation
  { Private Procedures/Functions }
  
  procedure AppendToStrArray(var AArray: TStringDynArray; const AStr: String);
  begin
    SetLength(AArray, Length(AArray)+1);
    AArray[HIGH(AArray)] := AStr;
  end;

  { Public Procedures/Functions }

  function HTMLParseSection(const ASection: TSection;
                            const ADoChildren: Boolean): String;
  var
    lines, line_split, styles, colors: TStringDynArray;
    current_line: String;
    i, skip: Integer;
    section: TSection;
  begin
    HTMLParseSection := '';

    lines := SplitString(ASection.contents, sLineBreak);

    for current_line in lines do
    begin
      line_split := SplitString(current_line, ' ');

      skip := 0;
      for i := 0 to Length(line_split) - 1 do
      begin
        if skip > 0 then
        begin
          dec(skip);
          continue;
        end;

        case line_split[i] of
        HEADER: begin
          skip := Length(line_split);
        end;
        SUB_HEADER: begin
          skip := Length(line_split);
        end;
        STYLE, COLOR: begin
          if Length(line_split) <= i+1 then
            continue;

          if line_split[i] = STYLE then
            AppendToStrArray(styles, linesplit[i+1])
          else
            AppendToStrArray(colors, linesplit[i+1];

          
        end;
        RESET_: begin
        end;
        RESET_ALL: ParseSection := ParseSection + STYLE_RESET;
        else
        begin
          { Leave out any unimplemented Switches }
          if pos('{$', line_split[i]) = 1 then
            continue;
          ParseSection := ParseSection + line_split[i] + ' ';
        end;
        end;
      end;

      ParseSection := ParseSection + sLineBreak;  end;
end.
