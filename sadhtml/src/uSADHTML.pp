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
    preserve_mode, current_line, tmp: String;
    i, j, skip: Integer;
    section: TSection;
  begin
    HTMLParseSection := '';
    preserve_mode := ASection.owning_document^.preserve_mode;

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
          HTMLParseSection := HTMLParseSection + '<h2>' +
                                MergeStringArray(
                                Copy(line_split, i+1, Length(line_split)-1),
                                ' '
                              ) + '</h1>';
        end;
        SUB_HEADER: begin
          skip := Length(line_split);
          HTMLParseSection := HTMLParseSection + '<h3>' +
                                MergeStringArray(
                                Copy(line_split, i+1, Length(line_split)-1),
                                ' '
                              ) + '</h1>';
        end;
        STYLE, COLOR: begin
          if Length(line_split) <= i+1 then
            continue;

          tmp := Copy(
                  line_split[i+1],
                  1,
                  Length(line_split[i+1])-1
                );

          if line_split[i] = STYLE then
            AppendToStrArray(styles, tmp)
          else
            AppendToStrArray(colors, tmp);


          HTMLParseSection := HTMLParseSection + Format
            (
              '<span class="%s%s">',
              [
                Copy(line_split[i], 3, Length(line_split[i])),
                tmp
              ]
            );

          skip := 1;
        end;
        RESET_: begin
          for j := 0 to (Length(styles) + Length(colors)) - 1 do
            HTMLParseSection := HTMLParseSection + '</span>';

          if not (preserve_mode = STYLE) then
            SetLength(styles, 0)
          else if not (preserve_mode = COLOR) then
            SetLength(colors, 0);

          if (preserve_mode = '') then continue;

          for tmp in styles do
            HTMLParseSection := HTMLParseSection + '<span class="' +
                                preserve_mode +
                                tmp + '">';
        end;
        RESET_ALL: begin
          for j := 0 to (Length(styles) + Length(colors)) - 1 do
            HTMLParseSection := HTMLParseSection + '</span>';

          SetLength(styles, 0);
          SetLength(colors, 0);
        end;
        else
        begin
          { Leave out any unimplemented Switches }
          if pos('{$', line_split[i]) = 1 then
            continue;
          HTMLParseSection := HTMLParseSection + line_split[i] + ' ';
        end;
        end;
      end;

      HTMLParseSection := HTMLParseSection + '<br />' + sLineBreak;
    end;

    if not ADoChildren then exit;

    for section in ASection.children do
      HTMLParseSection := HTMLParseSection + HTMLParseSection(section, True);

  end;
end.
