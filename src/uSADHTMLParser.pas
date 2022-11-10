{$mode objfpc}
unit uSADHTMLParser;

{  uSADHTMLParser.pas ; SAD -> HTML Parser for Simple-Ansi-Documents  }
{                                                                     }
{ Author: Felix Eckert                                                }
{ Written for sadv (SAD-Viewer), licensed under the 3-Clause          }
{ BSD License. See below.                                             }

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
  uses SysUtils, StrUtils, Types, uSADParser;

  type
    TSADHTMLParser = class(TSADParser)
    private
      FStyleOpen, FColorOpen: Boolean;
      FNStyles, FNColors: Integer;
      FPageTitle: String;
    public
      procedure Open; override;
      function NextLine: String; override;
      function WriteHtml(const APath, ACSS: String): String;
    end;
implementation
  procedure TSADHTMLParser.Open;
  begin
    FNStyles := 0;
    FNColors := 0;

    inherited Open;
  end;

  function TSADHTMLParser.NextLine: String;
  var
    i, j, skipWords: Integer;
    isBack, addTitle, addReset: Boolean;
    openTag: String;
    lsplit: TStringDynArray;
  begin
    // TODO: Implement Parsing
    readln(FFile, FLine);
    FLineNumber := FLineNumber + 1;

    result := '';
    lsplit := SplitString(FLine, ' ');
    skipWords := 0;
    addReset := False;
    addTitle := False;
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
            exit(Format('<div id="%s">', [FCurrentSection])); 
          end;
          '{$end-section}': begin 
            FCurrentSection := FPreviousSections[Length(FPreviousSections)-1];
            FPreviousSections := Copy(FPreviousSections, 1, Length(FPreviousSections)-1);
            exit('</div>'+sLineBreak); 
          end;

          '{$title}': begin
            addTitle := True;
          end;

          '{$head}': begin
            addReset := True;
            result := result + '<h2>';
            openTag := 'h2';
          end;

          '{$sub-head}': begin
            addReset := True;
            result := result + '<h3>';
            openTag := 'h3';
          end;

          { Styling }
          // Text-Color
          '{$color': begin
            if (Length(lsplit)-1 < i+1) then
              raise EMalformedDocumentException.Create('Missing color value for color-switch at line: '+IntToStr(FLineNumber));

            skipWords := 1;
            isBack := not EndsStr('}', lsplit[i+1]);
            if isBack then
            begin
              lsplit[i+1] := 'back'+Copy(lsplit[i+1], 1, Length(lsplit[i+1]));
              skipWords := skipWords + 1
            end else
              lsplit[i+1] := Copy(lsplit[i+1], 1, Length(lsplit[i+1])-1);

            FLastColor := lsplit[i+1];
  
            if FNColors > 0 then
            begin
              result := result + '</div>';
              FNColors := FNColors-1;
            end;
            result := result + '<span class="col'+FLastColor+'">';
            FNColors := FNColors+1;
          end;

          // Reset with preserves
          '{$reset}': begin
            result := Copy(result, 1, Length(result)-1) + '</div>';
            
            // This could be optimised a lot
            for j := 0 to FNColors+FNStyles-2 do
            begin
              case FPreserveMode of
                1: begin
                  if FLastStyle <> '' then 
                  begin
                    result := result + '<span class="stl'+FLastStyle+'">';
                    FNStyles := FNStyles - 1;
                  end;
                end;
                2: begin
                  if FLastColor <> '' then 
                  begin
                    result := result + '<span class="col'+FLastColor+'">';
                    FNColors := FNColors - 1;
                  end;
                end;
              end;
            end;
          end;

          '{$reset-all}': begin
            for j := 0 to FNColors+FNStyles-2 do
              result := Copy(result, 1, Length(result)-1) + '</span>';

            FNColors := 0;
            FNStyles := 0;
          end;

          // Text-Styling
          '{$style': begin
            if (Length(lsplit)-1 < i+1) then
              raise EMalformedDocumentException.Create('Missing style value for style-switch at line: '+IntToStr(FLineNumber));

            skipWords := 1;

            FLastStyle := Copy(lsplit[i+1], 1, Length(lsplit[i+1])-1);
            result := result + '<span class="stl'+FLastStyle+'">';
            FNStyles := FNStyles + 1;
          end;
        end;
      end else if (StartsStr('{{$', lsplit[i])) then
        result := result + Copy(lsplit[i], 2, Length(lsplit[i])) + ' '
      else
        result := result + lsplit[i] + ' ';
    end;

    // Remove trailing spaces
    {
    while (result[Length(result)] = ' ') do
      result := Copy(result, 1, Length(result)-1);
    }
    if addReset then result := result + Format('</%s>', [openTag]);
    if addTitle then begin FMetaData.title := result; result := '<h1>'+result+'</h1>' end;
  end;

  function TSADHTMLParser.WriteHtml(const APath, ACSS: String): String;
  var
    _file, cssfile: TextFile;
    tmp: String;
  begin
    Assign(_file, APath);
    ReWrite(_file);
    WriteLn(_file, Format('<html><head><title>%s</title></head><body><style>',
    [FMetaData.Title]));

    Assign(cssfile, ACSS);
    ReSet(cssfile);
    while not eof(cssfile) do
    begin
      ReadLn(cssfile, tmp);
      WriteLn(_file, tmp);
    end;
    Close(cssfile);
    WriteLn(_file, '</style>');

    while not IsEof() and not FinishedRequiredSection do
      WriteLn(_file, NextLine()+'<br>');

    WriteLn(_file, '</body></html>');
    Close(_file);
  end;
end.
