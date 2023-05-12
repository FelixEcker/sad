unit uPathResolve;

{ uPathResolve.pas - Resolver for Environment variables in UNIX Paths }
{                                                                     }
{ Author: Felix Eckert                                                }
{ Created as part of "sadv",                                          }
{ Licensed under the 3-Clause BSD License. See below.                 }

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

{$H+}

interface
  uses Dos, SysUtils;//, Types;  

  { Check if Character is Valid for an Environment Variable Name }
  function ValidEnvChar(const AChar: Char): Boolean;

  { Resolve Environment Variables in Path }
  function ResolveEnvsInPath(const APath: String): String;

implementation
  function ValidEnvChar(const AChar: Char): Boolean;
  var
    bval: Byte;
  begin
    bval := Byte(AChar);
    if (bval > 47) and (bval < 58) then
      exit(True)
    else if (bval > 64) and (bval < 91) then
      exit(True)
    else
      exit(AChar = '_');
  end;

  function ResolveEnvsInPath(const APath: String): String;
  var
    i: Integer;
    c, varname, result: String;
    escaping, buildingVarName: Boolean;
  begin
    result := '';
    escaping := False;

    for i := 1 to Length(APath) do
    begin
      c := APath[i];

      if (c = '$') then
      begin
        if escaping then
          result := result + c
        else begin
          varname := '';
          buildingVarName := True;
          continue;
        end;
      end else if (c = '\') and not escaping then
      begin
        buildingVarName := False;
        result := result + GetEnv(varname);

        escaping := True;
        continue;
      end;

      if buildingVarName then
      begin
        if (ValidEnvChar(c[1])) then
          varname := varname + c
        else
        begin
          buildingVarName := False;
          result := result + GetEnv(varname) + c;
        end;
      end else
        result := result + c;

      escaping := False;
    end;

    ResolveEnvsInPath := result;
  end;
end.
