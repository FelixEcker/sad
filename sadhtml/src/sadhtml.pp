{$mode fpc}
program sadhtml;

{  sadhtml - Simple Ansi Document to HTML converter  }
{                                                    }
{ Author: Marie Eckert                               }
{ Licensed under the 3-Clause BSD License. See below }

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

uses SysUtils, uSADHTML;

const
  VERSION = '1.0.0';
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

end.
