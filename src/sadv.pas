{$mode objfpc}
program sadv;

uses SysUtils, uSADParser;

var
  i, lastparam: Integer;
  path, _line, required_section, cparam: String;
  print_meta: Boolean;
  parser: TSADParser;
begin
  if (ParamCount() = 0) then
  begin
    writeln('SNF Command Line Viewer ; 1.0.0 by Felix Eckert');
    writeln('Usage: snf [file] <parameters> <#section>');
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
