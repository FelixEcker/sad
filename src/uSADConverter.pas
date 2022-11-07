{$mode objfpc}
unit uSADConverter;

interface
  uses SysUtils, uSADParser;

  type
    TFormat = (fHTML);
    TSADConverter = class
    private
      FParser: TSADParser;
    public
      constructor Create(var AParser: TSADParser);

      function ConvertTo(const AFormat: TFormat): String;
    end;

  const
    HTML_BASE = '<!DOCTYPE html>' + sLineBreak
              + '<html>' + sLineBreak
              + '  <head>' + sLineBreak
              + '    <title> %s </title>' + sLineBreak
              + '  </head>' + sLineBreak
              + '  <body> ' + sLineBreak
              + '    %s' + sLineBreak
              + '  </body>' + sLineBreak
              + '</html>';
implementation
  constructor TSADConverter.Create(var AParser: TSADParser);
  begin
    FParser := AParser;
  end;

  function TSADConverter.ConvertTo(const AFormat: TFormat): String;
  var
    bytes: TByteArray;
  begin
    bytes := FParser.ParseFileAsBytes;

    if AFormat = fHTML then
    begin
      exit(HTML_BASE);
    end;
  end;
end.