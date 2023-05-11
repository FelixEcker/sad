{$mode fpc}
unit uSADHTML;

{$H+}

interface
  uses uSADParser;

  function HTMLParseSection(const ASection: TSection;
                            const ADoChildren: Boolean): String;
implementation
  function HTMLParseSection(const ASection: TSection;
                            const ADoChildren: Boolean): String;
  begin
    HTMLParseSection := '';
  end;
end.
