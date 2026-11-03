{
  Project: Lape (https://github.com/nielsAD/lape)
  License: Mozilla Public License (http://mozilla.org/MPL/2.0)
}
unit lpdisassemblyhighlighter;

{$I lape.inc}

interface

uses
  Classes, SysUtils,
  SynEditHighlighter;

type
  ELapeDisassemblyToken = (dtText, dtHeader, dtAddress, dtName, dtDetail, dtConstant, dtOffset, dtString, dtLink);
  TLapeDisassemblyIsLink = function(const Hex: string): Boolean of object;
  TLapeDisassemblyHighlighter = class(TSynCustomHighlighter)
  protected
  type
    ELinePart = (lpWhole, lpAddress, lpName, lpDetail, lpOperands);
  protected
    FAttributes: array[ELapeDisassemblyToken] of TSynHighlighterAttributes;
    FOnIsLink: TLapeDisassemblyIsLink;
    FInTables: Boolean;
    FLineText: string;
    FLine: PChar;
    Run, FTokenPos: Integer;
    FToken: ELapeDisassemblyToken;
    FPart: ELinePart; // what the next token is
    FDepth: Integer; // how many [ are open
    FAfterDeref: Boolean; // )^ was the last thing: a number now is its offset

    procedure ScanOperand;

    function GetDefaultAttribute(Index: Integer): TSynHighlighterAttributes; override;
    function GetAttri(Token: ELapeDisassemblyToken): TSynHighlighterAttributes;
  public
    constructor Create(AOwner: TComponent); override;
    class function GetLanguageName: string; override;

    procedure SetLine(const NewValue: string; LineNumber: Integer); override;
    procedure Next; override;
    function GetEol: Boolean; override;
    function GetToken: string; override;
    procedure GetTokenEx(out TokenStart: PChar; out TokenLength: Integer); override;
    function GetTokenAttribute: TSynHighlighterAttributes; override;
    function GetTokenKind: Integer; override;
    function GetTokenPos: Integer; override;

    function GetRange: Pointer; override;
    procedure SetRange(Value: Pointer); override;
    procedure ResetRange; override;

    property OnIsLink: TLapeDisassemblyIsLink read FOnIsLink write FOnIsLink;
  published
    property TextAttri: TSynHighlighterAttributes index dtText read GetAttri;
    property HeaderAttri: TSynHighlighterAttributes index dtHeader read GetAttri;
    property AddressAttri: TSynHighlighterAttributes index dtAddress read GetAttri;
    property NameAttri: TSynHighlighterAttributes index dtName read GetAttri;
    property DetailAttri: TSynHighlighterAttributes index dtDetail read GetAttri;
    property ConstantAttri: TSynHighlighterAttributes index dtConstant read GetAttri;
    property OffsetAttri: TSynHighlighterAttributes index dtOffset read GetAttri;
    property StringAttri: TSynHighlighterAttributes index dtString read GetAttri;
    property LinkAttri: TSynHighlighterAttributes index dtLink read GetAttri;
  end;

implementation

const
  TokenNames: array[ELapeDisassemblyToken] of string = (
    'Text', 'Header', 'Address', 'Name', 'Detail', 'Constant', 'Offset', 'String', 'Link'
  );
  HexDigits = ['0'..'9', 'A'..'F'];
  NameChars = ['A'..'Z', 'a'..'z', '_', '0'..'9'];

constructor TLapeDisassemblyHighlighter.Create(AOwner: TComponent);
var
  Token: ELapeDisassemblyToken;
begin
  inherited Create(AOwner);

  for Token := Low(ELapeDisassemblyToken) to High(ELapeDisassemblyToken) do
  begin
    FAttributes[Token] := TSynHighlighterAttributes.Create(TokenNames[Token], TokenNames[Token]);
    AddAttribute(FAttributes[Token]);
  end;
end;

class function TLapeDisassemblyHighlighter.GetLanguageName: string;
begin
  Result := 'Lape disassembly';
end;

function TLapeDisassemblyHighlighter.GetDefaultAttribute(Index: Integer): TSynHighlighterAttributes;
begin
  case Index of
    SYN_ATTR_IDENTIFIER, SYN_ATTR_WHITESPACE: Result := FAttributes[dtText];
    SYN_ATTR_STRING: Result := FAttributes[dtString];
    SYN_ATTR_NUMBER: Result := FAttributes[dtConstant];
    else
      Result := nil;
  end;
end;

function TLapeDisassemblyHighlighter.GetAttri(Token: ELapeDisassemblyToken): TSynHighlighterAttributes;
begin
  Result := FAttributes[Token];
end;

procedure TLapeDisassemblyHighlighter.SetLine(const NewValue: string; LineNumber: Integer);
begin
  inherited SetLine(NewValue, LineNumber);

  FLineText := NewValue;
  FLine := PChar(FLineText);
  Run := 0;
  FDepth := 0;
  FAfterDeref := False;

  if (Copy(FLineText, 1, 3) = '-- ') then
  begin
    FPart := lpWhole;
    FInTables := True;
  end
  else if FInTables then
    FPart := lpOperands
  else if (FLine[0] = '$') then
    FPart := lpAddress
  else
    FPart := lpWhole;

  Next();
end;

procedure TLapeDisassemblyHighlighter.ScanOperand;
var
  Start: Integer;
begin
  Start := Run;
  FToken := dtText;

  case FLine[Run] of
    '"':
      begin
        Inc(Run);
        while (not (FLine[Run] in [#0, '"'])) do
          Inc(Run);
        if (FLine[Run] = '"') then
          Inc(Run);
        FToken := dtString;
      end;
    '$':
      begin
        Inc(Run);
        while (FLine[Run] in HexDigits) do
          Inc(Run);
        if (Run - Start = 1) then
          Exit;
        if Assigned(FOnIsLink) and FOnIsLink(Copy(FLineText, Start + 2, Run - Start - 1)) then
          FToken := dtLink
        else
          FToken := dtConstant;
      end;
    '0'..'9':
      begin
        while (FLine[Run] in ['0'..'9', 'A'..'F', 'a'..'f', 'x']) do
          Inc(Run);
        if (FDepth > 0) or FAfterDeref then
          FToken := dtOffset
        else
          FToken := dtConstant;
      end;
    'A'..'Z', 'a'..'z', '_':
      begin
        while (FLine[Run] in NameChars) do
          Inc(Run);
        if (Run - Start = 3) and (StrLComp(FLine + Start, 'nil', 3) = 0) then
          FToken := dtConstant;
      end;
    ' ', '+', '-':
      begin
        while (FLine[Run] in [' ', '+', '-']) do
          Inc(Run);
        Exit;
      end;
    else
      begin
        case FLine[Run] of
          '[': Inc(FDepth);
          ']': Dec(FDepth);
        end;
        Inc(Run);
        FAfterDeref := (FLine[Run - 1] = '^') and (Run > 1) and (FLine[Run - 2] = ')');
        Exit;
      end;
  end;
  FAfterDeref := False;
end;

procedure TLapeDisassemblyHighlighter.Next;
begin
  FTokenPos := Run;
  if (FLine[Run] = #0) then
    Exit;

  case FPart of
    lpWhole:
      begin
        Run := Length(FLineText);
        FToken := dtHeader;
      end;
    lpAddress:
      begin
        while (not (FLine[Run] in [#0, ' '])) do
          Inc(Run);
        FToken := dtAddress;
        FPart := lpName;
      end;
    lpName:
      if (FLine[Run] = ' ') then
      begin
        while (FLine[Run] = ' ') do
          Inc(Run);
        FToken := dtText;
      end
      else
      begin
        while (not (FLine[Run] in [#0, ' '])) do
          Inc(Run);
        FToken := dtName;
        FPart := lpDetail;
      end;
    lpDetail:
      begin
        while (FLine[Run] <> #0) and (not ((FLine[Run] = ' ') and (FLine[Run + 1] in [#0, ' ']))) do
          Inc(Run);
        FToken := dtDetail;
        FPart := lpOperands;
        if (Run = FTokenPos) then
          Next();
      end;
    lpOperands:
      ScanOperand();
  end;
end;

function TLapeDisassemblyHighlighter.GetEol: Boolean;
begin
  Result := FLine[FTokenPos] = #0;
end;

function TLapeDisassemblyHighlighter.GetToken: string;
begin
  Result := Copy(FLineText, FTokenPos + 1, Run - FTokenPos);
end;

procedure TLapeDisassemblyHighlighter.GetTokenEx(out TokenStart: PChar; out TokenLength: Integer);
begin
  TokenStart := FLine + FTokenPos;
  TokenLength := Run - FTokenPos;
end;

function TLapeDisassemblyHighlighter.GetTokenAttribute: TSynHighlighterAttributes;
begin
  Result := FAttributes[FToken];
end;

function TLapeDisassemblyHighlighter.GetTokenKind: Integer;
begin
  Result := Ord(FToken);
end;

function TLapeDisassemblyHighlighter.GetTokenPos: Integer;
begin
  Result := FTokenPos;
end;

function TLapeDisassemblyHighlighter.GetRange: Pointer;
begin
  Result := Pointer(PtrUInt(Ord(FInTables)));
end;

procedure TLapeDisassemblyHighlighter.SetRange(Value: Pointer);
begin
  FInTables := Value <> nil;
end;

procedure TLapeDisassemblyHighlighter.ResetRange;
begin
  FInTables := False;
end;

end.
