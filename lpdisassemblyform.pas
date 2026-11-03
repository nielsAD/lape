{
  Project: Lape (https://github.com/nielsAD/lape)
  License: Mozilla Public License (http://mozilla.org/MPL/2.0)
}
unit lpdisassemblyform;

{$I lape.inc}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, StdCtrls,
  SynEdit, SynEditMiscClasses,
  lpcompiler, lpdisassemblyhighlighter;

type
  TLapeDisassemblyCreateEditor = function(Owner: TComponent): TSynEdit;
  TLapeDisassemblyForm = class(TForm)
  protected
    FEdit: TSynEdit;
    FFind: TComboBox;
    FInTables: Boolean;
    FHighlighter: TLapeDisassemblyHighlighter;
    FAddressToRow: TStringList;
    FHovered: Integer;
    FLinkedColor: TColor;

    procedure Print(const Line: string);
    function RowOf(Address: Int64): Integer;
    function IsLink(const Hex: string): Boolean;
    function LinkAt(Pixels: TPoint): Integer;
    procedure ShowRow(Row: Integer);
    procedure LineMarkup(Sender: TObject; Line: Integer; var Special: Boolean; Markup: TSynSelectedColor);
    procedure EditMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure EditMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure EditMouseLeave(Sender: TObject);
    procedure FindNext(Backwards: Boolean);
    procedure FindChange(Sender: TObject);
    procedure FindSelect(Sender: TObject);
    procedure FindKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);

    procedure DoClose(var CloseAction: TCloseAction); override;
  public
    constructor Create(TheOwner: TComponent; CreateEditor: TLapeDisassemblyCreateEditor = nil); reintroduce;
    destructor Destroy; override;

    procedure Disassemble(Compiler: TLapeCompiler);

    property Edit: TSynEdit read FEdit;
    property Highlighter: TLapeDisassemblyHighlighter read FHighlighter;
    property LinkedColor: TColor read FLinkedColor write FLinkedColor;
  end;

implementation

uses
  Math, LCLType,
  SynEditTypes, SynEditHighlighter,
  lpvartypes, lpdisassembler;

constructor TLapeDisassemblyForm.Create(TheOwner: TComponent; CreateEditor: TLapeDisassemblyCreateEditor);
var
  i: Integer;
begin
  inherited CreateNew(TheOwner);

  Caption := 'Disassembly';
  Width := Round(Screen.WorkAreaWidth * 0.6);
  Height := Round(Screen.WorkAreaHeight * 0.75);
  Position := poOwnerFormCenter;

  if Assigned(CreateEditor) then
    FEdit := CreateEditor(Self)
  else
    FEdit := TSynEdit.Create(Self);
  FEdit.Parent := Self;
  FEdit.Align := alClient;
  FEdit.ReadOnly := True;
  for i := 0 to FEdit.Gutter.Parts.Count - 1 do
    FEdit.Gutter.Parts[i].Visible := False;
  FEdit.Gutter.LeftOffset := 0;
  FEdit.Gutter.RightOffset := Scale96ToScreen(6);
  FEdit.OnSpecialLineMarkup := @LineMarkup;
  FEdit.OnMouseUp := @EditMouseUp;
  FEdit.OnMouseMove := @EditMouseMove;
  FEdit.OnMouseLeave := @EditMouseLeave;

  FFind := TComboBox.Create(Self);
  FFind.Parent := Self;
  FFind.Align := alTop;
  FFind.Style := csDropDown;
  FFind.DropDownCount := 30;
  FFind.AutoComplete := False;
  FFind.BorderSpacing.Around := Scale96ToScreen(4);
  FFind.OnChange := @FindChange;
  FFind.OnSelect := @FindSelect;
  FFind.OnKeyDown := @FindKeyDown;

  KeyPreview := True;
  OnKeyDown := @FormKeyDown;

  FHighlighter := TLapeDisassemblyHighlighter.Create(Self);
  FHighlighter.OnIsLink := @IsLink;
  FAddressToRow := TStringList.Create();
  FAddressToRow.Sorted := True;
  FHovered := -1;
  FLinkedColor := clNone;
end;

destructor TLapeDisassemblyForm.Destroy;
begin
  FAddressToRow.Free();
  inherited Destroy();
end;

procedure TLapeDisassemblyForm.DoClose(var CloseAction: TCloseAction);
begin
  inherited DoClose(CloseAction);
  CloseAction := caFree;
end;

procedure TLapeDisassemblyForm.Disassemble(Compiler: TLapeCompiler);
begin
  FEdit.BeginUpdate();
  FFind.Items.BeginUpdate();
  try
    DisassembleCode(Compiler.Emitter.Code, Compiler.Emitter.CodeLen, [Compiler.ManagedDeclarations.GetByClass(TLapeGlobalVar), Compiler.GlobalScope.GetByClass(TLapeGlobalVar)], @Print);
    FEdit.Highlighter := FHighlighter;
  finally
    FFind.Items.EndUpdate();
    FEdit.EndUpdate();
  end;
  if (FFind.Items.Count > 0) then
    FFind.ItemIndex := 0;
end;

procedure TLapeDisassemblyForm.Print(const Line: string);
var
  i: Integer;
  Value: Int64;
begin
  if (Copy(Line, 1, 3) = '-- ') then
    FInTables := True;
  if (Copy(Line, 1, 3) = '-- ') or ((not FInTables) and (Line <> '') and (Line[1] = '"')) then
    FFind.Items.AddObject(Line, TObject(PtrInt(FEdit.Lines.Count)));

  if (Line <> '') and (Line[1] = '$') then
  begin
    i := System.Pos(' ', Line);
    if (i = 0) then
      i := Length(Line) + 1;
    Value := StrToInt64Def(Copy(Line, 1, i - 1), -1);
    if (Value >= 0) and (RowOf(Value) < 0) then
      FAddressToRow.AddObject(IntToHex(Value, 8), TObject(PtrInt(FEdit.Lines.Count)));
  end;
  FEdit.Lines.Add(Line);
end;

function TLapeDisassemblyForm.RowOf(Address: Int64): Integer;
var
  i: Integer;
begin
  Result := -1;
  i := FAddressToRow.IndexOf(IntToHex(Address, 8));
  if (i >= 0) then
    Result := PtrInt(FAddressToRow.Objects[i]);
end;

function TLapeDisassemblyForm.IsLink(const Hex: string): Boolean;
begin
  Result := RowOf(StrToInt64Def('$' + Hex, -1)) >= 0;
end;

function TLapeDisassemblyForm.LinkAt(Pixels: TPoint): Integer;
var
  Token: string;
  Kind, Start: Integer;
  Attri: TSynHighlighterAttributes;
begin
  Result := -1;
  if FEdit.GetHighlighterAttriAtRowColEx(FEdit.PixelsToLogicalPos(Pixels), Token, Kind, Start, Attri) and (Kind = Ord(dtLink)) then
    Result := RowOf(StrToInt64Def(Token, -1));
end;

procedure TLapeDisassemblyForm.ShowRow(Row: Integer);
begin
  FEdit.CaretY := Row + 1;
  FEdit.CaretX := 1;
  FEdit.TopLine := Max(1, FEdit.CaretY - FEdit.LinesInWindow div 2);
end;

procedure TLapeDisassemblyForm.LineMarkup(Sender: TObject; Line: Integer; var Special: Boolean; Markup: TSynSelectedColor);
begin
  if (Line - 1 = FHovered) then
  begin
    Special := True;
    Markup.Background := FLinkedColor;
    Markup.Foreground := clNone;
    Markup.Style := [fsBold];
  end;
end;

procedure TLapeDisassemblyForm.EditMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
var
  Pixels: TPoint;
  Row: Integer;
begin
  Pixels.X := X;
  Pixels.Y := Y;
  Row := LinkAt(Pixels);
  if (Row <> FHovered) then
  begin
    FHovered := Row;
    FEdit.Invalidate();
  end;
end;

procedure TLapeDisassemblyForm.EditMouseLeave(Sender: TObject);
begin
  if (FHovered >= 0) then
  begin
    FHovered := -1;
    FEdit.Invalidate();
  end;
end;

procedure TLapeDisassemblyForm.EditMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  Pixels: TPoint;
  Row: Integer;
begin
  if (Button <> mbLeft) or FEdit.SelAvail then
    Exit;
  Pixels.X := X;
  Pixels.Y := Y;
  Row := LinkAt(Pixels);
  if (Row >= 0) then
    ShowRow(Row);
end;

procedure TLapeDisassemblyForm.FindNext(Backwards: Boolean);
var
  Options: TSynSearchOptions;
  BlockBegin, BlockEnd: TPoint;
  TopLine: Integer;
  Found: Boolean;
begin
  if (FFind.Text = '') then
    Exit;
  Options := [];
  if Backwards then
    Include(Options, ssoBackwards);
  BlockBegin := FEdit.BlockBegin;
  BlockEnd := FEdit.BlockEnd;
  TopLine := FEdit.TopLine;

  Found := FEdit.SearchReplace(FFind.Text, '', Options) > 0;
  if (not Found) then
  begin
    if Backwards then
    begin
      FEdit.CaretY := FEdit.Lines.Count;
      FEdit.CaretX := Length(FEdit.Lines[FEdit.Lines.Count - 1]) + 1;
    end
    else
    begin
      FEdit.CaretY := 1;
      FEdit.CaretX := 1;
    end;
    Found := FEdit.SearchReplace(FFind.Text, '', Options) > 0;
  end;

  if (not Found) then
  begin
    FEdit.LogicalCaretXY := BlockEnd;
    FEdit.BlockBegin := BlockBegin;
    FEdit.BlockEnd := BlockEnd;
    FEdit.TopLine := TopLine;
  end
  else if (FEdit.CaretY < FEdit.TopLine) or (FEdit.CaretY >= FEdit.TopLine + FEdit.LinesInWindow) then
    FEdit.TopLine := Max(1, FEdit.CaretY - FEdit.LinesInWindow div 2);
end;

procedure TLapeDisassemblyForm.FindSelect(Sender: TObject);
begin
  if (FFind.ItemIndex >= 0) then
  begin
    FEdit.SetHighlightSearch('', []);
    ShowRow(PtrInt(FFind.Items.Objects[FFind.ItemIndex]));
  end;
end;

procedure TLapeDisassemblyForm.FindChange(Sender: TObject);
begin
  if (FFind.Items.IndexOf(FFind.Text) >= 0) then
    Exit;
  FEdit.SetHighlightSearch(FFind.Text, []);
  if (FFind.Text = '') then
    Exit;
  FEdit.LogicalCaretXY := FEdit.BlockBegin;
  FEdit.BlockEnd := FEdit.BlockBegin;
  FindNext(False);
end;

procedure TLapeDisassemblyForm.FindKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if FFind.DroppedDown then
    Exit;
  if (Key = VK_RETURN) or (Key = VK_F3) then
  begin
    FindNext(ssShift in Shift);
    Key := 0;
  end;
end;

procedure TLapeDisassemblyForm.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if (Key = VK_F) and (Shift = [ssCtrl]) then
  begin
    FFind.SetFocus();
    FFind.SelectAll();
    Key := 0;
  end
  else if (Key = VK_F3) and (not FFind.Focused) then
  begin
    FindNext(ssShift in Shift);
    Key := 0;
  end
  else if (Key = VK_ESCAPE) and FFind.Focused and (not FFind.DroppedDown) then
  begin
    FEdit.SetFocus();
    Key := 0;
  end;
end;

end.
