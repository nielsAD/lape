{
  Author: Niels A.D
  Project: Lape (https://github.com/nielsAD/lape)
  License: Mozilla Public License (http://mozilla.org/MPL/2.0)

  Method parsing, in the order that a method is written: its header, its directives (which declare it), its body.
  Methods are plain declarations. Several can have one name, the newest with the same parameters hides the others,
  and a call finds its own: see findMethods and TLapeTree_MethodByName. The body comes after every declaration.
}
unit lpmethodparser;

{$I lape.inc}

interface

uses
  SysUtils,
  lptypes, lpvartypes, lptree, lpcompiler;

type
  TLapeParsedMethod = record
    Imported: Boolean;
    Header: TLapeType_Method;
    Method: TLapeGlobalVar;
    Overridden: TLapeGlobalVar;
  end;

  // Never created: a compiler is cast to it. (A class helper breaks incremental builds: lpcompiler uses this unit)
  TLapeMethodParser = class(TLapeCompiler)
  private
    function ParseParamDefault(const Param: TLapeParameter; Def: EMethodDef): TLapeVar;
    procedure ParseParams(var M: TLapeParsedMethod);
    procedure NewHeader(var M: TLapeParsedMethod; AName: lpString; Def: EMethodDef; AType: TLapeType = nil);
    function ParseTypeName(var AName: lpString; const TypePos: TDocPos): TLapeType;
    procedure ParseOperatorHeader(var M: TLapeParsedMethod);
    procedure ParsePropertyHeader(var M: TLapeParsedMethod);
    procedure ParseFunctionHeader(var M: TLapeParsedMethod);
    procedure ParseHeader(var M: TLapeParsedMethod);

    function DeclaredIn(const M: TLapeParsedMethod): TLapeDeclarationList;
    procedure TakeOuterVars(var M: TLapeParsedMethod);
    procedure FillStackInfo(const M: TLapeParsedMethod);
    procedure NewMethod(var M: TLapeParsedMethod);
    procedure AddMethod(var M: TLapeParsedMethod);
    procedure MakeInherited(const M: TLapeParsedMethod);
    procedure OverrideMethod(var M: TLapeParsedMethod);
    function ParseDirectives(var M: TLapeParsedMethod): Boolean;
    function Parse(var M: TLapeParsedMethod): TLapeTree_Method;
  public
    // The header of a method, or of a method type
    function ParseMethodHeader: TLapeType_Method;
    // A method as a script has it, from its first word
    function ParseMethod: TLapeTree_Method;
    // One that the program that runs Lape gives: a header, without body
    function ParseImportedMethod: TLapeGlobalVar;
    // What comes after AHeader, which the compiler made: what has no name in it is Param0, Param1 and so on
    function ParseMethodBody(AHeader: TLapeType_Method): TLapeTree_Method;
    // What comes after the header of Method, in its scope
    procedure ParseBody(Method: TLapeTree_Method);
  end;

implementation

uses
  lptokenizer, lpmessages, lpinterpreter_types;

function TLapeMethodParser.ParseParamDefault(const Param: TLapeParameter; Def: EMethodDef): TLapeVar;
var
  Default: TLapeTree_ExprBase;
begin
  if (Def = mdProperty) then
    LapeException(lpeDefaultParamInProperties, Tokenizer.DocPos);

  Default := EnsureExpression(ParseExpression([tk_sym_ParenthesisClose], True, False)).setExpectedType(Param.VarType) as TLapeTree_ExprBase;
  try
    Result := Default.Evaluate();
    if (not (Param.ParType in Lape_ValParams)) and ((Result = nil) or Result.isConstant) then
      LapeException(lpeVariableExpected, Default.DocPos);
  finally
    Default.Free();
  end;
end;

// (the tokenizer is on the bracket that opens them)
procedure TLapeMethodParser.ParseParams(var M: TLapeParsedMethod);
var
  Param: TLapeParameter;
  Identifiers: TStringArray;
  Positions: TDocPosArray;
  i: Integer;
begin
  repeat
    Param := NullParameter;

    case Next() of
      tk_NULL: Break;
      tk_sym_ParenthesisClose:
        begin
          if (FLastTok <> tk_sym_ParenthesisOpen) then
            Expect(tk_sym_SemiColon, False, False);
          Break;
        end;
      tk_kw_Const:    Param.ParType := lptConst;
      tk_kw_ConstRef: Param.ParType := lptConstRef;
      tk_kw_Out:      Param.ParType := lptOut;
      tk_kw_Var:      Param.ParType := lptVar;
    end;

    Identifiers := ParseIdentifierList(Positions, Param.ParType <> NullParameter.ParType);
    Expect([tk_sym_Colon, tk_sym_SemiColon, tk_sym_ParenthesisClose], False, False);
    if (Tokenizer.Tok = tk_sym_Colon) then
    begin
      Param.VarType := ParseType(nil, True);
      if (Param.VarType = nil) then
        LapeException(lpeTypeExpected, Tokenizer.DocPos);
      Expect([tk_sym_Equals, tk_sym_SemiColon, tk_sym_ParenthesisClose], False, False);

      if (Tokenizer.Tok = tk_sym_Equals) then
      begin
        Param.Default := ParseParamDefault(Param, M.Header.MethodDef);
        Expect([tk_sym_SemiColon, tk_sym_ParenthesisClose], False, False);
      end;
    end
    else if (not (Param.ParType in Lape_RefParams)) then
      Expect(tk_sym_Colon, False, False);

    for i := 0 to High(Identifiers) do
    begin
      Param.Name := Identifiers[i];
      Param.DocPos := Positions[i];
      M.Header.addParam(Param);
    end;
  until (Tokenizer.Tok = tk_sym_ParenthesisClose);
end;

procedure TLapeMethodParser.NewHeader(var M: TLapeParsedMethod; AName: lpString; Def: EMethodDef; AType: TLapeType = nil);
begin
  if (AType <> nil) then
    M.Header := TLapeType_MethodOfType.Create(Self, AType, nil, AName)
  else
    M.Header := TLapeType_Method.Create(Self, nil, AName);
  M.Header := addManagedType(M.Header) as TLapeType_Method;
  M.Header.DocPos := Tokenizer.DocPos;
  M.Header.MethodDef := Def;
end;

// TFoo.Name, on the dot: gives TFoo
function TLapeMethodParser.ParseTypeName(var AName: lpString; const TypePos: TDocPos): TLapeType;
var
  Typ: TLapeDeclaration;
begin
  Typ := getDeclaration(AName, nil, False, False);
  if (Typ is TLapeVar) then
    Typ := TLapeVar(Typ).VarType;

  Expect(tk_Identifier, True, False);
  AName := Tokenizer.TokString;

  if (not (Typ is TLapeType)) then
    LapeException(lpeTypeExpected, TypePos);
  Result := TLapeType(Typ);
end;

// operator +(Left, Right: T): T;
procedure TLapeMethodParser.ParseOperatorHeader(var M: TLapeParsedMethod);
var
  Pos: TDocPos;
  Op: EOperator;
  Left, Right: TLapeType;
begin
  Pos := Tokenizer.DocPos;

  Next();
  Op := op_Unknown;
  if (Tokenizer.Tok in ParserToken_Operators) then
    Op := ParserTokenToOperator(Tokenizer.Tok);
  if (not (Op in OverloadableOperators)) then
    LapeExceptionFmt(lpeCannotOverloadOperator, [Tokenizer.TokString], Tokenizer.DocPos);

  NewHeader(M, '!op_' + op_name[Op], mdOperator);
  if isNext([tk_sym_ParenthesisOpen]) then
    ParseParams(M);

  if (M.Header.Params.Count <> 2) then
    LapeExceptionFmt(lpeInvalidOperator, [op_name[Op], 2], Pos);
  Left := M.Header.Params[0].VarType;
  Right := M.Header.Params[1].VarType;
  if (Left = nil) or (Right = nil) then
    LapeException(lpeTypeExpected, Pos);
  if (Left.EvalRes(Op, Right) <> nil) then
    LapeExceptionFmt(lpeCannotOverrideOperator, [op_name[Op], Left.AsString, Right.AsString], Pos);
end;

// property TFoo.Name: T;  property TFoo.Name(Value: T);
procedure TLapeMethodParser.ParsePropertyHeader(var M: TLapeParsedMethod);
var
  AName: lpString;
  AType: TLapeType;
  TypePos: TDocPos;
begin
  Expect(tk_Identifier, True, False);
  AName := Tokenizer.TokString;
  TypePos := Tokenizer.DocPos;
  if (not isNext([tk_sym_Dot])) then
    LapeException(lpeExpectedProperty, Tokenizer.DocPos);

  AType := ParseTypeName(AName, TypePos);
  NewHeader(M, AName, mdProperty, AType);
  if isNext([tk_sym_ParenthesisOpen]) then
    ParseParams(M);
end;

// function Name(...): T;  procedure TFoo.Name(...);  and a method type, which has no name
procedure TLapeMethodParser.ParseFunctionHeader(var M: TLapeParsedMethod);
var
  AName: lpString;
  AType: TLapeType;
  TypePos: TDocPos;
begin
  AName := '';
  AType := nil;

  if isNext([tk_Identifier]) then
  begin
    AName := Tokenizer.TokString;
    TypePos := Tokenizer.DocPos;
    if isNext([tk_sym_Dot]) then
      AType := ParseTypeName(AName, TypePos);
  end;

  NewHeader(M, AName, mdUnspecified, AType);
  if isNext([tk_sym_ParenthesisOpen]) then
    ParseParams(M)
  else if (lcoMethodDeclarationParentheses in FOptions) then
  begin
    Next();
    LapeException(lpeMethodDeclarationParenthesesExpected, Tokenizer.DocPos);
  end;
end;

procedure TLapeMethodParser.ParseHeader(var M: TLapeParsedMethod);
var
  Keyword: EParserToken;
begin
  Keyword := Tokenizer.Tok;
  case Keyword of
    tk_kw_Operator: ParseOperatorHeader(M);
    tk_kw_Property: ParsePropertyHeader(M);
    else            ParseFunctionHeader(M);
  end;

  // (a property's is there when it reads)
  Next();
  if (Keyword in [tk_kw_Function, tk_kw_Operator]) or ((Keyword = tk_kw_Property) and (Tokenizer.Tok = tk_sym_Colon)) then
  begin
    Expect(tk_sym_Colon, False, False);
    M.Header.Res := ParseType(nil);
    if (M.Header.Res = nil) then
      LapeException(lpeTypeExpected, Tokenizer.DocPos);
  end;
end;

function TLapeMethodParser.DeclaredIn(const M: TLapeParsedMethod): TLapeDeclarationList;
begin
  if (M.Header.ObjectType <> nil) then
    Result := M.Header.ObjectType.Declarations
  else
    Result := FStackInfo.Owner;
end;

// A method in a method takes the variables of that as the first things that it takes
procedure TLapeMethodParser.TakeOuterVars(var M: TLapeParsedMethod);
var
  i: Integer;
  Around: TLapeStackInfo;
  Header: TLapeType_Method;
  Param: TLapeParameter;
begin
  Around := FStackInfo.Owner;
  if (Around.VarCount < 1) then
    Exit;

  Header := TLapeType_Method.Create(M.Header);
  for i := 0 to Around.VarCount - 1 do
    if (not (Around[i] is TLapeStackTempVar)) and (Around[i].Name <> '') then
    begin
      Param := NullParameter;
      Param.VarType := Around[i].VarType;
      Param.Default := Around[i];
      if Around[i].isConstant then
        Param.ParType := lptConstRef
      else
        Param.ParType := lptVar;

      Header.Params.Insert(Param, Header.ImplicitParams);
      Header.ImplicitParams := Header.ImplicitParams + 1;
    end;

  M.Header := TLapeType_Method(addManagedType(Header));
end;

// What the method starts with: its Self, what it has of the method around it, what it takes, and its result
procedure TLapeMethodParser.FillStackInfo(const M: TLapeParsedMethod);

  procedure CheckName(AName: lpString; const APos: TDocPos);
  begin
    if LapeSameString(M.Header.Name, AName) or hasDeclaration(AName, nil, True) then
      LapeExceptionFmt(lpeDuplicateDeclaration, [AName], APos);
  end;

var
  i: Integer;
  Param: TLapeParameter;
  ResultVar: TLapeStackVar;
  SelfType: TLapeType;
begin
  FStackInfo.Method := M.Header;

  if (M.Header is TLapeType_MethodOfType) then
    FStackInfo.addSelfVar(Lape_SelfParam, M.Header.ObjectType)
  else if (M.Header is TLapeType_MethodOfObject) then
    FStackInfo.addSelfVar(lptConstRef, getGlobalType('ConstPointer'))
  else if (M.Header.ObjectType <> nil) then
    // (Self is the type, for Self.x)
  begin
    SelfType := M.Header.ObjectType.CreateAlias();
    SelfType.Name := 'Self';
    FStackInfo.addDeclaration(addManagedType(SelfType));
  end;

  for i := 0 to M.Header.ImplicitParams - 1 do
    FStackInfo.inheritVar(M.Header.Params[i].Default as TLapeStackVar);

  for i := M.Header.ImplicitParams to M.Header.Params.Count - 1 do
  begin
    Param := M.Header.Params[i];
    if (Param.Name = '') then
      FStackInfo.addVar(Param.ParType, Param.VarType, lpString('Param' + IntToStr(i)))
    else
    begin
      CheckName(Param.Name, Param.DocPos);
      FStackInfo.addVar(Param.ParType, Param.VarType, Param.Name).DocPos := Param.DocPos;
    end;
  end;

  if (M.Header.Res <> nil) then
  begin
    CheckName('Result', M.Header.DocPos);
    // (tracked for the hint that it is not set, see TLapeTree_Method.Compile)
    ResultVar := FStackInfo.addResultVar(M.Header.Res);
    ResultVar.IsUsed := not (lcoHints in FOptions);
    ResultVar.DocPos := M.Header.DocPos;
  end;
end;

procedure TLapeMethodParser.NewMethod(var M: TLapeParsedMethod);
begin
  M.Method := M.Header.NewGlobalVar(EndJump);
  M.Method.DocPos := M.Header.DocPos;
  M.Method.isConstant := True;
  M.Method.Known := False;

  DeclaredIn(M).addDeclaration(M.Method);
end;

procedure TLapeMethodParser.AddMethod(var M: TLapeParsedMethod);
var
  Decls, Others: TLapeDeclArray;
  Def: EMethodDef;
begin
  if (M.Header.ObjectType = nil) and FStackInfo.Owner.isReserved(M.Header.Name) then
    LapeExceptionFmt(lpeDuplicateDeclaration, [M.Header.Name], Tokenizer.DocPos);

  Others := nil;
  Decls := DeclaredIn(M).GetAll(M.Header.Name);
  if (not addMethods(Others, Decls)) then
    LapeExceptionFmt(lpeDuplicateDeclaration, [M.Header.Name], Tokenizer.DocPos);

  if (SameMethod(Others, M.Header, True) <> nil) then
    LapeExceptionFmt(lpeDuplicateDeclaration, [M.Header.Name], Tokenizer.DocPos);

  if (Length(Others) > 0) then
  begin
    Def := TLapeType_Method(TLapeGlobalVar(Others[0]).VarType).MethodDef;
    if (Def <> M.Header.MethodDef) then
      LapeExceptionFmt(lpeCannotOverload2, [LapeMethodDefToString(Def), LapeMethodDefToString(M.Header.MethodDef)], Tokenizer.DocPos);
  end;

  NewMethod(M);
end;

// `inherited` in the method: what it overrides, called with what the method was called with when it is given nothing
procedure TLapeMethodParser.MakeInherited(const M: TLapeParsedMethod);
var
  Header: TLapeType_Method;
  Param: TLapeParameter;
  Source, Inh: TLapeGlobalVar;
  i, First: Integer;
begin
  Source := M.Overridden;
  if hasDeclaration('inherited', FStackInfo, True) then
    LapeExceptionFmt(lpeDuplicateDeclaration, ['inherited'], Tokenizer.DocPos);

  // (a plain method: of a property too, `inherited` is called)
  if (Source.VarType is TLapeType_MethodOfType) then
  begin
    // (it takes the Self of the method as a method in a method takes the variables around it)
    Header := TLapeType_Method.Create(TLapeType_Method(Source.VarType));
    Param := NullParameter;
    Param.ParType := Lape_SelfParam;
    Param.VarType := M.Header.ObjectType;
    Param.Default := FStackInfo[0];
    Header.Params.Insert(Param, 0);
    Header.ImplicitParams := Header.ImplicitParams + 1;
  end
  else
    Header := TLapeType_Method(Source.VarType.CreateCopy());
  Header := TLapeType_Method(addManagedType(Header));
  Header.MethodDef := mdUnspecified;

  First := Ord(MethodOfObject(Header));
  if (Header.ParamSize > 0) and (FStackInfo.TotalParamSize = Header.ParamSize) then
  begin
    for i := Header.ImplicitParams to Header.Params.Count - 1 do
    begin
      Param := Header.Params[i];
      Param.Default := FStackInfo[First + i];
      Header.Params[i] := Param;
    end;
  end;

  Inh := TLapeMethod.Create(Header, Source.Ptr, 'inherited');
  Inh.DocPos := M.Header.DocPos;
  Inh.isConstant := Source.isConstant;
  Inh.Known := Source.Known;
  FStackInfo.addDeclaration(Inh);
end;

// A newer method, which hides what it overrides: of its type and those it comes from, or of the scopes around it
procedure TLapeMethodParser.OverrideMethod(var M: TLapeParsedMethod);
var
  Candidates: TLapeDeclArray;
  Old: TLapeGlobalVar;
  OfHost: Boolean;
begin
  if (M.Header.ObjectType = nil) and (FStackInfo.Owner <> GlobalScope) then
    LapeException(lpeCannotOverrideNested, Tokenizer.DocPos);

  OfHost := False;
  if (M.Header.ObjectType <> nil) then
    Candidates := M.Header.ObjectType.findMethods(M.Header.Name)
  else
    Candidates := FStackInfo.Owner.findMethods(M.Header.Name);

  Old := SameMethod(Candidates, M.Header);
  if (Old = nil) and (M.Header.ObjectType = nil) then
    Old := GenerateMethod(M.Header.Name, nil, M.Header.ParamTypes(), M.Header.Res);
  if (Old = nil) and (Length(Candidates) = 1) then
    LapeException(lpeNoOverrideMatch, Tokenizer.DocPos);

  if (Old = nil) then
    LapeException(lpeUnknownParent, Tokenizer.DocPos);

  // (of ToString: another type can be what writes this one)
  if TLapeType_Method(Old.VarType).EqualParams(M.Header) then
  begin
    if (not TLapeType_Method(Old.VarType).EqualParams(M.Header, False)) then
      LapeException(lpeNoOverrideMatch, Tokenizer.DocPos);

    // (one that the program that runs Lape gave with an object of its own: that is the Self of what overrides it)
    OfHost := (not M.Imported) and (Old.VarType is TLapeType_MethodOfObject) and (not (Old.VarType is TLapeType_MethodOfType));
    if OfHost then
      M.Header := TLapeType_Method(addManagedType(TLapeType_MethodOfObject.Create(M.Header)));
    if (not M.Imported) and (MethodOfObject(Old.VarType) <> MethodOfObject(M.Header)) then
      LapeException(lpeNoOverrideMatch, Tokenizer.DocPos);
  end;

  NewMethod(M);
  if OfHost then
    TMethod(M.Method.Ptr^).Data := TMethod(Old.Ptr^).Data;
  M.Overridden := Old;
end;

// What comes after the header, which declares the method. True: its body comes next
function TLapeMethodParser.ParseDirectives(var M: TLapeParsedMethod): Boolean;
const
  Directives = [tk_kw_Static, tk_kw_Overload, tk_kw_Override, tk_kw_External] + ParserToken_Hints;
var
  IsStatic, IsOverride: Boolean;
  ObjectType: TLapeType;
begin
  Result := False;

  if (M.Header.Name = '') then
    LapeException(lpeBlockExpected, Tokenizer.DocPos)
  else if (M.Header.ObjectType <> nil) and (not hasDeclaration(M.Header.ObjectType, FStackInfo.Owner, True)) then
    LapeException(lpeParentOutOfScope, Tokenizer.DocPos);

  IsStatic := False;
  IsOverride := False;

  // (override comes last: its body is next)
  isNext(Directives);
  while (not IsOverride) and (Tokenizer.Tok in [tk_kw_Static, tk_kw_Overload, tk_kw_Override]) do
  begin
    case Tokenizer.Tok of
      tk_kw_Static:
        IsStatic := True;
      tk_kw_Override:
        if (M.Header.MethodDef = mdProperty) then
          LapeException(lpeCannotOverrideProperty, Tokenizer.DocPos)
        else
          IsOverride := True;
      // (every method overloads the others of its name)
      tk_kw_Overload:
        if (M.Header.MethodDef in [mdOperator, mdProperty]) then
          LapeExceptionFmt(lpeUnexpectedToken, [LapeTokenToString(Tokenizer.Tok)], DocPos);
    end;

    ParseExpressionEnd(tk_sym_SemiColon, True, False);
    if (not IsOverride) then
      isNext(Directives);
  end;

  if (not IsStatic) then
  begin
    if (not M.Imported) and (not MethodOfObject(M.Header)) and (M.Header.ObjectType = nil) then
      TakeOuterVars(M);
  end
  else if MethodOfObject(M.Header) then
  begin
    ObjectType := M.Header.ObjectType;
    M.Header := TLapeType_Method(addManagedType(TLapeType_Method.Create(M.Header)));
    M.Header.ObjectType := ObjectType;
  end;

  if IsOverride then
    OverrideMethod(M)
  else
    AddMethod(M);

  // (these need the method)
  if (Tokenizer.Tok = tk_kw_External) then
  begin
    if ({$IFNDEF FPC}@{$ENDIF}FOnHandleExternal = nil) then
      LapeException(lpeBlockExpected, Tokenizer.DocPos);
    if (not FOnHandleExternal(Self, M.Method)) then
      LapeException(lpeUnknownParent, Tokenizer.DocPos);

    ParseExpressionEnd(tk_sym_SemiColon, False, True);
  end
  else
  begin
    while (Tokenizer.Tok in ParserToken_Hints) do
    begin
      ParseDeclHint(M.Method.VarType);
      if (Peek() in ParserToken_Hints) then
        Next();
    end;

    Result := not M.Imported;
  end;
end;

// (M.Header: the compiler made it, there is none to read)
function TLapeMethodParser.Parse(var M: TLapeParsedMethod): TLapeTree_Method;
var
  Pos: TDocPos;
  HasBody: Boolean;
begin
  Result := nil;

  IncStackInfo([]);
  try
    try
      if (M.Header = nil) then
      begin
        ParseHeader(M);
        ParseExpressionEnd(tk_sym_SemiColon, False, False);
      end;

      Pos := Tokenizer.DocPos;
      HasBody := ParseDirectives(M);
      if (not HasBody) then
        Exit;

      FillStackInfo(M);
      if (M.Overridden <> nil) then
        MakeInherited(M);

      Result := TLapeTree_Method.Create(M.Method, FStackInfo, Self);
      Result.DocPos := Pos;

      if FDeferring then
        DeferBody(Result, nil)
      else
        ParseBody(Result);
    finally
      DecStackInfo([lsfFunction], Result = nil);
    end;
  except
    FreeAndNil(Result);
    raise;
  end;
end;

function TLapeMethodParser.ParseMethodHeader: TLapeType_Method;
var
  M: TLapeParsedMethod;
begin
  M := Default(TLapeParsedMethod);
  ParseHeader(M);
  Result := M.Header;
end;

function TLapeMethodParser.ParseMethod: TLapeTree_Method;
var
  M: TLapeParsedMethod;
begin
  M := Default(TLapeParsedMethod);
  Result := Parse(M);
end;

function TLapeMethodParser.ParseImportedMethod: TLapeGlobalVar;
var
  M: TLapeParsedMethod;
begin
  M := Default(TLapeParsedMethod);
  M.Imported := True;
  Parse(M);
  Result := M.Method;
end;

function TLapeMethodParser.ParseMethodBody(AHeader: TLapeType_Method): TLapeTree_Method;
var
  M: TLapeParsedMethod;
  OldDeferring: Boolean;
begin
  M := Default(TLapeParsedMethod);
  M.Header := AHeader;

  OldDeferring := FDeferring;
  FDeferring := False;
  try
    Result := Parse(M);
  finally
    FDeferring := OldDeferring;
  end;
end;

procedure TLapeMethodParser.ParseBody(Method: TLapeTree_Method);
begin
  Next();
  Method.Statements := ParseBlockList();

  if (Method.Statements = nil) or (not (Method.Statements.Statements[Method.Statements.Statements.Count - 1] is TLapeTree_StatementList)) then
    Expect(tk_kw_Begin, False, False);

  Assert(FStackInfo.WithStack.Count = 0);

  checkDeclHints(FStackInfo);
end;

end.
