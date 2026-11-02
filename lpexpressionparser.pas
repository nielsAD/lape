{
  Author: Niels A.D
  Project: Lape (https://github.com/nielsAD/lape)
  License: Mozilla Public License (http://mozilla.org/MPL/2.0)

  Expression parsing: an operand (ParsePrimary) with the operators after it (ParseRightHandSide). Every part starts at
  its first token and ends at the first token after it. What a name means is said where it is used: see ValueOf.

  What a body names is known when it is parsed: bodies come after every declaration, see TLapeCompiler.DeferBody.
}
unit lpexpressionparser;

{$I lape.inc}

interface

uses
  lptypes, lpvartypes, lptokenizer, lptree, lpinvoke, lpcompiler;

type
  // Never created: a compiler is cast to it. (A class helper breaks incremental builds: lpcompiler uses this unit)
  TLapeExpressionParser = class(TLapeCompiler)
  private
    function ValueOf(Node: TLapeTree_ExprBase): TLapeTree_ExprBase;
    function UseOperands(Op: TLapeTree_Operator): TLapeTree_ExprBase;

    function ParseNumber(Signed: Boolean): TLapeTree_ExprBase;
    function ParseString: TLapeTree_ExprBase;
    function ParseOpenArray: TLapeTree_ExprBase;
    procedure ParseArguments(Call: TLapeTree_Invoke; Close: EParserToken = tk_sym_ParenthesisClose);
    procedure ParseSpecialParam(Method: TLapeTree_InternalMethod; Ends: EParserTokenSet);
    function ParseName: TLapeTree_ExprBase;
    function ParseIdentifier(Ends: EParserTokenSet): TLapeTree_ExprBase;

    procedure ParseMember(var Node: TLapeTree_ExprBase);
    procedure ParseIndex(var Node: TLapeTree_ExprBase);
    procedure ParseCast(var Node: TLapeTree_ExprBase);
    procedure ParseInvoke(var Node: TLapeTree_ExprBase);
    procedure ParseSelectors(var Node: TLapeTree_ExprBase);

    function ParsePrimary(Ends: EParserTokenSet): TLapeTree_ExprBase;
    function ParseRightHandSide(Left: TLapeTree_ExprBase; MaxPrecedence: Byte; Ends: EParserTokenSet): TLapeTree_ExprBase;
  public
    // An expression. Ends: tokens that end it (not inside brackets of its own)
    function Parse(Ends: EParserTokenSet; FirstNext, DoFold: Boolean): TLapeTree_ExprBase;
    // An expression, or a range: Lo..Hi
    function ParseRange(Ends: EParserTokenSet; FirstNext: Boolean): TLapeTree_Base;
  end;

implementation

uses
  lpinternalmethods, lpmessages;

// What is named, as a value: a property is read, @ of a method is the method, a method on its own takes () or @
function TLapeExpressionParser.ValueOf(Node: TLapeTree_ExprBase): TLapeTree_ExprBase;
begin
  Result := Node;

  if IsPropertyNode(Node) then
  begin
    Result := TLapeTree_InvokeProperty.Create(Node, Node);
  end
  else if (Node is TLapeTree_Operator) and (TLapeTree_Operator(Node).OperatorType = op_Addr) then
  begin
    if (TLapeTree_Operator(Node).Left is TLapeTree_MethodByName) or IsMethodNode(TLapeTree_Operator(Node).Left) then
    begin
      Result := TLapeTree_Operator(Node).Left;
      Result.Parent := nil;
      Node.Free();
    end;
  end
  else if ((Node is TLapeTree_Var) or (Node is TLapeTree_Member) or (Node is TLapeTree_MethodByName) or
           ((Node is TLapeTree_Operator) and (TLapeTree_Operator(Node).OperatorType in [op_Index, op_Deref]))) and
          IsMethodNode(Node)
  then
    LapeException(lpeMethodNotInvoked, Node.DocPos)
  else if (Node is TLapeTree_MethodByName) then
    LapeExceptionFmt(lpeUnknownDeclaration, [TLapeTree_MethodByName(Node).Name], Node.DocPos);
end;

// (the operands of Op as it uses them: an assignment to a property is a write, in Op's place)
function TLapeExpressionParser.UseOperands(Op: TLapeTree_Operator): TLapeTree_ExprBase;
var
  Prop: TLapeTree_InvokeProperty;
  Value: TLapeTree_ExprBase;
begin
  Result := Op;

  // (an index was a whole expression already)
  if (Op.Right <> nil) and (Op.OperatorType <> op_Index) then
    Op.Right := ValueOf(Op.Right);

  if (Op.OperatorType in AssignOperators) and (IsPropertyNode(Op.Left) or (Op.Left is TLapeTree_InvokeProperty)) then
  begin
    Value := Op.Right;
    Value.Parent := nil;

    if (Op.Left is TLapeTree_InvokeProperty) then
    begin
      Prop := TLapeTree_InvokeProperty(Op.Left);
      Prop.Parent := nil;
    end
    else
      Prop := TLapeTree_InvokeProperty.Create(Op.Left, Op);

    Prop.addParam(Value.FoldConstants() as TLapeTree_ExprBase);
    Prop.AssignOp := Op.OperatorType;

    Op.Free();
    Exit(Prop);
  end;

  // (a method variable assigned to stays one)
  if (Op.OperatorType <> op_Addr) and ((not (Op.OperatorType in AssignOperators)) or (not IsMethodNode(Op.Left))) then
    Op.Left := ValueOf(Op.Left);
end;

// (a minus right before a number is part of it: -128 is an Int8, and Int64's lowest only exists signed)
function TLapeExpressionParser.ParseNumber(Signed: Boolean): TLapeTree_ExprBase;
var
  Value: TLapeGlobalVar;
  IntValue: UInt64;
  FloatValue: lpFloat;
begin
  if (Tokenizer.Tok = tk_typ_Float) then
  begin
    if (not Tokenizer.TokFloat(FloatValue)) then
      LapeExceptionFmt(lpeInvalidNumber, [Tokenizer.TokString], Tokenizer.DocPos);
    if Signed then
      FloatValue := -FloatValue;
    Result := TLapeTree_Var.Create(getFloatConst(FloatValue), Self);
  end
  else
  begin
    if (not Tokenizer.TokInteger(IntValue)) then
      LapeExceptionFmt(lpeIntegerTooLarge, [Tokenizer.TokString], Tokenizer.DocPos);
    if Signed and (IntValue > UInt64(High(Int64)) + 1) then
      LapeExceptionFmt(lpeIntegerTooLarge, [lpString('-') + Tokenizer.TokString], Tokenizer.DocPos);

    // One past Int64 is a UInt64 of its own
    if Signed then
      Value := getIntConst(Int64(UInt64(0) - IntValue), ltNativeInt)
    else if (IntValue <= UInt64(High(Int64))) then
      Value := getIntConst(Int64(IntValue), ltNativeInt)
    else
    begin
      Value := addManagedDecl(getBaseType(ltUInt64).NewGlobalVarP()) as TLapeGlobalVar;
      PUInt64(Value.Ptr)^ := IntValue;
    end;
    Result := TLapeTree_Var.Create(Value, Self);
  end;
end;

// (of one token or of several written together: the last is the current token)
function TLapeExpressionParser.ParseString: TLapeTree_ExprBase;

  // A Char, or a WideChar when it is past 255
  function CharConst(Code: UInt32): TLapeGlobalVar;
  begin
    Assert(Code <= $FFFF);
    if (Code > 255) then
      Result := getCharConst(Code, ltWideChar)
    else
      Result := getCharConst(Code, ltChar);
  end;

var
  Str, Text, FirstText: lpString;
  StartPos: TDocPos;
  Token: EParserToken;
  Code, FirstCode: UInt32;
  Count: Integer;
  FirstIsChar: Boolean;
begin
  Result := nil;
  StartPos := Tokenizer.DocPos;
  Token := Tokenizer.Tok;
  FirstIsChar := (Token = tk_typ_Char);
  FirstCode := 0;
  Count := 0;

  repeat
    case Token of
      tk_typ_String, tk_typ_HereString:
        begin
          Text := Tokenizer.TokString;
          if (Count > 0) and (FLastTok = Token) then
            Str := Str + Text[1];
          Str := Str + Copy(Text, 2, Length(Text) - 2);
        end;
      tk_typ_Char:
        begin
          if (not Tokenizer.TokCharCode(Code)) then
            LapeExceptionFmt(lpeInvalidCharacter, [Tokenizer.TokString], Tokenizer.DocPos);
          if (Count = 0) then
          begin
            FirstCode := Code;
            FirstText := Tokenizer.TokString;
          end;

          // A string is of characters of one size. (The first can also be a character by itself, of any size:
          // which it is, is seen at the end.)
          if (Code <= Ord(High(lpChar))) then
            Str := Str + lpChar(Code)
          else if (Count > 0) then
            LapeExceptionFmt(lpeInvalidCharacterInString, [Tokenizer.TokString, Ord(High(lpChar))], Tokenizer.DocPos);
        end;
      else
        LapeException(lpeImpossible);
    end;
    Inc(Count);
  until (not (Tokenizer.getChar(1) in [#39, #34, '#'])) or
        (not isNext([tk_typ_String, tk_typ_HereString, tk_typ_Char], Token));

  if FirstIsChar and (Count = 1) then
    Result := TLapeTree_Var.Create(CharConst(FirstCode), Self)
  else if FirstIsChar and (FirstCode > Ord(High(lpChar))) then
    LapeExceptionFmt(lpeInvalidCharacterInString, [FirstText, Ord(High(lpChar))], StartPos)
  else if (Count = 1) and (Length(Str) = 1) then
    Result := TLapeTree_Var.Create(CharConst(Ord(Str[1])), Self)
  else
    Result := TLapeTree_Var.Create(getStringConst(Str), Self);
  Result.DocPos := StartPos;
end;

// ([a, b..c]: the ']' is the current token)
function TLapeExpressionParser.ParseOpenArray: TLapeTree_ExprBase;

  function Value(FirstNext: Boolean): TLapeTree_Base;
  begin
    Result := ParseTypeExpression([tk_sym_BracketClose, tk_sym_Comma], FirstNext);
    if (Result = nil) then
      LapeException(lpeExpressionExpected, Tokenizer.DocPos);
  end;

var
  OpenArr: TLapeTree_OpenArray;
begin
  OpenArr := TLapeTree_OpenArray.Create(Self);
  try
    if (Next() <> tk_sym_BracketClose) then
    begin
      OpenArr.addValue(Value(False));
      while True do
        case Tokenizer.Tok of
          tk_sym_BracketClose: Break;
          tk_sym_Comma: OpenArr.addValue(Value(True));
          else Expect(tk_sym_BracketClose, False, False);
        end;
    end;
  except
    OpenArr.Free();
    raise;
  end;
  Result := OpenArr;
end;

// (from its '(' or '['; Close, its ')' or ']', is the current token)
procedure TLapeExpressionParser.ParseArguments(Call: TLapeTree_Invoke; Close: EParserToken);

  function Argument(FirstNext: Boolean): TLapeTree_ExprBase;
  begin
    Result := ParseExpression([Close, tk_sym_Comma], FirstNext, True);
    if (Result = nil) then
      LapeException(lpeExpressionExpected, Tokenizer.DocPos);
  end;

begin
  if (Next() = Close) then
    Exit;

  Call.addParam(Argument(False));
  while (Tokenizer.Tok = tk_sym_Comma) do
    Call.addParam(Argument(True));

  if (Tokenizer.Tok <> Close) then
    if (Close = tk_sym_BracketClose) then
      LapeException(lpeClosingBracketExpected, Tokenizer.DocPos)
    else
      LapeException(lpeClosingParenthesisExpected, Tokenizer.DocPos);

  // (what a call means is known now: every declaration is, and what it is given)
  if (Call.Callee is TLapeTree_MethodByName) then
    TLapeTree_MethodByName(Call.Callee).Resolve(Call.Params);
end;

// (what an internal method takes without brackets after its name, the current token)
procedure TLapeExpressionParser.ParseSpecialParam(Method: TLapeTree_InternalMethod; Ends: EParserTokenSet);
begin
  case Method.SpecialParam of
    spForce:
      if (not (Tokenizer.Tok in Ends)) then
        Method.addParam(EnsureExpression(ParseExpression(Ends, False)));

    spTypeThenParams:
      begin
        Method.addParam(EnsureExpression(getExpression(Tokenizer.TokString)));
        if (Expect([tk_sym_ParenthesisOpen, tk_sym_SemiColon]) = tk_sym_ParenthesisOpen) then
        begin
          ParseArguments(Method);
          Expect(tk_sym_ParenthesisClose, False, True);
        end;
      end;

    spType:
      begin
        Expect(tk_cmp_LessThan, False, False);
        Method.addParam(EnsureExpression(ParseExpression([tk_cmp_GreaterThan, tk_sym_Comma], True, True)));
        while True do
          case Tokenizer.Tok of
            tk_cmp_GreaterThan: Break;
            tk_sym_Comma:
              Method.addParam(EnsureExpression(ParseExpression([tk_cmp_GreaterThan, tk_sym_Comma], True, True)));
            else
              LapeException(lpeClosingParenthesisExpected, Tokenizer.DocPos);
          end;
        Expect(tk_cmp_GreaterThan, False, True);
      end;
  end;
end;

// (the current token, and System. in front of it)
function TLapeExpressionParser.ParseName: TLapeTree_ExprBase;
begin
  // (no declaration can be called System: an internal method there is the real one, whatever the script has declared)
  if (Peek() = tk_sym_Dot) and LapeSameString(Tokenizer.TokString, LapeSystemName) then
  begin
    Next();
    Expect(tk_Identifier, True, False);
    if (FInternalMethods[Tokenizer.TokString] <> nil) then
      Result := FInternalMethods[Tokenizer.TokString].Create(Self)
    else
      Result := GlobalNode(Tokenizer.TokString, Self);
    Result.DocPos := getDocPos();
    Exit;
  end;

  Result := getExpression(Tokenizer.TokString);

  // (what is called can be made for what it is called with: see addMethodGenerate)
  if (Result = nil) then
    if (Peek() = tk_sym_ParenthesisOpen) then
      Result := TLapeTree_MethodByName.Create(Tokenizer.TokString, FStackInfo, Self)
    else
      LapeExceptionFmt(lpeUnknownDeclaration, [Tokenizer.TokString], Tokenizer.DocPos);
end;

// (an internal method gets what follows its name)
function TLapeExpressionParser.ParseIdentifier(Ends: EParserTokenSet): TLapeTree_ExprBase;
begin
  Result := ParseName();

  try
    if (not (Result is TLapeTree_Invoke)) then
      Next()
    else if (Next() = tk_sym_ParenthesisOpen) then
    begin
      ParseArguments(TLapeTree_Invoke(Result));
      Next();
    end
    else if (TLapeTree_InternalMethod(Result).SpecialParam <> spNo) then
      ParseSpecialParam(TLapeTree_InternalMethod(Result), Ends);
  except
    Result.Free();
    raise;
  end;
end;

procedure TLapeExpressionParser.ParseMember(var Node: TLapeTree_ExprBase);
var
  DotPos: TDocPos;
begin
  DotPos := getDocPos();
  if (Next() <> tk_Identifier) then
    LapeExceptionFmt(lpeExpected, [LapeTokenToString(tk_Identifier)], Tokenizer.DocPos);

  Node := ValueOf(Node);
  Node := MemberNode(Node, Tokenizer.TokString, Self);
  Node.DocPos := DotPos;
  Next();
end;

// (a[i, j] is a[i][j]. Of a property: a read of it, p[i, j] gives it parameters, p[i] := x is a write: see UseOperands)
procedure TLapeExpressionParser.ParseIndex(var Node: TLapeTree_ExprBase);
var
  Indexable: Boolean;
  Prop: TLapeTree_InvokeProperty;
  Op: TLapeTree_Operator;
begin
  if (Peek() = tk_sym_BracketClose) then
  begin
    Next();
    LapeException(lpeExpectedIndexValue, Tokenizer.DocPos);
  end;

  if IsPropertyNode(Node, Indexable) then
  begin
    Prop := TLapeTree_InvokeProperty.Create(Node, Self);
    Node := Prop;

    if Indexable then
    begin
      ParseArguments(Prop, tk_sym_BracketClose);
      Next();
      Exit;
    end;
  end;

  repeat
    Op := TLapeTree_Operator.Create(op_Index, Self);
    Op.Left := Node;
    Node := Op;
    Op.Right := ParseExpression();
    Node := UseOperands(Op);
  until (Tokenizer.Tok <> tk_sym_Comma);

  Expect(tk_sym_BracketClose, False, True);
end;

// (T(x): Node is the type)
procedure TLapeExpressionParser.ParseCast(var Node: TLapeTree_ExprBase);
var
  Cast: TLapeTree_Cast;
begin
  Expect(tk_sym_ParenthesisOpen, False, True);
  Cast := TLapeTree_Cast.Create(Self);
  Cast.CastTo := TLapeTree_VarType(Node).VarType;
  Node.Free();
  Node := Cast;
  Cast.Param := EnsureExpression(ParseExpression([tk_sym_ParenthesisClose, tk_sym_Comma], False, True));
  Expect(tk_sym_ParenthesisClose, False, True);
end;

procedure TLapeExpressionParser.ParseInvoke(var Node: TLapeTree_ExprBase);
var
  Call: TLapeTree_Invoke;
begin
  if IsPropertyNode(Node) then
    LapeException(lpeCannotInvokeProperty, Tokenizer.DocPos);

  if (Node is TLapeTree_Operator) and (TLapeTree_Operator(Node).OperatorType = op_Addr) then
    Node := ValueOf(Node);

  Node := Node.FoldConstants() as TLapeTree_ExprBase;
  if (Node is TLapeTree_VarType) then
  begin
    ParseCast(Node);
    Exit;
  end;

  if (Node is TLapeTree_MethodByName) then
    Call := TLapeTree_Invoke.Create(Node, Self)
  else
    Call := TLapeTree_InvokePtr.Create(Node, Self);
  Node := Call;

  ParseArguments(Call);
  Next();
end;

// (what follows an operand, binding tighter than any operator)
procedure TLapeExpressionParser.ParseSelectors(var Node: TLapeTree_ExprBase);
var
  Op: TLapeTree_Operator;
begin
  while True do
    case Tokenizer.Tok of
      tk_sym_Dot:             ParseMember(Node);
      tk_sym_BracketOpen:     ParseIndex(Node);
      tk_sym_ParenthesisOpen: ParseInvoke(Node);
      tk_sym_Caret:
        begin
          Op := TLapeTree_Operator.Create(op_Deref, Self);
          Op.Left := Node;
          Node := Op;
          Node := UseOperands(Op);
          Next();
        end;
      else
        Break;
    end;
end;

// The operators in front of an operand (- - x is x, + x is x), a value, and what follows it. Nil: there is none
function TLapeExpressionParser.ParsePrimary(Ends: EParserTokenSet): TLapeTree_ExprBase;
var
  Op: TLapeTree_Operator;
  OpType: EOperator;
  OpPos: TDocPos;
  Negate, OneToken: Boolean;
begin
  Result := nil;

  case Tokenizer.Tok of
    tk_op_Plus:
      begin
        Next();
        Exit(ParsePrimary(Ends));
      end;

    tk_op_Minus, tk_op_NOT, tk_op_Addr:
      begin
        case Tokenizer.Tok of
          tk_op_Minus: OpType := op_UnaryMinus;
          tk_op_NOT:   OpType := op_NOT;
          else         OpType := op_Addr;
        end;

        OpPos := Tokenizer.DocPos;
        Next();

        Negate := True;
        while (OpType = op_UnaryMinus) and (Tokenizer.Tok in [tk_op_Minus, tk_op_Plus]) do
        begin
          if (Tokenizer.Tok = tk_op_Minus) then
            Negate := not Negate;
          Next();
        end;

        if (OpType = op_UnaryMinus) and (not Negate) then
          Exit(ParsePrimary(Ends));

        if (OpType = op_UnaryMinus) and (Tokenizer.Tok in [tk_typ_Float, tk_typ_Integer, tk_typ_Integer_Hex, tk_typ_Integer_Bin]) then
        begin
          Result := ParseNumber(True);
          try
            Next();
            ParseSelectors(Result);
          except
            Result.Free();
            raise;
          end;
          Exit;
        end;

        Result := ParsePrimary(Ends);
        if (Result = nil) then
          LapeException(lpeExpressionExpected, OpPos);

        try
          Op := TLapeTree_Operator.Create(OpType, Self);
          Op.DocPos := OpPos;
          Op.Left := Result;
          Result := Op;
          Result := UseOperands(Op);
        except
          Result.Free();
          raise;
        end;
        Exit;
      end;
  end;

  // (the value: one that ends on its last token still has that as the current token)
  OneToken := True;
  case Tokenizer.Tok of
    tk_typ_Float, tk_typ_Integer, tk_typ_Integer_Hex, tk_typ_Integer_Bin:
      Result := ParseNumber(False);

    tk_typ_String, tk_typ_HereString, tk_typ_Char:
      Result := ParseString();

    tk_sym_BracketOpen:
      Result := ParseOpenArray();

    tk_sym_ParenthesisOpen:
      begin
        Next();
        Result := ParseRightHandSide(ParsePrimary([]), High(Byte), []);
        if (Result = nil) then
          LapeException(lpeExpressionExpected, Tokenizer.DocPos);
        if (Tokenizer.Tok <> tk_sym_ParenthesisClose) then
        begin
          Result.Free();
          LapeException(lpeClosingParenthesisExpected, Tokenizer.DocPos);
        end;
      end;

    tk_kw_Type:
      begin
        Result := TLapeTree_VarType.Create(ParseType(nil), Self);
        OneToken := False;
      end;

    tk_Identifier:
      begin
        Result := ParseIdentifier(Ends);
        OneToken := False;
      end;

    else
      if (Tokenizer.Tok in ParserToken_Operators) then
        LapeException(lpeExpressionExpected, Tokenizer.DocPos)
      else
        Exit;
  end;

  try
    if OneToken then
      Next();
    ParseSelectors(Result);
  except
    Result.Free();
    raise;
  end;
end;

// The operators after Left with what is right of them, of at most MaxPrecedence: a lower one binds tighter. nil Left: nil
function TLapeExpressionParser.ParseRightHandSide(Left: TLapeTree_ExprBase; MaxPrecedence: Byte; Ends: EParserTokenSet): TLapeTree_ExprBase;
var
  OpType: EOperator;
  Op: TLapeTree_Operator;
  OpPos: TDocPos;
  Right: TLapeTree_ExprBase;
begin
  Result := Left;
  if (Result = nil) then
    Exit;

  try
    while (not (Tokenizer.Tok in Ends)) do
    begin
      OpType := ParserTokenToOperator(Tokenizer.Tok);
      if (OpType in [op_Unknown, op_Dot, op_Index] + UnaryOperators) or (OperatorPrecedence[OpType] > MaxPrecedence) then
        Break;

      OpPos := Tokenizer.DocPos;
      Op := TLapeTree_Operator.Create(OpType, Self);
      Op.Left := Result;
      Result := Op;
      Next();

      // (a - -b is a + b)
      if (OpType in [op_Plus, op_Minus]) then
        while (Tokenizer.Tok in [tk_op_Minus, tk_op_Plus]) do
        begin
          if (Tokenizer.Tok = tk_op_Minus) then
            if (Op.OperatorType = op_Plus) then
              Op.OperatorType := op_Minus
            else
              Op.OperatorType := op_Plus;
          Next();
        end;

      Right := ParsePrimary(Ends);
      if (Right = nil) then
        LapeException(lpeExpressionExpected, OpPos);

      if (OperatorAssociative[OpType] = assocRight) then
        Op.Right := ParseRightHandSide(Right, OperatorPrecedence[OpType], Ends)
      else
        Op.Right := ParseRightHandSide(Right, OperatorPrecedence[OpType] - 1, Ends);

      Result := UseOperands(Op);
    end;
  except
    Result.Free();
    raise;
  end;
end;

function TLapeExpressionParser.Parse(Ends: EParserTokenSet; FirstNext, DoFold: Boolean): TLapeTree_ExprBase;
var
  Lbl: TLapeTree_Invoke;
begin
  if FirstNext then
    Next();
  if (Tokenizer.Tok in Ends) then
    Exit(nil);

  Result := ParseRightHandSide(ParsePrimary(Ends), High(Byte), Ends);
  if (Result = nil) then
    LapeException(lpeExpressionExpected, Tokenizer.DocPos);

  try
    Result := ValueOf(Result);

    // (Name: a label)
    if (Tokenizer.Tok = tk_sym_Colon) and (not (tk_sym_Colon in Ends)) then
    begin
      if (Result is TLapeTree_Operator) and (not (TLapeTree_Operator(Result).OperatorType in [op_Index, op_Deref])) then
        LapeException(lpeInvalidLabel, Tokenizer.DocPos);

      Lbl := TLapeTree_InternalMethod_Label.Create(Self);
      Lbl.addParam(Result);
      Result := Lbl;
      Next();
    end
    else if (not (Tokenizer.Tok in Ends)) then
      if (Tokenizer.Tok = tk_sym_ParenthesisClose) then
        LapeException(lpeLostClosingParenthesis, Tokenizer.DocPos)
      else if (Tokenizer.Tok in [tk_Identifier, tk_kw_Type, tk_typ_Float, tk_typ_Integer, tk_typ_Integer_Hex,
                                 tk_typ_Integer_Bin, tk_typ_String, tk_typ_HereString, tk_typ_Char]) then
        LapeException(lpeOperatorExpected, Tokenizer.DocPos);
  except
    Result.Free();
    raise;
  end;

  if DoFold and (Result <> nil) then
    Result := TLapeTree_ExprBase(Result.FoldConstants());
end;

function TLapeExpressionParser.ParseRange(Ends: EParserTokenSet; FirstNext: Boolean): TLapeTree_Base;
var
  ExprLo: TLapeTree_ExprBase;
  TypeHi: TLapeType;
begin
  Result := nil;
  TypeHi := nil;

  ExprLo := ParseExpression(Ends, FirstNext);
  if (Tokenizer.Tok <> tk_sym_DotDot) then
    Result := ExprLo
  else
  try
    Result := TLapeTree_Range.Create(Self);
    TLapeTree_Range(Result).Lo := ExprLo;
    TLapeTree_Range(Result).Hi := ParseExpression(Ends);

    if (ExprLo <> nil) and (TLapeTree_Range(Result).Hi <> nil) then
      TypeHi := TLapeTree_Range(Result).Hi.resType();
    if (TypeHi = nil) or (not TypeHi.CompatibleWith(ExprLo.resType())) then
      LapeException(lpeInvalidRange, Tokenizer.DocPos);
  except
    if (Result <> nil) then
      Result.Free();
    raise;
  end;
end;

end.
