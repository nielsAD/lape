{
  Author: Niels A.D
  Project: Lape (http://code.google.com/p/la-pe/)
  License: GNU Lesser GPL (http://www.gnu.org/licenses/lgpl.html)

  Lape exceptions.
}
unit lpmessages;

{$I lape.inc}

interface

uses
  SysUtils,
  lptypes;

type
  lpException = class(Exception)
  protected
    FDocPos: TDocPos;
    FError: lpString;
  public
    property DocPos: TDocPos read FDocPos;
    property Error: lpString read FError;

    function hasDocPos: Boolean;

    constructor Create(AMessage: lpString; ADocPos: TDocPos);
  end;

const
  lpeArrayLengthsDontMatch = 'Length of arrays (%s) don''t match';
  lpeAssertionFailure = 'Assertion failure';
  lpeAssertionFailureMsg = 'Assertion failure: %s';
  lpeBlockExpected = 'Block expected';
  lpeCannotAssign = 'Target cannot be assigned to';
  lpeCannotBreak = 'Cannot break out of this statement';
  lpeCannotContinue = 'Cannot use continue in this context';
  lpeCannotEvalConst = 'Cannot be evaluated as constant';
  lpeCannotEvalConstProc = 'Procedures cannot be used for constant evaluation';
  lpeCannotEvalRunTime = 'Cannot be evaluated at runtime';
  lpeCannotInvoke = 'Cannot invoke identifier';
  lpeCannotMixStaticOverload = 'Cannot mix static with non-static method overload';
  lpeCannotOverload = 'Cannot overload function';
  lpeCannotOverloadOperator = 'Cannot overload operator "%s"';
  lpeCannotOverrideOperator = 'Cannot override operator "%s" with types "%s" and "%s"';
  lpeClosingParenthesisExpected = 'Closing parenthesis expected';
  lpeConditionalNotClosed = 'Conditional statement not properly closed';
  lpeConstantExpected = 'Constant expression expected';
  lpeDeclarationOutOfScope = 'Declaration "%s" out of scope';
  lpeDefaultToMoreThanOne = 'Runtime default value can only be assigned to one variable';
  lpeDuplicateDeclaration = 'Duplicate declaration "%s"';
  lpeErrorScanningString = '%s while scanning string literal';
  lpeExceptionAt = '%s at line %d, column %d';
  lpeExceptionIn = '%s in file "%s"';
  lpeExpected = '%s expected';
  lpeExpectedNormalMethod = 'Normal method expected';
  lpeExpectedOther = 'Found unexpected token "%s", expected "%s"';
  lpeExpressionExpected = 'Expression expected';
  lpeExpectedEnum = 'Enum expected';
  lpeFileNotFound = 'File "%s" not found';
  lpeImpossible = 'It''s impossible!';
  lpeIncompatibleAssignment = 'Can''t assign "%s" to "%s"';
  lpeIncompatibleOperator = 'Operator "%s" is not supported for type';
  lpeIncompatibleOperator1 = 'Operator "%s" not compatible with "%s"';
  lpeIncompatibleOperator2 = 'Operator "%s" not compatible with types "%s" and "%s"';
  lpeInvalidAssignment = 'Invalid assignment';
  lpeInvalidCast = 'Invalid cast';
  lpeInvalidCaseStatement = 'Invalid case statement';
  lpeInvalidCondition = 'Invalid condition';
  lpeInvalidEvaluation = 'Invalid evaluation';
  lpeInvalidForward = 'Forwarded declaration "%s" not resolved';
  lpeInvalidLabel = 'Invalid label';
  lpeInvalidIndex = 'Invalid index "%s"';
  lpeInvalidIterator = 'Variable cannot be used for iteration';
  lpeInvalidJump = 'Invalid jump';
  lpeInvalidOpenArrayElement = 'Invalid open array element (%s) (index: %d)';
  lpeInvalidOperator = 'Operator "%s" expects %d parameters';
  lpeInvalidRange = 'Expression is not a valid range';
  lpeInvalidUnionType = 'Invalid union type';
  lpeInvalidValueForType = 'Invalid value for type "%s"';
  lpeInvalidWithReference = 'Invalid with-reference';
  lpeLostClosingParenthesis = 'Found closing parenthesis without matching opening parenthesis';
  lpeLostConditional = 'Found conditional without matching opening statement';
  lpeMethodOfObjectExpected = 'Expected method of object';
  lpeNoDefaultForParam = 'No default value for parameter %d found';
  lpeNoForwardMatch = 'Forwarded declaration doesn''t match';
  lpeNoOverloadedMethod = 'Don''t know which overloaded method to call with params (%s)';
  lpeOperatorExpected = 'Operator expected';
  lpeOutOfStackRange = 'Out of stack range';
  lpeOutOfTypeRange  = 'Out of type range';
  lpeOutOfTypeRange1 = 'Out of type range (value:%d, low:%d, high:%d)';
  lpeOutOfTypeRangeLow  = 'Out of type range (value:%d, low:%d)';
  lpeOutOfTypeRangeHigh = 'Out of type range (value:%d, high:%d)';
  lpeOutsideExceptionBlock = 'Can only be used in an except/finally block';
  lpeOutsideExceptBlock = 'Can only be used in an except block';
  lpeIndexOutOfRange = 'Index out of range (index:%d, low:%d, high:%d)';
  lpeIndexOutOfRangeLow  = 'Index out of range (index:%d, low:%d)';
  lpeIndexOutOfRangeHigh = 'Index out of range (index:%d, high:%d)';
  lpeParentOutOfScope = 'Parent declaration is out of scope';
  lpeRuntime = 'Runtime error: "%s"';
  lpeStatementNotAllowed = 'Statement not allowed here';
  lpeTooMuchParameters = 'Too many parameters found';
  lpeTypeExpected = 'Type expected';
  lpeUnexpectedToken = 'Found unexpected token "%s"';
  lpeUnknownDeclaration = 'Unknown declaration "%s"';
  lpeUnknownDeclarationOpenArray = 'Unknown declaration in open array (index: %d)';
  lpeUnknownDirective = 'Unknown compiler directive';
  lpeUnknownOC = 'Unknown opcode';
  lpeUnknownParent = 'Cannot find parent declaration';
  lpeVariableExpected = 'Variable expected';
  lpeVariableOfTypeExpected = 'Expected variable of type "%s", got "%s"';
  lpeWrongNumberParams = 'Wrong number of parameters found, expected %d';
  lpeScriptMethodExpected = 'Script method expected';

  lphVariableNotUsed = 'Variable "%s" not used';
  lphParameterNotUsed = 'Parameter "%s" not used';
  lphParamterNotSet = 'Parameter "%s" not set';
  lphResultNotSet = 'Result not set';
  lphDeprecatedMethod = 'Method "%s" is deprecated';
  lphDeprecatedMethodHint = 'Method "%s" is deprecated "%s"';
  lphUnImplementedMethod = 'Method "%s" is not implemented';
  lphExperimentalMethod = 'Method "%s" is experimental';

procedure LapeException(Msg: lpString); overload;
procedure LapeException(Msg: lpString; DocPos: TDocPos); overload;
procedure LapeException(Msg: lpString; DocPos: array of TLapeBaseDeclClass); overload;
procedure LapeExceptionFmt(Msg: lpString; Args: array of const); overload;
procedure LapeExceptionFmt(Msg: lpString; Args: array of const; DocPos: TDocPos); overload;
procedure LapeExceptionFmt(Msg: lpString; Args: array of const; DocPos: array of TLapeBaseDeclClass); overload;

function FormatLocation(Msg: lpString; DocPos: TDocPos): lpString;

implementation

{$IFDEF Lape_NeedAnsiStringsUnit}
uses
  AnsiStrings;
{$ENDIF}

function lpException.hasDocPos: Boolean;
begin
  Result := (DocPos.Col <> NullDocPos.Col) and (DocPos.Line <> NullDocPos.Line);
end;

constructor lpException.Create(AMessage: lpString; ADocPos: TDocPos);
begin
  if (ADocPos.Col <> NullDocPos.Col) and (ADocPos.Line <> NullDocPos.Line) then
    inherited Create(string(FormatLocation(AMessage, ADocPos)))
  else
    inherited Create(string(AMessage));

  FDocPos := ADocPos;
  FError := AMessage;
end;

function FormatLocation(Msg: lpString; DocPos: TDocPos): lpString;
begin
  Result := Msg;
  if (DocPos.Line > 0) and (DocPos.Col > 0) then
    Result := Format(lpString(lpeExceptionAt), [Result, DocPos.Line, DocPos.Col]);
  if (DocPos.FileName <> '') then
    Result := Format(lpString(lpeExceptionIn), [Result, DocPos.FileName]);
end;

{$IF DEFINED(Delphi) AND (CompilerVersion <= 21.00)}
function ReturnAddress: Pointer;
asm
  MOV  EAX, [EBP+4]
end;
{$IFEND}

procedure _LapeException(Msg: lpString; DocPos: TDocPos); inline;
{$IFDEF FPC}
begin
  raise lpException.Create(Msg, DocPos) at get_caller_addr(get_frame);
end;
{$ELSE}
begin
  raise lpException.Create(Msg, DocPos) at ReturnAddress;
end;
{$ENDIF}

procedure LapeException(Msg: lpString);
begin
  _LapeException(Msg, NullDocPos);
end;

procedure LapeException(Msg: lpString; DocPos: TDocPos);
begin
  _LapeException(Msg, DocPos);
end;

procedure LapeException(Msg: lpString; DocPos: array of TLapeBaseDeclClass);
var
  i: Integer;
begin
  for i := 0 to High(DocPos) do
    if (DocPos[i] <> nil) and
       (DocPos[i].DocPos.Col <> NullDocPos.Col) and
       (DocPos[i].DocPos.Line <> NullDocPos.Line)
    then
    begin
      _LapeException(Msg, DocPos[i].DocPos);
      Exit;
    end;
  _LapeException(Msg, NullDocPos);
end;

procedure LapeExceptionFmt(Msg: lpString; Args: array of const);
begin
  _LapeException(Format(Msg, Args), NullDocPos);
end;

procedure LapeExceptionFmt(Msg: lpString; Args: array of const; DocPos: TDocPos);
begin
  _LapeException(Format(Msg, Args), DocPos);
end;

procedure LapeExceptionFmt(Msg: lpString; Args: array of const; DocPos: array of TLapeBaseDeclClass);
var
  i: Integer;
begin
  Msg := Format(Msg, Args);
  for i := 0 to High(DocPos) do
    if (DocPos[i] <> nil) and
       (DocPos[i].DocPos.Col <> NullDocPos.Col) and
       (DocPos[i].DocPos.Line <> NullDocPos.Line)
    then
    begin
      _LapeException(Msg, DocPos[i].DocPos);
      Exit;
    end;
  _LapeException(Msg, NullDocPos);
end;

end.


