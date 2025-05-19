{
  Author: Niels A.D
  Project: Lape (https://github.com/nielsAD/lape)
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
    FStackTrace: lpString;
    FHint: lpString;
  public
    property DocPos: TDocPos read FDocPos;
    property Error: lpString read FError;
    property Hint: lpString read FHint;
    property StackTrace: lpString read FStackTrace;

    constructor Create(AMessage: lpString; ADocPos: TDocPos; AStackTrace: lpString = ''; AHint: lpString = '');
  end;

const
  lpeCannotEvalPreprocessorFunc = 'Cannot evalulate preprocessor function "%s"';
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
  lpeCannotFallthrough = 'Cannot use fallthrough in this context';
  lpeCannotInvoke = 'Cannot invoke identifier';
  lpeCannotInvokeProperty = 'Cannot invoke property like this';
  lpeCannotMixStaticOverload = 'Cannot mix static with non-static method overload';
  lpeCannotOverload = 'Cannot overload function';
  lpeCannotOverload2 = 'Cannot overload "%s" with a "%s"';
  lpeCannotOverloadOperator = 'Cannot overload operator "%s"';
  lpeCannotOverrideOperator = 'Cannot override operator "%s" with types "%s" and "%s"';
  lpeClosingParenthesisExpected = 'Closing parenthesis expected';
  lpeClosingBracketExpected = 'Closing bracket expected';
  lpeConditionalNotClosed = 'Conditional statement not properly closed';
  lpeConstantExpected = 'Constant expression expected';
  lpeDeclarationOutOfScope = 'Declaration "%s" out of scope';
  lpeDefaultParamInProperties = 'Properties do not support default parameters';
  lpeDefaultToMoreThanOne = 'Runtime default value can only be assigned to one variable';
  lpeDuplicateDeclaration = 'Duplicate declaration "%s"';
  lpeDuplicateHashBucket = 'Duplicate hash bucket with "%s" and "%s"';
  lpeErrorScanningString = '%s while scanning string literal';
  lpeExceptionAt = '%s at line %d, column %d';
  lpeExceptionIn = '%s in "%s"';
  lpeExceptionInFile = '%s in file "%s"';
  lpeExpected = '%s expected';
  lpeExpectedAssignOperator = 'Expected assign operator';
  lpeExpectedArray = 'Array expected';
  lpeExpectedArrayOfType = 'Array expected of type "%s"';
  lpeExpectedBoolExpression = 'Boolean expression expected';
  lpeExpectedDynamicArray = 'Dynamic array expected';
  lpeExpectedNormalMethod = 'Normal method expected';
  lpeExpectedPointerType = 'Pointer type expected';
  lpeExpectedIndexValue = 'Expected at least one index value';
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
  lpeInvalidConstSet = 'Invalid constant set';
  lpeInvalidConstByteSet = 'Invalid constant (Byte) set';
  lpeInvalidConstCharSet = 'Invalid constant (Char) set';
  lpeInvalidConstructMethod = 'Invalid construct method';
  lpeInvalidConstructMethod1 = 'Invalid construct method: "%s"';
  lpeInvalidCompareMethod = 'Invalid compare method';
  lpeInvalidEvaluation = 'Invalid evaluation';
  lpeInvalidForward = 'Forwarded declaration "%s" not resolved';
  lpeInvalidLabel = 'Invalid label';
  lpeInvalidIndex = 'Invalid index "%s"';
  lpeInvalidIterator = 'Variable cannot be used for iteration';
  lpeInvalidJump = 'Invalid jump';
  lpeInvalidOpenArrayElement = 'Invalid open array element (%s) (index: %d)';
  lpeInvalidOperator = 'Operator "%s" expects %d parameters';
  lpeInvalidDictionaryOperation = 'Invalid operation (%s) on dictionary';
  lpeInvalidRange = 'Expression is not a valid range';
  lpeInvalidUnionType = 'Invalid union type';
  lpeInvalidValueForType = 'Invalid value for type "%s"';
  lpeInvalidWithReference = 'Invalid with-reference';
  lpeInvalidSet = 'Invalid set';
  lpeLostClosingParenthesis = 'Found closing parenthesis without matching opening parenthesis';
  lpeLostConditional = 'Found conditional without matching opening statement';
  lpeMethodOfObjectExpected = 'Expected method of object';
  lpeNoDefaultForParam = 'No default value for parameter %d found';
  lpeNoForwardMatch = 'Forwarded declaration doesn''t match';
  lpeNoMatchingProperty = 'No matching property found';
  lpeNoProperty = 'Don''t know which property to call with params (%s)';
  lpeNoOverloadedMethod = 'Don''t know which overloaded method to call with params (%s)';
  lpeNeedMoreBuckets = 'Need more hash buckets (currently %d)';
  lpeOperatorExpected = 'Operator expected';
  lpeOperatorRequiredForType = 'Operator "%s" is required for type "%s"';
  lpeOutOfStackRange = 'Out of stack range';
  lpeOutOfTypeRange  = 'Out of type range';
  lpeOutOfTypeRange1 = 'Out of type range (value:%d, low:%d, high:%d)';
  lpeOutOfTypeRangeLow  = 'Out of type range (value:%d, low:%d)';
  lpeOutOfTypeRangeHigh = 'Out of type range (value:%d, high:%d)';
  lpeOutsideExceptBlock = 'Can only be used in an except block';
  lpeIndexOutOfRange = 'Index out of range (index:%d, low:%d, high:%d)';
  lpeIndexOutOfRangeLow  = 'Index out of range (index:%d, low:%d)';
  lpeIndexOutOfRangeHigh = 'Index out of range (index:%d, high:%d)';
  lpeParentOutOfScope = 'Parent declaration is out of scope';
  lpeExpectedProperty = 'Property has to be of a type';
  lpeRuntime = 'Runtime error: "%s"';
  lpeStatementNotAllowed = 'Statement not allowed here';
  lpeStaticMethodExpected = 'Variable expected. "%s" is not a static method';
  lpeTooMuchParameters = 'Too many parameters found';
  lpeTypeExpected = 'Type expected';
  lpeUnClosedComment = 'Unclosed comment';
  lpeUnExpectedToken = 'Found unexpected token "%s"';
  lpeUnknownDeclaration = 'Unknown declaration "%s"';
  lpeUnknownDeclarationOpenArray = 'Unknown declaration in open array (index: %d)';
  lpeUnknownDirective = 'Unknown compiler directive';
  lpeUnknownOC = 'Unknown opcode';
  lpeUnknownParent = 'Cannot find parent declaration';
  lpeUserDefinedError = 'User defined error: "%s"';
  lpeVariableExpected = 'Variable expected';
  lpeVariableOfTypeExpected = 'Expected variable of type "%s", got "%s"';
  lpeWrongNumberParams = 'Wrong number of parameters found, expected %d';
  lpeScriptMethodExpected = 'Script method expected';
  lpeMethodDeclarationParenthesesExpected = 'Missing parentheses in function declaration';

  lphVariableNotUsed = 'Variable "%s" not used';
  lphParameterNotUsed = 'Parameter "%s" not used';
  lphParamterNotSet = 'Parameter "%s" not set';
  lphResultNotSet = 'Result not set';
  lphDeprecatedMethod = 'Method "%s" is deprecated';
  lphDeprecatedMethodHint = 'Method "%s" is deprecated "%s"';
  lphUnImplementedMethod = 'Method "%s" is not implemented';
  lphExperimentalMethod = 'Method "%s" is experimental';
  lphDuplicateLocalName = 'Local declaration "%s" exists globally';
  lphUserDefined = 'User defined hint: "%s"';
  lphExpectedParameters = 'Expected parameters for "%s":' + LineEnding + '> %s';
  lphAvailableMethods = 'Available "%s" methods:' + LineEnding;

procedure LapeException(Msg: lpString; Hint: String = ''); overload;
procedure LapeException(Msg: lpString; DocPos: TDocPos; Hint: String = ''); overload;
procedure LapeException(Msg: lpString; DocPos: array of TLapeBaseDeclClass; Hint: String = ''); overload;
procedure LapeExceptionFmt(Msg: lpString; Args: array of const; Hint: String = ''); overload;
procedure LapeExceptionFmt(Msg: lpString; Args: array of const; DocPos: TDocPos; Hint: String = ''); overload;
procedure LapeExceptionFmt(Msg: lpString; Args: array of const; DocPos: array of TLapeBaseDeclClass; Hint: String = ''); overload;

procedure LapeExceptionRuntime(e: lpException; StackTrace: lpString); overload;
procedure LapeExceptionRuntime(e: Exception; StackTrace: lpString; DocPos: TDocPos); overload;

// reraise exception with a different docpos
procedure LapeExceptionReraise(e: lpException; DocPos: TDocPos); overload;
procedure LapeExceptionReraise(e: lpException; DocPos: array of TLapeBaseDeclClass); overload;

function FormatLocation(Msg: lpString; DocPos: TDocPos): lpString;

implementation

constructor lpException.Create(AMessage: lpString; ADocPos: TDocPos; AStackTrace: lpString; AHint: lpString);
begin
  if (ADocPos.Col <> NullDocPos.Col) and (ADocPos.Line <> NullDocPos.Line) then
    inherited Create(string(FormatLocation(AMessage, ADocPos)))
  else
    inherited Create(string(AMessage));

  FDocPos := ADocPos;
  FError := AMessage;
  FStackTrace := AStackTrace;
  FHint := AHint;
end;

procedure LapeExceptionRuntime(e: lpException; StackTrace: lpString);
begin
  raise lpException.Create(Format(lpeRuntime, [e.Error]), e.DocPos, StackTrace);
end;

procedure LapeExceptionRuntime(e: Exception; StackTrace: lpString; DocPos: TDocPos);
begin
  raise lpException.Create(Format(lpeRuntime, [e.Message]), DocPos, StackTrace);
end;

procedure LapeExceptionReraise(e: lpException; DocPos: TDocPos);
begin
  raise lpException.Create(e.Error, DocPos, e.StackTrace, e.Hint);
end;

procedure LapeExceptionReraise(e: lpException; DocPos: array of TLapeBaseDeclClass);
var
  i: Integer;
  NewDocPos: TDocPos;
begin
  NewDocPos := NullDocPos;
  for i := 0 to High(DocPos) do
    if (DocPos[i] <> nil) and
       (DocPos[i].DocPos.Col <> NullDocPos.Col) and
       (DocPos[i].DocPos.Line <> NullDocPos.Line) then
    begin
      NewDocPos := DocPos[i].DocPos;
      Break;
    end;

  raise lpException.Create(e.Error, NewDocPos, e.StackTrace, e.Hint);
end;

function FormatLocation(Msg: lpString; DocPos: TDocPos): lpString;
begin
  Result := Msg;
  if (DocPos.Line > 0) and (DocPos.Col > 0) then
    Result := Format(lpString(lpeExceptionAt), [Result, DocPos.Line, DocPos.Col]);
  if (DocPos.FileName <> '') then
    Result := Format(lpString(lpeExceptionInFile), [Result, DocPos.FileName]);
end;

{$IF DEFINED(Delphi) AND (CompilerVersion <= 21.00)}
function ReturnAddress: Pointer;
asm
  MOV  EAX, [EBP+4]
end;
{$IFEND}

procedure _LapeException(Msg: lpString; DocPos: TDocPos; Hint: String = ''); inline;
begin
  raise lpException.Create(Msg, DocPos, '', Hint) at {$IFDEF FPC}get_caller_addr(get_frame){$ELSE}ReturnAddress{$ENDIF};
end;

procedure LapeException(Msg: lpString; Hint: String);
begin
  _LapeException(Msg, NullDocPos, Hint);
end;

procedure LapeException(Msg: lpString; DocPos: TDocPos; Hint: String);
begin
  _LapeException(Msg, DocPos, Hint);
end;

procedure LapeException(Msg: lpString; DocPos: array of TLapeBaseDeclClass; Hint: String);
var
  i: Integer;
begin
  for i := 0 to High(DocPos) do
    if (DocPos[i] <> nil) and
       (DocPos[i].DocPos.Col <> NullDocPos.Col) and
       (DocPos[i].DocPos.Line <> NullDocPos.Line)
    then
    begin
      _LapeException(Msg, DocPos[i].DocPos, Hint);
      Exit;
    end;
  _LapeException(Msg, NullDocPos, Hint);
end;

procedure LapeExceptionFmt(Msg: lpString; Args: array of const; Hint: String);
begin
  _LapeException(Format(Msg, Args), NullDocPos);
end;

procedure LapeExceptionFmt(Msg: lpString; Args: array of const; DocPos: TDocPos; Hint: String);
begin
  _LapeException(Format(Msg, Args), DocPos, Hint);
end;

procedure LapeExceptionFmt(Msg: lpString; Args: array of const; DocPos: array of TLapeBaseDeclClass; Hint: String);
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
      _LapeException(Msg, DocPos[i].DocPos, Hint);
      Exit;
    end;
  _LapeException(Msg, NullDocPos, Hint);
end;

end.
