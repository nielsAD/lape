{
  Author: Niels A.D
  Project: Lape (http://code.google.com/p/la-pe/)
  License: GNU Lesser GPL (http://www.gnu.org/licenses/lgpl.html)

  This include constains all the wrappers for file related functions.
}
{%MainUnit lpcompiler.pas}

addGlobalVar(DirectorySeparator, 'DirectorySeparator').isConstant := True;

addGlobalFunc('function ExtractFilePath(const FileName: String): String;', @_LapeExtractFilePath);
addGlobalFunc('function ExtractFileDrive(const FileName: String): String;', @_LapeExtractFileDrive);
addGlobalFunc('function ExtractFileName(const FileName: String): String;', @_LapeExtractFileName);
addGlobalFunc('function ExtractFileExt(const FileName: String): String;', @_LapeExtractFileExt);
addGlobalFunc('function ExtractFileDir(const FileName: String): String;', @_LapeExtractFileDir);
addGlobalFunc('function ExpandFileName(const FileName: String): String;', @_LapeExpandFileName);
addGlobalFunc('function ExtractRelativepath(const BaseName, DestName: String): String;', @_LapeExtractRelativePath);
addGlobalFunc('function IncludeTrailingPathDelimiter(const Path: String) : String;', @_LapeIncludeTrailingPathDelimiter);
addGlobalFunc('function ExcludeTrailingPathDelimiter(const Path: String): String;', @_LapeExcludeTrailingPathDelimiter);
addGlobalFunc('function IncludeTrailingBackslash(const Path: String) : String;', @_LapeIncludeTrailingBackslash);
addGlobalFunc('function ExcludeTrailingBackslash(const Path: String): String;', @_LapeExcludeTrailingBackslash);
addGlobalFunc('function IncludeLeadingPathDelimiter(const Path : String) : String;', @_LapeIncludeLeadingPathDelimiter);
addGlobalFunc('function ExcludeLeadingPathDelimiter(const Path: String): String;', @_LapeExcludeLeadingPathDelimiter);
