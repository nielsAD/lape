{
  Author: Niels A.D
  Project: Lape (https://github.com/nielsAD/lape)
  License: GNU Lesser GPL (http://www.gnu.org/licenses/lgpl.html)

  This include constains all the wrappers for file related functions.
}
{%MainUnit lpcompiler.pas}

addGlobalVar(DirectorySeparator, 'DirectorySeparator').isConstant := True;
addGlobalVar(LineEnding, 'LineEnding').isConstant := True;

addGlobalFunc('function FindFiles(const Directory: String; Recursive: Boolean = False): array of String; overload;', @_LapeFindAllFiles);
addGlobalFunc('function FindFiles(const Directory: String; Extensions: array of String; Recursive: Boolean = False): array of String; overload;', @_LapeFindFiles);
addGlobalFunc('function FindDirectories(const Directory: String; Recursive: Boolean = False): array of String;', @_LapeFindDirectories);
addGlobalFunc('function AppendFileContents(const FileName: String; const Text: String): Boolean;', @_LapeAppendFileContents);
addGlobalFunc('function WriteFileContents(const FileName: String; const Text: String): Boolean;', @_LapeWriteFileContents);
addGlobalFunc('function ReadFileContents(const FileName: String): String;', @_LapeReadFileContents);
addGlobalFunc('function CreateFile(const FileName: String): Boolean;', @_LapeCreateFile);
addGlobalFunc('function CreateDirectory(const Directory: String): Boolean;', @_LapeCreateDirectory);
addGlobalFunc('function ForceDirectories(const Directory: String): Boolean;', @_LapeForceDirectories);
addGlobalFunc('function DeleteDirectory(const Directory: String; OnlyChildren: Boolean): Boolean;', @_LapeDeleteDirectory);
addGlobalFunc('function DeleteFile(const FileName: String): Boolean;', @_LapeDeleteFile);
addGlobalFunc('function RenameFile(const OldFileName, NewFileName: String): Boolean;', @_LapeRenameFile);
addGlobalFunc('function CopyFile(const SourceFileName, DestFileName: String; Overwrite: Boolean = False): Boolean;', @_LapeCopyFile);
addGlobalFunc('function FileExists(const FileName: String): Boolean;', @_LapeFileExists);
addGlobalFunc('function DirectoryExists(const Directory: String): Boolean;', @_LapeDirectoryExists);
addGlobalFunc('function FileSize(const FileName: String): Int64;', @_LapeFileSize);
addGlobalFunc('function FileAge(const FileName: String): Int32; overload;', @_LapeFileAge);
addGlobalFunc('function FileAge(const FileName: String; out FileDateTime: TDateTime): Boolean; overload;', @_LapeFileAgeDateTime);
addGlobalFunc('function ExtractFilePath(const FileName: String): String;', @_LapeExtractFilePath);
addGlobalFunc('function ExtractFileDrive(const FileName: String): String;', @_LapeExtractFileDrive);
addGlobalFunc('function ExtractFileName(const FileName: String): String;', @_LapeExtractFileName);
addGlobalFunc('function ExtractFileExt(const FileName: String): String;', @_LapeExtractFileExt);
addGlobalFunc('function ExtractFileDir(const FileName: String): String;', @_LapeExtractFileDir);
addGlobalFunc('function ExpandFileName(const FileName: String): String;', @_LapeExpandFileName);
addGlobalFunc('function ExtractRelativePath(const BaseName, DestName: String): String;', @_LapeExtractRelativePath);
addGlobalFunc('function IncludeTrailingPathDelimiter(const Path: String): String;', @_LapeIncludeTrailingPathDelimiter);
addGlobalFunc('function ExcludeTrailingPathDelimiter(const Path: String): String;', @_LapeExcludeTrailingPathDelimiter);
addGlobalFunc('function IncludeTrailingBackslash(const Path: String): String;', @_LapeIncludeTrailingBackslash);
addGlobalFunc('function ExcludeTrailingBackslash(const Path: String): String;', @_LapeExcludeTrailingBackslash);
addGlobalFunc('function IncludeLeadingPathDelimiter(const Path: String): String;', @_LapeIncludeLeadingPathDelimiter);
addGlobalFunc('function ExcludeLeadingPathDelimiter(const Path: String): String;', @_LapeExcludeLeadingPathDelimiter);
addGlobalFunc('function ChangeFileExtension(FileName, Extension: String): String;', @_LapeChangeFileExtension);
addGlobalFunc('function SetPathSeperators(Path: String): String;', @_LapeSetPathSeperators);
addGlobalFunc('function JoinPaths(const Paths: array of String): String;', @_LapeJoinPaths);

