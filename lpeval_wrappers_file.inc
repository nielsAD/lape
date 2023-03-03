{
  Author: Niels A.D
  Project: Lape (https://github.com/nielsAD/lape)
  License: GNU Lesser GPL (http://www.gnu.org/licenses/lgpl.html)

  This include constains all the wrappers for file related functions.
}
{%MainUnit lpeval.pas}

procedure _LapeFindAllFiles(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}

  procedure Find(Directory: lpString; Recursive: Boolean; var Files: TStringArray);
  var
    List: TStringList;
    Info: TSearchRec;
    FileName: lpString;
    Len, i: Integer;
  begin
    List := TStringList.Create();
    Directory := IncludeTrailingPathDelimiter(ExpandFileName(Directory));

    if (FindFirst(Directory + '*', faAnyFile, Info) = 0) then
    begin
      repeat
        if (Info.Name = '.') or (Info.Name = '..') then
          Continue;

        FileName := Directory + Info.Name;
        if (Info.Attr and faDirectory <> 0) then
        begin
          if Recursive then
            Find(IncludeTrailingPathDelimiter(FileName), Recursive, Files);

          Continue;
        end;

        List.Add(FileName);
      until (FindNext(Info) <> 0);

      SysUtils.FindClose(Info);
    end;

    Len := Length(Files);
    SetLength(Files, Len + List.Count);
    for i := 0 to List.Count - 1 do
      Files[Len + i] := List[i];

    List.Free();
  end;

begin
  Find(PlpString(Params^[0])^, PBoolean(Params^[1])^, PStringArray(Result)^);
end;

procedure _LapeFindFiles(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}

  procedure Find(Directory: lpString; Extensions: TStringArray; Recursive: Boolean; var Files: TStringArray);
  var
    List: TStringList;
    Info: TSearchRec;
    FileName, FileExtension: lpString;
    Len, i: Integer;
  begin
    List := TStringList.Create();
    Directory := IncludeTrailingPathDelimiter(ExpandFileName(Directory));

    if (FindFirst(Directory + '*', faAnyFile, Info) = 0) then
    begin
      repeat
        if (Info.Name = '.') or (Info.Name = '..') then
          Continue;

        FileName := Directory + Info.Name;
        FileExtension := ExtractFileExt(Info.Name);

        if (Info.Attr and faDirectory <> 0) then
        begin
          if Recursive then
            Find(FileName, Extensions, Recursive, Files);

          Continue;
        end;

        for i := 0 to High(Extensions) do
          if (Extensions[i] = '*') or (Extensions[i] = FileExtension) or ('.' + Extensions[i] = FileExtension) then
          begin
            List.Add(FileName);

            Break;
          end;
      until (FindNext(Info) <> 0);

      SysUtils.FindClose(Info);
    end;

    Len := Length(Files);
    SetLength(Files, Len + List.Count);
    for i := 0 to List.Count - 1 do
      Files[Len + i] := List[i];

    List.Free();
  end;

begin
  Find(PlpString(Params^[0])^, PStringArray(Params^[1])^, PBoolean(Params^[2])^, PStringArray(Result)^);
end;

procedure _LapeFindDirectories(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}

  procedure Find(Directory: String; Recursive: Boolean; var Directories: TStringArray);
  var
    List: TStringList;
    Info: TSearchRec;
    FileName: lpString;
    Len, i: Integer;
  begin
    List := TStringList.Create();
    Directory := IncludeTrailingPathDelimiter(ExpandFileName(Directory));

    if (FindFirst(Directory + '*', faAnyFile, Info) = 0) then
    begin
      repeat
        if (Info.Name = '.') or (Info.Name = '..') then
          Continue;

        FileName := Directory + Info.Name;

        if (Info.Attr and faDirectory <> 0) then
        begin
          List.Add(FileName);

          if Recursive then
            Find(FileName, Recursive, Directories);
        end;
      until (FindNext(Info) <> 0);

      SysUtils.FindClose(Info);
    end;

    Len := Length(Directories);
    SetLength(Directories, Len + List.Count);
    for i := 0 to List.Count - 1 do
      Directories[Len + i] := List[i];

    List.Free();
  end;

begin
  Find(PlpString(Params^[0])^, PBoolean(Params^[1])^, PStringArray(Result)^);
end;

procedure _LapeAppendFileContents(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}

  function AppendFileContents(const FileName: lpString; const Text: lpString): Boolean;
  var
    Stream: TFileStream;
  begin
    Result := False;
    if (Length(Text) = 0) then
      Exit;

    if FileExists(FileName) then
      Stream := TFileStream.Create(FileName, fmOpenReadWrite or fmShareDenyWrite)
    else
      Stream := TFileStream.Create(FileName, fmCreate or fmShareDenyWrite);

    try
      Stream.Seek(0, soEnd);

      Result := Stream.Write(Text[1], Length(Text)) = Length(Text);
    finally
      Stream.Free();
    end;
  end;

begin
  PBoolean(Result)^ := AppendFileContents(PlpString(Params^[0])^, PlpString(Params^[1])^);
end;

procedure _LapeWriteFileContents(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}

  function WriteFileContents(const FileName: lpString; const Text: lpString): Boolean;
  var
    Stream: TFileStream;
  begin
    Result := False;
    if (Length(Text) = 0) then
      Exit;

    if FileExists(FileName) then
      Stream := TFileStream.Create(FileName, fmOpenReadWrite or fmShareDenyWrite)
    else
      Stream := TFileStream.Create(FileName, fmCreate or fmShareDenyWrite);

    try
      Stream.Size := 0;

      Result := Stream.Write(Text[1], Length(Text)) = Length(Text);
    finally
      Stream.Free();
    end;
  end;

begin
  PBoolean(Result)^ := WriteFileContents(PlpString(Params^[0])^, PlpString(Params^[1])^);
end;

procedure _LapeReadFileContents(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}

  function ReadFileContents(FileName: lpString): lpString;
  var
    Stream: TFileStream;
  begin
    SetLength(Result, 0);

    if FileExists(FileName) then
    begin
      Stream := TFileStream.Create(FileName, fmOpenReadWrite or fmShareDenyWrite);
      try
        SetLength(Result, Stream.Size);

        Stream.Read(Result[1], Length(Result));
      finally
        Stream.Free();
      end;
    end;
  end;

begin
  PlpString(Result)^ := ReadFileContents(PlpString(Params^[0])^);
end;

procedure _LapeForceDirectories(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PBoolean(Result)^ := ForceDirectories(PlpString(Params^[0])^);
end;

procedure _LapeCreateDirectory(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PBoolean(Result)^ := CreateDir(PlpString(Params^[0])^);
end;

procedure _LapeDeleteDirectory(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}

  function DeleteDirectory(Directory: lpString; OnlyChildren: Boolean): Boolean;
  var
    Info: TSearchRec;
    FileName: String;
  begin
    Directory := IncludeTrailingPathDelimiter(ExpandFileName(Directory));

    if (FindFirst(Directory + '*', faAnyFile, Info) = 0) then
    begin
      repeat
        if (Info.Name = '.') or (Info.Name = '..') then
          Continue;

        FileName := Directory + Info.Name;

        if (Info.Attr and faDirectory <> 0) then
        begin
          if (not DeleteDirectory(FileName, False)) then
            Exit(False);

          Continue;
        end;

        if (not SysUtils.DeleteFile(FileName)) then
          Exit(False);
      until (FindNext(Info) <> 0);

      SysUtils.FindClose(Info);
    end;

    Result := OnlyChildren or RemoveDir(Directory);
  end;

begin
  PBoolean(Result)^ := DeleteDirectory(PlpString(Params^[0])^, PBoolean(Params^[1])^);
end;

procedure _LapeCreateFile(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}

  function CreateFile(const FileName: String): Boolean;
  const
    INVALID_FILE_HANDLE = THandle(-1);
  var
    Handle: THandle;
  begin
    if FileExists(FileName) then
      Exit(True);

    Handle := FileCreate(FileName);

    Result := Handle <> INVALID_FILE_HANDLE;
    if Result then
      FileClose(Handle);
  end;

begin
  PBoolean(Result)^ := CreateFile(PlpString(Params^[0])^);
end;

procedure _LapeDeleteFile(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PBoolean(Result)^ := SysUtils.DeleteFile(PlpString(Params^[0])^);
end;

procedure _LapeRenameFile(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PBoolean(Result)^ := RenameFile(PlpString(Params^[0])^, PlpString(Params^[1])^);
end;

procedure _LapeCopyFile(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}

  function CopyFile(const SourceFileName, DestFileName: lpString; Overwrite: Boolean = False): Boolean;
  var
    SourceStream, DestStream: TFileStream;
  begin
    Result := False;
    if (not FileExists(SourceFileName)) or ((not Overwrite) and FileExists(DestFileName)) then
      Exit;

    SourceStream := TFileStream.Create(SourceFileName, fmOpenReadWrite or fmShareDenyWrite);
    try
      if FileExists(DestFileName) then
        DestStream := TFileStream.Create(DestFileName, fmOpenReadWrite or fmShareDenyWrite)
      else
        DestStream := TFileStream.Create(DestFileName, fmCreate or fmShareDenyWrite);

      try
        Result := DestStream.CopyFrom(SourceStream, SourceStream.Size) = SourceStream.Size;
      finally
        DestStream.Free();
      end;
    finally
      SourceStream.Free();
    end;
  end;

begin
  PBoolean(Result)^ := CopyFile(PlpString(Params^[0])^, PlpString(Params^[1])^, PBoolean(Params^[2])^);
end;

procedure _LapeFileExists(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PBoolean(Result)^ := FileExists(PlpString(Params^[0])^);
end;

procedure _LapeDirectoryExists(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PBoolean(Result)^ := DirectoryExists(PlpString(Params^[0])^);
end;

procedure _LapeFileSize(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}

  function FileSize(const FileName: String): Int64;
  var
    Stream: TFileStream;
  begin
    Result := -1;
    if not FileExists(FileName) then
      Exit;

    Stream := TFileStream.Create(FileName, fmOpenRead or fmShareDenyWrite);
    try
      Result := Stream.Seek(0, soEnd);
    finally
      Stream.Free();
    end;
  end;

begin
  PInt64(Result)^ := FileSize(PlpString(Params^[0])^);
end;

procedure _LapeFileAge(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PInteger(Result)^ := FileAge(PlpString(Params^[0])^);
end;

procedure _LapeFileAgeDateTime(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PBoolean(Result)^ := FileAge(PlpString(Params^[0])^, PDateTime(Params^[1])^);
end;

procedure _LapeExtractFilePath(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PlpString(Result)^ := ExtractFilePath(PlpString(Params^[0])^);
end;

procedure _LapeExtractFileDrive(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PlpString(Result)^ := ExtractFileDrive(PlpString(Params^[0])^);
end;

procedure _LapeExtractFileName(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PlpString(Result)^ := ExtractFileName(PlpString(Params^[0])^);
end;

procedure _LapeExtractFileExt(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PlpString(Result)^ := ExtractFileExt(PlpString(Params^[0])^);
end;

procedure _LapeExtractFileDir(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PlpString(Result)^ := ExtractFileDir(PlpString(Params^[0])^);
end;

procedure _LapeExpandFileName(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PlpString(Result)^ := ExpandFileName(PlpString(Params^[0])^);
end;

procedure _LapeExtractRelativePath(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PlpString(Result)^ := ExtractRelativePath(PlpString(Params^[0])^, PlpString(Params^[1])^);
end;

procedure _LapeIncludeTrailingPathDelimiter(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PlpString(Result)^ := IncludeTrailingPathDelimiter(PlpString(Params^[0])^);
end;

procedure _LapeExcludeTrailingPathDelimiter(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PlpString(Result)^ := ExcludeTrailingPathDelimiter(PlpString(Params^[0])^);
end;

procedure _LapeIncludeTrailingBackslash(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PlpString(Result)^ := IncludeTrailingBackslash(PlpString(Params^[0])^);
end;

procedure _LapeExcludeTrailingBackslash(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PlpString(Result)^ := ExcludeTrailingBackslash(PlpString(Params^[0])^);
end;

procedure _LapeIncludeLeadingPathDelimiter(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}

  {$IFNDEF FPC}
  function IncludeLeadingPathDelimiter(const Path: lpString): lpString;
  begin
    Result := Path;
    If (Length(Result) = 0) or (Result[1] <> DirectorySeparator) then
      Result := DirectorySeparator + Result;
  end;
  {$ENDIF}

begin
  PlpString(Result)^ := IncludeLeadingPathDelimiter(PlpString(Params^[0])^);
end;

procedure _LapeExcludeLeadingPathDelimiter(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}

  {$IFNDEF FPC}
  function ExcludeLeadingPathDelimiter(const Path: lpString): lpString;
  begin
    Result := Path;
    If (Length(Result) > 0) and (Result[1] = DirectorySeparator) then
      Delete(Result, 1, 1);
  end;
  {$ENDIF}

begin
  PlpString(Result)^ := ExcludeLeadingPathDelimiter(PlpString(Params^[0])^);
end;

procedure _LapeChangeFileExtension(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PlpString(Result)^ := ChangeFileExt(PlpString(Params^[0])^, PlpString(Params^[1])^);
end;

procedure _LapeSetPathSeperators(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}

  function SetPathSeparators(const Path: lpString): lpString;
  var
    i: Integer;
  begin
    Result := Path;
    for i := 1 to Length(Result) do
      if CharInSet(Result[I], ['\', '/']) then
        Result[I] := DirectorySeparator;
  end;

begin
  PlpString(Result)^ := SetPathSeparators(PlpString(Params^[0])^);
end;

procedure _LapeJoinPaths(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}

  {$IFNDEF FPC}
  function ExcludeLeadingPathDelimiter(const Path: lpString): lpString;
  begin
    Result := Path;
    If (Length(Result) > 0) and (Result[1] = DirectorySeparator) then
      Delete(Result, 1, 1);
  end;
  {$ENDIF}

  function JoinPaths(const Paths: TStringArray): lpString;
  var
    I: Integer;
  begin
    if Length(Paths) > 0 then
    begin
      Result := Paths[0];
      for I := 1 to Length(Paths) - 1 do
        Result := IncludeTrailingPathDelimiter(Result) + ExcludeLeadingPathDelimiter(Paths[I]);
    end else
      Result := '';
  end;

begin
  PlpString(Result)^ := JoinPaths(PStringArray(Params^[0])^);
end;
