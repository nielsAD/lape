{
  Author: Niels A.D
  Project: Lape (https://github.com/nielsAD/lape)
  License: GNU Lesser GPL (http://www.gnu.org/licenses/lgpl.html)

  This include constains all the wrappers for file related functions.
}
{%MainUnit lpeval.pas}

procedure _LapeFindFiles(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}

  procedure Find(Directory: lpString; Extensions: TStringArray; Recursive: Boolean; var Files: TStringArray);
  var
    Info: TSearchRec;
    FileName, FileExtension: lpString;
    i: Integer;
  begin
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
            Find(FileName, Extensions,  Recursive, Files);

          Continue;
        end;

        for i := 0 to High(Extensions) do
          if (Extensions[i] = '*') or (Extensions[i] = FileExtension) or ('.' + Extensions[i] = FileExtension) then
          begin
            SetLength(Files, Length(Files) + 1);
            Files[High(Files)] := FileName;

            Break;
          end;
      until (FindNext(Info) <> 0);

      FindClose(Info);
    end;
  end;

begin
  Find(PlpString(Params^[0])^, PStringArray(Params^[1])^, PBoolean(Params^[2])^, PStringArray(Result)^);
end;

procedure _LapeFindDirectories(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}

  procedure Find(Directory: String; Recursive: Boolean; var Directories: TStringArray);
  var
    Info: TSearchRec;
    FileName: lpString;
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
          SetLength(Directories, Length(Directories) + 1);
          Directories[High(Directories)] := FileName;

          if Recursive then
            Find(FileName, Recursive, Directories);
        end;
      until (FindNext(Info) <> 0);

      FindClose(Info);
    end;
  end;

begin
  Find(PlpString(Params^[0])^, PBoolean(Params^[1])^, PStringArray(Result)^);
end;

procedure _LapeWriteFileContents(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}

  function WriteFileContents(const FileName: lpString; const Text: lpString; Append: Boolean): Boolean;
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
      if Append then
        Stream.Seek(0, soEnd)
      else
        Stream.Size := 0;

      Result := Stream.Write(Text[1], Length(Text)) = Length(Text);
    finally
      Stream.Free();
    end;
  end;

begin
  PBoolean(Result)^ := WriteFileContents(PlpString(Params^[0])^, PlpString(Params^[1])^, PBoolean(Params^[2])^);
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
    FileName: lpString;
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

        if (not DeleteFile(FileName)) then
          Exit(False);
      until (FindNext(Info) <> 0);

      FindClose(Info);
    end;

    Result := OnlyChildren or RemoveDir(Directory);
  end;

begin
  PBoolean(Result)^ := DeleteDirectory(PlpString(Params^[0])^, PBoolean(Params^[1])^);
end;

procedure _LapeDeleteFile(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PBoolean(Result)^ := DeleteFile(PlpString(Params^[0])^);
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
begin
  PlpString(Result)^ := IncludeLeadingPathDelimiter(PlpString(Params^[0])^);
end;

procedure _LapeExcludeLeadingPathDelimiter(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PlpString(Result)^ := ExcludeLeadingPathDelimiter(PlpString(Params^[0])^);
end;

