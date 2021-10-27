{%MainUnit lptypes.pas}

begin
  Item := GetMemory(ElSize);

  repeat
    Lo := iLo;
    Hi := iHi;
    Mid := Weights[(Lo + Hi) shr 1];
    repeat
      if SortUp then
      begin
        while (Weights[Lo] < Mid) do Inc(Lo);
        while (Weights[Hi] > Mid) do Dec(Hi);
      end else
      begin
        while (Weights[Lo] > Mid) do Inc(Lo);
        while (Weights[Hi] < Mid) do Dec(Hi);
      end;
      if (Lo <= Hi) then
      begin
        if (Lo <> Hi) then
        begin
          T := Weights[Lo];
          Weights[Lo] := Weights[Hi];
          Weights[Hi] := T;

          Move(Arr[Lo * ElSize], Item^, ElSize);
          Move(Arr[Hi * ElSize], Arr[Lo * ElSize], ElSize);
          Move(Item^, Arr[Hi * ElSize], ElSize);
        end;
        Inc(Lo);
        Dec(Hi);
      end;
    until Lo > Hi;

    // sort the smaller range recursively
    // sort the bigger range via the loop
    // Reasons: memory usage is O(log(n)) instead of O(n) and loop is faster than recursion
    if Hi - iLo < iHi - Lo then
    begin
      if iLo < Hi then
        QuickSort(Arr, ElSize, Weights, iLo, Hi, SortUp);
      iLo := Lo;
    end else
    begin
      if Lo < iHi then
        QuickSort(Arr, ElSize, Weights, Lo, iHi, SortUp);
      iHi := Hi;
    end;
  until iLo >= iHi;

  FreeMem(Item);
end;

