// https://gitlab.com/freepascal.org/fpc/source/-/blob/main/rtl/i386/i386.inc
// https://gitlab.com/freepascal.org/fpc/source/-/blob/main/rtl/x86_64/x86_64.inc

{$IFDEF CPUX86_64}
procedure FillXxxx_MoreThanTwoXmms; assembler; nostackframe;
{ Input:
  rcx = 'x'
  rdx = byte count
  xmm0 = pattern for ALIGNED writes
  First and last 16 bytes are written. }
const
  NtThreshold = 4 * 1024 * 1024;
asm
    { x can start and end misaligned on the vector boundary:

      x = ~~][H1][H2][...][T2][T1]~
          [UH]                 [UT]

      UH (“unaligned head”) potentially overlaps with H1 and is already written with 'movdqu' by the caller.
      At least 1 of its bytes is exclusive to it, i.e. if x is already aligned, H1 starts at byte 16.

      H1 and so on are called “aligned heads” or just “heads”.
      T1 and so on are called “aligned tails” or just “tails”.

      UT (“unaligned tail”) is written by the caller as well.
      At least 1 of its bytes is exclusive to it as well, that’s why 65 is subtracted below instead of 64. }

    lea    -65(%rcx,%rdx), %rax
    and    $-16, %rax { rax = “T4” (possibly fictive). }
    mov    %rax, %rdx { Remember T4 to rdx. }
    and    $-16, %rcx { rcx = H1 − 16. }
    sub    %rcx, %rax { rax = aligned byte count − 48. }
    movdqa %xmm0, 16(%rcx) { Write H1. }
    cmp    $32-48, %rax
    jle    .LOneAlignedTailWrite
    movdqa %xmm0, 32(%rcx) { Write H2. }
    cmp    $64-48, %rax
    jle    .LTwoAlignedTailWrites
    sub    $48, %rax { rax = aligned byte count − 96 (32 bytes already written + 64 bytes written after loop). }
    jle    .LFourAlignedTailWrites

    add    $48, %rcx { rcx = H3. }
    cmp    $NtThreshold, %rax
    jae    .L64xNT_Body

.balign 16
.L64x_Body:
    movdqa %xmm0, (%rcx)
    movdqa %xmm0, 16(%rcx)
    movdqa %xmm0, 32(%rcx)
    movdqa %xmm0, 48(%rcx)
    add    $64, %rcx
    sub    $64, %rax
    ja     .L64x_Body

.LFourAlignedTailWrites:
    movdqa %xmm0, (%rdx) { T4 }
    movdqa %xmm0, 16(%rdx) { T3 }
.LTwoAlignedTailWrites:
    movdqa %xmm0, 32(%rdx) { T2 }
.LOneAlignedTailWrite:
    movdqa %xmm0, 48(%rdx) { T1 }
    ret

.LRepStosIsNotBetter:
    cmp    $NtThreshold-64, %rax
    jb     .L64x_Body

.balign 16
.L64xNT_Body:
    movntdq %xmm0, (%rcx)
    movntdq %xmm0, 16(%rcx)
    movntdq %xmm0, 32(%rcx)
    movntdq %xmm0, 48(%rcx)
    add    $64, %rcx
    sub    $64, %rax
    ja     .L64xNT_Body
    sfence
    jmp    .LFourAlignedTailWrites
end;

procedure FillChar(var x;count:SizeInt;value:byte); assembler; nostackframe;
asm
{ win64: rcx dest, rdx count, r8b value
  linux: rdi dest, rsi count, rdx value }
    movzbl {$ifdef win64} %r8b {$else} %dl {$endif}, %eax
    imul   $0x01010101, %eax
{$ifndef win64}
    mov    %rsi, %rdx
    mov    %rdi, %rcx
{$endif win64}

    cmp    $3, %rdx
    jle    .L3OrLess
    cmp    $16, %rdx
    jl     .L4to15

    movd   %eax, %xmm0
    pshufd $0, %xmm0, %xmm0
    movdqu %xmm0, (%rcx)
    movdqu %xmm0, -16(%rcx,%rdx)
    cmp    $32, %rdx
    jg     FillXxxx_MoreThanTwoXmms
    ret

.L4to15:
    mov    %eax, (%rcx)
    cmp    $8, %edx
    jle    .LLast4
    mov    %eax, 4(%rcx)
    mov    %eax, -8(%rcx,%rdx)
.LLast4:
    mov    %eax, -4(%rcx,%rdx)
    ret

.L3OrLess:
    test   %rdx, %rdx
    jle    .LQuit
    mov    %al, (%rcx)
    mov    %al, -1(%rcx,%rdx)
    shr    $1, %edx
    mov    %al, (%rcx,%rdx)
.LQuit:
  end;

procedure Move(const source;var dest;count:SizeInt); assembler; nostackframe;
{ Linux: rdi source, rsi dest, rdx count
  win64: rcx source, rdx dest, r8 count }
const
  NtThreshold = 256 * 1024; { this limit must be processor-specific (1/2 L2 cache size) }
  PrefetchDistance = 512;
asm
{$ifndef win64}
    mov    %rdx, %r8
    mov    %rsi, %rdx
    mov    %rdi, %rcx
{$endif win64}

    cmp    $3, %r8
    jle    .L3OrLess
    cmp    $8, %r8
    jle    .L4to8
    cmp    $16, %r8
    jle    .L9to16
    movups (%rcx), %xmm4         { First and last 16 bytes, used both in .L33OrMore and 17–32 branch. }
    movups -16(%rcx,%r8), %xmm5
    cmp    $32, %r8
    jg     .L33OrMore
    movups %xmm4, (%rdx)         { 17–32 bytes }
    movups %xmm5, -16(%rdx,%r8)
    ret

    .balign 16
.L3OrLess:
    cmp    $1, %r8
    jl     .LZero
    movzbl (%rcx), %eax
    je     .LOne
    movzwl -2(%rcx,%r8), %r9d
    mov    %r9w, -2(%rdx,%r8)
.LOne:
    mov    %al, (%rdx)
.LZero:
    ret

.L4to8:
    mov    (%rcx), %eax
    mov    -4(%rcx,%r8), %r9d
    mov    %eax, (%rdx)
    mov    %r9d, -4(%rdx,%r8)
    ret

.L9to16:
    mov    (%rcx), %rax
    mov    -8(%rcx,%r8), %r9
    mov    %rax, (%rdx)
    mov    %r9, -8(%rdx,%r8)
.Lquit:
    ret
    .byte  102,102,102,102,102,102,102,102,102,102,102,144 { Turns .balign 16 before .Lloop32f into a no-op. }

.L33OrMore:
    movups -32(%rcx,%r8), %xmm3  { Second vector from the end. Wasted read if .Lback branch is taken (it uses second vector from the start instead), }
                                 { but -32(%rcx,%r8) is about to become not accessible so easily, .Lback is rare, and small .Lback is even rarer / matters even less. }

    sub    %rdx, %rcx            { rcx = src - dest }
    jz     .Lquit                { exit if src=dest }

    mov    %rcx, %rax
    neg    %rax
    cmp    %rax, %r8
    ja     .Lback                { count (r8) > unsigned(dest - src) (rax) if regions overlap }

    mov    %rdx, %r9             { remember original dest to write first 16 bytes }
    add    %rdx, %r8             { Move dest to the next 16-byte boundary. +16 if already aligned, as first 16 bytes will be writen separately anyway. }
    add    $16, %rdx
    and    $-16, %rdx
    sub    %rdx, %r8

.LRestAfterNTf:
    sub    $32, %r8              { During the N× loop, r8 is N bytes less than actually remained to allow sub N+jae .LLoop instead of sub N+cmp N+jae .LLoop. }
    jbe    .LPost32f
    cmp    $NtThreshold-32, %r8
    jae    .Lntf                 { might jump back right away after more checks, but the branch is taken only on huge moves so it's better to take these checks out of here... }

    .balign 16                   { no-op }
.Lloop32f:
    movups (%rcx,%rdx), %xmm0
    movaps %xmm0, (%rdx)
    movups 16(%rcx,%rdx), %xmm0
    movaps %xmm0, 16(%rdx)
    add    $32, %rdx
    sub    $32, %r8
    ja     .Lloop32f

.LPost32f:                       { +32 fixup not applied after 32× loop, r8 = remaining - 32 here. }
    movups %xmm3, (%rdx, %r8)
    movups %xmm5, 16(%rdx,%r8)   { Write first and last 16 bytes after everything else. }
    movups %xmm4, (%r9)          { Important for <16-byte step between src and dest. }
    ret

    .balign 16
.Lntf:
    cmp    $NtThreshold, %rcx    { Maybe change mind: don't bother bypassing cache if src and dest are close to each other }
    jb     .Lloop32f             { (this check is performed here to not stand in the way of smaller counts) }
    sub    $PrefetchDistance+32, %r8 { r8 = remaining - prefetch distance - bytes per loop (64), but 32 was subtracted already. }

    .balign 16                   { no-op }
.Lntloop64f:
    prefetchnta 0+PrefetchDistance(%rcx,%rdx,1)
    movups (%rcx,%rdx,1), %xmm0
    movntps %xmm0, (%rdx)
    movups 16(%rcx,%rdx,1), %xmm0
    movntps %xmm0, 16(%rdx)
    movups 32(%rcx,%rdx,1), %xmm0
    movntps %xmm0, 32(%rdx)
    movups 48(%rcx,%rdx,1), %xmm0
    movntps %xmm0, 48(%rdx)
    add    $64, %rdx
    sub    $64, %r8
    jae    .Lntloop64f

    sfence
    add    $PrefetchDistance+64, %r8
    jmpq   .LRestAfterNTf        { go handle remaining bytes }
    .byte  102,102,102,102,102,102,144 { Turns .balign 16 before .Lloop32b into a no-op. }

{ backwards move }
.Lback:
    movups 16(%rcx,%rdx), %xmm3  { Second vector from the start. }
    lea    (%rdx,%r8), %r9       { points to the end of dest; remember to write last 16 bytes }
    lea    -1(%r9), %r8          { move dest to the previous 16-byte boundary... }
    and    $-16, %r8
    sub    %rdx, %r8
    add    %r8, %rdx

.LRestAfterNTb:
    sub    $32, %r8
    jbe    .LPost32b
    cmp    $NtThreshold-32, %r8
    jae    .Lntb

    .balign 16                   { no-op }
.Lloop32b:
    sub    $32, %rdx
    movups 16(%rcx,%rdx), %xmm0
    movaps %xmm0, 16(%rdx)
    movups (%rcx,%rdx), %xmm0
    movaps %xmm0, (%rdx)
    sub    $32, %r8
    ja     .Lloop32b

.LPost32b:
    sub    %r8, %rdx
    movups %xmm3, -16(%rdx)
    movups %xmm4, -32(%rdx)
    movups %xmm5, -16(%r9)
    ret

    .balign 16
.Lntb:
    cmp    $-NtThreshold,%rcx
    jnb    .Lloop32b
    sub    $PrefetchDistance+32, %r8

    .balign 16                   { no-op }
.Lntloop64b:
    prefetchnta -PrefetchDistance(%rcx,%rdx,1)
    sub    $64, %rdx
    movups 48(%rcx,%rdx,1), %xmm0
    movntps %xmm0, 48(%rdx)
    movups 32(%rcx,%rdx,1), %xmm0
    movntps %xmm0, 32(%rdx)
    movups 16(%rcx,%rdx,1), %xmm0
    movntps %xmm0, 16(%rdx)
    movups (%rcx,%rdx,1), %xmm0
    movntps %xmm0, (%rdx)
    sub    $64, %r8
    jae    .Lntloop64b

    sfence
    add    $PrefetchDistance+64, %r8
    jmpq   .LRestAfterNTb
end;
{$ENDIF}

{$IFDEF CPUX86}
const
  FillXxxx_RepStosThreshold_NoERMS = 512 * 1024;

procedure FillXxxx_U32Pattern_RepStos_8OrMore; assembler; nostackframe;
{ eax — x, ecx — uint32 pattern, edx — byte count >= 8 (preferably >= FillXxxx_RepStosThreshold_(No)ERMS, depending on fast_large_repmovstosb). }
asm
{$ifdef FPC_ENABLED_CLD}
        cld
{$endif FPC_ENABLED_CLD}
        mov    %ecx, (%eax) { Write first 4 bytes unaligned. }
        push   %ecx { pattern }
        push   %edi
        mov    %eax, %edi { Move x to edi, as expected by ‘rep stosl’. }
        xchg   %eax, %ecx { now eax = pattern (as expected by ‘rep stosl’) and ecx = x (to rotate the pattern by its misalignment) }
        shl    $3, %ecx { ecx = misalignment of x in bits. }
        rol    %cl, %eax { misalign the pattern; no-op for FillChar, but handles misaligned cases of FillWord+. }
        add    %edi, %edx { edx = x end }
        lea    -1(%edx), %ecx { ecx = x end - 1. }
        add    $4, %edi
        and    $-4, %edi { edi = 4-byte aligned pointer strictly to the right of the start. }
        and    $-4, %ecx { ecx = 4-byte aligned pointer strictly to the left of the end. }
        sub    %edi, %ecx { ecx = byte count between them. }
        shr    $2, %ecx { ecx = uint32 count, as expected by ‘rep stosl’. }
        rep stosl
        pop    %edi
        pop    %ecx
        mov    %ecx, -4(%edx) { Write last 4 bytes unaligned. }
end;

label
  FillXxxx_MoreThanTwoXMMs;

procedure FillXxxx_U32Pattern_SSE2_16OrMore; assembler; nostackframe;
{ eax — x, ecx — uint32 pattern, edx — byte count >= 16 (preferably > 16). }
const
  NtThreshold = 4 * 1024 * 1024;
asm
        movd   %ecx, %xmm0
        pshufd $0, %xmm0, %xmm0 { xmm0 = pattern for unaligned writes }
        movdqu %xmm0, (%eax)
        movdqu %xmm0, -16(%eax,%edx)
        cmp    $32, %edx
        ja     .LMoreThanTwoVectors
        ret
        .byte  144 { Turn .balign 16 before .L64x_Body into a no-op. }

      { x can start and end misaligned on the vector boundary:
        x = ~~][H1][H2][...][T2][T1]~
            [UH]                 [UT]
        UH/UT stands for “unaligned head/tail”, both have 1~16 bytes. }

.LMoreThanTwoVectors:
        push   %esi
        mov    %ecx, %esi { esi = pattern }
        mov    %eax, %ecx
        shl    $3, %ecx { ecx = misalignment of x in bits }
        rol    %cl, %esi { misalign the pattern }
        movd   %esi, %xmm0
        pshufd $0, %xmm0, %xmm0
        pop    %esi

{ FillChar (to skip the misaligning above) and FillQWord jump here.
  eax — x, edx — byte count > 32, xmm0 = pattern for ALIGNED writes, first and last 16 bytes written. }
FillXxxx_MoreThanTwoXMMs:
        lea    -65(%eax,%edx), %ecx
        and    $-16, %ecx { ecx = “T4” (possibly fictive) = loop bound. }
        mov    %ecx, %edx { Remember T4 to edx. }
        and    $-16, %eax { eax = H1 − 16. }
        sub    %eax, %ecx { ecx = aligned byte count − 48. }
        movdqa %xmm0, 16(%eax) { Write H1. }
        cmp    $32-48, %ecx
        jle    .LOneAlignedTailWrite
        movdqa %xmm0, 32(%eax) { Write H2. }
        cmp    $64-48, %ecx
        jle    .LTwoAlignedTailWrites
        sub    $48, %ecx { ecx = aligned byte count − 96 (32 bytes already written + 64 bytes written after loop). }
        jle    .LFourAlignedTailWrites { ecx was ≤ 96−48 }

        add    $48, %eax { eax = H3. }
        cmp    $NtThreshold, %ecx
        jae    .L64xNT_Body

.balign 16 { no-op }
.L64x_Body:
        movdqa %xmm0, (%eax)
        movdqa %xmm0, 16(%eax)
        movdqa %xmm0, 32(%eax)
        movdqa %xmm0, 48(%eax)
        add    $64, %eax
        sub    $64, %ecx
        ja     .L64x_Body
.LFourAlignedTailWrites:
        movdqa %xmm0, (%edx) { T4 }
        movdqa %xmm0, 16(%edx) { T3 }
.LTwoAlignedTailWrites:
        movdqa %xmm0, 32(%edx) { T2 }
.LOneAlignedTailWrite:
        movdqa %xmm0, 48(%edx) { T1 }
        ret

.balign 16
.L64xNT_Body:
        movntdq %xmm0, (%eax)
        movntdq %xmm0, 16(%eax)
        movntdq %xmm0, 32(%eax)
        movntdq %xmm0, 48(%eax)
        add    $64, %eax
        sub    $64, %ecx
        ja     .L64xNT_Body
        sfence
        jmp    .LFourAlignedTailWrites
end;

procedure FillXxxx_U32Pattern_Plain_16OrMore; assembler; nostackframe;
{ eax — x, ecx — uint32 pattern, edx — byte count >= 12 (preferably >= 16). }
asm
        mov     %ecx, (%eax) { Write first 4 bytes. }
        lea     -9(%eax,%edx), %edx
        mov     %ecx, 5(%edx) { Write last 4 bytes. }
        and     $-4, %edx { edx = loop bound. }
        push    %esi
        mov     %ecx, %esi { esi = pattern }
        mov     %eax, %ecx
        shl     $3, %ecx { ecx = misalignment of x in bits }
        rol     %cl, %esi { misalign the pattern }
        add     $4, %eax
        and     $-4, %eax
.balign 16
.L8xLoop:
        mov     %esi, (%eax)
        mov     %esi, 4(%eax)
        add     $8, %eax
        cmp     %edx, %eax
        jb      .L8xLoop
        mov     %esi, (%edx)
        mov     %esi, 4(%edx)
        pop     %esi
end;

procedure FillXxxx_U32Pattern_Ladder_4to16; assembler; nostackframe;
{ eax — x, ecx — uint32 pattern, edx — byte count, 4 <= edx <= 16. }
asm
        mov     %ecx, (%eax)
        cmp     $8, %edx
        jle     .LLast4
        mov     %ecx, 4(%eax)
        mov     %ecx, -8(%eax,%edx)
.LLast4:
        mov     %ecx, -4(%eax,%edx)
end;

procedure FillChar_3OrLess; assembler; nostackframe;
{ cl — x, edx — byte count, Low(int32) <= edx <= 3. }
asm
        test    %edx, %edx
        jle     .LQuit
        mov     %cl, (%eax)
        mov     %cl, -1(%eax,%edx)
        shr     $1, %edx
        mov     %cl, (%eax,%edx)
.LQuit:
end;

procedure FillChar_Plain(var x;count:SizeInt;value:byte); assembler; nostackframe;
asm
        cmp     $3, %edx
        jle     FillChar_3OrLess

        movzbl  %cl, %ecx
        imul    $0x01010101, %ecx
        cmp     $16, %edx
        jbe     FillXxxx_U32Pattern_Ladder_4to16
        jmp     FillXxxx_U32Pattern_Plain_16OrMore
end;

procedure FillChar_SSE2(var x;count:SizeInt;value:byte); assembler; nostackframe;
asm
        cmp     $3, %edx
        jle     FillChar_3OrLess

        movzbl  %cl, %ecx
        imul    $0x01010101, %ecx
        cmp     $16, %edx
        jbe     FillXxxx_U32Pattern_Ladder_4to16
        cmp     $FillXxxx_RepStosThreshold_NoERMS, %edx
        jae     FillXxxx_U32Pattern_RepStos_8OrMore

        movd   %ecx, %xmm0
        pshufd $0, %xmm0, %xmm0 { xmm0 = pattern for unaligned writes }
        movdqu %xmm0, (%eax)
        movdqu %xmm0, -16(%eax,%edx)
        cmp    $32, %edx
        ja     FillXxxx_MoreThanTwoXMMs
end;

procedure FillChar(var x;count:SizeInt;value:byte);
begin
  if has_sse2_support then
    FillChar_SSE2(x, count, value)
  else
    FillChar_Plain(x, count, value);
end;
{$ENDIF}
