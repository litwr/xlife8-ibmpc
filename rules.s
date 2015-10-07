;*;live, born - word
;*;fillrt
;*;setrconst

;;fillrt1: mov #1,r4
;;         tstb r3
;;         beq 1$
fillrt1: mov ax,1
         or ch,ch
         jz .c1

;;2$:      asl r4
;;         dec r3
;;         bne 2$
.c2:     shl ax,1
         dec ch
         jnz .c2

;;1$:      return
.c1:     retn

;;fillrtsl: adc r3
;;         add r2,r3
;;         call @#fillrt1
;;         mov r4,@#temp
;;         clr r3
;;         bisb r1,r3
;;         return
fillrtsl:adc ch,dh
         call fillrt1    ;sets ch=0
         mov [temp],ax
         or ch,bl
         retn

;;fillrtsr: adc r3
;;         call @#fillrt1
;;         mov r4,@#temp2
;;         return
fillrtsr:adc ch,bh
         call fillrt1
         mov [temp2],ax
         retn

;;fillrt2: bcc 1$
fillrt2: jnc .c1

;;         mov @#live,r4
;;         bit @#temp,r4
;;         beq 3$
         mov ax,[live]
         test [temp],ax
         jz .c3

;;2$:      asl r0
;;         bisb r0,gentab(r1)
.c2:     shl dl,1
         or [gentab+bx],dl

;;         asr r0
;;         bne 3$
         shr dl,1
         jnz .c3

;;1$:      mov @#born,r4
;;         bit @#temp,r4
;;         bne 2$
.c1:     mov ax,[born]
         test [temp],ax
         jnz .c2

;;3$:      tst r2
;;         beq 11$
.c3:     or dh,dh
         jz .c11

;;         mov @#live,r4
;;         bit r4,@#temp2
;;         beq 13$
         mov ax,[live]
         test [temp2],ax
         jz .c13

;;12$:     bisb r0,gentab(r1)
;;         return 
.c12:    or [bx+gentab],dl
         retn

;;11$:     mov @#born,r4
;;         bit @#temp2,r4
;;         bne 12$
;;13$:     return
.c11:    mov ax,[born]
         test [temp2],ax
         jnz .c12
.c13:    retn

;;fillrt:  clr r1          ;XR
fillrt:  xor bx,bx

;;1$:      mov #1,r0       ;AC
;;         clrb gentab(r1)
.c1:     mov dl,1
         mov [bx+gentab],bh

;;         mov r1,r2
;;         bic #65535-1,r2     ;i1
;;         clr r3
;;         bisb r1,r3
;;         asr r3
;;         asr r3
;;         asr r3
;;         asr r3
;;         asr r3
         mov dh,bl
         and dh,1
         xor ch,ch
         or ch,bl
         mov cl,5
         shr ch,cl

;;         clc
;;         push r3
;;         call @#fillrtsl
         clc
         push cx
         call fillrtsl

;;         bic #65535-30,r3
;;         asr r3
;;         asr r3
;;         mfps r5
;;         call @#fillrtsr
         and ch,30
         shr ch,1
         shr ch,1
         pushf
         call fillrtsr

;;         mtps r5
;;         call @#fillrt2
         popf
         call fillrt2

;;         mov #4,r0
;;         mov r1,r2
;;         bic #65535-8,r2
;;         asr r2
;;         asr r2
;;         asr r2
;;         pop r3
         mov dl,4
         mov dh,bl
         and dh,8
         mov cl,3
         shr dh,cl
         pop cx

;;         call @#fillrtsl
;;         bic #65535-16,r3
;;         aslb r3
;;         aslb r3
;;         aslb r3
;;         aslb r3
         call fillrtsl
         and ch,16
         mov cl,4
         shl ch,cl

;;         mfps r5
;;         mov r1,r3
;;         bic #65535-7,r3
;;         call @#fillrtsr
;;         mtps r5
         pushf
         mov ch,bl
         and ch,7
         popf
         pushf
         call fillrtsr
         popf
         pushf

;;         call @#fillrt2
;;         mov #16,r0
;;         mtps r5
;;         call @#fillrt2
;;         mov #64,r0
;;         mov r1,r2
;;         bic #65535-64,r2
         call fillrt2
         mov dl,16
         popf
         call fillrt2
         mov dl,64
         mov dh,bl
         and dh,64

;;         aslb r2
;;         aslb r2
;;         adc r2
;;         mov r1,r3
;;         bic #65535-56,r3
;;         asr r3
;;         asr r3
;;         asr r3
;;         call @#fillrtsl
;;         aslb r3
;;         mfps r5
;;         mov r1,r3
;;         bic #65535-7,r3
         shl dh,1
         shl dh,1
         adc dh,bh
         mov ch,bl
         and ch,56
         mov cl,3
         shr ch,cl
         call fillrtsl
         shl ch,1
         pushf
         mov ch,bl
         and ch,7
         popf
         pushf

;;         call @#fillrtsr
;;         mtps r5
;;         call @#fillrt2
;;         incb r1
;;         movb r1,r1
;;         bne 1$
;;         return   ;ZF=1 required for loadpat
         call fillrtsr
         popf
         call fillrt2
         inc bl
         jnz .c1
.ep1:    retn

;;setrconst:    ;IN: R4 - string, R3 - end of string, R5 - live/born
setrconst:      ;IN: si - string, di - end of string, bp - live/born

;;          clr @r5
;;2$:       cmp r4,r3
;;          bpl exit5
          mov word [bx],0
.c2:      cmp si,di
          jns fillrt.ep1

;;          movb (r4)+,r0
;;          mov #1,r1
;;          sub #'0,r0
;;          beq 11$
          lodsb
          mov dx,1
          sub al,'0'
          jz .c11

;;1$:       asl r1
;;          sob r0,1$
.c1:      shl dx,1
          dec al
          jnz .c1

;;11$:      bis r1,@r5
;;          br 2$
.c11:     or [bx],dx
          jmp .c2

;;showrules proc
;;        local loop1,loop2,loop3,loop4,loop5,cont1,cont2,cont4,cont5,showr0,showr1
;;        ld hl,$1519
;;        call TXT_SET_CURSOR
;;        ld a,2
;;        call TXT_SET_PEN
;;        call printn
;;        db "        $"
;;        ld a,21
;;        call TXT_SET_COLUMN
;;        ld ix,live
;;        ld b,0

;;;*        lda #1
;;;*loop1   bit live
;;;*        bne cont1
;;        ld a,1
;;loop1   ld c,a
;;        and (ix)
;;        ld a,c
;;        jr nz,cont1

;;loop2   sla a
;;        jr nz,loop1

;;        ld a,(ix+1)
;;        or a
;;        jr z,cont4

;;;*        lda #"8"
;;;*        jsr showr1
;;;*        beq cont3
;;        ld a,8
;;        call showr1
;;        ret z

;;;*cont4   lda #"/"
;;;*        jsr showr1
;;;*        beq cont3
;;cont4   ld a,"/"
;;        call showr1
;;        ret z

;;;*        lda #1
;;;*loop4   bit born
;;;*        bne cont5
;;        ld a,1
;;loop4   ld c,a
;;        and (ix+2)
;;        ld a,c
;;        jr nz,cont5

;;;*loop5   asl
;;;*        bne loop4
;;loop5   sla a
;;        jr nz,loop4

;;;*        lda born+1
;;;*        beq cont3
;;        ld a,(ix+3)
;;        or a
;;        ret z

;;;*        lda #"8"
;;;*        jmp showr1
;;        ld a,"8"
;;        jr showr1

;;;*cont5   jsr showr0
;;;*        bne loop5
;;cont5   call showr0
;;        jr nz,loop5
;;        ret

;;;*cont1   jsr showr0
;;;*        bne loop2
;;cont1   call showr0
;;        jr nz,loop2
;;        ret

;;;*showr0  sta t1
;;;*        ldx #$ff
;;showr0  ld c,a
;;        ld e,$ff
;;;*loop3   inx
;;;*        lsr
;;;*        bcc loop3
;;loop3   inc e
;;        rrca
;;        jr nc,loop3

;;        ld a,e
;;        xor $30
;;showr1  ld e,a
;;        ld a,b
;;        cp 10
;;        ld a,e
;;        jr nz,cont2

;;        ld a,"*"
;;cont2   call TXT_OUTPUT
;;        inc b
;;        ld a,b
;;        cp 11
;;        ld a,c
;;        ret
;;        endp

;;showrules2:
;;        mov #stringbuf,r3
;;        mov r3,r4
;;        mov #1,r1
;;        clr r2
;;1$:     bit r1,@#live
;;        beq 2$

;;        call @#20$
;;2$:     inc r2
;;        asl r1
;;        bpl 1$

;;        movb #'/,(r4)+
;;        mov #1,r1
;;        clr r2
;;4$:     bit r1,@#born
;;        beq 5$

;;        call @#20$
;;5$:     inc r2
;;        asl r1
;;        bpl 4$

;;        clrb @r4
;;        sub r3,r4
;;        mov #32,r1
;;        sub r4,r1
;;        asr r1
;;        mov #19,r2
;;        jmp @#showptxt

;;20$:    mov r2,r0
;;        add #'0,r0
;;        movb r0,(r4)+
;;exit5:  return
