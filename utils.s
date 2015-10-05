;calcspd
;zerocnt
;zerocc
;todec

zerocc:   inibcd cellcnt,4
          ret

zerogc:   inibcd gencnt,6
          retn

todec:    mov bx,stringbuf+1    ;convert dx:ax to stringbuf
          mov si,10
.l1:      mov cx,ax
          mov ax,dx
          xor dx,dx
          div si
          xchg ax,cx
          div si
          or dl,'0'
          mov [bx],dl
          inc bx
          mov dx,cx
          or cx,ax
          jnz .l1

          sub bx,stringbuf+1
          mov [stringbuf],bl
          retn

;;boxsz:   mov #192,@#boxsz_ymin
;;         mov #160,@#boxsz_xmin
;;         clr r3                ;boxsz_ymax
;;         clr r5                ;boxsz_xmax
;;         clr @#boxsz_curx
;;         clr @#boxsz_cury
;;         mov #tiles,r4
;;         clr @#lowbench         ;binary cell count
;;0$:      push r5
;;         mov #8,r5
;;         clr r2
;;9$:      clr r0
;;         bisb (r4)+,r0
;;         movb tab3(r0),r1
;;         add r1,@#lowbench
;;         bis r0,r2
;;         sob r5,9$
;;         pop r5
;;         sub #8,r4
;;         tst r2
;;         beq 17$

;;         push r2
;;         clr r1
;;         dec r1
;;2$:      inc r1
;;         aslb r2
;;         bcc 2$

;;;         sty t1
;;;         lda curx
;;         mov @#boxsz_curx,r2

;;;         asl
;;;         asl
;;;         asl
;;         asl r2
;;         asl r2
;;         asl r2

;;;         tax
;;;         adc t1
;;         mov r2,r0
;;         add r2,r1

;;;         cmp xmin
;;;         bcs cont2
;;         cmp r1,@#boxsz_xmin
;;         bcc 12$

;;;         sta xmin
;;         mov r1,@#boxsz_xmin

;;;cont2    pla
;;;         ldy #8
;;;loop3    lsr
;;;         dey
;;;         bcc loop3
;;12$:     pop r2
;;         mov #8,r1
;;3$:      dec r1
;;         asr r2
;;         bcc 3$

;;;         sty t1
;;;         txa
;;;         clc
;;;         adc t1
;;         add r0,r1

;;;         cmp xmax
;;;         bcc cont3
;;        cmp r1,r5
;;        bcs 13$

;;;         sta xmax
;;        mov r1,r5

;;;cont3    ldy #0
;;;loop4    lda (currp),y
;;;         bne cont4

;;;         iny
;;;         bpl loop4
;;13$:     mov r4,r1
;;4$:      tstb (r1)+
;;         beq 4$

;;         sub r4,r1
;;         dec r1

;;;cont4    sty t1
;;;         lda cury
;;;         asl
;;;         asl
;;;         asl
;;;         tax
;;;         adc t1
;;;         cmp ymin
;;;         bcs cont5
;;         mov @#boxsz_cury,r2
;;         asl r2
;;         asl r2
;;         asl r2
;;         mov r2,r0
;;         add r1,r2
;;         cmp r2,@#boxsz_ymin
;;         bcc 15$

;;;         sta ymin
;;         mov r2,@#boxsz_ymin

;;;cont5    ldy #7
;;;loop5    lda (currp),y
;;;         bne cont6

;;;         dey
;;;         bpl loop5
;;15$:     mov r4,r1
;;         add #8,r1
;;5$:      tstb -(r1)
;;         beq 5$

;;         sub r4,r1
;;         add r0,r1
;;         cmp r1,r3
;;         bcs 17$

;;         mov r1,r3
;;17$:     add #tilesize,r4
;;         inc @#boxsz_curx
;;         cmp #hormax,@#boxsz_curx
;;         bne 0$

;;         clr @#boxsz_curx
;;         inc @#boxsz_cury
;;         cmp #vermax,@#boxsz_cury
;;         bne 0$

;;;         sty cury
;;;         jmp loop0

;;;cont1    lda ymax
;;;         sbc ymin
;;;         adc #0
;;;         sta cury
;;;         sec
;;;         lda xmax
;;;         sbc xmin
;;;         adc #0
;;;         sta curx
;;;         lda xmax
;;;         ora ymax
;;;         ora tiles
;;;         rts
;;         mov r3,r0
;;         sub @#boxsz_ymin,r0
;;         inc r0
;;         mov r0,@#boxsz_cury
;;         mov r5,r4
;;         sub @#boxsz_xmin,r4
;;         inc r4       ;returns xsize
;;         mov r4,@#boxsz_curx
;;         mov @#tiles,r1
;;         bis r3,r1
;;         return

rndbyte: push cx   ;in: di
         push dx
         push bx

         mov cl,[density]
         xor dl,dl
         mov al,80h
         cli
         out 43h,al
         in al,42h
         mov ah,al
.l1:     shr ah,1
         jnz .l1

         mov ah,al
         xor al,al
         out 43h,al
         in al,40h
         shr al,1        ;mode 3 decrements counter by 2
         xor al,ah
         and al,7
         mov bx,bittab
         xlatb
         or dl,al
         in al,40h
         sti
         loop .l1

         or [di],dl
         inc di
         pop bx
         pop dx
         pop cx
         retn
