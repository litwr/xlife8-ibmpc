loadpat: mov ax,3d00h
         mov dx,fn
         int 21h
         jc .exit

         mov [filehl],ax
         mov bx,ax
         mov ax,4202h   ;fseek
         xor cx,cx
         xor dx,dx
         int 21h
         or dx,dx
         jnz .exit2

         sub ax,6
         jbe .exit2

         shr ax,1
         mov [filesz],ax
         mov ax,4200h
         int 21h
         mov si,[live]
         mov di,[born]
         mov ah,3fh   ;read file
         mov cx,6
         mov dx,x0
         int 21h
         mov al,byte [live+1]
         or al,byte [born+1]
         cmp al,2
         jnc .l1
 
         test byte [born],1
         jz .l2

.l1:     mov [live],si
         mov [born],di
         jmp .exit2

.l2:     push si
         push di
         ;call readtent   ;should adjust filesz
         ;call showrect
         pop di
         pop si
         ;jc .l1

         call fillrt
         ;call puttent
.l3:     mov ah,3fh
         mov bx,[filehl]
         mov cx,2
         mov dx,x0
         int 21h
         call putpixel
         dec [filesz]
         jnz .l3

.exit2:  mov ah,3eh    ;fclose
         mov bx,[filehl]
         int 21h
.exit:   retn

printbp: mov dl,' '
         cmp bp,10
         jnc .l1

         mov ah,2
         int 21h
.l1:     cmp bp,100
         jnc .l2

         mov ah,2
         int 21h
.l2:     mov ax,bp
.ee:     xor dx,dx
.ee1:    call todec
         lea si,[stringbuf+bx]
.l3:     mov dl,[si]
         dec si
         mov ah,2
         int 21h
         dec bx
         jnz .l3
         retn
         
showdir: call printstr   ;OUT: BP
         db ansiclrscn,10,'$'

         xor bp,bp
         mov dx,svfn
         mov cx,20h
         mov ah,4eh
         int 21h
         push es
         jc .exit

.l3:     call printstr
         db red,'$'
         call printbp
         call printstr
         db black,' $'
         les di,dword [dta]
         push di
.l2:     mov dl,[es:di+1eh] ;fn offset in DTA
         inc di
         cmp dl,'.'
         jz .l1

         mov ah,2
         int 21h
         jmp .l2

.l1:     call printstr
         db ' ',blue,'$'
         pop di
         mov ax,[es:di+1ah]  ;size offset in DTA
         call printbp.ee
         call printstr
         db green,'$'
         test bp,1
         jnz .l4

         mov ah,3
         xor bx,bx
         int 10h
         dec ah
         mov dl,20
         int 10h
         jmp .l5

.l4:     call printstr
         db 0dh,10,'$'
.l5:     inc bp
         cmp bp,1000
         jz .exit

         push ds
         mov ah,4fh
         lds dx,dword [dta]
         int 21h
         pop ds
         jnc .l3

.exit:   pop es
         test bp,1
         jz showfree

         call printstr
         db 0dh,10,'$'
showfree:mov ah,36h     ;after showdir
         xor dx,dx
         int 21h
         mul cx
         mov cx,10    ;1024
.l1:     shr dx,1
         rcr ax,1
         loop .l1
         mul bx
         call printbp.ee1
         call printstr
         db 'K free',black,'$'
;exitio:
         retn

findfn:  xor bp,bp          ;in: ax
         mov di,ax
         mov dx,svfn
         mov cx,20h
         mov ah,4eh
         int 21h
.l3:     cmp di,bp
         jz .l7
         
         inc bp
         push ds
         mov ah,4fh
         lds dx,dword [dta]
         int 21h
         pop ds
         jmp .l3

.l7:     push es
         les di,dword [dta]
         mov si,fn
.l2:     mov al,[es:di+1eh] ;fn offset in DTA
         inc di
         mov [si],al
         inc si
         or al,al
         jnz .l2
         jmp showdir.exit

;;savepat: call @#commonin
;;         dec @#io_op
;;         mov #svfn,r2
;;         mov #12,r3
;;1$:      movb (r2)+,(r0)+
;;         sob r3,1$

;;         mov #16384,r2
;;         mov @#lowbench,r0
;;         asl r0
;;         add #7,r0
;;         rol r0
;;         rol r0
;;         rol r0
;;         bic #65532,r0
;;         mov r0,(r2)+         ;number of blocks
;;         movb @#boxsz_curx,(r2)+    ;sizex
;;         movb @#boxsz_cury,(r2)+    ;sizey
;;         mov @#live,(r2)+
;;         mov @#born,(r2)+
;;         mov #tiles,r4
;;         clr @#boxsz_curx
;;         clr @#boxsz_cury
;;         mov #4,@#io_len
;;0$:      mov #8,r5
;;2$:      mov #todata,@#pageport
;;         movb (r4)+,r0
;;         bne 11$
;;4$:      sob r5,2$

;;         add #tilesize-8,r4
;;         inc @#boxsz_curx
;;         cmp #hormax,@#boxsz_curx
;;         bne 0$

;;         clr @#boxsz_curx
;;         inc @#boxsz_cury
;;         cmp #vermax,@#boxsz_cury
;;         bne 0$
;;         br 20$

;;11$:     mov #65535,r1
;;3$:      inc r1
;;         aslb r0
;;         bcs 14$
;;         beq 4$
;;         br 3$

;;14$:     mov @#boxsz_curx,r3
;;         asl r3
;;         asl r3
;;         asl r3
;;         add r1,r3
;;         sub @#boxsz_xmin,r3
;;         mov #toio,@#pageport
;;         movb r3,(r2)+
;;         mov @#boxsz_cury,r3
;;         asl r3
;;         asl r3
;;         asl r3
;;         add #8,r3
;;         sub r5,r3
;;         sub @#boxsz_ymin,r3
;;         movb r3,(r2)+
;;         inc @#io_len
;;         cmp #8192,@#io_len
;;         bne 3$

;;20$:     asl @#io_len
;;         beq exit20

;;21$:     push r1
;;         mov #io_op,r1
;;         mov #toio,@#pageport
;;         emt ^O36
;;         pop r1
;;         clr @#io_len
;;         tstb @#io_op+1
;;         bne ioerr1   ;????

;;         cmp #plainbox,r4
;;         beq exit20

;;         mov #io_fn+14,r3
;;25$:     tstb -(r3)
;;         beq 25$
;;         incb @r3
;;         mov #16384,r2
;;         br 3$

;;commonin:mov #toio,@#pageport
;;         mov #io_op,r0
;;         mov r0,r1
;;         mov #3,(r0)+
;;         mov #16384,(r0)+
;;         clr (r0)+
;;exit20:  return

;;iocf:    mov #io_op,r0    ;IN: R2 - 2/3 - write/read
;;         mov r0,r1
;;         mov r2,(r0)+
;;         mov #palette,(r0)+
;;         mov #1,(r0)+
;;         mov #"CO,(r0)+
;;         mov #"LO,(r0)+
;;         mov #"RS,(r0)+
;;         mov #".C,(r0)+
;;         mov #"FG,(r0)+
;;         clr @r0
;;         emt ^O36
;;         tstb @#io_op+1
;;         beq exit20

;;ioerror: tstb @#errst           ;must be after iocf
;;         beq exit20

;;ioerr1:  mov #toandos,@#pageport  ;must be after ioerror
;;         jsr r3,@#printstr
;;         .byte 12
;;         .asciz "IO ERROR"
;;         jmp @#getkey

;;showcomm:tstb @#fn
;;         beq exit20

;;         call @#totext
;;         jsr r3,@#printstr
;;         .byte 155,0
;;         call @#commonin
;;         mov #fn,r2
;;         mov #12,r3
;;1$:      movb (r2)+,r4
;;         movb r4,(r0)+
;;         cmpb #'.,r4
;;         bne 5$

;;         movb #'T,(r0)+
;;         movb #'X,(r0)+
;;         movb #'T,(r0)+
;;         sub #3,r3
;;         add #3,r2
;;5$:      sob r3,1$
;;         call @#showtxt0
;;         jsr r3,@#printstr
;;         .byte 155,0
;;         jmp @#tograph

;;copyr:   call @#commonin
;;         mov #"CR,(r0)+
;;         mov #".T,(r0)+
;;         mov #"XT,(r0)+
;;         mov #5,r2
;;1$:      clr (r0)+
;;         sob r2,1$

;;showtxt0:emt ^O36            ;must be after copyr
;;         tstb @#io_op+1
;;         bne ioerr1

;;         mov @#loaded_sz,r2
;;         mov #16384,r1
;;2$:      mov #toio,@#pageport
;;         movb (r1)+,r0
;;         mov #toandos,@#pageport
;;         emt ^O16
;;         push r1
;;1$:      call @#getkey2
;;         bne 1$

;;         mov #1000,r1
;;3$:      sob r1,3$
;;         pop r1
;;         sob r2,2$
;;         jmp @#getkey
