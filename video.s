insteps: call totext
.c38:    call printstr
         db ansiclrscn,green,"NUMBER OF GENERATIONS: "
         db black,'$'

.c3:     mov si,stringbuf
         xor bx,bx
         mov [temp2],bx
.c1:     call getkey
         cmp al,0dh  ;enter
         jz .c11

         cmp al,27   ;esc
         jnz .c16
.c20:    jmp curoff

.c16:    cmp al,8    ;backspace
         jz .c12

         cmp al,'0'+10
         jnc .c1

         cmp al,'0'
         jc .c1

         cmp bl,5
         jz .c1

         inc bx
         mov dl,al
         mov ah,2
         int 21h

         sub al,'0'
         mov [si],al
         inc si
         jmp .c1

.c12:    dec si
         dec bx
         js .c3

         call delchr
         jmp .c1

.c11:    cmp bl,0
         jz .c20

         sub si,bx   ;convert to binary
         xor dx,dx
         dec bx
         shl bx,1
.c33:    lodsb
         or al,al
         jz .c34

         mov di,[tobin+bx]
.c32:    add dx,di
         jnc .c38a
         jmp .c38       ;65535=max

.c38a:   dec al
         jnz .c32

.c34:    sub bx,2
         jns .c33

         mov [temp2],dx
         jmp .c20

bornstay:
         mov si,stringbuf
.c3:     mov di,si
.c1:     call getkey
         cmp al,0dh    ;Enter
         jz .c11

         cmp al,8     ;backspace
         jz .c12

         cmp cl,'0'
         jz .c40

         cmp al,27    ;esc
         jz .c11

.c40:    cmp al,cl
         jc .c1

         cmp al,'9'
         jnc .c1

         mov bp,si
.c4:     cmp bp,di
         jz .c5

;;         cmpb (r2)+,r0
         cmp al,[ds:bp]
         lahf
         inc bp
         sahf
         jz .c1
         jmp .c4

.c5:     mov [di],al
         inc di
         mov dl,al
         mov ah,2
         int 21h
         jmp .c1

.c11:    retn

.c12:    dec di
         cmp di,si
         js .c3

         call delchr
         jmp .c1

delchr:  push si              ;changes: ax,dx
         call printstr
         db 8,' ',8,'$'
         pop si
         retn

inborn:  call printstr
         db green,'THE RULES ARE DEFINED BY ',blue
         db 'BORN',green,' AND ',blue,'STAY',green
         db ' VALUES.  FOR EXAMPLE, ',purple
         db 'CONWAYS''S LIFE',green,' HAS BORN=3 AND STAY=23, '
         db  purple,'SEEDS',green,' - BORN=2 AND EMPTY STAY, '
         db purple,'HIGHLIFE,',green,' - BORN=36 AND STAY=23, '
         db purple,'LIFE WITHOUT DEATH',green
         db ' - BORN=3 AND STAY=012345678, ...',0dh,10,black
         db 'BORN = $'

;;         mov #'1,r5
;;         jmp @#bornstay
         mov cl,'1'
         jmp bornstay


instay:  call printstr
         db 0dh,10,'STAY = $'

         mov cl,'0'
         jmp bornstay

indens:  call totext
         call printstr
;         .byte 146
;         .ascii "SELECT DENSITY OR PRESS "
;         .byte 145
;         .ascii "KT"
;         .byte 146
;         .ascii " TO EXIT"
         db black,'SELECT DENSITY OR PRESS ',red,'ESC',black,' TO EXIT'
;         .byte 10,9,145,'0,147
;         .ascii " - 12.5%"
         db 0dh,10,9,red,'0',green,' - 12.5%'
;         .byte 10,9,145,'1,147
;         .ascii " - 28%"
         db 0dh,10,9,red,'1',green,' - 28%'
;         .byte 10,9,145,'2,147
;         .ascii " - 42%"
         db 0dh,10,9,red,'2',green,' - 42%'
;         .byte 10,9,145,'3,147
;         .ascii " - 54%"
         db 0dh,10,9,red,'3',green,' - 54%'
;         .byte 10,9,145,'4,147
;         .ascii " - 64%"
         db 0dh,10,9,red,'4',green,' - 64%'
;         .byte 10,9,145,'5,147
;         .ascii " - 73%"
         db 0dh,10,9,red,'5',green,' - 73%'
;         .byte 10,9,145,'6,147
;         .ascii " - 81%"
         db 0dh,10,9,red,'6',green,' - 81%'
;         .byte 10,9,145,'7,147
;         .ascii " - 88.5%"
         db 0dh,10,9,red,'7',green,' - 88.5%'
;         .byte 10,9,145,'8,147
;         .ascii " - 95%"
         db 0dh,10,9,red,'8',green,' - 95%'
;         .byte 10,9,145,'9,147
;         .asciz " - 100%"
;         .byte 0
         db 0dh,10,9,red,'9',green,' - 100%',black,'$'
;1$:      call @#getkey
;         cmpb #9,r0
;         beq 2$
         call curoff
.c1:     call getkey
         cmp al,27   ;ESC
         jz .c2

;         cmpb r0,#'0
;         bcs 1$
         cmp al,'0'
         jc .c1

;         cmpb r0,#'0+10
;         bcc 1$
         cmp al,'0'+10
         jnc .c1

;         sub #'0-1,r0
;         movb r0,@#density
         sub al,'0'-1
         mov [density],al
         
;2$:      jmp @#tograph
.c2:     jmp tograph

inmode:  call printstr
         db 0dh,10,0dh,10,green
         db 'SELECT BENCHMARK MODE'
         db 0dh,10,32,red,'0',green
         db ' - CALCULATIONS'
         db 0dh,10,32,red,'1',green
         db ' - VIDEO'
         db 0dh,10,32,red,'2',green
         db " - BOTH$"
.c1:     call getkey
         cmp al,'0'
         jc .c1

         cmp al,'3'
         jnc .c1

         sub al,'1'
         retn

help:    call totext
         call printstr
         db 9,bold,'*** XLIFE COMMANDS ***',normal
         db 0dh,10,red,'!',green
         db ' randomize screen'
         db 0dh,10,red,'%',green
         db ' set random density - default=42%'
         db 0dh,10,red,'+',green,'/',red,'-',green
         db ' zoom in/out'
         db 0dh,10,red,'.',green,'/',red,'Home',green
         db ' center/home cursor'
         db 0dh,10,red,'?',green
         db ' show this help'
         db 0dh,10,red,'B',green
         db ' benchmark'
         db 0dh,10,red,'C',green
         db ' clear screen'
         db 0dh,10,red,'E',green
         db ' toggle pseudocolor mode'
         db 0dh,10,red,'g',green
         db ' toggle run/stop mode'
         db 0dh,10,red,'h',green
         db ' toggle hide mode - about 20% faster'
         db 0dh,10,red,'l',green
         db ' load and transform file'
         db 0dh,10,red,'L',green
         db ' reload pattern'
         db 0dh,10,red,'o',green
         db ' one step'
         db 0dh,10,red,'Q',green
         db ' quit'
         db 0dh,10,red,'R',green
         db ' set the rules'
         db 0dh,10,red,'S',green
         db ' save'
         db 0dh,10,red,'t',green
         db ' toggle plain/torus topology'
         db 0dh,10,red,'v',green
         db ' show some info'
         db 0dh,10,red,'V',green
         db ' show comments to the pattern'
         db 0dh,10,red,'X',green,'/',red,'Z',green
         db ' reload/set&save palette'
         db 0dh,10,0dh,10,black
         db 'Use ',red,'cursor keys'
         db black, ' to set the position and '
         db red, 'space key', black
         db ' to toggle the current cell. '
         db 'Use ',red, 'shift', black
         db ' to speed up the movement$'
         call curoff
         call getkey
         jmp tograph

xyout:   cmp [zoom],0
         jnz xyout2
         
         mov dx,3
         mov di,192*40+66
         mov bx,xcrsr
         call digiout
         mov dl,3
         mov di,192*40+74
         ;mov bx,ycrsr
         jmp digiout

infoout: ;must be before showtinfo

         cmp [zoom],0
         jnz infoout2

         mov bx,gencnt
         mov dx,7

;;         mov #<statusline*64+16384+2>,r2
         mov di,192*40+2
         call digiout
         mov bx,cellcnt
         mov dx,5

;;         mov #<statusline*64+16384+18>,r2
         mov di,192*40+18
         call digiout

showtinfo:  ;;mov #tinfo,r0  ;must be after infoout
;;            mov @#tilecnt,r3
;;            asr r3
;;            asr r3
;;            cmp #120,r3   ;sets CY=0
;;            bne 1$
         ;cmp [zoom],0
         ;jnz showtinfo2
         
            mov bx,tinfo
            mov si,[tilecnt]
            shr si,1
            shr si,1
            cmp si,120
            jnz .c1

           ;;mov #1,@r0
           ;;clrb 2(r0)
           ;;br 2$
           mov word [bx],1
           mov byte [bx+2],0
           jmp .c2

;;1$:         mov #2570,@r0      ;$a0a
;;            movb ttab(r3),r1
;;            mov r1,r2
;;            bic #^B11110000,r1
;;            movb r1,2(r0)
;;            rorb r2   ;uses CY=0
;;            asrb r2
;;            asrb r2
;;            asrb r2
;;            beq 2$
.c1:        mov word [bx],0a0ah
            mov al,[si+ttab]
            mov ah,al
            and al,0fh
            mov [bx+2],al
            mov al,ah
            mov cl,4
            shr al,cl
            jz .c2

;;            movb r2,1(r0)
            mov [bx+1],al

;;2$:         mov #3,r1
;;            mov #<statusline*64+16384+30>,r2
;;            call @#digiout
;;            mov #todata,@#pageport
.c2:       mov dx,3
           mov di,192*40+30
           jmp digiout

xyout2:  mov cx,3
         mov di,24*80+66
         mov si,xcrsr
         call digiout2
         mov cl,3
         mov di,24*80+74
         jmp digiout2

infoout2: ;must be before showtinfo2
         mov si,gencnt
         mov cx,7
         mov di,24*80+2
         call digiout2
         mov si,cellcnt
         mov cl,5
         mov di,24*80+18
         call digiout2

showtinfo2:  ;must be after infoout2
           mov bx,tinfo      ;it is too similar to showtinfo
           mov si,[tilecnt]
           shr si,1
           shr si,1
           cmp si,120
           jnz .c1

           mov word [bx],1
           mov byte [bx+2],0
           jmp .c2

.c1:       mov word [bx],0f0f0h
           mov al,[si+ttab]
           mov cl,al
           and al,0fh
           mov [bx+2],al
           mov al,cl
           mov cl,4
           shr al,cl
           jz .c2

           mov [bx+1],al
.c2:       mov cx,3
           mov si,bx
           mov di,24*80+30
           jmp digiout2

calcx:   ;;movb @#crsrbit,r1  ;$80 -> 0, $40 -> 1, ...
;;         bis #65280,r1      ;$ff00, IN: R1, OUT: R1
;;1$:      add #256,r1
;;         aslb r1
;;         bcc 1$

;;         swab r1
;;         return
         mov ah,[crsrbit]
         mov al,0ffh
.c1:     inc al
         shl ah,1
         jnc .c1
         retn

showscnz:
;;xlimit   = $14
;;ylimit   = $15
;;         #assign16 i1,viewport
         mov si,[viewport]

;;         lda #5
;;         sta xlimit
         mov bl,5

;;         lda pseudoc
;;         bne showscnzp
         cmp [pseudoc],0
         jnz showscnzp

;;         lda #$c
;;         sta cont2+2
;;         lda #0
;;         sta cont2+1
         xor di,di

;;loop3    lda #3
;;         sta ylimit
.loop3:  mov bh,3

;;loop4    ldy #0              ;check sum?
;;loop2    lda (i1),y
;;         ldx #0
;;loop1    asl
;;         sta 7
;;         lda #32
;;         bcc cont2
.loop4:  mov cx,8
.loop2:  lodsb
         mov dl,8
         mov dh,al
.loop1:  shl dh,1
         mov ax,220h   ;space char
         jnc .cont2

;;         lda #81         ;live cell char
;;cont2    sta $c00,x
;;         lda 7
;;         inx
;;         cpx #8
;;         bne loop1
         mov al,9   ;live cell char and attribute
.cont2:  stosw
         dec dl
         jnz .loop1

;;         lda #39    ;CY=1
;;         adc cont2+1
;;         sta cont2+1
;;         bcc nocy1
         add di,80-16

;;         inc cont2+2
;;nocy1    iny
;;         cpy #8
;;         bne loop2
         loop .loop2

;;         dec ylimit
;;         beq cont3
         dec bh
         jz .cont3

;;         lda #<tilesize*20-1 ;CY=1
;;         adc i1
;;         sta i1
;;         lda i1+1
;;         adc #>tilesize*20
;;         sta i1+1
;;         bcc loop4
         add si,tilesize*20-8
         jmp .loop4

;;cont3    dec xlimit
;;         bne cont11
;;         rts
.cont3:  dec bl         ;xlimit
         jnz .cont11
         retn

;;cont11   lda cont2+1    ;CY=1
;;         sbc #<952
;;         sta cont2+1
;;         lda cont2+2
;;         sbc #>952
;;         sta cont2+2
;;         lda i1   ;CY=1
;;         sbc #<tilesize*39
;;         sta i1
;;         lda i1+1
;;         sbc #>tilesize*39
;;         sta i1+1
;;         bne loop3
.cont11: sub di,24*80-16
         sub si,tilesize*39+8
         jmp .loop3

showscnzp:
         mov si,[viewport]
         mov dl,5  ;xlimit
         xor di,di
.loop3:  mov dh,3   ;ylimit
.loop4:  mov cl,8
         lea bp,[si+count0]
.loop2:  mov ax,[ds:bp]
         and ax,18c0h
         mov ch,al
         shl ah,1
         or ch,ah
         mov ax,[ds:bp+2]
         and ax,318h
         shr al,1
         or ch,al
         or ch,ah
         lodsb
         add bp,4
         mov bl,8
         mov bh,al
.loop1:  shl ch,1
         rcr al,1
         shl bh,1
         rcr al,1
         test al,0c0h
         mov ax,220h   ;space char and attribute
         jns .cont2

         mov al,9    ;live cell char
         jpe .cont2

         mov ah,3    ;new cell attr
.cont2:  stosw
         dec bl
         jnz .loop1

         add di,80-16
         dec cl
         jnz .loop2

         dec dh
         jz .cont3

         add si,tilesize*20-8
         jmp .loop4

.cont3:  dec dl         ;xlimit
         jnz .cont11
         retn

.cont11: sub di,24*80-16
         sub si,tilesize*39+8
         jmp .loop3

gexit:    jmp crsrset

showscn:  call infoout
          or [zoom],0
          jz .l1    ;optimize for i8088???
          jmp showscnz

;;          tst @#tilecnt
;;          beq gexit
.l1:      or [tilecnt],0
          jz gexit

;;          tstb @#pseudoc
;;          beq showscn2
;;          br  showscnp
          or [pseudoc],0
          jz showscn2
          jmp showscnp

showscn2: ;;mov @#startp,r0
          mov si,[startp]

;;1$:       mov video(r0),r5
.l1:      mov di,[video+si]

;;          mov @r0,r1
;;          mov 2(r0),r2
;;          mov 4(r0),r3
;;          mov 6(r0),@#temp
;;          mov #tovideo,@#pageport

          lodsw
;;          movb r1,r4        ;word output!
;;          asl r4
          xor ch,ch
          mov cl,al
          mov bx,cx
          mov cl,ah
          shl bx,1

;;          mov vistab(r4),@r5
          mov ax,[vistab+bx]
          stosw

;;          swab r1
;;          movb r1,r1
;;          asl r1
          mov bx,cx
          shl bx,1

;;          mov vistab(r1),64(r5)
          mov ax,[vistab+bx]
          mov [es:di+2000h-2],ax

          lodsw
;;          movb r2,r4
;;          asl r4
          mov cl,al
          mov bx,cx
          mov cl,ah
          shl bx,1

;;          mov vistab(r4),128(r5)
          mov ax,[vistab+bx]
          mov [es:di+80-2],ax

;;          swab r2
;;          movb r2,r2
;;          asl r2
          mov bx,cx
          shl bx,1

;;          mov vistab(r2),192(r5)
          mov ax,[vistab+bx]
          mov [es:di+2000h+80-2],ax

          lodsw
;;          movb r3,r4
;;          asl r4
          mov cl,al
          mov bx,cx
          mov cl,ah
          shl bx,1

;;          mov vistab(r4),256(r5)
          mov ax,[vistab+bx]
          mov [es:di+80*2-2],ax

;;          swab r3
;;          movb r3,r3
;;          asl r3
          mov bx,cx
          shl bx,1

;;          mov vistab(r3),320(r5)
          mov ax,[vistab+bx]
          mov [es:di+2000h+2*80-2],ax

          lodsw
;;          mov @#temp,r2
;;          movb r2,r4
;;          asl r4
          mov cl,al
          mov bx,cx
          mov cl,ah
          shl bx,1

;;          mov vistab(r4),384(r5)
          mov ax,[vistab+bx]
          mov [es:di+80*3-2],ax

;;          swab r2
;;          movb r2,r2
;;          asl r2
          mov bx,cx
          shl bx,1

;;          mov vistab(r2),448(r5)
          mov ax,[vistab+bx]
          mov [es:di+2000h+3*80-2],ax

;;          mov #todata,@#pageport
;;          mov next(r0),r0
          mov si,[next-8+si]

;;          cmp #1,r0
;;          bne 1$
          cmp si,1
          jz .lz
          jmp .l1
          ;jnz .l1  ;optimize 8088? it was short at K1801BM1!

.lz:      jmp crsrset


showscnp: ;;mov @#startp,r0
;;1$:       call @#showscnp1
;;          mov next(r0),r0
;;          cmp #1,r0
;;          bne 1$
;;          jmp @#crsrset
          mov si,[startp]
.c1:      call showscnp1
          mov si,[si+next-8]
          cmp si,1
          jnz .c1
          jmp crsrset

showscnp1: ;;mov video(r0),r5
;;          mov @r0,r1
;;          bne 3$
          mov di,[si+video]
          lodsb
          or al,al
          jnz .c3

;;          mov #tovideo,@#pageport
;;          clr @r5
;;          clr 64(r5)
;;          mov #todata,@#pageport
;;          br 4$
          mov word [es:di],0
          jmp .c4

;;3$:       vidmacp count0,0
;;          swab r1
;;          bne 5$
.c3:      vidmacp count0-1,0
.c4:      lodsb
          or al,al
          jnz .c5

          mov word [es:di+2000h],0
          jmp .c6

.c5:      vidmacp count1-2,2000h
.c6:      lodsb
          or al,al
          jnz .c7

          mov word [es:di+80],0
          jmp .c8

.c7:      vidmacp count2-3,80
.c8:      lodsb
          or al,al
          jnz .c9

          mov word [es:di+2050h],0
          jmp .c10

.c9:      vidmacp count3-4,2050h
.c10:     lodsb
          or al,al
          jnz .c12

          mov word [es:di+160],0
          jmp .c14

.c12:     vidmacp count4-5,160
.c14:     lodsb
          or al,al
          jnz .c15

          mov word [es:di+20a0h],0
          jmp .c16

.c15:     vidmacp count5-6,20a0h
.c16:     lodsb
          or al,al
          jnz .c17

          mov word [es:di+240],0
          jmp .c18

.c17:     vidmacp count6-7,240
.c18:     lodsb
          or al,al
          jnz .c19

          mov word [es:di+20f0h],0
          retn

.c19:     vidmacp count7-8,20f0h
          retn

chgdrv:  mov al,[curdrv]
         mov bx,drives
.l2:     inc ax
         cmp al,26
         jnz .l1

         xor ax,ax
.l1:     mov [curdrv],al
         xlatb
         or al,al
         mov dl,al
         mov al,[curdrv]
         jz .l2
         
         mov [loadmenu.c80],dl
         mov [es:240],dl
         sub dl,'A'
         mov ah,0eh
         int 21h
         retn

loadmenu:call totext
         call printstr
         db green,'INPUT FILENAME, AN EMPTY STRING MEANS TO SHOW DIRECTORY. PRESS '
         db red,'TAB',green,' TO USE RAMDISK, ',red,'*',green
         db ' TO CHANGE DRIVE, ',red,'ESC',green,' TO EXIT',0dh,10,black
.c80:    db 'A:$'

.c3:     mov di,fn
         xor cl,cl    ;length
.c1:     call getkey
         cmp al,0dh
         jz .c11

         cmp al,8   ;backspace
         jz .c12

         cmp al,27   ;esc
         jnz .c17

.c100:   mov ch,al
.c101:   ;call curoff
         or ch,ch
         retn

.c17:    cmp al,'*'
         jnz .c21

         mov ch,al
         call chgdrv
         jmp .c1

.c21:    cmp al,9    ;TAB
         jnz .c18

         ;call curoff
;;         call @#ramdisk
         ;call curon

         mov ch,1
         jmp .c101

.c18:    cmp al,'!'
         jc .c1

         cmp al,126
         jnc .c1

         mov si,nofnchar   ;+1? allows ?-char
         mov dl,al
.c5:     lodsb
         cmp dl,al
         jz .c1

         cmp dl,'a'
         jc .c6

         cmp dl,'z'+1
         jnc .c6

         sub dl,'a'-'A'
.c6:     cmp si,stringbuf
         jnz .c5

         cmp cl,8
         jnc .c1

         mov [di],dl
         inc di
         inc cl
         mov ah,2
         int 21h
         jmp .c1

.c11:    or cl,cl
         jz menu2

         mov word [di],'8'*256+'.'
         mov word [di+2],'L'*256+'X'
         xor ch,ch
         mov [di+4],ch
         jmp .c101

.c12:    dec di
         dec cl
         js .c3

         call delchr
         jmp .c1

menu2:   call setdirmsk
         cmp al,27     ;esc
         jz .c100

;;*         .text "run/stop"
;;*         .byte 30
;;*         .text " and "
;;*         .byte 28
;;*         .text "cbm key"
;;*         .byte 30
;;*         .text " as usual"
;;*         .byte $d,0

         call showdir  ;returns number of directory entries in BP
.c6:     call printstr
         db home,clrtoeol,green
         db 'ENTER FILE# OR ',red,'ESC',green,': ',black,'$'

.c3:     mov di,stringbuf+1      ;+1?
         mov si,di
         xor cx,cx
.c1:     call getkey
         cmp al,27     ;esc
         jnz .c17

.c100:   ;call curon
         jmp loadmenu

.c17:    cmp al,0dh
         jz .c11

         cmp al,8   ;backspace
         jz .c12

         cmp al,'0'
         jc .c1

         cmp al,'9'+1
         jnc .c1

         cmp cl,3
         jz .c1

         mov [di],al
         inc di
         inc cl
         mov ah,2
         mov dl,al
         int 21h
         jmp .c1

.c11:    xor ax,ax
         or cl,cl
         jz .c3

         lodsb
         sub al,'0'
         dec cl
         jz .c21

         mov ch,10
.l2:     mul ch
         add al,[si]
         inc si
         sub al,'0'
         dec cl
         jnz .l2
         
.c21:    cmp ax,bp
         jc .l3
         jmp .c6      ;optimize 8088

.l3:     call findfn
         ;call curoff
         xor ax,ax     ;sets ZF
         retn

.c12:    dec di
         dec cl
         js .c3

         call delchr
         jmp .c1

getsvfn: ;;call @#totext
;;         movb @#andos_disk,r0
;;         add #'A-1,r0
;;         movb r0,@#80$
;;         jsr r3,@#printstr
;;         .byte 12,146
;;         .ascii "Enter filename ("
;;         .byte 145
;;         .ascii "KT"
;;         .byte 146
;;         .ascii " - exit, "
;;         .byte 145, '*, 146
;;         .ascii " - drive)"
;;         .byte 147,10
;;80$:     .byte 'A,':,154,0,0

;;3$:      mov #svfn,r5
;;         clr r2
;;1$:      call @#getkey
;;         cmpb r0,#10
;;         beq 11$

;;         cmpb r0,#24  ;backspace
;;         beq 12$

;;         cmpb r0,#3   ;kt
;;         bne 17$

;;100$:    movb r0,r4
;;101$:    jsr r3,@#printstr
;;         .byte 154,0
;;         mtps r4
;;         return

;;17$:     cmpb r0,#'*
;;         bne 18$

;;         mov #2,r4
;;         call @#chgdrv
;;         br 1$

;;18$:     cmpb r0,#'!
;;         bcs 1$

;;         cmpb r0,#126
;;         bcc 1$

;;         mov #nofnchar,r3
;;5$:      cmpb r0,(r3)+
;;         beq 1$

;;         cmpb r0,#'a
;;         bcs 6$

;;         cmpb r0,#'z+1
;;         bcc 6$

;;         sub #'a-'A,r0
;;6$:      tstb @r3
;;         bne 5$

;;         cmp r2,#8
;;         bcc 1$

;;         movb r0,(r5)+
;;         inc r2
;;         emt ^O16 
;;14$:     br 1$

;;11$:     tst r2
;;         beq 100$

;;         movb #'.,(r5)+
;;         movb #'8,(r5)+
;;         movb #'L,(r5)+
;;         movb #'0,(r5)+
;;         clr r4
;;42$:     cmp r5,#msghide
;;         beq 101$

;;         clrb (r5)+
;;         br 42$

;;12$:     dec r5
;;         dec r2
;;         bmi 3$

;;         jsr r3,@#printstr
;;         .byte 24,0
;;         br 14$

showrect:
         mov dx,1800h  ;row+column
         mov ah,2
         int 10h
         call printstr
         db clrtoeol,gpurple,'MOVE, ',gwhite,'R',gpurple,'OTATE, '
         db gwhite,'F',gpurple,'LIP, ',gwhite,'ENTER',gpurple,', '
         db gwhite,'ESC',gcyan,'  X   Y',black,'$'
         call xyout
         xor bx,bx
         mov word [xdir],bx
         mov [xchgdir],bl
.c10:    call drawrect
         ;call showtent
.c11:    ;call crsrflash
         call getkey2
         cmp ax,4b00h    ;cursor left
         jz .c100

         cmp ax,4d00h  ;cursor right
         jz .c100

         cmp ax,4800h  ;cursor up
         jz .c100

         cmp ax,5000h   ;cursor down
         jz .c100

         cmp al,'.'     ;to center
         jz .c100

         cmp ax,4700h     ;to home
         jz .c100

         cmp al,'r'
         jnz .c1

         call clrrect
         not [xchgdir]
         mov ax,word [xdir]
         xchg al,ah
         not al
         mov word [xdir],ax
         jmp .c10

.c1:     cmp al,'f'
         jne .c2

         call clrrect
         not [xdir]
         jmp .c10

.c2:     cmp al,0dh
         je exit7

         cmp al,27      ;esc
         stc
         je exit7
         jmp .c11

.c100:   push ax
         call clrrect
         pop ax
         call dispatcher.e0
         jmp .c10

xchgxy:  or [xchgdir],0
         jz exit7

         mov al,[x0]
         xchg al,[y0]
         mov [x0],al
exit7:   retn


drawrect: call xchgxy
         xor bx,bx
         mov [xcut],bx       ;0 -> xcut,ycut
         mov al,[crsrbyte]
         mov [y8byte],al
         call calcx
         or al,[crsrx]
         mov dl,al   ;r1 - rectulx
         cmp bh,[xdir]
         je .c4

         sub dl,[x0]
         cmp al,dl
         jnc .c2
 
         not dl
         or dl,dl
         je .c10 

         inc byte [xcut]
.c10:    mov dl,al
         inc dl
         jmp .c7

.c4:     add dl,[x0]
         cmp dl,al
         jc .c5

         cmp dl,161
         jc .c2

.c5:     mov dl,160
         inc byte [xcut]

.c2:     mov bl,dl
         sub dl,al
         cmp bl,dl
         jnc .c7

         neg dl
.c7:     mov [x8poscp],dl
         mov dh,[crsry]
         or dh,[crsrbyte]
         mov al,dh
         mov bl,[y0]
         cmp [ydir],bh
         je .c3
         
         mov cl,dh
         sub dh,bl
         cmp cl,dh
         jnc .c1

         not dh
         or dh,dh
         je .c12

         inc byte [ycut]
.c12:    mov dh,al
         inc dh
         jmp .c8

.c3:     add dh,bl
         cmp dh,bl
         jc .c6

         cmp dh,193
         jc .c1

.c6:     mov dh,192
         inc byte [ycut]

.c1:     mov bl,dh
         sub dh,al
         cmp bl,dh
         jnc .c8
 
         neg dh
.c8:     mov [y8poscp],dh

         mov si,[crsrtile]
         mov bl,[crsrbit]
         call ymove
         cmp bh,[ycut]
         jne .c11

         call xmove
.c11:    mov dl,[x8poscp]
         mov dh,[y8poscp]
         mov cl,[crsrbyte]
         mov [y8byte],cl
         mov bl,[crsrbit]
         mov si,[crsrtile]
         call xmove
         cmp bh,[xcut]
         ;jne exitdrawrect   ;optimize 8088
         je ymove
         jmp exitdrawrect

ymove:   cmp [ydir],0
         jne loopup

loopdn:  call drrect1
.c10:    call pixel11p
         dec dh
         je exitdrawrect

         inc byte [y8byte]
         cmp byte [y8byte],8
         jne loopdn

         mov si,[down+si]
         mov [y8byte],bh
         jmp loopdn

loopup:  call drrect1
.c11:    call pixel11p
         dec dh
         je exitdrawrect

         dec byte [y8byte]
         jns loopup

         mov si,[up+si]
         mov byte [y8byte],7
         jmp loopup

xmove:   cmp [xdir],bh
         jne looplt

looprt:  call drrect1
.c12:    call pixel11p
         dec dl
         je exitdrawrect

         shr bl,1
         jnc .c12

         mov si,[right+si]
         mov bl,128
         jmp looprt

looplt:  call drrect1
.c15:    call pixel11p
         dec dl
         je exitdrawrect

         shl bl,1
         ;;movb r0,r0
         jnc .c15

         mov si,[si+left]
         mov bl,1
         jmp looplt

drrect1: mov di,[si+video]
         mov cl,[y8byte]
         shr cl,1
         jnc .l1

         add di,2000h
.l1:     mov al,80
         mul cl
         add di,ax
exitdrawrect: retn

loopup2: call xclrect2
         je exitdrawrect

         dec byte [y8byte]
         jns loopup2

         mov si,[si+up]
         mov byte [y8byte],7
         jmp loopup2

clrrect:  ;in: x8poscp, y8poscp
         call xchgxy
         call calcx   ;sets ah=0
         cmp [xdir],ah
         je .c3

         sub al,8
         not al
.c3:     add al,[x8poscp]
         shr al,1
         shr al,1
         shr al,1
         mov dl,al
         inc dl
         mov [x8poscp],dl
         mov dh,[y8poscp]
         mov cl,[crsrbyte]
         mov [y8byte],cl
         cmp [pseudoc],ah
         jne clrectpc

         mov si,[crsrtile]
         cmp [ydir],ah
         jne loopup2

loopdn2: call xclrect2
         je exitclrect2

         inc byte [y8byte]
         cmp byte [y8byte],8
         jne loopdn2

         mov si,[si+down]
         mov byte [y8byte],0
         jmp loopdn2

xclrect2:push si
         cmp [xdir],0
         jne .c2

.c1:     call clrect12
         mov si,[si+right]
         dec dl
         jnz .c1
         jmp .c3

.c2:     call clrect12
         mov si,[si+left]
         dec dl
         jnz .c2

.c3:     pop si
         mov dl,[x8poscp]
         dec dh
exitclrect2: retn

clrect12:xor bx,bx
         mov al,[y8byte]
         mov bl,al
         mov di,[si+video]
         shr al,1
         jnc .l1

         add di,2000h
.l1:     mov cl,80
         mul cl
         add di,ax
         mov bl,[si+bx]
         shl bx,1
         mov ax,[bx+vistab]
         stosw
         retn

clrectpc: ;;movb @#crsrbyte,r4
;;         tstb @#ydir
;;         beq 3$

;;         sub #8,r4
;;         comb r4
;;3$:      movb @#y8poscp,r0
;;         add r0,r4
;;         clc
;;         rorb r4
;;         asrb r4
;;         asrb r4
;;         inc r4
;;         mov @#crsrtile,r0
;;         tstb @#ydir
;;         bne loopuppc

loopdnpc: ;;call @#xclrectpc
;;         beq exitclrectpc

;;         mov down(r0),r0
;;         br loopdnpc

loopuppc: ;;call @#xclrectpc
;;       beq exitclrectpc

;;         mov up(r0),r0
;;         br loopuppc

xclrectpc: ;;push r0
;;         tstb @#xdir
;;         bne 2$

;;1$:      call @#clrect1pc
;;         mov right(r0),r0
;;         sob r3,1$
;;         br 3$

;;2$:      call @#clrect1pc
;;         mov left(r0),r0
;;         sob r3,2$
;;3$:      pop r0
;;         movb @#x8poscp,r3
;;         decb r4
exitclrectpc: ;;return

clrect1pc: ;;push r3
;;         push r4
;;         call @#showscnp1
;;         pop r4
;;         pop r3
;;         return
         
crsrset1:
;;         mov @#crsrtile,r0     ;sets r0,r1
;;         movb @#crsrbyte,r1
;;         swab r1
;;         asr r1
;;         asr r1
;;         add video(r0),r1
;;         movb @#crsrbit,r0
;;         return
         mov si,[crsrtile]
         xor bx,bx
         mov bl,[crsrbyte]
         shl bx,1
         mov di,[crsrtab+bx]
         add di,[si+video]
         xor bx,bx
         mov bl,[crsrbit]
         retn

setdirmsk:
         call printstr
         db ansiclrscn,green,'SET DIRECTORY MASK ('
         db red,'ENTER',green
         db ' = *)',black,0dh,10,'$'

.c3:     mov di,svfn
         xor cx,cx
.c1:     call getkey
         cmp al,0dh
         jz .c11

         cmp al,8    ;backspace
         jz .c12

         cmp al,27     ;esc
         jz .c13

         cmp al,'!'
         jc .c1

         cmp al,126
         jnc .c1

         cmp cl,8    ;fn length limit
         jnc .c1

         mov si,nofnchar+1
         mov dl,al
.c50:    lodsb
         cmp al,dl
         jz .c1

         cmp si,stringbuf
         jnz .c50

         cmp dl,'a'
         jc .c6

         cmp dl,'z'+1
         jnc .c6

         sub dl,'a'-'A'
.c6:     mov [di],dl
         inc di
         inc cl
         mov ah,2
         int 21h
         jmp .c1

.c11:    or cl,cl
         jnz .c5

         mov byte [di],'*'
         inc di
.c5:     mov word [di],'8'*256+'.'          
         mov word [di+2],'L'*256+'X'
         mov byte [di+4],ch
.c13:    retn

.c12:    dec di
         dec cl
         js .c3

         call delchr
         jmp .c1

setviewport:
;         ld hl,(crsrtile)
;         ld (viewport),hl
;         ld ix,vptilecx
;;        mov #viewport,r3
;;        mov @#crsrtile,@r3
;;        mov #vptilecx,r0
;;        movb @#crsry,r1
        mov di,viewport
        mov ax,[crsrtile]
        mov [di],ax
        mov si,vptilecx
        mov al,[crsry]

;         ld a,2
;         ld (vptilecx),a
;         dec a
;         ld (vptilecy),a
;;        mov #258,@r0      ;$102
       mov word [si],102h

;         ld hl,(ycrsr)
;         ld a,l
;         or h
;         jr nz,cont1
 
;         ld a,(ycrsr+2)
;         cp 8
;         jr nc,cont1
;;        cmpb r1,#8
;;        bcc 1$
        cmp al,8
        jnc .c1

;;        decb @#vptilecy
;;        add #tilesize*hormax,@r3  ;up
;;        br 2$
        dec [vptilecy]
        add word [di],tilesize*hormax
        jmp .c2

;cont1    ld a,(ycrsr)
;         dec a
;         jr nz,cont2

;         ld a,(ycrsr+1)
;         cp 8
;         jr c,cont2
;         jr nz,cont4

;         ld a,(ycrsr+2)
;         cp 4
;         jr c,cont2
;;1$:     cmpb r1,#184
;;        bcs 2$

;;        incb @#vptilecy     ;down
;;        sub #tilesize*hormax,@r3
.c1:    cmp al,184
        jc .c2

        inc [vptilecy]
        sub word [di],tilesize*hormax

;cont2    ld hl,(xcrsr)
;         ld a,l
;         or h
;         jr nz,cont3

;         ld a,(xcrsr+2)
;         cp 8
;         jr nc,cont3
;;2$:     movb @#crsrx,r1
;;        cmpb r1,#8
;;        bcc 3$
.c2:    mov al,[crsrx]
        cmp al,8
        jnc .c3

;         dec (ix)
;         dec (ix)
;         ld hl,(viewport)      ;left2
;         ld de,tilesize*2
;         add hl,de
;         ld (viewport),hl
;         jr cont5
;;        decb @r0
;;        decb @r0
;;        add #tilesize*2,@r3
;;        br 5$
        sub byte [si],2
        add word [di],tilesize*2
        jmp .c5

;cont3    ld a,(xcrsr)
;         or a
;         jr nz,cont6

;         ld a,(xcrsr+1)
;         cp 1
;         jr c,cont7
;         jr nz,cont6

;         ld a,(xcrsr+2)
;         cp 6
;         jr nc,cont6
;;3$:     cmpb r1,#16
;;        bcc 6$
.c3:    cmp al,16
        jnc .c6

;cont7    dec (ix)
;         ld hl,(viewport)      ;left1
;         ld de,tilesize
;         add hl,de
;         ld (viewport),hl
;         jr cont5
;;        decb @r0
;;        add #tilesize,@r3
;;        br 5$
        dec byte [si]
        add word [di],tilesize
        jmp .c5

;cont6    ld a,(xcrsr)
;         dec a
;         jr nz,cont8

;         ld a,(xcrsr+1)
;         cp 5
;         jr nz,cont8

;         ld a,(xcrsr+2)
;         cp 2
;         jr c,cont8
;;6$:     cmpb r1,#152
;;        bcs 8$
.c6:    cmp al,152
        jc .c8

;         inc (ix)
;         inc (ix)
;         ld hl,(viewport)      ;right2
;         ld de,(~(tilesize*2))+1
;         add hl,de
;         ld (viewport),hl
;         jr cont5
;;        incb @r0
;;        incb @r0
;;        sub #tilesize*2,@r3
;;        br 5$
        add byte [si],2
        sub word [di],tilesize*2
        jmp .c5
        
;cont8    ld a,(xcrsr)
;         dec a
;         jr nz,cont5

;         ld a,(xcrsr+1)
;         cp 4
;         jr c,cont5
;         jr nz,cont10

;         ld a,(xcrsr+2)
;         cp 4
;         jr c,cont5
;;8$:     cmpb r1,#144
;;        bcs 5$
.c8:    cmp al,144
        jc .c5

;cont10   inc (ix)
;         ld hl,(viewport)      ;right1
;         ld de,(~tilesize)+1
;         add hl,de
;         ld (viewport),hl
;;        incb @r0
;;        sub #tilesize,@r3
        inc byte [si]
        sub word [di],tilesize

;cont5    ld iy,(viewport)
;         ld hl,fixvp
;         call calllo
;         ld (viewport),hl
;;5$:     mov @r3,r4
;;        mov ul(r4),r4
;;        mov left(r4),@r3
.c5:    mov bx,[di]
        mov bx,[bx+ul]
        mov ax,[bx+left]
        mov [di],ax

;         ld b,3
;loop12   sla (ix)
;         sla (ix+1)
;         djnz loop12
;;        asl @r0
;;        asl @r0
;;        asl @r0
        mov cl,3
        shl word [si],cl

;         ld a,(crsrbyte)
;         add a,(ix+1)
;         ld (ix+1),a
;;        movb @#crsrbyte,r1
;;        swab r1
;;        add r1,@r0    ;vptilecy
;;        call @#calcx
;;        add r1,@r0
;;        return
        call calcx
        mov ah,[crsrbyte]
        add [si],ax
        retn

crsrset: call crsrset1
         cmp [zoom],0
         jnz gexit2

pixel11:
;;         asl r0
;;         mov vistab(r0),r2
;;         mov r2,r0
;;         asl r2
;;         bis r0,r2
;;         bis r2,@r1
         shl bx,1    ;it should be after crsrset, IN: bx - crsrbit, di - addr of video tile line
         mov si,[bx+vistab]
         mov ax,si
         shl ax,1
         or ax,si
         or [es:di],ax
gexit2:  retn         ;this is also gexit3

pixel11p:push si
         push bx
         call pixel11
         pop bx
         pop si
         retn

crsrclr: ;;tstb @#zoom
;;         bne 1$
         cmp [zoom],0
         jnz gexit2

;;         mov @#crsrtile,r0
;;         movb @#crsrbyte,r1
         mov si,[crsrtile]
         xor bx,bx
         mov bl,[crsrbyte]

;;         mov r1,r2
;;         add r0,r2
;;         movb @r2,r2
         mov al,[si+bx]

;;         swab r1
;;         asr r1
;;         asr r1
;;         add video(r0),r1
         shl bx,1
         mov bp,bx
         mov di,[crsrtab+bx]
         add di,[si+video]

;;         tstb @#pseudoc
;;         bne 2$
         cmp [pseudoc],0
         jnz .c2

;;         mov #tovideo,@#pageport
;;         asl r2
;;         mov vistab(r2),r2
         xor bx,bx
         mov bl,al
         shl bx,1
         mov ax,[bx+vistab]
         stosw
         retn

;;2$:      movb @#crsrbyte,r3
;;         asl r3
;;         asl r3
;;         add r0,r3
;;         bitb #15,@#crsrbit
;;         bne 3$
.c2:     shl bp,1
         add bp,si
         mov cl,4
         test [crsrbit],0fh
         jnz .c3

;;         mov count0(r3),r3
;;         bic #^B1110011100111111,r3
;;         mov r3,r4
;;         swab r4
;;         aslb r4   ;sets CY=0
;;         bis r3,r4
;;         rorb r2    ;uses CY=0
;;         asrb r2
;;         asrb r2
;;         asrb r2
;;         call @#8$
;;         br 5$
         mov dx,[ds:bp+count0]
         and dx,1100011000000b
         shl dh,1
         or dh,dl
         shr al,cl
.c4:     or al,dh
         mov bx,vistabpc
         xlatb
         stosb
         retn

;;3$:      inc r1
;;         mov count0+2(r3),r3
;;         bic #^B1111110011100111,r3
;;         asl r3
;;         asl r3
;;         asl r3
;;         mov r3,r4
;;        swab r4
;;         asl r4
;;         bis r3,r4
;;         bic #^B1111111111110000,r2
;;         call @#8$
;;         swab r2
;;         br 5$
.c3:     mov dx,[ds:bp+count0+2]
         and dx,1100011000b
         shr dl,1
         or dh,dl
         shl dh,4
         and al,0fh
         inc di
         jmp .c4

;;1$:      clrb @#crsrpgmk
;;         call @#showscnz
;;         incb @#crsrpgmk
;;         ;mov @#crsrtile,r0   ;do not remove! ???
;;         return

;;8$:      bisb r4,r2
;;         bic #^B1111111100000000,r2
;;         mov #tovideo,@#pageport
;;         movb vistabpc(r2),r2
;;         return

crsrcalc:
;;        mov @#crsrtile,r0
;;        mov video(r0),r0     ;start of coorditates calculation
        mov bx,[crsrtile]
        mov bx,[bx+video]
        push bx

;;        sub #videostart,r0
        sub bx,14h  ;20
        push bx
        
;;        asl r0
;;        asl r0
;;        mov r0,@#crsrx
        shl bx,1
        shl bx,1
        mov [crsrx],bl
        pop ax
        mov cl,40
        div cl
        mov [crsry],al        
        pop ax
;;        clr r1
;;        movb @#crsrbit,r2
;;10$:    aslb r2
;;        bcs 8$

;;        inc r1
;;        br 10$
        xor bh,bh
        xchg bl,bh
        mov cl,[crsrbit]
.c10:   shl cl,1
        jc .c8

        inc bl
        jmp .c10

;;8$:     add r1,r0
;;        clr r1
;;        cmpb r0,#100
;;        bcs 1$
.c8:    add bh,bl
        xor bl,bl
        cmp bh,100
        jc .c1
        
;;        inc r1
;;        sub #100,r0
        inc bl
        sub bh,100

;;1$:     movb r1,@#xcrsr
;;        clr r1
.c1:    mov [xcrsr],bl
        xor bl,bl

;;3$:     cmpb r0,#10
;;        bcs 2$
.c3:    cmp bh,10
        jc .c2

;;        inc r1
;;        sub #10,r0
;;        br 3$
        inc bl
        sub bh,10
        jmp .c3

.c2:    mov word [xcrsr+1],bx
        mov cl,80
        div cl
        shl al,1
        add al,[crsrbyte]

        mov [ycrsr],1
        sub al,100
        jnc .l1

        dec [ycrsr]
        add al,100
.l1:    xor ah,ah 
        mov cl,10
        div cl
        mov word [ycrsr+1],ax
        call xyout
        cmp [zoom],0
        jnz .c18
        retn

;;18$:    mov #up,r1
;;        movb @#vptilecy,r3
;;        add #8,r3
;;        movb @#vptilecy,r2
;;        bmi 33$
.c18:   mov di,up
        mov al,[vptilecy]
        mov ah,al
        add al,8
        or ah,ah
        js .c33

;;        mov #down,r1
;;        sub #16,r3
;;        cmpb r2,#24
;;        bcs 34$
        mov di,down
        sub al,16
        cmp ah,24
        jc .c34

;;33$:    movb r3,@#vptilecy
;;        br 31$
.c33:   mov [vptilecy],al
        jmp .c31

;;34$:    mov #left,r1
;;        movb @#vptilecx,r3
;;        add #8,r3
;;        movb @#vptilecx,r2
;;        bmi 35$
.c34:   mov di,left
        mov al,[vptilecx]
        mov ah,al
        add al,8
        or ah,ah
        js .c35
 
;;        mov #right,r1
;;        sub #16,r3
;;        cmpb r2,#40
;;        bcs 30$
        mov di,right
        sub al,16
        cmp ah,40
        jc .c30

;;35$:    movb r3,@#vptilecx
.c35:   mov [vptilecx],al

;;31$:    add @#viewport,r1
;;       mov @r1,r3
;;        mov r3,@#viewport
;;        mov dr(r3),r1
;;        mov dr(r1),r1
;;        mov right(r1),r1
;;        mov right(r1),r1
.c31:   add di,[viewport]
        mov bx,[di]
        mov [viewport],bx
        mov di,[bx+dr]
        mov di,[di+dr]
        mov di,[di+right]
        mov di,[di+right]

;;        add #44*tilesize,r3
;;        cmp r1,r3
;;        beq 30$
        add bx,44*tilesize
        cmp bx,di
        jz .c30

;;        call @#setviewport
;;30$:    jmp @#showscnz
        call setviewport
.c30:   call showscnz
        mov ah,2
        xor bh,bh
        mov dx,word [vptilecx]
        int 10h
        retn
        
outdec:  ;;clr r4            ;in: r3
;;         call @#todec
;;         mov #stringbuf+7,r1
;;         mov #3,r2
;;2$:      cmpb #'0,(r1)+
;;         bne 1$
;;         sob r2,2$

;;         inc r2
;;1$:      dec r1
;;         emt ^O20
;;         return

infov:   ;;call @#totext
;;         mov #146,r0
;;         emt ^O16
;;         mov #fn,r3
;;         tstb @r3
;;         beq 11$

;;         jsr r3,@#printstr
;;         .asciz "Last loaded filename: "
;;         .byte 0

;;1$:      movb (r3)+,r0
;;         emt ^O16
;;         cmpb #'.,@r3
;;         bne 1$

;;11$:     mov #todata,@#pageport
;;         call @#boxsz
;;         mov #toandos,@#pageport
;;         bis r5,r1  ;r5 = boxsz_ymax, this instruction is part of boxsz
;;         beq 12$

;;         jsr r3,@#printstr
;;         .byte 10
;;         .asciz "Active pattern size: "
;;         .byte 0

;;         push r3
;;         push r5
;;         mov r4,r3
;;         call @#outdec
;;         mov #'x,r0
;;         emt ^O16
;;         mov @#boxsz_cury,r3
;;         call @#outdec
;;         jsr r3,@#printstr
;;         .byte 10
;;         .ascii "Box life bounds:"
;;         .byte 10,32,0

;;         mov @#boxsz_xmin,r3
;;         call @#outdec
;;         jsr r3,@#printstr
;;         .asciz "<=x<="

;;         pop r3
;;         call @#outdec

;;         mov #32,r0
;;         emt ^O16
;;         mov @#boxsz_ymin,r3
;;         call @#outdec
;;         jsr r3,@#printstr
;;         .asciz "<=y<="
;;         pop r3
;;         call @#outdec
;;12$:     call @#getkey
;;         jmp @#tograph

;;chgcolors:
;;        movb @#palette,r0
;;        movb #'1,r1
;;        sub #10,r0
;;        bpl 14$

;;        movb #146,r1
;;        add #10,r0
;;14$:    add #'0,r0
;;        movb r1,@#88$
;;        movb r0,@#88$+1
;;        jsr r3,@#printstr
;;        .byte 154,0
;;2$:     jsr r3,@#printstr
;;        .byte 12,146
;;        .ascii "PRESS "
;;        .byte 145
;;        .ascii "ENTER"
;;        .byte 146
;;        .ascii " TO USE DEFAULT PALETTE OR INPUT DECIMAL NUMBER OF PALETTE ("
;;88$:    .ascii "  ): "
;;        .byte 147,0

;;3$:      mov #stringbuf,r2
;;         clr r1
;;1$:      call @#getkey
;;         cmpb #10,r0
;;         beq 11$

;;         cmpb #24,r0    ;backspace=zaboy
;;         beq 12$

;;         cmpb r0,#'0+10
;;         bcc 1$

;;         cmpb r0,#'0
;;         bcs 1$

;;         cmpb #2,r1
;;         beq 1$

;;         inc r1
;;         emt ^O16
;;         sub #'0,r0
;;         movb r0,(r2)+
;;         br 1$

;;12$:     dec r2
;;         dec r1
;;         bmi 3$

;;         jsr r3,@#printstr
;;         .byte 24,0
;;         br 1$

;;11$:      tst r1
;;          beq 20$

;;          dec r1
;;          beq 16$

;;          movb -(r2),r0
;;          movb -(r2),r1
;;          mov #10,r2
;;          cmpb r1,#1
;;          beq 4$
;;          bcc 2$

;;          clr r2
;;4$:       add r0,r2
;;          cmp r2,#16
;;          bcc 2$

;;24$:     movb r2,@#palette
;;         swab r2
;;         asl r2
;;         mov r2,@#kbddtport
;;20$:     jsr r3,@#printstr
;;         .byte 10
;;         .asciz "TO SAVE THIS CONFIG?"

;;8$:      call @#getkey
;;         bis #32,r0
;;         cmp r0,#'n
;;         beq 7$

;;         cmp r0,#'y
;;         bne 8$

;         jsr savecf
;;         mov #2,r2
;;         call @#iocf
;;7$:      jsr r3,@#printstr
;;         .byte 154,0

;;         return

;;16$:     movb -(r2),r2
;;         br 24$

putpixel2:
;;         mov video(r2),r2
;;         asl r4
;;         mov vistab(r4),r4
;;         swab r3
;;         asr r3
;;         asr r3
;;         add r3,r2
;;22$:     cmp r2,#16384
;;         bcs 22$

;;         cmp r2,#32768
;;         bcc 22$

;;         mov #tovideo,@#pageport
;;         bic r4,@r2
;;         asl r4
;;         bis r4,@r2
;;         jmp @#gexit3

showtent: ;;mov #toio,@#pageport
;;         mov #16384+8,r0
;;         mov @#loaded_sz,r5
;;         sub #8,r5
;;         cmp r5,#2560
;;         bcs 2$

;;         mov #2560,r5
;;2$:      asr r5

;         lda x0
;         pha
;         lda y0
;         pha
;         lda #0
;         sta $14
;         sta $15
;         sta ppmode
;;         push @#x0
;;         clrb @#ppmode

;loop     lda $15
;         cmp $b9
;         bne l1

;         ldx $14
;         cpx $b8
;         beq exit

;l1       eor #8
;         sta $15
;         ldx #0
;         lda ($14,x)
;         sta x0
;         lda $15
;         eor #4
;         sta $15
;         lda ($14,x)
;         sta y0
;         ora x0
;         beq l3
;;1$:      mov #toio,@#pageport
;;         mov (r0)+,@#x0

;         jsr putpixel
;;         mov #todata,@#pageport
;;         call @#putpixel

;l3       lda $15
;         eor #$c
;         sta $15
;         inc $14
;         bne loop
;
;         inc $15
;         bne loop
;;         sob r5,1$

;exit     pla
;         sta y0
;         pla
;         sta x0
;         inc ppmode
;         rts
;;         pop @#x0
;;         incb @#ppmode
;;         return

setpalette:
;;         mov #3,r2
;;         call @#iocf
;;         movb @#palette,r2
;;         swab r2
;;         asl r2
;;        mov r2,@#kbddtport   ;sets also active video page and enables raster interrupt
exit55:  ;;return

crsrclr2: ;;mov #135,@#crsrflash    ;135 = $87 = return
;;          tstb @#zoom
;;          bne exit55
          
;;          jmp @#crsrclr

crsrset2: ;;mov #135,@#crsrflash    ;135 = $87 = return
;;          jmp @#crsrset

showtopology:
;;         mov #27,r1
;;         mov #2,r2
;;         mov #msgtore,r3
;;         tstb @#topology
;;         beq showptxt
         call printstr
         db 27, '[12;33H$'
         mov dx,msgtore
         cmp [topology],0
         jz .l1

         mov dx,msgplain
.l1:     mov ah,9
         int 21h
         retn

;;         mov #msgplan,r3

showptxt:     ;IN: R1 - X, R2 - Y, R3 - msg
;;         mov #toandos,@#pageport
;;         emt ^O24
;;         mov r3,r1
spt1:    ;;clr r2
         ;;emt ^O20
         ;;jmp @#gexit3

showtxt:     ;IN: R1 - msg
;;         mov #toandos,@#pageport
;;         br spt1

shownum: ;;mov #stringbuf,r0
;;         mov r0,r2
;;         mov #stringbuf+10,r1
;;2$:      cmpb #'0,(r0)+
;;         bne 1$

;;         cmp r2,r0
;;         bne 2$

;;1$:      dec r0
;;5$:      cmpb #'0,-(r1)
;;         bne 4$

;;         cmp r0,r1
;;         bne 5$

;;4$:      cmp r0,#stringbuf+7
;;         bcs 3$

;;         mov r0,r5
;;         sub #stringbuf+7,r5
;;         sub r5,r0
;;8$:      movb #'.,(r2)+
;;7$:      movb (r0)+,(r2)+
;;         cmp r1,r0
;;         bcc 7$

;;         mov #stringbuf,r1
;;         sub r1,r2
;;         emt ^O20
;;         return

;;3$:      movb (r0)+,(r2)+
;;         cmp r0,#stringbuf+7
;;         beq 8$
;;         br 3$

printfloat: mov si,stringbuf
            xor cx,cx
            mov cl,[stringbuf]
            mov ah,2
            cmp cl,1
            jnz .l3

            mov dl,'.'
            int 21h
            mov dl,'0'
            int 21h
.l3:        add si,cx
.l2:        std
            lodsb
            cld
            cmp cl,ah
            jnz .l1

            mov dl,'.'
            push ax
            int 21h
            pop ax
.l1:        mov dl,al
            int 21h
            loop .l2
            retn
