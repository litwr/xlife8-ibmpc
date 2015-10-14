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

boxsz:   mov byte [boxsz_ymin],192
         mov byte [boxsz_xmin],160
         xor cx,cx               ;dl=boxsz_ymax, ch=boxsz_xmax
         xor dx,dx
         mov [boxsz_curx],cx
         mov [boxsz_cury],cx
         mov si,tiles
         mov [tsz],cx         ;binary cell count
.c0:     mov cl,8
         xor bx,bx
         xor ax,ax
.c9:     mov bl,[si]
         inc si
         or ah,bl
         mov bl,[bx+tab3]
         add [tsz],bx    ;for save
         dec cl
         jnz .c9

         sub si,8
         or ah,ah
         je .c17

         mov cl,ah
         mov dh,0ffh
.c2:     inc dh
         shl cl,1
         jnc .c2

         mov al,[boxsz_curx]
         shl al,1
         shl al,1
         shl al,1
         mov bl,al
         add dh,al
         cmp dh,[boxsz_xmin]
         jnc .c12

         mov [boxsz_xmin],dh
.c12:    mov dh,8
.c3:     dec dh
         shr ah,1
         jnc .c3

         add dh,bl
         cmp dh,ch
         jc .c13

         mov ch,dh
.c13:    mov di,si
.c4:     lodsb
         or al,al
         je .c4

         sub si,di
         dec si
         mov al,[boxsz_cury]
         shl al,1
         shl al,1
         shl al,1
         mov bl,al
         add ax,si
         cmp al,[boxsz_ymin]
         jnc .c15

         mov [boxsz_ymin],al
.c15:    mov si,di
         add di,8
.c5:     dec di
         cmp [di],bh
         je .c5

         sub di,si
         add di,bx
         xor dh,dh
         cmp di,dx
         jc .c17

         mov dx,di
.c17:    add si,tilesize
         inc byte [boxsz_curx]
         cmp byte [boxsz_curx],hormax
         ;jne .c0   ;optimize 8088
         je .c8
.c01:    jmp .c0

.c8:     mov [boxsz_curx],bh
         inc byte [boxsz_cury]
         cmp byte [boxsz_cury],vermax
         jne .c01

.c7:     mov bl,dl
         sub bl,[boxsz_ymin]
         inc bx
         mov [boxsz_cury],bl
         mov al,ch
         sub al,[boxsz_xmin]
         inc ax       ;returns xsize in al
         mov [boxsz_curx],al
         mov ah,[tiles]
         or ah,cl
         retn

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
