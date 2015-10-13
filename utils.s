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
         xor cx,cx               ;cl=boxsz_ymax, ch=boxsz_xmax
         mov [boxsz_curx],cx
         mov [boxsz_cury],cx
         mov si,[tiles]
         mov [tsz],cx         ;binary cell count
.c0:     push cx  ;ch
         mov cl,8
         xor dx,dx
         xor bx,bx
.c9:     mov bl,[si]
         inc si
         mov dh,[bx+tab3]
         add byte [tsz],dh    ;for save
         adc byte [tsz+1],bh
         or dl,bl
         loop .c9

         pop cx
         sub si,8
         or dl,dl
         je .c17

         push dx   ;dl
         mov dh,0ffh
.c2:     inc dh
         shl dl,1
         jnc .c2

         mov dl,[boxsz_curx]
         shl dl,1
         shl dl,1
         shl dl,1
         mov bl,dl
         add dh,dl
         cmp dh,[boxsz_xmin]
         jnc .c12

         mov [boxsz_xmin],dh
.c12:    pop dx
         mov dh,8
.c3:     dec dh
         shr dl,1
         jnc .c3

         add dh,bl
         cmp dh,ch
         jc .c13

         mov ch,dh
.c13:    xor dx,dx
         mov di,si
.c4:     lodsb
         or al,al
         je .c4

         sub si,di
         dec di
         mov dl,[boxsz_cury]
         shl dl,1
         shl dl,1
         shl dl,1
         mov bl,dl
         add dx,si
         cmp dl,[boxsz_ymin]
         jnc .c15

         mov [boxsz_ymin],dl
.c15:    mov si,di
         add di,8
.c5:     dec di
         cmp [di],dh
         je .c5

         sub di,si
         add di,bx
         push dx
         mov dx,di
         cmp dl,cl
         jc .c17

         mov cl,dl
.c17:    pop dx
         add si,tilesize
         inc byte [boxsz_curx]
         cmp byte [boxsz_curx],hormax
         jne .c0

         mov [boxsz_curx],bh
         inc byte [boxsz_cury]
         cmp byte [boxsz_cury],vermax
         jne .c0

         mov bl,cl
         sub bl,[boxsz_ymin]
         inc bx
         mov [boxsz_cury],bl
         mov al,ch
         sub al,[boxsz_xmin]
         inc ax       ;returns xsize in al
         mov [boxsz_curx],al
         mov [tiles],ah
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
