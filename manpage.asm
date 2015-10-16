         org 100h
         use16

start:   mov ax,3
         int 10h
         mov si,mandata
         mov ax,0b800h
         mov es,ax
         xor bp,bp
         mov ah,1
         mov cx,201fh
         int 10h
.loop:   lodsb
         cmp al,26
         jz .eof

         mov dl,al
         cmp al,10
         jnz .l3

         inc bp
         cmp bp,[lines]
         jnz .l2

.l1:     xor ah,ah
         int 16h
         xor bp,bp
         mov [lines],24
.l2:     mov ah,2
         int 21h
         jmp .loop

.l3:     mov ah,3
         xor bh,bh
         int 10h

         xchg al,dl
         cmp al,79
         jnz .l2

         inc bp
         cmp bp,[lines]
         jnz .l2

         mov dh,[es:3997]
         mov [es:3998],dx
         jmp .l1
         
.eof:    mov ah,1
         mov cx,607h
         int 10h
         int 20h
lines    dw 25

mandata: