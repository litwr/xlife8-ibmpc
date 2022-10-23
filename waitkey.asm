         org 100h
         use16

start:   xor ax,ax
         int 16h
         cmp al,'1'
         jb start

         cmp al,'5'
         ja start

         xor al,'0'
         mov ah,4ch
         int 21h
