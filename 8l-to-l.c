#include<stdio.h>
int main(int argc, char **argv) {
    unsigned char buf[65536];
    int sz, i, n = 6;

    if (argc >= 2) {
        puts("USAGE: 8l-to-l <INFILE >OUTFILE");
        puts("       8l-to-l -h");
        return 0;
    }
    sz = fread(buf, 1, 65536, stdin);
    if ((buf[0] + 256*buf[1])*2 == sz - 8) n = 8; //Amstrad CPC
    puts("#R");
    for (i = 8; i < sz; i += 2)
       printf("%d %d\n", buf[i], buf[i + 1]);
    return 0;
}
