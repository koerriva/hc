#include <stdio.h>

extern int add(int,int);

void main(void){
    int r = add(100,1000);
    printf("r : %d\n",r);
}