package com.koerriva;

public class Test {

    public static native void srand(long seek);
    public static native long clock();
    public static native int rand();

    public static int add(int a,int b){
        return a+b;
    }

    public static int random(int bound){
        srand(clock());
        return rand()%bound;
    }

    public static int main(){
        int a=0,b=add(random(100),random(100));
        return a+b+1;
    }
}