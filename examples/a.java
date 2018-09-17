package com.koerriva;

public class Test {
    public static int add(int a,int b){
        return a+b;
    }

    public static int mul(int a,int b){
        return a*b;
    }

    public static int main(){
        add(1,mul(1,2));
    }
}