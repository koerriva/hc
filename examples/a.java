package com.koerriva;

public class Test {
    public static int add(int a,int b){
        return a+b;
    }

    public static int mul(int a,int b){
        return a*b;
    }

    public static int div(int a,int b){
        return a/b;
    }

    public static int main(){
        int a = mul(2,2);
        int r = add(1,a);
        return div(r,5);
    }
}