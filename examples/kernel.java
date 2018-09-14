public class Hal{
    public static native void poke(long address,byte data);
}

public class Kernel{
    public static void main(){
        for (int i=0;i<10;i++){
            Hal.poke(0xB8000 + 2*i, (byte) ('0' + i));
            Hal.poke(0xB8000 + 2*i + 1, (byte) 0x1F);
        }
    }
}