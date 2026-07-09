import java.util.*;
class hill
{
    public static void main(String args[])
    {
        Scanner sc=new Scanner(System.in);
        int ch=1;
        while(ch!=0)
        {
            System.out.println("****************************************");
            System.out.println("Enter choice:");
            System.out.println("0. Exit");
            System.out.println("1. Encrypt");
            System.out.println("****************************************");
            ch=sc.nextInt();
            switch(ch)
            {
                case 1:
                    System.out.print("Enter 3 letter plain text: ");
                    String plain=sc.next();
                    plain=plain.toUpperCase();
                    int plain1[]=new int[3];
                    for(int i=0;i<plain.length();i++)
                    {
                        plain1[i] = plain.charAt(i) - 'A';
                    }
                    System.out.print("Enter key matrix:(3x3 matrix ie. 9 values) ");
                    int[][] keyMatrix = new int[3][3];
                    for(int i=0;i<3;i++)
                    {
                        for(int j=0;j<3;j++)
                        {
                            keyMatrix[i][j] = sc.nextInt();
                        }
                    }
                    int cipher1[]=new int[3];
                    for(int i=0;i<3;i++)
                    {
                        cipher1[i] = 0;
                        for(int j=0;j<3;j++)
                        {
                            cipher1[i] += keyMatrix[i][j] * plain1[j];
                        }
                        cipher1[i] = cipher1[i] % 26;
                    }
                    String cipher = "";
                    for(int i=0;i<3;i++)
                    {
                        cipher += (char)(cipher1[i] + 'A');
                    }
                    System.out.println("Cipher Text: " + cipher);
                    break;
                case 0:
                    System.out.print("Exiting...");
                    break;
            }
        }
        sc.close();
    }
}