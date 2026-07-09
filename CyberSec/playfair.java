import java.util.*;

class playfair
{
    static char matrix[][] = new char[5][5];

    public static void generateMatrix(String key)
    {
        boolean used[] = new boolean[26];
        key = key.toUpperCase().replace("J", "I");

        int k = 0;

        for (int i = 0; i < key.length(); i++)
        {
            char ch = key.charAt(i);
            if (ch >= 'A' && ch <= 'Z' && !used[ch - 'A'])
            {
                used[ch - 'A'] = true;
                matrix[k / 5][k % 5] = ch;
                k++;
            }
        }

        for (char ch = 'A'; ch <= 'Z'; ch++)
        {
            if (ch == 'J')
                continue;
            if (!used[ch - 'A'])
            {
                matrix[k / 5][k % 5] = ch;
                k++;
            }
        }
    }

    public static int[] find(char ch)
    {
        if (ch == 'J')
            ch = 'I';

        for (int i = 0; i < 5; i++)
        {
            for (int j = 0; j < 5; j++)
            {
                if (matrix[i][j] == ch)
                    return new int[] { i, j };
            }
        }
        return null;
    }

    public static String prepare(String text)
    {
        text = text.toUpperCase().replace("J", "I");
        String s = "";

        for (int i = 0; i < text.length(); i++)
        {
            char ch = text.charAt(i);
            if (ch >= 'A' && ch <= 'Z')
                s += ch;
        }

        String result = "";

        for (int i = 0; i < s.length(); i++)
        {
            result += s.charAt(i);

            if (i + 1 < s.length() && s.charAt(i) == s.charAt(i + 1))
                result += 'X';
        }

        if (result.length() % 2 != 0)
            result += 'X';

        return result;
    }

    public static void encrypt(String plain)
    {
        plain = prepare(plain);

        System.out.println("Cypher Text: ");

        for (int i = 0; i < plain.length(); i += 2)
        {
            char a = plain.charAt(i);
            char b = plain.charAt(i + 1);

            int p1[] = find(a);
            int p2[] = find(b);

            if (p1[0] == p2[0])
            {
                System.out.print(matrix[p1[0]][(p1[1] + 1) % 5]);
                System.out.print(matrix[p2[0]][(p2[1] + 1) % 5]);
            }
            else if (p1[1] == p2[1])
            {
                System.out.print(matrix[(p1[0] + 1) % 5][p1[1]]);
                System.out.print(matrix[(p2[0] + 1) % 5][p2[1]]);
            }
            else
            {
                System.out.print(matrix[p1[0]][p2[1]]);
                System.out.print(matrix[p2[0]][p1[1]]);
            }
        }
        System.out.println();
    }

    public static void decrypt(String cypher)
    {
        cypher = cypher.toUpperCase();

        System.out.println("Plain Text: ");

        for (int i = 0; i < cypher.length(); i += 2)
        {
            char a = cypher.charAt(i);
            char b = cypher.charAt(i + 1);

            int p1[] = find(a);
            int p2[] = find(b);

            if (p1[0] == p2[0])
            {
                System.out.print(matrix[p1[0]][(p1[1] + 4) % 5]);
                System.out.print(matrix[p2[0]][(p2[1] + 4) % 5]);
            }
            else if (p1[1] == p2[1])
            {
                System.out.print(matrix[(p1[0] + 4) % 5][p1[1]]);
                System.out.print(matrix[(p2[0] + 4) % 5][p2[1]]);
            }
            else
            {
                System.out.print(matrix[p1[0]][p2[1]]);
                System.out.print(matrix[p2[0]][p1[1]]);
            }
        }
        System.out.println();
    }

    public static void main(String args[])
    {
        Scanner sc = new Scanner(System.in);

        System.out.print("Enter key: ");
        String key = sc.next();

        generateMatrix(key);

        int ch = 1;

        while (ch != 0)
        {
            System.out.println("****************************************");
            System.out.println("Enter choice:");
            System.out.println("0. Exit");
            System.out.println("1. Encrypt");
            System.out.println("2. Decrypt");
            System.out.println("****************************************");

            ch = sc.nextInt();

            switch (ch)
            {
                case 1:
                    System.out.print("Enter plain text: ");
                    String plain = sc.next();
                    encrypt(plain);
                    break;

                case 2:
                    System.out.print("Enter cypher text: ");
                    String cypher = sc.next();
                    decrypt(cypher);
                    break;

                case 0:
                    System.out.println("Exiting...");
                    break;
            }
        }

        sc.close();
    }
}