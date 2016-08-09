public class Readline
{
    fun static string readline(string prompt)
    {
        int done;
        string input;
        KBHit kb;
        int char;
        string repr;

        ["NUL", "SOH", "STX", "ETX", "EOT", "ENQ", "ACK", "BEL",
         "BS", "HT", "LF", "VT", "FF", "CR", "SO", "SI",
         "DLE", "DC1", "DC2", "DC3", "DC4", "NAK", "SYN", "ETB",
         "CAN", "EM", "SUB", "ESC", "FS", "GS", "RS", "US",
         " ", "!", "\"", "#", "$", "%", "&", "'",
         "(", ")", "*", "+", ",", "-", ".", "/",
         "0", "1", "2", "3", "4", "5", "6", "7",
         "8", "9", ":", ";", "<", "=", ">", "?",
         "@", "A", "B", "C", "D", "E", "F", "G",
         "H", "I", "J", "K", "L", "M", "N", "O",
         "P", "Q", "R", "S", "T", "U", "V", "W",
         "X", "Y", "Z", "[", "\\", "]", "^", "_",
         "`", "a", "b", "c", "d", "e", "f", "g",
         "h", "i", "j", "k", "l", "m", "n", "o",
         "p", "q", "r", "s", "t", "u", "v", "w",
         "x", "y", "z", "{", "|", "}", "~", "DEL"] @=> string asciiTable[];

        chout <= prompt;
        chout.flush();

        while( !done )
        {
            kb => now;

            while( kb.more() && !done )
            {
                kb.getchar() => char;
                asciiTable[char] => repr;

                if( repr == "EOT" || repr == "LF" || repr == "CR" )
                {
                    true => done;
                }
                else if( repr == "DEL" && Std.getenv("TERM") != "dumb")
                {
                    if( input.length() > 0)
                    {
                        chout <= "\033[1D\033[0K";
                        chout.flush();
                        input.substring(0, input.length()-1) => input;
                    }
                }
                else
                {
                    chout <= repr;
                    chout.flush();
                    repr +=> input;
                }
            }
        }

        chout <= "\n";

        if( repr == "EOT" )
        {
            return null;
        }

        return input;
    }
}

