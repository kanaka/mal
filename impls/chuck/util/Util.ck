public class Util
{
    fun static void println(string message)
    {
        chout <= message + "\n";
    }

    fun static void panic(string message)
    {
        println("This shouldn't happen because: " + message);
        Machine.crash();
    }
}
