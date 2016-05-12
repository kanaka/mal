public class Util
{
    fun static MalObject[] sequenceToMalObjectArray(MalObject m)
    {
        if( m.type == "list" )
        {
            return (m$MalList).value();
        }
        else if( m.type == "vector" )
        {
            return (m$MalVector).value();
        }
    }

    fun static void print(string message)
    {
        chout <= message;
    }

    fun static void println(string message)
    {
        chout <= message + "\n";
    }
}
