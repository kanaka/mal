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
        else
        {
            return null;
        }
    }

    fun static string keyName(MalObject m)
    {
        if( m.type == "string" || m.type == "keyword" )
        {
            return ((m.object)$String).value;
        }
        else
        {
            return "this shouldn't happen";
        }
    }

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
