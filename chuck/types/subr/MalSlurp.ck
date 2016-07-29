public class MalSlurp extends MalSubr
{
    fun MalObject call(MalObject args[])
    {
        (args[0]$MalString).value() => string filename;
        FileIO f;
        string output[0];

        f.open(filename, FileIO.READ);

        while( f.more() )
        {
            output << f.readLine();
        }

        f.close();

        return MalString.create(String.join(output, "\n"));
    }
}
