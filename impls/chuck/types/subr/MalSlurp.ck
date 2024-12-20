public class MalSlurp extends MalSubr
{
    fun MalObject call(MalObject args[])
    {
        args[0].stringValue => string filename;
        FileIO f;
        string output[0];

        f.open(filename, FileIO.READ);

        while( f.more() )
        {
            output << f.readLine();
        }

        // HACK: not only do we assume files are joined by \n, but the
        // final newline cannot be detected otherwise
        String.join(output, "\n") => string content;
        if( f.size() == content.length() + 1 )
        {
           "\n"  +=> content;
        }

        f.close();
        return MalString.create(content);
    }
}
