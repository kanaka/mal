public class MalTimeMs extends MalSubr
{
    fun MalObject call(MalObject args[])
    {
        // HACK: Std.system returns the status code only...
        "/tmp/chuck-date." + Std.rand2(1000,9999) => string temp_file;
        Std.system("date +%s%3N > " + temp_file);

        FileIO f;
        f.open(temp_file, FileIO.READ);
        f => int timestamp;
        f.close();

        Std.system("rm " + temp_file);

        return MalInt.create(timestamp);
    }
}
