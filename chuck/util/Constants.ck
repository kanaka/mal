public class Constants
{
    static MalTrue @ TRUE;
    static MalFalse @ FALSE;
    static MalNil @ NIL;
}

MalTrue.create() @=> Constants.TRUE;
MalFalse.create() @=> Constants.FALSE;
MalNil.create() @=> Constants.NIL;
