public class MalEqual extends MalSubr
{
    fun MalObject call(MalObject args[])
    {
        args[0] @=> MalObject a;
        args[1] @=> MalObject b;

        if( ( a.type == "list" || a.type == "vector" ) &&
            ( b.type == "list" || b.type == "vector" ) )
        {
            Util.sequenceToMalObjectArray(a) @=> MalObject as[];
            Util.sequenceToMalObjectArray(b) @=> MalObject bs[];

            if( as.size() != bs.size() )
            {
                return Constants.FALSE;
            }

            for( 0 => int i; i < as.size(); i++ )
            {
                call([as[i], bs[i]]) @=> MalObject value;
                if( value.type != "true" )
                {
                    return Constants.FALSE;
                }
            }

            return Constants.TRUE;
        }

        if( a.type != b.type )
        {
            return Constants.FALSE;
        }

        // NOTE: normally I'd go for a type variable, but its scope
        // isn't handled properly in the presence of a member variable
        a.type => string kind;
        if( kind == "true" || kind == "false" || kind == "nil" )
        {
            return Constants.TRUE;
        }
        else if( kind == "int" )
        {
            if( (a$MalInt).value() == (b$MalInt).value() )
            {
                return Constants.TRUE;
            }
            else
            {
                return Constants.FALSE;
            }
        }
        else if( kind == "string" )
        {
            if( (a$MalString).value() == (b$MalString).value() )
            {
                return Constants.TRUE;
            }
            else
            {
                return Constants.FALSE;
            }
        }
        else if( kind == "symbol" )
        {
            if( (a$MalSymbol).value() == (b$MalSymbol).value() )
            {
                return Constants.TRUE;
            }
            else
            {
                return Constants.FALSE;
            }
        }
        else if( kind == "keyword" )
        {
            if( (a$MalKeyword).value() == (b$MalKeyword).value() )
            {
                return Constants.TRUE;
            }
            else
            {
                return Constants.FALSE;
            }
        }

        // HACK: return false for everything unknown for now
        return Constants.FALSE;
    }
}
