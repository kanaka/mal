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

        if( a.type == "hashmap" && b.type == "hashmap" )
        {
            (a$MalHashMap).value() @=> MalObject akvs[];
            (b$MalHashMap).value() @=> MalObject bkvs[];

            if( akvs.size() != bkvs.size() )
            {
                return Constants.FALSE;
            }

            MalObject bmap[0];

            for( 0 => int i; i < bkvs.size(); 2 +=> i )
            {
                Util.keyName(bkvs[i]) => string keyName;
                bkvs[i+1] @=> bmap[keyName];
            }


            for( 0 => int i; i < akvs.size(); 2 +=> i )
            {
                akvs[i] @=> MalObject key;
                akvs[i+1] @=> MalObject value;
                Util.keyName(key) => string keyName;

                if( bmap[keyName] == null ||
                    call([value, bmap[keyName]]).type != "true" )
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
