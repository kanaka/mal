// HACK: it's hard to pull in util before data types

public class MalHashMap extends MalObject
{
    "hashmap" => type;


    fun string keyName(MalObject m)
    {
        if( m.type == "string" || m.type == "keyword" )
        {
            return m.stringValue;
        }
        else
        {
            cherr <= "User error (non-string/keyword key)\n";
            return "";
        }
    }

    fun void init(MalObject values[])
    {
        MalObject result[0];
        MalObject cachedKeys[0];
        MalObject cachedValues[0];
        string keys[0];

        for( 0 => int i; i < values.size(); 2 +=> i )
        {
            keyName(values[i]) => string key;

            if( cachedValues[key] == null )
            {
                keys << key;
            }

            values[i] @=> cachedKeys[key];
            values[i+1] @=> cachedValues[key];
        }

        for( 0 => int i; i < keys.size(); i++ )
        {
            keys[i] => string key;
            result << cachedKeys[key];
            result << cachedValues[key];
        }

        MalObject.toObjectArray(result) @=> objects;
    }

    fun static MalHashMap create(MalObject values[])
    {
        MalHashMap m;
        m.init(values);
        return m;
    }

    fun MalObject clone()
    {
        MalHashMap value;

        this.type => value.type;
        this.object @=> value.object;
        this.objects @=> value.objects;
        this.meta @=> value.meta;

        return value;
    }
}
