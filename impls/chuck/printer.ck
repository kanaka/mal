public class Printer
{
    fun static string pr_str(MalObject m, int print_readably)
    {
        m.type => string type;

        if( type == "true" || type == "false" || type == "nil" )
        {
            return type;
        }
        else if( type == "int" )
        {
            return Std.itoa((m$MalInt).value());
        }
        else if( type == "string" )
        {
            (m$MalString).value() => string value;
            if( print_readably )
            {
                return String.repr(value);
            }
            else
            {
                return value;
            }
        }
        else if( type == "symbol" )
        {
            return (m$MalSymbol).value();
        }
        else if( type == "keyword" )
        {
            return ":" + (m$MalKeyword).value();
        }
        else if( type == "atom" )
        {
            return "(atom " + pr_str((m$MalAtom).value(), print_readably) + ")";
        }
        else if( type == "subr" )
        {
            return "#<Subr>";
        }
        else if( type == "func" )
        {
            return "#<Func>";
        }
        else if( type == "list" )
        {
            return pr_list((m$MalList).value(), print_readably, "(", ")");
        }
        else if( type == "vector" )
        {
            return pr_list((m$MalVector).value(), print_readably, "[", "]");
        }
        else if( type == "hashmap" )
        {
            return pr_list((m$MalHashMap).value(), print_readably, "{", "}");
        }
        else
        {
            return "Unknown type";
        }
    }

    fun static string pr_list(MalObject m[], int print_readably, string start, string end)
    {
        string parts[m.size()];

        for( 0 => int i; i < m.size(); i++ )
        {
            pr_str(m[i], print_readably) => parts[i];
        }

        return start + String.join(parts, " ") + end;
    }
}
