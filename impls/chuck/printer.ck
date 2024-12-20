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
            return Std.itoa(m.intValue);
        }
        else if( type == "string" )
        {
            if( print_readably )
            {
                return String.repr(m.stringValue);
            }
            else
            {
                return m.stringValue;
            }
        }
        else if( type == "symbol" )
        {
            return m.stringValue;
        }
        else if( type == "keyword" )
        {
            return ":" + m.stringValue;
        }
        else if( type == "atom" )
        {
            return "(atom " + pr_str(m.malObjectValue(), print_readably) + ")";
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
            return pr_list(m.malObjectValues(), print_readably, "(", ")");
        }
        else if( type == "vector" )
        {
            return pr_list(m.malObjectValues(), print_readably, "[", "]");
        }
        else if( type == "hashmap" )
        {
            return pr_list(m.malObjectValues(), print_readably, "{", "}");
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
