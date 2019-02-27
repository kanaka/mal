public class String
{
    string value;

    fun static String create(string value)
    {
        String s;
        value => s.value;
        return s;
    }

    // helpers

    // "x".substring(1) errors out (bug?), this doesn't
    fun static string slice(string input, int index)
    {
        if( index == input.length() )
        {
            return "";
        }
        else
        {
            return input.substring(index);
        }
    }

    fun static string slice(string input, int start, int end)
    {
        if( start == input.length() )
        {
            return "";
        }
        else
        {
            return input.substring(start, end - start);
        }
    }

    fun static string join(string parts[], string separator)
    {
        if( parts.size() == 0 )
        {
            return "";
        }

        parts[0] => string output;

        for( 1 => int i; i < parts.size(); i++ )
        {
            output + separator + parts[i] => output;
        }

        return output;
    }

    fun static string[] split(string input, string separator)
    {
        string output[0];

        if( input == "" )
        {
            return output;
        }

        0 => int offset;
        int index;

        while( true )
        {
            input.find(separator, offset) => index;

            if( index == -1 )
            {
                output << input.substring(offset);
                break;
            }

            output << input.substring(offset, index - offset);
            index + separator.length() => offset;
        }

        return output;
    }

    fun static string replaceAll(string input, string pat, string rep)
    {
        0 => int offset;
        input => string output;
        int index;

        while( true )
        {
            if( offset >= output.length() )
            {
                break;
            }

            output.find(pat, offset) => index;

            if( index == -1 )
            {
                break;
            }

            output.replace(index, pat.length(), rep);
            index + rep.length() => offset;
        }

        return output;
    }

    fun static string parse(string input)
    {
        slice(input, 1, input.length() - 1) => string output;
        replaceAll(output, "\\\\", "\177") => output;
        replaceAll(output, "\\\"", "\"") => output;
        replaceAll(output, "\\n", "\n") => output;
        replaceAll(output, "\177", "\\") => output;
        return output;
    }

    fun static string repr(string input)
    {
        input => string output;
        replaceAll(output, "\\", "\\\\") => output;
        replaceAll(output, "\n", "\\n") => output;
        replaceAll(output, "\"", "\\\"") => output;
        return "\"" + output + "\"";
    }
}
