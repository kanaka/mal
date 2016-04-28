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

    fun static string parse(string input)
    {
        slice(input, 1, input.length() - 1) => string output;
        RegEx.replaceAll("\\\\\"", "\"", output) => output;
        RegEx.replaceAll("\\\\n", "\n", output) => output;
        RegEx.replaceAll("\\\\\\\\", "\\", output) => output;
        return output;
    }

    fun static string repr(string input)
    {
        input => string output;
        RegEx.replaceAll("\\\\", "\\\\", output) => output;
        RegEx.replaceAll("\n", "\\n", output) => output;
        RegEx.replaceAll("\"", "\\\"", output) => output;
        return "\"" + output + "\"";
    }
}
