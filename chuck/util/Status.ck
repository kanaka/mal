public class Status
{
    static int SUCCESS;
    static int EMPTY_INPUT;
    static int UNEXPECTED_TERMINATOR;
    static int EXPECTED_TERMINATOR;
    static int SYMBOL_NOT_FOUND;
    static int OUT_OF_BOUNDS;

    static string status_codes[];

    fun static string toMessage(MalError m)
    {
        m.value() => int status_code;
        m.data => string data;

        if( status_code < status_codes.size() )
        {
            status_codes[status_code] => string message;
            // NOTE: for some reason, the string replacement API is
            // different from the regex one, so I'm using the latter
            RegEx.replace("%", data, message) => message;
            return message;
        }
        else
        {
            return "Undefined status code";
        }
    }
}

0 => Status.SUCCESS;
1 => Status.EMPTY_INPUT;
2 => Status.UNEXPECTED_TERMINATOR;
3 => Status.EXPECTED_TERMINATOR;
4 => Status.SYMBOL_NOT_FOUND;
5 => Status.OUT_OF_BOUNDS;

["success",
 "empty input",
 "unexpected '%'",
 "expected '%', got EOF",
 "'%' not found",
 "out of bounds"] @=> Status.status_codes;
