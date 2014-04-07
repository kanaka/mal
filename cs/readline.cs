using System;
using Mono.Terminal; // LineEditor (getline.cs)

namespace Mal {
    public class readline {
        public enum Mode { Terminal, Raw };
        public static Mode mode = Mode.Terminal;

        static LineEditor lineedit = null;

        public static string Readline(string prompt) {
            if (mode == Mode.Terminal) {
                if (lineedit == null) {
                    lineedit = new LineEditor("Mal");
                }
                return lineedit.Edit(prompt, "");
            } else {
                Console.Write(prompt);
                Console.Out.Flush();
                return Console.ReadLine();
            }
        }
    }
}
