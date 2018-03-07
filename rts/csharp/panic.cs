static void panic(int exitcode, string str, params Object[] args)
{
    var progname = Environment.GetCommandLineArgs()[0];
    Console.Write(String.Format("{0}:", prog_name));
    Console.Write(String.Format(fmt, args));
    Environment.Exit(exitcode);
}
