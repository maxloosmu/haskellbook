public class captaintest
{
  public bool ReadFile (String file)
  {
    //find read a file.
    // Source: https://stackoverflow.com/a/27616117 .
    using (StreamReader sr = File.OpenText(fileName))
    {
            string s = sr.ReadToEnd();
    }
    //you then have to process the string

  }
}