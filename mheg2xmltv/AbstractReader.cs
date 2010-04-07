
using System;
using System.Diagnostics;
using System.IO;
using System.Threading;

namespace mheg2xmltv
{
  public abstract class AbstractReader : IDSMCCReader
  {
    private string rootPath;
    
    public Stream GetCarouselFile(string path)
    {
      Debug.WriteLine("Info: Getting file \"" + path + "\".");

      return new FileStream(Path.Combine(rootPath, path), FileMode.Open);
    }

    public string[] GetCarouselFiles(string path)
    {
      Debug.WriteLine("Info: Getting file list for \"" + path + "\".");

      return Directory.GetFiles(Path.Combine(rootPath, path));
    }

    public string[] GetCarouselDirectories(string path)
    {
      Debug.WriteLine("Info: Getting directories for \"" + path + "\".");

      return Directory.GetDirectories(Path.Combine(rootPath, path));
    }
    
    protected void setRootPath(string rootPath)
    {
      this.rootPath = rootPath;
    }
    
    protected abstract bool dumpFileExists();
    
        
    // Checks whether the channel information has been dumped by the reader. The time this takes
    // depends on the reader implementation
    protected void checkDumpFileExists()
    {
      Console.WriteLine("Grabbing epg information (note: grabbing will abort if epg info is not found within 5 minutes). Please wait");
      int threadSleepCount = 0;
      while( !dumpFileExists() )
      {
        Thread.Sleep(5000);
        Console.Write(".");
        
        // TODO A hack but will do for now
        if(threadSleepCount == 60)
        {
          Console.Error.WriteLine("Error: failed to get epg information because it took to long. Is your tuner in use?");
          
          throw new TimeoutException();
        }
        threadSleepCount++;
      }
    }
  }
}
