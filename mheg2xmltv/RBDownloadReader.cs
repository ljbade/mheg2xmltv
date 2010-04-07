/*    
    xml2mheg - A XmlTv grabber for Freeview NZ MHEG-5 EPG data
    Copyright (C) 2008-2009 Leith Bade

    This program is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program.  If not, see <http://www.gnu.org/licenses/>.
*/

using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.IO;
using System.Text;
using System.Threading;

namespace mheg2xmltv
{
  class RBDownload : AbstractReader
  {
    private string dumpDir = null;
    
    public RBDownload(string path)
    {
      dumpDir = Environment.CurrentDirectory + Path.DirectorySeparatorChar + "services";
      
      prepareDumpDir(dumpDir);
      
      Debug.WriteLine("Info: Grabbing data using rb-download");
      Process process = new Process();
      process.StartInfo = new ProcessStartInfo("rb-download", "-f " + path);
      process.Start();

      checkDumpFileExists();

      process.Kill();
      process.Close();
      
      setRootPath(Directory.GetDirectories(dumpDir)[0]);

      Debug.WriteLine("Info: Finished grabbing data.");
    } 
        
    protected override bool dumpFileExists()
    {
      return Directory.GetDirectories(dumpDir).Length > 0;
    }
    
    // The dump dir is created where the input path file
    private void prepareDumpDir(string dumpDir)
    {
      if( !Directory.Exists( dumpDir ) )
      {
        Directory.CreateDirectory( dumpDir );
      }
      else
      { 
        // TODO find a way to get the root directory so we don't need to delete directories.
        string[] dumpDirectories = Directory.GetDirectories(dumpDir);
        foreach(string dirFound in dumpDirectories)
        {
          Directory.Delete(dirFound, true);
        }
        string[] allFilesFound = Directory.GetFiles(dumpDir);
        foreach(string fileFound in allFilesFound)
        {
          File.Delete(fileFound);
        }
      }
    }
  } 
  
}
