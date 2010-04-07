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
using DirectShowLib;
using DCDVBInterop;

namespace mheg2xmltv
{
  class DCDVBSourceReader : AbstractReader
  {
    private string mhpDataDirName = null;
    
    public DCDVBSourceReader(string dvbFile)
    {       
      Debug.WriteLine("Info: Grabbing data using DC-DVB Source");
      
      IGraphBuilder graph = (IGraphBuilder)new FilterGraph();
      DsError.ThrowExceptionForHR(graph.RenderFile(dvbFile, null));      

      IVideoWindow videoWindow = (IVideoWindow)graph;
      DsError.ThrowExceptionForHR(videoWindow.put_AutoShow(OABool.False));

      IEnumFilters filters;
      DsError.ThrowExceptionForHR(graph.EnumFilters(out filters));

      IBaseFilter[] filter = new IBaseFilter[1];
      IDVBSource dvbSource = null;
      while (filters.Next(1, filter, IntPtr.Zero) == 0)
      {
        dvbSource = filter[0] as IDVBSource;
        if (dvbSource != null)
          break;
      }

      IMediaControl control = (IMediaControl)graph;
      DsError.ThrowExceptionForHR(control.Run());
            
      dvbSource.put_ChannelSelected(0);
      
      // TODO get_MHPRoot returns null
      //string mhpRootDir = null;
      //dvbSource.get_MHPRoot(out mhpRootDir);      
      string mhpRootDir = Path.GetDirectoryName(dvbFile) + Path.DirectorySeparatorChar + "MHPData";
      
      string channelName = null;
      dvbSource.get_ChannelInfo(0, out channelName );
      
      mhpDataDirName = mhpRootDir + Path.DirectorySeparatorChar + channelName;
      
      if( Directory.Exists( mhpDataDirName ) )
      {
      	Directory.Delete( mhpDataDirName, true );
      }
      
      checkDumpFileExists();
            
      setRootPath( mhpDataDirName );

      DsError.ThrowExceptionForHR(control.StopWhenReady());
      DsError.ThrowExceptionForHR(control.Stop());

      Debug.WriteLine("Info: Finished grabbing data.");
    }
        
    protected override bool dumpFileExists()
    {
      return Directory.Exists(mhpDataDirName);
    }
  }
}