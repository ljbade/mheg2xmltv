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
    class DCDVBSourceReader : IDSMCCReader
    {
        string rootPath;

        public DCDVBSourceReader(string dvbFile)
        {
            Debug.WriteLine("Info: Grabbing data using DC-DVB Source");
            Console.WriteLine("Preparing to download data.");

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
            //dvbSource.get_MHPRoot(out rootPath);
            rootPath = Directory.GetDirectories(Path.GetDirectoryName(dvbFile) + Path.DirectorySeparatorChar + "MHPData")[0];

            Console.Write("Downloading data: 0% ");

            for (int n = 10; n <= 100; n += 10)
            {
                Thread.Sleep(1000);
                Console.Write(n + "% ");
            }

            Console.WriteLine(".");

            DsError.ThrowExceptionForHR(control.StopWhenReady());
            DsError.ThrowExceptionForHR(control.Stop());

            Debug.WriteLine("Info: Finished grabbing data.");
        }

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
    }
}
