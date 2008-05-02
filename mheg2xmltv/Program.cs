/*    
    xml2mheg - A XmlTv grabber for Freeview NZ MHEG-5 EPG data
    Copyright (C) 2008 Leith Bade

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
using System.IO;
using System.Text;
using System.Text.RegularExpressions;
using System.Xml;

namespace mheg2xmltv
{
    class Program
    {
        static void Main(string[] args)
        {
            try
            {
                if (args.Length == 0)
                {
                    Console.WriteLine("mheg2xml input output");
                    Console.WriteLine();
                    switch (Environment.OSVersion.Platform)
                    {
                        case PlatformID.Win32NT:
                        case PlatformID.Win32Windows:
                            Console.WriteLine("input = path to a DC-DVB Source .dvb file or a path to a DSM-CC dump");
                            break;

                        case PlatformID.Unix:
                            Console.WriteLine("input = path to a channels.conf file or a path to a DSM-CC dump");
                            break;

                        default:
                            Console.WriteLine("input = a path to a DSM-CC dump");
                            break;
                    }
                    Console.WriteLine("output = path to save XML-TV file");
                    return;
                }

                if (args.Length < 2)
                {
                    Console.Error.WriteLine("Error: Input and output not specified.");
                    Environment.Exit(1);
                }

                string inputFileName = args[0];
                string outputFileName = args[1];

                //Console.WriteLine("Info: Input = \"" + inputFileName + "\", output = \"" + outputFileName + "\".");

                WriteXml(outputFileName, ReadData(inputFileName));
            }
            catch (Exception e)
            {
                Console.Error.WriteLine("Error: An exception occured: " + e.Message);

                if (e.InnerException != null)
                    Console.Error.WriteLine("Error: Caused by exception: " + e.InnerException.Message);

                Environment.Exit(1);
            }
        }

        static TVChannel[] ReadData(string path)
        {
            //Console.WriteLine("Info: Reading data from \"" + path + "\".");

            List<TVChannel> channels = new List<TVChannel>();

            IDSMCCReader reader = null;

            if (Directory.Exists(path))
            {
                //Console.WriteLine("Info: Input is a directory so using file reader.");
                reader = new FileReader(path);
            }
            else
            {
                if (!File.Exists(path))
                {
                    Console.Error.WriteLine("Error: Input is not a file or a directory.");
                    Environment.Exit(1);
                }

                //Console.WriteLine("Info: Input is a file so selecting DSM-CC grabber from OS type.");

                switch (Environment.OSVersion.Platform)
                {
                    case PlatformID.Win32NT:
                    case PlatformID.Win32Windows:
                        //Console.WriteLine("Info: Running on Windows so using DC-DVB Source.");
                        reader = new DCDVBSourceReader(path);
                        break;

                    case PlatformID.Unix:
                        //Console.WriteLine("Info: Running on Linux so using rb-download.");
                        reader = new RBDownload(path);
                        break;

                    default:
                        //Console.WriteLine("Info: Running on an unkown OS o using file reader.");
                        reader = new FileReader(path);
                        break;
                }
            }

            //Console.WriteLine("Info: Getting list of dates.");

            string[] folders = reader.GetCarouselDirectories("epg" + Path.DirectorySeparatorChar + "data");

            foreach (string folder in folders)
            {
                //Console.WriteLine("Info: Getting list of channels for \"" + folder + "\".");

                string[] files = reader.GetCarouselFiles(folder);

                foreach (string file in files)
                {
                    //Console.WriteLine("Info: Getting list of programmes for \"" + file + "\".");

                    string data;

                    using (StreamReader dataFile = new StreamReader(reader.GetCarouselFile(file)))
                        data = dataFile.ReadToEnd();

                    TVChannel channel = ParseData(data, file);

                    channels.Add(channel);
                }
            }

            //Console.WriteLine("Info: Finished reading data.");

            return channels.ToArray();
        }

        static void WriteXml(string fileName, TVChannel[] channels)
        {
            //Console.WriteLine("Info: Writing XML data to \"" + fileName + "\".");

            Regex whitespace = new Regex(@"\s+");
            List<uint> pidList = new List<uint>();
            XmlWriterSettings settings = new XmlWriterSettings();

            settings.Indent = true;
            settings.NewLineOnAttributes = true;
            settings.Encoding = new UTF8Encoding(false); // Remove UTF-8 byte-order-mark

            //Console.WriteLine("Info: Creating file \"" + fileName + "\".");

            using (XmlWriter xmlFile = XmlWriter.Create(fileName, settings))
            {
                //Console.WriteLine("Info: Add tv tag.");

                xmlFile.WriteStartDocument();
                xmlFile.WriteStartElement("tv");
                xmlFile.WriteAttributeString("generator-info-name", System.Reflection.Assembly.GetExecutingAssembly().GetName().Name
                    + "/" + System.Reflection.Assembly.GetExecutingAssembly().GetName().Version.ToString());

                foreach (TVChannel channel in channels)
                {
                    if (pidList.BinarySearch(channel.pid) < 0)
                    {
                        //Console.WriteLine("Info: Add channel tag for " + channel.name +".");

                        xmlFile.WriteStartElement("channel");
                        xmlFile.WriteAttributeString("id", channel.pid.ToString() + ".dvb.guide");
                        xmlFile.WriteElementString("display-name", channel.name);
                        xmlFile.WriteEndElement();

                        pidList.Add(channel.pid);
                    }

                    foreach (TVProgramme programme in channel.programmes)
                    {
                        if (programme.title == null) // Skip empty programmes - why do they do this?
                            continue;

                        //Console.WriteLine("Info: Add programme tag for " + programme.title + ".");

                        xmlFile.WriteStartElement("programme");
                        xmlFile.WriteAttributeString("start", programme.startDateTime.ToString("yyyyMMddHHmmss zzz").Replace(":", "")); // Remove : from timezone (grrr)
                        xmlFile.WriteAttributeString("stop", programme.stopDateTime.ToString("yyyyMMddHHmmss zzz").Replace(":", "")); // Remove : from timezone (grrr)
                        xmlFile.WriteAttributeString("channel", channel.pid.ToString());

                        string[] titlePieces = programme.title.Split(new char[] { ':' }, StringSplitOptions.RemoveEmptyEntries); // Use ':' to indicate a subtitle
                        xmlFile.WriteElementString("title", whitespace.Replace(titlePieces[0].Trim(), " ")); // Need to check how this effect subtitles
                        if (titlePieces.Length > 1)
                            xmlFile.WriteElementString("sub-title", whitespace.Replace(titlePieces[1].Trim(), " "));

                        xmlFile.WriteElementString("desc", whitespace.Replace(programme.description, " ")); // Remove whitespace

                        foreach (string icon in programme.icons)
                        {
                            switch (icon)
                            {
                                case "/pngs/ao.png":
                                    xmlFile.WriteStartElement("rating");
                                    xmlFile.WriteElementString("value", "AO"); // add rating system and icon?
                                    xmlFile.WriteEndElement();
                                    break;

                                case "/pngs/pgr.png":
                                    xmlFile.WriteStartElement("rating");
                                    xmlFile.WriteElementString("value", "PGR");
                                    xmlFile.WriteEndElement();
                                    break;

                                case "/pngs/g.png":
                                    xmlFile.WriteStartElement("rating");
                                    xmlFile.WriteElementString("value", "G");
                                    xmlFile.WriteEndElement();
                                    break;

                                case "/pngs/ear.png":
                                    xmlFile.WriteElementString("subtitles", null); // Add teletext type
                                    break;

                                case "/pngs/hd.png":
                                    xmlFile.WriteStartElement("video");
                                    xmlFile.WriteElementString("quality", "HDTV");
                                    xmlFile.WriteEndElement();
                                    break;
                            }
                        }

                        xmlFile.WriteEndElement();
                    }
                }

                xmlFile.WriteEndElement();
                xmlFile.WriteEndDocument();
                xmlFile.Flush();
            }

            //Console.WriteLine("Info: Finished writing XML data.");
        }

        static TVChannel ParseData(string data, string path)
        {
            //Console.WriteLine("Info: Parsing data for \"" + path  + "\".");

            int dataIndex = 0, entryIndex = 0;
            string entry = String.Empty;

            // Data format:
            // One or more entries split with ASCII file seperator 0x1C
            // One or values in an entry split with ASCII group seperator 0x1D
            // First entry decribes the TV channel
            // Rest describe TV programs on that channel

            // Get  TV channel information
            TVChannel channel;

            entry = NextEntry(data, ref dataIndex);

            channel.reserved = Convert.ToUInt32(NextValue(entry, ref entryIndex)); // Unsure what this is
            channel.friendlyDate = NextValue(entry, ref entryIndex);
            channel.name = NextValue(entry, ref entryIndex);
            channel.programCount = Convert.ToUInt32(NextValue(entry, ref entryIndex));
            
            channel.pid = Convert.ToUInt32(Path.GetFileName(path));

            // Get TV programme information
            channel.programmes = new TVProgramme[channel.programCount];

            //Console.WriteLine("Info: Found channel " + channel.name + ".");

            for (int programIndex = 0; programIndex < channel.programCount; programIndex++)
            {
                TVProgramme programme;

                entry = NextEntry(data, ref dataIndex);
                entryIndex = 0;

                programme.index = Convert.ToUInt32(NextValue(entry, ref entryIndex));
                programme.startTimeSeconds = Convert.ToUInt32(NextValue(entry, ref entryIndex));
                programme.stopTimeSeconds = Convert.ToUInt32(NextValue(entry, ref entryIndex));
                programme.titleLineCount = Convert.ToUInt32(NextValue(entry, ref entryIndex));
                programme.friendlyTime = NextValue(entry, ref entryIndex);
                programme.title = NextValue(entry, ref entryIndex);
                programme.description = NextValue(entry, ref entryIndex);
                programme.iconCount = Convert.ToUInt32(NextValue(entry, ref entryIndex));

                programme.icons = new string[programme.iconCount];

                for (int iconIndex = 0; iconIndex < programme.iconCount; iconIndex++)
                    programme.icons[iconIndex] = NextValue(entry, ref entryIndex);

                string folderName = Path.GetDirectoryName(path).Substring(Path.GetDirectoryName(path).LastIndexOf(Path.DirectorySeparatorChar) + 1);

                programme.startDateTime = DateTime.ParseExact(folderName, "yyyyMMdd", null);
                programme.startDateTime = programme.startDateTime.AddSeconds(programme.startTimeSeconds);

                programme.stopDateTime = DateTime.ParseExact(folderName, "yyyyMMdd", null);
                programme.stopDateTime = programme.stopDateTime.AddSeconds(programme.stopTimeSeconds);

                channel.programmes[programIndex] = programme;

                //Console.WriteLine("Info: Found programme " + programme.title + ".");
            }

            //Console.WriteLine("Info: Finished parsing data.");

            return channel;
        }

        static string NextEntry(string data, ref int index)
        {
            //Console.WriteLine("Info: Getting entry at " + index + ".");

            string remaining = data.Substring(index);
            int length = remaining.IndexOf('\x1C');
            if (length == -1)
                length = remaining.Length;
            if (length == 0)
                return null;
            string entry = remaining.Substring(0, length);
            index += length + 1; // Skip seperator
            return entry;
        }

        static string NextValue(string entry, ref int index)
        {
            //Console.WriteLine("Info: Getting value at " + index + ".");

            string remaining = entry.Substring(index);
            int length = remaining.IndexOf('\x1D');
            if (length == -1)
                length = remaining.Length;
            if (length == 0)
                return null;
            string value = remaining.Substring(0, length);
            index += length + 1; // Skip seperator
            return value;
        }
    }

    struct TVChannel
    {
        public uint reserved;
        public string friendlyDate;
        public string name;
        public uint programCount;
        public TVProgramme[] programmes;
        public uint pid;
    }

    struct TVProgramme
    {
        public uint index;
        public uint startTimeSeconds;
        public uint stopTimeSeconds;
        public uint titleLineCount;
        public string friendlyTime;
        public string title;
        public string description;
        public uint iconCount;
        public string[] icons;
        public DateTime startDateTime;
        public DateTime stopDateTime;
    }
}
