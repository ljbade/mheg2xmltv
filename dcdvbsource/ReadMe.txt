

   DirectShow DVB Source Filter Version 0.1.7

   (C) 2004-2006 Milenko "DCoder" Mitrovic

   Mail: dcoder@dsp-worx.de
   Web:  http://www.dsp-worx.de


   Table of Contest
   
     1) Introduction
     2) Features
     3) FAQ
     4) Licensing
     5) Third Party Librarys
     6) How to implement IStreamBufferMediaSeeking
     7) Tested Players


   Date: May, 21. 2006






1) --- Introduction --------------------------------------------------------------

  The DC-DVB Source is a DirectShow Source Filter for DVB/ATSC Graphs. Currently 
  DVB-T, DVB-S and DVB-C has been successfully tested. ATSC Playback isn't supported yet.
  The Filter uses a Plugins System to access the Hardware. Currently supported Devices 
  are all Devices using BDA Drivers and Twinhan Devices with WDM Drivers. Other Devices
  can be supported as well by creating a Plugin.

  The Filter is split into two parts. The first part, the Channel Scan, will scan 
  the BDA Device for all available Channels. In this Application it´s also needed to 
  specify what Video/Audio Decoders shall be used. In the Channel Editor you can 
  specify what Audio/Subtitle Stream should be used as "default" Stream when tuning
  to this Channel. Finally, the Settings can be saved to a .dvb File. This File will 
  be used later by the Source Filter to create the Graph. The Filter registers the 
  .dvb extension, so when loading the .dvb File in a MediaPlayer, DirectShow will 
  use the this DVB Source Filter automatically.

  The Channel selection is implemented on the Filter using IAMStreamSelect, so any 
  Player should be able to switch between Channels. There is also an internal 
  Interface present that offers more features. Take a look into IDVBSource.pas for
  more Informations.

  For Channel selection in Windows Media Player 10 select "Tools -> Plugins -> TV Channels".

  An additional Tool is the DeviceReset Application. It´s build to reset Hardware Device 
  from the Device Manager. BDA Cards that uses the Conexant 2388x Chipset stops working 
  when the Graph or Filter crashes one Time, which is caused by a Bug in the 
  Drivers (Version <= 4.0.108.2). This Utility will reset the Device, so that the 
  Graphbuilding will run again. It´s needed to specify the Hardware Device String in the 
  DeviceReset.bat Batch File to make this work.

  Note: This Release is an Alpha Version for testing. It is not ment to be used in 
  a productive environment. 


2) --- Features ------------------------------------------------------------------

- Timeshifting and recording to dvr-ms Files. A Recording can be any Channel on 
  the same Frequency/Transponder as the Channel you are watching.

- Channels can have a "default" Audio and Subtitle stream that will be mapped when 
  changing Channels. This is usefull when a Channel owns more than 1 Audio Stream 
  (like a secondary AC3 Stream) or someone wants to have Subtitles always displayed.

- Writing of the TS-Stream to the HardDisk.

- Implements a Server where clients can connect and receive the TS-Stream. I´ve 
  done this to be able to Stream the TS in my LAN and watch TV on my NoteBook(s).
  Different Channels can be watched as long as they are on the same frequency.
  The Server supports Multicast as well as TCP Streams.

- On Screen Display showing the current Channel, current and next Event.

- Full EPG parsing. The EPG is saved under the "EPGData" Directory where the .dvb 
  File is located. There is a Java Sourcecode available on the Download Page which
  shows the Format of the EPG File.

- MHP/MHEG parsing. The DSM-CC parser should be able to parse Java, Picture, HTML and 
  any other Files that are transmitted in the TS Stream. It is saved under the 
  "MHPData" Directory where the .dvb File is located. 
  (NOTE: Executing such Applications is somewhat complicated. Running MHP can be done 
  using the OpenMHP implementation though not all Applications may work. 
  I don't know about running MHEG Applications yet)

- Full Teletext decoding. The Teletext is displayed using a SourceStream Filter, which 
  itself is connected to a VMR9. Thus, the Teletext will be drawn onto the Video. Make 
  sure that you have selected VMR9 as default Renderer in your Player, otherwise Teletext
  won't work. Also supported is Teletext Transparency and double sizing of the upper
  or lower Region.

- Plugin API which can access the Graph and Transport Stream. So, modifying and reading 
  the Transport stream and mapping PID's on the Demultiplexer can now be controlled 
  through Plugins.

- Tuning Plugins, so a Device could be supported by simply creating a Plugin for it.

- Full decoding of Teletext and DVB Subtitles. 


3) --- FAQ -----------------------------------------------------------------------

Q: When starting the ChannelScan, it tells me "ERROR: Connecting Receiver to MPEG-2 
   Demultiplexer". 

A: This happens when another Software is using the Hardware. Close all Programs that 
   might use the Hardware. On Windows XP MCE, run the included 
   "Stop MCE Service.bat".

Q: When Time Shifting is enabled, the Filter does not get destroyed when closing the
   Graph.

A: Because the Stream Buffer Engine is holding a Referency on the Graph, it's needed
   to Stop the Graph before closing (releasing) it, so, Stop the Player before 
   closing  it.

Q: Are there any additional Parameters that can be set up?

A: Yes, there are. There is one Option to set the Buffersize of Packets when streaming
   over the LAN. This fixes an issue with VLC. You have to edit the .dvb with a Text
   Editor for this. In the <debug> section, add the parameter packetsize="32767" where
   32767 can be replaced by any other Value. A Value of <= 0 will stream the Data using
   the Full TS Buffer. 


4) --- Licensing -----------------------------------------------------------------

  This software is released together with the Source Code under the 

  GNU GENERAL PUBLIC LICENSE VERSION 2

  For more Information about this License read the attached "License.txt" File or
  enter the following Link into your Web Browser 

  http://www.gnu.org/copyleft/gpl.html


5) --- Third Party Librarys ------------------------------------------------------

  This Software uses the following third Party Librarys

- Fastmove Library from the Delphi Fastcode Project

- Delphi ZLib Conversion by Roberto Della Pasqua

- XML Parser and TrayIcon by the JEDI Visual Component Library

- Some Icons by fasticon.com


6) --- How to implement IStreamBufferMediaSeeking in the Player ------------------

IStreamBufferMediaSeeking is actually the same as IMediaSeeking, except that 
Position and Duration is handled different. 

To get the IStreamBufferMediaSeeking Interface, Enumerate all Filters in the Graph
and find the "Stream Buffer Source Filter" using CLSID_StreamBufferSource. If you 
found it, Query it for IStreamBufferMediaSeeking. If the Interface or the Filter
is not found, just use IMediaSeeking like you used it before.

The Min and Max position of the Players Trackbar must be setup using 
IStreamBufferMediaSeeking.GetAvailable(Earliest, Latest). 
Min = Earliest
Max = Latest

Important: The Min and Max position changes dynamically, so it´s needed to update 
Min/Max Position too and not only the current Position !!!

The Current Position must be retrieved by calling 
IStreamBufferMediaSeeking.GetCurrentPosition(Position). The Player has to take 
care that Position is between Min and Max of the Trackbar.

To set the new Position call IStreamBufferMediaSeeking.SetPositions() like you 
would do it for IMediaSeeking.

DON'T use IStreamBufferMediaSeeking.GetDuration() !!! GetDuration returns only the 
difference between Min and Max. Use the "Latest" Value from 
IStreamBufferMediaSeeking.GetAvailable(Earliest, Latest).


7) --- Tested Players ------------------------------------------------------------

- ZoomPlayer -> no Problems. Since Version 4.5 it runs even in customized Mode.

- MPC -> no Problems, but needs to be stopped before closing the Stream with 
  Stream Buffer Engine (Time Shifting) Enabled. Also Languages/EPG Events aren't
  displayed in the Popupmenu.
  EDIT: Newer Versions of MPC fails rendering the File.

- MPlayer 6.4 -> same as MPC

- DSPlayer -> no Problems. Needs at least Version 0.743 Alpha.

- WMP 9/10 -> Rendering succeeds. Channel Selection possible from the 
  "TV Channel" Window.

- The Core Media Player -> removes the DVB Source Filter, so no Channel 
  Selection possible. Will be changed in one of the next Releases.

- BSPlayer -> Not working at all. Removes the Source Filter.

