/*    
    OpenMheg - A free MHEG-5 library
    Copyright (C) 2008 Dharmesh Malam

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
using System.Text;
using System.Runtime.InteropServices;
using System.Runtime.CompilerServices;

namespace DCDVBInterop
{
    
    public enum  TVideoType {
    vtMPEG2,
    vtAVC
    }

  public enum TAudioType  {
    atMPEG1Audio,
    atMPEG2Audio,
    atAC3,
    atDTS,
    atAAC
  }

  public enum TAspectRatio  {
    ar4_3,
    ar16_9,
    ar221_1
  }

  public enum TTeletextSizeMode {
    tsmNormal,
    tsmDoubleUpper,
    tsmDoubleLower
  }

  //TEventDate = record
  //  Year: Integer;
  //  Month: Integer;
  //  Day: Integer;
  //  Hour: Integer;
  //  Minute: Integer;
  //end;

  public enum TDVBRecordingStatus {
    rsWaiting,
    rsRecording,
    rsStopped,
    rsFailed,
    rsInvalid
  }

  //TDVBRecordingSetting = record
  //  ID: TGuid;
  //  StartTime: TEventDate;
  //  EndTime: TEventDate;
  //  Name: WideString;
  //  Location: WideString;
  //  ChannelIndex: Integer;
  //  Status: TDVBRecordingStatus;
  //end;
  //PDVBRecordingSetting = ^TDVBRecordingSetting;

  public enum TSaveTeletextType {
    ttBitmap,
    ttTXT,
    ttVTX
  }

  public enum TFrameRate{
    frForbidden,
    fr23_976,
    fr24,
    fr25,
    fr29_97,
    fr30,
    fr50,
    fr59_94,
    fr60
  }

  public enum TTeletextFastext {
    tfeRed,
    tfeGreen,
    tfeYellow,
    tfeBlue,
    tfeReserved,
    tfeInfo
  }

  //    PDSStream = ^TDSStream;
  //TDSStream = record
  //  Size,
  //  Frequency,
  //  Channels,
  //  Bits : integer;
  //  Float : BOOL;
  //  SPDIF : BOOL;
  //  DTS : BOOL;
  //end;
  //      [StructLayout(LayoutKind.Sequential, Pack=1)]
  //  public struct DSStream
  //  {
  //      public int Size;
  //      public int Frequency;
  //      public int Channels;
  //      public int Bits;
  //      [MarshalAs(UnmanagedType.Bool)]
  //      public bool Float;
  //      [MarshalAs(UnmanagedType.Bool)]
  //      public bool SPDIF;
  //      [MarshalAs(UnmanagedType.Bool)]
  //      public bool DTS;
  //  }

  //      [StructLayout(LayoutKind.Sequential, Pack=1)]
  //  public struct StreamInfo
  //      {
  //          public long TotalTSBytes;
  //          [MarshalAs(UnmanagedType.Bool)]
  //          public bool VideoPresent;
  //          public int VideoPID;
  //          public VideoType 


  //      }


  //TStreamInfo = record
  //  TotalTSBytes: Int64;
  //  VideoPresent: Boolean;
  //  VideoPID: Integer;
  //  VideoType: TVideoType;
  //  VideoWidth: Integer;
  //  VideoHeight: Integer;
  //  VideoAspectRatio: TAspectRatio;
  //  VideoBitRate: Integer;
  //  VideoFrameRate: TFrameRate;
  //  VideoTotalBytes: Int64;
  //  DSMCCTotalBytes: Int64;
  //  AudioPresent: Boolean;
  //  AudioPID: Integer;
  //  AudioType: TAudioType;
  //  AudioChannels: Integer;
  //  AudioSamplerate: Integer;
  //  AudioBitrate: Integer;
  //  AudioTotalBytes: Int64;
  //end;
  //PStreamInfo = ^TStreamInfo;

       
    
    [ComImport, Guid( "8FA96AC0-D500-4939-B022-D3820B400199" ), InterfaceType( ComInterfaceType.InterfaceIsIUnknown )]
    public interface IDVBSource
    {


        //function get_Version(out Version: Cardinal): HRESULT; stdcall;
        [PreserveSig, MethodImpl( MethodImplOptions.InternalCall, MethodCodeType=MethodCodeType.Runtime )]
        void get_Version ( [Out] out uint Version );

        //function get_SignalStatistics(out Strength: Integer; out Quality: Integer; out SignalPresent: LongBool; out SignalLocked: LongBool): HRESULT; stdcall;
        [PreserveSig, MethodImpl( MethodImplOptions.InternalCall, MethodCodeType=MethodCodeType.Runtime )]
        void get_SignalStatistics ( [Out] out int Strength, [Out] out int Quality, [Out, MarshalAs( UnmanagedType.Bool )] out bool SignalPresent, [Out, MarshalAs( UnmanagedType.Bool )] out bool SignalLocked );



        #region Channels

        //function get_ChannelCount(out ChannelCount: Integer): HRESULT; stdcall;
        [PreserveSig, MethodImpl( MethodImplOptions.InternalCall, MethodCodeType=MethodCodeType.Runtime )]
        void get_ChannelCount ( [Out] out int ChannelCount );

        //function get_ChannelInfo(Index: Integer; out Name: PChar): HRESULT; stdcall;
        [PreserveSig, MethodImpl( MethodImplOptions.InternalCall, MethodCodeType=MethodCodeType.Runtime )]
        void get_ChannelInfo ( [In] int Index, [Out, MarshalAs( UnmanagedType.LPStr )] out string Name );

        //function get_ChannelSelected(out Index: Integer): HRESULT; stdcall;
        [PreserveSig, MethodImpl( MethodImplOptions.InternalCall, MethodCodeType=MethodCodeType.Runtime )]
        void get_ChannelSelected ( [Out] out int Index );

        //function put_ChannelSelected(Index: Integer): HRESULT; stdcall;
        [PreserveSig, MethodImpl( MethodImplOptions.InternalCall, MethodCodeType=MethodCodeType.Runtime )]
        void put_ChannelSelected ( [In] int Index );

        //function put_PreviousChannel: HRESULT; stdcall;
        [PreserveSig, MethodImpl( MethodImplOptions.InternalCall, MethodCodeType=MethodCodeType.Runtime )]
        void put_PreviousChannel ();

        //function put_NextChannel: HRESULT; stdcall;
        [PreserveSig, MethodImpl( MethodImplOptions.InternalCall, MethodCodeType=MethodCodeType.Runtime )]
        void put_NextChannel ();

        #endregion

        #region Audio Streams
        //function get_AudioStreamCount(out CountStreams: Integer): HRESULT; stdcall;
        [PreserveSig, MethodImpl( MethodImplOptions.InternalCall, MethodCodeType=MethodCodeType.Runtime )]
        void get_AudioStreamCount ( [Out] out int CountStreams );

        //function get_AudioStreamInfo(Index: Integer; out Name: PChar): HRESULT; stdcall;
        [PreserveSig, MethodImpl( MethodImplOptions.InternalCall, MethodCodeType=MethodCodeType.Runtime )]
        void get_AudioStreamInfo ( [In] int Index, [Out, MarshalAs( UnmanagedType.LPStr )] out string Name );

        //function get_AudioStreamSelected(out Index: Integer): HRESULT; stdcall;
        [PreserveSig, MethodImpl( MethodImplOptions.InternalCall, MethodCodeType=MethodCodeType.Runtime )]
        void get_AudioStreamSelected ( [Out] out int Index );

        //function put_AudioStreamSelected(Index: Integer): HRESULT; stdcall;
        [PreserveSig, MethodImpl( MethodImplOptions.InternalCall, MethodCodeType=MethodCodeType.Runtime )]
        void put_AudioStreamSelected ( [In] int Index );
        #endregion

        #region Teletext
        //function put_TeletextShow(Show: LongBool): HRESULT; stdcall;
        [PreserveSig, MethodImpl( MethodImplOptions.InternalCall, MethodCodeType=MethodCodeType.Runtime )]
        void put_TeletextShow([In,MarshalAs(UnmanagedType.Bool)] bool Show);

        //function get_TeletextShow(out Show: LongBool): HRESULT; stdcall;
        [PreserveSig, MethodImpl( MethodImplOptions.InternalCall, MethodCodeType=MethodCodeType.Runtime )]
        void get_TeletextShow ( [Out, MarshalAs( UnmanagedType.Bool )] out bool Show );

        //function put_TeletextTransparent(Transparent: LongBool): HRESULT; stdcall;
        [PreserveSig, MethodImpl( MethodImplOptions.InternalCall, MethodCodeType=MethodCodeType.Runtime )]
        void put_TeletextTransparent ( [In, MarshalAs( UnmanagedType.Bool )] bool Transparent );

        //function get_TeletextTransparent(out Transparent: LongBool): HRESULT; stdcall;
        [PreserveSig, MethodImpl( MethodImplOptions.InternalCall, MethodCodeType=MethodCodeType.Runtime )]
        void get_TeletextTransparent ( [Out, MarshalAs( UnmanagedType.Bool )] out bool Transparent );

        //function put_TeletextPage(Page: Integer; SubPage: Integer): HRESULT; stdcall;
        [PreserveSig, MethodImpl( MethodImplOptions.InternalCall, MethodCodeType=MethodCodeType.Runtime )]
        void put_TeletextPage ( [In] int Page, [In] int SubPage );

        //function get_TeletextPage(out Page: Integer; out SubPage: Integer): HRESULT; stdcall;
        [PreserveSig, MethodImpl( MethodImplOptions.InternalCall, MethodCodeType=MethodCodeType.Runtime )]
        void get_TeletextPage ( [Out] out int Page, [Out] out int SubPage );

        //function put_TeletextNumber(Number: Integer): HRESULT; stdcall;
        [PreserveSig, MethodImpl( MethodImplOptions.InternalCall, MethodCodeType=MethodCodeType.Runtime )]
        void put_TeletextNumber ( [In] int Number );

        //function put_SaveTeletext(AType: TSaveTeletextType; AFilename: PWideChar): HRESULT; stdcall;
        [PreserveSig, MethodImpl( MethodImplOptions.InternalCall, MethodCodeType=MethodCodeType.Runtime )]
        void put_SaveTeletext ( [In] TSaveTeletextType AType, [In,MarshalAs(UnmanagedType.LPStr)] string AFilename );

        #endregion

        #region EPG
        //function put_EPGClearAll: HRESULT; stdcall;
        [PreserveSig, MethodImpl( MethodImplOptions.InternalCall, MethodCodeType=MethodCodeType.Runtime )]
        void putEPGClearAll ();

        //function get_EPG(ChannelIndex: Integer; out EPG: PByte; out Size: Integer): HRESULT; stdcall;
        [PreserveSig, MethodImpl( MethodImplOptions.InternalCall, MethodCodeType=MethodCodeType.Runtime )]
        void get_EPG ( [In] int ChannelIndex, [Out] out byte EPG, [Out] out int Size );

        //function get_EPGTimeOffset(out TimeOffset: Integer): HRESULT; stdcall;
        [PreserveSig, MethodImpl( MethodImplOptions.InternalCall, MethodCodeType=MethodCodeType.Runtime )]
        void get_EPGTimeOffset ( [Out] out int TimeOffset );

        #endregion

        #region MHP
        //function get_MHPRoot(out MHP: PChar): HRESULT; stdcall;
        [PreserveSig, MethodImpl( MethodImplOptions.InternalCall, MethodCodeType=MethodCodeType.Runtime )]
        void get_MHPRoot ( [Out, MarshalAs( UnmanagedType.LPStr )] out string MHP );
        #endregion

        #region Recordings
        //function get_RecordingsCount(out Count: Integer): HRESULT; stdcall;
        [PreserveSig, MethodImpl( MethodImplOptions.InternalCall, MethodCodeType=MethodCodeType.Runtime )]
        void get_RecordingsCount ( [Out] out int Count );

        //function get_Recording(Index: Integer; out Recording: PDVBRecordingSetting): HRESULT; stdcall;
        [PreserveSig, MethodImpl( MethodImplOptions.InternalCall, MethodCodeType=MethodCodeType.Runtime )]
        void get_Recording ( [In] int Index );

        //function put_Recording(Recording: PDVBRecordingSetting): HRESULT; stdcall;
        [PreserveSig, MethodImpl( MethodImplOptions.InternalCall, MethodCodeType=MethodCodeType.Runtime )]
        void put_Recording ( [In] int Index );

        //function put_EditRecording(Recording: PDVBRecordingSetting): HRESULT; stdcall;
        [PreserveSig, MethodImpl( MethodImplOptions.InternalCall, MethodCodeType=MethodCodeType.Runtime )]
        void put_EditRecording ( [In] int Index );

        //function put_DeleteRecording(Recording: PDVBRecordingSetting): HRESULT; stdcall;
        [PreserveSig, MethodImpl( MethodImplOptions.InternalCall, MethodCodeType=MethodCodeType.Runtime )]
        void put_DeleteRecording ( [In] int Index );

        #endregion

        #region Subtitle
        //function get_SubtitleStreamCount(out CountStreams: Integer): HRESULT; stdcall;
        [PreserveSig, MethodImpl( MethodImplOptions.InternalCall, MethodCodeType=MethodCodeType.Runtime )]
        void get_SubtitlesStreamCount([Out] out int CountStreams);
        
        //function get_SubtitleStreamInfo(Index: Integer; out Name: PChar): HRESULT; stdcall;
        [PreserveSig, MethodImpl( MethodImplOptions.InternalCall, MethodCodeType=MethodCodeType.Runtime )]
        void get_SubtitlesStreamInfo([In] int Integer, [Out,MarshalAs(UnmanagedType.LPStr)] out string Name);

        //function get_SubtitleStreamSelected(out Index: Integer): HRESULT; stdcall;
        [PreserveSig, MethodImpl( MethodImplOptions.InternalCall, MethodCodeType=MethodCodeType.Runtime )]
        void get_SubtitlesStreamSelected([Out] out int Index);

        //function put_SubtitleStreamSelected(Index: Integer): HRESULT; stdcall;
        [PreserveSig, MethodImpl( MethodImplOptions.InternalCall, MethodCodeType=MethodCodeType.Runtime )]
        void put_SubtitlesStreamCount([In] int CountStreams);

        #endregion

        //function ShowOSD: HRESULT; stdcall;
        [PreserveSig, MethodImpl( MethodImplOptions.InternalCall, MethodCodeType=MethodCodeType.Runtime )]
        void ShowOSD ();

        //function get_StreamInfo(out AStreamInfo: TStreamInfo): HRESULT; stdcall;
        [PreserveSig, MethodImpl(MethodImplOptions.InternalCall, MethodCodeType = MethodCodeType.Runtime)]
        void get_StreamInfo();

        //function put_TeletextSizeMode(ASizeMode: TTeletextSizeMode): HRESULT; stdcall;
        [PreserveSig, MethodImpl(MethodImplOptions.InternalCall, MethodCodeType = MethodCodeType.Runtime)]
        void put_TeletextSizeMode();

        //function get_TeletextSizeMode(out ASizeMode: TTeletextSizeMode): HRESULT; stdcall;
        [PreserveSig, MethodImpl(MethodImplOptions.InternalCall, MethodCodeType = MethodCodeType.Runtime)]
        void get_TeletextSizeMode();

        //function get_PluginsCount(out ACount: Integer): HRESULT; stdcall;
        [PreserveSig, MethodImpl(MethodImplOptions.InternalCall, MethodCodeType = MethodCodeType.Runtime)]
        void get_PluginsCount([Out] out int ACount);

        //function put_PluginEnabled(AIndex: Integer; AEnabled: Boolean): HRESULT; stdcall;
        [PreserveSig, MethodImpl(MethodImplOptions.InternalCall, MethodCodeType = MethodCodeType.Runtime)]
        void put_PluginEnabled([In] int AIndex, [In,MarshalAs(UnmanagedType.Bool)] bool AEnabled);

        //function get_PluginEnabled(AIndex: Integer; out AEnabled: Boolean): HRESULT; stdcall;
        [PreserveSig, MethodImpl(MethodImplOptions.InternalCall, MethodCodeType = MethodCodeType.Runtime)]
        void get_PluginEnabled([Out] out int AIndex, [Out,MarshalAs(UnmanagedType.Bool)] out bool AEnabled);

        //function get_Plugin(AIndex: Integer; out APlugin: IUnknown): HRESULT; stdcall;
        [PreserveSig, MethodImpl(MethodImplOptions.InternalCall, MethodCodeType = MethodCodeType.Runtime)]
        void get_Plugin([In] int AIndex);

        //function put_TeletextFastext(AFastext: TTeletextFastext): HRESULT; stdcall;
        [PreserveSig, MethodImpl(MethodImplOptions.InternalCall, MethodCodeType = MethodCodeType.Runtime)]
        void put_TeletextFastext([In] TTeletextFastext AFastext);

        //function put_CaptureVideoWindowCursor(ACapture: Boolean): HRESULT; stdcall;
        [PreserveSig, MethodImpl(MethodImplOptions.InternalCall, MethodCodeType = MethodCodeType.Runtime)]
        void put_CaptureVideoWindowCursor([In,MarshalAs(UnmanagedType.Bool)] bool ACapture);



    }

    [ComImport, Guid( "3703EF76-94CF-41D7-B050-085E63C93562" ), InterfaceType( ComInterfaceType.InterfaceIsIUnknown )]
       public interface IAMStreamSelectInfo {

    //function get_Name(out Name: PWideChar): HRESULT; stdcall;
    //function get_Now(out Event: PWideChar; out Start: TEventDate; out End_: TEventDate): HRESULT; stdcall;
    //function get_Next(out Event: PWideChar; out Start: TEventDate; out End_: TEventDate): HRESULT; stdcall;
    }


    [ComImport, Guid( "25B7068D-83CE-488B-87EC-9B7DBD4FA447" ), InterfaceType( ComInterfaceType.InterfaceIsIUnknown )]

  public interface IDemuxControl 
    {    
    //function put_VideoPID(APID: Integer; AType: TVideoType): HRESULT; stdcall;
    //function get_VideoPID(out APID: Integer; out AType: TVideoType): HRESULT; stdcall;
    //function put_AudioPID(APID: Integer; AType: TAudioType): HRESULT; stdcall;
    //function get_AudioPID(out APID: Integer; out AType: TAudioType): HRESULT; stdcall;
    //function put_TeletextPID(APID: Integer): HRESULT; stdcall;
    //function get_TeletextPID(out APID: Integer): HRESULT; stdcall;
    //function put_SubtitlePID(APID: Integer; APCRPID: Integer; ACPID: Integer; AAPID: Integer): HRESULT; stdcall;
    //function get_SubtitlePID(out APID: Integer; out APCRPID: Integer; out ACPID: Integer; out AAPID: Integer): HRESULT; stdcall;
    }

}
