/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 *  Copyright (C) 2004-2006 Milenko "DCoder" Mitrovic                        *
 *  Mail: dcoder@dsp-worx.de                                                 *
 *  Web:  http://www.dsp-worx.de                                             *
 *                                                                           *
 *  This Program is free software; you can redistribute it and/or modify     *
 *  it under the terms of the GNU General Public License as published by     *
 *  the Free Software Foundation; either version 2, or (at your option)      *
 *  any later version.                                                       *
 *                                                                           *
 *  This Program is distributed in the hope that it will be useful,          *
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of           *
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the             *
 *  GNU General Public License for more details.                             *
 *                                                                           *
 *  You should have received a copy of the GNU General Public License        *
 *  along with GNU Make; see the file COPYING.  If not, write to             *
 *  the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.    *
 *  http://www.gnu.org/copyleft/gpl.html                                     *
 *                                                                           *
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

#ifdef __cplusplus
extern "C" {
#endif
DEFINE_GUID(CLSID_DCDVBSource,	0xC88CB623, 0x2CEC, 0x4870, 0xA7, 0x73, 0x36, 0xF3, 0x41, 0x14, 0xA1, 0xBD);
DEFINE_GUID(IID_IDCDVBSource,	0x8FA96AC0, 0xD500, 0x4939, 0xB0, 0x22, 0xD3, 0x82, 0x0B, 0x40, 0x01, 0x99);

#define DVB_FILTER_VERSION 0x00010100;

DECLARE_INTERFACE_(IDCDVBSource, IUnknown)
{
    STDMETHOD (get_Version) (THIS_ unsigned int *version) PURE;
    STDMETHOD (get_SignalStatistics) (THIS_ int *strength, int *quality, unsigned int *signalPresent) PURE;
    // Channels
    STDMETHOD (get_ChannelCount) (THIS_ int *channelCount) PURE;
    STDMETHOD (get_ChannelInfo) (THIS_ int index, char **name) PURE;
    STDMETHOD (get_ChannelSelected) (THIS_ int *index) PURE;
    STDMETHOD (put_ChannelSelected) (THIS_ int index) PURE;
    STDMETHOD (put_PreviousChannel) (THIS_) PURE;
    STDMETHOD (put_NextChannel) (THIS_) PURE;
    // Audio Streams
    STDMETHOD (get_AudioStreamCount) (THIS_ int *countStreams) PURE;
    STDMETHOD (get_AudioStreamInfo) (THIS_ int index, char **name) PURE;
    STDMETHOD (get_AudioStreamSelected) (THIS_ int *index) PURE;
    STDMETHOD (put_AudioStreamSelected) (THIS_ int index) PURE;
    // Teletext
    STDMETHOD (put_TeletextShow) (THIS_ unsigned int show) PURE;
    STDMETHOD (get_TeletextShow) (THIS_ unsigned int *show) PURE;
    STDMETHOD (put_TeletextTransparent) (THIS_ unsigned int transparent) PURE;
    STDMETHOD (get_TeletextTransparent) (THIS_ unsigned int *transparent) PURE;
    STDMETHOD (put_TeletextPage) (THIS_ int page, int subPage) PURE;
    STDMETHOD (get_TeletextPage) (THIS_ int *page, int *subPage) PURE;
    STDMETHOD (put_TeletextNumber) (THIS_ int number) PURE;
    // EPG
    STDMETHOD (put_EPGLock) (THIS_ unsigned int lock) PURE;
    STDMETHOD (put_EPGClearAll) (THIS_) PURE;
    STDMETHOD (get_EPGRoot) (THIS_ int channelIndex; IXMLDOMNode **epg) PURE;
    STDMETHOD (get_EPGTimeOffset) (THIS_ int *timeOffset) PURE;
    // MHP
    STDMETHOD (get_MHPRoot(THIS_ IXMLDOMNode **mhp) PURE;
};

#ifdef __cplusplus
}
#endif
