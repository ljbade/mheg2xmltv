(* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 *  Copyright (C) 2004, 2005 Milenko Mitrovic                                *
 *  Mail: dcoder@dsp-worx.de                                                 *
 *  Web:  http://www.dsp-worx.de                                             *
 *                                                                           *
 *  SDK for DC-DVB Filter Version 0.1.6                                      *
 *                                                                           *
 *  The Source Code is given "as is" without warranty of any kind. The       *
 *  Author is not responsible for any damage due to the use of this Code.    *
 *  The complete Source Code remains property of the Author and must be      *
 *  used only for creating Plugins for the DC-DVB Filter.                    *
 *                                                                           *
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *)

library DemuxControl;

{$E ddp}
{$R Resource.res}

uses
  UDemuxControl in 'UDemuxControl.pas',
  PropSettings in 'PropSettings.pas',
  PropAbout in 'PropAbout.pas',
  DCDVBShared in '..\..\SDK\DCDVBShared.pas',
  DCDVBDataPlugins in '..\..\SDK\DCDVBDataPlugins.pas',
  IDVBSource in '..\..\SDK\IDVBSource.pas';

exports

  DllGetClassObject,
  DllCanUnloadNow,
  DllRegisterServer,
  DllUnregisterServer;

begin
end.
 