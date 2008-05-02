(* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
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
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *)

unit DSMCCConst;

{$I Compiler.inc}

interface

const
  DSMCC_TABLE_ID_DATA_MIN                    = $3B;
  DSMCC_TABLE_ID_DATA_MAX                    = $3C;

type
  TDSMCCHeaderType = ( htBase, htDII, htDC, htDSI, htDDB );
  TDSMCCServerID = array[0..19] of Byte;

  TModuleInfoTap = packed record
    ID: Word;
    Use: Word;
    AssociationTag: Word;
    SelectorLength: Byte;
  end;
  PModuleInfoTap = ^TModuleInfoTap;

implementation

end.
 