      SUBROUTINE WAWA2A (NAMEST, NAME, CLASS, SEQ, PTYPE, VOL, USID)
C-----------------------------------------------------------------------
C! unpacks WaWa IO NameString into its components
C# IO-WaWa
C-----------------------------------------------------------------------
C;  Copyright (C) 1995
C;  Associated Universities, Inc. Washington DC, USA.
C;
C;  This program is free software; you can redistribute it and/or
C;  modify it under the terms of the GNU General Public License as
C;  published by the Free Software Foundation; either version 2 of
C;  the License, or (at your option) any later version.
C;
C;  This program is distributed in the hope that it will be useful,
C;  but WITHOUT ANY WARRANTY; without even the implied warranty of
C;  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
C;  GNU General Public License for more details.
C;
C;  You should have received a copy of the GNU General Public
C;  License along with this program; if not, write to the Free
C;  Software Foundation, Inc., 675 Massachusetts Ave, Cambridge,
C;  MA 02139, USA.
C;
C;  Correspondence concerning AIPS should be addressed as follows:
C;         Internet email: aipsmail@nrao.edu.
C;         Postal address: AIPS Project Office
C;                         National Radio Astronomy Observatory
C;                         520 Edgemont Road
C;                         Charlottesville, VA 22903-2475 USA
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
C   unpacks Wawa-IO Namestring having format A12, A6, A2, I7, I2, I7
C   for name, class, ptype, seq, vol, usid into component parts
C   Input:
C      NAMEST  C*36   WaWa Namestring
C   Outputs:
C      NAME    C*12   file name
C      CLASS   C*6    file class (6 chars)
C      SEQ     I      file sequence number
C      PTYPE   C*2    file physical type (2 chars)
C      VOL     I      file disk number
C      USID    I      user number
C-----------------------------------------------------------------------
      CHARACTER NAME*12, CLASS*6, PTYPE*2
      INTEGER   SEQ, VOL, USID
      CHARACTER NAMEST*36
C-----------------------------------------------------------------------
      READ (NAMEST,1000,ERR=999) NAME, CLASS, PTYPE, SEQ, VOL, USID
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT (A12,A6,A2,I7,I2,I7)
      END
