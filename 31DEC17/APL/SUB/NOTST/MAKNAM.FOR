      SUBROUTINE MAKNAM (INAME, INCLAS, SEQ, VOL, TYPE, USER, NAMSTR)
C-----------------------------------------------------------------------
C! Constructs WAWA namestring (Now use H2WAWA or A2WAWA)
C# IO-wawa Obselete
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
C   This routine will construct a WaWa IO name string, given the
C   values that make up the thing.
C   NOTE: Now call H2WAWA or A2WAWA directly instead of MAKNAM.
C   Inputs:
C      INAME   C*12    file name
C      INCLAS  C*6     file class
C      SEQ     R       file sequence number.
C      VOL     R       file disk volume.
C      TYPE    C*2     file type.
C      USER    R       file user number.
C   Output:
C      NAMSTR  C*36    "Name string" in the tradition of WaWa IO.
C-----------------------------------------------------------------------
      CHARACTER INAME*12, INCLAS*6, TYPE*2, NAMSTR*36
      REAL      SEQ, VOL, USER
      INTEGER   ISEQ, IVOL, IUSER
C-----------------------------------------------------------------------
      ISEQ = SEQ + 0.5
      IVOL = VOL + 0.5
      IUSER = USER + 0.5
      CALL A2WAWA (INAME, INCLAS, ISEQ, TYPE, IVOL, IUSER, NAMSTR)
C
 999  RETURN
      END
