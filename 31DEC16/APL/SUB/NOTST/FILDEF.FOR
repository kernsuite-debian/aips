      SUBROUTINE FILDEF (INSTR, OUTSTR)
C-----------------------------------------------------------------------
C! Fills in default values in WAWA namestring
C# IO-wawa
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
C  FILDEF will fill in the default values for any 'indefinite' portion
C  of a name string.
C  INPUTS:  INSTR   C*36  a name string containing the values to use
C                   for any indefinite values.
C  IN/OUT:  OUTSTR  C*36   a name string that will have blanks or
C                   zeros filled in with values from INSTR.
C-----------------------------------------------------------------------
      CHARACTER INSTR*36, OUTSTR*36, INAME*12, ONAME*12, ICLASS*6,
     *   OCLASS*6, IPTYP*2, OPTYP*2
      INTEGER   ISEQ, OSEQ, IVOL, OVOL, IUSID, OUSID
C-----------------------------------------------------------------------
      CALL WAWA2A (INSTR, INAME, ICLASS, ISEQ, IPTYP, IVOL, IUSID)
      CALL WAWA2A (OUTSTR, ONAME, OCLASS, OSEQ, OPTYP, OVOL, OUSID)
C                                       Name
      IF (ONAME.EQ.'            ') ONAME = INAME
C                                       Class.
      IF (OCLASS.EQ.'      ') OCLASS = ICLASS
C                                       File type.
      IF (OPTYP.EQ.'  ') OPTYP = IPTYP
C                                       User number.
      IF (OUSID.EQ.0) OUSID = IUSID
C                                       Put it back.
      CALL A2WAWA (ONAME, OCLASS, OSEQ, OPTYP, OVOL, OUSID, OUTSTR)
C
 999  RETURN
      END
