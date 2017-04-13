      SUBROUTINE TABFRM (FORMAT, IWIDTH, FRAC, ITYPE, IERR)
C-----------------------------------------------------------------------
C! Parses format for FITS ASCI table entries.
C# FITS Tape EXT-util Parsing
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
C   This routine will parse a table file format specifier and determine
C   the format width, fractional part (for D or E formats) and the
C   format code.
C   Inputs:
C      FORMAT   C*8   Format code in the form Iww or Aww or
C                     Eww.dd or Dww.dd, padded with blanks.
C   Output:
C      IWIDTH   I     The width of the field.
C      FRAC     I     The fractional part of the field (E and D only).
C      ITYPE    I     The type of the field (D=1, E=2, A=3, I=4)
C      IERR     I     Error code: 0 = ok.
C-----------------------------------------------------------------------
      CHARACTER FORMAT*8
      INTEGER   IWIDTH, FRAC, ITYPE, IERR
C
      CHARACTER TYPES*10, NUMS*10, ONECHR*1
      INTEGER   IT, IPOS, ISTART, ITYPES(10)
      INCLUDE 'INCS:DMSG.INC'
      DATA NUMS /'0123456789'/
      DATA TYPES /'DdEeAaIiFf'/
C                                       Note that F format is E format
      DATA ITYPES /1,1,2,2,3,3,4,4,2,2/
C-----------------------------------------------------------------------
C                                       Find type
      IPOS = INDEX (TYPES, FORMAT(1:1))
      IF (IPOS.LE.0) GO TO 970
      ITYPE = ITYPES(IPOS)
C                                       Determine width.
      IWIDTH = 0
      DO 300 IPOS = 2,8
         ONECHR = FORMAT(IPOS:IPOS)
         IF ((ONECHR.EQ.'.') .AND. (ITYPE.LT.3)) GO TO 400
         IF ((ONECHR.EQ.' ') .AND. (ITYPE.GE.3)) GO TO 600
         IT = INDEX (NUMS, ONECHR)
         IF (IT.LE.0) GO TO 980
         IWIDTH = 10 * IWIDTH + (IT - 1)
 300     CONTINUE
      IF (ITYPE.GE.3) GO TO 600
C                                       Determine fraction.
 400  ISTART = IPOS + 1
      FRAC = 0
      DO 500 IPOS = ISTART,8
         ONECHR = FORMAT(IPOS:IPOS)
         IF (ONECHR.EQ.' ') GO TO 999
         IT = INDEX (NUMS, ONECHR)
         IF (IT.LE.0) GO TO 980
         FRAC = 10 * FRAC + (IT - 1)
 500     CONTINUE
C                                       Integer or character
 600  FRAC = -1
      GO TO 999
C                                       Problem decoding
 970  WRITE (MSGTXT,1970) FORMAT
      GO TO 990
 980  WRITE (MSGTXT,1980) FORMAT
 990  CALL MSGWRT (6)
      IERR = 1
 999  RETURN
C-----------------------------------------------------------------------
 1970 FORMAT ('TABFRM: BAD FITS TABLE FORMAT ',A)
 1980 FORMAT ('TABFRM: BAD FITS TABLE ASCII NUMBER, FORMAT IS ',A)
      END
