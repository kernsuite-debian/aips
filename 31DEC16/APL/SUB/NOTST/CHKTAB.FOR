      SUBROUTINE CHKTAB (SRTORD, IERR)
C-----------------------------------------------------------------------
C! Check fields of known FITS table types.
C# FITS
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
C  Check fields of known table types and report non AIPS characteristics
C  that may indicate trouble.
C  Input:
C     COMMON /THDR/
C  Output:
C     SRTORD    I     Logical column number we think data is sorted on.
C                     Sets to -1 (descending 1st col) for CC,
C                     0 (unknown) for others.
C     IERR      I     Error code, 0=ok.
C-----------------------------------------------------------------------
      CHARACTER TABTYP(2)*2
      INTEGER   SRTORD, IERR
      INTEGER   I
      INCLUDE 'INCS:DTHD.INC'
      INCLUDE 'INCS:DMSG.INC'
      DATA TABTYP /'TA','CC'/
C-----------------------------------------------------------------------
C
      IERR = 0
      SRTORD = 0
C                                       Loop for all known table types.
      DO 800 I = 1,2
        IF (TABTYP(I).EQ.ITYPE) GO TO (100,200), I
        GO TO 800
C                                       General.
 100       WRITE (MSGTXT,1100) ITYPE
           GO TO 980
C                                       CC table.
 200       SRTORD = -1
           IF ((ITNCOL.EQ.3).OR.(ITNCOL.EQ.7)) GO TO 210
              WRITE (MSGTXT,1200) ITYPE, ITNCOL
              CALL MSGWRT (7)
 210       CONTINUE
           WRITE (MSGTXT,1100) ITYPE
           GO TO 980
C
 800    CONTINUE
C                                       Unrecognized table type.
      WRITE (MSGTXT,1800) ITYPE
 980  CALL MSGWRT (6)
 999  RETURN
C-----------------------------------------------------------------------
 1100 FORMAT ('Reading table of type ', A2)
 1200 FORMAT ('NON STANDARD NUMBER OF COLUMNS FOR ',A2,' TABLE =',I4)
 1800 FORMAT ('READING UNKNOWN TABLE OF TYPE ', A2)
      END
