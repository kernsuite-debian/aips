      SUBROUTINE PLCLOS (INDISK, ICNO, CATBLK, PLTBUF, IRET)
C-----------------------------------------------------------------------
C! terminates plotting, flushes plot buffers
C# Utility Plot
C-----------------------------------------------------------------------
C;  Copyright (C) 1997, 2003, 2007
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
C   Terminate plotting / close current PL file
C   Inputs:
C      INDISK    I        Disk vol. no. of uv-file
C      ICNO      I        Slot number of uv-file
C   Input/output:
C      CATBLK    I(256)   Catalog header
C      PLTBUF    I(256)   Work buffer
C   Output
C      IRET      I        Error code:
C                         -1 => user requested stop on multi-page TV
C                          1 => disk error
C                          2 => data error
C                          3 => compress error
C                          4 => close error
C--------------------------------------------------------------------
      INTEGER CATBLK(256), PLTBUF(256), INDISK, ICNO, IRET
C
      INCLUDE 'INCS:DPLD.INC'
      CHARACTER LNAME*12, LCLASS*6, LSTAT*4, LTYPE*2
      INTEGER   IERR, INSEQ, IUSER, IERSAV
C--------------------------------------------------------------------
C                                       Skip if already closed
      IF (IPLSTA.EQ.0) GO TO 999
C                                       Terminate plotting
      CALL GFINIS (PLTBLK, IRET)
      IERSAV = IRET
      IPLSTA = 0
C                                       Clear status
      IF (.NOT.WPDOTV) THEN
         LSTAT = 'CLRD'
         CALL CATDIR ('CSTA', INDISK, ICNO, LNAME, LCLASS, INSEQ,
     *      LTYPE, IUSER, LSTAT, PLTBUF, IERR)
C                                       Close PL file if necc.
C                                       Add history record
         IF (IERSAV.EQ.0) THEN
            CALL HIPLOT (INDISK, ICNO, JPLVER, PLTBUF, IERR)
C                                       Destroy failed PL file
         ELSE
            CALL ZCLOSE (IGLUN, IGFIND, IERR)
            CALL ZPHFIL ('PL', INDISK, ICNO, JPLVER, LPLNAM, IERR)
            CALL ZDESTR (INDISK, LPLNAM, IERR)
            CALL DELEXT ('PL', INDISK, ICNO, 'WRIT', CATBLK, PLTBUF,
     *         JPLVER, IERR)
            END IF
         END IF
C                                       Exit
 999  RETURN
      END
