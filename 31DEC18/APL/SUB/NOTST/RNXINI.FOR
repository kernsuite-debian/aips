      SUBROUTINE RNXINI (DISK, CNO, CATBLK, IRET)
C-----------------------------------------------------------------------
C! Indexes a uv data file while writing it: init the process
C# UV EXT-appl
C-----------------------------------------------------------------------
C;  Copyright (C) 2007, 2010-2011
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
C   Creates a new NX table for an output file, to be maintained and
C   updated with RNXUPD and RNXCLS.
C   Inputs:
C      DISK     I        Disk number
C      CNO      I        Volume number
C   In/Out:
C      CATBLK   I(256)   UV data set header (NX ext added on output)
C   Output:
C      IRET     I        Error code
C-----------------------------------------------------------------------
      INTEGER   DISK, CNO, CATBLK(256), IRET
C
      INTEGER   VER, LUN
      INCLUDE 'INCS:DRNX.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DUVH.INC'
C-----------------------------------------------------------------------
C                                       init DRNX
      CALL FILL (MAXSUB, 0, RNXANT)
      CALL COPY (MAXSUB, RNXFIR, RNXCUR)
      MAXGAP = 10.0 / (60. * 24.0)
      MAXLEN = 1.0 / 24.0
      RNXVIS = 0
      NGAPS = 0
      SGAPS = 0.0
      IRET = 0
C                                       <~ 0.2 sec
      RNXEPS = 2.3E-6
C                                       check sort order
      IF (ISORT(1:1).NE.'T') THEN
         IRET = 1
         MSGTXT = 'RNXINI: DATA NOT IN TIME ORDER => NO NX TABLE'
         GO TO 990
         END IF
C                                       make NX table
      VER = 1
      LUN = 79
      CALL NDXINI ('WRIT', RNXBUF, DISK, CNO, VER, CATBLK, LUN,
     *   RNXRNO, RNXKOL, RNXNUM, IRET)
      IF (IRET.GT.0) THEN
         WRITE (MSGTXT,1000) IRET, 'NDXINI CALL'
         GO TO 990
      ELSE IF (RNXRNO.GT.1) THEN
         MSGTXT = 'RNXINI: USING PRE-EXSITING NX TABLE'
         CALL MSGWRT (6)
         END IF
      GO TO 999
C
 990  CALL MSGWRT (8)
      RNXRNO = -1
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('RNXINI: ERROR',I3,' DOING ',A)
      END
