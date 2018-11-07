      SUBROUTINE RNXCLS (IRET)
C-----------------------------------------------------------------------
C! Indexes a uv data file while writing it: empty memory and close file
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
C   Updates NX table on disk and closes file
C   In/output:
C     IRET     I      Error code: input IRET unchanged if failure flag
C                     is set  (RNXRNO < 0)
C-----------------------------------------------------------------------
      INTEGER   IRET
C
      INTEGER   CURSUB
      REAL      CTIME, DTIME
      INCLUDE 'INCS:DRNX.INC'
      INCLUDE 'INCS:DMSG.INC'
C-----------------------------------------------------------------------
C                                       leave if not doing
      IF (RNXRNO.LE.0) GO TO 999
      IRET = 0
C                                       write all out
      DO 10 CURSUB = 1,MAXSUB
         IF (RNXANT(CURSUB).GT.0) THEN
            CTIME = (ETIME(CURSUB) + STIME(CURSUB)) / 2.0
            DTIME = ETIME(CURSUB) - STIME(CURSUB)
            CALL TABNDX ('WRIT', RNXBUF, RNXRNO, RNXKOL, RNXNUM, CTIME,
     *         DTIME, SRC(CURSUB), CURSUB, VSTART(CURSUB), VEND(CURSUB),
     *         FQID(CURSUB), IRET)
            IF (IRET.NE.0) THEN
               WRITE (MSGTXT,1000) IRET, 'WRITE NX TABLE'
               GO TO 990
               END IF
            END IF
 10      CONTINUE
C                                       close
      CURSUB = 1
      CALL TABNDX ('CLOS', RNXBUF, RNXRNO, RNXKOL, RNXNUM, CTIME, DTIME,
     *   SRC(CURSUB), CURSUB, VSTART(CURSUB), VEND(CURSUB),
     *   FQID(CURSUB), IRET)
      RNXRNO = -1
      GO TO 999
C
 990  CALL MSGWRT (8)
      RNXRNO = -1
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('RNXUPD: ERROR',I3,' DOING ',A)
      END
