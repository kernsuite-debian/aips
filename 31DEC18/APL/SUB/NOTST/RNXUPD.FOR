      SUBROUTINE RNXUPD (RPARM, IRET)
C-----------------------------------------------------------------------
C! Indexes a uv data file while writing it: get new record
C# UV EXT-appl
C-----------------------------------------------------------------------
C;  Copyright (C) 2007, 2010-2011, 2015
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
C   Updates tables in core if needed, and if necessary, write a new
C   NX table record.
C   Inputs:
C      RPARM    R(*)   Random parameters
C   Common:
C      RNXEPS   R      Min time change to count as change (days)
C                      RNXINI sets it to 0.2 sec, calling can change
C                      rights after if an interval norm is known
C   In/output:
C     IRET      I      Error code: input IRET unchanged if failure flag
C                      is set  (RNXRNO < 0)
C-----------------------------------------------------------------------
      INTEGER   IRET
      REAL      RPARM(*)
C
      INTEGER   IA1, IA2, CURSUB, CURSOU, CURFQI, ISUB
      REAL      CURTIM, TEMP, GAP, LENGTH, CTIME, DTIME, T
      INCLUDE 'INCS:DRNX.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DMSG.INC'
C-----------------------------------------------------------------------
C                                       leave if not doing
      IF (RNXRNO.LE.0) GO TO 999
      IRET = 0
C                                       parameters of this record
      IF (ILOCB.GE.0) THEN
         TEMP = RPARM(1+ILOCB)
         IA1 = TEMP / 256.0 + 0.001
         IA2 = TEMP - 256 * IA1 + 0.01
         CURSUB = 100.0 * (TEMP - IA1*256 -IA2) + 1.5
      ELSE
         CURSUB = RPARM(1+ILOCSA)
         END IF
      IF (ILOCSU.GE.0) THEN
         CURSOU = RPARM(1+ILOCSU)
      ELSE
         CURSOU = 1
         END IF
      IF (ILOCFQ.GE.0) THEN
         CURFQI = RPARM(1+ILOCFQ)
      ELSE
         CURFQI = 1
         END IF
      CURTIM = RPARM(1+ILOCT)
C                                       new vis
      RNXVIS = RNXVIS + 1
C                                       start of scan
      IF (RNXANT(CURSUB).EQ.0) THEN
         VSTART(CURSUB) = RNXVIS
         STIME(CURSUB)  = CURTIM
         SRC(CURSUB)    = CURSOU
         FQID(CURSUB)   = CURFQI
 10      IF (RNXNOS(CURSUB).GT.0) THEN
            IF (CURTIM.GT.RNXTSC(RNXCUR(CURSUB)+1)) THEN
               RNXCUR(CURSUB) = RNXCUR(CURSUB) + 1
               IF (RNXCUR(CURSUB)-RNXFIR(CURSUB)+1.GE.RNXNOS(CURSUB))
     *            RNXNOS(CURSUB) = 0
               GO TO 10
               END IF
            END IF
C                                       Time has gone down!
      ELSE IF (CURTIM.LT.ETIME(CURSUB)) THEN
         GAP    = CURTIM - ETIME(CURSUB)
         STIME(CURSUB) = MIN (STIME(CURSUB), CURTIM)
         IF (GAP.LT.-MAXGAP) THEN
            IRET = 1
            MSGTXT = 'RNXUPD: DATA SERIOUSLY OUT OF TIME ORDER'
            GO TO 990
            END IF
C                                       Time has increased
      ELSE IF (CURTIM.GT.ETIME(CURSUB)) THEN
C                                       use previous info
         IF (RNXNOS(CURSUB).GT.0) THEN
            GAP = 0.0
            LENGTH = 0.0
 15         IF (CURTIM.GT.RNXTSC(RNXCUR(CURSUB)+1)) THEN
               RNXCUR(CURSUB) = RNXCUR(CURSUB) + 1
               LENGTH = 2. * MAXLEN
               IF (RNXCUR(CURSUB)-RNXFIR(CURSUB)+1.GE.RNXNOS(CURSUB))
     *            THEN
                  RNXNOS(CURSUB) = 0
               ELSE
                  GO TO 15
                  END IF
               END IF
C                                       figure it out ourselves
         ELSE
            GAP    = CURTIM - ETIME(CURSUB)
            LENGTH = CURTIM - STIME(CURSUB)
C                                       try to find proper maxgap
            IF ((NGAPS.GE.0) .AND. (GAP.GT.RNXEPS) .AND. (GAP.LE.MAXGAP)
     *         .AND. (CURSOU.EQ.SRC(CURSUB)) .AND.
     *         (CURFQI.EQ.FQID(CURSUB))) THEN
               T = MAX (10.0*SGAPS/NGAPS, 0.0007)
               IF (NGAPS.EQ.0) THEN
                  NGAPS = 1
                  SGAPS = GAP
               ELSE IF ((NGAPS.GT.1) .AND. (GAP.GT.T)) THEN
                  MAXGAP = (T + GAP) / 2.0
                  NGAPS = -1
               ELSE
                  NGAPS = NGAPS + 1
                  SGAPS = SGAPS + GAP
                  END IF
               END IF
            END IF
C                                       close this scan
         IF ((LENGTH.GT.MAXLEN) .OR. (GAP.GT.MAXGAP) .OR.
     *      (CURSOU.NE.SRC(CURSUB)) .OR. (CURFQI.NE.FQID(CURSUB))) THEN
C                                       dump others??
            DO 20 ISUB = 1,MAXSUB
               IF ((RNXANT(ISUB).GT.0) .AND. (ISUB.NE.CURSUB) .AND.
     *            (ETIME(ISUB).LT.STIME(CURSUB))) THEN
                  CTIME = (ETIME(ISUB) + STIME(ISUB)) / 2.0
                  DTIME = ETIME(ISUB) - STIME(ISUB)
                  CALL TABNDX ('WRIT', RNXBUF, RNXRNO, RNXKOL, RNXNUM,
     *               CTIME, DTIME, SRC(ISUB), ISUB, VSTART(ISUB),
     *               VEND(ISUB), FQID(ISUB), IRET)
                  IF (IRET.NE.0) THEN
                     WRITE (MSGTXT,1000) IRET, 'WRITE NX TABLE'
                     GO TO 990
                     END IF
                  RNXANT(ISUB) = 0
                  END IF
 20            CONTINUE
C                                       now write current
            CTIME = (ETIME(CURSUB) + STIME(CURSUB)) / 2.0
            DTIME = ETIME(CURSUB) - STIME(CURSUB)
            CALL TABNDX ('WRIT', RNXBUF, RNXRNO, RNXKOL, RNXNUM,
     *         CTIME, DTIME, SRC(CURSUB), CURSUB, VSTART(CURSUB),
     *         VEND(CURSUB), FQID(CURSUB), IRET)
            IF (IRET.NE.0) THEN
               WRITE (MSGTXT,1000) IRET, 'WRITE NX TABLE'
               GO TO 990
               END IF
C                                       Set up new scan:
            RNXANT(CURSUB) = 0
            VSTART(CURSUB) = RNXVIS
            STIME(CURSUB)  = CURTIM
            SRC(CURSUB)    = CURSOU
            FQID(CURSUB)   = CURFQI
            END IF
         END IF
C                                       Update scan information:
      VEND(CURSUB) = RNXVIS
      ETIME(CURSUB) = CURTIM
      RNXANT(CURSUB) = 1
      GO TO 999
C
 990  CALL MSGWRT (8)
      RNXRNO = -1
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('RNXUPD: ERROR',I3,' DOING ',A)
      END
