      SUBROUTINE FQMATC (DISK, CNO, CATTMP, LUN, SELBAN, SELFRQ,
     *   MATCH, FRQSEL, IRET)
C-----------------------------------------------------------------------
C! Check if selection criteria match FQ table entries.
C# UV IO-appl Calibration Spectral
C-----------------------------------------------------------------------
C;  Copyright (C) 1995, 2012
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
C   Routine to determine if the frequency selection adverbs match
C   entries in the FQ table.
C   Inputs:
C     DISK           I            Volume number
C     CNO            I            Catalogue. number
C     CATTMP(256)    I            catalogue header
C     LUN            I            LUN
C     SELBAN         R            Bandwidth requested
C     SELFRQ         D            Frequency requested
C   Input (via common DUVH)
C     ILOCFQ         I            FRQSEL random parm pointer
C
C   Outputs:
C     MATCH          L            True if match
C     FRQSEL         I            FQ ID number which matches
C     IRET           I            Error code, 0 => OK
C                                 > 0 => FQ table error
C-----------------------------------------------------------------------
      LOGICAL  MATCH
      INTEGER  DISK, CNO, CATTMP(256), LUN, FRQSEL, IRET
      REAL     SELBAN
      DOUBLE PRECISION SELFRQ
C
      INCLUDE 'INCS:PUVD.INC'
      INTEGER  NIF, NUMFQE, FQID, FQSID(MAXIF), I, CATEQU(256), VER,
     *   FQKOLS(MAXFQC), FQNUMV(MAXFQC), IFQRNO, FQBUFF(512)
      REAL     FQCHB(MAXIF), FQTBW(MAXIF), TOL, HZBAN, HZFREQ
      DOUBLE PRECISION FQFRQ(MAXIF), CATD(128), REFF, ACTFRQ
      LOGICAL  FRQOK, T, F, NOBAND, DOFQS, TABLE, CHEXIS, FITASC
      CHARACTER BNDCOD(MAXIF)*8
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DUVH.INC'
      EQUIVALENCE (CATEQU, CATD)
      DATA T, F /.TRUE., .FALSE./
C-----------------------------------------------------------------------
      CALL COPY (256, CATTMP, CATEQU)
      REFF = CATD(KDCRV+JLOCF)
C                                       Do we need to check
      MATCH = T
      IRET = 0
C                                       Is FRQ r.p. present
      IF (ILOCFQ.LT.0) THEN
         FRQSEL = -1
         SELBAN = 0.0
         SELFRQ = 0.D0
         GO TO 999
         END IF
C                                       Check for CH table, remote
C                                       possibility is present
      CALL ISTAB ('CH', DISK, CNO, 1, LUN, FQBUFF, TABLE, CHEXIS,
     *   FITASC, IRET)
      IF (CHEXIS) THEN
         FRQSEL = 1
         SELBAN = 0.0
         SELFRQ = 0.D0
         MATCH = T
         IRET = 0
         GO TO 999
         END IF
C                                       Are selection adverbs set?
      DOFQS = FRQSEL.GT.0
      NOBAND = (SELBAN.LE.0.0) .AND. (SELFRQ.LE.0.D0)
C
      MATCH = F
      HZBAN = SELBAN * 1.0E3
      HZFREQ = SELFRQ * 1.0E6
      FRQOK = F
C                                       Open FQ table
      VER = 1
      CALL FQINI ('READ', FQBUFF, DISK, CNO, VER, CATTMP, LUN, IFQRNO,
     *   FQKOLS, FQNUMV, NIF, IRET)
      IF (IRET.NE.0) GO TO 999
      NUMFQE = FQBUFF(5)
C
      DO 100 I = 1,NUMFQE
         IFQRNO = I
         CALL TABFQ ('READ', FQBUFF, IFQRNO, FQKOLS, FQNUMV, NIF, FQID,
     *      FQFRQ, FQCHB, FQTBW, FQSID, BNDCOD, IRET)
         IF (IRET.NE.0) GO TO 999
C
         IF ((.NOT.DOFQS) .AND. (NOBAND)) THEN
            FRQSEL = FQID
            MATCH = T
            WRITE (MSGTXT,1020) FRQSEL
            IF (NUMFQE.GT.1) CALL MSGWRT (6)
            CALL TABIO ('CLOS', 0, IFQRNO, FQBUFF, FQBUFF, IRET)
            GO TO 999
            END IF
         IF (DOFQS .AND. NOBAND) THEN
            IF (FRQSEL.EQ.FQID) THEN
               MATCH = T
               CALL TABIO ('CLOS', 0, IFQRNO, FQBUFF, FQBUFF, IRET)
               GO TO 999
               END IF
            GO TO 100
            END IF
C                                       Use SELBAN/SELFRQ
         ACTFRQ = REFF + FQFRQ(1)
         IF (SELBAN.GT.0.0) THEN
            TOL = (SELBAN/4.0) * 1.0E3
         ELSE
            TOL = FQTBW(1)/4.0
            END IF
C
         IF ((ACTFRQ .LT. (HZFREQ+TOL)) .AND.
     *       (ACTFRQ .GT. (HZFREQ-TOL))) FRQOK = T
C
         IF (MATCH .AND. FRQOK .AND. (HZBAN.EQ.FQTBW(1))) THEN
            MATCH = F
            WRITE (MSGTXT,1000)
            CALL TABIO ('CLOS', 0, IFQRNO, FQBUFF, FQBUFF, IRET)
            GO TO 990
            END IF
         IF (FRQOK .AND. (HZBAN.EQ.FQTBW(1))) THEN
            MATCH = T
            FRQSEL = FQID
            FRQOK = F
            END IF
 100     CONTINUE
C                                       No match to FRQSEL
      IF (DOFQS .AND. NOBAND) THEN
         WRITE (MSGTXT,1030) FRQSEL
         IRET = 1
         CALL TABIO ('CLOS', 0, IFQRNO, FQBUFF, FQBUFF, IRET)
         GO TO 990
         END IF
C                                       Close table
      CALL TABIO ('CLOS', 0, IFQRNO, FQBUFF, FQBUFF, IRET)
      GO TO 999
C                                       Error
 990  CALL MSGWRT (6)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('FQMATC: MULTIPLE MATCH TO SELBAN & SELFREQ, USE FREQID')
 1010 FORMAT ('FQMATC: NO FREQ. SELECTION ADVERBS SET - CHECK INPUTS')
 1020 FORMAT ('FQMATC: Setting FQ id number = ',I4)
 1030 FORMAT ('FQMATC: FREQID = ',I4,' HAS NO MATCH IN FQ TABLE')
      END
