      SUBROUTINE DATFLG (RPARM, VIS, DROP, IERR)
C-----------------------------------------------------------------------
C! Flags data specified in flagging table
C# Calibration
C-----------------------------------------------------------------------
C;  Copyright (C) 1995-1996, 1998, 2000-2001, 2007, 2011, 2014-2015
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
C   Flags data specified in flagging table
C     Inputs:
C      RPARM(*)   R    Random parameter array
C      VIS(3,*)   R    Visibility array
C     Inputs from include DSEL.INC, DFLG.INC:
C      CURSOU     I    Current source number
C      NUMFLG     I    Number of flagging entries.
C      TMFLST     R    Time of last visibility for which flagging
C                      was checked.
C      FLGSOU(*)  I    Source id numbers to flag, 0=all.
C      FLGANT(*)  I    Antenna numbers to flag, 0=all.
C      FLGBAS(*)  I    Baseline (A1*256+A2) numbers to flag, 0=all.
C      FLGSUB(*)  I    Subarray numbers to flag, 0=all.
C      FLGFQD(*)  I    Freqid numbers to flag, <=0=all.
C                      Following should have defaults filled in.
C      FLGBIF(*)  I    First IF to flag.
C      FLGEIF(*)  I    Highest IF to flag.
C      FLGBCH(*)  I    First channel to flag.
C      FLGECH(*)  I    Highest channel to flag.
C      FLGPOL(4,*)L    Flags for the polarizations, should correspond
C                      to selected polarization types.
C     Output:
C      RPARM(*)   R    Random parameter array
C      VIS(3,*)   R    Visibility array
C      DROP       L    True if data all flagged.
C      IERR       I    Return code, 0=OK, else NXTFLG error number.
C-----------------------------------------------------------------------
      REAL      RPARM(*), VIS(3,*)
      LOGICAL   DROP
      INTEGER   IERR
C
      INTEGER   IFLAG, KBASE, A1, A2, FLGA, SUBA, JIF, JCHAN, JPOLN,
     *   LIMF1, LIMF2, LIMC1, LIMC2, IFADD, INDEX, STADD, IPOLPT, LFQ
      LOGICAL   GOOD
      REAL      TIME, SUM
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DSEL.INC'
      INCLUDE 'INCS:PFLG.INC'
      INCLUDE 'INCS:DFLG.INC'
C-----------------------------------------------------------------------
      IERR = 0
      DROP = .FALSE.
C                                       Check if new time
      TIME = RPARM(1+ILOCT)
      IF (TMFLST.LT.TIME) CALL NXTFLG (TIME, .FALSE., IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Check if there are current flags
      IF (NUMFLG.LE.0) GO TO 999
C                                       Loop thru flagging criteria
      IF (ILOCB.GE.0) THEN
         KBASE = RPARM(1+ILOCB) + 0.1
         A1 = KBASE / 256
         A2 = KBASE - 256 * A1
         SUBA = (RPARM(1+ILOCB) - KBASE) * 100.0 + 1.5
      ELSE
         A1 = RPARM(1+ILOCA1) + 0.1
         A2 = RPARM(1+ILOCA2) + 0.1
         SUBA = RPARM(1+ILOCSA) + 0.1
         END IF
      KBASE = 32768 * MIN (A1,A2) + MAX (A1,A2)
      DO 500 IFLAG = 1,NUMFLG
C                                       Check time if needed
         IF (.NOT.TIMORD) THEN
            IF ((TIME.LT.FLGTST(IFLAG)) .OR. (TIME.GT.FLGTND(IFLAG)))
     *         GO TO 500
            END IF
C                                       Check source
         IF ((FLGSOU(IFLAG).NE.CURSOU) .AND. (FLGSOU(IFLAG).NE.0) .AND.
     *      (CURSOU.NE.0)) GO TO 500
C                                       Check antenna
         FLGA = FLGANT(IFLAG)
         IF ((FLGA.NE.0) .AND. (FLGA.NE.A1) .AND. (FLGA.NE.A2))
     *      GO TO 500
C                                       Check baseline
         IF ((FLGBAS(IFLAG).NE.0) .AND. (FLGBAS(IFLAG).NE.KBASE))
     *      GO TO 500
C                                       Check subarray
         IF ((FLGSUB(IFLAG).GT.0) .AND. (FLGSUB(IFLAG).NE.SUBA))
     *      GO TO 500
C                                       Check freqid.
         IF (ILOCFQ.GE.0) THEN
            LFQ = RPARM(1+ILOCFQ) + 0.1
            IF ((FLGFQD(IFLAG).GT.0) .AND. (FLGFQD(IFLAG).NE.LFQ))
     *         GO TO 500
            END IF
C                                       Some data to be flagged
C                                       Set limits
         LIMF1 = FLGBIF(IFLAG)
         LIMF2 = FLGEIF(IFLAG)
         LIMC1 = FLGBCH(IFLAG)
         LIMC2 = FLGECH(IFLAG)
C                                       Loop over polarizations
         IPOLPT = ABS(KCOR0) - 1
         IF (KCOR0.LT.-4) IPOLPT = IPOLPT - 4
         DO 400 JPOLN = 1,KNCOR
            IF (FLGPOL(JPOLN+IPOLPT,IFLAG)) THEN
               STADD = (JPOLN-1) * KNCS + 1
C                                       Loop over IF
               DO 300 JIF = LIMF1,LIMF2
                  INDEX = STADD + (JIF-1) * KNCIF + (LIMC1-1) * KNCF
                  IF (LIMC1.EQ.LIMC2) THEN
C                                       Single channel
                     VIS(3,INDEX) = - ABS (VIS(3,INDEX))
                  ELSE
C                                       Loop over channel
                     DO 200 JCHAN = LIMC1,LIMC2
C                                       Flag
                        VIS(3,INDEX) = - ABS (VIS(3,INDEX))
                        INDEX = INDEX + KNCF
 200                    CONTINUE
                     END IF
 300              CONTINUE
               END IF
 400        CONTINUE
 500     CONTINUE
C                                       Check if data all bad
      GOOD = .FALSE.
C                                       Loop over IF
      DO 530 JIF = BIF,EIF
         IFADD = (JIF-1) * KNCIF + 1
C                                       Loop over polarizations
         DO 520 JPOLN = 1,KNCOR
            INDEX = IFADD + (JPOLN-1) * KNCS + (BCHAN-1) * KNCF
C                                       Single channel
            IF (BCHAN.EQ.ECHAN) THEN
               GOOD = GOOD .OR. (VIS(3,INDEX).GT.0.0)
C                                       Multiple channels
            ELSE
               SUM = 0.0
               DO 510 JCHAN = BCHAN,ECHAN
                  SUM = SUM + MAX (0.0, VIS(3,INDEX))
                  INDEX = INDEX + KNCF
 510              CONTINUE
               GOOD = GOOD .OR. (SUM.GT.0.0)
               END IF
 520        CONTINUE
 530     CONTINUE
      DROP = .NOT.GOOD
C
 999  RETURN
      END
