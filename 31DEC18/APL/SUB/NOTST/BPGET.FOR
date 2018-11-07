      SUBROUTINE BPGET (TIME, IA1, IA2, IERR)
C-----------------------------------------------------------------------
C! Sets bandpass correction arrays in common
C# Calibration
C-----------------------------------------------------------------------
C;  Copyright (C) 1995-1998, 2000, 2005-2006, 2010-2011, 2014, 2018
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
C   Gets next set of bandpass data in dynamic array BPBUF if needed.
C   It sets pointers to the correction(s) for the specified antennas
C   and, if needed, updates the BPIBUF data so that the 2 times in the
C   corrections for DOBAN 2 and 3 span the specified time.
C   Inputs:
C      TIME     R      Current time of data (days)
C      IA1      I      First antenna to be selected
C      IA2      I      Second antenna to be selected
C   Inputs from common /...../
C      DOBAND   I      Bandpass selection option
C      PBPBUF   LI     pointer to actual BPBUF array
C   Output:
C      IERR     I      Return error code. 0=>OK, else error
C                      -1 => NO BP found for this pair
C   Output to common /...../
C      BPBUF    R(*)   Array containing bandpass spectra
C      ANTPNT   LI(2,3)  Pointers to 1 or 2 soln for ant1 and ant2
C      BPGOT    I(2,3)   Antenna # (1,*) should be ant1, (2,*) ant2
C                        1st is interp, 2nd is early time 3rd is later
C-----------------------------------------------------------------------
      REAL      TIME
      INTEGER   IA1, IA2, IERR
C
      INTEGER   LRECS, KLOCT, KLOCA, OFFW, SOURID, SUBA, ANTNO,
     *   FREQID, BPREF(2), IIC, IIF, J, IIP, JA(2)
      LONGINT   BIND, BINX, VINX, BINC, INX, INC, IND, WNC, WND,
     *   LNC, LND
      LOGICAL   GOT(2,3), DOIT
      REAL      INTERV, WT1, WT2, TEMP, AMPC, AMPD, AMPX, TIM1, TIM2,
     *   AWT1, AWT2
      DOUBLE PRECISION BPTIME
      INCLUDE 'INCS:DSEL.INC'
      INCLUDE 'INCS:DBPC.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DCVL.INC'
C-----------------------------------------------------------------------
      IERR = -1
      IF ((DOBAND.LT.0) .OR. (DOBAND.GT.5)) THEN
         IERR = 1
         MSGTXT = 'BPGET: INVALID DOBAND'
         GO TO 990
         END IF
C                                       Init for reading
      LRECS = 2 * NCHNBP * NIFBP * NPOLBP + 2
      OFFW = LRECS
      IF (DOBAND.GT.1) LRECS = LRECS + NPOLBP * NIFBP
      KLOCT = 0
      KLOCA = 1
      JA(1) = IA1
      JA(2) = IA2
C                                       See if antennae present
C                                       in I/O buffer index array
      CALL IOBSRC (IA1, IA2, GOT)
      IF (.NOT. (GOT(1,1) .AND. GOT(2,1))) GO TO 999
C                                       Check times for non-averaged
      IF (DOBAND.GT.1) THEN
         DO 100 J = 1,2
            DOIT = .FALSE.
            BIND = ANTNX(3,JA(J))
            BINC = ANTNX(2,JA(J))
            BINX = ANTNX(1,JA(J))
            VINX = BINX + PVLBUF - PBPBUF
C                                       Update antenna 1
 20         IF ((GOT(J,2)) .AND. (GOT(J,3))) THEN
C                                       time moved enough
               DOIT = 50.*(TIME-BPBUF(BINX)).GT.
     *            BPIBUF(BIND)-BPIBUF(BINC)
C                                       read in new BP data
               IF (TIME.GT.BPIBUF(BIND)) THEN
                  DOIT = .TRUE.
C                                       move 2nd to 1st
                  CALL RCOPY (LRECS, BPIBUF(BIND), BPIBUF(BINC))
                  ANTREC(1,JA(J)) = ANTREC(1,JA(J)) + 1
C                                       done reading: off 2nd
                  IF (ANTREC(1,JA(J)).GE.ANTREC(2,JA(J))) THEN
                     BPIBUF(BIND+KLOCA) = -1.0
                     BPGOT(J,3) = 0
                     GOT(J,3) = .FALSE.
C                                       else read new 2nd
                  ELSE
 30                  IBPRNO = ANTREC(1,JA(J)) + 1
                     CALL TABBP ('READ', BPBUFF, IBPRNO, BPKOLS, BPNUMV,
     *                  NIFBP, NCHNBP, NPOLBP, BPTIME, INTERV, SOURID,
     *                  SUBA, ANTNO, CHNBND, BPFREQ, FREQID, BPREF,
     *                  BPIBUF(BIND+OFFW), BPIBUF(BIND+2), IERR)
                     IF (IERR.GT.0) THEN
                        WRITE (MSGTXT,1000) IERR, 'READ', 'BP TABLE'
                        GO TO 990
                        END IF
C                                       Check freqid
                     IF (((FREQID.NE.FRQSEL) .AND. (FREQID.GT.0) .AND.
     *                  (FRQSEL.GT.0)) .OR. ((SUBA.NE.SUBARR) .AND.
     *                  (SUBA.GT.0) .AND. (SUBARR.GT.0)) .OR.
     *                  (IERR.EQ.-3)) THEN
                        ANTREC(1,JA(J)) = ANTREC(1,JA(J)) + 1
                        IF (ANTREC(1,JA(J)).GE.ANTREC(2,JA(J))) THEN
                           BPIBUF(BIND+KLOCA) = -1.0
                           BPGOT(J,3) = 0
                           GOT(J,3) = .FALSE.
                        ELSE
                           GO TO 30
                           END IF
                        END IF
                     BPIBUF(BIND+KLOCT) = BPTIME
                     BPIBUF(BIND+KLOCA) = ANTNO + 0.01*MAX (SUBARR-1, 0)
                     END IF
                  GO TO 20
                  END IF
               END IF
C                                       do interp/nearest
            IF (DOIT) THEN
C                                       copy remaining one
               IF (BPGOT(J,3).LE.0) THEN
                  CALL RCOPY (OFFW, BPIBUF(BINC), BPBUF(BINX))
C                                       work through arrays
               ELSE
                  INX = BINX + 2
                  INC = BINC + 2
                  IND = BIND + 2
                  WNC = INC + NPOLBP * NIFBP * NCHNBP * 2
                  WND = IND + NPOLBP * NIFBP * NCHNBP * 2
                  BPBUF(BINX) = TIME
                  BPBUF(BINX+1) = BPIBUF(BINC+1)
                  DO 80 IIP = 1,NPOLBP
                     DO 70 IIF = 1,NIFBP
                        TIM2 = BPIBUF(BIND)
                        TIM1 = BPIBUF(BINC)
                        WT1 = BPIBUF(WNC)
                        WT2 = BPIBUF(WND)
                        IF ((WT1.LE.0.0) .AND. (WT2.LE.0.0)) THEN
                           WT1 = 0.0
                           WT2 = 0.0
                        ELSE IF (WT2.LE.0.0) THEN
                           WT1 = 1.0
                           WT2 = 0.0
                        ELSE IF (WT1.LE.0.0) THEN
                           WT2 = 1.0
                           WT1 = 0.0
                        ELSE
                           IF (DOBAND.GT.3) THEN
                              WT1 = 1.0
                              WT2 = 1.0
                              END IF
                           TEMP = WT1 * (TIM2 - TIME)
                           WT2 = WT2 * (TIME - TIM1)
                           IF (TEMP+WT2.EQ.0.0) TEMP = 1.0
                           WT1 = TEMP / (TEMP + WT2)
                           WT2 = WT2 / (TEMP + WT2)
                           END IF
                        WNC = WNC + 1
                        WND = WND + 1
C                                       nearest
                        IF ((DOBAND.EQ.2) .OR. (DOBAND.EQ.4)) THEN
                           IF (WT2.GT.WT1) THEN
                              CALL RCOPY (2*NCHNBP, BPIBUF(IND),
     *                           BPBUF(INX))
                           ELSE
                              CALL RCOPY (2*NCHNBP, BPIBUF(INC),
     *                           BPBUF(INX))
                              END IF
                           INX = INX + NCHNBP * 2
                           INC = INC + NCHNBP * 2
                           IND = IND + NCHNBP * 2
                        ELSE
                           LNC = INC
                           LND = IND
                           AWT1 = 0.0
                           AWT2 = 0.0
                           DO 50 IIC = 1,NCHNBP
                              IF ((BPIBUF(LNC).NE.FBLANK) .AND.
     *                           (BPIBUF(LNC+1).NE.FBLANK)) AWT1 = WT1
                              IF ((BPIBUF(LND).NE.FBLANK) .AND.
     *                           (BPIBUF(LND+1).NE.FBLANK)) AWT2 = WT2
                              LNC = LNC + 2
                              LND = LND + 2
 50                           CONTINUE
                           IF ((AWT1.LE.0.0) .AND. (AWT2.GT.0.0)) THEN
                              AWT2 = 1.0
                              AWT1 = 0.0
                           ELSE IF ((AWT1.GT.0.0) .AND. (AWT2.LE.0.0))
     *                        THEN
                              AWT1 = 1.0
                              AWT2 = 0.0
                              END IF
                           DO 60 IIC = 1,NCHNBP
                              IF ((AWT1.LE.0.0) .AND. (AWT2.LE.0.0))
     *                           THEN
                                 BPBUF(INX) = FBLANK
                                 BPBUF(INX+1) = FBLANK
                              ELSE IF (((BPIBUF(INC).EQ.FBLANK) .OR.
     *                           (BPIBUF(INC+1).EQ.FBLANK)) .AND.
     *                           (AWT1.GT.0.0)) THEN
                                 BPBUF(INX) = BPIBUF(IND)
                                 BPBUF(INX+1) = BPIBUF(IND+1)
                              ELSE IF (((BPIBUF(IND).EQ.FBLANK) .OR.
     *                           (BPIBUF(IND+1).EQ.FBLANK)) .AND.
     *                           (AWT2.GT.0.0)) THEN
                                 BPBUF(INX) = BPIBUF(INC)
                                 BPBUF(INX+1) = BPIBUF(INC+1)
C                                       amp scalar
                              ELSE
                                 BPBUF(INX) = AWT1 * BPIBUF(INC) +
     *                              AWT2 * BPIBUF(IND)
                                 BPBUF(INX+1) = AWT1 * BPIBUF(INC+1) +
     *                              AWT2 * BPIBUF(IND+1)
                                 IF (.NOT.WPOLY) THEN
                                    AMPC = SQRT (BPIBUF(INC)**2 +
     *                                 BPIBUF(INC+1)**2)
                                    AMPD = SQRT (BPIBUF(IND)**2 +
     *                                 BPIBUF(IND+1)**2)
                                    AMPX = SQRT (BPBUF(INX)**2 +
     *                                 BPBUF(INX+1)**2)
                                    IF (AMPX.LT.1.E-10) AMPX = 1.
                                    AMPX = (AWT1*AMPC + AWT2*AMPD) /
     *                                 AMPX
                                    BPBUF(INX) = BPBUF(INX) * AMPX
                                    BPBUF(INX+1) = BPBUF(INX+1) * AMPX
                                    END IF
                                 END IF
                              INX = INX + 2
                              INC = INC + 2
                              IND = IND + 2
 60                           CONTINUE
                           END IF
 70                     CONTINUE
 80                  CONTINUE
                  END IF
               CURSHF(JA(J)) = 0.
               IF (ISVLBA) CALL RCOPY (OFFW, BPBUF(BINX), VLBUF(VINX))
               END IF
 100        CONTINUE
         END IF
      IERR = 0
      GO TO 999
C
 990  CALL MSGWRT (6)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('BPGET: ERROR',I3,1X,A,'ING ',A)
      END
