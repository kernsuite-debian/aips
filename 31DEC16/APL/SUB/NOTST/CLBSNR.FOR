      SUBROUTINE CLBSNR (VOBS, IS, JS, WT, NUMBLS, NUMTEL, GAIN, SNR,
     *   CLOSER, SNRMIN, TIME, IF, IST, COUNT, PRTLV, RMSLIM, PRTSOU)
C-----------------------------------------------------------------------
C! Gain Soln: Compute antenna based SNRs from phase residuals.
C# UV Calibration
C-----------------------------------------------------------------------
C;  Copyright (C) 1995-1997, 2006
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
C   CLBSNR computes antenna based signal-to-noise ratioes (SNR) based
C   on phase residuals from a gain model.  The approximation used is
C   that SNR = 1.0 / RMS phase residual.
C      If CLOSER are greater than 0 then any values exceeding these
C   limits will be printed under control of PRTLV
C      Does a weighted solution for the SNR.  If insufficient data is
C   present to compute an RMS but at least one observation exists, the
C   SNR is set to 6.0.
C   Inputs:
C      IS       I(*)     Array of first antenna numbers.
C      JS       I(*)     Array of second antenna numbers.
C      WT       R(*)     Array of visibility baseline weights.
C      NUMBLS   I        Number of observations (baselines)
C      NUMTEL   I        Number of antennas.
C      GAIN     R(2,*)   Antenna gains to be applied (real, imaginary)
C      CLOSER   R(2,2)   (i,j) If (j=1/2) amplitude/phase closure errors
C                        exceed these limits on average (i=1) or
C                        individually (i=2) they will be printed
C      SNRMIN   R        Minimum SNR allowed.
C      TIME     D        Time in days of data, used for labeling.
C      IF       I        IF number used for labeling only.
C      IST      I        Stokes parameter of soln. 1-5 => R,L,R,L,I.
C      PRTLV    I        Print level: 0 none, 1 statistics of failures,
C                        2 individual failures, 3 the antenna SNRs
C      RMSLIM   R        Print only closures > rmslim * expected sigma
C      PRTSOU   C*16     Current source name.
C   Input/Output:
C      VOBS     R(2,*)   Normalized visibility (real, imaginary)
C                        Zero value assumed invalid on input.
C                        On return real part is replaced with the phase
C                        difference.
C      COUNT    I(*)     A work array used for the counts for each
C                        antenna, must be at least MAXANT in size.
C   Outputs:
C      SNR      R(*)     Signal to noise ratio for each antenna.
C                        If an antenna is used in fewer than 3 baselines
C                        the returned value is SNRMIN + 1.0
C-----------------------------------------------------------------------
      CHARACTER PRTSOU*16
      INTEGER   IS(*), JS(*), NUMBLS, NUMTEL, COUNT(*), IF, IST, PRTLV
      REAL      VOBS(2,*), GAIN(2,*), SNR(*), WT(*), CLOSER(2,2),
     *   SNRMIN, RMSLIM
      DOUBLE PRECISION TIME
C
      CHARACTER POL(5)*4
      INTEGER   LOOP, II, JJ, NPRT, ID, IH, IM, LS, BLPRT(3,3), NE
      LOGICAL   DOCLOS, MSGDUN, DOCLS1, DOCLS2
      REAL      ZR, ZI, ZZR, ZZI, PRTSNR, TMTEMP, PHSQ, AE, PE, AW, PW,
     *   NW, BLRPRT(3)
      INCLUDE 'INCS:PUVD.INC'
      REAL      SUMWT(MAXANT)
      INTEGER   ERROR(MAXANT), PERROR(MAXANT)
      INCLUDE 'INCS:DMSG.INC'
      DATA POL /'Rpol','Lpol','Rpol','Lpol','Ipol'/
C-----------------------------------------------------------------------
      DOCLS1 = (CLOSER(1,1) * CLOSER(1,2).GT.1.0E-20) .AND.
     *   (CLOSER(1,1) * CLOSER(1,2).LT.1.0E20) .AND. (PRTLV.GT.0)
      DOCLS2 = ((CLOSER(2,1) * CLOSER(2,2).GT.1.0E-20) .AND.
     *   (CLOSER(2,1) * CLOSER(2,2).LT.1.0E20)) .AND. (PRTLV.GT.0)
      DOCLOS = (DOCLS1) .OR. (DOCLS2)
      MSGDUN = .FALSE.
C                                       Label for any closure errors:
      IF (DOCLOS) THEN
         ID = TIME
         TMTEMP = (TIME - ID) * 24.0
         IH = TMTEMP
         TMTEMP = (TMTEMP - IH) * 60.0
         IM = TMTEMP
         TMTEMP = (TMTEMP - IM) * 60.0
         LS = TMTEMP
         IF (LS.EQ.60) THEN
            LS = 0
            IM = IM + 1
            IF (IM.EQ.60)  THEN
               IM = 0
               IH = IH + 1
               IF (IH.EQ.24) THEN
                  IH = 0
                  ID = ID + 1
                  END IF
               END IF
            END IF
         WRITE (MSGTXT,1000) ID, IH, IM, LS, PRTSOU, IF, POL(IST)
         END IF
C                                       Zero sums, counts etc.
      NPRT = 0
      DO 10 LOOP = 1,NUMTEL
         SNR(LOOP) = 0.0
         SUMWT(LOOP) = 0.0
         COUNT(LOOP) = 0
         ERROR(LOOP) = 0
         PERROR(LOOP) = 0
 10      CONTINUE
C                                       Determine phase residuals.
      NE = 0
      AE = 0.0
      PE = 0.0
      AW = 0.0
      PW = 0.0
      NW = 0.0
      DO 30 LOOP = 1,NUMBLS
         II = IS(LOOP)
         JJ = JS(LOOP)
         ZR = GAIN(1,II) * GAIN(1,JJ) + GAIN(2,II) * GAIN(2,JJ)
         ZI = GAIN(1,II) * GAIN(2,JJ) - GAIN(2,II) * GAIN(1,JJ)
         ZZR = VOBS(1,LOOP) * ZR - VOBS(2,LOOP) * ZI
         ZZI = VOBS(1,LOOP) * ZI + VOBS(2,LOOP) * ZR
         VOBS(2,LOOP) = SQRT (ZZR*ZZR + ZZI*ZZI)
         IF (VOBS(2,LOOP)*WT(LOOP).GT.1.0E-20) THEN
            VOBS(1,LOOP) = ATAN2 (ZZI, ZZR)
            IF (DOCLS1) THEN
               NE = NE + 1
               PE = PE + ABS (VOBS(1,LOOP))
               AE = AE + ABS (LOG10 (ABS (VOBS(2,LOOP)+1.E-20)))
               NW = NW + WT(LOOP)
               PW = PW + ABS (VOBS(1,LOOP)) * WT(LOOP)
               AW = AW + ABS (LOG10 (ABS (VOBS(2,LOOP)+1.E-20))) *
     *            WT(LOOP)
               END IF
            END IF
 30      CONTINUE
C                                       Statistical failure
      IF ((DOCLS1) .AND. (NE.GT.0)) THEN
         PE = PE / NE
         AE = AE / NE
         AE = (10.0 ** AE) - 1.0
         PW = PW / NW
         AW = AW / NW
         AW = (10.0 ** AW) - 1.0
C                                       print header, message
         IF ((AE.GT.CLOSER(1,1)) .OR. (PE.GT.CLOSER(1,2))) THEN
            CALL MSGWRT (4)
            MSGDUN = .TRUE.
            AE = AE * 100.0
            PE = PE * 57.296
            WRITE (MSGTXT,1030) AE, PE
            CALL MSGWRT (4)
            AW = AW * 100.0
            PW = PW * 57.296
            WRITE (MSGTXT,1031) AW, PW
            CALL MSGWRT (4)
            END IF
         END IF
C                                       Sum square residuals
      NE = 0
      DO 50 LOOP = 1,NUMBLS
         IF (VOBS(2,LOOP)*WT(LOOP).GE.1.0E-20) THEN
            PHSQ = VOBS(1,LOOP) * VOBS(1,LOOP) * WT(LOOP)
            II = IS(LOOP)
            COUNT(II) = COUNT(II) + 1
            SNR(II) = SNR(II) + PHSQ
            SUMWT(II) = SUMWT(II) + WT(LOOP)
            JJ = JS(LOOP)
            COUNT(JJ) = COUNT(JJ) + 1
            SNR(JJ) = SNR(JJ) + PHSQ
            SUMWT(JJ) = SUMWT(JJ) + WT(LOOP)
C                                       check closure error
            IF (DOCLS2) THEN
               PE = ABS (VOBS(1,LOOP))
               AE = VOBS(2,LOOP)
               IF ((AE.LT.1.0) .AND. (AE.GT.0.0)) AE = 1.0 / AE
               AE = AE - 1.0
               AW = SQRT (1.0 / WT(LOOP))
               PW = ASIN (MIN (1.0, AW))
               AW = RMSLIM * AW
               PW = RMSLIM * PW
               IF ((AE.GT.CLOSER(2,1)) .OR. (PE.GT.CLOSER(2,2))) THEN
                  ERROR(II) = ERROR(II) + 1
                  ERROR(JJ) = ERROR(JJ) + 1
                  NE = NE + 1
C                                       individual messages
                  IF ((PRTLV.GE.2) .AND. ((AE.GT.AW) .OR. (PE.GT.PW)))
     *               THEN
                     PERROR(II) = PERROR(II) + 1
                     PERROR(JJ) = PERROR(JJ) + 1
C                                       Print header message
                     IF (.NOT.MSGDUN) THEN
                        CALL MSGWRT (4)
                        MSGDUN = .TRUE.
                        END IF
C                                       Flush buffer if full
                     IF (NPRT.GE.3) THEN
                        WRITE (MSGTXT,1035) (BLPRT(1,LS), BLPRT(2,LS),
     *                     BLRPRT(LS), BLPRT(3,LS), LS = 1,NPRT)
                        CALL MSGWRT (4)
                        NPRT = 0
                        END IF
C                                       New entry
                     NPRT = NPRT + 1
                     BLPRT(1,NPRT) = II
                     BLPRT(2,NPRT) = JJ
                     BLPRT(3,NPRT) = PE * 57.296 + 0.5
                     BLRPRT(NPRT) = MAX (-9999., MIN (9999., AE*100.))
                     END IF
                  END IF
               END IF
            END IF
 50      CONTINUE
C                                       Flush closure message buffer
      IF (NPRT.GT.0) THEN
         WRITE (MSGTXT,1035) (BLPRT(1,LOOP), BLPRT(2,LOOP),
     *      BLRPRT(LOOP), BLPRT(3,LOOP), LOOP = 1,NPRT)
         CALL MSGWRT (4)
         NPRT = 0
         END IF
C                                       summary by antenna
      IF (NE.GT.0) THEN
         IF (.NOT.MSGDUN) THEN
            CALL MSGWRT (4)
            MSGDUN = .TRUE.
            END IF
         DO 60 LOOP = 1,NUMTEL
            IF (ERROR(LOOP).GT.0) THEN
               WRITE (MSGTXT,1050) LOOP, ERROR(LOOP), PERROR(LOOP)
               CALL MSGWRT (4)
               END IF
 60         CONTINUE
         END IF
C                                       Convert to SNRs
      DO 70 LOOP = 1,NUMTEL
         IF (COUNT(LOOP).LE.0) SNR(LOOP) = 0.0
C                                       For insufficient data (but
C                                       some) use SNRMIN + 1.0
         IF ((COUNT(LOOP).GE.1) .AND. (COUNT(LOOP).LE.2))
     *      SNR(LOOP) = SNRMIN + 1.0
         IF ((COUNT(LOOP).GE.3) .AND. (SNR(LOOP).GT.1.0E-20))
     *      SNR(LOOP) = 1.0 / SQRT (SNR(LOOP) /
     *      ((COUNT(LOOP)-1) * SUMWT(LOOP)))
C                                       Print result if desired.
         IF ((COUNT(LOOP).GE.1) .AND. (PRTLV.GE.3)) THEN
            PRTSNR = MIN (9999.999, SNR(LOOP))
            WRITE (MSGTXT,1060) LOOP, COUNT(LOOP), PRTSNR
            CALL MSGWRT (4)
            END IF
 70      CONTINUE
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('Closure errors at',I3,'/',I2.2,2(':',I2.2),1X,A12,
     *   ' IF no.',I3,2X,A4)
 1030 FORMAT ('Average closure error',F10.3,'%',F8.2,'d')
 1031 FORMAT ('Average closure error',F10.3,'%',F8.2,'d - weighted avg')
 1035 FORMAT (3(I4.2,'-',I2.2,F7.1,'%',I5,'d'))
 1050 FORMAT ('Antenna ',I2,' had',I5,' excess closure errors,',I5,
     *   ' printed')
 1060 FORMAT ('Antenna(',I2,') ',I3,' obs, SNR =',F10.3)
      END
