LOCAL INCLUDE 'BSTST.INC'
C                                                          Include BSTST
C                                       Local include for BSTST
      REAL      XSIZE(2), XBCH, XECH, XGAUSS, GMAX(4), GPOSN(2,4),
     *   GWIDTH(3,4), DPARM(10), FLUX, FACTOR, APARM(10), XNIT, GAIN,
     *   PRTLEV, XSEQ, XDISK, XYR, XDOTV, XCHAN
      HOLLERITH XNAME(3), XCLASS(2)
      COMMON /INPARM/ XSIZE, XBCH, XECH, XGAUSS, GMAX, GPOSN, GWIDTH,
     *   DPARM, FLUX, FACTOR, APARM, XNIT, GAIN, PRTLEV, XNAME, XCLASS,
     *   XSEQ, XDISK, XYR, XDOTV, XCHAN
C
      INTEGER   NCHAN, NGAUSS, SCRTCH(256), DISK, SEQ, CNO, PLUN, PFIND,
     *   NPARM, IUSER, BCHAN, ECHAN, NITER
      REAL      YS(3), Y0(2)
      CHARACTER NAME*12, CLASS*6
      COMMON /BSTSCO/  SCRTCH, NCHAN, NGAUSS, DISK, SEQ, CNO, PLUN,
     *   PFIND, NPARM, IUSER, YS, Y0, BCHAN, ECHAN, NITER
      COMMON /BSTSCH/ NAME, CLASS
C                                                          End BSTST.
LOCAL END
      PROGRAM BSTST
C-----------------------------------------------------------------------
C! Tests frequency-switch correction
C# Map singledish
C-----------------------------------------------------------------------
C;  Copyright (C) 1996-1999, 2002
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
C   BSTST correctes two beam-switch images (each total power) producing
C   a best estimate of the combination
C   Inputs:
C      AIPS adverb  Prg. name.          Description.
C   Programmer Eric W. Greisen:  August 1996
C-----------------------------------------------------------------------
      CHARACTER PRGM*6
      INTEGER   IRET
      INCLUDE 'BSTST.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DCAT.INC'
      DATA PRGM /'BSTST '/
C-----------------------------------------------------------------------
C                                       Get input parameters create
C                                       output file
      CALL BSTSIN (PRGM, IRET)
C                                       correction/output routine
      IF (IRET.EQ.0) CALL BSTSDO (IRET)
C
      CALL DIE (IRET, SCRTCH)
C
 999  STOP
      END
      SUBROUTINE BSTSIN (PRGN, IRET)
C-----------------------------------------------------------------------
C   BSTSIN gets input parameters for BSTST and creates an output file.
C   Inputs:
C      PRGN   C*6   Program name
C   Output:
C      IRET   I     Error code: 0 => ok
C-----------------------------------------------------------------------
      CHARACTER PRGN*6
      INTEGER   IRET
C
      INTEGER   IERR
      CHARACTER FTYPE*2, STAT*4
      INCLUDE 'BSTST.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DCAT.INC'
C-----------------------------------------------------------------------
C                                       Init for AIPS, disks, ...
      CALL ZDCHIN (.TRUE.)
      CALL VHDRIN
      IRET = 0
      FTYPE = ' '
C                                       Initialize /CFILES/
      NSCR = 0
      NCFILE = 0
      CALL FILL (10, 0, IBAD)
C                                       Get input parameters.
      NPARM = 64
      CALL GTPARM (PRGN, NPARM, RQUICK, XSIZE, SCRTCH, IERR)
      IF (IERR.NE.0) THEN
         RQUICK = .TRUE.
         IRET = 8
         IF (IERR.EQ.1) THEN
            GO TO 999
         ELSE
            WRITE (MSGTXT,1000) IERR, 'OBTAINING INPUT PARAMETERS'
            CALL MSGWRT (8)
            END IF
         END IF
C                                       Restart AIPS
      IF (RQUICK) CALL RELPOP (IRET, SCRTCH, IERR)
      IF (IRET.NE.0) GO TO 999
      NCHAN = XSIZE(1) + 0.5
      IF (NCHAN.LE.16) NCHAN = 1024
      NCHAN = MIN (8192, NCHAN)
      BCHAN = XBCH + 0.1
      IF (BCHAN.LE.0) BCHAN = 1
      ECHAN = XECH + 0.1
      IF (ECHAN.LE.BCHAN+10) ECHAN = NCHAN
      NGAUSS = XGAUSS + 0.5
      IF (NGAUSS.LE.0) NGAUSS = 1
      IF (APARM(1).LE.0.1) APARM(1) = 1.0
      IF (APARM(2).LE.0.1) APARM(2) = 1.0
      IF (APARM(3).LE.0.1) APARM(3) = 1.0
      IF ((GAIN.LE.0.0) .OR. (GAIN.GT.1.0)) GAIN = 0.1
      NITER = XNIT + 0.1
C                                       plot
      IF (PRTLEV.LT.0.0) THEN
         CALL H2CHR (12, 1, XNAME, NAME)
         CALL H2CHR (6, 1, XCLASS, CLASS)
         SEQ = XSEQ + 0.1
         DISK = XDISK + 0.1
         IUSER = NLUSER
         CNO = 1
         CALL CATDIR ('SRCH', DISK, CNO, NAME, CLASS, SEQ, FTYPE,
     *      IUSER, STAT, SCRTCH, IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1010) IRET, NAME, CLASS, SEQ, FTYPE,
     *         DISK, IUSER
            GO TO 990
            END IF
C                                       Get catblk, mark file write
         IF (XDOTV.LE.0.0) THEN
            CALL CATIO ('READ', DISK, CNO, CATBLK, 'WRIT', SCRTCH, IRET)
            IF (IRET.NE.0) THEN
               WRITE (MSGTXT,1000) IRET, 'READ CATALOG HEADER'
               GO TO 990
               END IF
            NCFILE = 1
            FVOL(1) = DISK
            FCNO(1) = CNO
            FRW(1) = 1
            END IF
         END IF
      GO TO 999
C
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('BSTSIN: ERROR',I5,' DOING ',A)
 1010 FORMAT ('ERROR',I3,' FINDING ',A12,'.',A6,'.',I3,1X,A2,
     *   'DISK=',I2,' USER=',I5)
      END
      SUBROUTINE BSTSDO (IRET)
C-----------------------------------------------------------------------
C   BSTSDO does all the work
C   Output:
C      PIMAG   R(*)   memory for plus image
C      MIMAG   R(*)   memory for minus image
C      IRET    I      Return code, 0 => OK, otherwise abort.
C-----------------------------------------------------------------------
      INTEGER   IRET
C
      INTEGER   I, J, N, IC, NSUM, JJ, TVCHN, GRCHN, TVCORN(2), IVER,
     *   PBUFF(256), IROUND, FSUM, CH1, CH2, CH3, CH4, NAC
      REAL      BUFFP(8192), BUFFR(8192), BUFFD(8192), THROWP, THROWM,
     *   CONV(16500), A(4), BSUM, X, XSUMS, XMAX, RMAX, RMIN, DX, DY, Y,
     *   PMAX, PMIN, THRAWP, THRAWM, WP, WM, DP, DM, MMAX, RSUMS, RMAXS,
     *   FSUMS, FMAX, BUFFC(8192), BUFFW(8192), CSUMS, CMAXS, CMAXA,
     *   CAVG, XAVG
      LOGICAL   DOTV, PENUP, WASUP
      CHARACTER PFILE*48, STRNG1*80, STRNG2*80
      INCLUDE 'BSTST.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DGPH.INC'
C-----------------------------------------------------------------------
      DOTV = XDOTV.GT.0.0
      NAC = ECHAN - BCHAN + 1
      TVCHN = 0
      TVCORN(1) = 0
      TVCORN(2) = 0
      GRCHN = IROUND (XCHAN)
      IF (GRCHN.LE.0) GRCHN = 1
      PLUN = 26
      IF (FLUX.GT.0.0) CALL RANDIN (I)
      DO 10 N = 1,NGAUSS
         IF (GWIDTH(1,N).LE.0.01) GWIDTH(1,N) = 3.0
         A(N) = 4.0 * LOG (2.0) / (GWIDTH(1,N) * GWIDTH(1,N))
 10      CONTINUE
C                                       Reference spectrum
      MMAX = 0.0
      DO 20 I = 1,NCHAN
         BUFFD(I) = 0.0
         DO 15 N = 1,NGAUSS
            BUFFD(I) = BUFFD(I) + GMAX(N) * EXP (-A(N) *
     *         (I - GPOSN(1,N)) * (I - GPOSN(1,N)))
 15         CONTINUE
         MMAX = MAX (MMAX, BUFFD(I))
 20      CONTINUE
      MMAX = MMAX / 100.0
      CH1 = APARM(4) + 0.1
      CH2 = APARM(5) + 0.1
      IF (CH1.LE.0) THEN
         DO 21 I = BCHAN,ECHAN
            IF (BUFFD(I).GT.MMAX) THEN
               CH1 = MAX (1,I -1)
               GO TO 22
               END IF
 21         CONTINUE
         END IF
 22   IF (CH2.LE.0) THEN
         DO 23 I = BCHAN,ECHAN
            IF (BUFFD(NCHAN+1-I).GT.MMAX) THEN
               CH2 = MIN (NCHAN,NCHAN+2-I)
               GO TO 24
               END IF
 23         CONTINUE
         END IF
C                                       loop over throws
 24   IF (FACTOR.LE.0.1) FACTOR = 1.0
      DO 100 J = 1,10
         IF (DPARM(J).NE.0.0) THEN
            THROWP = - DPARM(J) / 2.0
            THROWM = DPARM(J) / 2.0
            CH4 = THROWM + 0.5
            CH3 = MAX (BCHAN, CH1-CH4)
            CH4 = MIN (ECHAN, CH2+CH4)
            THRAWP = THROWP * FACTOR * SQRT (APARM(1))
            THRAWM = THROWM * FACTOR / SQRT (APARM(1))
            WP = 1.0 / APARM(3)
            WM = 1.0 / WP
            DP = SQRT (APARM(2))
            DM = 1.0 / DP
            DO 30 I = BCHAN,ECHAN
               BUFFP(I) = 0.0
               DO 25 N = 1,NGAUSS
                  BUFFP(I) = BUFFP(I) + GMAX(N) *
     *               (DP * EXP (-WP*A(N)*((I+THRAWP-GPOSN(1,N))**2)) -
     *               DM * EXP (-WM*A(N)*((I+THRAWM-GPOSN(1,N))**2)))
 25               CONTINUE
               IF (FLUX.GT.0) THEN
                  CALL NOISE (X)
                  BUFFP(I) = BUFFP(I) + FLUX * X
                  END IF
 30            CONTINUE
C                                       Convolving function
            CALL BSCONV (NAC, THROWP, THROWM, 1.0, IC, CONV)
C                                       Do it
            DO 40 N = 1,NAC
               BSUM = 0.0
               NSUM = 0.0
               DO 35 I = 1,NAC
                  JJ = I - N + IC
                  IF ((JJ.GT.0) .AND. (JJ.LE.2*NCHAN) .AND.
     *               (CONV(JJ).NE.0.0)) THEN
                     NSUM = NSUM + 1
                     BSUM = BSUM + BUFFP(I+BCHAN-1) * CONV(JJ)
                     END IF
 35               CONTINUE
               IF (NSUM.GT.0) THEN
                  BUFFR(N+BCHAN-1) = BSUM
               ELSE
                  BUFFR(N+BCHAN-1) = FBLANK
                  END IF
 40            CONTINUE
C                                       Do Clean too
            IF (NITER.GT.0) THEN
               CALL RFILL (8192, FBLANK, BUFFC)
               CALL RCOPY (NAC, BUFFP(BCHAN), BUFFW)
               CALL BSCLEN (NAC, THROWP, THROWM, NITER, GAIN, BUFFW,
     *            BUFFC(BCHAN))
               END IF
C                                       Find plot extrema
            XSUMS = 0.0
            XMAX = 0.0
            RSUMS = 0.0
            RMAXS = 0.0
            FSUMS = 0.0
            CMAXS = 0.0
            CMAXA = 0.0
            XAVG = 0.0
            CAVG = 0.0
            CSUMS = 0.0
            FMAX = 0.0
            NSUM = 0
            FSUM = 0
            RMIN = 0.0
            RMAX = 0.0
            PMIN = 0.0
            PMAX = 0.0
            DO 50 I = BCHAN,ECHAN
               X = BUFFD(I) - BUFFR(I)
               XSUMS = XSUMS + X * X
               XMAX = MAX (XMAX, ABS(X))
               XAVG = XAVG + X
               IF (NITER.GT.0) THEN
                  Y = BUFFD(I) - BUFFC(I)
                  CAVG = CAVG + Y
                  CMAXA = MAX (CMAXA, ABS(Y))
                  END IF
               IF ((I.LE.CH1) .OR. (I.GE.CH2)) THEN
                  NSUM = NSUM + 1
                  RSUMS = RSUMS + X * X
                  RMAXS = MAX (RMAXS, ABS(X))
                  IF (NITER.GT.0) THEN
                     CSUMS = CSUMS + Y * Y
                     CMAXS = MAX (CMAXS, ABS(Y))
                     END IF
                  END IF
               IF ((I.LE.CH3) .OR. (I.GE.CH4)) THEN
                  FSUM = FSUM + 1
                  FSUMS = FSUMS + BUFFP(I) ** 2
                  FMAX = MAX (FMAX, ABS(BUFFP(I)))
                  END IF
               IF (PRTLEV.LT.0.0) THEN
                  RMAX = MAX (RMAX, BUFFD(I))
                  RMAX = MAX (RMAX, BUFFR(I))
                  RMIN = MIN (RMIN, BUFFD(I))
                  RMIN = MIN (RMIN, BUFFR(I))
                  PMIN = MIN (PMIN, BUFFP(I))
                  PMAX = MAX (PMAX, BUFFP(I))
                  IF (NITER.GT.0) THEN
                     RMAX = MAX (RMAX, BUFFC(I))
                     RMIN = MIN (RMIN, BUFFC(I))
                     END IF
                  END IF
 50            CONTINUE
            IF (APARM(6).GT.0.0) THEN
               IF (APARM(8).GT.APARM(7)) RMIN = APARM(7)
               IF (APARM(8).GT.APARM(7)) RMAX = APARM(8)
               IF (APARM(10).GT.APARM(9)) PMIN = APARM(9)
               IF (APARM(10).GT.APARM(9)) PMAX = APARM(10)
               END IF
            X = (PMAX - PMIN) * 0.025
            RMAX = RMAX + X
            RMIN = RMIN - X
            PMAX = PMAX + X
            PMIN = PMIN - X
            XSUMS = SQRT (XSUMS / NAC)
            XAVG = XAVG / NAC
            IF (NSUM.GT.0) RSUMS = SQRT (RSUMS / NSUM)
            IF (FSUM.GT.0) FSUMS = SQRT (FSUMS / FSUM)
            WRITE (MSGTXT,1050) J, THROWP, THROWM, XMAX, XSUMS
            CALL MSGWRT (5)
            WRITE (MSGTXT,1052) CH1, CH2, RMAXS, RSUMS
            CALL MSGWRT (5)
            IF (NITER.GT.0) THEN
               IF (NSUM.GT.0) CSUMS = SQRT (CSUMS / NSUM)
               CAVG = CAVG / NAC
               WRITE (MSGTXT,1053) CH1, CH2, CMAXS, CSUMS
               CALL MSGWRT (5)
               END IF
            WRITE (MSGTXT,1054) CH3, CH4, FMAX, FSUMS
            CALL MSGWRT (5)
C                                       print
            IF (PRTLEV.GE.0.0) THEN
               DO 55 I = BCHAN,ECHAN
                  X = BUFFD(I) - BUFFR(I)
                  IF (ABS(X).GT.PRTLEV) THEN
                     WRITE (MSGTXT,1051) I, BUFFD(I), BUFFR(I), X
                     CALL MSGWRT (4)
                     END IF
 55               CONTINUE
C                                       plot
            ELSE
               IVER = 0
               IF (.NOT.DOTV) THEN
                  CALL MADDEX ('PL', DISK, CNO, CATBLK, PBUFF, .TRUE.,
     *               'WRIT', IVER, IRET)
                  IF (IRET.NE.0) THEN
                     WRITE (MSGTXT,1000) IRET, 'CREATE PLOT FILE'
                     GO TO 990
                     END IF
                  END IF
               CALL ZPHFIL ('PL', DISK, CNO, IVER, PFILE, IRET)
               CALL GINIT (DISK, CNO, PFILE, 0, 1, NPARM, IUSER, DOTV,
     *            TVCHN, GRCHN, TVCORN, CATBLK, PBUFF, PLUN, PFIND,
     *            IRET)
               IF (IRET.NE.0) GO TO 999

               WRITE (STRNG1,1055) THROWP, THROWM, XMAX, XSUMS
               CALL REFRMT (STRNG1, '_', I)
               IF ((FACTOR.NE.1.0) .OR. (APARM(1).NE.1.0) .OR.
     *            (APARM(2).NE.1.0) .OR. (APARM(3).NE.1.0)) THEN
                  WRITE (STRNG2,1056) FACTOR, APARM(1), APARM(2),
     *               APARM(3)
                  CALL REFRMT (STRNG2, '_', I)
               ELSE
                  STRNG2 = ' '
                  END IF
               CALL BSTLAB (NCHAN, NITER, IVER, RMIN, RMAX, PMIN, PMAX,
     *            STRNG1, STRNG2, YS, Y0, XYR, PBUFF, IRET)
               IF (IRET.NE.0) GO TO 999
C                                       label panels: EKH
               X = 0.0
               Y = (RMAX - RMIN) * YS(1) + Y0(1)
               DX = 3.0
               DY = -2.5
               CALL GPOS (X, Y, PBUFF, IRET)
               IF (IRET.NE.0) GO TO 999
               WRITE (STRNG1,1057) 'max err', XMAX
               CALL REFRMT (STRNG1, '_', I)
               CALL GICHAR (1, I, 0, DX, DY, STRNG1, PBUFF, IRET)
               IF (IRET.NE.0) GO TO 999
               WRITE (STRNG1,1057) 'rms err', RSUMS
               DY = DY - 1.333
               CALL GPOS (X, Y, PBUFF, IRET)
               IF (IRET.NE.0) GO TO 999
               CALL REFRMT (STRNG1, '_', I)
               CALL GICHAR (1, I, 0, DX, DY, STRNG1, PBUFF, IRET)
               IF (IRET.NE.0) GO TO 999
               WRITE (STRNG1,1057) 'avg err', XAVG
               DY = DY - 1.333
               CALL GPOS (X, Y, PBUFF, IRET)
               IF (IRET.NE.0) GO TO 999
               CALL REFRMT (STRNG1, '_', I)
               CALL GICHAR (1, I, 0, DX, DY, STRNG1, PBUFF, IRET)
               IF (IRET.NE.0) GO TO 999
               X = 1000.0
               DX = -6.0
               DY = -2.5
               CALL GPOS (X, Y, PBUFF, IRET)
               IF (IRET.NE.0) GO TO 999
               CALL GCHAR (3, 0, DX, DY, 'EKH', PBUFF, IRET)
               IF (IRET.NE.0) GO TO 999
C                                       label Clean
               IF (NITER.GT.0) THEN
                  X = 0.0
                  Y = (RMAX - RMIN) * YS(2) + Y0(2)
                  DX = 3.0
                  DY = -2.5
                  CALL GPOS (X, Y, PBUFF, IRET)
                  IF (IRET.NE.0) GO TO 999
                  WRITE (STRNG1,1057) 'max err', CMAXA
                  CALL REFRMT (STRNG1, '_', I)
                  CALL GICHAR (1, I, 0, DX, DY, STRNG1, PBUFF, IRET)
                  IF (IRET.NE.0) GO TO 999
                  WRITE (STRNG1,1057) 'rms err', CSUMS
                  DY = DY - 1.333
                  CALL GPOS (X, Y, PBUFF, IRET)
                  IF (IRET.NE.0) GO TO 999
                  CALL REFRMT (STRNG1, '_', I)
                  CALL GICHAR (1, I, 0, DX, DY, STRNG1, PBUFF, IRET)
                  IF (IRET.NE.0) GO TO 999
                  WRITE (STRNG1,1057) 'avg err', CAVG
                  DY = DY - 1.333
                  CALL GPOS (X, Y, PBUFF, IRET)
                  IF (IRET.NE.0) GO TO 999
                  CALL REFRMT (STRNG1, '_', I)
                  CALL GICHAR (1, I, 0, DX, DY, STRNG1, PBUFF, IRET)
                  IF (IRET.NE.0) GO TO 999
                  X = 1000.0
                  DX = -8.0
                  DY = -2.5
                  CALL GPOS (X, Y, PBUFF, IRET)
                  IF (IRET.NE.0) GO TO 999
                  CALL GICHAR (1, 5, 0, DX, DY, 'Clean', PBUFF, IRET)
                  IF (IRET.NE.0) GO TO 999
                  END IF
C                                       label difference
               X = 0.0
               Y = (PMAX - PMIN) * YS(3)
               DX = 3.0
               DY = -2.5
               CALL GPOS (X, Y, PBUFF, IRET)
               IF (IRET.NE.0) GO TO 999
               WRITE (STRNG1,1057) 'max',FMAX
               CALL REFRMT (STRNG1, '_', I)
               CALL GICHAR (1, I, 0, DX, DY, STRNG1, PBUFF, IRET)
               IF (IRET.NE.0) GO TO 999
               WRITE (STRNG1,1057) 'rms',FSUMS
               DY = DY - 1.333
               CALL GPOS (X, Y, PBUFF, IRET)
               IF (IRET.NE.0) GO TO 999
               CALL REFRMT (STRNG1, '_', I)
               CALL GICHAR (1, I, 0, DX, DY, STRNG1, PBUFF, IRET)
               IF (IRET.NE.0) GO TO 999
               X = 1000.0
               DX = -13.0
               DY = -2.5
               CALL GPOS (X, Y, PBUFF, IRET)
               IF (IRET.NE.0) GO TO 999
               CALL GICHAR (1, 10, 0, DX, DY, 'Difference', PBUFF, IRET)
               IF (IRET.NE.0) GO TO 999
C                                       plot model
               CALL GLTYPE (2, PBUFF, IRET)
               IF (IRET.NE.0) GO TO 999
               DX = NCHAN + 2
               DX = 1000.0 / DX
               X = DX / 2.0 - DX
               PENUP = .TRUE.
               DO 60 I = 1,NCHAN
                  X = X + DX
                  Y = (BUFFD(I) - RMIN) * YS(1) + Y0(1)
                  WASUP = PENUP
                  PENUP = (BUFFD(I).GT.RMAX) .OR. (BUFFD(I).LT.RMIN)
                  IF (.NOT.PENUP) THEN
                     IF (WASUP) THEN
                        CALL GPOS (X, Y, PBUFF, IRET)
                     ELSE
                        CALL GVEC (X, Y, PBUFF, IRET)
                        END IF
                     IF (IRET.NE.0) GO TO 999
                     END IF
 60               CONTINUE
C                                       plot model on Clean
               IF (NITER.GT.0) THEN
                  X = DX / 2.0 - DX
                  PENUP = .TRUE.
                  DO 65 I = 1,NCHAN
                     X = X + DX
                     Y = (BUFFD(I) - RMIN) * YS(2) + Y0(2)
                     WASUP = PENUP
                     PENUP = (BUFFD(I).GT.RMAX) .OR. (BUFFD(I).LT.RMIN)
                     IF (.NOT.PENUP) THEN
                        IF (WASUP) THEN
                           CALL GPOS (X, Y, PBUFF, IRET)
                        ELSE
                           CALL GVEC (X, Y, PBUFF, IRET)
                           END IF
                        IF (IRET.NE.0) GO TO 999
                        END IF
 65                  CONTINUE
                  END IF
C                                       plot difference spectrum
               X = DX / 2.0 + (BCHAN-2) * DX
               PENUP = .TRUE.
               DO 70 I = BCHAN,ECHAN
                  X = X + DX
                  Y = (BUFFP(I) - PMIN) * YS(3)
                  WASUP = PENUP
                  PENUP = (BUFFP(I).GT.PMAX) .OR. (BUFFP(I).LT.PMIN)
                  IF (.NOT.PENUP) THEN
                     IF (WASUP) THEN
                        CALL GPOS (X, Y, PBUFF, IRET)
                     ELSE
                        CALL GVEC (X, Y, PBUFF, IRET)
                        END IF
                     IF (IRET.NE.0) GO TO 999
                     END IF
 70               CONTINUE
C                                       plot result: EKH
               X = DX / 2.0 + (BCHAN-2) * DX
               PENUP = .TRUE.
               DO 75 I = BCHAN,ECHAN
                  X = X + DX
                  Y = (BUFFR(I) - RMIN) * YS(1) + Y0(1)
                  WASUP = PENUP
                  PENUP = (BUFFR(I).GT.RMAX) .OR. (BUFFR(I).LT.RMIN)
                  IF (.NOT.PENUP) THEN
                     IF (WASUP) THEN
                        CALL GPOS (X, Y, PBUFF, IRET)
                     ELSE
                        CALL GVEC (X, Y, PBUFF, IRET)
                        END IF
                     IF (IRET.NE.0) GO TO 999
                     END IF
 75               CONTINUE
C                                       plot result: Clean
               IF (NITER.GT.0) THEN
                  X = DX / 2.0 + (BCHAN-2) * DX
                  PENUP = .TRUE.
                  DO 80 I = BCHAN,ECHAN
                     X = X + DX
                     Y = (BUFFC(I) - RMIN) * YS(2) + Y0(2)
                     WASUP = PENUP
                     PENUP = (BUFFC(I).GT.RMAX) .OR. (BUFFC(I).LT.RMIN)
                     IF (.NOT.PENUP) THEN
                        IF (WASUP) THEN
                           CALL GPOS (X, Y, PBUFF, IRET)
                        ELSE
                           CALL GVEC (X, Y, PBUFF, IRET)
                           END IF
                        IF (IRET.NE.0) GO TO 999
                        END IF
 80                  CONTINUE
                  END IF
C                                       finish plot and continue?
               GPHPAG = (DOTV) .AND. (J.LT.10) .AND. (DPARM(J+1).NE.0.0)
               WRITE (MSGTXT,1070) IVER
               IF (.NOT.DOTV) CALL MSGWRT (2)
               CALL GFINIS (PBUFF, IRET)
               IF (IRET.NE.0) GO TO 999
               END IF
            END IF
 100     CONTINUE
      GO TO 999
C
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('BSTSDO: ERROR',I5,' DOING ',A)
 1050 FORMAT (I2,': THROWS',2F8.2,'  MAX,RMS',2(1PE13.4))
 1051 FORMAT ('CHAN, MODEL, RESULT',I5,2(0PF10.4),1PE13.4)
 1052 FORMAT ('Non-signal regions EKH ',I4,I5,' MAX,RMS',2F9.4)
 1053 FORMAT ('Non-signal regions Cln ',I4,I5,' MAX,RMS',2F9.4)
 1054 FORMAT ('Non-signal diff regions',I4,I5,' MAX,RMS',2F9.4)
 1055 FORMAT ('Throws',2F8.2,'__max',1PE13.4,'__rms',1PE13.4)
 1056 FORMAT ('Factor',F7.4,'__Ratio throw',F7.4,' Peak',F7.4,' Width',
     *   F7.4)
 1057 FORMAT (A,'_',F8.3)
 1070 FORMAT ('Successful plot file version',I5,'  created.')
      END
      SUBROUTINE NOISE (ANOISE)
C-----------------------------------------------------------------------
C   Random noise generator
C    Output: ANOISE  R    Result
C-----------------------------------------------------------------------
      REAL      ANOISE, TEMP
      INTEGER   J
C-----------------------------------------------------------------------
      ANOISE = -6.0
      DO 10 J = 1,12
         CALL RANDUM (TEMP)
         ANOISE = ANOISE + TEMP
 10      CONTINUE
C
 999  RETURN
      END
      SUBROUTINE BSTLAB (NCHAN, NITER, IVER, RMIN, RMAX, PMIN, PMAX,
     *   STRNG1, STRNG2, YS, Y0, XYR, PBUFF, IERR)
C-----------------------------------------------------------------------
C   Do the init for line drawing, ...
C   Inputs:
C      NCHAN   I      Number channels
C      NITER   I      Number Clean iterations (0 none)
C      IVER    I      File version number
C      RMIN    R      Min value to plot
C      RMAX    R      Max value to plot
C   Output:
C      IERR   I   Error code
C-----------------------------------------------------------------------
      REAL      RMIN, RMAX, PMIN, PMAX, YS(3), Y0(2), XYR
      INTEGER   NCHAN, NITER, IVER, PBUFF(*), IERR
      CHARACTER STRNG1*(*), STRNG2*(*)
C
      REAL      BLC(7), CH(4), TRC(7), X, XYRATO, DX, DY, DCX, DCY, YB,
     *   YINTER(9), YINT, PIXR(2)
      INTEGER   IDEPTH(5), I, LTYPE, IIP, IIF, XINTER(10), XINT,
     *   XLO, XHI, ID(3), IT(3), ITRIM
      CHARACTER STRING*80, CTIME*8, CDATE*12
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DHDR.INC'
      DATA XINTER /2,5,10,20,50,100,200,500,1000,2000/
      DATA YINTER /0.1, 0.2, 0.5, 1., 2., 5., 10., 20., 50./
C-----------------------------------------------------------------------

C                                       number characters around
      CALL RFILL (4, 0.5, CH)
      LTYPE = 3
      PIXR(1) = RMIN
      PIXR(2) = RMAX
      CALL GTICNT (LTYPE, PIXR, I)
      IF (LTYPE.EQ.2) CH(1) = 2.5
      IF (LTYPE.GT.2) CH(1) = I + 4.0
      IF (LTYPE.GT.1) CH(2) = 2.0
      IF (LTYPE.GT.2) THEN
         CH(2) = CH(2) + 2 * 1.333
         IF (STRNG2.NE.' ') CH(2) = CH(2) + 1.333
         END IF
      IF (LTYPE.EQ.2) CH(3) = 2.5
      IF (LTYPE.GT.1) CH(4) = CH(4) + 1.5
      IF (LTYPE.GT.2) CH(4) = CH(4) + 1.333
      CH(4) = CH(4) + 1.333
C                                       Set BLC, TRC, XYRATO.
      CALL RFILL (5, 1.0, BLC(3))
      CALL RFILL (5, 1.0, TRC(3))
      CALL FILL (5, 1, IDEPTH)
      BLC(2) = 0.0
      BLC(1) = 0.0
      TRC(1) = 1000.0
      TRC(2) = 1500.0
      IF (NITER.LE.0) TRC(2) = 1000.0
      XYRATO = XYR
      IF ((XYRATO.LT.0.1) .OR. (XYRATO.GT.10.0)) XYRATO = 1.0
      YS(1) = 500.0 / (RMAX - RMIN)
      YS(2) = YS(1)
      YS(3) = 500.0 / (PMAX - PMIN)
      Y0(2) = 500.
      Y0(1) = 1000.
      IF (NITER.LE.0) Y0(1) = 500.
C                                       Initialize plot file line drw.
      CALL GINITL (BLC, TRC, XYRATO, CH, IDEPTH, PBUFF, IERR)
      IF (IERR.NE.0) GO TO 999
      CALL GLTYPE (1, PBUFF, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Draw borders.
      CALL GPOS (BLC(1), BLC(2), PBUFF, IERR)
      IF (IERR.NE.0) GO TO 999
      CALL GVEC (TRC(1), BLC(2), PBUFF, IERR)
      IF (IERR.NE.0) GO TO 999
      CALL GVEC (TRC(1), TRC(2), PBUFF, IERR)
      IF (IERR.NE.0) GO TO 999
      CALL GVEC (BLC(1), TRC(2), PBUFF, IERR)
      IF (IERR.NE.0) GO TO 999
      CALL GVEC (BLC(1), BLC(2), PBUFF, IERR)
      IF (IERR.NE.0) GO TO 999
      CALL GPOS (BLC(1), Y0(1), PBUFF, IERR)
      IF (IERR.NE.0) GO TO 999
      CALL GVEC (TRC(1), Y0(1), PBUFF, IERR)
      IF (IERR.NE.0) GO TO 999
      IF (NITER.GT.0) THEN
         CALL GPOS (BLC(1), Y0(2), PBUFF, IERR)
         IF (IERR.NE.0) GO TO 999
         CALL GVEC (TRC(1), Y0(2), PBUFF, IERR)
         IF (IERR.NE.0) GO TO 999
         END IF
C                                       draw zeros
      YB = -RMIN * YS(1) + Y0(1)
      IF (YB.GT.1.0) THEN
         CALL GPOS (BLC(1), YB, PBUFF, IERR)
         IF (IERR.NE.0) GO TO 999
         CALL GVEC (TRC(1), YB, PBUFF, IERR)
         IF (IERR.NE.0) GO TO 999
         END IF
      IF (NITER.GT.0) THEN
         YB = -RMIN * YS(2) + Y0(2)
         IF (YB.GT.1.0) THEN
            CALL GPOS (BLC(1), YB, PBUFF, IERR)
            IF (IERR.NE.0) GO TO 999
            CALL GVEC (TRC(1), YB, PBUFF, IERR)
            IF (IERR.NE.0) GO TO 999
            END IF
         END IF
      YB = -PMIN * YS(3)
      IF (YB.GT.1.0) THEN
         CALL GPOS (BLC(1), YB, PBUFF, IERR)
         IF (IERR.NE.0) GO TO 999
         CALL GVEC (TRC(1), YB, PBUFF, IERR)
         IF (IERR.NE.0) GO TO 999
         END IF
C                                       Horizontal ticks
      IIP = 9
      DO 10 I = 1,10
         IIF = NCHAN / XINTER(I)
         IF (IIF.LE.IIP) GO TO 15
 10      CONTINUE
      GO TO 25
C                                       horizontal tick drawing
 15   XINT = XINTER(I)
      XLO = (1 / XINT) * XINT
      IF (XLO.LT.1) XLO = XLO + XINT
      XHI = (NCHAN / XINT) * XINT
      DX = 1000.0 / (NCHAN+2)
      DY = 25.
      DCY = -1.5
      DO 20 I = XLO,XHI,XINT
         X = DX * (1.0 + I)
         WRITE (STRING,1015) I
         CALL CHTRIM (STRING, 4, STRING, IIF)
         DCX = -IIF + 0.5
         CALL GPOS (X, TRC(2), PBUFF, IERR)
         IF (IERR.NE.0) GO TO 999
         CALL GVEC (X, TRC(2)-DY, PBUFF, IERR)
         IF (IERR.NE.0) GO TO 999
         CALL GPOS (X, Y0(1)+DY, PBUFF, IERR)
         IF (IERR.NE.0) GO TO 999
         CALL GVEC (X, Y0(1)-DY, PBUFF, IERR)
         IF (IERR.NE.0) GO TO 999
         IF (NITER.GT.0) THEN
            CALL GPOS (X, Y0(2)+DY, PBUFF, IERR)
            IF (IERR.NE.0) GO TO 999
            CALL GVEC (X, Y0(2)-DY, PBUFF, IERR)
            IF (IERR.NE.0) GO TO 999
            END IF
         CALL GPOS (X, DY, PBUFF, IERR)
         IF (IERR.NE.0) GO TO 999
         CALL GVEC (X, 0.0, PBUFF, IERR)
         IF (IERR.NE.0) GO TO 999
         CALL GCHAR (IIF, 0, DCX, DCY, STRING(:IIF), PBUFF, IERR)
         IF (IERR.NE.0) GO TO 999
 20      CONTINUE
C                                       Vertical ticks
 25   IIP = 7
      DO 30 I = 1,9
         IIF = (RMAX - RMIN) / YINTER(I)
         IF (IIF.LE.IIP) GO TO 35
 30      CONTINUE
      GO TO 45
C                                       vertical tick drawing
 35   YINT = YINTER(I)
      XLO = RMIN / YINT
      IF (XLO*YINT.LT.RMIN) XLO = XLO + 1
      XHI = RMAX / YINT
      DX = 25.
      DCY = -0.5
      YB = 0.
      DO 40 I = XLO,XHI
         X = I * YINT
         WRITE (STRING,1035) X
         IF (YINT.GT.0.9) STRING(5:6) = ' '
         CALL CHTRIM (STRING, 6, STRING, IIF)
         DCX = -IIF - 1.0
         YB = MIN (YB, DCX)
         X = (X - RMIN) * YS(1) + Y0(1)
         CALL GPOS (1000.0, X, PBUFF, IERR)
         IF (IERR.NE.0) GO TO 999
         CALL GVEC (1000.0-DX, X, PBUFF, IERR)
         IF (IERR.NE.0) GO TO 999
         CALL GPOS (DX, X, PBUFF, IERR)
         IF (IERR.NE.0) GO TO 999
         CALL GVEC (0.0, X, PBUFF, IERR)
         IF (IERR.NE.0) GO TO 999
         CALL GCHAR (IIF, 0, DCX, DCY, STRING(:IIF), PBUFF, IERR)
         IF (IERR.NE.0) GO TO 999
         IF (NITER.GT.0) THEN
            X = I * YINT
            X = (X - RMIN) * YS(2) + Y0(2)
            CALL GPOS (1000.0, X, PBUFF, IERR)
            IF (IERR.NE.0) GO TO 999
            CALL GVEC (1000.0-DX, X, PBUFF, IERR)
            IF (IERR.NE.0) GO TO 999
            CALL GPOS (DX, X, PBUFF, IERR)
            IF (IERR.NE.0) GO TO 999
            CALL GVEC (0.0, X, PBUFF, IERR)
            IF (IERR.NE.0) GO TO 999
            CALL GCHAR (IIF, 0, DCX, DCY, STRING(:IIF), PBUFF, IERR)
            IF (IERR.NE.0) GO TO 999
            END IF
 40      CONTINUE
C                                       lower plot
 45   IIP = 7
      DO 50 I = 1,9
         IIF = (PMAX - PMIN) / YINTER(I)
         IF (IIF.LE.IIP) GO TO 55
 50      CONTINUE
      GO TO 70
C                                       vertical tick drawing
 55   YINT = YINTER(I)
      XLO = PMIN / YINT
      IF (XLO*YINT.LT.PMIN) XLO = XLO + 1
      XHI = PMAX / YINT
      DX = 25.
      DCY = -0.5
      DO 60 I = XLO,XHI
         X = I * YINT
         WRITE (STRING,1035) X
         IF (YINT.GT.0.9) STRING(5:6) = ' '
         CALL CHTRIM (STRING, 6, STRING, IIF)
         DCX = -IIF - 1.0
         YB = MIN (YB, DCX)
         X = (X - PMIN) * YS(3)
         CALL GPOS (1000.0, X, PBUFF, IERR)
         IF (IERR.NE.0) GO TO 999
         CALL GVEC (1000.0-DX, X, PBUFF, IERR)
         IF (IERR.NE.0) GO TO 999
         CALL GPOS (DX, X, PBUFF, IERR)
         IF (IERR.NE.0) GO TO 999
         CALL GVEC (0.0, X, PBUFF, IERR)
         IF (IERR.NE.0) GO TO 999
         CALL GCHAR (IIF, 0, DCX, DCY, STRING(:IIF), PBUFF, IERR)
         IF (IERR.NE.0) GO TO 999
 60      CONTINUE
C                                       horizontal axis label
 70   DCY = -1.5 - 1.333
      CALL GPOS (500., 0., PBUFF, IERR)
      IF (IERR.NE.0) GO TO 999
      DCX = -8.5
      CALL GCHAR (17, 0, DCX, DCY, 'Spectral channels', PBUFF, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       extra text
      DCY = DCY - 1.333
      CALL GPOS (500., 0., PBUFF, IERR)
      IF (IERR.NE.0) GO TO 999
      I = ITRIM (STRNG1)
      DCX = -I / 2.0
      CALL GCHAR (I, 0, DCX, DCY, STRNG1(:I), PBUFF, IERR)
      IF (IERR.NE.0) GO TO 999
      IF (STRNG2.NE.' ') THEN
         DCY = DCY - 1.333
         CALL GPOS (500., 0., PBUFF, IERR)
         IF (IERR.NE.0) GO TO 999
         I = ITRIM (STRNG2)
         DCX = -I / 2.0
         CALL GCHAR (I, 0, DCX, DCY, STRNG2(:I), PBUFF, IERR)
         IF (IERR.NE.0) GO TO 999
         END IF
C                                       vertical axis label
      CALL GPOS (0.0, TRC(2)/2.0, PBUFF, IERR)
      IF (IERR.NE.0) GO TO 999
      DCX = YB - 2.0
      CALL GCHAR (10, 1, DCX, 4.0, 'Brightness', PBUFF, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       more labels
      CALL GPOS (BLC(1), TRC(2), PBUFF, IERR)
      IF (IERR.NE.0) GO TO 999
      CALL ZDATE (ID)
      CALL ZTIME (IT)
      CALL TIMDAT (IT, ID, CTIME, CDATE)
      WRITE (STRING,1130) IVER, CDATE, CTIME
      CALL REFRMT (STRING, '_', IIF)
      DCY = 0.6
      DCX = 0.0
      CALL GCHAR (IIF, 0, DCX, DCY, STRING, PBUFF, IERR)
C
 999  RETURN
C-----------------------------------------------------------------------
 1015 FORMAT (I4)
 1035 FORMAT (F6.1)
 1130 FORMAT ('Plot file version',I4,'__created ',A,A)
      END
      SUBROUTINE BSCLEN (NCH, THROWP, THROWM, NITER, GAIN, BUFFW, BUFFC)
C-----------------------------------------------------------------------
C   Does a Clean on the 1-D switched spectrum
C   Inputs:
C      NCH      I      Number channels
C      THROWP   R      Plus spectrum throw (channels)
C      THROWM   R      Minus spectrum throw (channels)
C      NITER    I      Number Clean iterations
C      GAIN     R      Clean loop gain
C   In/out:
C      BUFFW    R(*)   Difference spectrum: in = full, out = residual
C   Output
C      BUFFC    R(*)   Cleaned spectrum
C-----------------------------------------------------------------------
      INTEGER   NCH, NITER
      REAL      THROWP, THROWM, GAIN, BUFFW(*), BUFFC(*)
C
      INTEGER   N, I, I1, I2, IP, IM, IROUND, IX, IN
      REAL      T, TX, TN, TH
      INCLUDE 'INCS:DMSG.INC'
C-----------------------------------------------------------------------
      CALL RFILL (NCH, 0.0, BUFFC)
      IF (NITER.LE.0) GO TO 999
      IP = IROUND (THROWP)
      IM = IROUND (THROWM)
      IF (IP.LT.0) THEN
         I1 = 1 + IM
         I2 = NCH + IP
      ELSE
         I1 = 1 + IP
         I2 = NCH + IM
         END IF
      DO 50 N = 1,NITER
         TX = 0.0
         TN = 0.0
         DO 20 I = I1,I2
            T = BUFFW(I-IP) - BUFFW(I-IM)
            IF (T.GT.TX) THEN
               TX = T
               IX = I
               END IF
            IF (T.LT.TN) THEN
               TN = T
               IN = I
               END IF
 20         CONTINUE
          IF (-TN.GT.TX) THEN
            TX = TN
            IX = IN
            END IF
         TX = GAIN * TX * 0.5
         BUFFC(IX) = BUFFC(IX) + TX
         BUFFW(IX-IP) = BUFFW(IX-IP) - TX
         BUFFW(IX-IM) = BUFFW(IX-IM) + TX
 50      CONTINUE
C                                       max resid
         TH = 0.0
         DO 60 I = 1,NCH
            IF (ABS(BUFFW(I)).GT.TH) TH = ABS (BUFFW(I))
            BUFFC(I) = BUFFC(I) + BUFFW(I)
 60         CONTINUE
         WRITE (MSGTXT,1060) TH
         CALL MSGWRT (3)
C
 999  RETURN
C-----------------------------------------------------------------------
 1060 FORMAT ('BSCLEN: peak residual',1PE12.5)
      END

