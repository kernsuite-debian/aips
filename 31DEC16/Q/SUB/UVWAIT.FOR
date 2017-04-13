      SUBROUTINE UVWAIT (DISKI, CNOSCI, INCATB, LUN, JBUFSZ, FREQS,
     *   BUFF1, BUFF2, BUFF3, IERR)
C-----------------------------------------------------------------------
C! Does uniform or other UV-data weighting
C# UV-util AP-appl
C-----------------------------------------------------------------------
C;  Copyright (C) 1995, 1997, 1999, 2006, 2008-2009, 2012, 2014-2015
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
C   Does uniform weighting - or simpler semi-natural weightings - plus
C   any tapering of UV data.  Uses the "AP" to grid the weights and
C   count the samples and creates/destroys a scratch file if needed.
C      The contents of DCAT.INC and DUVH.INC commons are modified.
C   Input:
C      DISKI    I        Input file disk number for cataloged files,
C                        .LE. 0 => /CFILES/ scratch file.
C      CNOSCI   I        Input file catalog slot number or /CFILES/
C                        scratch file number.
C      LUN      I(3)     LUNs for uv file(twice) and scratch
C      INCATB   I(256)   The input uv file catalog header
C      JBUFSZ   I        I/O buffer size in AIPS bytes
C      FREQS    D(*)     Frequencies of the channels to be weighted
C   Input/Output:
C      BUFF1    R(*)     I/O buffer (uv read)
C      BUFF2    R(*)     I/O buffer (uv write)
C      BUFF3    R(*)     I/O buffer (grid)
C   Common: DMPR.INC
C      NCHAVG            Channels to average
C      FREQUV            Reference frequency of UV data set
C      NXUNF, NYUNF      Uniform grid size
C      UNFBOX            Weighting function support radius
C      NBXFUN            Weighting function type
C      DOUNIF            Uniform weighting ?
C      WTPOWR            Take input wt**WTPOWR
C      ROBUST            Robust weighting control
C      TAPERU, TAPERV    Tapering
C   Output:
C      IERR     I        Return code, 0=OK else failed.
C-----------------------------------------------------------------------
      INTEGER   DISKI, CNOSCI, LUN(3), INCATB(256), JBUFSZ, IERR
      REAL      BUFF1(*), BUFF2(*), BUFF3(*)
      DOUBLE PRECISION FREQS(*)
C
      INTEGER   APSIZE, JERR, ISIZE, VOL(3), FILCNO(3), I, J, I1, I2,
     *   NEED, NROW, NPTS(2), IND(3), IAPC(20), EXTRA, NMAX, MAXREC, UV,
     *   BO, VO, WIN(4), UHI, ULO, ILENBU , NIO, LUV, INCNT, CNT, IU,
     *   BIND(3), INPTR, N, OPTR, NR, INSCR, END1, END2, LOCS, KEYTYP,
     *   NU, NFI, NNUU, DROUND, H1, H2, L1, L2, HR, LR
      LOGICAL   DODISK, DOROB, T, F, DOTAPR
      CHARACTER PHNAME*48, KEYW*8
      INCLUDE 'INCS:PSTD.INC'
      INCLUDE 'INCS:PUVD.INC'
      REAL      RTEMP
      DOUBLE PRECISION DFMAX, DFMIN, WTIN, WTOUT, SWTIN, SWTOU, SWTOS,
     *   WTS, WTC, FRSCAL(MAXCIF), FMAX, FMIN, USCAL, VSCAL, TEMP, WTP,
     *   AVGRID, UU, VV, NOISR, WTSCAL, UUMAX, TUC, TVC, UUS, VVS
      SAVE FRSCAL
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DGDS.INC'
      INCLUDE 'INCS:DMPR.INC'
      INCLUDE 'INCS:DAPM.INC'
      DATA T, F /.TRUE., .FALSE./
      DATA BO, VO /1, 0/
C-----------------------------------------------------------------------
      IERR = 0
C                                       "AP" size
      APSIZE = PSAPNW * 1024 - 20
      WTSCAL = 1.0D0
C                                       How many rows can be weighted
      CALL COPY (256, INCATB, CATBLK)
      CALL UVPGET (JERR)
      DODISK = .FALSE.
      CALL FILL (3, -1, IND)
C                                       Open UV data file IO
      IF (DISKI.GT.0) THEN
         VOL(1) = DISKI
         VOL(2) = DISKI
         FILCNO(1) = CNOSCI
         FILCNO(2) = CNOSCI
         CALL ZPHFIL ('UV', VOL(1), FILCNO(1), 1, PHNAME, IERR)
      ELSE
         VOL(1) = SCRVOL(CNOSCI)
         VOL(2) = SCRVOL(CNOSCI)
         FILCNO(1) = SCRCNO(CNOSCI)
         FILCNO(2) = SCRCNO(CNOSCI)
         CALL ZPHFIL ('SC', VOL(1), FILCNO(1), 1, PHNAME, IERR)
         END IF
      CALL ZOPEN (LUN(1), IND(1), VOL(1), PHNAME, T, F, T, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1000) IERR, 'OPEN', 'UV data scratch file'
         GO TO 990
         END IF
      CALL ZOPEN (LUN(2), IND(2), VOL(2), PHNAME, T, F, T, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1000) IERR, 'OPEN', 'UV data scratch file'
         GO TO 990
         END IF
C                                       Uniform weighting
      DFMAX = FREQS(1)
      DFMIN = FREQS(1)
      FMAX = FREQS(1) / FREQUV
      FMIN = FREQS(1) / FREQUV
      DO 10 I = 1,NCHAVG
         DFMAX = MAX (DFMAX, FREQS(I))
         DFMIN = MIN (DFMIN, FREQS(I))
         FRSCAL(I) = FREQS(I) / FREQUV
         FMAX = MAX (FMAX, FRSCAL(I))
         FMIN = MIN (FMIN, FRSCAL(I))
 10      CONTINUE
      IF (DOUNIF) THEN
         MSGTXT = 'UVWAIT: begin finding uniform weights'
         CALL MSGWRT (2)
C                                       get the max U
         UUMAX = -1.0D3
         CALL CATKEY ('REED', VOL(1), FILCNO(1), 'MAXBLINE', 1, LOCS,
     *      RTEMP, KEYTYP, BUFF1, IERR)
         UUMAX = RTEMP
         IF (IERR.NE.0) UUMAX = -1.D3
C                                       make odd
         NXUNF = ((NXUNF-1)/2) * 2 + 1
         NYUNF = ((NYUNF-1)/2) * 2 + 1
         USCAL = ABS(CELLSG(1)) * NXUNF / RAD2AS
         VSCAL = ABS(CELLSG(2)) * NYUNF / RAD2AS
         NU = NXUNF / 2 + 1
         NNUU = NU
         IF (UUMAX.GT.0.0) THEN
            NNUU = UUMAX * USCAL * FMAX + 0.9D0
            I = 1 + UNFBOX + NNUU
            NU = MIN (I, NU)
            END IF
         NROW = NU + 2 * UNFBOX
         WIN(1) = 1
         WIN(2) = NU
         WIN(3) = NYUNF
         WIN(4) = 1
         I2 = NROW
         I = 20 * LREC + (UNFBOX+1)**2 + 20 + NCHAVG
         I1 = 1
         DOROB = ((ROBUST.LE.7.) .AND. (ROBUST.GE.-7.))
         IF (DOROB) I1 = 2
         NEED = I1 * NYUNF * NROW + I
         I1 = (NEED - 1) / 1024 + 10
         CALL QINIT (I1, I2, I)
         IF ((I.EQ.0) .OR. (PSAPNW.EQ.0)) THEN
            MSGTXT = 'UVWAIT COULD NOT GET NEEDED MEMORY'
            GO TO 990
            END IF
         APSIZE = PSAPNW * 1024 - 20
         DODISK = NEED.GE.APSIZE
         IF (DODISK) THEN
            NROW = ((DFMAX-DFMIN)/DFMAX) * NNUU + 2.99 + 2*UNFBOX
            NEED = I1 * NYUNF * NROW + I
            EXTRA = APSIZE - NEED
            IF (NEED.GE.APSIZE) THEN
               WRITE (MSGTXT,1010) NEED, APSIZE
               GO TO 990
               END IF
            IF (ISORT(1:1).NE.'X') THEN
               MSGTXT = 'UVWAIT: DATA MUST BE XY SORTED, NOT ' // ISORT
               GO TO 990
               END IF
C                                       Create the disk scratch grid
            NPTS(1) = NYUNF
            NPTS(2) = NU
            CALL MAPSIZ (2, NPTS, ISIZE)
            CALL SCREAT (ISIZE, BUFF3, IERR)
            INSCR = NSCR
            VOL(3) = SCRVOL(NSCR)
            FILCNO(3) = SCRCNO(NSCR)
            CALL ZPHFIL ('SC', VOL(3), FILCNO(3), 1, PHNAME, IERR)
            CALL ZOPEN (LUN(3), IND(3), VOL(3), PHNAME, T, T, T, IERR)
            IF (IERR.NE.0) THEN
               WRITE (MSGTXT,1000) IERR, 'OPEN', 'WEIGHT GRID'
               GO TO 990
               END IF
            END IF
C                                       Grid addresses
         IAPC(1) = NU
         IAPC(2) = NYUNF
         IAPC(3) = UNFBOX
         IAPC(4) = UNFBOX
         IAPC(5) = NCHAVG
         IAPC(6) = 0
         IAPC(7) = NYUNF / 2 + 1
         IAPC(14) = 18
         IAPC(11) = 20
         IAPC(10) = 20 + NCHAVG
         UV = IAPC(10) + (1 + UNFBOX) * (1 + UNFBOX)
         IAPC(12) = UV + ILOCU
         IAPC(13) = UV + NRPARM + 2
         IAPC(9) = -1
         IF (DODISK) THEN
            IF (DOROB) THEN
               I1 = (APSIZE * 0.333) / NYUNF
               NROW = MIN (I2, MAX (I1, NROW))
            ELSE
               I1 = (APSIZE * 0.666) / NYUNF
               NROW = MIN (I2, MAX (NROW, I1))
               END IF
            END IF
         NMAX = NYUNF * NROW
         IF (DOROB) THEN
            IAPC(9) = APSIZE - NMAX
            IAPC(8) = IAPC(9) - NMAX
         ELSE
            IAPC(8) = APSIZE - NMAX
            END IF
         CALL FILL (6, 0, IAPC(15))
         MAXREC = (IAPC(8) - UV) / LREC - 1
         IF (MAXREC.LT.1) THEN
            MSGTXT = 'UVWAIT: MAXREC < 1 -> BOO-BOO'
            GO TO 990
            END IF
C                                       AP
         CALL QPUT (IAPC, 0, 20, 1)
         CALL QWD
         CALL WTFUNC (UNFBOX, NBXFUN, IAPC(10), BUFF2)
         IF (DOROB) CALL QVCLR (IAPC(9), 1, NMAX)
         CALL QVCLR (IAPC(8), 1, NMAX)
         UHI = NU - 1 + UNFBOX
         ULO = UHI - NROW + 1
         CALL QDPUT (FRSCAL, IAPC(11), NCHAVG)
C                                       Init UV read
         ILENBU = 0
         CALL UVINIT ('READ', LUN(1), IND(1), NVIS, VO, LREC, ILENBU,
     *      JBUFSZ, BUFF1, BO, BIND(1), IERR)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,1000) IERR, 'INIT', 'READ UV SCRATCH'
            GO TO 990
            END IF
C                                       Init grid write if needed
         IF (DODISK) THEN
            CALL MINIT ('WRIT', LUN(3), IND(3), IAPC(2), IAPC(1), WIN,
     *         BUFF3, JBUFSZ, BO, IERR)
            IF (IERR.NE.0) THEN
               WRITE (MSGTXT,1000) IERR, 'INIT', 'WRITE WEIGHT GRID'
               GO TO 990
               END IF
            END IF
         LUV = UV
         INCNT = 0
         CNT = 0
         WTP = MAX (0.0, WTPOWR)
C                                       loop to read UV
 100     CALL UVDISK ('READ', LUN(1), IND(1), BUFF1, NIO, BIND(1), IERR)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,1000) IERR, 'READ', 'UV SCRATCH'
            GO TO 990
            END IF
         INPTR = BIND(1)
C                                       loop through samples in buffer
         IF (NIO.GT.0) THEN
            DO 120 I = 1,NIO
               J = INPTR + ILOCU
               IF (BUFF1(J).LT.0.0) THEN
                  BUFF1(J) = -BUFF1(J)
                  BUFF1(J+1) = -BUFF1(J+1)
                  END IF
               TEMP = BUFF1(J) * USCAL * FMAX
               I2 = DROUND (TEMP) + UNFBOX
               TEMP = BUFF1(J) * USCAL * FMIN
               I1 = DROUND (TEMP) - UNFBOX
               BUFF1(J) = BUFF1(J) * USCAL
               BUFF1(J+1) = BUFF1(J+1) * VSCAL
               IF ((I2.GT.UHI) .OR. ((.NOT.DODISK) .AND. (I1.LT.ULO)))
     *            THEN
                  WRITE (MSGTXT,1100) I1, I2, ULO, UHI
                  GO TO 990
                  END IF
C                                       Modify the weights
               IF (WTP.LT.0.9) THEN
                  N = INPTR + NRPARM + 2
                  DO 105 J = 1,NCHAVG
                     TEMP = BUFF1(N)
                     IF (TEMP.GT.0) THEN
                        IF (WTP.LT.0.1) THEN
                           BUFF1(N) = 1.0
                        ELSE
                           BUFF1(N) = TEMP ** WTP
                           END IF
                        END IF
                     N = N + 3
 105                 CONTINUE
                  END IF
C                                       need to finish part of grid
               IF (I1.LT.ULO) THEN
C                                       load any data
                  IF (CNT.GT.0) THEN
                     N = LREC * CNT
                     CALL QPUT (BUFF1(INPTR-N), LUV, N, 2)
                     CALL QWD
                     LUV = LUV + N
                     INCNT = INCNT + CNT
                     CNT = 0
                     END IF
C                                       grid loaded data
                  IF (INCNT.GT.0) THEN
                     CALL QWTGRD (ULO, UHI, INCNT, LREC)
                     CALL QWR
                     INCNT = 0
                     LUV = UV
                     END IF
C                                       sum, read, write completed rows
                  N = UHI - I2
                  DO 110 J = 1,N
                     IU = UHI + 1 - J
                     IF (IU.LT.NU) THEN
                        CALL MDISK ('WRIT', LUN(3), IND(3), BUFF3,
     *                     BIND(3), IERR)
                        IF (IERR.NE.0) THEN
                           WRITE (MSGTXT,1000) IERR, 'WRIT',
     *                        'WEIGHT GRID'
                           GO TO 990
                           END IF
                        IF (IU.GE.ULO) THEN
                           CALL QWTSUM (IU, ULO, UHI)
                           CALL QWTGET (IU, ULO, UHI, BUFF3(BIND(3)))
                        ELSE
                           CALL QWTSUM (IU, ULO, UHI)
                           CALL RFILL (NYUNF, 0.0, BUFF3(BIND(3)))
                           END IF
                        END IF
 110                 CONTINUE
C                                       shift remaining AP grid
                  IF (I2.GE.ULO) THEN
                     N = (I2 - ULO + 1) * NYUNF
                     END1 = IAPC(8) + N - 1
                     END2 = IAPC(8) + NMAX - 1
                     CALL QVMOV (END1, -1, END2, -1, N)
                     CALL QVCLR (IAPC(8), 1, NMAX-N)
                     IF (DOROB) THEN
                        END1 = IAPC(9) + N - 1
                        END2 = IAPC(9) + NMAX - 1
                        CALL QVMOV (END1, -1, END2, -1, N)
                        CALL QVCLR (IAPC(9), 1, NMAX-N)
                        END IF
C                                       no rest in ap
                  ELSE
                     CALL QVCLR (IAPC(8), 1, NMAX)
                     IF (DOROB) CALL QVCLR (IAPC(9), 1, NMAX)
                     END IF
                  UHI = I2
                  ULO = UHI - NROW + 1
                  END IF
C                                       include this sample
               CNT = CNT + 1
               INPTR = INPTR + LREC
C                                       does this fill the AP?
               IF (CNT+INCNT.GE.MAXREC) THEN
C                                       load any data
                  IF (CNT.GT.0) THEN
                     N = LREC * CNT
                     CALL QPUT (BUFF1(INPTR-N), LUV, N, 2)
                     CALL QWD
                     INCNT = INCNT + CNT
                     LUV = LUV + N
                     CNT = 0
                     END IF
C                                       grid loaded data
                  IF (INCNT.GT.0) THEN
                     CALL QWTGRD (ULO, UHI, INCNT, LREC)
                     CALL QWR
                     INCNT = 0
                     LUV = UV
                     END IF
                  END IF
 120           CONTINUE
C                                       load any data
            IF (CNT.GT.0) THEN
               N = LREC * CNT
               CALL QPUT (BUFF1(INPTR-N), LUV, N, 2)
               CALL QWD
               INCNT = INCNT + CNT
               LUV = LUV + N
               CNT = 0
               END IF
C                                       loop for more data
            GO TO 100
            END IF
C                                       data all read: grid last
         IF (INCNT.GT.0) THEN
            CALL QWTGRD (ULO, UHI, INCNT, LREC)
            CALL QWR
            INCNT = 0
            LUV = UV
            END IF
C                                       sum, read, write completed rows
         N = UHI + 1
         IF (DODISK) THEN
            DO 130 J = 1,N
               IU = UHI + 1 - J
               IF (IU.LT.NU) THEN
                  CALL MDISK ('WRIT', LUN(3), IND(3), BUFF3, BIND(3),
     *               IERR)
                  IF (IERR.NE.0) THEN
                     WRITE (MSGTXT,1000) IERR, 'WRIT', 'WEIGHT GRID'
                     GO TO 990
                     END IF
                  IF (IU.GE.ULO) THEN
                     CALL QWTSUM (IU, ULO, UHI)
                     CALL QWTGET (IU, ULO, UHI, BUFF3(BIND(3)))
                  ELSE
                     CALL QWTSUM (IU, ULO, UHI)
                     CALL RFILL (NYUNF, 0.0, BUFF3(BIND(3)))
                     END IF
                  END IF
 130           CONTINUE
            CALL MDISK ('FINI', LUN(3), IND(3), BUFF3, BIND(3), IERR)
            IF (IERR.NE.0) THEN
               WRITE (MSGTXT,1000) IERR, 'FINISH', 'WEIGHT GRID'
               GO TO 990
               END IF
C                                       Just do sums & Hermitian
         ELSE
            DO 140 J = 1,N
               IU = UHI + 1 - J
               IF (IU.LT.NU) CALL QWTSUM (IU, ULO, UHI)
 140           CONTINUE
            END IF
C                                       Read back the average grid sum
         IF (DOROB) THEN
            CALL QDGET (AVGRID, 19, 1)
            WRITE (MSGTXT,1140) AVGRID
            CALL MSGWRT (2)
            WTSCAL = AVGRID
            AVGRID = AVGRID * (10.0D0 ** (ROBUST)) / 5.0D0
            WRITE (MSGTXT,1141) AVGRID
            CALL MSGWRT (2)
            WTSCAL = WTSCAL + AVGRID
         ELSE
            AVGRID = 0.0D0
            MSGTXT = 'UVWAIT: Adding no temperance (S) term'
            CALL MSGWRT (2)
            END IF
C                                       No Uniform - get scales
      ELSE
         IF (NXUNF.LE.0) NXUNF = NXMAX
         IF (NYUNF.LE.0) NYUNF = NYMAX
C                                       make odd
         NXUNF = ((NXUNF-1)/2) * 2 + 1
         NYUNF = ((NYUNF-1)/2) * 2 + 1
         USCAL = ABS(CELLSG(1)) * NXUNF / RAD2AS
         VSCAL = ABS(CELLSG(2)) * NYUNF / RAD2AS
         END IF
C                                       Init summing for effective S/N
      MSGTXT = 'UVWAIT: begin applying uniform or other weights'
      CALL MSGWRT (2)
      SWTIN = 0.0D0
      SWTOU = 0.0D0
      SWTOS = 0.0D0
      WTP = ABS (WTPOWR)
      WTS = 0.0D0
      WTC = 0.0D0
C                                       Taper
      IF (TAPERU.GT.0.0) THEN
         TUC = LOG(0.3D0) / (TAPERU*1.D3*USCAL)**2
      ELSE
         TUC = 0.0D0
         END IF
      IF (TAPERV.GT.0.0) THEN
         TVC = LOG(0.3D0) / (TAPERV*1.D3*VSCAL)**2
      ELSE
         TVC = 0.0D0
         END IF
      DOTAPR = (TUC.LT.0.0D0) .OR. (TVC.LT.0.0D0)
C                                       Init UV read
      ILENBU = 0
      CALL UVINIT ('READ', LUN(1), IND(1), NVIS, VO, LREC, ILENBU,
     *   JBUFSZ, BUFF1, BO, BIND(1), IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1000) IERR, 'INIT', 'READ UV SCRATCH'
         GO TO 990
         END IF
C                                       Init UV write
      CALL UVINIT ('WRIT', LUN(2), IND(2), NVIS, VO, LREC, ILENBU,
     *   JBUFSZ, BUFF2, BO, BIND(2), IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1000) IERR, 'INIT', 'WRITE UV SCRATCH'
         GO TO 990
         END IF
      OPTR = BIND(2)
C                                       Init grid write if needed
      IF (DODISK) THEN
         CALL MINIT ('READ', LUN(3), IND(3), IAPC(2), IAPC(1), WIN,
     *      BUFF3, JBUFSZ, BO, IERR)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,1000) IERR, 'INIT', 'READ WEIGHT GRID'
            GO TO 990
            END IF
C                                       Refill the AP
         UHI = NU - 1
         ULO = MAX (0, UHI - NROW + 1)
         N = UHI - MAX (0, ULO) + 1
         DO 190 J = 1,N
            CALL MDISK ('READ', LUN(3), IND(3), BUFF3, BIND(3), IERR)
            IF (IERR.NE.0) THEN
               WRITE (MSGTXT,1000) IERR, 'READ', 'WEIGHT GRID'
               GO TO 990
               END IF
            IU = (UHI + 1 - J - ULO) * IAPC(2) + IAPC(8)
            CALL QPUT (BUFF3(BIND(3)), IU, IAPC(2), 2)
 190        CONTINUE
         END IF
C                                       Begin weighting loop.
C                                       Read vis record.
 200  CALL UVDISK ('READ', LUN(1), IND(1), BUFF1, NIO, BIND(1), IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1000) IERR, 'READ', 'UV SCRATCH'
         GO TO 990
         END IF
      INPTR = BIND(1)
C                                       loop through record
      IF (NIO.GT.0) THEN
         DO 240 I = 1,NIO
C                                       Copy vis.
            CALL RCOPY (LREC, BUFF1(INPTR), BUFF2(OPTR))
            J = INPTR + ILOCU
            UU = BUFF1(J) * USCAL
            VV = BUFF1(J+1) * VSCAL
            IF (UU.LT.0.0) THEN
               UU = -UU
               VV = -VV
               END IF
            IF (DOUNIF) THEN
               TEMP = UU * FMAX
               I2 = DROUND (TEMP)
               TEMP = UU * FMIN
               I1 = DROUND (TEMP)
               IF ((I2.GT.UHI) .OR. ((.NOT.DODISK) .AND. (I1.LT.ULO)))
     *            THEN
                  WRITE (MSGTXT,1100) I1, I2, ULO, UHI
                  GO TO 990
                  END IF
C                                       done with part of grid
               IF (I1.LT.ULO) THEN
C                                       shift remaining AP grid
                  IF (I2.GE.ULO-1) THEN
                     NR = I2 - ULO + 1
                     N = NR * NYUNF
                     END1 = IAPC(8) + N - 1
                     END2 = IAPC(8) + NMAX - 1
                     IF (N.GT.0) CALL QVMOV (END1, -1, END2, -1, N)
C                                       skip some rows
                  ELSE
                     NR = 0
                     IU = ULO - 1 - I2
                     DO 210 J = 1,IU
                        CALL MDISK ('READ', LUN(3), IND(3), BUFF3,
     *                     BIND(3), IERR)
                        IF (IERR.NE.0) THEN
                           WRITE (MSGTXT,1000) IERR, 'READ',
     *                        'WEIGHT GRID'
                           GO TO 990
                           END IF
 210                    CONTINUE
                     END IF
                  UHI = I2
                  ULO = UHI - NROW + 1
C                                       read in more rows
                  N = UHI - MAX (0, ULO) + 1
                  NR = NR + 1
                  DO 220 J = NR,N
                     CALL MDISK ('READ', LUN(3), IND(3), BUFF3, BIND(3),
     *                  IERR)
                     IF (IERR.NE.0) THEN
                        WRITE (MSGTXT,1000) IERR, 'READ', 'WEIGHT GRID'
                        GO TO 990
                        END IF
                     IU = (UHI + 1 - J - ULO) * IAPC(2) + IAPC(8)
                     CALL QPUT (BUFF3(BIND(3)), IU, IAPC(2), 2)
 220                 CONTINUE
                  END IF
               END IF
C                                       loop over freq
            N = OPTR + NRPARM + 2
            DO 230 J = 1,NCHAVG
               WTIN = BUFF2(N)
               IF (WTIN.GT.0.0) THEN
C                                       Modify the weights
                  WTOUT = WTIN
                  IF (WTP.LT.0.9) THEN
                     IF (WTP.LT.0.1) THEN
                        WTOUT = 1.0D0
                     ELSE
                        WTOUT = WTIN ** WTP
                        END IF
                     END IF
C                                       Actual u,v
                  IF (DOTAPR) THEN
                     UUS = UU * FRSCAL(J)
                     VVS = VV * FRSCAL(J)
                     WTOUT = WTOUT * EXP (TUC*UUS*UUS + TVC*VVS*VVS)
                     END IF
C                                       uniform weighting
                  IF (DOUNIF) THEN
                     I1 = UU * FRSCAL(J) + 0.5D0
                     I2 = VV * FRSCAL(J) + 0.5D0 + IAPC(7)
                     NR = (I1 - ULO) * IAPC(2) + I2 + IAPC(8)
                     CALL QDGET (TEMP, NR, 1)
C                                       0.0 is wrong
C                                       if round off issue
                     IF (TEMP.LE.0.0D0) THEN
                        H1 = UU * FRSCAL(J) + 0.51D0
                        H2 = VV * FRSCAL(J) + 0.51D0 + IAPC(7)
                        L1 = UU * FRSCAL(J) + 0.49D0
                        L2 = VV * FRSCAL(J) + 0.49D0 + IAPC(7)
                        HR = (H1 - ULO) * IAPC(2) + H2 + IAPC(8)
                        LR = (L1 - ULO) * IAPC(2) + L2 + IAPC(8)
                        IF (HR.NE.NR) CALL QDGET (TEMP, HR, 1)
                        IF (LR.NE.NR) CALL QDGET (TEMP, LR, 1)
                        END IF
                     IF (TEMP.LE.0.0D0) THEN
                        WRITE (MSGTXT,1220) I1, I2, NR, TEMP
                        CALL MSGWRT (8)
                        END IF
                     WTS = WTS + TEMP
                     WTC = WTC + 1.0D0
C                                       add robust term
                     TEMP = TEMP + AVGRID
                     IF (TEMP.LE.0.0) TEMP = 1.0D0
                     WTOUT = WTOUT * WTSCAL / TEMP
                     END IF
C                                       sums
                  SWTIN = SWTIN + WTIN
                  SWTOU = SWTOU + WTOUT
                  SWTOS = SWTOS + WTOUT * WTOUT / WTIN
                  BUFF2(N) = WTOUT
                  END IF
               N = N + 3
 230           CONTINUE
            INPTR = INPTR + LREC
            OPTR = OPTR + LREC
 240        CONTINUE
C                                      Write
         N = NIO
         CALL UVDISK ('WRIT', LUN(2), IND(2), BUFF2, N, BIND(2), IERR)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,1000) IERR, 'WRIT', 'UV SCRATCH'
            GO TO 990
            END IF
         OPTR = BIND(2)
         GO TO 200
         END IF
C                                       Finish write.
      N = 0
      CALL UVDISK ('FLSH', LUN(2), IND(2), BUFF2, N, BIND(3), IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1000) IERR, 'FLSH', 'UV SCRATCH'
         GO TO 990
         END IF
C                                       Noise numbers
      WRITE (MSGTXT,1240) SWTIN, SWTOU
      CALL MSGWRT (4)
      IF (SWTOU.LE.1.D-9) SWTOU = 1.0D0
      NOISR = SWTOS * SWTIN / SWTOU**2
      NOISR = SQRT (MAX (0.0D0, NOISR))
      WRITE (MSGTXT,1241) NOISR
      CALL MSGWRT (4)
      IF (WTC.GT.0.0D0) WTS = WTS / WTC
      WRITE (MSGTXT,1242) WTS, WTC
      IF (DOUNIF) CALL MSGWRT (4)
C                                       put NOISR in UV header
      KEYW = 'WTNOISE'
      I = 1
      LOCS = 1
      KEYTYP = 1
      CALL CATKEY ('WRIT', VOL(1), FILCNO(1), KEYW, I, LOCS, NOISR,
     *   KEYTYP, BUFF1, IERR)
C                                       put SUMWTIN in UV header
      KEYW = 'SUMWTIN'
      I = 1
      LOCS = 1
      KEYTYP = 1
      NOISR = SWTIN
      CALL CATKEY ('WRIT', VOL(1), FILCNO(1), KEYW, I, LOCS, NOISR,
     *   KEYTYP, BUFF1, IERR)
C                                       close files
      CALL ZCLOSE (LUN(1), IND(1), IERR)
      CALL ZCLOSE (LUN(2), IND(2), I)
      IF (IERR.EQ.0) IERR = I
C                                       Delete scratch files
      IF (DODISK) THEN
         CALL ZCLOSE (LUN(3), IND(3), I)
         NFI = 1
         CALL MAPCLR (NFI, VOL(3), FILCNO(3), 2, BUFF1)
         IF (INSCR.EQ.NSCR) NSCR = NSCR - 1
         END IF
      GO TO 999
C                                       Error
 990  CALL MSGWRT (7)
      MSGTXT = 'UVWAIT: ERROR WEIGHTING UV DATA'
      CALL MSGWRT (7)
      IERR = MAX (IERR, 1)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('UVWAIT ERROR',I4,1X,A,'ING ',A)
 1010 FORMAT ('UVWAIT: REQUIRE',I9,' HAVE',I9,' WORDS IN AP')
 1100 FORMAT ('UVWAIT SORT ERROR: ROWS',2I6,' OUTSIDE',2I6)
 1140 FORMAT ('UVWAIT: Average grid weight',1PE11.3)
 1141 FORMAT ('UVWAIT: Adding temperance S',1PE11.3)
 1220 FORMAT ('UVWAIT U, V, NR, WT <= 0',2I5,I10,1PE12.4)
 1240 FORMAT ('UVWAIT: Sum of weights in',1PE11.3,' and out',1PE11.3)
 1241 FORMAT ('UVWAIT: Noise is increased by a factor',F6.3,
     *   ' due to weighting')
 1242 FORMAT ('UVWAIT: Average summed weight',1PE11.3,' over',0PF12.0,
     *   ' vis')
      END
