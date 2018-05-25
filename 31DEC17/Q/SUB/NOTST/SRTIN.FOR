      SUBROUTINE SRTIN (DISKI, CNOSCI, DISKO, CNOSCO, SORT, ROTATE,
     *   ONEPAS, NSORT, LUN, JBUFSZ, BUFF1, BUFF2, IERR)
C-----------------------------------------------------------------------
C! Reads input data and does (partial) sort in AP memory
C# Sort UV-util AP-appl
C-----------------------------------------------------------------------
C;  Copyright (C) 1995, 2006, 2015
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
C   Sort routine called by UVSORT.
C   Reads input data and does (partial) sort in AP memory. Applies
C   rotation if necessary.
C      If ONEPAS is true the the sort keys are stripped off the output
C   else they are left on.
C   Input:
C      DISKI    I        Input file disk number for cataloged files,
C                        .LE. 0 => /CFILES/ scratch file.
C      CNOSCI   I        Input file catalog slot number or /CFILES/
C                        scratch file number.
C      DISKO    I        Output file disk number for cataloged files,
C                        .LE. 0 => /CFILES/ scratch file.
C      CNOSCO   I        Output file catalog slot number or /CFILES/
C                        scratch file number.
C      SORT    C*2       desired sort order keys:
C                        T=time, B=baseline, X=abs (U), Y=abs (V)
C      ROTATE   R        U, V rotation in deg.
C      ONEPAS   L        If true strip sort keys from output
C      NSORT    I        Number of vis to presort.
C      LUN      I(2)     LUNs for I/O
C   Input from DUVH.INC common:
C      LREC     I        Length of input vis rec.
C      NVIS     I        Number of visibilities
C   Input/Output:
C      JBUFSZ   I        I/O buffer size in bytes
C      BUFF1    R(*)     I/O buffer
C      BUFF2    R(*)     I/O buffer
C   Output:
C      IERR     I        Return code, 0=OK else failed.
C-----------------------------------------------------------------------
      INTEGER   DISKI, CNOSCI, DISKO, CNOSCO, NSORT, LUN(2), JBUFSZ,
     *   IERR
      LOGICAL   ONEPAS
      CHARACTER SORT*2
      REAL      ROTATE, BUFF1(*), BUFF2(*)
C
      INTEGER   APW1, APW2, APVIS, IAPLOC, IAPU, IAPKEY, FINDI, FINDO,
     *   DISK1, DISK2, JERR, LENBU, VO, BO, IBIND, OBIND, NIOUT, LREC2,
     *   LRECO, LOOP, IVIS, NDO, NAP, INIO, I, IAPKE2, I4096
      LOGICAL   EXCL
      REAL      ROT(2), R4096
      CHARACTER FILE1*48, FILE2*48, ERRTXT*80
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DFIL.INC'
C-----------------------------------------------------------------------
C                                       Set AP pointers
      APW1 = 2
      APW2 = APW1 + 2*NSORT + 5
      APVIS = APW2 + NSORT + 3
      IAPU = APVIS + 2 + ILOCU
      I4096 = 0
      R4096 = 4096.0
C                                       Open input
      ERRTXT = 'SRTIN: ERROR OPENING UV DATA FILE'
C                                       Scratch file
      IF (DISKI.LE.0) THEN
         DISK1 = SCRVOL(CNOSCI)
         CALL ZPHFIL ('SC', DISK1, SCRCNO(CNOSCI), 1, FILE1, JERR)
C                                       UV data file
      ELSE
         DISK1 = DISKI
         CALL ZPHFIL ('UV', DISK1, CNOSCI, 1, FILE1, JERR)
         END IF
C                                       Open Output
C                                       Scratch file
      IF (DISKO.LE.0) THEN
         DISK2 = SCRVOL(CNOSCO)
         CALL ZPHFIL ('SC', DISK2, SCRCNO(CNOSCO), 1, FILE2, JERR)
C                                       UV data file
      ELSE
         DISK2 = DISKO
         CALL ZPHFIL ('UV', DISK2, CNOSCO, 1, FILE2, JERR)
         END IF
      EXCL = FILE1.NE.FILE2
      CALL ZOPEN (LUN(1), FINDI, DISK1, FILE1, .TRUE., EXCL, .TRUE.,
     *   IERR)
      IF (IERR.NE.0) GO TO 990
      CALL ZOPEN (LUN(2), FINDO, DISK2, FILE2, .TRUE., EXCL, .TRUE.,
     *   IERR)
      IF (IERR.NE.0) GO TO 990
C                                       Init I/O
      LENBU = 1
      VO = 0
      BO = 1
      ERRTXT = 'SRTIN: ERROR INITIALIZING I/O'
      CALL UVINIT ('READ', LUN(1), FINDI, NVIS, VO, LREC, LENBU,
     *   JBUFSZ, BUFF1, BO, IBIND, IERR)
      IF (IERR.NE.0) GO TO 990
C                                       Output
      LENBU = 1
      LREC2 = LREC + 2
      IF (ONEPAS) THEN
         LRECO = LREC
      ELSE
         LRECO = LREC2
         END IF
      CALL UVINIT ('WRIT', LUN(2), FINDO, NVIS, VO, LRECO, LENBU,
     *   JBUFSZ, BUFF2, BO, OBIND, IERR)
      IF (IERR.NE.0) GO TO 990
C                                       Allocate AP
C                                       size was set by UVSORT
      CALL QINIT (0, 0, NAP)
      CALL QPUT (R4096, I4096, 1, 2)
C                                       Loop thru file by NSORT
      ERRTXT = 'SRTIN: ERROR DOING I/O'
      DO 500 LOOP = 1,NVIS,NSORT
         NDO = MIN (NSORT, (NVIS-LOOP+1))
C                                       Load into AP
         IAPLOC = APVIS
         INIO = 1
         DO 100 IVIS = 1,NDO
            CALL UVDISK ('READ', LUN(1), FINDI, BUFF1, INIO, IBIND,
     *         IERR)
            IF (IERR.NE.0) GO TO 990
            CALL QPUT (BUFF1(IBIND), IAPLOC+2, LREC, 2)
            IAPLOC = IAPLOC + LREC2
 100        CONTINUE
C                                       Rotate if necessary
         IF (ABS (ROTATE).GT.1.0E-10) THEN
            IAPLOC = APVIS
            ROT(1) = 1.0
            ROT(2) = -ROTATE / 57.29578
            CALL QPUT (ROT, IAPLOC, 2, 2)
            CALL QVFILL (IAPLOC, IAPLOC, LREC2, NDO)
            CALL QVFILL (IAPLOC+1, IAPLOC+1, LREC2, NDO)
            CALL QRECT (IAPLOC, LREC2, IAPLOC, LREC2, NDO)
C                                       Rotate U,V
            CALL QCVMUL (IAPLOC, LREC2, IAPU, LREC2, IAPU, LREC2,
     *         NDO, 1)
            END IF
C                                       Set sort keys
         IAPLOC = APVIS
         DO 200 I = 1,2
C                                       Time
            IF (SORT(I:I).EQ.'T') THEN
               IAPKEY = APVIS + 2 + ILOCT
               CALL QVMOV (IAPKEY, LREC2, IAPLOC, LREC2, NDO)
               CALL QVNEG (IAPLOC, LREC2, IAPLOC, LREC2, NDO)
C                                       Baseline
            ELSE IF (SORT(I:I).EQ.'B') THEN
               IF (ILOCB.GE.0) THEN
                  IAPKEY = APVIS + 2 + ILOCB
                  CALL QVMOV (IAPKEY, LREC2, IAPLOC, LREC2, NDO)
                  CALL QVNEG (IAPLOC, LREC2, IAPLOC, LREC2, NDO)
               ELSE
                  IAPKEY = APVIS + 2 + ILOCA1
                  IAPKE2 = APVIS + 2 + ILOCA2
                  CALL QVSMA (IAPKEY, LREC2, I4096, IAPKE2, LREC2,
     *               IAPLOC, LREC2, NDO)
                  CALL QVNEG (IAPLOC, LREC2, IAPLOC, LREC2, NDO)
                  END IF
C                                       X (desc abs (U))
            ELSE IF (SORT(I:I).EQ.'X') THEN
               IAPKEY = APVIS + 2 + ILOCU
               CALL QVABS (IAPKEY, LREC2, IAPLOC, LREC2, NDO)
C                                       Y (desc abs (V))
            ELSE IF (SORT(I:I).EQ.'Y') THEN
               IAPKEY = APVIS + 2 + ILOCV
               CALL QVABS (IAPKEY, LREC2, IAPLOC, LREC2, NDO)
               END IF
C                                       Ready for second key
            IAPLOC = IAPLOC + 1
 200        CONTINUE
C                                       Sort
         CALL QSORT (APVIS, 1, 2, LREC2, NDO, APW1, APW2)
C                                       Copy back to disk
         IAPLOC = APVIS
C                                       Strip keys?
         IF (ONEPAS) IAPLOC = IAPLOC + 2
         DO 400 IVIS = 1,NDO
            CALL QGET (BUFF2(OBIND), IAPLOC, LRECO, 2)
            IAPLOC = IAPLOC + LREC2
            NIOUT = 1
            CALL UVDISK ('WRIT', LUN(2), FINDO, BUFF2, NIOUT, OBIND,
     *         IERR)
            IF (IERR.NE.0) GO TO 990
 400        CONTINUE
 500     CONTINUE
C                                       Release AP
      CALL QRLSE
C                                       Flush output buffer
      NIOUT = 0
      ERRTXT = 'SRTIN: FLUSHING VIS FILE'
      CALL UVDISK ('FLSH', LUN(2), FINDO, BUFF2, NIOUT, OBIND, IERR)
      IF (IERR.NE.0) GO TO 990
C                                       Close files
      CALL ZCLOSE (LUN(1), FINDI, IERR)
      IF (IERR.NE.0) GO TO 990
      CALL ZCLOSE (LUN(2), FINDO, IERR)
      IF (IERR.NE.0) GO TO 990
      GO TO 999
C                                       Error
 990  MSGTXT = ERRTXT
      CALL MSGWRT (7)
      MSGTXT = 'SRTIN: ERROR SORTING UV DATA'
 995  CALL MSGWRT (7)
C
 999  RETURN
      END
