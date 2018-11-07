      SUBROUTINE SRTOUT (DISKI, CNOSCI, DISKO, CNOSCO, LUN, JBUFSZ,
     *   BUFF1, BUFF2, IERR)
C-----------------------------------------------------------------------
C! Strips sort keys and writes output sorted data file.
C# Sort UV-util
C-----------------------------------------------------------------------
C;  Copyright (C) 1995
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
C-----------------------------------------------------------------------
C   Sort routine called by UVSORT.
C   Strips sort keys and writes output sorted data file.
C   Input:
C      DISKI    I        Input SC file disk number
C      CNOSCI   I        Input SC file catalog slot number.
C      DISKO    I        Output file disk number for cataloged files,
C                        .LE. 0 => /CFILES/ scratch file.
C      CNOSCO   I        Output file catalog slot number or /CFILES/
C                        scratch file number.
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
      INTEGER   DISKI, CNOSCI, DISKO, CNOSCO, LUN(2), JBUFSZ, IERR
      REAL      BUFF1(*), BUFF2(*)
C
      INTEGER   FINDI, FINDO, DISK, JERR, VO, BO, LENBU, IBIND,
     *   OBIND, NIOUT, LREC2, LOOP, INIO, I
      CHARACTER FILE*48, ERRTXT*80
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DFIL.INC'
C-----------------------------------------------------------------------
C                                       Open input
      ERRTXT = 'ERROR OPENING UV DATA FILE'
C                                       Scratch file
      CALL ZPHFIL ('SC', DISKI, CNOSCI, 1, FILE, JERR)
      CALL ZOPEN (LUN(1), FINDI, DISKI, FILE, .TRUE., .TRUE., .FALSE.,
     *   IERR)
      IF (IERR.NE.0) GO TO 990
C                                       Open Output
      IF (DISKO.LE.0) THEN
C                                       Scratch file
         DISK = SCRVOL(CNOSCO)
         CALL ZPHFIL ('SC', DISK, SCRCNO(CNOSCO), 1, FILE, JERR)
      ELSE
C                                       UV data file
         DISK = DISKO
         CALL ZPHFIL ('UV', DISK, CNOSCO, 1, FILE, JERR)
         END IF
      CALL ZOPEN (LUN(2), FINDO, DISK, FILE, .TRUE., .TRUE., .FALSE.,
     *   IERR)
      IF (IERR.NE.0) GO TO 990
C                                       Init I/O
      LENBU = 1
      VO = 0
      BO = 1
      LREC2 = LREC + 2
      ERRTXT = 'ERROR INITIALIZING I/O'
      CALL UVINIT ('READ', LUN(1), FINDI, NVIS, VO, LREC2, LENBU,
     *   JBUFSZ, BUFF1, BO, IBIND, IERR)
      IF (IERR.NE.0) GO TO 990
C                                       Output
      LENBU = 1
      CALL UVINIT ('WRIT', LUN(2), FINDO, NVIS, VO, LREC, LENBU,
     *   JBUFSZ, BUFF2, BO, OBIND, IERR)
      IF (IERR.NE.0) GO TO 990
C                                       Loop thru file by NSORT
      ERRTXT = 'ERROR DOING I/O'
      DO 500 LOOP = 1,NVIS
C                                       Read
         CALL UVDISK ('READ', LUN(1), FINDI, BUFF1, INIO, IBIND, IERR)
         IF (IERR.NE.0) GO TO 990
C                                       Copy stripping sort keys
         DO 200 I = 1,LREC
            BUFF2(OBIND+I-1) = BUFF1(IBIND+I+1)
 200        CONTINUE
         NIOUT = 1
         CALL UVDISK ('WRIT', LUN(2), FINDO, BUFF2, NIOUT, OBIND,
     *      IERR)
         IF (IERR.NE.0) GO TO 990
 500     CONTINUE
C                                       Flush output buffer
      NIOUT = 0
      ERRTXT = 'FLUSHING VIS FILE'
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
      MSGTXT = 'SRTOUT: ERROR SORTING UV DATA'
 995  CALL MSGWRT (7)
C
 999  RETURN
      END
