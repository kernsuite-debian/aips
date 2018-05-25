      SUBROUTINE FFTIM (MODEL, IFIELD, SCRGRD, SCRWRK, JBUFSZ, BUFF1,
     *   BUFF2, BUFF3, IRET)
C-----------------------------------------------------------------------
C! FFTs an image for uv interpolation.
C# AP-fft Map UV Modeling
C-----------------------------------------------------------------------
C;  Copyright (C) 1995, 1997, 2008-2009
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
C   FFTIM zero pads and ffts a cataloged image IFIELD in common
C   /MAPDES/ leaving it in the /CFILES/ common scratch file no. SCRGRD
C   in the proper form for interpolation, i.e. zero at center of the
C   row.
C   Also uses LUNs in LUNS in common /CFILES/ as set by GRDSET.
C   Inputs:
C      MODEL    I      2 regular image, 3 subimage
C      IFIELD   I      Field number in /MAPDES/ (INCLUDEs DGDS, CGDS)
C      SCRGRD   I      /CFILES/ scratch file number for output.
C      SCRWRK   I      /CFILES/ scratch file number for work, must be
C                      same size as SCRGRD.
C      JBUFSZ   I      Size of BUFF1,2,3 in bytes.
C      BUFF1    R(*)   Work buffers, must be big enough for complex
C                      version of largest image column.
C      BUFF2    R(*)   Work buffers, must be big enough for complex
C                      version of largest image column.
C      BUFF3    R(*)   Work buffers, must be big enough for complex
C                      version of largest image column.
C   Inputs from COMMON /MAPDES/:
C      CCDISK   I      Disk number for CC files
C      CCCNO    I(*)   Catalog slot numbers for CC files.
C      CCVER    I(*)   CC file version number for each field.
C   Output:
C      IRET     I      Return code, 0=>OK, otherwise failed.
C-----------------------------------------------------------------------
      INTEGER   MODEL, IFIELD, SCRGRD, SCRWRK, JBUFSZ, IRET
      REAL      BUFF1(*), BUFF2(*), BUFF3(*)
C
      CHARACTER FIL(3)*48
      INTEGER   CORN(7), MX, MY, VOL(3), BO(3), IDIR, XOFF, YOFF,
     *   JWIN(4)
      LOGICAL   FULL
      REAL      FMX, FMN, CATR(256)
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DGDS.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DFIL.INC'
      COMMON /MAPHDR/ CATR
      DATA CORN, JWIN /7*1, 4*0/
      DATA BO /1, 1, 1/
      DATA FULL /.FALSE./
C-----------------------------------------------------------------------
      IRET = 0
      MX = FLDSZ(1,IFIELD) * OSFX + 0.1
      MY = FLDSZ(2,IFIELD) * OSFY + 0.1
C                                       Save output CATBLK
      CALL RCOPY (256, CATR, BUFF3)
C                                       Fetch image with zero padding.
      IF (MODEL.NE.3) THEN
C                                       Set pixel offsets
         XOFF = -1
         YOFF = 0
         CALL PLNGET (CCDISK(IFIELD), CCCNO(IFIELD), CORN, JWIN, XOFF,
     *      YOFF, SCRGRD, MX, MY, BUFF1, BUFF2, JBUFSZ, JBUFSZ, LUNS(1),
     *      LUNS(2), IRET)
C                                       fetach with much 0 pad
      ELSE
         CALL SUBGET (CCDISK(IFIELD), CCCNO(IFIELD), CORN, SCRGRD, MX,
     *      MY, BUFF1, BUFF2, JBUFSZ, JBUFSZ, LUNS(1), LUNS(2), IRET)
         END IF
      IF (IRET.NE.0) GO TO 999
C                                       Restore output CATBLK
      CALL RCOPY (256, BUFF3, CATR)
C                                       FFT
C                                       Fill arrays for PASS1,2
      VOL(1) = SCRVOL(SCRGRD)
      VOL(2) = SCRVOL(SCRWRK)
      VOL(3) = SCRVOL(SCRGRD)
      CALL ZPHFIL ('SC', VOL(1), SCRCNO(SCRGRD), 1, FIL(1), IRET)
      CALL ZPHFIL ('SC', VOL(2), SCRCNO(SCRWRK), 1, FIL(2), IRET)
      CALL ZPHFIL ('SC', VOL(3), SCRCNO(SCRGRD), 1, FIL(3), IRET)
      IDIR = 4
      CALL PASS1 (IDIR, FULL, LUNS, VOL, FIL, BO, BUFF1, JBUFSZ,
     *   BUFF2, JBUFSZ, MX, MY, IRET)
      IF (IRET.NE.0) GO TO 999
      CALL PASS2 (IDIR, FULL, LUNS, VOL, FIL, BO, BUFF1, JBUFSZ,
     *   BUFF2, JBUFSZ, MX, MY, FMX, FMN, IRET)
      IF (IRET.NE.0) GO TO 999
C
 999  RETURN
      END
