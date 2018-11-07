      SUBROUTINE FWRITE (FDVEC, TBIND, TAPBUF, FITBLK, IERR)
C-----------------------------------------------------------------------
C! converts FITS header to AIPS header and displays it with MSGWRT
C# Tape FITS
C-----------------------------------------------------------------------
C;  Copyright (C) 1995, 1999, 2003, 2007, 2012
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
C   FWRITE converts a FITS header record (2880 8-bit characters) to
C   the local character form, parses it, and prints a summary using
C   MSGWRT.
C   Inputs:
C      FDVEC   I(50)       TAPIO control parms (1st record read as FITS
C                          with blocking up to 10 and infinite reads
C                          allowed)
C   In/out:
C      TBIND   I           Pointer to data in TAPBUF
C      TAPBUF  I(*)        TAPIO I/O buffer (1st tape record already
C                          read)
C   Output:
C      FITBLK  C*2880      Work buffer: 1 header record
C      IERR    I           Error codes from IO system
C-----------------------------------------------------------------------
      INTEGER   FDVEC(50), TBIND, TAPBUF(*), IERR
      CHARACTER FITBLK*2880
C
      CHARACTER PTYPES(20)*8, ORIGIN*8, TAPEDA*8, CTEST*8
      DOUBLE PRECISION PSCAL(20), POFF(20), BSCALE(2), BZERO(2)
      INTEGER   GROUP, TABLES, I, NC, JJ, MSGLEV, BITPIX, NAXIS, NPV(4),
     *   USED(300)
      LOGICAL   LEND, UVTABL
      REAL      PCMATX(7,7), CDMATX(7,7), PVMATX(7,7)
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DCAT.INC'
      DATA MSGLEV /3/
      DATA ORIGIN, TAPEDA /'ORIGIN ', 'DATE '/
C-----------------------------------------------------------------------
C                                       Initialize header, et al.
      TABLES = 0
      DO 10 I = 1,20
         PSCAL(I) = 0.0D0
         POFF(I) = 0.0D0
         PTYPES(I) = ' '
 10      CONTINUE
      CALL RFILL (49, 0.0, CDMATX)
      CALL RFILL (49, 0.0, PCMATX)
      CALL RFILL (49, 0.0, PVMATX)
      CALL FILL (4, 0, NPV)
      CALL FILL (300, 0, USED)
      NAXIS = 0
      BSCALE(1) = 1.0D0
      BZERO(1) = 0.0D0
      BSCALE(2) = 1.0D0
      BZERO(2) = 0.0D0
      GROUP = 0
      CALL FILL (256, 0, CATBLK)
      I = (6 + KIPTPN + KICTPN) * 2
      CALL RFILL (I, HBLANK, CATH(1))
      CALL RFILL (5, HBLANK, CATH(KHIMN))
C                                       Read loop
      DO 30 JJ = 1,30000
         IF (JJ.GT.1) THEN
            CALL TAPIO ('READ', FDVEC, TAPBUF, TBIND, IERR)
            IF (IERR.NE.0) GO TO 999
            END IF
         CALL ZC8CL (2880, 1, TAPBUF(TBIND), FITBLK)
C                                       Parse cards individually
         DO 20 NC = 1,36
            I = (NC-1) * 80 + 1
            CTEST = FITBLK(I:I+7)
            IF ((CTEST.EQ.TAPEDA) .OR. (CTEST.EQ.ORIGIN)) THEN
               MSGTXT = FITBLK(I:I+63)
               CALL MSGWRT (MSGLEV)
               END IF
            CALL FPARSE (NC, FITBLK, PSCAL, POFF, PTYPES, TABLES, LEND,
     *         GROUP, BITPIX, BSCALE, BZERO, NAXIS, NPV, UVTABL, USED,
     *         IERR)
            IF (LEND) GO TO 50
            IF ((IERR.EQ.1) .AND. ((CTEST(:2).EQ.'CD') .OR.
     *         (CTEST(:2).EQ.'PC') .OR. (CTEST(:2).EQ.'PV')))
     *         CALL PCCARD (GROUP, FITBLK(I:I+79), PCMATX, CDMATX,
     *         PVMATX)
 20         CONTINUE
 30      CONTINUE
C                                       test for uv tables
 50   IF (UVTABL) THEN
         MSGTXT = 'Reading uv table data'
         CALL MSGWRT (MSGLEV)
         IF ((NPV(1).GT.0) .OR. (NPV(2).GT.0)) THEN
            WRITE (MSGTXT,1050) NPV(1), NPV(2)
            CALL MSGWRT (MSGLEV)
            WRITE (MSGTXT,1055) NPV(3), NPV(4)
            CALL MSGWRT (MSGLEV)
            END IF
         IF ((CATBLK(KINAX)/100.EQ.7777777) .AND.
     *      (CATBLK(KINAX+1).EQ.0)) THEN
            MSGTXT = 'Header information located later in file'
            CALL MSGWRT (MSGLEV)
            MSGTXT = 'use PRTTP rather than TPHEAD to read this UV info'
            CALL MSGWRT (MSGLEV)
            GO TO 999
            END IF
         END IF
C                                       PC -> CROTA
C                                       CD -> CDELT, CROTA
      CALL PCHDR (PCMATX, CDMATX, PVMATX)
C                                       Output the stuff
      CALL CHR2H (2, 'MA', KHPTYO, CATH(KHPTY))
      IF (CATBLK(KIPCN).GT.0) CALL CHR2H (2, 'UV', KHPTYO, CATH(KHPTY))
      CALL MSGHDR (MSGLEV, PSCAL, POFF, PTYPES, TABLES, BITPIX, BSCALE,
     *   BZERO)
C
 999  RETURN
C-----------------------------------------------------------------------
 1050 FORMAT ('Piece',I3,' of',I3,' in the full data set')
 1055 FORMAT ('Visibilities',I9,' through',I9,' from original')
      END
