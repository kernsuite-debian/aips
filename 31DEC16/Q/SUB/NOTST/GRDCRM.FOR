      SUBROUTINE GRDCRM (IFIELD, DOSUM, APLO, APBUF, FIRST, NUMBER,
     *   NLOAD, GPARMS, JBUFSZ, BUFF1, BUFF2, BUFF3, IRET)
C-----------------------------------------------------------------------
C! Loads CLEAN components into AP for uv model computation.
C# AP-util Map UV EXT-appl Modeling
C-----------------------------------------------------------------------
C;  Copyright (C) 1995-1997, 1999-2002, 2005-2006, 2008, 2012-2013, 2015
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
C   GRDCRM loads CLEAN components for field IFIELD into the AP in
C   preparation for transformation to the data plane.  If GRDCRM starts
C   from the first component, the flux density of the CLEAN components
C   is summed.  The signs of the cell offsets are adjusted for flips
C   made to the map.
C   Input:
C      IFIELD   I      Current field number.
C      DOSUM    L      If true sum flux densities.
C      APLO     I      AP start location for the component array.
C      APBUF    I      AP start location for the buffer.
C      FIRST    I      First CLEAN component to be loaded.
C      NUMBER   I      Number of CLEAN components to be loaded.
C      NLOAD    I      AP buffer size.
C      JBUFSZ   I      Size in bytes of buffers. Dimension of BUFF1,2,3
C                      must be at least (2048 + 512) * 2
C   Output:
C      GPARMS   R(3)   BMAJ, BMIN, BPA of 1 size Gaussian in file
C      BUFF1    R(*)   Working buffer
C      BUFF2    R(*)   Working buffer
C      BUFF3    R(*)   Working buffer
C      NUMBER   I      Number of clean components loaded.
C      IRET     I      Return error code, 0=OK, 10=>no Comps found,
C                      otherwise fatal error.
C   Inputs from COMMON /MAPDES/:
C      CCDISK   I(*)     Disk numbers for CC files
C      CCCNO    I(*)     Catalog slot numbers for CC files.
C      CCVER    I(*)     CC file version number for each field.
C      NONEG    L        Stop reading comps. from a file past the first
C                        negative component.
C      LIMFLX   R        Stop reading comps < LIMFLX in abs value
C      NCLNG    I(*)     Changed if flux limit hit.
C   CLEAN components loaded into the AP in blocks of 5 words as:
C      0   = Y (integer form)
C      1   = FLUXG * cos(UX)
C      2   = FLUXG * sin(UX)
C      3,4 = cos, sin(X)
C-----------------------------------------------------------------------
      INTEGER   IFIELD, APLO, APBUF, FIRST, NUMBER, NLOAD, JBUFSZ, IRET
      LOGICAL   DOSUM
      REAL      GPARMS(3), BUFF1(*), BUFF2(*), BUFF3(*)
C
      CHARACTER KEYS(7)*8
      INTEGER   IERR, NREC, LIMIT, NCOUNT, I, LREC, LUNC, NX, NY, NKEY,
     *   KOLS(7), RAKOL, DECKOL, FLXKOL, TYPKOL, IPOINT, ICOUNT, JCOUNT,
     *   APLOC,  APLOC1, APLOC2, APLOC3, APLOC4, NNCNT, IDDX, IDDY,
     *   IROUND, BADCNT, BMJKOL, BMNKOL, BPAKOL, KLNBL2(256)
      REAL      FNX, FNY
      LOGICAL   F, DONE
      REAL      TEMP(6), TWOPIX, RECORD(7), DDX, DDY, CSX, CSY, XDDX,
     *   XDDY, CSXY
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DGDS.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DHDR.INC'
      EQUIVALENCE (KOLS(1), RAKOL),       (KOLS(2), DECKOL),
     *   (KOLS(3), FLXKOL),               (KOLS(4), TYPKOL),
     *   (KOLS(5), BMJKOL), (KOLS(6), BMNKOL), (KOLS(7), BPAKOL)
      DATA F /.FALSE./
      DATA KEYS /'DELTAX  ','DELTAY  ','FLUX    ','TYPE OBJ',
     *   'MAJOR AX',  'MINOR AX', 'POSANGLE'/
      DATA LUNC /28/
C-----------------------------------------------------------------------
C                                       Get image size
      NX = FLDSZ(1,IFIELD) * OSFX + 0.1
      NY = FLDSZ(2,IFIELD) * OSFY + 0.1
      FNX = NX
      FNY = NY
C                                       Open CLEAN component file.
      LREC = 0
      NREC = 1
      NKEY = 0
      JCOUNT = 0
C                                       Make sure this header applies
C                                       to this image
      CALL CATIO ('READ', CCDISK(IFIELD), CCCNO(IFIELD), KLNBL2, 'REST',
     *   BUFF1(2049), IRET)
      IF ((IRET.NE.0) .AND. (IRET.LT.5)) THEN
         WRITE (MSGTXT,1000) IRET, 'READ HEADER WITH CATIO'
         CALL MSGWRT(8)
         GO TO 999
         END IF
C                                       Tabini can update header on disk
      CALL TABINI ('READ', 'CC', CCDISK(IFIELD), CCCNO(IFIELD),
     *   ABS(CCVER(IFIELD)), KLNBL2, LUNC, NKEY, NREC, LREC,
     *   BUFF2(2049), BUFF1(2049), IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1000) IRET, 'OPEN TABLE FOR READ'
         GO TO 990
         END IF
      CALL RFILL (7, 0.0, RECORD)
C                                       Find columns (physical)
      NKEY = 3
      TYPKOL = 7
      IF (LREC.GT.3) NKEY = 7
      CALL FNDCOL (NKEY, KEYS, 8, F, BUFF1(2049), KOLS, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1010) IRET
         GO TO 990
         END IF
      APLOC = APLO
      ICOUNT = 0
      IPOINT = FIRST
      DDX = XPOFF(IFIELD)
      DDY = YPOFF(IFIELD)
      CSX = ABS (CELLSG(1)) / 3600.0
      CSY = ABS (CELLSG(2)) / 3600.0
      CSXY = SQRT (CSX*CSY)
      BADCNT = 0
C                                       Begin loading into computer buff
C                                       Make sure to load in small
C                                       enough pieces.
      LIMIT = JBUFSZ / 8
      LIMIT = MIN (LIMIT, NLOAD, 2048)
      IF (NUMBER.LT.LIMIT) LIMIT = NUMBER
      CALL RFILL (3, 0.0, GPARMS)
C                                       Jump to here if more passes
C                                       necessary.
 30   CONTINUE
         NCOUNT = 0
         DO 50 I = 1,LIMIT
            CALL TABIO ('READ', 0, IPOINT, RECORD, BUFF1(2049), IRET)
            IF (IRET.GT.0) THEN
               WRITE (MSGTXT,1030) IRET, IPOINT
               GO TO 990
            ELSE IF (IRET.LT.0) THEN
               IRET = 0
               GO TO 45
               END IF
C                                       Deal with component.
C                                       Make sure a point.
            DONE = (ABS(RECORD(FLXKOL)).LT.LIMFLX) .OR.
     *         ((NONEG) .AND. (RECORD(FLXKOL).LT.0.0))
            IF (DONE) THEN
               NCLNG(IFIELD) = IPOINT - 1
               GO TO 55
               END IF
            IF ((LREC.LE.3) .OR. (RECORD(TYPKOL).LE.1.0)) THEN
               NCOUNT = NCOUNT + 1
C                                       Gaussians
C                                       Check points for difmap also
               IF (LREC.GT.3) THEN
                  IF (NCOUNT.EQ.1) THEN
                     IF ((RECORD(TYPKOL).EQ.1.0) .OR.
     *                  (RECORD(TYPKOL).EQ.0.0)) THEN
                        GPARMS(1) = RECORD(BMJKOL)
                        GPARMS(2) = RECORD(BMNKOL)
                        GPARMS(3) = RECORD(BPAKOL)
                        END IF
                  ELSE
                     IF ((RECORD(TYPKOL).EQ.1.0) .OR.
     *                  (RECORD(TYPKOL).EQ.0.0)) THEN
                        IF ((ABS(GPARMS(1)-RECORD(BMJKOL))/CSXY.LT.0.05)
     *                     .AND.
     *                     (ABS(GPARMS(2)-RECORD(BMNKOL))/CSXY.LT.0.05)
     *                     .AND. (ABS(GPARMS(3)-RECORD(BPAKOL)).LT.0.2))
     *                     GO TO 40
                        END IF
                     MSGTXT = 'CC FILE NOT ALL ONE TYPE/SIZE'
                     CALL MSGWRT (8)
                     MSGTXT = 'TRY DFT MODEL SUBTRACTION/DIVISION'
                     IRET = 10
                     GO TO 990
                     END IF
                  END IF
 40            RECORD(RAKOL) = RECORD(RAKOL) + DDX
               RECORD(DECKOL) = RECORD(DECKOL) + DDY
C                                       Check for bad cell numbers
               XDDX = RECORD(RAKOL) / CSX
               XDDY = RECORD(DECKOL) / CSY
               IDDX = IROUND (XDDX)
               IDDY = IROUND (XDDY)
               IF ((ABS(XDDX-IDDX).GT.0.05) .OR.
     *            (ABS(XDDY-IDDY).GT.0.05)) THEN
                  BADCNT = BADCNT + 1
                  IF (BADCNT.LE.50) THEN
                     WRITE (MSGTXT,1040) I, XDDX, XDDY
                     CALL MSGWRT (8)
                     END IF
                  END IF
               BUFF1(NCOUNT) = RECORD(FLXKOL)
               BUFF2(NCOUNT) = RECORD(RAKOL)
               BUFF3(NCOUNT) = RECORD(DECKOL)
               IF (DOSUM) THEN
                  FLUXG(IFIELD) = FLUXG(IFIELD) + RECORD(FLXKOL)
                  TFLUXG = TFLUXG + RECORD(FLXKOL)
                  END IF
               END IF
 45         IPOINT = IPOINT + 1
 50         CONTINUE
 55      NSUBG(IFIELD) = IPOINT
         IF (NCOUNT.GT.0) THEN
            NNCNT = NCOUNT
C                                       Form AP indexes.
            APLOC1 = APLOC + 1
            APLOC2 = APLOC + 2
            APLOC3 = APLOC + 3
            APLOC4 = APLOC + 4
C                                       Load data into AP buffer APBUF.
C                                       Load flux (FLUXG*cos(ux))
            CALL QPUT (BUFF1, APBUF, NNCNT, 2)
            CALL QWD
            CALL QVMOV (APBUF, 1, APLOC1, 5, NNCNT)
C                                       Load and fix dec cell numbers.
            CALL QWR
            CALL QPUT (BUFF3, APBUF, NNCNT, 2)
            CALL QWD
C                                       Move values before fixing.
            CALL QVMOV (APBUF, 1, APLOC, 5, NNCNT)
            CALL QWR
C                                       Set constants in AP.
            TEMP(1) = (-FNY/2.) * CELLSG(2) / 3600.
            TEMP(2) = (FNY/2.0-1.0) * CELLSG(2) / 3600.
            TEMP(3) = FNY * CELLSG(2) / 3600.
            TEMP(4) = 2.0 / (CELLSG(2) / 3600.)
            TEMP(5) = 0.0
            TEMP(6) = -0.5 * CELLSG(2) / 3600.
            CALL QPUT (TEMP, 1, 6, 2)
            CALL QWD
C                                       Put declinations in the range
C                                       (0,FNY-1) and multiply by 2,fix.
C                                       Use APLOC2 for temporary use.
            CALL QVFILL (6, APLOC2, 5, NNCNT)
            CALL QVCLIP (APLOC, 5, 1, 2, APLOC, 5, NNCNT)
            CALL QLVGT (APLOC2, 5, APLOC, 5, APLOC2, 5, NNCNT)
            CALL QVSMUL (APLOC2, 5, 3, APLOC2, 5, NNCNT)
            CALL QVADD (APLOC2, 5, APLOC, 5, APLOC, 5, NNCNT)
            CALL QVSMAF (APLOC, 5, 4, 5, APLOC, 5, NNCNT)
C                                       Load and float RA cell numbers.
            CALL QWR
            CALL QPUT (BUFF2, APBUF, NNCNT, 2)
            CALL QWD
            CALL QVMOV (APBUF, 1, APLOC3, 5, NNCNT)
C                                       Store -2 PI/FNX/CELLSG(1)
            TWOPIX = ((-2.0 * 3.1415926) / FNX) / (CELLSG(1) / 3600.)
            CALL QWR
            CALL QPUT (TWOPIX, 1, 1, 2)
            CALL QWD
C                                       Scale RA by -2 PI/FNX/CELLSG(1)
            CALL QVSMUL (APLOC3, 5, 1, APLOC3, 5, NNCNT)
C                                       Clear APLOC+2 (FLUXG*sin(ux) ).
            CALL QVCLR (APLOC2, 5, NNCNT)
C                                       Take sine and cosine of
C                                       RA to 3 and 4.
            CALL QVSIN (APLOC3, 5, APLOC4, 5, NNCNT)
            CALL QVCOS (APLOC3, 5, APLOC3, 5, NNCNT)
            END IF
         ICOUNT = ICOUNT + LIMIT
         JCOUNT = JCOUNT + NCOUNT
C                                       If load complete close CC
C                                       file and return.
C                                       Update APLOC.
         APLOC = APLOC + (NNCNT * 5)
C                                       Return for another load.
         IF ((ICOUNT.LT.NUMBER) .AND. (.NOT.DONE)) THEN
            IF (NUMBER-ICOUNT.LT.LIMIT) LIMIT = NUMBER - ICOUNT
            GO TO 30
            END IF
C                                       Make sure CC loaded.
      IF (JCOUNT.LE.0) IRET = 10
      NUMBER = JCOUNT
C                                       Close CLNFIL.
      CALL TABIO ('CLOS', 0, IPOINT, RECORD, BUFF1(2049), IERR)
      IF (IERR.EQ.0) GO TO 999
         WRITE (MSGTXT,1060) IERR
         GO TO 990
C
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('GRDCRM: ERROR',I3,' ON ',A)
 1010 FORMAT ('GRDCRM: ERROR',I3,' FINDING REQUIRED CC COLUMNS')
 1030 FORMAT ('GRDCRM: READ ERROR',I3,' RECORD ',I5)
 1040 FORMAT ('GRDCRM: BAD CELL(',I7,') =', 2F10.3)
 1060 FORMAT ('GRDCRM: ERROR',I3,' CLOSING FILE ')
      END
