      SUBROUTINE MFINI (LUN, VOL, CNO, DEPTH, PLANE, VER, CATBLK,
     *   BUFFER, IERR)
C-----------------------------------------------------------------------
C! Creates/opens Model Fit table files for SAD and others
C# Map Modeling
C-----------------------------------------------------------------------
C;  Copyright (C) 1995-1998, 2006, 2010, 2015
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
C   MFINI creates and/or opens for writing (and reading) a specified
C   MF or Model-fit file.
C   FORMAT REVISION 4
C   Inputs:
C      LUN     I        Logical unit number to use
C      VOL     I        Disk number
C      CNO     I        Catalog number
C      DEPTH   I(5)     Depth into cube
C      PLANE   I        Which plane is this?
C   In/out:
C      VER     I        Input: desired version number 0 -> new
C                       Output: that used
C      CATBLK  I(256)   Image header
C   Output:
C      BUFFER     I(512)   Required for later calls to TABIO
C      IERR    I        Error codes from TABINI or TABIO
C   Common: DLOC.INC must be initialized
C-----------------------------------------------------------------------
      INTEGER   LUN, VOL, CNO, DEPTH(5), PLANE, VER, CATBLK(256),
     *   BUFFER(512), IERR
C
      INCLUDE 'INCS:PMFC.INC'
      INTEGER   IRNO, NKEY, NREC, ITITLE(8), NCOL, I, LDEPTH(5), LPLANE,
     *   DATP(128,2), ITRIM, NC, MFKOLS(NUMCOL), REVISN
      HOLLERITH HTITLE(8)
      REAL      RTITLE(8)
      CHARACTER TTITLE*32, CTITLE(NUMCOL)*8, UNITS(NUMCOL)*8, TITLE*24,
     *   TUNIT*8, TTYPE*8, NUMCH*8
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DLOC.INC'
      INCLUDE 'INCS:DMSG.INC'
      EQUIVALENCE (ITITLE, HTITLE, RTITLE)
      DATA TTITLE /'AIPS MODEL FIT COMPONENTS TABLE'/
      DATA CTITLE /  'PLANE',    'PEAK INT', 'I FLUX  ', 'DELTAX  ',
     *   'DELTAY  ', 'MAJOR AX', 'MINOR AX', 'POSANGLE', 'Q FLUX  ',
     *   'U FLUX  ', 'V FLUX  ', 'ERR PEAK', 'ERR FLUX', 'ERR DLTX',
     *   'ERR DLTY', 'ERR MAJA', 'ERR MINA', 'ERR PA  ', 'ERR QFLX',
     *   'ERR UFLX', 'ERR VFLX', 'TYPE MOD', 'D0 MAJOR', 'D0 MINOR',
     *   'D0 POSAN', 'D- MAJOR', 'D- MINOR', 'D- POSAN', 'D+ MAJOR',
     *   'D+ MINOR', 'D+ POSAN', 'RES RMS ', 'RES PEAK', 'RES FLUX',
     *   'CENTER X', 'CENTER Y', 'MAJ AXIS', 'MIN AXIS', 'PIXEL PA',
     *   'PB FACT ', 'DLY FACT'/
      DATA NUMCH /'12345678'/
      DATA REVISN /4/
C-----------------------------------------------------------------------
C                                       Init parameters
      NCOL = NUMCOL
      NKEY = 7
      NREC = 500
C                                       Data = 7 R's   x 1
      CALL FILL (256, 0, DATP)
      CALL FILL (NCOL, 12, DATP(1,2))
C                                       create/open
      CALL TABINI ('WRIT', 'MF', VOL, CNO, VER, CATBLK, LUN, NKEY, NREC,
     *   NCOL, DATP, BUFFER, IERR)
      IF (IERR.GT.0) THEN
         CALL TABERR ('WRIT', 'TABINI', 'MFINI', IERR)
         GO TO 990
         END IF
C                                       New file created
      IF (IERR.LT.0) THEN
C                                       write column titles
         DO 20 IRNO = 1,NCOL
            TITLE = CTITLE(IRNO)
            CALL CHR2H (24, TITLE, 1, HTITLE)
            CALL TABIO ('WRIT', 3, IRNO, HTITLE, BUFFER, IERR)
            IF (IERR.GT.0) THEN
               CALL TABERR ('WRIT', 'TABIO', 'MFINI', IERR)
               GO TO 990
               END IF
 20         CONTINUE
C                                       figure out the units
         DO 25 I = 1,NUMCOL
            UNITS(I) = 'DEGREES'
 25         CONTINUE
         UNITS(COLPLN) = ' '
         UNITS(COPCEX) = 'PIXELS'
         UNITS(COPCEY) = 'PIXELS'
         UNITS(COPMAJ) = 'PIXELS'
         UNITS(COPMIN) = 'PIXELS'
         CALL H2CHR (8, 1, CATBLK(KHBUN), TUNIT)
         UNITS(COLPEK) = TUNIT
         UNITS(COLFLX) = TUNIT
         CALL CHLTOU (8, TUNIT)
         IF (TUNIT(:3).EQ.'JY/') UNITS(COLFLX) = 'JY'
         IF (TUNIT(:4).EQ.'MAG/') UNITS(COLFLX) = 'MAG'
         UNITS(COEPEK) = UNITS(COLPEK)
         UNITS(COEFLX) = UNITS(COLFLX)
         UNITS(COLQFL) = UNITS(COLFLX)
         UNITS(COLUFL) = UNITS(COLFLX)
         UNITS(COLVFL) = UNITS(COLFLX)
         UNITS(COEQFL) = UNITS(COLFLX)
         UNITS(COEUFL) = UNITS(COLFLX)
         UNITS(COEVFL) = UNITS(COLFLX)
         UNITS(COLTYP) = ' '
         UNITS(CORRMS) = UNITS(COLPEK)
         UNITS(CORPEK) = UNITS(COLPEK)
         UNITS(CORFLX) = UNITS(COLFLX)
         UNITS(COBMFA) = ' '
         UNITS(CODLFA) = ' '
C                                       X axis
         DO 30 I = 1,2
            IF ((AXTYP(LOCNUM).NE.1) .AND. (AXTYP(LOCNUM).NE.1+I)) THEN
               CALL H2CHR (8, 1, CATBLK(KHCTP+2*(I-1)), TTYPE)
               IF ((TTYPE(:4).EQ.'VELO') .OR. (TTYPE(:4).EQ.'FELO'))
     *            THEN
                  TUNIT = 'M/SEC'
               ELSE IF (TTYPE(:4).EQ.'TIME') THEN
                  TUNIT = 'SECONDS'
               ELSE IF (TTYPE(:4).EQ.'FREQ') THEN
                  TUNIT = 'HERTZ'
               ELSE IF ((TTYPE(:6).EQ.'LAMBDA') .OR.
     *            (TTYPE.EQ.'DISTANCE')) THEN
                  TUNIT = 'METERS'
               ELSE IF ((TTYPE.EQ.'PIXEL') .OR. (TTYPE.EQ.' ')) THEN
                  TUNIT = 'UNITLESS'
               ELSE
                  TUNIT = '????????'
                  END IF
               UNITS(COLDLX+I-1) = TUNIT
               UNITS(COLMJX+I-1) = TUNIT
               UNITS(COEDLX+I-1) = TUNIT
               UNITS(COEMJX+I-1) = TUNIT
               UNITS(COLD0J+I-1) = TUNIT
               UNITS(COLDMJ+I-1) = TUNIT
               UNITS(COLDPJ+I-1) = TUNIT
               END IF
 30         CONTINUE
C                                       write units
         DO 35 IRNO = 1,NCOL
            TITLE = UNITS(IRNO)
            CALL CHR2H (24, TITLE, 1, HTITLE)
            CALL TABIO ('WRIT', 4, IRNO, HTITLE, BUFFER, IERR)
            IF (IERR.GT.0) THEN
               CALL TABERR ('WRIT', 'TABIO', 'MFINI', IERR)
               GO TO 990
               END IF
 35         CONTINUE
C                                       write keywords
C                                       loop 100 must match
         TUNIT = 'REVISION'
         CALL CHR2H (8, TUNIT, 1, HTITLE)
         ITITLE(3) = REVISN
         ITITLE(4) = 0
         ITITLE(5) = 4
         CALL TABIO ('WRIT', 5, 1, HTITLE, BUFFER, IERR)
         IF (IERR.GT.0) THEN
            CALL TABERR ('WRIT', 'TABIO', 'MFINI', IERR)
            GO TO 990
            END IF
         DO 40 I = 1,5
            TUNIT = 'DEPTH' // NUMCH(I:I)
            CALL CHR2H (8, TUNIT, 1, HTITLE)
            ITITLE(3) = DEPTH(I)
            CALL TABIO ('WRIT', 5, I+1, HTITLE, BUFFER, IERR)
            IF (IERR.GT.0) THEN
               CALL TABERR ('WRIT', 'TABIO', 'MFINI', IERR)
               GO TO 990
               END IF
 40         CONTINUE
         TUNIT = 'REALRMS'
         CALL CHR2H (8, TUNIT, 1, HTITLE)
         RTITLE(3) = 0.0
         ITITLE(4) = 0
         ITITLE(5) = 2
         CALL TABIO ('WRIT', 5, 7, HTITLE, BUFFER, IERR)
         IF (IERR.GT.0) THEN
            CALL TABERR ('WRIT', 'TABIO', 'MFINI', IERR)
            GO TO 990
            END IF
C                                       table title
         CALL CHR2H (32, TTITLE, 1, HTITLE)
         CALL COPY (8, ITITLE, BUFFER(101))
C                                       check depth!
      ELSE
         DO 100 I = 2,6
            CALL TABIO ('READ', 5, I, HTITLE, BUFFER, IERR)
            IF (IERR.GT.0) THEN
               CALL TABERR ('READ', 'TABIO', 'MFINI', IERR)
               GO TO 990
               END IF
            LDEPTH(I-1) = ITITLE(3)
 100        CONTINUE
C                                       get plane number
         LPLANE = 1
         IF (CATBLK(KIDIM).GE.7) LPLANE = MAX (LDEPTH(5), 1)
         IF (CATBLK(KIDIM).GE.6) LPLANE = MAX (LDEPTH(4), 1) +
     *      (LPLANE-1) * MAX (CATBLK(KINAX+5), 1)
         IF (CATBLK(KIDIM).GE.5) LPLANE = MAX (LDEPTH(3), 1) +
     *      (LPLANE-1) * MAX (CATBLK(KINAX+4), 1)
         IF (CATBLK(KIDIM).GE.4) LPLANE = MAX (LDEPTH(2), 1) +
     *      (LPLANE-1) * MAX (CATBLK(KINAX+3), 1)
         IF (CATBLK(KIDIM).GE.3) LPLANE = MAX (LDEPTH(1), 1) +
     *      (LPLANE-1) * MAX (CATBLK(KINAX+2), 1)
         IF (PLANE.NE.LPLANE) THEN
            WRITE (MSGTXT,1100) PLANE, LPLANE
            CALL MSGWRT (6)
            END IF
C                                       check that columns are there
         CALL FNDCOL (NUMCOL, CTITLE, 8, .TRUE., BUFFER, MFKOLS, I)
         DO 110 I = 1,NUMCOL
            NC = ITRIM (CTITLE(I))
            IF (MFKOLS(I).LE.0) THEN
               WRITE (MSGTXT,1110) CTITLE(I)(:NC)
               CALL MSGWRT (6)
               IERR = 11
            ELSE IF (MFKOLS(I).NE.I) THEN
               WRITE (MSGTXT,1111) CTITLE(I)(:NC)
               CALL MSGWRT (6)
               IERR = 12
            ELSE IF (DATP(I,2).NE.12) THEN
               WRITE (MSGTXT,1112) CTITLE(I)(:NC)
               CALL MSGWRT (6)
               IERR = 12
               END IF
 110        CONTINUE
         IF (IERR.NE.0) GO TO 990
         END IF
      GO TO 999
C
 990  WRITE (MSGTXT,1990)
      CALL MSGWRT (7)
C
 999  RETURN
C-----------------------------------------------------------------------
 1100 FORMAT ('Warning: current plane =',I4,' not = initial plane',I4)
 1110 FORMAT ('MFINI: ''',A,''' COLUMN NOT FOUND')
 1111 FORMAT ('MFINI: ''',A,''' COLUMN OUT OF ORDER')
 1112 FORMAT ('MFINI: ''',A,''' COLUMN NOT CORRECT TYPE OR NUMBER')
 1990 FORMAT ('MFINI: ERROR INITIALIZING MODEL FIT TABLE FOR WRIT')
      END
