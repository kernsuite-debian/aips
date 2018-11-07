      SUBROUTINE STOPEN (SLUN, IDIV, IVOL, ISLOT, OUVER, ICTYPE, BUF,
     *   IERR)
C-----------------------------------------------------------------------
C! Creates and initializes a star table
C# EXT-util Map-util Utility
C-----------------------------------------------------------------------
C;  Copyright (C) 1995, 1996
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
C   STOPEN initializes a star table
C   Inputs:
C      SLUN     I        Logical unit for star table
C   Outputs:
C      IDIV     I        Correct Axis IDIV for cosine declination
C      ICTYPE   I(6)     Column type codes
C      BUF      I(512)   Scratch buffer
C      IERR     I        Error code 0=> Good
C-----------------------------------------------------------------------
      INTEGER   SLUN, IDIV, IVOL, ISLOT, OUVER, ICTYPE(*), BUF(*), IERR
C                                       Max Number Columns, Label Length
      INTEGER   MXSTCL, MXSTLB
      PARAMETER (MXSTCL=7, MXSTLB=24)
      CHARACTER TTITLE*28, ATYPE(18)*4, NTYPE(7)*8, CHTM12*12, TITLE*48,
     *   UNITS(MXSTCL)*8
      HOLLERITH HOLTMP(6)
      INTEGER   I, J, K, XBUF(256), NKEY, NREC, NCOL, ICOL, IRNO
      LOGICAL   EQUAL
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DCAT.INC'
      DATA TTITLE /'AIPS ST star positions table'/
      DATA ATYPE /'TIME','FREQ','LAMB','VELO','FELO','    ','DIST',
     *   'ANGL','ELON','ELAT', 'GLON','GLAT','RA  ','RA--','LL  ',
     *   'DEC ','DEC-','MM  '/
      DATA NTYPE /'SECONDS ', 'HERTZ   ', 'METERS  ',
     *   'METR/SEC', 'METR/SEC', 'PIXELS  ', 'DEGREES '/
C-----------------------------------------------------------------------
C                                       Create/open ST file
      NCOL = MXSTCL
      NKEY = 1
      NREC = 50
C                                       data types: 2 D, 4 R, Chars
      CALL FILL (256, 0, XBUF)
C                                       First two entries are Ra+Dec
      XBUF(129) = 11
      XBUF(130) = 11
C                                       Second two are Star size
      XBUF(131) = 12
      XBUF(132) = 12
C                                       Fifth is orientation
      XBUF(133) = 12
C                                       Sixth is type (cross, ellipse)
      XBUF(134) = 12
C                                       Seventh is Character Star label
      XBUF(135) = (MXSTLB*10) + 3
C                                       create/open
      CALL TABINI ('WRIT', 'ST', IVOL, ISLOT, OUVER, CATBLK, SLUN, NKEY,
     *    NREC, NCOL, XBUF, BUF, IERR)
      IF (IERR.GT.0) THEN
         WRITE (MSGTXT,1030) IERR, OUVER
         CALL MSGWRT (8)
         GO TO 999
         END IF
C                                       table title
      CALL CHR2H (28, TTITLE, 1, BUF(101))
C                                       write column titles
      TITLE = ' '
      DO 45 ICOL = 1,NCOL
         IRNO = ICOL
         J = MOD (ICOL-1, 2) * 2
C                                       star size
         IF (ICOL.EQ.3 .OR. ICOL.EQ.4) THEN
            TITLE(1:4) = 'DELT'
            CALL H2CHR (4, 1, CATH(KHCTP+J), TITLE(5:))
            END IF
C                                       Star coordinates
         IF (ICOL.EQ.1 .OR. ICOL .EQ.2) THEN
            CALL H2CHR (8, 1, CATH(KHCTP+J), TITLE)
            END IF
C                                       Star Position Angle
         IF (ICOL.EQ.5) TITLE = 'POSANG'
         IF (ICOL.EQ.6) TITLE = 'STARTYPE'
         IF (ICOL.EQ.7) TITLE = 'LABEL'
         CALL CHR2H (MXSTLB, TITLE, 1, HOLTMP)
         CALL TABIO ('WRIT', 3, IRNO, HOLTMP, BUF, IERR)
         IF (IERR.NE.0) GO TO 999
 45      CONTINUE
C                                       Figure out units
      DO 55 I = 1,2
         ICTYPE(I) = 1
         J = (I-1) * 2 + KHCTP
         UNITS(I) = ' '
         DO 50 K = 1,18
            CALL H2CHR (4, 1, CATH(J), CHTM12)
            EQUAL = CHTM12(1:4) .EQ. ATYPE(K)(1:4)
C                                       If the units are found
            IF (EQUAL) THEN
               IF (K.LT.7) UNITS(I) = NTYPE(K)
               IF (K.GE.7) UNITS(I) = NTYPE(7)
               IF (K.GT.12) ICTYPE(I) = 2
               IF (K.GT.15) ICTYPE(I) = 3
               J = I + 2
               UNITS(J) = UNITS(I)
C                                       Done with this axis, exit
               GO TO 55
               END IF
 50         CONTINUE
 55      CONTINUE
      ICTYPE(3) = -ICTYPE(1)
      ICTYPE(4) = -ICTYPE(2)
C                                       Position angle is degrees
      UNITS(5) = NTYPE(7)
      ICTYPE(5) = 10
C                                       Star type is an index
C                                       0=cross, 1=elipse, 2=none
      UNITS(6) = 'INDEX   '
      ICTYPE(6) = 10
C                                       Lable  is an arbitary string
      UNITS(7) = 'STRING  '
      ICTYPE(6) = 10
C                                       should column be scaled?
      IDIV = 0
      IF ((ICTYPE(1).EQ.2) .AND. (ICTYPE(2).EQ.3)) IDIV = 1
      IF ((ICTYPE(1).EQ.3) .AND. (ICTYPE(2).EQ.2)) IDIV = 2
C                                       write units
      DO 60 ICOL = 1,NCOL
         IRNO = ICOL
         CALL CHR2H (8, UNITS(ICOL), 1, HOLTMP)
         CALL TABIO ('WRIT', 4, IRNO, HOLTMP, BUF, IERR)
         IF (IERR.NE.0) GO TO 999
 60      CONTINUE
C
 999  RETURN
C-----------------------------------------------------------------------
 1020 FORMAT ('STOPEN: ERROR',I4,' OPENING TEXT FILE')
 1030 FORMAT ('STOPEN: ERROR',I4,' CREATING/OPENING ST FILE VERSION',I4)
      END


