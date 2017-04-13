      SUBROUTINE YFIND (MAXPL, TYPE, IPL, UNIQUE, CATBLK, SCRTCH, IERR)
C-----------------------------------------------------------------------
C! determines the unique TV image of desired type, returns catalog block
C# Y0 TV-util
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
C   YFIND determines which of the visible TV images the user wishes
C   to select.  The TV must already be open.
C   Inputs:
C      MAXPL   I        Highest plane number allowed (i.e. do graphics
C                       count?)
C      TYPE    C*2      2-char image type to restrict search
C   Output:
C      IPL     I        Plane number found
C      UNIQUE  L        T => only one image visible now
C                           (all types except zeroed ones ('ZZ'))
C      CATBLK  I(256)   Image catalog block found
C      SCRTCH  I(256)   Scratch buffer
C      IERR    I        Error code: 0 => ok
C                          1 => no image
C                          2 => IO error in image catalog
C                          3 => TV error
C                         10 => > 1 image of requested type
C   Virtual TV version.
C-----------------------------------------------------------------------
      CHARACTER TYPE*2
      LOGICAL   UNIQUE
      INTEGER   SCRTCH(256), MAXPL, IPL, CATBLK(256), IERR
C
      INTEGER   ONPL, ZOR, IXT(2), CATI(256), I
      REAL      CATR(256), XT(2)
      HOLLERITH CATH(256)
      DOUBLE PRECISION CATD(128)
      INCLUDE 'INCS:DVTV.INC'
      INCLUDE 'INCS:DTVC.INC'
      EQUIVALENCE (IXT, XT)
      EQUIVALENCE (CATI, CATH, CATR, CATD, BUFFER(1025))
C-----------------------------------------------------------------------
C                                       Which planes are on ?
      IERR = 1
      UNIQUE = .TRUE.
      ONPL = 0
      DO 10 I = 1,4
         ONPL = ZOR (ONPL, TVLIMG(I))
 10      CONTINUE
      IF (ONPL.EQ.0) GO TO 999
C                                       set up write
      BUFFER(1) = MAXPL
      CALL ZCLC8 (2, TYPE, 1, XT)
      CALL ZI16IL (1, 1, IXT, BUFFER(2))
      BUFSW = 2
      BUFSR = 303
      CALL MKYBUF ('READ', 'YFIND', BUFSW, BUFSR, BUFFER, HEADER)
      CALL ZVTVXF (BUFSW, BUFSR, HEADER, IERR)
C                                       unpack answers
      IF (IERR.EQ.0) THEN
         CALL UNYBUF (BUFSR, BUFFER, HEADER)
         IPL = BUFFER(3)
         UNIQUE = BUFFER(4).GT.0
         CALL HDRBUF (2, CATI, CATH, CATR, CATD, BUFFER(5))
         CALL COPY (256, CATI, CATBLK)
         END IF
C
 999  RETURN
      END
