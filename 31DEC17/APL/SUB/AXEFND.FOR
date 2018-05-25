      SUBROUTINE AXEFND (NCHC, TYPE, NAXIS, AXES, IOFF, IERR)
C-----------------------------------------------------------------------
C! finds axis number for specified axis type
C# header
C-----------------------------------------------------------------------
C;  Copyright (C) 1995, 2003
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
C   AXEFND determines the order number of an axis whose name is in the
C   character string TYPE.  It will work for either regular or random
C   axes.
C   Inputs:
C      NCHC       I       Compare only first NCHC characters of type
C      TYPE       C*(*)   Unpacked char. axis type.
C      NAXIS      I       the number of axes to search,
C                          for uniform axes use: CATBLK(KIDIM) or KICTPN
C                          for random axes use:  CATBLK(KIPCN) OR KIPTPN
C      AXES       H(*)    Catalog axis name list,
C                          for uniform axes use: CATH(KHCTP)
C                          for random axes use : CATH(KHPTP)
C   Output:
C      IOFF       I       Axis offset ( zero relative axis number)
C      IERR       I       Return error code, 0=>OK, 1=>could not find.
C-----------------------------------------------------------------------
      CHARACTER TYPE*(*)
      HOLLERITH AXES(*)
      INTEGER   NCHC, NAXIS, IOFF, IERR
C
      INTEGER   I, INC, INDEX
      CHARACTER CHTH*8
C-----------------------------------------------------------------------
C                                       Initialize IERR
      IERR = 1
      IF ((NCHC.LT.1) .OR. (NCHC.GT.8)) GO TO 999
C                                       Initialize IOFF
      IOFF = 0
C                                       Search
      INC = 2
      DO 10 I = 1,NAXIS
         INDEX = 1 + (I-1) * INC
         CALL H2CHR (NCHC, 1, AXES(INDEX), CHTH)
C                                       Found match
         IF (TYPE(1:NCHC).EQ.CHTH(1:NCHC)) THEN
            IOFF = I - 1
            IERR = 0
            GO TO 999
            END IF
 10      CONTINUE
C
 999  RETURN
      END
