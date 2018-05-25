      SUBROUTINE CLAB1 (BLC, TRC, CH, ILTYPE, XYR, DOGRID, IBUFF, IERR)
C-----------------------------------------------------------------------
C! puts axis labels in plot file and calls CTICS to draw and label ticks
C# Plot-util
C-----------------------------------------------------------------------
C;  Copyright (C) 1995, 1998, 2002-2003, 2012, 2014
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
C   CLAB1 controls some axis drawing and labeling functions: labels each
C   axis with RA/DEC or the 8-char type, calls CTICS to draw tics & tick
C   labels
C   Inputs:
C      BLC      R(2)    X, Y pixels to form bottom left hand corner
C      TRC      R(2)    X, Y pixels to form the top right hand corner
C      CH       R(4)    left, bot, right, top : total character offsets
C      ILTYPE   I       label type: 1 none, 2 no ticks, 3 RA/DEC
C                          4 center relative
C      XYR      R       The ratio of the distance between X axis pixels
C                       and the distance between Y axis pixels on plot
C      DOGRID   L       T => full coord grid, else ticks
C   In/out:
C      IBUFF    I(256)  the updated graphics output buffer.
C      IERR     I       error indicator: 0 = No error.
C   If AXTYPE(i,LOCNUM)='NO TICKS' then CTICS is not called.
C-----------------------------------------------------------------------
      REAL      BLC(2), TRC(2), CH(4), XYR
      INTEGER   ILTYPE, IBUFF(256), IERR
      LOGICAL   DOGRID
C
      REAL      X, X0, X1, Y, Y0, Y1, DCX, DCY
      INTEGER   I, IANGL, IERR2, INCHAR, LTYPE, NLABEL(9), IROUND, IE,
     *   IEPO
      DOUBLE PRECISION  ZERO
      CHARACTER SPRTXT*30, PLABEL(9)*16, CPT(2)*5, CTY(2)*20, SUBR*6,
     *   ELABEL(2)*8, NOTICK*8
      INCLUDE 'INCS:DLOC.INC'
      INCLUDE 'INCS:DMSG.INC'
      DATA ZERO /0.0D0/
      DATA NLABEL /14, 13, 14, 13, 15, 11, 12, 10, 11/
      DATA PLABEL /'Ecliptic Long.  ', 'Ecliptic Lat.   ',
     *   'Galactic Long.  ', 'Galactic Lat.   ', 'Right Ascension ',
     *   'Declination     ', 'Time (hours)    ', 'HA (hours)     ',
     *   'LST (hours)     '/
      DATA ELABEL /' (B1950)', ' (J2000)'/
      DATA NOTICK /'NO TICKS'/
C-----------------------------------------------------------------------
      CALL CHECKL ('CLAB1')
      LTYPE = MOD (ABS (ILTYPE), 100)
      IF (LTYPE.EQ.1) GO TO 999
      IEPO = IROUND (REPOCH(LOCNUM))
      IF (IEPO.EQ.1950) THEN
         IE = 1
      ELSEIF (IEPO.EQ.2000) THEN
         IE = 2
      ELSE
         IE = 0
         END IF
C                                       Initial values.
      X0 = BLC(1)
      X1 = TRC(1)
      Y0 = BLC(2)
      Y1 = TRC(2)
C                                       vertical axes
      IF (((CPREF(2,LOCNUM).NE.' ') .OR. (CTYP(2,LOCNUM).NE.' ')) .AND.
     *   (CPREF(2,LOCNUM).NE.'-1') .AND. (CTYP(2,LOCNUM).NE.NOTICK))
     *   THEN
         I = LABTYP(LOCNUM) / 10
         IF (I.GT.9) I = 0
         Y = (Y1-Y0)/2.0 + Y0
         CALL GPOS (X0, Y, IBUFF, IERR)
         SUBR = 'GPOS'
         IF (IERR.NE.0) GO TO 980
         IF (I.GT.0) THEN
            SPRTXT = PLABEL(I)
            INCHAR = NLABEL(I)
            IF ((IE.GT.0) .AND. ((I.EQ.5) .OR. (I.EQ.6))) THEN
               SPRTXT(INCHAR+1:) = ELABEL(IE)
               INCHAR = INCHAR + 8
               END IF
         ELSE
            SPRTXT = CPREF(2,LOCNUM) // CTYP(2,LOCNUM)
            CALL CHTRIM (SPRTXT, 25, SPRTXT, INCHAR)
            END IF
         IANGL = 1
         DCX = -CH(1) + 1.0
         DCY = INCHAR / 2.0 - 1.0
         CALL GCHAR (INCHAR, IANGL, DCX, DCY, SPRTXT, IBUFF, IERR)
         SUBR = 'GCHAR'
         IF (IERR.NE.0) GO TO 980
         END IF
C                                       horizontal axes
      IF (((CPREF(1,LOCNUM).NE.' ') .OR. (CTYP(1,LOCNUM).NE.' ')) .AND.
     *   (CPREF(1,LOCNUM).NE.'-1') .AND. (CTYP(1,LOCNUM).NE.NOTICK))
     *   THEN
         I = MOD (LABTYP(LOCNUM), 10)
         IF (I.GT.9) I = 0
         X = (X1-X0)/2.0 + X0
         CALL GPOS (X, Y0, IBUFF, IERR)
         SUBR = 'GPOS'
         IF (IERR.NE.0) GO TO 980
         IF (I.GT.0) THEN
            SPRTXT = PLABEL(I)
            INCHAR = NLABEL(I)
            IF ((IE.GT.0) .AND. ((I.EQ.5) .OR. (I.EQ.6))) THEN
               SPRTXT(INCHAR+1:) = ELABEL(IE)
               INCHAR = INCHAR + 8
               END IF
         ELSE
            SPRTXT = CPREF(1,LOCNUM) // CTYP(1,LOCNUM)
            CALL CHTRIM (SPRTXT, 25, SPRTXT, INCHAR)
            END IF
         IANGL = 0
         DCX = -INCHAR / 2.0
         DCY = -2.83
         IF (LTYPE.EQ.2) DCY = -1.5
         CALL GCHAR (INCHAR, IANGL, DCX, DCY, SPRTXT, IBUFF, IERR)
         SUBR = 'GCHAR'
         IF (IERR.NE.0) GO TO 980
         END IF
C                                       do ticks
      CPT(1) = CPREF(1,LOCNUM)
      CPT(2) = CPREF(2,LOCNUM)
      CTY(1) = CTYP(1,LOCNUM)
      CTY(2) = CTYP(2,LOCNUM)
      IF (LTYPE.EQ.2) THEN
         CPREF(1,LOCNUM) = ' '
         CPREF(2,LOCNUM) = ' '
         CTYP(1,LOCNUM) = ' '
         CTYP(2,LOCNUM) = ' '
         END IF
      IF (CTY(1).NE.NOTICK) THEN
         CALL CTICS (1, BLC, TRC, XYR, ZERO, DOGRID, IBUFF, IERR2)
         IF (IERR2.EQ.2) THEN
            IERR = 2
            GO TO 999
            END IF
         END IF
      IF (CTY(2).NE.NOTICK) THEN
         CALL CTICS (2, BLC, TRC, XYR, ZERO, DOGRID, IBUFF, IERR2)
         IF (IERR2.EQ.2) THEN
            IERR = 2
            GO TO 999
            END IF
         END IF
      IF (LTYPE.EQ.2) THEN
         CPREF(1,LOCNUM) = CPT(1)
         CPREF(2,LOCNUM) = CPT(2)
         CTYP(1,LOCNUM) = CTY(1)
         CTYP(2,LOCNUM) = CTY(2)
         END IF
      GO TO 999
C                                       Graph drawing error.
 980  WRITE (MSGTXT,1980) IERR, SUBR
      CALL MSGWRT (7)
      GO TO 999
C
 999  RETURN
C-----------------------------------------------------------------------
 1980 FORMAT ('CLAB1: GRAPH LABEL WRITING ERROR',I5,' FROM ',A)
      END
