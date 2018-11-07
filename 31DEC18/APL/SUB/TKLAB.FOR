      SUBROUTINE TKLAB (BLC, TRC, LABEL, YGAP, TEXT, NTEXT, CH, DOGRID,
     *   IERR)
C-----------------------------------------------------------------------
C! labels axes on plot directly to a TK graphics device, draw ticks
C# Graphics Plot-util
C-----------------------------------------------------------------------
C;  Copyright (C) 1995, 1998, 2014
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
C   TKLAB will write axis labeling to the TEK for a plots:
C   erase screen, draw borders, image name etc at top, text at bottom,
C   axis labels, and tick (via TKTICS)
C   Inputs:
C      BLC     R(2)     bottom left corner of the plot.
C      TRC     R(2)     top right corner of the plot.
C      LABEL   I        type of labeling: 1 none, 2 no ticks,
C                       3 Ra/Dec, 4 center-rel, 5 subim centered
C                       6 pixels, 7-10 as 3-6 no non-axis labels
C      YGAP    R        Number character lines below plot for TEXT
C      TEXT    C(2)*80  Text to put at bottom of plot
C      NTEXT   I        Number of lines of TEXT actually used
C      CH      R(4)     # chars to left,bot,right,top of plot
C      DOGRID  L        T => full coord. grid, else just ticks
C   Output:
C      IERR    I        error code, 0=ok, 1=problems.
C   Common:
C      /MAPHDR/  input  image header
C-----------------------------------------------------------------------
      INTEGER   LABEL, NTEXT, IERR
      REAL      BLC(2), TRC(2), YGAP, CH(4)
      CHARACTER TEXT(2)*80
      LOGICAL   DOGRID
C
      CHARACTER PLABEL(9)*16, SPTEXT*80, CHTMP*18, ELABEL(2)*8
      REAL      TKPAWS, XYRATO, DCX, DCY, X, Y
      INTEGER   IT, INCHAR, IANGL, I, ITRIM, IE, IEPO, IROUND, LTYPE
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DLOC.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DCAT.INC'
      DATA PLABEL /'Ecliptic Long.  ', 'Ecliptic Lat.   ',
     *   'Galactic Long.  ', 'Galactic Lat.   ', 'Right Ascension ',
     *   'Declination     ', 'Time (hours)    ', 'HA (hours)      ',
     *   'LST (hours)     '/
      DATA ELABEL /' (B1950)', ' (J2000)'/
      DATA TKPAWS /1.5/
C-----------------------------------------------------------------------
      CALL CHECKL ('TKLAB')
      LTYPE = MOD (ABS(LABEL), 100)
C                                       Clear screen.
      IT = 27
      CALL ZTKBUF (IT, 1, IERR)
      IF (IERR.NE.0) GO TO 999
      IT = 12
      CALL ZTKBUF (IT, 1, IERR)
      IF (IERR.NE.0) GO TO 999
      CALL TEKFLS (IERR)
      IF (IERR.NE.0) GO TO 999
      CALL ZDELAY (TKPAWS, IERR)
      XYRATO = (TRC(2) - BLC(2)) / (TRC(1) - BLC(1))
C                                       Draw border.
      CALL TEKVEC (BLC(1), BLC(2), 1, IERR)
      IF (IERR.NE.0) GO TO 999
      CALL TEKVEC (TRC(1), BLC(2), 2, IERR)
      IF (IERR.NE.0) GO TO 999
      CALL TEKVEC (TRC(1), TRC(2), 2, IERR)
      IF (IERR.NE.0) GO TO 999
      CALL TEKVEC (BLC(1), TRC(2), 2, IERR)
      IF (IERR.NE.0) GO TO 999
      CALL TEKVEC (BLC(1), BLC(2), 2, IERR)
      IF (IERR.NE.0) GO TO 999
      IF (LTYPE.EQ.1) GO TO 999
C                                       Source name, stokes, freq.
      IF (LTYPE.LT.7) THEN
         CALL TEKVEC (BLC(1), TRC(2), 1, IERR)
         IF (IERR.NE.0) GO TO 999
         DCX = 0.0
         DCY = 0.5
         IANGL = 0
         SPTEXT = ' '
         CALL H2CHR (8, 1, CATH(KHOBJ), SPTEXT)
         INCHAR = 12
         IF (NCHLAB(1,LOCNUM).GT.0) THEN
            SPTEXT(INCHAR:) = SAXLAB(1,LOCNUM)(:NCHLAB(1,LOCNUM))
            INCHAR = INCHAR + 3 + NCHLAB(1,LOCNUM)
            END IF
         IF (NCHLAB(2,LOCNUM).GT.0) THEN
            SPTEXT(INCHAR:) = SAXLAB(2,LOCNUM)(:NCHLAB(2,LOCNUM))
            INCHAR = INCHAR + 3 + NCHLAB(2,LOCNUM)
            END IF
C                                       Image name.
         CALL H2CHR (12, KHIMNO, CATH(KHIMN), CHTMP)
         CALL H2CHR (6, KHIMCO, CATH(KHIMC), CHTMP(13:18))
         CALL NAMEST (CHTMP, CATBLK(KIIMS), SPTEXT(INCHAR:), I)
         INCHAR = INCHAR + I - 1
         CALL TKCHAR (INCHAR, IANGL, DCX, DCY, SPTEXT, IERR)
         IF (IERR.NE.0) GO TO 999
C                                       Text at bottom
         IF (NTEXT.GT.0) THEN
            DCX = 0.
            DCY = -YGAP
            DO 20 I = 1,NTEXT
               CALL TEKVEC (BLC(1), BLC(2), 1, IERR)
               IF (IERR.NE.0) GO TO 999
               CALL CHTRIM (TEXT(I), 80, TEXT(I), INCHAR)
               CALL TKCHAR (INCHAR, 0, DCX, DCY, TEXT(I), IERR)
               IF (IERR.NE.0) GO TO 999
               DCY = DCY - 1.333
 20            CONTINUE
            END IF
         END IF
C                                       Axis labels
      IEPO = IROUND (REPOCH(LOCNUM))
      IF (IEPO.EQ.1950) THEN
         IE = 1
      ELSE IF (IEPO.EQ.2000) THEN
         IE = 2
      ELSE
         IE = 0
         END IF
C                                       vertical axes
      I = LABTYP(LOCNUM) / 10
      IF (I.GT.9) I = 0
      Y = (BLC(2) + TRC(2)) / 2.0
      DCX = -CH(1) + 0.5
      IANGL = 1
      CALL TEKVEC (BLC(1), Y, 1, IERR)
      IF (IERR.NE.0) GO TO 999
      IF (I.GT.0) THEN
         INCHAR = ITRIM(PLABEL(I))
         SPTEXT = PLABEL(I)
         IF ((IE.GT.0) .AND. ((I.EQ.5) .OR. (I.EQ.6))) THEN
            SPTEXT(INCHAR+1:) = ELABEL(IE)
            INCHAR = INCHAR + 8
            END IF
      ELSE
         SPTEXT = CPREF(2,LOCNUM) // CTYP(2,LOCNUM)
         CALL CHTRIM (SPTEXT, 25, SPTEXT, INCHAR)
         END IF
      DCY = INCHAR / 2.0 - 1.0
      CALL TKCHAR (INCHAR, IANGL, DCX, DCY, SPTEXT, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       horizontal axes
      I = MOD (LABTYP(LOCNUM), 10)
      X = (BLC(1) + TRC(1)) / 2.0
      CALL TEKVEC (X, BLC(2), 1, IERR)
      IF (IERR.NE.0) GO TO 999
      DCY = -2.833
      IF (LTYPE.EQ.2) DCY = -1.5
      IANGL = 0
      IF (I.GT.0) THEN
         INCHAR = ITRIM (PLABEL(I))
         SPTEXT = PLABEL(I)
         IF ((IE.GT.0) .AND. ((I.EQ.5) .OR. (I.EQ.6))) THEN
            SPTEXT(INCHAR+1:) = ELABEL(IE)
            INCHAR = INCHAR + 8
            END IF
      ELSE
         SPTEXT = CPREF(1,LOCNUM) // CTYP(1,LOCNUM)
         CALL CHTRIM (SPTEXT, 25, SPTEXT, INCHAR)
         END IF
      DCX = -INCHAR / 2.0
      CALL TKCHAR (INCHAR, IANGL, DCX, DCY, SPTEXT, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       do ticks
      IF (LTYPE.NE.2) THEN
         IF (AXINC(1,LOCNUM).NE.0) CALL TKTICS (1, BLC, TRC, XYRATO,
     *      DOGRID, IERR)
         IF (AXINC(2,LOCNUM).NE.0) CALL TKTICS (2, BLC, TRC, XYRATO,
     *      DOGRID, IERR)
         END IF
C
 999  RETURN
      END
