      SUBROUTINE TVLAB (BLC, TRC, LABEL, YGAP, TEXT, NTEXT, CH, DOGRID,
     *   IERR)
C-----------------------------------------------------------------------
C! labels axes on plot directly to a TK graphics device, draw ticks
C# Graphics Plot-util
C-----------------------------------------------------------------------
C;  Copyright (C) 2000, 2005, 2014, 2016
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
C   TVLAB will write axis labeling to the TV for a plots:
C      draw borders, image name etc at top, text at bottom, axis labels,
C      and tick (via TKTICS)
C   Inputs:
C      BLC      R(2)     bottom left corner of the plot.
C      TRC      R(2)     top right corner of the plot.
C      LABEL    I        type of labeling: 1 none, 2 no ticks,
C                        3 Ra/Dec, 4 center-rel, 5 subim centered
C                        6 pixels, 7-10 as 3-6 no non-axis labels
C      YGAP     R        Number character lines below plot for TEXT
C      TEXT     C(2)*80  Text to put at bottom of plot
C      NTEXT    I        Number of lines of TEXT actually used
C                        If NTEXT > 0, TEXT is plotted on all LABEL vals
C      CH       R(4)     # chars to left,bot,right,top of plot
C      DOGRID   L        T => full coord. grid, else just ticks
C   Output:
C      IERR     I        error code, 0=ok, 1=problems.
C   Common:
C      /MAPHDR/  input  image header
C-----------------------------------------------------------------------
      INTEGER   LABEL, NTEXT, IERR
      REAL      BLC(2), TRC(2), YGAP, CH(4)
      CHARACTER TEXT(2)*80
      LOGICAL   DOGRID
C
      CHARACTER PLABEL(9)*16, SPTEXT*80, CHTMP*18, ELABEL(2)*8
      REAL      XYRATO, X, Y, RXT, RYT, ISCX, ISCY
      INTEGER   INCHAR, IANGL, I, ITRIM, IE, IEPO, IROUND, DCX, DCY,
     *   LLABEL
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DLOC.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DTVS.INC'
      DATA PLABEL /'Ecliptic Long.  ', 'Ecliptic Lat.   ',
     *   'Galactic Long.  ', 'Galactic Lat.   ', 'Right Ascension ',
     *   'Declination     ', 'Time (hours)    ', 'HA (hours)      ',
     *   'LST (hours)     '/
      DATA ELABEL /' (B1950)', ' (J2000)'/
C-----------------------------------------------------------------------
      CALL CHECKL ('TVLAB')
      XYRATO = (TRC(2) - BLC(2)) / (TRC(1) - BLC(1))
C                                       Draw border.
      CALL TVVEC (BLC(1), BLC(2), 1, IERR)
      IF (IERR.NE.0) GO TO 999
      CALL TVVEC (TRC(1), BLC(2), 2, IERR)
      IF (IERR.NE.0) GO TO 999
      CALL TVVEC (TRC(1), TRC(2), 2, IERR)
      IF (IERR.NE.0) GO TO 999
      CALL TVVEC (BLC(1), TRC(2), 2, IERR)
      IF (IERR.NE.0) GO TO 999
      CALL TVVEC (BLC(1), BLC(2), 2, IERR)
      IF (IERR.NE.0) GO TO 999
      LLABEL = MOD (ABS(LABEL), 100)
      IF (LLABEL.EQ.1) GO TO 999
C                                       Source name, stokes, freq.
      IF (LLABEL.LT.7) THEN
         DCX = BLC(1) * SCALEX + RX0
         DCY = TRC(2) * SCALEY + RY0 + 0.5 * CSIZTV(2) + 0.5
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
         IF (INCHAR.GT.0) THEN
            CALL IMCHAR (IGR, DCX, DCY, IANGL, 0, SPTEXT(:INCHAR),
     *         INBUF, IERR)
            IF (IERR.NE.0) GO TO 999
            END IF
         END IF
C                                       Text at bottom
      IF (NTEXT.GT.0) THEN
         DCX = BLC(1) * SCALEX + RX0
         DCY = BLC(2) * SCALEY + RY0 - YGAP * CSIZTV(2)
         DO 20 I = 1,NTEXT
            CALL CHTRIM (TEXT(I), 80, TEXT(I), INCHAR)
            IF (INCHAR.GT.0) THEN
               CALL IMCHAR (IGR, DCX, DCY, 0, 0, TEXT(I)(:INCHAR),
     *            INBUF, IERR)
               IF (IERR.NE.0) GO TO 999
               DCY = DCY - 1.333 * CSIZTV(2)
               END IF
 20         CONTINUE
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
      Y = ((BLC(2) + TRC(2)) / 2.0) * SCALEY + RY0
      DCX = BLC(1)*SCALEX+RX0 - (CH(1)-0.5)*CSIZTV(1) + 0.5
      IANGL = 3
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
      DCY = Y + (INCHAR-2)*CSIZTV(2) / 2.0 + 0.5
      IF (INCHAR.GT.0) THEN
         CALL IMCHAR (IGR, DCX, DCY, IANGL, 0, SPTEXT(:INCHAR), INBUF,
     *      IERR)
         IF (IERR.NE.0) GO TO 999
         END IF
C                                       horizontal axes
      I = MOD (LABTYP(LOCNUM), 10)
      X = ((BLC(1) + TRC(1)) / 2.0) * SCALEX + RX0
      DCY = BLC(2) * SCALEY + RY0 - 2.833*CSIZTV(2)
      IF (MOD(LABEL,100).EQ.2) DCY = BLC(2)*SCALEY + RY0 - 1.5*CSIZTV(2)
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
      DCX = X - (INCHAR*CSIZTV(1)) / 2.0
      IF (INCHAR.GT.0) THEN
         CALL IMCHAR (IGR, DCX, DCY, IANGL, 0, SPTEXT(:INCHAR), INBUF,
     *      IERR)
         IF (IERR.NE.0) GO TO 999
         END IF
C                                       do ticks
      IF (LLABEL.NE.2) THEN
         ISCX = 1.0 / SCALEX
         ISCY = 1.0 / SCALEY
         RXT = RX0 / SCALEX
         RYT = RY0 / SCALEY
         IF (AXINC(1,LOCNUM).NE.0) CALL ITICS (1, IGR-NGRAY, .FALSE.,
     *      RXT, RYT, ISCX, ISCY, BLC, TRC, DOGRID, INBUF, IERR)
         IF (AXINC(2,LOCNUM).NE.0) CALL ITICS (2, IGR-NGRAY, .FALSE.,
     *      RXT, RYT, ISCX, ISCY, BLC, TRC, DOGRID, INBUF, IERR)
         END IF
C
 999  RETURN
      END
