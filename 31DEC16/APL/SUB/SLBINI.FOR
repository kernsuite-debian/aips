      SUBROUTINE SLBINI (IDROP, INOSL, RANGE, BLC, TRC, XBLC, XTRC,
     *   FQFREQ, FQFINC, IDEPTH, LABEL, YGAP, CH, TEXT, NTEXT)
C-----------------------------------------------------------------------
C! initializes labeling for slice plots
C# Plot-util Coordinates
C-----------------------------------------------------------------------
C;  Copyright (C) 1995, 2001-2002, 2004, 2009, 2012, 2014
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
C   SLBINI prepares for labeling slices: applies end drops, computes
C   actual corners, inits /LOCATI/ and then converts it to represent
C   the slice, prepares text for labeling slice plots.
C   If XTRC(3) not equal XBLC(3) then the XY area is assumed summed and
C   a slice along the 3rd axis forms the slice data.
C   Inputs:
C      INOSL   I          # points in slice
C      RANGE   R(2)       Min/max true intensity range
C   In/out:
C      IDROP   I(2)       Left, right # points to drop from slice
C                         set to zero if too large
C      BLC     R(2)       Plot corners: sets(1), needs (2)
C      TRC     R(2)       Plot corners: sets (1), needs (2)
C      XBLC    R(7)       Map pixel pos: left end slice - resets
C                         (1),(2) for drops
C      XTRC    R(7)       Map pixel pos: right end slice - ""
C      FQFREQ  D          Frequency of 1st slice point when an FQID axis
C                         has been converted 0 -> not
C      FQFINC  R          Frequency increment in that case  0 -> not
C      LABEL   I          Requested labeling in, what you get out
C   Output:
C      IDEPTH  I(5)       Image depth axes 3 - 7
C      YGAP    R          # lines to go below plot for TEXT
C      CH      R(4)       # chars: left, bot, right, top outside
C      TEXT    C(2)*80    Text to put at bottom of plot
C      NTEXT   I          Number of lines used in text.
C   Common:
C      /MAPHDR/  input    image header
C      /LOCATI/  output   location common
C-----------------------------------------------------------------------
      INTEGER   IDROP(2), INOSL, LABEL, IDEPTH(5), NTEXT
      REAL      RANGE(2), BLC(2), TRC(2), XBLC(7), XTRC(7), FQFINC,
     *   CH(4), YGAP
      CHARACTER TEXT(2)*(*)
      DOUBLE PRECISION FQFREQ
C
      CHARACTER SPTEXT*20, CEND(2)*4, AXLABL*8
      INTEGER   I2TMP1, ILABEL, IERR, NAAX, IL, IE, ICH, ILEN, I, JLABEL
      REAL      OBLC(3), OTRC(3), DX, DY
      DOUBLE PRECISION X(3), Y(3), Z(3)
      LOGICAL   T, FLAG, NOTICK, ISFQ
      INCLUDE 'INCS:DLOC.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DCAT.INC'
      DATA T /.TRUE./
      DATA CEND /'LEND','REND'/
C-----------------------------------------------------------------------
      CALL CHECKL ('SLBINI')
      ILABEL = MOD (ABS (LABEL), 100)
      JLABEL = (ABS(LABEL) / 100) * 100
      YGAP = 0.0
      ISFQ = (FQFREQ.GT.0.0D0) .AND. (FQFINC.NE.0.0)
C                                       Init TEXT to blanks.
      TEXT(1) = ' '
      TEXT(2) = ' '
C                                       Test for spectrum on 3rd axis
      IF (ABS(XTRC(3)-XBLC(3)).LT.0.1) THEN
C                                       init /LOCATI/
         DO 10 I = 1,5
            IDEPTH(I) = XBLC(I+2) + 0.5
 10         CONTINUE
         CALL SETLOC (IDEPTH, T)
C                                       apply Drops
         BLC(1) = 1.0 + IDROP(1)
         TRC(1) = INOSL - IDROP(2)
         OBLC(1) = XBLC(1)
         OBLC(2) = XBLC(2)
         OTRC(1) = XTRC(1)
         OTRC(2) = XTRC(2)
C                                       invalid drops: ignore
         IF (TRC(1).LE.BLC(1)+2.0) THEN
            BLC(1) = 1.0
            TRC(1) = INOSL
            WRITE (MSGTXT,1010)
            CALL MSGWRT (6)
            IDROP(1) = 0
            IDROP(2) = 0
C                                       new map corners
         ELSE
            DX = (XTRC(1) - XBLC(1)) / INOSL
            DY = (XTRC(2) - XBLC(2)) / INOSL
            XBLC(1) = XBLC(1) + DX * IDROP(1)
            XBLC(2) = XBLC(2) + DY * IDROP(1)
            XTRC(1) = XTRC(1) - DX * IDROP(2)
            XTRC(2) = XTRC(2) - DY * IDROP(2)
            END IF
C                                       labeling type: set & check
         NOTICK = ILABEL.EQ.2
         NTEXT = 0
         CH(1) = 0.
         CH(2) = 0.
         CH(3) = 0.
         CH(4) = 0.
         IF (ILABEL.EQ.1) GO TO 999
         IF (ILABEL.EQ.2) ILABEL = 3
         NAAX = 0
         IF (XBLC(1).EQ.XTRC(1)) NAAX = 2
         IF (XBLC(2).EQ.XTRC(2)) NAAX = 1
         IF ((ILABEL.EQ.3) .AND. (NAAX.EQ.0)) ILABEL = 4
         IF ((ILABEL.EQ.7) .AND. (NAAX.EQ.0)) ILABEL = 8
         IF (((ILABEL.EQ.3) .OR. (ILABEL.EQ.7)) .AND.
     *      (ABS(ROT(LOCNUM)).GT.0.005) .AND. (NAAX+AXTYP(LOCNUM).NE.4)
     *      .AND. (AXTYP(LOCNUM).NE.4)) ILABEL = ILABEL + 1
         IF ((ILABEL.GT.3) .AND. (ILABEL.LT.7) .AND.
     *      (AXTYP(LOCNUM).NE.1).AND. (NAAX.EQ.0)) ILABEL = 99
         DX = (XBLC(1) + XTRC(1)) / 2.0
         DY = (XBLC(2) + XTRC(2)) / 2.0
C                                       "type 99" labels: end points
         IF (ILABEL.EQ.99) THEN
C                                       top label "z" axis
            IF ((AXTYP(LOCNUM).EQ.2) .OR. (AXTYP(LOCNUM).EQ.3)) THEN
               CALL XYVAL (DX, DY, X, Y, Z, IERR)
               IF (IERR.EQ.0) CALL AXSTRN (CTYP(3,LOCNUM), Z,
     *            KLOCA(LOCNUM), NCHLAB(1,LOCNUM), SAXLAB(1,LOCNUM))
               END IF
            NTEXT = 2
            IL = 3
            IF (AXTYP(LOCNUM).LT.2) IL = 2
            DO 20 IE = 1,2
               IF (IE.EQ.1) CALL XYVAL (XBLC, XBLC(2), X, X(2), X(3),
     *            IERR)
               IF (IE.EQ.2) CALL XYVAL (XTRC, XTRC(2), X, X(2), X(3),
     *            IERR)
               TEXT(IE) = CEND(IE)
               ICH = 5
               DO 15 I = 1,IL
                  ICH = ICH + 2
                  IF (IERR.EQ.0) THEN
                     I2TMP1 = I - 1
                     CALL AXSTRN (CTYP(I,LOCNUM), X(I), I2TMP1, ILEN,
     *                  SPTEXT)
                     TEXT(IE)(ICH:) = SPTEXT(:ILEN)
                     ICH = ICH + ILEN
                     END IF
 15               CONTINUE
 20            CONTINUE
C                                       x axis is null
            CPREF(1,LOCNUM) = ' '
            CTYP(1,LOCNUM) = ' '
            RPVAL(1,LOCNUM) = 0.0
            AXINC(1,LOCNUM) = 20.0 / (TRC(1)-BLC(1))
            LABTYP(LOCNUM) = 0
            RPLOC(1,LOCNUM) = (TRC(1) + BLC(1)) / 2.0
            YGAP = 1.5
            CH(3) = 0.5
            IF (ILABEL.GE.7) THEN
               NTEXT = 0
               CH(4) = 0.5
               CH(2) = 0.5
            ELSE
               CH(4) = 2.0
               IF (LABEL.GT.0) CH(4) = 3.5
               CH(2) = YGAP + (NTEXT-1) * 1.333 + 0.5
               END IF
C                                       Others: reset /LOCATI/
         ELSE
            IF (LABEL.LT.0) LABEL = -ILABEL - JLABEL
            IF (LABEL.GE.0) LABEL = ILABEL + JLABEL
            CH(1) = -101.
            IF ((ILABEL.EQ.4) .OR. (ILABEL.EQ.8)) THEN
               CALL LABINI (OBLC, OTRC, IDEPTH, CH, LABEL, T, YGAP,
     *            TEXT, NTEXT)
            ELSE
               CALL LABINI (XBLC, XTRC, IDEPTH, CH, LABEL, T, YGAP,
     *            TEXT, NTEXT)
               END IF
            IF ((NOTICK) .AND. (CTYP(1,LOCNUM).NE.' ')) THEN
               YGAP = YGAP - 1.333
               CH(2) = CH(2) - 1.333
               END IF
C                                       Values at special points
            CALL XYVAL (XBLC(1), XBLC(2), X(1), Y(1), Z(1), IERR)
            CALL XYVAL (XTRC(1), XTRC(2), X(2), Y(2), Z(2), IERR)
            CALL XYVAL (DX, DY, X(3), Y(3), Z(3), IERR)
C                                       Set up new x axis
            RPLOC(1,LOCNUM) = (TRC(1) + BLC(1)) / 2.0
C                                       FQID axis
            IF (ISFQ) THEN
               X(1) = FQFREQ + IDROP(1) * FQFINC
               X(2) = FQFREQ + (INOSL - IDROP(2)) * FQFINC
               RPLOC(1,LOCNUM) = (TRC(1) + BLC(1)) / 2.0
               AXINC(1,LOCNUM) = FQFINC
               CTYP(1,LOCNUM) = 'FREQ'
               DX = X(2) - X(1)
               IF ((ILABEL.EQ.3) .OR. (ILABEL.EQ.7)) THEN
                  RPVAL(1,LOCNUM) = (X(1) + X(2)) / 2.0D0
               ELSE IF ((ILABEL.EQ.4) .OR. (ILABEL.EQ.8)) THEN
                  RPVAL(1,LOCNUM) = 0.0D0
               ELSE IF ((ILABEL.EQ.5) .OR. (ILABEL.EQ.9)) THEN
                  RPVAL(1,LOCNUM) = 0.0D0
               ELSE IF ((ILABEL.EQ.6) .OR. (ILABEL.EQ.10)) THEN
                  RPVAL(1,LOCNUM) = (1+IDROP(1)+INOSL-IDROP(2)) / 2.0D0
                  AXINC(1,LOCNUM) = 1.0
C                                       interp always by 4
                  DX = (2.0D0 * RPVAL(1,LOCNUM)) / 4.0D0
                  CTYP(1,LOCNUM) = 'PIXELS'
                  END IF
               LABTYP(LOCNUM) = 0
               DY = DX
               CALL METSCL (LABEL, DY, CPREF(1,LOCNUM), FLAG)
               RPVAL(1,LOCNUM) = RPVAL(1,LOCNUM) * DY / DX
               AXINC(1,LOCNUM) = AXINC(1,LOCNUM) * DY / DX
C                                       Slice at const y
            ELSE IF (NAAX.EQ.1) THEN
               RPVAL(1,LOCNUM) = X(3)
               AXINC(1,LOCNUM) = (X(2) - X(1)) / (TRC(1) - BLC(1))
               LABTYP(LOCNUM) = MOD (LABTYP(LOCNUM), 10)
C                                       Slice at const x
            ELSE IF (NAAX.EQ.2) THEN
               RPVAL(1,LOCNUM) = Y(3)
               AXINC(1,LOCNUM) = (Y(2) - Y(1)) / (TRC(1) - BLC(1))
               CTYP(1,LOCNUM) = CTYP(2,LOCNUM)
               CPREF(1,LOCNUM) = CPREF(2,LOCNUM)
               LABTYP(LOCNUM) = LABTYP(LOCNUM) / 10
C                                       Diagonal: only type 4,5 here
            ELSE
               AXINC(1,LOCNUM) = SQRT ((X(2)-X(1))**2 + (Y(2)-Y(1))**2)
     *            / (TRC(1) - BLC(1))
               RPVAL(1,LOCNUM) = 0
               IF ((ILABEL.EQ.4) .OR. (ILABEL.EQ.8)) RPVAL(1,LOCNUM) =
     *            (RPLOC(1,LOCNUM) - (1.0+INOSL)/2.0) * AXINC(1,LOCNUM)
               END IF
            END IF
C                                       Rest of x axis
         ROT(LOCNUM) = 0.
         AXTYP(LOCNUM) = 0
         CORTYP(LOCNUM) = 0
C                                       Pretend all axes linear
         AXFUNC(1,LOCNUM) = 0
         AXFUNC(2,LOCNUM) = 0
C                                       Y axis -> intensity
         RPVAL(2,LOCNUM) = (RANGE(2) + RANGE(1)) / 2.0
         AXINC(2,LOCNUM) = (RANGE(2) - RANGE(1)) / (TRC(2) - BLC(2))
         RPLOC(2,LOCNUM) = (TRC(2) + BLC(2)) / 2.0
         CPREF(2,LOCNUM) = ' '
         CALL H2CHR (8, 1, CATH(KHBUN), CTYP(2,LOCNUM))
         DX = RANGE(2) - RANGE(1)
         DY = DX
         CALL METSCL (LABEL, DY, CPREF(2,LOCNUM), FLAG)
         RPVAL(2,LOCNUM) = RPVAL(2,LOCNUM) * DY / DX
         AXINC(2,LOCNUM) = AXINC(2,LOCNUM) * DY / DX
C                                       Character boundaries
         CH(1) = 2.5
         IF (NOTICK) THEN
            ILABEL = 2
         ELSE
            CALL CHNTIC (BLC, TRC, ICH)
            IF (ICH.GT.0) CH(1) = CH(1) + ICH + 1.5
            END IF
         IF (LABEL.LT.0) LABEL = -ILABEL - JLABEL
         IF (LABEL.GE.0) LABEL = ILABEL + JLABEL
C                                       spectrum on 3rd axis
      ELSE
C                                       init /LOCATI/
         DO 110 I = 1,5
            IDEPTH(I) = (XBLC(I+2) + XTRC(I+2)) / 2.0 + 0.5
 110        CONTINUE
         CALL SETLOC (IDEPTH, T)
C                                       apply Drops
         BLC(1) = 1.0 + IDROP(1)
         TRC(1) = INOSL - IDROP(2)
         OBLC(1) = XBLC(1)
         OBLC(2) = XBLC(2)
         OTRC(1) = XTRC(1)
         OTRC(2) = XTRC(2)
         OBLC(3) = XBLC(3)
         OTRC(3) = XTRC(3)
C                                       invalid drops: ignore
         IF (TRC(1).LE.BLC(1)+2.0) THEN
            BLC(1) = 1.0
            TRC(1) = INOSL
            WRITE (MSGTXT,1010)
            CALL MSGWRT (6)
            IDROP(1) = 0
            IDROP(2) = 0
C                                       new map corners
         ELSE
            DX = (XTRC(3) - XBLC(3)) / (INOSL - 1)
            XBLC(3) = XBLC(3) + DX * IDROP(1)
            XTRC(3) = XTRC(3) - DX * IDROP(2)
            END IF
C                                       labeling type: set & check
         NOTICK = ILABEL.EQ.2
         NTEXT = 0
         CH(1) = 0.
         CH(2) = 0.
         CH(3) = 0.
         CH(4) = 0.
         IF (ILABEL.EQ.1) GO TO 999
         IF (ILABEL.EQ.2) ILABEL = 3
         IF (LABEL.LT.0) LABEL = -ILABEL - JLABEL
         IF (LABEL.GE.0) LABEL = ILABEL + JLABEL
         NAAX = 3
         DX = (XBLC(1) + XTRC(1)) / 2.0
         DY = (XBLC(2) + XTRC(2)) / 2.0
         CH(1) = -101.
C                                       extra line
         CALL XYVAL (DX, DY, X(3), Y(3), Z(3), IERR)
         NTEXT = 1
         TEXT(1) = 'XY Center: '
         ICH = 12
         CALL H2CHR (8, 1, CATH(KHCTP), AXLABL)
         CALL AXSTRN (AXLABL, X(3), 0, ILEN, SPTEXT)
         TEXT(1)(ICH:) = SPTEXT
         ICH = ICH + ILEN + 2
         CALL H2CHR (8, 1, CATH(KHCTP+2), AXLABL)
         CALL AXSTRN (AXLABL, Y(3), 1, ILEN, SPTEXT)
         TEXT(1)(ICH:) = SPTEXT
         IF (ISFQ) THEN
            X(1) = FQFREQ + IDROP(1) * FQFINC
            X(2) = FQFREQ + (INOSL - IDROP(2)) * FQFINC
            RPLOC(1,LOCNUM) = (TRC(1) + BLC(1)) / 2.0
            AXINC(1,LOCNUM) = FQFINC
            CTYP(1,LOCNUM) = 'FREQ'
            DX = X(2) - X(1)
            IF ((ILABEL.EQ.3) .OR. (ILABEL.EQ.7)) THEN
               RPVAL(1,LOCNUM) = (X(1) + X(2)) / 2.0D0
            ELSE IF ((ILABEL.EQ.4) .OR. (ILABEL.EQ.8)) THEN
               RPVAL(1,LOCNUM) = 0.0D0
            ELSE IF ((ILABEL.EQ.5) .OR. (ILABEL.EQ.9)) THEN
               RPVAL(1,LOCNUM) = 0.0D0
            ELSE IF ((ILABEL.EQ.6) .OR. (ILABEL.EQ.10)) THEN
               RPVAL(1,LOCNUM) = (1+IDROP(1)+INOSL-IDROP(2)) / 2.0D0
               AXINC(1,LOCNUM) = 1.0
C                                       interp always by 4
               DX = (2.0D0 * RPVAL(1,LOCNUM)) / 4.0D0
               CTYP(1,LOCNUM) = 'PIXELS'
               END IF
         ELSE
            X(2) = CATR(KRCIC+2) * (XTRC(3) - CATR(KRCRP+2))
            X(1) = CATR(KRCIC+2) * (XBLC(3) - CATR(KRCRP+2))
            RPLOC(1,LOCNUM) = (TRC(1) + BLC(1)) / 2.0
            AXINC(1,LOCNUM) = (X(2) - X(1)) / (TRC(1) - BLC(1))
            CALL H2CHR(8, 1, CATH(KHCTP+4), CTYP(1,LOCNUM))
            DX = X(2) - X(1)
            IF ((ILABEL.EQ.3) .OR. (ILABEL.EQ.7)) THEN
               RPVAL(1,LOCNUM) = (X(1) + X(2)) / 2.0D0 + CATD(KDCRV+2)
            ELSE IF ((ILABEL.EQ.4) .OR. (ILABEL.EQ.8)) THEN
               RPVAL(1,LOCNUM) = (X(1) + X(2)) / 2.0D0
            ELSE IF ((ILABEL.EQ.5) .OR. (ILABEL.EQ.9)) THEN
               RPVAL(1,LOCNUM) = 0.0
            ELSE IF ((ILABEL.EQ.6) .OR. (ILABEL.EQ.10)) THEN
               RPVAL(1,LOCNUM) = (XBLC(3) + XTRC(3)) / 2.0D0
               AXINC(1,LOCNUM) = (XTRC(3) - XBLC(3)) / (TRC(1) - BLC(1))
               DX = XTRC(3) - XBLC(3)
               CTYP(1,LOCNUM) = 'PIXELS'
               END IF
            END IF
         LABTYP(LOCNUM) = 0
         DY = DX
         CALL METSCL (LABEL, DY, CPREF(1,LOCNUM), FLAG)
         RPVAL(1,LOCNUM) = RPVAL(1,LOCNUM) * DY / DX
         AXINC(1,LOCNUM) = AXINC(1,LOCNUM) * DY / DX
C                                       Rest of x axis
         ROT(LOCNUM) = 0.
         AXTYP(LOCNUM) = 0
         CORTYP(LOCNUM) = 0
C                                       Pretend all axes linear
         AXFUNC(1,LOCNUM) = 0
         AXFUNC(2,LOCNUM) = 0
C                                       Y axis -> intensity
         RPVAL(2,LOCNUM) = (RANGE(2) + RANGE(1)) / 2.0
         AXINC(2,LOCNUM) = (RANGE(2) - RANGE(1)) / (TRC(2) - BLC(2))
         RPLOC(2,LOCNUM) = (TRC(2) + BLC(2)) / 2.0
         CPREF(2,LOCNUM) = ' '
         CALL H2CHR (8, 1, CATH(KHBUN), CTYP(2,LOCNUM))
         DX = RANGE(2) - RANGE(1)
         DY = DX
         CALL METSCL (LABEL, DY, CPREF(2,LOCNUM), FLAG)
         RPVAL(2,LOCNUM) = RPVAL(2,LOCNUM) * DY / DX
         AXINC(2,LOCNUM) = AXINC(2,LOCNUM) * DY / DX
C                                       Character boundaries
         YGAP = 4.5
         IF (ILABEL.GT.6) YGAP = 3.0
         CH(1) = 2.5
         CH(2) = YGAP + 0.5
         CH(4) = 3.5
         IF (LABEL.LT.0) CH(4) = 2.0
         IF (ILABEL.GT.6) CH(4) = 0.5
         IF (NOTICK) THEN
            ILABEL = 2
         ELSE
            CALL CHNTIC (BLC, TRC, ICH)
            IF (ICH.GT.0) CH(1) = CH(1) + ICH + 1.5
            END IF
         IF (LABEL.LT.0) THEN
            LABEL = -ILABEL - JLABEL
         ELSE
            LABEL = ILABEL + JLABEL
            END IF
         END IF
C
 999  RETURN
C-----------------------------------------------------------------------
 1010 FORMAT ('BDROP AND EDROP IGNORED: OUT OF RANGE')
      END
