      SUBROUTINE LABINI (BLC, TRC, IDEP, CH, ILTYPE, SLICE, YGAP, TEXT,
     *   NTEXT)
C-----------------------------------------------------------------------
C! initializes commons for labeling of plots (calls SETLOC)
C# Coordinates Plot-util
C-----------------------------------------------------------------------
C;  Copyright (C) 1995, 2002, 2007, 2009, 2012, 2014
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
C   LABINI performs initializations for labeling: calls SETLOC, converts
C   units (w METSCL) to get reasonable scaling, and, for LTYPEC   = 4
C   (center-relative labeling), converts units ("lies") and prepares
C   text giving true center position.
C   Inputs:
C      BLC    R(2)     Bottom left corner
C      TRC    R(2)     Top right corner
C      IDEP   I(5)     Depth on axes 3 - 7
C      ILTYPE I        Labeling type: 1 none, 2 no ticks, 3 RA/dec,
C                      4 center-relative, 5 subim center-rel, 6 pixels,
C                      7 like 3, but no top line + n *100 for METSCL
C      SLICE  L        T => for slice display: rotation will be the
C                      slice PA - any map rotation; F => for maps: the
C                      rotation is any map rot.
C   In/out:
C      CH     R(4)     # chars outside plot to left, bot, right, top:
C                      on in # extra for plot, on out totals
C                      These must have input, reasonable values, LABINI
C                      just adds a 0.5 border and room for the labeling.
C                      IF CH(1) < -10., do not count axis label
C                      characters with CHNTIC and CH(1) has little
C                      meaning on output.
C   Output:
C      YGAP   R        Char row position for 1st line of text at bottom
C      TEXT   C(2)*80  Text to put at YGAP, used only if LTYPE=4
C      NTEXT  I        Number of lines used in TEXT: 0, 1, 2
C-----------------------------------------------------------------------
      REAL      BLC(2), TRC(2), CH(4), YGAP
      INTEGER   ILTYPE, IDEP(5), NTEXT
      LOGICAL   SLICE
      CHARACTER TEXT(2)*(*)
C
      CHARACTER RCHARS*8, SPTXT*20, ATYPE(8)*4, NTYPE(8)*20
      REAL      DX, DY, RROTA, DXX, INCH
      DOUBLE PRECISION    X(4)
      INTEGER   IL, ICH, ILEN, NCTY, IX, IR, I, J, I2TMP1, LTYPE, IERR,
     *   JTRIM
      LOGICAL   T, FLAG, TIC(2)
      INCLUDE 'INCS:DLOC.INC'
      DATA NCTY /8/
      DATA ATYPE /'TIME','FREQ','LAMB','VELO','FELO','    ','DIST',
     *   'ANGL'/
      DATA NTYPE /'Seconds ', 'Hertz ', 'Meters ', 'Meters/sec',
     *   'Meters/sec', 'Pixels  ', 'Degrees ', 'Arc seconds '/
      DATA T /.TRUE./
      DATA RCHARS /'ROT '/
C-----------------------------------------------------------------------
      NTEXT = 0
      LTYPE = MOD (ABS (ILTYPE), 100)
      INCH = CH(1)
C                                       init common /LOCATI/
      CALL CHECKL ('LABINI')
      CALL SETLOC (IDEP, T)
C                                       Center value
      DX = (BLC(1) + TRC(1)) / 2.0
      DY = (BLC(2) + TRC(2)) / 2.0
      IF ((SLICE) .OR. ((LTYPE.NE.4) .AND. (LTYPE.NE.8)))
     *   CALL XYVAL (DX, DY, X(1), X(2), X(3), IERR)
      IF ((.NOT.SLICE) .AND. ((LTYPE.EQ.4) .OR. (LTYPE.EQ.8)))
     *   CALL XYVAL (RPLOC(1,LOCNUM), RPLOC(2,LOCNUM), X(1), X(2), X(3),
     *   IERR)
C                                       Z-axis label string
      IF ((AXTYP(LOCNUM).EQ.2) .OR. (AXTYP(LOCNUM).EQ.3)) CALL AXSTRN
     *   (CTYP(3,LOCNUM), X(3), KLOCA(LOCNUM), NCHLAB(1,LOCNUM),
     *   SAXLAB(1,LOCNUM))
      DX = ABS (AXINC(1,LOCNUM) * (TRC(1) - BLC(1)))
      DY = ABS (AXINC(2,LOCNUM) * (TRC(2) - BLC(2)))
      DXX = MAX (DX, DY)
C                                       center-relative labeling
      IF ((LTYPE.GE.4) .AND. (LTYPE.NE.7)) THEN
C                                       special for slice along X axis
         IF ((SLICE) .AND. ((TRC(2).EQ.BLC(2)) .OR. (TRC(1).EQ.BLC(1)))
     *      .AND. ((AXTYP(LOCNUM).EQ.2) .OR. (AXTYP(LOCNUM).EQ.3))) THEN
            NTEXT = 1
            IF (BLC(1).EQ.TRC(1)) CALL AXSTRN (CTYP(1,LOCNUM), X(1), 0,
     *         ILEN, SPTXT)
            IF (BLC(2).EQ.TRC(2)) CALL AXSTRN (CTYP(2,LOCNUM), X(2), 1,
     *         ILEN, SPTXT)
            TEXT(1) = 'Center at  '
            ICH = 11
            TEXT(1)(ICH:) = SPTXT(:ILEN)
            IF ((AXTYP(LOCNUM).EQ.2) .OR. (AXTYP(LOCNUM).EQ.3)) THEN
               ICH = JTRIM (TEXT(1)) + 3
               CALL AXSTRN (CTYP(3,LOCNUM), X(3), 2, ILEN, SPTXT)
               TEXT(1)(ICH:) = SPTXT(:ILEN)
               END IF
            CH(2) = CH(2) + NTEXT * 1.3333
         ELSE
C                                       preserve the truth
            RROTA = ROT(LOCNUM)
            IF (SLICE) RROTA = 180.0 * ATAN2 (AXINC(1,LOCNUM) *
     *         (TRC(1)-BLC(1)), AXINC(2,LOCNUM) * (TRC(2)-BLC(2))) /
     *         3.14159265  -  ROT(LOCNUM)
            IL = 3
            IR = IL
            X(IL) = RROTA
            IF (RROTA.EQ.0.0) IL = IL - 1
            NTEXT = 1
            IX = 1
            TEXT(1) = 'Center at  '
            TEXT(2) = ' '
            ICH = 11
            DO 10 I = 1,IL
               IF (I.EQ.3) THEN
                  IX = 2
                  ICH = 1
                  NTEXT = 2
                  END IF
               IF (I.EQ.IR) CALL AXSTRN (RCHARS, X(I), 2, ILEN, SPTXT)
               I2TMP1 = I - 1
               IF (I.NE.IR) CALL AXSTRN (CTYP(I,LOCNUM), X(I), I2TMP1,
     *            ILEN, SPTXT)
               TEXT(IX)(ICH:) = SPTXT(:ILEN)
               ICH = ICH + ILEN + 2
 10            CONTINUE
            END IF
C                                       Lie for Pixel type labeling
         IF ((LTYPE.EQ.6) .OR. (LTYPE.EQ.10)) THEN
            RPVAL(1,LOCNUM) = BLC(1)
            RPVAL(2,LOCNUM) = BLC(2)
            RPLOC(1,LOCNUM) = BLC(1)
            RPLOC(2,LOCNUM) = BLC(2)
            ROT(LOCNUM) = 0.0
            AXINC(1,LOCNUM) = 1.0
            AXINC(2,LOCNUM) = 1.0
            CTYP(1,LOCNUM) = NTYPE(6)
            CTYP(2,LOCNUM) = NTYPE(6)
            AXFUNC(1,LOCNUM) = 0
            AXFUNC(2,LOCNUM) = 0
C                                       Now "lie": refs to zero
         ELSE
            RPVAL(1,LOCNUM) = 0.0
            RPVAL(2,LOCNUM) = 0.0
            RPVAL(3,LOCNUM) = 0.0
            ROT(LOCNUM) = 0.0
            DX = DXX
            DO 40 I = 1,2
               AXFUNC(I,LOCNUM) = 0
C                                       find type
               DO 20 J = 1,NCTY
                  IF (CTYP(I,LOCNUM)(1:4).EQ.ATYPE(J)) GO TO 25
 20               CONTINUE
               IF ((KLOCL(LOCNUM)+1.NE.I) .AND. (KLOCM(LOCNUM)+1.NE.I))
     *            GO TO 40
               J = 7
C                                       angles: degrees or arcsec
 25            IF (J.GE.7) THEN
                  CTYP(I,LOCNUM) = NTYPE(7)
                  IF (AXTYP(LOCNUM).NE.1) DX = ABS (AXINC(I,LOCNUM) *
     *               (TRC(I)-BLC(I)))
                  IF (DX.LT.1.001) THEN
                     AXINC(I,LOCNUM) = AXINC(I,LOCNUM) * 3600.0
                     CTYP(I,LOCNUM) = NTYPE(8)
                     END IF
C                                       others: insert units
               ELSE
                  CTYP(I,LOCNUM) = NTYPE(J)
                  END IF
 40            CONTINUE
            END IF
C                                       No longer special axes
 45      CORTYP(LOCNUM) = 0
         LABTYP(LOCNUM) = 0
C                                       Sub image centering
         IF ((LTYPE.EQ.5) .OR. (LTYPE.EQ.9)) THEN
            RPLOC(1,LOCNUM) = (BLC(1)+TRC(1))/2.0
            RPLOC(2,LOCNUM) = (BLC(2)+TRC(2))/2.0
            END IF
         END IF
C                                       Extra labels for Slices
C                                       LTYPE=3 only for x or y const.
      IF  ((SLICE) .AND. (LTYPE.EQ.3)) THEN
         NTEXT = 1
         IF (BLC(1).EQ.TRC(1)) CALL AXSTRN (CTYP(1,LOCNUM), X(1), 0,
     *      ILEN, SPTXT)
         IF (BLC(2).EQ.TRC(2)) CALL AXSTRN (CTYP(2,LOCNUM), X(2), 1,
     *      ILEN, SPTXT)
         TEXT(1) = 'Center at  '
         ICH = 11
         TEXT(1)(ICH:) = SPTXT(:ILEN)
         IF ((AXTYP(LOCNUM).EQ.2) .OR. (AXTYP(LOCNUM).EQ.3)) THEN
            ICH = JTRIM (TEXT(1)) + 3
            CALL AXSTRN (CTYP(3,LOCNUM), X(3), 2, ILEN, SPTXT)
            TEXT(1)(ICH:) = SPTXT(:ILEN)
            END IF
         CH(2) = CH(2) + NTEXT * 1.3333
         END IF
C                                       Scale non-angle by 10**3's
 60   DX = ABS (AXINC(1,LOCNUM) * (TRC(1) - BLC(1)))
      DY = ABS (AXINC(2,LOCNUM) * (TRC(2) - BLC(2)))
      DX = MAX (DX, DY)
      DO 70 I = 1,2
         IF (((KLOCL(LOCNUM)+1.EQ.I) .OR. (KLOCM(LOCNUM)+1.EQ.I)) .AND.
     *      (LABTYP(LOCNUM).GT.0)) GO TO 70
            IF (AXTYP(LOCNUM).NE.1) DX = ABS (AXINC(I,LOCNUM) * (BLC(I)
     *         - TRC(I)))
            IF (DX.EQ.0.0) GO TO 70
               DY = DX
               CALL METSCL (ILTYPE, DY, CPREF(I,LOCNUM), FLAG)
               RPVAL(I,LOCNUM) = RPVAL(I,LOCNUM) * DY / DX
               AXINC(I,LOCNUM) = AXINC(I,LOCNUM) * DY / DX
 70      CONTINUE
C                                       character borders: .5 safety
      YGAP = 0.0
      DO 75 I = 1,4
         CH(I) = CH(I) + 0.5
 75      CONTINUE
      IF ((LTYPE.GT.6) .OR. (LTYPE.EQ.1)) NTEXT = 0
C                                       do some labeling
      IF (LTYPE.NE.1) THEN
         DO 80 I = 1,2
            TIC(I) = (CPREF(I,LOCNUM).NE.' ') .OR. (CTYP(I,LOCNUM).NE.
     *         ' ')
 80         CONTINUE
         IF (TIC(2)) CH(1) = CH(1) + 2.5
         IF (TIC(1)) CH(2) = CH(2) + 1.5
         IF (LTYPE.LT.7) CH(4) = CH(4) + 1.5
         IF ((LTYPE.LT.7) .AND. (ILTYPE.GT.0)) CH(4) = CH(4) + 1.333
         YGAP = 1.5
         IF (TIC(1)) YGAP = YGAP + 1.333
C                                       including ticks
         IF (LTYPE.NE.2) THEN
            IF (TIC(1)) YGAP = YGAP + 1.333
            ICH = 0
            IF (INCH.GT.-10.) CALL CHNTIC (BLC, TRC, ICH)
            IF (ICH.GT.0) CH(1) = CH(1) + ICH + 1.0
            IF (TIC(1)) CH(2) = CH(2) + 1.333
C                                       and center pos info
            IF ((LTYPE.NE.3) .AND. (LTYPE.LT.7)) THEN
               CH(2) = CH(2) + NTEXT * 1.3333
               AXTYP(LOCNUM) = 0
               END IF
            END IF
         END IF
C
 999  RETURN
      END
