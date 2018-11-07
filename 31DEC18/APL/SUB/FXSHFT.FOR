      SUBROUTINE FXSHFT (INCATR, IERR)
C-----------------------------------------------------------------------
C;  Copyright (C) 1995, 1999
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
C   This routine works for -SIN projection without 3D only.  It should
C   be obsolete and is not currently called.
C
C   FXSHFT compares the image reference pixel and the shift and rotate
C   parameters in the supplied image header.  If they disagree, FXSHFT
C   assumes that the error is due to the old error in MX and UVMAP in
C   which the rotation was used in degrees where radians was required.
C   It changes the header for this, if that assumption produces
C   agreement.  The bug was in images produced by UVMAP and MX prior to
C   19-JUL-93 in release 15JUL93 and all earlier releases.
C   In/out:
C      INCATR   R(256)   Image header
C   Output:
C      IERR     I        Condition code: 0 nothing done, all okay
C                           -1 => header fixed
C                           +1 => shift and reference pixel inconsistent
C                                 even with fix.
C-----------------------------------------------------------------------
      REAL      INCATR(256)
      INTEGER   IERR
C
      INTEGER   CATBLK(256), I, IR, ID, N, J
      REAL      CATR(256), ROT, XP, YP
      HOLLERITH CATH(256)
      DOUBLE PRECISION CATD(128), XSHIFT, YSHIFT, YSHRFT, XSHRFT, DEC,
     *   XS, YS, SS, RA, XDEC, XRA
      CHARACTER CHTEMP*8
      INCLUDE 'INCS:PSTD.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DMSG.INC'
      EQUIVALENCE (CATBLK, CATR, CATH, CATD)
C-----------------------------------------------------------------------
      CALL RCOPY (256, INCATR, CATR)
C                                       find RA/Dec axes
      N = CATBLK(KIDIM)
      IR = -1
      ID = -1
      DO 10 I = 1,N
         J = KHCTP + 2 * (I - 1)
         CALL H2CHR (8, 1, CATR(J), CHTEMP)
         IF ((CHTEMP(:4).EQ.'RA  ') .OR. (CHTEMP(:4).EQ.'RA--'))
     *      IR = I - 1
         IF ((CHTEMP(:4).EQ.'DEC ') .OR. (CHTEMP(:4).EQ.'DEC-'))
     *      ID = I - 1
 10      CONTINUE
      IF ((IR.LT.0) .OR. (ID.LT.0)) THEN
         IERR =  0
         MSGTXT = 'FXSHFT: COORDINATE AXIS MISSING'
         CALL MSGWRT (6)
         GO TO 999
         END IF
C                                       no shift: check reference
      XP = CATBLK(KINAX+IR) / 2
      YP = CATBLK(KINAX+ID) / 2 + 1
      IF ((CATR(KRXSH).EQ.0.0) .AND. (CATR(KRYSH).EQ.0.0)) THEN
         IERR = 0
         IF ((ABS(XP-CATR(KRCRP+IR)).GT.0.03) .OR.
     *      (ABS(YP-CATR(KRCRP+ID)).GT.0.03)) THEN
            IERR =  1
            MSGTXT = 'FXSHFT: REFERENCE PIXELS OFF WITH NO SHIFTS'
            CALL MSGWRT (7)
            END IF
         GO TO 999
C                                       shift
      ELSE
C                                       do we have a problem?
C                                       find XSH, YSH from RP
         DEC = CATD(KDCRV+ID) * DG2RAD
         RA = CATD(KDCRV+IR) * DG2RAD
         ROT = CATR(KRCRT+ID) * DG2RAD
         XS = ABS (CATR(KRCIC+IR)) * (CATR(KRCRP+IR) - XP)
         YS = ABS (CATR(KRCIC+ID)) * (CATBLK(KINAX+ID) - YP + 2 -
     *      CATR(KRCRP+ID))
         XSHRFT = (XS * COS (ROT) - YS * SIN (ROT)) * DG2RAD
         YSHRFT = (XS * SIN (ROT) + YS * COS (ROT)) * DG2RAD
         XS = ATAN (XSHRFT / (YSHRFT * SIN (DEC) - COS (DEC) *
     *      SQRT (1.0D0 - XSHRFT * XSHRFT - YSHRFT * YSHRFT)))
         XRA = RA - XS
         XDEC = ASIN (YSHRFT * COS (DEC) + SIN (DEC) *
     *      SQRT (1.0D0 - XSHRFT * XSHRFT - YSHRFT * YSHRFT))
         XSHIFT = (XRA - RA) / DG2RAD
         YSHIFT = (XDEC - DEC) / DG2RAD
         XS = MAX (ABS(CATR(KRCIC+IR)), ABS(CATR(KRCIC+ID))) * 0.05
         IERR = 0
         IF ((ABS(XSHIFT-CATR(KRXSH)).GT.XS) .OR.
     *      (ABS(YSHIFT-CATR(KRYSH)).GT.XS)) THEN
C                                       find RP from XSH, YSH
C                                       transcendental problem
            SS = 1.0D0
            DO 50 I = 1,40
               YS = (SIN (DEC + CATR(KRYSH)*DG2RAD) - SIN (DEC) * SS) /
     *            COS (DEC)
               XS = TAN(DG2RAD * CATR(KRXSH)) * (COS(DEC) * SS - YS *
     *            SIN(DEC))
               SS = SQRT ( 1.0D0 - XS * XS - YS * YS)
 50            CONTINUE
            YSHRFT = (SIN (DEC + CATR(KRYSH)*DG2RAD) - SIN (DEC) * SS) /
     *         COS (DEC)
            XSHRFT = TAN(DG2RAD * CATR(KRXSH)) * (COS(DEC) * SS -
     *         YSHRFT * SIN(DEC))
            XSHIFT = XSHRFT * COS (ROT) + YSHRFT * SIN (ROT)
            YSHIFT = YSHRFT * COS (ROT) - XSHRFT * SIN (ROT)
            XSHIFT = XSHIFT * RAD2DG / ABS (CATR(KRCIC+IR))
            YSHIFT = YSHIFT * RAD2DG / ABS (CATR(KRCIC+ID))
            XP = XP + XSHIFT
            YP = YP - YSHIFT
C                                       disagree
            IF ((ABS(XP-CATR(KRCRP+IR)).GT.0.04) .OR.
     *         (ABS(YP-CATR(KRCRP+ID)).GT.0.04)) THEN
C                                       try fix
               IF (ROT.NE.0.0) THEN
                  ROT = CATR(KRCRT+ID)
                  XSHIFT = XSHRFT * COS (ROT) + YSHRFT * SIN (ROT)
                  YSHIFT = YSHRFT * COS (ROT) - XSHRFT * SIN (ROT)
                  XSHIFT = XSHIFT * RAD2DG / ABS (CATR(KRCIC+IR))
                  YSHIFT = YSHIFT * RAD2DG / ABS (CATR(KRCIC+ID))
                  XP = CATBLK(KINAX+IR) / 2 + XSHIFT
                  YP = CATBLK(KINAX+ID) / 2 + 1 - YSHIFT
C                                       fix it
                  IF ((ABS(XP-CATR(KRCRP+IR)).LE.0.003) .AND.
     *               (ABS(YP-CATR(KRCRP+ID)).LE.0.003)) THEN
                     XSHIFT = XSHIFT / RAD2DG * ABS (CATR(KRCIC+IR))
                     YSHIFT = YSHIFT / RAD2DG * ABS (CATR(KRCIC+ID))
                     ROT = CATR(KRCRT+ID) * DG2RAD
                     XSHRFT = XSHIFT * COS (ROT) - YSHIFT * SIN (ROT)
                     YSHRFT = YSHIFT * COS (ROT) + XSHIFT * SIN (ROT)
                     SS = SQRT (1.0D0 - XSHRFT * XSHRFT - YSHRFT
     *                  * YSHRFT)
                     INCATR(KRXSH) = ATAN (XSHRFT / (COS(DEC) * SS -
     *                  YSHRFT * SIN(DEC))) * RAD2DG
                     INCATR(KRYSH) = (ASIN (YSHRFT * COS(DEC) + SIN(DEC)
     *                  * SS) - DEC) * RAD2DG
                     MSGTXT = 'FXSHFT: fixed pre-19-July-1993 header' //
     *                  ' shift bug'
                     CALL MSGWRT (2)
                     IERR = -1
                     GO TO 999
                     END IF
                  END IF
               MSGTXT = 'FXSHFT: SHIFTS AND REFERENCE PIXELS DISAGREE'
               IERR = 1
               CALL MSGWRT (7)
               END IF
            END IF
         END IF
C
 999  RETURN
      END


