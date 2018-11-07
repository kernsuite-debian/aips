      SUBROUTINE DGINIT (STOKES, BCHAN, ECHAN, BIF, EIF, MVIS, JADR,
     *   SELFAC, ALLWT, PMODE, IERR)
C-----------------------------------------------------------------------
C! Sets arrays for selecting data and changing Stokes' parameter.
C# UV Calibration
C-----------------------------------------------------------------------
C;  Copyright (C) 1995-1996, 2007, 2010, 2016-2017
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
C   Sets up tables for selecting data from vis. record. Checks if
C   requested data in data base.  Requires catalog header record
C   in common /MAPHDR/ and setup of common /UVHDR/ by UVPGET before
C   call.
C   Note: STOKES='HALF' will work if only partial information (i.e.
C   1 polarization) is available in the data.
C   Inputs:
C      STOKES   C*4      Desired output data format: 'I','V','Q','U',
C                        'IQU','IQUV','IV','RR','LL','RL','LR'
C                        'HALF' (=parallel pol.), 'FULL' (=RR,LL,RL,LR)
C      BCHAN    I        First channel desired.
C      ECHAN    I        Last channel desired.
C      BIF      I        First IF desired.
C      EIF      I        Last IF desired.
C   Input from common /MAPHDR/
C      CATBLK   I(256)   Catalog header record.
C   Output:
C      MVIS     I(8)     Number of visibilities in requested output
C                        format., # pol, BIF, EIF, BCH, ECH, JNCIF, JNCF
C      JADR     I(2,4)   Pointers to the first and second visibility
C                        input records to be used in the output record.
C                        If JADR(1,n) is negative use IABS (JADR(1,n))
C                        and multiply the visibility by i (=SQRT(-1))
C      SELFAC   R(2,4)   Factors to be multiplied by the first and
C                        second input vis's to make the output vis.
C      ALLWT    L        Flag, = .TRUE. if all visibilities must have
C                        positive weight.
C      PMODE    I        Polarization mode:
C                            1 = I,    2 = V,   3 = Q
C                            4 = U,    5 = IQU, 6 = IQUV
C                            7 = IV,   8 = QU   9 = RR,  10 = LL
C                           11 = RL,  12 = LR, 13 = parallel (RR,LL)
C                           14 = (RL,LR)   15 = (RR,LL,RL,LR)
C                           16 = XX,  17 = YY, 18 = XY, 19 = YX,
C                           20=XXYY,  21 = XYYX  22=(XX,YY,XY,YX)
C      IERR     I        Error flag. 0 => ok, 1 = unrecognized stokes,
C                           2 = data unavailable.
C-----------------------------------------------------------------------
      CHARACTER STOKES*4
      INTEGER   BCHAN, ECHAN, BIF, EIF, MVIS(8), JADR(2,4), PMODE, IERR
      REAL      SELFAC(2,4)
      LOGICAL   ALLWT
C
      INTEGER   NMODE, NMODEF
      PARAMETER (NMODE = 22, NMODEF = 7)
      CHARACTER CHIF*8, CHMODE(NMODE)*4, CHMODF(NMODEF)*4, LSTOKE*4
      INTEGER   I, IFB, IFE, ICB, ICE, JERR, JNCS, JNCF, JNCIF,
     *   IFOFF, IVPNT, KCOR0
      LOGICAL   FORMAL
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DPDC.INC'
      DATA CHMODE /'I', 'V', 'Q', 'U', 'IQU', 'IQUV', 'IV', 'QU',
     *   'RR', 'LL', 'RL', 'LR', 'RRLL', 'RLLR', 'RLRL',
     *   'XX', 'YY', 'XY', 'YX', 'XXYY', 'XYYX', 'XYXY'/
      DATA CHMODF /'F', 'V', 'Q', 'U', 'FQU', 'FQUV', 'FV'/
      DATA CHIF /'IF'/
C-----------------------------------------------------------------------
      IERR = 1
C                                       Stokes in data
      IF (CATD(KDCRV+JLOCS).GT.0.0D0) THEN
         KCOR0 = CATD(KDCRV+JLOCS) + 0.5D0
      ELSE
         KCOR0 = CATD(KDCRV+JLOCS) - 0.5D0
         END IF
C                                       Find PMODE
      LSTOKE = STOKES
C                                       translate general terms
      IF (KCOR0.LE.-5) THEN
         IF (LSTOKE.EQ.'HALF') LSTOKE = 'XXYY'
         IF (LSTOKE.EQ.'CROS') LSTOKE = 'XYYX'
         IF (LSTOKE.EQ.'FULL') LSTOKE = 'XYXY'
      ELSE
         IF (LSTOKE.EQ.'HALF') LSTOKE = 'RRLL'
         IF (LSTOKE.EQ.'CROS') LSTOKE = 'RLLR'
         IF (KCOR0.GT.0) THEN
            IF (LSTOKE.EQ.'FULL') LSTOKE = 'IQUV'
         ELSE
            IF (LSTOKE.EQ.'FULL') LSTOKE = 'RLRL'
            END IF
         END IF
      PMODE = -1
      FORMAL = .FALSE.
      DO 5 I = 1,NMODE
         IF (LSTOKE.EQ.CHMODE(I)) PMODE = I
 5       CONTINUE
      IF (PMODE.LT.0) THEN
         DO 6 I = 1,NMODEF
            IF (LSTOKE.EQ.CHMODF(I)) PMODE = I
 6          CONTINUE
         FORMAL = .TRUE.
         END IF
C                                       Unrecognized STOKES'
      IF (PMODE.LT.0) THEN
         WRITE (MSGTXT,1010) STOKES
         GO TO 990
         END IF
C                                       Set pointers.
      JNCS = INCS / MAX (1, CATBLK(KINAX))
      JNCF = INCF / MAX (1, CATBLK(KINAX))
C                                       Get number channels, IFs
      IFB = BIF
      IFE = EIF
      ICB = BCHAN
      ICE = ECHAN
      IERR = 1
C                                       Check channel.
C                                       See if null spec. = all
      IF ((ICB.LE.0) .AND. (JLOCF.GE.0)) ICE = CATBLK(KINAX+JLOCF)
      ICB = MAX (ICB, 1)
      ICE = MAX (ICE, 1)
C                                       Axis exists - check
      IF (JLOCF.GE.0) THEN
         IF (ICB.GT.CATBLK(KINAX+JLOCF)) GO TO 970
         IF (ICE.GT.CATBLK(KINAX+JLOCF)) GO TO 970
         IF (ICE.LT.ICB) GO TO 970
         END IF
C                                       IF.
C                                       Check defaults - find IF in CAT.
      CALL AXEFND (4, CHIF, KICTPN, CATH(KHCTP), IFOFF, JERR)
C                                       Check IF.
C                                       See if null spec. = all
      IF ((IFB.LE.0) .AND. (JERR.EQ.0)) IFE = CATBLK(KINAX+IFOFF)
      IFB = MAX (IFB, 1)
      IFE = MAX (IFE, 1)
C                                       Axis exists - check
      IF (JERR.EQ.0) THEN
         IF (IFB.GT.CATBLK(KINAX+IFOFF)) GO TO 960
         IF (IFE.GT.CATBLK(KINAX+IFOFF)) GO TO 960
         IF (IFE.LT.IFB) GO TO 960
         END IF
C                                       Find IF increment.
      JNCIF = INCIF / MAX (1, CATBLK(KINAX))
      IF (JNCIF.LE.0) JNCIF = (LREC - NRPARM) /
     *   MAX (1, CATBLK(KINAX))
C                                       Set error return code.
      IERR = 2
C                                       Set visibility pointer
      IVPNT = 0
C                                       Branch by PMODE
      GO TO (10, 20, 30, 40, 10, 10, 10, 30, 80, 90, 100, 110, 80, 100,
     *   80, 150, 160, 170, 180, 150, 170, 150), PMODE
C                                       IPOL wanted, Check data type.
 10   CONTINUE
         IVPNT = IVPNT + 1
C                                       True IPOL, check.
         IF (KCOR0.GT.0) THEN
            IF (KCOR0.GT.1) GO TO 980
            JADR(1,IVPNT) = (1-KCOR0) * JNCS + 1
            JADR(2,IVPNT) = JADR(1,IVPNT)
            SELFAC(1,IVPNT) = 1.0
            SELFAC(2,IVPNT) = 0.0
C                                       XX, YY etc., check.
         ELSE IF (KCOR0.LE.-5) THEN
            IF (KCOR0.LT.-6) GO TO 980
            JADR(1,IVPNT) = 1
            JADR(2,IVPNT) = JADR(1,IVPNT) + JNCS
            SELFAC(1,IVPNT) = 0.5
            SELFAC(2,IVPNT) = 0.5
C                                       do it right??
            IF ((NCOR.EQ.4) .AND. (KCOR0.EQ.-5)) THEN
               TRXYQU = 1
               SELFAC(1,IVPNT) = -11.0
               SELFAC(2,IVPNT) = -11.0
C                                       Check if only XX or YY and
C                                       if so use it.
            ELSE IF ((NCOR.LT.2) .OR. (KCOR0.NE.-5)) THEN
               IF (FORMAL) GO TO 980
               JADR(2,IVPNT) = JADR(1,IVPNT)
               END IF
C                                       RR, LL etc., check.
         ELSE
            IF (KCOR0.LT.-2) GO TO 980
            JADR(1,IVPNT) = 1
            JADR(2,IVPNT) = JADR(1,IVPNT) + JNCS
            SELFAC(1,IVPNT) = 0.5
            SELFAC(2,IVPNT) = 0.5
C                                       Check if only RR or LL and
C                                       if so use it.
            IF ((NCOR.LT.2) .OR. (KCOR0.NE.-1)) THEN
               IF (FORMAL) GO TO 980
               JADR(2,IVPNT) = JADR(1,IVPNT)
               END IF
            END IF
C                                       What next?
         GO TO (900, 900, 900, 900, 30, 30, 20), PMODE
C                                       VPOL wanted.
 20   CONTINUE
         IVPNT = IVPNT + 1
C                                       True VPOL.
         IF (KCOR0.GE.0) THEN
            IF (NCOR+KCOR0.LE.4) GO TO 980
            JADR(1,IVPNT) = (4-KCOR0) * JNCS + 1
            JADR(2,IVPNT) = JADR(1,IVPNT)
            SELFAC(1,IVPNT) = 1.0
            SELFAC(2,IVPNT) = 0.0
C                                       XX, YY: don't know how
         ELSE IF (KCOR0.LE.-5) THEN
            IF ((KCOR0.LT.-5) .OR. (NCOR.LT.4)) GO TO 980
            JADR(1,IVPNT) = 1
            JADR(2,IVPNT) = JADR(1,IVPNT) + JNCS
            SELFAC(1,IVPNT) = -14.0
            SELFAC(2,IVPNT) = -14.0
            TRXYQU = 1
C                                       RR, LL
         ELSE
            IF ((NCOR.LT.2) .OR. (KCOR0.LT.-1)) GO TO 980
            JADR(1,IVPNT) = (1+KCOR0) * JNCS + 1
            JADR(2,IVPNT) = JADR(1,IVPNT) + JNCS
            SELFAC(1,IVPNT) = 0.5
            SELFAC(2,IVPNT) = -0.5
            END IF
C                                       V always last
         GO TO 900
C                                       Q - Check data type.
 30   CONTINUE
         IVPNT = IVPNT + 1
C                                       True stokes. Check.
         IF (KCOR0.GE.0) THEN
            IF((NCOR+KCOR0.LE.2) .OR. (KCOR0.GT.2)) GO TO 980
            JADR(1,IVPNT) = (2-KCOR0) * JNCS + 1
            JADR(2,IVPNT) = JADR(1,IVPNT)
            SELFAC(1,IVPNT) = 1.0
            SELFAC(2,IVPNT) = 0.0
C                                       XX, YY: don't know how
         ELSE IF (KCOR0.LE.-5) THEN
            IF ((KCOR0.LT.-5) .OR. (NCOR.LT.4)) GO TO 980
            JADR(1,IVPNT) = (7+KCOR0) * JNCS + 1
            JADR(2,IVPNT) = JADR(1,IVPNT) + JNCS
            SELFAC(1,IVPNT) = -12.0
            SELFAC(2,IVPNT) = -12.0
            TRXYQU = 1
C                                       RL, LR - Check.
         ELSE
            IF ((KCOR0-NCOR.GE.-4) .OR. (KCOR0.LT.-3)) GO TO 980
            JADR(1,IVPNT) = (3+KCOR0) * JNCS + 1
            JADR(2,IVPNT) = JADR(1,IVPNT) + JNCS
            SELFAC(1,IVPNT) = 0.5
            SELFAC(2,IVPNT) = 0.5
            END IF
C                                       What next?
         IF ((PMODE.NE.5) .AND. (PMODE.NE.6) .AND. (PMODE.NE.8))
     *      GO TO 900
C                                       U - Check data type.
 40   CONTINUE
         IVPNT = IVPNT + 1
C                                       True stokes. Check.
         IF (KCOR0.GE.0) THEN
            IF((NCOR+KCOR0.LE.3) .OR. (KCOR0.GT.3)) GO TO 980
            JADR(1,IVPNT) = (3-KCOR0) * JNCS + 1
            JADR(2,IVPNT) = JADR(1,IVPNT)
            SELFAC(1,IVPNT) = 1.0
            SELFAC(2,IVPNT) = 0.0
C                                       XX, YY: don't know how
         ELSE IF (KCOR0.LE.-5) THEN
            IF ((KCOR0.LT.-5) .OR. (NCOR.LT.4)) GO TO 980
            JADR(1,IVPNT) = (7+KCOR0) * JNCS + 1
            JADR(2,IVPNT) = JADR(1,IVPNT) + JNCS
C                                       Make JADR(1,IVPNT) negative
C                                       to tell DGGET to multiply
C                                       by i (= SQRT (-1))
            JADR(1,IVPNT) = -JADR(1,IVPNT)
            SELFAC(1,IVPNT) = -13.0
            SELFAC(2,IVPNT) = -13.0
            TRXYQU = 1
C                                       RL, LR - Check.
         ELSE
            IF ((KCOR0-NCOR.GE.-4) .OR. (KCOR0.LT.-3)) GO TO 980
            JADR(1,IVPNT) = (3+KCOR0) * JNCS + 1
            JADR(2,IVPNT) = JADR(1,IVPNT) + JNCS
C                                       Make JADR(1,IVPNT) negative
C                                       to tell DGGET to multiply
C                                       by i (= SQRT (-1))
            JADR(1,IVPNT) = -JADR(1,IVPNT)
            SELFAC(1,IVPNT) = -0.5
            SELFAC(2,IVPNT) = 0.5
            END IF
C                                       What next?
         IF (PMODE.EQ.6) GO TO 20
         GO TO 900
C                                       RR, check type.
 80   CONTINUE
         IVPNT = IVPNT + 1
C                                       True stokes - check.
         IF (KCOR0.GT.0) THEN
            IF ((KCOR0.GT.1) .OR. (NCOR.LT.4)) GO TO 980
            JADR(1,IVPNT) = (1-KCOR0) * JNCS + 1
            JADR(2,IVPNT) = (4-KCOR0) * JNCS + 1
            SELFAC(1,IVPNT) = 1.0
            SELFAC(2,IVPNT) = 1.0
C                                       XX, YY: don't know how
         ELSE IF (KCOR0.LE.-5) THEN
            GO TO 980
C                                       RR
         ELSE
            IF (KCOR0.LT.-1) GO TO 980
            JADR(1,IVPNT) = 1
            JADR(2,IVPNT) = JADR(1,IVPNT)
            SELFAC(1,IVPNT) = 1.0
            SELFAC(2,IVPNT) = 0.0
            END IF
C                                       What next
         IF ((PMODE.NE.13) .AND. (PMODE.NE.15)) GO TO 900
C                                       LL, check type.
 90   CONTINUE
         IVPNT = IVPNT + 1
C                                       True stokes - check.
         IF (KCOR0.GT.0) THEN
            IF ((KCOR0.GT.1) .OR. (NCOR.LT.4)) GO TO 980
            JADR(1,IVPNT) = (1-KCOR0) * JNCS + 1
            JADR(2,IVPNT) = (4-KCOR0) * JNCS + 1
            SELFAC(1,IVPNT) = 1.0
            SELFAC(2,IVPNT) = -1.0
C                                       XX, YY: don't know how
         ELSE IF (KCOR0.LE.-5) THEN
            GO TO 980
C                                       LL - check.
         ELSE
            IF ((KCOR0.LT.-2) .OR. (KCOR0-NCOR.GE.-2)) GO TO 980
            JADR(1,IVPNT) = (2+KCOR0) * JNCS + 1
            JADR(2,IVPNT) = JADR(1,IVPNT)
            SELFAC(1,IVPNT) = 1.0
            SELFAC(2,IVPNT) = 0.0
            END IF
C                                       What next
         IF (PMODE.NE.15) GO TO 900
C                                       RL, check type.
 100  CONTINUE
         IVPNT = IVPNT + 1
C                                       True stokes
         IF (KCOR0.GT.0) THEN
            IF ((KCOR0.GT.2) .OR. (KCOR0+NCOR.LE.3)) GO TO 980
            JADR(1,IVPNT) = (2-KCOR0) * JNCS + 1
            JADR(2,IVPNT) = JADR(1,IVPNT) + JNCS
            SELFAC(1,IVPNT) = 1.0
            SELFAC(2,IVPNT) = 1.0
C                                       Q + iU code
            JADR(2,IVPNT) = -JADR(2,IVPNT)
C                                       XX, YY: don't know how
         ELSE IF (KCOR0.LE.-5) THEN
            GO TO 980
C                                       RL - check.
         ELSE
            IF ((KCOR0.LT.-3) .OR. (KCOR0-NCOR.GE.-3)) GO TO 980
            JADR(1,IVPNT) = (3+KCOR0) * JNCS + 1
            JADR(2,IVPNT) = JADR(1,IVPNT)
            SELFAC(1,IVPNT) = 1.0
            SELFAC(2,IVPNT) = 0.0
            END IF
C                                       What next?
         IF ((PMODE.NE.14) .AND. (PMODE.NE.15)) GO TO 900
C                                       LR, check type.
 110  CONTINUE
         IVPNT = IVPNT + 1
C                                       True stokes
         IF (KCOR0.GT.0) THEN
            IF ((KCOR0.GT.2) .OR. (KCOR0+NCOR.LE.3)) GO TO 980
            JADR(1,IVPNT) = (2-KCOR0) * JNCS + 1
            JADR(2,IVPNT) = JADR(1,IVPNT) + JNCS
            SELFAC(1,IVPNT) = 1.0
            SELFAC(2,IVPNT) = -1.0
C                                       Q + iU code
            JADR(2,IVPNT) = -JADR(2,IVPNT)
C                                       XX, YY: don't know how
         ELSE IF (KCOR0.LE.-5) THEN
            GO TO 980
C                                       LR - check.
         ELSE
            IF ((KCOR0.LT.-4) .OR. (KCOR0-NCOR.GE.-4)) GO TO 980
            JADR(1,IVPNT) = (4+KCOR0) * JNCS + 1
            JADR(2,IVPNT) = JADR(1,IVPNT)
            SELFAC(1,IVPNT) = 1.0
            SELFAC(2,IVPNT) = 0.0
            END IF
         GO TO 900
C                                       XX, check type.
 150  CONTINUE
         IVPNT = IVPNT + 1
C                                       True stokes: don't know how
         IF (KCOR0.GT.0) THEN
            GO TO 980
C                                       XX, YY: okay
         ELSE IF (KCOR0.LE.-5) THEN
            IF (KCOR0.LT.-5) GO TO 980
            JADR(1,IVPNT) = 1
            JADR(2,IVPNT) = JADR(1,IVPNT)
            SELFAC(1,IVPNT) = 1.0
            SELFAC(2,IVPNT) = 0.0
C                                       RR: dont know how
         ELSE
            GO TO 980
            END IF
C                                       What next
         IF ((PMODE.NE.20) .AND. (PMODE.NE.22)) GO TO 900
C                                       YY, check type.
 160  CONTINUE
         IVPNT = IVPNT + 1
C                                       True stokes: don't know how
         IF (KCOR0.GT.0) THEN
            GO TO 980
C                                       XX, YY- check
         ELSE IF (KCOR0.LE.-5) THEN
            IF ((KCOR0.LT.-6) .OR. (KCOR0-NCOR.GE.-6)) GO TO 980
            JADR(1,IVPNT) = (6+KCOR0) * JNCS + 1
            JADR(2,IVPNT) = JADR(1,IVPNT)
            SELFAC(1,IVPNT) = 1.0
            SELFAC(2,IVPNT) = 0.0
C                                       LL: don't know how
         ELSE
            GO TO 980
            END IF
C                                       What next
         IF (PMODE.NE.22) GO TO 900
C                                       XY, check type.
 170  CONTINUE
         IVPNT = IVPNT + 1
C                                       True stokes: dont know how
         IF (KCOR0.GT.0) THEN
            GO TO 980
C                                       XX, YY: do know
         ELSE IF (KCOR0.LE.-5) THEN
            IF ((KCOR0.LT.-7) .OR. (KCOR0-NCOR.GE.-7)) GO TO 980
            JADR(1,IVPNT) = (7+KCOR0) * JNCS + 1
            JADR(2,IVPNT) = JADR(1,IVPNT)
            SELFAC(1,IVPNT) = 1.0
            SELFAC(2,IVPNT) = 0.0
C                                       RL - dont know
         ELSE
            GO TO 980
            END IF
C                                       What next?
         IF ((PMODE.NE.21) .AND. (PMODE.NE.22)) GO TO 900
C                                       YX, check type.
 180  CONTINUE
         IVPNT = IVPNT + 1
C                                       True stokes: dont know how
         IF (KCOR0.GT.0) THEN
            GO TO 980
C                                       XX, YY: do know
         ELSE IF (KCOR0.LE.-5) THEN
            IF ((KCOR0.LT.-8) .OR. (KCOR0-NCOR.GE.-8)) GO TO 980
            JADR(1,IVPNT) = (8+KCOR0) * JNCS + 1
            JADR(2,IVPNT) = JADR(1,IVPNT)
            SELFAC(1,IVPNT) = 1.0
            SELFAC(2,IVPNT) = 0.0
C                                       LR - check: dont know
         ELSE
            GO TO 980
            END IF
         GO TO 900
C                                       Finished - set ALLWT
C                                       ALLWT .TRUE. when output made
C                                       from several input channels
C                                       (Except IPOL only)
 900  ALLWT = (((PMODE.GE.2) .AND. (PMODE.LE.8)) .AND. (KCOR0.LT.0))
     *   .OR. ((PMODE.GE.9) .AND. (KCOR0.GT.0))
      ALLWT = ALLWT .OR. FORMAL
C                                       Set number of visibilities
      MVIS(1) = IVPNT * (IFE - IFB + 1) * (ICE - ICB + 1)
      MVIS(2) = IVPNT
      MVIS(3) = IFB
      MVIS(4) = IFE
      MVIS(5) = ICB
      MVIS(6) = ICE
      MVIS(7) = JNCIF
      MVIS(8) = JNCF
      IF (MVIS(1).GT.0) IERR = 0
      GO TO 999
C                                       Data unavailable
C                                       IF requested.
 960  WRITE (MSGTXT,1900) IFB, IFE
      GO TO 990
C                                       Channels requested
 970  WRITE (MSGTXT,1901) ICB, ICE
      GO TO 990
C                                       Polarizarion requested
 980  WRITE (MSGTXT,1902) STOKES
C                                       Error
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1010 FORMAT ('DGINIT: UNRECOGNIZED STOKES PARAMETER = ',A)
 1900 FORMAT ('DGINIT: DATA FOR IFS ',I2,' TO ',I3,
     *   ' UNAVAILABLE IN UV FILE')
 1901 FORMAT ('DGINIT: DATA FOR CHANNELS ',I2,' TO ',I3,
     *   ' UNAVAILABLE IN UV FILE')
 1902 FORMAT ('DGINIT: DATA FOR STOKES ',A,' UNAVAILABLE IN UV FILE')
      END
