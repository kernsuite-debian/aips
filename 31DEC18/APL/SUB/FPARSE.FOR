      SUBROUTINE FPARSE (ICARD, FITBLK, PSCAL, POFF, PTYPES, TABLES,
     *   END, GROUP, BITPIX, BSC, BZE, NAXIS, NPV, UVTABL, USED, IERR)
C-----------------------------------------------------------------------
C! interprets card image from FITS header into AIPS header format
C# FITS Parsing
C-----------------------------------------------------------------------
C;  Copyright (C) 1995, 1997-1999, 2001, 2007, 2010
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
C   FPARSE (parse FITS card) will unpack and interpret a card image
C   from a block of FITS data and put that data into the internal AIPS
C   header.  Corrects for dummy 1st axis in Groups extension.
C   Inputs:
C      ICARD   I         The card number (1-36) in block to interpret.
C      FITBLK  C*2880    A block of FITS header data.
C   In/out:
C      PSCAL   D(20)     Random parameter scalings
C      POFF    D(20)     Random parameter offsets
C      PTYPES  C*8(20)   Random parameter types
C      TABLES  I         # Tables extension
C      END     L         True if end card found, else false.
C      GROUP   I         Set to 0 or 1 as NAXIS1 not= or = 0.
C                        Checked if GROUPS keyword found later.
C      BITPIX  I         Number bits/pixel on the tape
C      BSC     D(2)      Scaling factor: (1) tape, (2) history
C      BZE     D(2)      Scaling offset: (1) tape, (2) history
C      NAXIS   I         # of axes
C      NPV     I(4)      IPIECE, NPIECE, FIRSTVIS, LASTVIS from
C                        piecewise parts of uv-table outputs
C      UVTABL  L         Special uv-table structure found
C      USED    I(*)      > 0 => read keyword already
C   Output:
C      IERR    I         error code 0=ok. 1=error.
C   COMMON /MAPHDR/
C   COMMON /FITINF/
C-----------------------------------------------------------------------
      INTEGER   ICARD, TABLES, GROUP, BITPIX, NAXIS, NPV(4), USED(*),
     *   IERR
      CHARACTER PTYPES(20)*8, FITBLK*2880
      DOUBLE PRECISION PSCAL(20), POFF(20), BSC(2), BZE(2)
      LOGICAL   END, UVTABL
C
      DOUBLE PRECISION X, XMX, XMN
      REAL      VAL
      CHARACTER KL*80, SYMBOL*8, STR*80
      INTEGER   KPNTR(58), PNTR, IPOFF, TABNO, NPNT, KT, IL, IVAL,
     *   NCHAR, NBYT, NN, NNST, IT, NPNTS
      LOGICAL   ISHIST, LHIST, FIRST
      INCLUDE 'INCS:DFUV.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DCAT.INC'
      EQUIVALENCE (KPNTR(1), KHOBJ)
      INCLUDE 'INCS:VFUV.INC'
C-----------------------------------------------------------------------
      XMX = 2.0D0**(NBITWD-1) - 1.0D0
      XMN = -XMX - 1.0D0
C                                       Find next symbol on the card
C                                       and look for it in the table.
      NPNT = 1
      FIRST = .TRUE.
      NN = NKT + NCT
      NNST = 1
 10   CALL GETCRD (ICARD, NN, NNST, AWORD, FITBLK, NPNT, KL, SYMBOL,
     *    TABNO, LHIST, END, IERR)
      IF (END) GO TO 999
      IF (FIRST) ISHIST = LHIST
      FIRST = .FALSE.
      IF (ISHIST) THEN
         IF (IERR.EQ.1) GO TO 10
         IF ((USED(TABNO).GT.0) .AND. (IERR.EQ.0)) GO TO 10
         END IF
      IF (IERR.NE.0) GO TO 999
      IF (.NOT.ISHIST) USED(TABNO) = USED(TABNO) + 1
C                                       Header pointer for this
C                                       keyword, number bytes and
C                                       offset position from pointer
      PNTR = APOINT(TABNO)
      PNTR = MOD (PNTR, 1000)
      IPOFF = PNTR / 100
      PNTR = MOD (PNTR, 100)
      IF (PNTR.GT.0) PNTR = KPNTR(PNTR)
      NBYT = APOINT(TABNO) / 1000
C                                       Readjust axis pointers for grp
      IF ((GROUP.EQ.1) .AND. (TABNO.GE.21) .AND. (TABNO.LE.60))
     *   IPOFF = IPOFF - 1
      IF ((GROUP.EQ.1) .AND. (TABNO.GE.4) .AND. (TABNO.LE.11))
     *   IPOFF = IPOFF - 1
C                                       Type value of keyword
C                                       1=LOGICAL
C                                       2=NUMBER
C                                       3=STRING
      KT = ATYPE(TABNO)
C                                       Logical value
      NPNTS = NPNT
      IF (KT.EQ.1) THEN
         CALL GETLG (KL, 80, NPNT, IL)
C                                       Illegal logical value.
         IF (IL.LT.0) THEN
            WRITE (MSGTXT,1100) SYMBOL
            GO TO 990
            END IF
C                                       Check for logical value
C                                       special cases.
         IF (AWORD(TABNO).EQ.'GROUPS') THEN
            IF (GROUP.NE.IL) THEN
               WRITE (MSGTXT,1110)
               GO TO 990
               END IF
C                                       Handle normal logical cases.
         ELSE
            CATBLK(PNTR+IPOFF) = IL
            END IF
C                                       Number
      ELSE IF (KT.EQ.2) THEN
         CALL GETNUM (KL, 80, NPNT, X)
C                                       special parse for EQUINOX
         IF (X.EQ.DBLANK) THEN
            IF ((AWORD(TABNO).EQ.'EQUINOX') .OR.
     *         (AWORD(TABNO).EQ.'EPOCH')) THEN
               NPNT = NPNTS
               CALL GETSTR (KL, 80, 68, NPNT, STR, NCHAR)
               IF (INDEX(STR,'1950').GT.0) THEN
                  X = 1950.0D0
               ELSE IF (INDEX(STR,'2000').GT.0) THEN
                  X = 2000.0D0
                  END IF
               END IF
            END IF
         IF (X.EQ.DBLANK) THEN
            WRITE (MSGTXT,1120) SYMBOL
            GO TO 990
            END IF
C                                       Blank pixel value.
         IF (AWORD(TABNO).EQ.'BLANK') THEN
            CATR(KRBLK) = FBLANK
C                                       Scaling factors and offsets for
C                                       random parms (used in FITDAT).
         ELSE IF (AWORD(TABNO)(:5).EQ.'PSCAL') THEN
            CALL GETI (AWORD(TABNO), IT)
            PSCAL(IT) = X
         ELSE IF (AWORD(TABNO)(:5).EQ.'TSCAL') THEN
            CALL GETI (AWORD(TABNO), IT)
            PSCAL(IT) = X
         ELSE IF (AWORD(TABNO)(:5).EQ.'PZERO') THEN
            CALL GETI (AWORD(TABNO), IT)
            POFF(IT) = X
         ELSE IF (AWORD(TABNO)(:5).EQ.'TZERO') THEN
            CALL GETI (AWORD(TABNO), IT)
            POFF(IT) = X
C                                       Handle normal cases. Put value
C                                       into proper header slot.
C                                       4-byte integer
         ELSE IF (NBYT.EQ.2) THEN
            IF ((X.LT.XMN) .OR. (X.GT.XMX)) THEN
               WRITE (MSGTXT,1120) SYMBOL
               GO TO 990
               END IF
            IVAL = X + SIGN (0.5D0, X)
            IF (PNTR.GT.0) CATBLK(PNTR+IPOFF) = IVAL
            IF (AWORD(TABNO).EQ.'TABLES') TABLES = IVAL
            IF (AWORD(TABNO).EQ.'BITPIX') BITPIX = IVAL
            IF (AWORD(TABNO)(1:5).EQ.'NAXIS') NAXIS = IVAL
            IF (AWORD(TABNO).EQ.'IPIECE  ') THEN
               NPV(1) = IVAL
            ELSE IF (AWORD(TABNO).EQ.'NPIECE  ') THEN
               NPV(2) = IVAL
            ELSE IF (AWORD(TABNO).EQ.'FIRSTVIS') THEN
               NPV(3) = IVAL
            ELSE IF (AWORD(TABNO).EQ.'LASTVIS ') THEN
               NPV(4) = IVAL
               END IF
C                                       Set GROUP on NAXIS1 or NAXIS = 0
            IF ((AWORD(TABNO).EQ.'NAXIS ') .AND. (NAXIS.EQ.0)) GROUP = 1
            IF (AWORD(TABNO).EQ.'NAXIS1') THEN
               GROUP = 0
               IF (IVAL.LE.0) THEN
                  GROUP = 1
                  CATBLK(KIDIM) = CATBLK(KIDIM) - 1
                  END IF
               END IF
            IF ((AWORD(TABNO).EQ.'NAXIS2') .AND. (.NOT.ISHIST)) THEN
               UVTABL = (CATBLK(KIDIM).EQ.2) .AND.
     *            (CATBLK(KINAX+1).EQ.0) .AND.
     *            (CATBLK(KINAX)/100.EQ.7777777)
               END IF
C                                       4-byte real
         ELSE IF (NBYT.EQ.4) THEN
            VAL = X
            IF (PNTR.GT.0) CATR(PNTR+IPOFF) = VAL

C                                       8-byte real
         ELSE IF (NBYT.EQ.8) THEN
            IF (PNTR.GT.0) CATD(PNTR+IPOFF) = X
C                                       Scaling parms
            IF (AWORD(TABNO).EQ.'BSCALE') BSC(1) = X
            IF (AWORD(TABNO).EQ.'ISCALE') BSC(2) = X
            IF (AWORD(TABNO).EQ.'BZERO') BZE(1) = X
            IF (AWORD(TABNO).EQ.'IZERO') BZE(2) = X
            END IF
C                                       String
      ELSE IF (KT.EQ.3) THEN
         CALL GETSTR (KL, 80, 68, NPNT, STR, NCHAR)
C                                       Dates are special
         IF (AWORD(TABNO)(:4).EQ.'DATE') THEN
            CALL DATFST ('F2L', STR)
            NCHAR = 8
            END IF
         NCHAR = MIN (NBYT, NCHAR)
         STR(NCHAR+1:) = ' '
C                                       IMCLASS
         IF (AWORD(TABNO).EQ.'IMCLASS') THEN
            IPOFF = NBYT * IPOFF + 1
            CALL CHR2H (NBYT, STR, IPOFF, CATH(PNTR))
         ELSE
            IPOFF = ((NBYT+3)/4) * IPOFF
C                                       Start string on integer boundary
            IF (AWORD(TABNO).EQ.'SORTORDR') THEN
               CALL CHR2H (NBYT, STR, 1, CATBLK(PNTR+IPOFF))
C                                       Random parameter type
            ELSE IF ((AWORD(TABNO)(:5).EQ.'PTYPE') .OR.
     *         (AWORD(TABNO)(:5).EQ.'TTYPE')) THEN
               CALL GETI (AWORD(TABNO), IT)
               PTYPES(IT) = STR(1:NCHAR)
C                                       Start string on real boundary.
            ELSE
               CALL CHR2H (NBYT, STR, 1, CATH(PNTR+IPOFF))
               END IF
            END IF
         END IF
C                                       Loop on History cards
      IF (ISHIST) GO TO 10
      GO TO 999
C                                       Error message
 990  CALL MSGWRT (7)
      IF (.NOT.ISHIST) IERR = 1
C
 999  RETURN
C-----------------------------------------------------------------------
 1100 FORMAT (A8,' LOGICAL VARIABLE HAS ILLEGAL VALUE')
 1110 FORMAT ('NAXIS AND GROUP STRUCTURE DO NOT MATCH')
 1120 FORMAT (A8,' HAS ILLEGAL VALUE')
      END
