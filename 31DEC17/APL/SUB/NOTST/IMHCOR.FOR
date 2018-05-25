      SUBROUTINE IMHCOR (IFIELD, CHINC)
C-----------------------------------------------------------------------
C! Corrects catalog header for imaging parameters.
C# Map Header
C-----------------------------------------------------------------------
C;  Copyright (C) 1995-1997, 1999, 2005
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
C   Corrects catalog header for imaging parameters.
C   Most input and output in common.
C   Input:
C      IFIELD   I        Field number, <0 => beam # -IFIELD
C      CHINC    I        Channel increment
C   Input from commons: (Includes DGDS, DMPR, DUVH, DSEL)
C      SCRBLK   I(256)   UV data catalog header.
C      NCHAVG   I        Number of channels to grid together.
C      BCHAN    I        First input channel number
C      ECHAN    I        Highest input channel number
C      FLDSZ    I(2,*)   Dimension of map in RA, Dec (cells)
C      NXBEM    I        X-Dimension (cells) of beam.
C      NYBEM    I        Y-Dimension (cells) of beam.
C      CELLSG   R(2)     The cell spacing in X and Y in arcseconds.
C      XSHIFT   R(16)    Shift in X (after rotation) in asec.
C                        in projected coordinates. 1 per field.
C      YSHIFT   R(16)    Shift in Y (after rotation) in asec.
C                        in projected coordinates. 1 per field.
C      STOKES   C*4      Stokes types wanted. 'I','Q','U','V','R','L'
C      FREQ     D        Average Frequency of output (Hz).
C   In/out from commons: (Includes DGDS, DMPR, DUVH, DSEL)
C      ICNTRX   I(*)     The center pixel in X for each field.  Optional
C                        but must be at least initialized to 0.  Set if
C                        input <= 0.
C      ICNTRY   I(*)     The center pixel in Y for each field.  Optional
C                        but must be at least initialized to 0.  Set if
C                        input <= 0.
C      CATBLK   I(256)   Image catalog header
C      UVROT    R        Set to rotation in UV header
C      MAPROT   R        Set to rotation in UV header
C-----------------------------------------------------------------------
      INTEGER   IFIELD, CHINC
C
      HOLLERITH CATH(256)
      INTEGER   II, IMODE, JERR, LFIELD
      REAL      CATR(256), CATSCR(256), MAPRR, XXP, YYP
      DOUBLE PRECISION CATD(128), CATSCD(64), STOK(7), XRA, XDEC
      CHARACTER ISTOK(6)*1, LL*8, MM*8, CFREQ*8, STOKE*8, JYBM*8, LLN*8,
     *   MMN*8
      INCLUDE 'INCS:PSTD.INC'
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DGDS.INC'
      INCLUDE 'INCS:DMPR.INC'
      INCLUDE 'INCS:DSEL.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DHDR.INC'
      EQUIVALENCE (CATBLK, CATH, CATR, CATD)
      EQUIVALENCE (SCRBLK, CATSCR, CATSCD)
      DATA ISTOK /'I','Q','U','V','R','L'/
      DATA    LL,         MM,       CFREQ,     STOKE,        JYBM
     *   /'RA---SIN', 'DEC--SIN', 'FREQ    ', 'STOKES  ',  'JY/BEAM '/
      DATA LLN, MMN /'RA---NCP','DEC--NCP'/
      DATA STOK /0.0D0,1.0D0,2.0D0,3.0D0,4.0D0,-1.0D0,-2.0D0/
C-----------------------------------------------------------------------
C                                       Get uv data rotation.
      CALL ROTFND (SCRBLK, UVROT, JERR)
      MAPROT = UVROT
      MAPRR = MAPROT * DG2RAD
C                                       Set for shift
      LFIELD = MAX (1, ABS(IFIELD))
      CALL XYSHFT (RA, DEC, XSHIFT(LFIELD), YSHIFT(LFIELD), MAPROT, XRA,
     *   XDEC)
      CALL SHFCRP (TYPUVD, RA, DEC, MAPROT, XRA, XDEC, XXP, YYP)
C                                       Beam.
      IF (IFIELD.LT.1) THEN
         CATR(KRCRP) = NXBEM(LFIELD) / 2
         CATR(KRCRP+1) = NYBEM(LFIELD) / 2 + 1
         IF (DO3DIM) THEN
            CATR(KRXSH) = XRA - RA
            CATR(KRYSH) = XDEC - DEC
            CATD(KDCRV) = XRA
            CATD(KDCRV+1) = XDEC
         ELSE
            CATR(KRXSH) = 0.0
            CATR(KRYSH) = 0.0
            CATD(KDCRV) = RA
            CATD(KDCRV+1) = DEC
            END IF
C                                       Map.
      ELSE
C                                       Set center pixel if necessary.
         IF (ICNTRX(LFIELD).LE.0) ICNTRX(LFIELD) = FLDSZ(1,LFIELD)
     *      / 2
         IF (ICNTRY(LFIELD).LE.0) ICNTRY(LFIELD) = FLDSZ(2,LFIELD)
     *      / 2 + 1
         CATR(KRXSH) = XRA - RA
         CATR(KRYSH) = XDEC - DEC
C                                       3D imaging
         IF (DO3DIM) THEN
            CATR(KRCRP) = ICNTRX(LFIELD)
            CATR(KRCRP+1) = ICNTRY(LFIELD)
            CATD(KDCRV) = XRA
            CATD(KDCRV+1) = XDEC
C                                       Bits of single tangent plane
         ELSE
            CATR(KRCRP) = ICNTRX(LFIELD) + XXP / ABS (CELLSG(1))
            CATR(KRCRP+1) = FLDSZ(2,LFIELD) - ICNTRY(LFIELD) + 2 -
     *         YYP / ABS (CELLSG(2))
            CATD(KDCRV) = RA
            CATD(KDCRV+1) = DEC
            END IF
         END IF
      CALL CHR2H (8, JYBM, 1, CATH(KHBUN))
      IF (TYPUVD.GE.0) THEN
         CALL CHR2H (8, LL, 1, CATH(KHCTP))
         CALL CHR2H (8, MM, 1, CATH(KHCTP+2))
      ELSE
         CALL CHR2H (8, LLN, 1, CATH(KHCTP))
         CALL CHR2H (8, MMN, 1, CATH(KHCTP+2))
         END IF
      CALL CHR2H (8, CFREQ, 1, CATH(KHCTP+2*2))
      CALL CHR2H (8, STOKE, 1, CATH(KHCTP+3*2))
      IMODE = 1
      DO 30 II = 1,6
         IF (STOKES(1:1).EQ.ISTOK(II)) IMODE = II
 30      CONTINUE
      CATD(KDCRV+3) = STOK(IMODE+1)
      IF (IFIELD.LE.0) CATD(KDCRV+3) = STOK(1)
      CATR(KRCIC) =  -ABS (CELLSG(1)) / 3.6D3
      CATR(KRCIC+1) = CELLSG(2) / 3.6D3
      IF (CATBLK(KINAX+2).LE.1) THEN
         CATR(KRCIC+2) = CATSCR(KRCIC+JLOCF) * MAX (1, NCHAVG)
      ELSE
         CATR(KRCIC+2) = CATSCR(KRCIC+JLOCF) * MAX (1, CHINC)
         END IF
      CATR(KRCIC+3) = 1.0
      CATR(KRCRP+3) = 1.
      CATR(KRCRT+1) = MAPROT
      CATR(KRCRT+0) = 0.0
      CATR(KRCRT+2) = 0.
      CATR(KRCRT+3) = 0.
      CATR(KRBLK) = 0.
C                                       imaging parameters
      IF (DO3DIM) THEN
         CATBLK(KIITY) = 2
         CATR(KRXPO) = 0.0
         CATR(KRYPO) = 0.0
      ELSE
         CATBLK(KIITY) = 1
         IF (IFIELD.LT.1) THEN
            CATR(KRXPO) = 0.0
            CATR(KRXPO) = 0.0
         ELSE
            CATR(KRXPO) = (CATR(KRCRP) - ICNTRX(IFIELD)) * CATR(KRCIC)
            CATR(KRYPO) = (CATR(KRCRP+1) - ICNTRY(IFIELD)) *
     *         CATR(KRCIC+1)
            END IF
         END IF
C
 999  RETURN
      END
