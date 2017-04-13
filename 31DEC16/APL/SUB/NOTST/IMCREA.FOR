      SUBROUTINE IMCREA (IFIELD, CREA, DISKO, CNOSCO, CHINC, BUFFER,
     *   IRET)
C-----------------------------------------------------------------------
C! Fills catalog header for an image and optionally creates and catalogs
C# Map Header
C-----------------------------------------------------------------------
C;  Copyright (C) 1995, 1997, 1999-2000, 2005
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
C   IMCREA fills the catalog header for a file and if CREA is
C   TRUE it creates and catalogs the file.
C   Input:
C      IFIELD   I        Field number (points to values in common)
C                           < 0 => beam.-IFIELD
C      CREA     L        If TRUE create and catalog file, else just fill
C                        catalog header in CATBLK in common /MAPHDR/.
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
C      MNAME    C*12     Output image name
C      MCLASS   C*6      Output image class
C      MDISKN   I        Desired image file output disk
C      MSEQ     I        Desired image file output sequence no.
C      ICNTRX   I(*)     The center pixel in X for each field.  Optional
C                        but must be at least initialized to 0.  Set if
C                        input <= 0.
C      ICNTRY   I(*)     The center pixel in Y for each field.  Optional
C                        but must be at least initialized to 0.  Set if
C                        input <= 0.
C      UVROT    R        Set to rotation in UV header
C      MAPROT   R        Set to rotation in UV header
C   Output:
C      DISKO    I        Output file disk number.
C      CNOSCO   I        Output file catalog number.
C      BUFFER   I(256)   Work buffer
C      IRET     I        0  => OK, otherwise failed.
C   Output from commons: (Includes DGDS, DMPR, DUVH, DSEL)
C      CATBLK   I(256)   Image catalog header
C-----------------------------------------------------------------------
      INTEGER   IFIELD, DISKO, CNOSCO, IRET, CHINC
      LOGICAL   CREA, DOAVE
      REAL      BUFFER(256)
C
      CHARACTER CCLASS*6, MMCLAS*6, ISTOK(6)*1, LL*8, MM*8, CFREQ*8,
     *   STOKE*8, JYBM*8, LLN*8, MMN*8
      HOLLERITH CATH(256)
      INTEGER   IMODE, II, INCREM, LFIELD
      LOGICAL   DOCLAS
      REAL      CATR(256), CATSCR(256), XXP, YYP
      DOUBLE PRECISION CATD(128), CATSCD(64), STOK(7), XRA, XDEC
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
      IF (NCFILE.GE.FILIST) THEN
         MSGTXT = 'IMCREA: FILE LIST OVERFLOWS'
         CALL MSGWRT (8)
         IRET = 9
         GO TO 999
         END IF
C                                       Set requested CLASS.
      MMCLAS = MCLASS
C                                       Create output file
C                                       Form new catalog header.
      CALL COPY (256, SCRBLK, CATBLK)
C                                       Get uv data rotation.
      CALL ROTFND (SCRBLK, UVROT, IRET)
      MAPROT = UVROT
C                                       average or increment
      DOAVE = NCHAVG.GT.1
      IF (DOAVE) THEN
         INCREM = NCHAVG
      ELSE
         INCREM = CHINC
         ENDIF
C                                       Beam.
      IF (IFIELD.LT.1) THEN
         LFIELD = MAX (1, -IFIELD)
         CATR(KRCRP) = NXBEM(LFIELD) / 2
         CATR(KRCRP+1) = NYBEM(LFIELD) / 2 + 1
         CATR(KRXSH) = 0.0
         CATR(KRYSH) = 0.0
         CATBLK(KINAX) = NXBEM(LFIELD)
         CATBLK(KINAX+1) = NYBEM(LFIELD)
C                                       Map.
      ELSE
         LFIELD = IFIELD
C                                       Set center pixel if necessary.
         IF (ICNTRX(IFIELD).LE.0) ICNTRX(IFIELD) = FLDSZ(1,IFIELD)
     *      / 2
         IF (ICNTRY(IFIELD).LE.0) ICNTRY(IFIELD) = FLDSZ(2,IFIELD)
     *      / 2 + 1
         CALL XYSHFT (RA, DEC, XSHIFT(IFIELD), YSHIFT(IFIELD), MAPROT,
     *      XRA, XDEC)
         CATR(KRXSH) = XRA - RA
         CATR(KRYSH) = XDEC - DEC
C                                       3D imaging
         IF (DO3DIM) THEN
            CATR(KRCRP) = ICNTRX(IFIELD)
            CATR(KRCRP+1) = ICNTRY(IFIELD)
            CATD(KDCRV) = XRA
            CATD(KDCRV+1) = XDEC
C                                       Bits of single tangent plane
         ELSE
            CALL SHFCRP (TYPUVD, RA, DEC, MAPROT, XRA, XDEC, XXP, YYP)
            CATR(KRCRP) = ICNTRX(IFIELD) + XXP / ABS (CELLSG(1))
            CATR(KRCRP+1) = FLDSZ(2,IFIELD) - ICNTRY(IFIELD) + 2 -
     *         YYP / ABS (CELLSG(2))
            CATD(KDCRV) = RA
            CATD(KDCRV+1) = DEC
            END IF
         CATBLK(KINAX) = FLDSZ(1,IFIELD)
         CATBLK(KINAX+1) = FLDSZ(2,IFIELD)
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
      IF (DOAVE) THEN
         CATD(KDCRV+2) = FREQ
      ELSE
         CATD(KDCRV+2) = CATSCD(KDCRV+2)
         END IF
C                                       Stokes' type.
      IMODE = 1
      DO 30 II = 1,6
         IF (STOKES(1:1).EQ.ISTOK(II)) IMODE = II
 30      CONTINUE
      CATD(KDCRV+3) = STOK(IMODE+1)
      IF (IFIELD.LE.0) CATD(KDCRV+3) = STOK(1)
      CATR(KRCIC) =  -ABS (CELLSG(1)) / 3.6D3
      CATR(KRCIC+1) = CELLSG(2) / 3.6D3
      CATR(KRCIC+2) = CATSCR(KRCIC+JLOCF) * INCREM
      CATR(KRCIC+3) = 1.0
C                                        Velocity Info
      IF (DOAVE) THEN
         CATR(KRCRP+2) = 1.0
         CATR(KRARP) = (CATSCR(KRARP) - (BCHAN+ECHAN)/2) / INCREM + 1.0
      ELSE
         CATR(KRCRP+2) = (CATSCR(KRCRP+JLOCF) - BCHAN) / INCREM + 1.0
         CATR(KRARP) = (CATSCR(KRARP) - BCHAN) / INCREM + 1.0
         END IF
      CATR(KRCRP+3) = 1.
      CATR(KRCRT+1) = MAPROT
      CATR(KRCRT+0) = 0.0
      CATR(KRCRT+2) = 0.
      CATR(KRCRT+3) = 0.
      CATR(KRDMX) = -1.0E20
      CATR(KRDMN) = 1.0E20
      CATR(KRBLK) = 0.
      CATBLK(KIGCN) = 0
      CATBLK(KIPCN) = 0
      CATBLK(KIDIM) = 4
      CATBLK(KINAX+2) = ((ECHAN - BCHAN) / INCREM) + 1
      CATBLK(KINAX+2) = MAX (CATBLK(KINAX+2), 1)
      CATBLK(KINAX+3) = 1
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
C                                       If .NOT.CREA quit here
      IF (CREA) THEN
C                                       Output name, class etc.
C                                       Set defaults.
         DOCLAS = MMCLAS.EQ.'      '
         IF (IFIELD.GT.0) THEN
            CCLASS = 'IIM'
         ELSE
            CCLASS = 'IBM'
            END IF

         CALL MAKOUT (UNAME, '      ', USEQ, CCLASS, MNAME, MMCLAS,
     *      MSEQ)
         CALL CHR2H (12, MNAME, KHIMNO, CATH(KHIMN))
C                                       Include field number if>1
         WRITE (MMCLAS(4:6),1030) LFIELD
C                                       Get right stokes type on class
         IF (DOCLAS) MMCLAS(1:1) = ISTOK(IMODE)(1:1)
         CALL CHR2H (6, MMCLAS, KHIMCO, CATH(KHIMC))
         CALL CHR2H (2, 'MA', KHPTYO, CATH(KHPTY))
         CATBLK(KIIMU) = NLUSER
C                                       Set sequence number.
         CATBLK(KIIMS) = MSEQ
C                                       Create output map file.
         DISKO = MDISKN
         CNOSCO = 1
         CALL MCREAT (DISKO, CNOSCO, BUFFER, IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1020) IRET
            GO TO 990
            END IF
C                                       Mark in /CFILE/
         NCFILE = NCFILE + 1
         FCNO(NCFILE) = CNOSCO
         FVOL(NCFILE) = DISKO
         FRW(NCFILE) = 2
C                                       Get values of MDISK, MSEQ
         MDISKN = DISKO
         MSEQ = CATBLK(KIIMS)
         END IF
      GO TO 999
C                                       error.
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1020 FORMAT ('ERROR',I3,' CREATING OUTPUT FILE')
 1030 FORMAT (I3.3)
      END
