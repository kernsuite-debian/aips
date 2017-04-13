      SUBROUTINE GRDAT (DOXY, IFIELD, CATR, BUFF1, IRET)
C-----------------------------------------------------------------------
C! Getn info about CLEAN components for GRDSUB.
C# EXT-appl Modeling
C-----------------------------------------------------------------------
C;  Copyright (C) 1995, 1997, 1999, 2005-2006, 2008, 2011-2012
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
C   GRDAT gets information about a CLEAN components file for GRDSUB.
C   Checks to see if data in XY order.
C   Input:
C      DOXY     L        If true then check sort order ('X*')
C      IFIELD   I        Current field number.  <0 => beam -IFIELD
C      CATR     R(256)   The uv data file catalog header record.
C   Input from commons:
C      RA       D        RA of uv data. (deg.)
C      DEC      D        Declination of uv data.
C      DO3DIM   L        T => images produced by reprojecting u,v,w
C   Output:
C      BUFF1    R(256)   Working buffer.
C      IRET     I        Return error code, 0=>OK, otherwise failed.
C   Output in common /MAPDES/
C      FLDSZ    I(2,*)   Size of the CLEAN map field.
C      NXUNF    I        Dimension (cells) of the map in RA to be used
C                        to determine uniform wt. counting box if 0
C                        when called.
C      NYUNF    I        Dimensions (cells) of the map in Dec to be
C                        used to determine uniform wt. counting box if
C                        0 when called.
C      CELLSG   R(2)     Cellsize of the CLEAN maps (asec.)
C      SCLUG    R(*)     Scaling to cells in u coordinate.
C      SCLVG    R(*)     Scaling to cells in v coordinate.
C      SCLWG    R(*)     Scaling to cells in w coordinate.
C      SCLUM    R        Scaling to cells in u for NXUNF
C      SCLVM    R        Scaling to cells in v for NYUNF
C      DXCG     R(*)     Position offset parameter in x (RA) corrected
C                        to uv data set rotation
C      DYCG     R(*)     Position offset parameter in y (dec) corrected
C                        to uv data set rotation
C      DZCG     R(*)     Position offset parameter in z.
C      XSHIFT   R(*)     X shift arc sec
C      YSHIFT   R(*)     Y shift in arc sec
C      XPOFF    R(*)     Pixel offset in x for clean components to
C                        field phase center from tang. point. (deg.)
C      YPOFF    R(*)     Pixel offset in Y for clean components to
C                        field phase center from tang. point. (deg.)
C      XFLD     R(*)     Field of view in X (RA) in seconds.
C      YFLD     R(*)     Field of view in Y (dec) in seconds.
C      CCROT    R        Cosine of position angle difference between
C                        orientation of uv data and CLEAN image times
C                        constant for coordinate conversion.
C      SSROT    R        Sine of position angle difference between
C                        orientation of uv data and CLEAN image.
C      OSFX     R        Over sampling factors in X
C      OSFY     R        Over sampling factors in Y
C      ICNTRX   I(*)     Center cell number in X
C      ICNTRY   I(*)     Center cell number in Y
C      KLNBLK   I(256)   Catalog header record of current CLEAN field.
C-----------------------------------------------------------------------
      LOGICAL   DOXY
      INTEGER   IFIELD, IRET
      REAL      CATR(256), BUFF1(*)
C
      CHARACTER CHTEMP*8, CHTYPE*2
      HOLLERITH KLNH(256)
      INTEGER   NX, NY, JFIELD, KVOL, KCNO, IRAOFF, IDECOF, IERR, NCHK,
     *   INDEX, LFIELD, II
      LOGICAL   DOUBX, DOUBY, ISUV
      REAL      KLNR(256)
      DOUBLE PRECISION KLND(128), KLNRA, KLNDEC, RAX, DECX
      INCLUDE 'INCS:PSTD.INC'
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'INCS:DGDS.INC'
      INCLUDE 'INCS:DMPR.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DDCH.INC'
      EQUIVALENCE (KLNBLK, KLNR, KLND, KLNH)
C-----------------------------------------------------------------------
C                                       If beam do field 1
      JFIELD = IFIELD
      IF (IFIELD.LE.0) THEN
         JFIELD = 1
         LFIELD = MAX (1, -IFIELD)
         MSGTXT = 'GRDAT: beam defines image - probably should not'
         CALL MSGWRT (6)
      ELSE
         JFIELD = IFIELD
         LFIELD = 1
         END IF
C                                       Check sort order in /UVHDR/.
C                                       Wrong sort order.
      IF ((ISORT(1:1).NE.'X') .AND. (DOXY)) THEN
         IRET = 9
         WRITE (MSGTXT,1000) ISORT, 'X*'
         GO TO 990
         END IF
      IF (IFIELD.LE.0) THEN
         KVOL = BEMVOL(LFIELD)
         KCNO = CNOBEM(LFIELD)
      ELSE
         KVOL = CCDISK(JFIELD)
         KCNO = CCCNO(JFIELD)
         END IF
      CALL CATIO ('READ', KVOL, KCNO, KLNBLK, 'REST', BUFF1, IRET)
      IF ((IRET.NE.0) .AND. (IRET.LE.4)) THEN
         WRITE (MSGTXT,1010) IRET
         GO TO 990
         END IF
      CALL H2CHR (2, KHPTYO, KLNH(KHPTY), CHTYPE)
      ISUV = CHTYPE.EQ.'UV'
C                                       Get field information
C                                       Drop hopefully obsolete fix
C     CALL FXSHFT (KLNBLK, IERR)
C                                       Field size
      IF (ISUV) THEN
         FLDSZ(1,JFIELD) = 24
         FLDSZ(2,JFIELD) = 24
      ELSE
         FLDSZ(1,JFIELD) = MAX (FLDSZ(1,JFIELD), KLNBLK(KINAX))
         FLDSZ(2,JFIELD) = MAX (FLDSZ(2,JFIELD), KLNBLK(KINAX+1))
         END IF
      CALL POWER2 (FLDSZ(1,JFIELD), II)
      IF (II.LT.FLDSZ(1,JFIELD)) FLDSZ(1,JFIELD) = II * 2
      CALL POWER2 (FLDSZ(2,JFIELD), II)
      IF (II.LT.FLDSZ(2,JFIELD)) FLDSZ(2,JFIELD) = II * 2
C                                       Uniform weighting size.
      IF (NXUNF.LE.0) NXUNF = FLDSZ(1,JFIELD)
      IF (NYUNF.LE.0) NYUNF = FLDSZ(2,JFIELD)
C                                       Cellsize
      IF (ISUV) THEN
         CELLSG(1) = 120.0
         CELLSG(2) = 120.0
      ELSE
         CELLSG(1) = KLNR(KRCIC) * 3.6E3
         CELLSG(2) = KLNR(KRCIC+1) * 3.6E3
         END IF
C                                       Over sampling factor.
      OSFX = 1.0
      OSFY = 1.0
C                                       Double if possible
      DOUBX = FLDSZ(1,JFIELD).LE.8192
      DOUBY = FLDSZ(2,JFIELD).LE.8192
      IF (DOUBX) OSFX = 2.0
      IF (DOUBY) OSFY = 2.0
      NX = FLDSZ(1,JFIELD)
      NY = FLDSZ(2,JFIELD)
C                                       Field of view
      XFLD(JFIELD) = ABS (CELLSG(1)) * NX
      YFLD(JFIELD) = ABS (CELLSG(2)) * NY
C                                       Set map center.
      ICNTRX(JFIELD) = NX / 2
      ICNTRY(JFIELD) = NY / 2 + 1
C                                       u,v,w scaling
      SCLUG(JFIELD) = NX * ABS (CELLSG(1)) * DG2RAD / 3600.0D0
      SCLVG(JFIELD) = -NY * CELLSG(2) * DG2RAD / 3600.0D0
      SCLWG(JFIELD) = 1.0
      SCLUM = NXUNF * ABS (CELLSG(1)) * DG2RAD / 3600.0D0
      SCLVM = -NYUNF * CELLSG(2) * DG2RAD / 3600.0D0
C                                       Find "CLEAN" RA and Dec axes.
      INDEX = JLOCR * 2
      CALL H2CHR (8, 1, CATR(KHCTP+INDEX), CHTEMP)
      IRAOFF = 0
      NCHK = 4
      IF (CHTEMP(1:4).EQ.'RA  ') NCHK = 2
      CALL AXEFND (NCHK, CHTEMP, KICTPN, KLNH(KHCTP), IRAOFF, IERR)
      INDEX = JLOCD * 2
      CALL H2CHR (8, 1, CATR(KHCTP+INDEX), CHTEMP)
      IDECOF = 1
      NCHK = 4
      IF (CHTEMP(1:4).EQ.'DEC ') NCHK = 3
      CALL AXEFND (NCHK, CHTEMP, KICTPN, KLNH(KHCTP), IDECOF, IERR)
C                                       Get position offsets of phase
C                                       centers.
      IF ((DO3DIM) .OR. (ISUV)) THEN
         KLNRA = KLND(KDCRV+IRAOFF)
         KLNDEC = KLND(KDCRV+IDECOF)
         RAX = RA
         DECX = DEC
      ELSE
         KLNRA = KLND(KDCRV+IRAOFF) + KLNR(KRXSH)
         KLNDEC = KLND(KDCRV+IDECOF) + KLNR(KRYSH)
         RAX = RA + CATR(KRXSH)
         DECX = DEC + CATR(KRYSH)
         END IF
C                                       Prepare for shift and rotate
C                                       of model.
      CALL ROTFND (KLNR, MAPROT, IRET)
      CALL ROTFND (CATR, UVROT, IRET)
C                                       Field offsets.
      CALL SHFTXY (RAX, DECX, MAPROT, KLNRA, KLNDEC, XSHIFT(JFIELD),
     *   YSHIFT(JFIELD))
C                                       set shift terms for field cent.
C                                       -NCP projection
      IF (TYPUVD.EQ.1) THEN
         CALL SHINCP (RAX, DECX, MAPROT, KLNRA, KLNDEC, DXCG(JFIELD),
     *      DYCG(JFIELD), DZCG(JFIELD))
C                                       -SIN projection
      ELSE
         CALL SHISIN (RAX, DECX, MAPROT, KLNRA, KLNDEC, DXCG(JFIELD),
     *      DYCG(JFIELD), DZCG(JFIELD))
         END IF
C                                       Get reference pixel offsets from
C                                       tangent point
      IF (ISUV) THEN
         XPOFF(JFIELD) = 0.0
         YPOFF(JFIELD) = 0.0
      ELSE IF ((KLNBLK(KIITY).EQ.1) .OR. (KLNBLK(KIITY).EQ.2)) THEN
         XPOFF(JFIELD) = KLNR(KRXPO)
         YPOFF(JFIELD) = KLNR(KRYPO)
      ELSE
         XPOFF(JFIELD) = (KLNR(KRCRP+IRAOFF) - KLNBLK(KINAX+IRAOFF)/2)
     *      * KLNR(KRCIC+IRAOFF)
         YPOFF(JFIELD) = (KLNR(KRCRP+IDECOF) - KLNBLK(KINAX+IDECOF)/2 -
     *      1) * KLNR(KRCIC+IDECOF)
         END IF
C                                       Reset rotation for components
      CCROT =  COS (DG2RAD * (UVROT - MAPROT))
      SSROT =  SIN (DG2RAD * (UVROT - MAPROT))
      IRET = 0
      GO TO 999
C                                       Error
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('UV DATA IN WRONG SORT ORDER =''',A2,''' NOT ''',A2,'''')
 1010 FORMAT ('GRDAT: ERROR',I5,' READING CATBLK ')
      END
