      SUBROUTINE GRDMEM (NFIELD, FIELDS, TAPERS, DISKI, CNOSCI, CATUVR,
     *   JBUFSZ, FREQID, BUFF1, GRID, NROWFT, SWTS, IRET)
C-----------------------------------------------------------------------
C! Grids a uv data set and leaves it in memory
C# AP-util UV
C-----------------------------------------------------------------------
C;  Copyright (C) 1995, 1997, 1999-2000, 2006-2009
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
C   Grids a uv data set; called by IMGMEM.  See IMGMEM for details.
C   Uses AIPS LUNs 18
C   Inputs:
C      NFIELD   I        Number of fields in FIELDS
C      FIELDS   I(*)     Field numbers to grid, if <0 then grid beam
C                        -IFIELD.  If < -1000000, then grid beam
C                        ABS(IFIELD)-1000000 but do not return BEMMAX
C      TAPERS   R(2,*)   U,V taper parameters for FIELDS
C      DISKI    I        Input file disk number for catalogd files,
C                        <=. 0 => /CFILES/ scratch file.
C      CNOSCI   I        Input file catalog slot number or /CFILES/
C                        scratch file number.
C      CATUVR   R(256)   UV data catalog header record.
C      JBUFSZ   I        Size in AIPS bytes of buffers. Dimension of
C                        BUFF1  must be at least 4096 words
C      FREQID   I        Freq ID number, if it exists.
C   Output:
C      BUFF1    R(*)     Working buffer
C      GRID     I(*)     "AP" base address of data grid: fields 1-NFIELD
C      NROWFT   I(*)     Number of "Y" rows from center contain data:
C                           fields 1-NFIELD
C      SWTS     R(*)     Sum of weights for all beams in 1-NFIELD
C      IRET     I        Return error code. 0=>OK, error otherwise.
C   Input/Output via common:
C      BEMMAX   R(*)     Sum of weights = normalization factor
C      GUARDB   R(2)     Fraction of UMAX and VMAX to blank on edge of
C                        field
C      GUAXAU,GUAXAV R   Max U, V in data set
C      GUAXBU,GUAXBV R   Max U, V used in grid
C-----------------------------------------------------------------------
      INTEGER   NFIELD, FIELDS(*), DISKI, CNOSCI, JBUFSZ, FREQID,
     *   GRID(*), NROWFT(*), IRET
      REAL      TAPERS(2,*), BUFF1(*), CATUVR(256), SWTS(*)
C
      INTEGER   MXIMAG
      PARAMETER (MXIMAG = 1000)
      CHARACTER NAME*48
      INTEGER   VO, BO, LLVIS, VOL, LUNUV, JJ, IFACT, INPTR, IERR, IU,
     *   JNPTR, NIO, NPOINT, NX2, BIND2, ILENBU, ICENX, ICENY, INDS2,
     *   NX, NY, JNDX, VSOFF, LVIS, WTOFF, IWOFF, NUMROW, NCHGRD, U,
     *   ROW, NUM, CY, CX, VIS,  WT, IDATA, UV, ITEMP, LLREC, NO2, M,
     *   LROW(MXIMAG),  MO2, CNT, MAXREC, APSIZE, APSMWT, II, INCNT,
     *   LIMIT, MCHGRD, IROW, APCONI(3), II0, LFIELD, SDAT,ISDAT,
     *   IFIELD, NEEDED
      LOGICAL   T, F, DOSHFT(MXIMAG), BEAM, ONEWT, DO3MUL
      REAL      APCONS(30,MXIMAG), TUC, TVC, XMAX, ZSCLU, ZSCLV, ZSCLW,
     *   XXFLD, YYFLD, DDXS(3), SUMWT, APXTMP, BLEN, BLMN2,
     *   BLMX2, UUMAX, VVMAX, DU, DV, DW, ZZSCLU, UUMAXG, VVMAXG, UIN,
     *   VIN, UMAT(3,3), PMAT(3,3), DUMAX, DVMAX, MAXROW
      DOUBLE PRECISION   WX, WY, FREQLO, FREQHI, XRA, XDEC, FFRAC
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DAPM.INC'
      INCLUDE 'INCS:DGDS.INC'
      INCLUDE 'INCS:DMPR.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:PSTD.INC'
      DATA T, F /.TRUE.,.FALSE./
      DATA VO, BO /0, 1/
      DATA LUNUV /18/
C-----------------------------------------------------------------------
      IRET = 0
      SUMWT = 0.0
      APSIZE = PSAPNW * 1024
      IF ((GUARDB(1).LT.0.0) .OR. (GUARDB(1).GT.0.9)) GUARDB(1) = 0.0
      IF ((GUARDB(2).LT.0.0) .OR. (GUARDB(2).GT.0.9)) GUARDB(2) = 0.0
      GUAXAU = 0.
      GUAXAV = 0.
      GUAXBU = 0.
      GUAXBV = 0.
      GUACNT = 0
      DUMAX = 0.0
      DVMAX = 0.0
C                                       Setup constants
      NCHGRD = NCHAVG
      MCHGRD = NCHAVG
      NO2 = MAX (YPARM(1) , 1.0) + 0.1
      M = MAX (XPARM(1) , 1.0) + 0.1
      M = M * 2 + 1
      MO2 = M / 2
      LLREC = LREC
C                                       IFACT is the number of REAL
C                                       accumulators in GRID per cell
      IFACT = 2
      NEEDED = MAX (20*LREC, JBUFSZ/2)
      NEEDED = ((NO2*2+1) * 100)  + (M * 100) + NCHAVG + 100 + NEEDED
      MAXROW = 0.0
C                                       Set freq. scaling factors
      FFRAC = (FREQG(CHUV1) / FREQUV) - 1.0D0
C                                       Loop over fields to set up
C                                       parameters
      DO 100 LFIELD = 1,NFIELD
         IFIELD = FIELDS(LFIELD)
         BEAM = IFIELD.LE.0
         IFIELD = MAX (1, ABS(IFIELD))
         IF (IFIELD.GT.1000000) IFIELD = IFIELD - 1000000

C                                       Get field info. if nec.
         IF (.NOT.NGRDAT) THEN
            CALL GRDAT (F, IFIELD, CATUVR, BUFF1, IRET)
            IF (IRET.NE.0) GO TO 999
            END IF
C                                       Set parameters for beam/map.
         DO3MUL = (DO3DIM) .AND. ((XSHIFT(IFIELD).NE.0.0) .OR.
     *      (YSHIFT(IFIELD).NE.0.0) .OR. (UVROT.NE.0.0) .OR.
     *      (MAPROT.NE.0.0))
         CALL XYSHFT (RA, DEC, XSHIFT(IFIELD), YSHIFT(IFIELD), MAPROT,
     *      XRA, XDEC)
         IF (DO3MUL) THEN
            CALL PRJMAT (RA, DEC, UVROT, XRA, XDEC, MAPROT, UMAT, PMAT)
         ELSE
            CALL P2DMAT (RA, DEC, UVROT, XRA, XDEC, MAPROT, UMAT, PMAT)
            END IF
C                                       Fill channel table.
         IF (BEAM) THEN
            IF (IFIELD.EQ.1) THEN
               CALL FRQTAB (DISKI, CNOSCI, LUNUV, CATUVR, FREQID, BUFF1,
     *            IRET)
               IF (IRET.NE.0) GO TO 999
               END IF
C                                       Beam
            NX = NXBEM(IFIELD)
            NY = NYBEM(IFIELD)
            ICENX = NX / 2
            ICENY = NY / 2 + 1
            XXFLD = ABS (CELLSG(1)) * NX
            YYFLD = ABS (CELLSG(2)) * NY
            ZSCLU = XXFLD / RAD2AS
C                                       Flip sign on v to make maps come
C                                       out upside down.
            ZSCLV = -YYFLD / RAD2AS
            ZSCLW = 1.0E-6
            DDXS(1) = 0.0
            DDXS(2) = 0.0
            DDXS(3) = 0.0
C                                       Map.
         ELSE
            NX = FLDSZ(1,IFIELD)
            NY = FLDSZ(2,IFIELD)
            ICENX = ICNTRX(IFIELD)
            ICENY = ICNTRY(IFIELD)
            ZSCLU = (NX * ABS (CELLSG(1))) / RAD2AS
C                                       Flip sign on v to make maps come
C                                       out upside down.
            ZSCLV =  - (NY * CELLSG(2)) / RAD2AS
            ZSCLW = 1.0
            XXFLD = XFLD(IFIELD)
            YYFLD = YFLD(IFIELD)
            DDXS(1) = -DXCG(IFIELD)
            DDXS(2) = -DYCG(IFIELD)
            DDXS(3) = -DZCG(IFIELD)
            CALL PRJMUL (2, DDXS, PMAT, DDXS)
            END IF
         DOSHFT(LFIELD) = (DDXS(1).NE.0.0) .OR. (DDXS(2).NE.0.0)
C                                       Set baseline limits
C                                       Determine some constants.
         DU = RAD2AS / (NX * ABS (CELLSG(1)))
         DV = RAD2AS / (NY * CELLSG(2))
         DUMAX = MAX (DU, DUMAX)
         DVMAX = MAX (DV, DVMAX)
         DW = 1.0
C-----------------------------------------------------------------------
C                                       Set various constants
C                                       Frequency scaling constants
         NX2 = NX / 2
C                                       Set miscellaneous AP values.
         LROW(LFIELD) = NY
         NUMROW = NX / 2 + 1 + M
C                                       Is there sufficient memory?
         IF (LFIELD.EQ.1) THEN
            GRID(1) = APSIZE - (NUMROW * IFACT) * NY - 11
         ELSE
            GRID(LFIELD) = GRID(LFIELD-1) - (NUMROW * IFACT) * NY - 1
            END IF
         IF (NEEDED.GT.GRID(LFIELD)) THEN
            IRET = 9
            WRITE (MSGTXT,1040) LFIELD, NFIELD
            GO TO 995
            END IF
C                                       Set AP pointers
C                                       Convert taper widths to
C                                       convenient constants.
         TUC = 0.0
         TVC = 0.0
         IF (TAPERS(1,LFIELD).GT.0.) TUC = LOG(.3) / (TAPERS(1,LFIELD) *
     *         1.E3 / 206265. * XXFLD) ** 2
         IF (TAPERS(2,LFIELD).GT.0.) TVC = LOG(.3) / (TAPERS(2,LFIELD) *
     *         1.E3 / 206265. * YYFLD) ** 2
C                                       Set AP gridding constants.
         WX = (-TWOPI * (ICENX-1-NX/2)) / NX
         WY = (-TWOPI * (ICENY-1-NY/2)) / NY
         APCONS(1,LFIELD) = COS ((NX / 2) * WX)
         APCONS(2,LFIELD) = SIN ((NX / 2) * WX)
         APCONS(3,LFIELD) = COS (WY)
         APCONS(4,LFIELD) = SIN (WY)
         APCONS(5,LFIELD) = COS (-WX)
         APCONS(6,LFIELD) = SIN (-WX)
         APCONS(7,LFIELD) = (NX/2 - MO2 - 0.5)
         APCONS(8,LFIELD) = (NY/2 - NO2 - 0.5)
         APCONS(9,LFIELD) = 0.0
         APCONS(10,LFIELD) = TUC
         APCONS(11,LFIELD) = TVC
         APCONS(12,LFIELD) = FFRAC*ZSCLU + ZSCLU
         APCONS(13,LFIELD) = FFRAC*ZSCLV + ZSCLV
         APCONS(14,LFIELD) = FFRAC*ZSCLW + ZSCLW
         APCONS(15,LFIELD) = DDXS(1) / ZSCLU
         APCONS(16,LFIELD) = DDXS(2) / ZSCLV
         APCONS(17,LFIELD) = DDXS(3) / ZSCLW
         NROWFT(LFIELD) = 0
         CALL RCOPY (9, UMAT, APCONS(21,LFIELD))
 100     CONTINUE
C                                       Non-field dependent inits
C                                       Set AP locations for conv. fn.
      CY = GRID(NFIELD) - 100 * (2 * NO2 + 1) - 1
      CX = CY - 100 * M - 1
C                                       Location of sum of weights for
C                                       beam.
      APSMWT = 30 + NCHAVG
C                                       Set AP data locations
      SDAT = APSMWT + 1
      UV = (CX - SDAT) / 2 + SDAT
C                                       Set AP data freq. increment.
      LVIS = INCF
      IF ((INCIF.GT.0) .AND. (INCIF.LT.INCF)) LVIS = INCIF
      LLVIS = LVIS
C                                       Find where weight is.
C                                       IWOFF + => Offset in rec.
C                                       IWOFF - => Offset in vis.
      IWOFF = 0
      CALL AXEFND (4, 'WEIG', KIPTPN, CATUVR(KHPTP), IWOFF, IERR)
C                                       If COMPLEX axis more then
C                                       2 long assume it has weight.
      ONEWT = (LVIS.LE.2)
      IF (.NOT.ONEWT) IWOFF = -2
C                                       Set constants.
C                                       To simplify the map output
C                                       routine, shift the map center
C                                       here in the gridding routine.
C                                       ICENX and ICENY define the
C                                       pixel number of the map center,
C                                       counted from the front of the
C                                       file.
C                                       Determine the maximum number
C                                       of visibility points which
C                                       fit in the AP.
      MAXREC = (CX - UV) / LREC - 5
C                                       Be sure MAXREC.GT.10
      IF (MAXREC.LE.10) THEN
         XMAX = - MAXREC * LREC
         IF (MAXREC.GT.0) XMAX = (11 - MAXREC) * LREC
         WRITE (MSGTXT,1041) XMAX
         IRET = 1
         GO TO 995
         END IF
C                                       Open files. Use UVINIT and
C                                       UVDISK to read the
C                                       visibility file,
C                                       APIO to read and write the
C                                       grid file.
      IF (DISKI.LE.0) THEN
         VOL = SCRVOL(CNOSCI)
         CALL ZPHFIL ('SC', VOL, SCRCNO(CNOSCI), 1, NAME, IRET)
      ELSE
         VOL = DISKI
         CALL ZPHFIL ('UV', VOL, CNOSCI, 1, NAME, IRET)
         END IF
      CALL ZOPEN (LUNUV, INDS2, VOL, NAME, T, F, T, IRET)
      IF (IRET.GT.0) THEN
         WRITE (MSGTXT,1050) IRET
         GO TO 995
         END IF
C-----------------------------------------------------------------------
C                                       Grid data.
C                                       Clear grid.
C                                       Set size of grid to clear
      NUM = APSIZE - GRID(NFIELD)
      CALL QVCLR (GRID(NFIELD), 1, NUM)
C                                       Offset VIS to freq channel.
      VSOFF = NRPARM + (CHUV1-1) * LVIS
      VIS = UV + VSOFF
C                                       Set weight pointer.
      WTOFF = IWOFF
      IF (IWOFF.LE.0) WTOFF = VSOFF - IWOFF
      WT = UV + WTOFF
C                                       Convolving functions
      CALL CONVFN (CX, CTYPX, XPARM, BUFF1)
      CALL CONVFN (CY, CTYPY, YPARM, BUFF1)
C                                       Frequency scaling table
      FREQLO = FREQG(CHUV1)
      FREQHI = FREQG(CHUV1)
      LIMIT = CHUV1 + NCHAVG - 1
      II0 = CHUV1 + 1
      DO 110 II = II0,LIMIT
C                                       Use BUFF1.
         IF (FREQG(II).GT.0.0) THEN
            FFRAC = (FREQG(II) / FREQG(II-1)) - 1.0D0
            BUFF1(II-CHUV1) = FFRAC
            FREQHI = MAX (FREQHI, FREQG(II))
         ELSE
            IRET = 8
            WRITE (MSGTXT,1090) II
            GO TO 995
            END IF
 110     CONTINUE
      DO 115 LFIELD = 1,NFIELD
         APCONS(7,LFIELD) = APCONS(7,LFIELD) * FREQLO/FREQHI
         APCONS(8,LFIELD) = APCONS(8,LFIELD) * FREQLO/FREQHI
 115     CONTINUE
C                                       tell user channels
      IF ((BEAM) .AND. (IFIELD.EQ.1)) THEN
         IF (NCHAVG.GT.1) THEN
            WRITE (MSGTXT,1100) NCHAVG, FREQG(CHUV1),
     *         FREQG(CHUV1+NCHAVG-1)
         ELSE
            WRITE (MSGTXT,1101) FREQG(CHUV1)
            END IF
         CALL MSGWRT (2)
         END IF
      CALL QPUT (BUFF1, 29, NCHGRD, 2)
C                                       Set freq. scaling factors
      FFRAC = (FREQG(CHUV1) / FREQUV) - 1.0D0
      APCONI(1) =  CX
      APCONI(2) =  CY
      APCONI(3) =  GRID(LFIELD)
C                                       Set limits.
C                                       Stay away from edge of grid.
      DU = RAD2AS / (2 * ABS (CELLSG(1)))
      DV = RAD2AS / (2 * CELLSG(2))
      UUMAX = DU - (0.5 + MAX (3.0, XPARM(1))) * DUMAX
      VVMAX = DV - (0.5 + MAX (3.0, YPARM(1))) * DVMAX
      BLMX2 = 1.0E6 * BLMAX * BLMAX
      IF (BLMAX.LE.1.0) BLMX2 = 1.0E20
      BLMN2 = 1.0E6 * BLMIN * BLMIN
C                                       Rescale baseline limits.
      BLMN2 = BLMN2 / ((1.0D0 + FFRAC) ** 2)
      BLMX2 = BLMX2 / ((1.0D0 + FFRAC) ** 2)
      UUMAX = UUMAX / (1.0D0 + FFRAC)
      VVMAX = VVMAX / (1.0D0 + FFRAC)
      UUMAXG = DU * (1. - GUARDB(1)) / (1.0D0 + FFRAC)
      VVMAXG = DV * (1. - GUARDB(2)) / (1.0D0 + FFRAC)
      UUMAXG = MIN (UUMAXG, UUMAX)
      VVMAXG = MIN (VVMAXG, VVMAX)
      NPOINT = 0
      INCNT = 1
      CNT = 0
      ILENBU = MAXREC
      IDATA = SDAT
C                                       Init UV file.
      CALL UVINIT ('READ', LUNUV, INDS2, NVIS, VO, LREC, ILENBU,
     *   JBUFSZ, BUFF1, BO, BIND2, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1060) IRET
         GO TO 995
         END IF
C                                       Loop through data.
 150     CALL UVDISK ('READ', LUNUV, INDS2, BUFF1, NIO, BIND2, IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1150) IRET
            GO TO 995
            END IF
         INPTR = BIND2
         IF (NIO.LE.0) GO TO 300
C                                       Munge Buffer
         DO 200 IU = 1,NIO
            JNPTR = INPTR + ILOCU
            UIN = ABS (BUFF1(JNPTR))
            VIN = ABS (BUFF1(JNPTR+1))
C                                       Check UV location
            BLEN = UIN*UIN + VIN*VIN
            IF (BLEN.LT.BLMX2) THEN
               GUAXAU = MAX (GUAXAU, UIN)
               GUAXAV = MAX (GUAXAV, VIN)
               END IF
C                                       Dummy data, zero weight.
            IF ((BLEN.LT.BLMN2) .OR. (BLEN.GT.BLMX2) .OR.
     *         (UIN.GE.UUMAXG) .OR. (VIN.GE.VVMAXG)) THEN
               JNDX = INPTR + VSOFF
               BUFF1(JNPTR) = 0.0
               BUFF1(JNPTR+1) = 0.0
               IF (WTOFF.GT.0) BUFF1(INPTR+WTOFF) = 0.0
               DO 160 JJ = 1,MCHGRD
                  BUFF1(JNDX) = 0.0
                  BUFF1(JNDX+1) = 0.0
                  IF (LVIS.GT.2) BUFF1(JNDX+2) = 0.0
                  JNDX = JNDX + LVIS
 160              CONTINUE
            ELSE
               GUAXBU = MAX (GUAXBU, UIN)
               GUAXBV = MAX (GUAXBV, VIN)
               GUACNT = GUACNT + 1
               END IF
C                                       if U negative, then flip
C                                       U, V, W, conjugate data.
            IF (BUFF1(JNPTR).LT.0.0) THEN
               BUFF1(JNPTR) = -BUFF1(JNPTR)
               BUFF1(JNPTR+1) = -BUFF1(JNPTR+1)
               BUFF1(JNPTR+2) = -BUFF1(JNPTR+2)
               JNDX = INPTR + VSOFF + 1
               DO 180 JJ = 1,MCHGRD
                  BUFF1(JNDX) = -BUFF1(JNDX)
                  JNDX = JNDX + LVIS
 180              CONTINUE
               END IF
            MAXROW = MAX (BUFF1(JNPTR), MAXROW)
            NPOINT = NPOINT + 1
            INPTR = INPTR + LREC
 200        CONTINUE
C                                       Move to  AP.
C                                       Time to grid
            IF ((CNT+NPOINT).GT.MAXREC) THEN
               DO 210 LFIELD = 1,NFIELD
                  CALL QPUT (APCONS(1,LFIELD), 0, 29, 2)
                  APCONI(3) = GRID(LFIELD)
                  CALL QPUT (APCONI, 17, 3, 1)
                  ITEMP = -CNT
                  IF (DOSHFT(LFIELD)) ITEMP = -ITEMP
                  ISDAT = SDAT
                  IF (FIELDS(LFIELD).LE.0) ISDAT = -ISDAT
                  NCHGRD = NCHAVG
                  IF ((TAPERS(1,LFIELD).GT.0.0) .OR.
     *               (TAPERS(2,LFIELD).GT.0.0)) NCHGRD = -NCHGRD
                  CALL QWAIT
                  CALL QNGRD (ISDAT, UV, VIS, LLREC, LLVIS, NO2, M,
     *               LROW(LFIELD), ITEMP, NCHGRD)
                  CALL QWR
 210              CONTINUE
               IDATA = SDAT
               CNT = 0
               END IF
C                                       Load into AP.
            CNT = CNT + NPOINT
            ITEMP = LREC * NPOINT
            CALL QPUT (BUFF1(BIND2), IDATA, ITEMP, 2)
            IDATA = IDATA + ITEMP
            NPOINT = 0
C                                       Read more data
            IF (NIO.GT.0) GO TO 150
C                                       Grid any remaining data
 300  IF (CNT.GT.0) THEN
         DO 310 LFIELD = 1,NFIELD
            CALL QPUT (APCONS(1,LFIELD), 0, 29, 2)
            APCONI(3) = GRID(LFIELD)
            CALL QPUT (APCONI, 17, 3, 1)
            ITEMP = -CNT
            IF (DOSHFT(LFIELD)) ITEMP = -ITEMP
            ISDAT = SDAT
            IF (FIELDS(LFIELD).LE.0) ISDAT = -ISDAT
            NCHGRD = NCHAVG
            IF ((TAPERS(1,LFIELD).GT.0.0) .OR.
     *         (TAPERS(2,LFIELD).GT.0.0)) NCHGRD = -NCHGRD
            CALL QWAIT
            CALL QNGRD (ISDAT, UV, VIS, LLREC, LLVIS, NO2, M,
     *         LROW(LFIELD), ITEMP, NCHGRD)
            CALL QWR
 310        CONTINUE
         END IF
C                                       Do zero spacing flux densities
      IF (DOZERO) THEN
C                                       Fill values
         CALL RFILL (NRPARM, 0.0, BUFF1)
         IF (LVIS.LE.2) BUFF1(1+WTOFF) = ZEROSP(5)
         JNDX = NRPARM + 1
         DO 320 II = 1,NCHAVG
            BUFF1(JNDX) = ZEROSP(1) - TFLUXG
            BUFF1(JNDX+1) = 0.0
            IF (LVIS.GT.2) BUFF1(JNDX+2) = ZEROSP(5)
            JNDX = JNDX + LVIS
 320        CONTINUE
C                                       Put into AP
         ITEMP = NRPARM + LVIS * NCHAVG
         CALL QWAIT
         CALL QPUT (BUFF1, SDAT, ITEMP, 2)
         CALL QWAIT
         DO 330 LFIELD = 1,NFIELD
            CALL QPUT (APCONS(1,LFIELD), 0, 29, 2)
            APCONI(3) = GRID(LFIELD)
            CALL QPUT (APCONI, 17, 3, 1)
            ITEMP = -1
            IF (DOSHFT(LFIELD)) ITEMP = -ITEMP
            ISDAT = SDAT
            IF (FIELDS(LFIELD).LE.0) ISDAT = -ISDAT
            NCHGRD = NCHAVG
            IF ((TAPERS(1,LFIELD).GT.0.0) .OR.
     *         (TAPERS(2,LFIELD).GT.0.0)) NCHGRD = -NCHGRD
            CALL QWAIT
            CALL QNGRD (ISDAT, UV, VIS, LLREC, LLVIS, NO2, M,
     *         LROW(LFIELD), ITEMP, NCHGRD)
            CALL QWR
 330        CONTINUE
         END IF
C                                       Loop over grids to finish
      DO 400 LFIELD = 1,NFIELD
         IFIELD = FIELDS(LFIELD)
         BEAM = IFIELD.LE.0
         IFIELD = MAX (1, ABS(IFIELD))
         IF (IFIELD.GT.1000000) IFIELD = IFIELD - 1000000
         CALL QPUT (APCONS(1,LFIELD), 0, 29, 2)
         APCONI(3) = GRID(LFIELD)
         CALL QPUT (APCONI, 17, 3, 1)
C                                       If beam sum weights
         SUMWT = 0
         IF (BEAM) THEN
            NX = NXBEM(IFIELD)
            NY = NYBEM(IFIELD)
            LIMIT = NX / 2 + M - 1
            XXFLD = ABS (CELLSG(1)) * NX
            ZSCLU = XXFLD / RAD2AS
            CALL QPUT (0.0, APSMWT, 1, 2)
            DO 350 IU = MO2,LIMIT
               U = LIMIT - IU - MO2
               ROW = GRID(LFIELD) + (U + MO2) * IFACT * NY
               CALL QWAIT
               CALL QSVE (ROW, 2, APSMWT, LROW(LFIELD))
C                                       Read sum of weights
               CALL QWR
               CALL QGET (APXTMP, APSMWT, 1, 2)
               CALL QWD
C                                       Double sum wt. since both
C                                       halves of plane used.
               SUMWT = SUMWT + 2 * APXTMP
 350           CONTINUE
            SWTS(LFIELD) = SUMWT
C                                       image
         ELSE
            NX = FLDSZ(1,IFIELD)
            NY = FLDSZ(2,IFIELD)
            LIMIT = NX / 2 + M - 1
            ZSCLU = (NX * ABS (CELLSG(1))) / RAD2AS
            SWTS(LFIELD) = BEMMAX(IFIELD)
            END IF
C                                       Finish grid
         DO 360 IU = MO2,LIMIT
            U = LIMIT - IU - MO2
            ROW = GRID(LFIELD) + (U + MO2) * IFACT * NY
C                                       Finish grid
            IF ((U.LE.(NX/2)) .AND. (U.GE.0)) THEN
               CALL Q1FIN (U, ROW, MO2, LROW(LFIELD))
               END IF
 360        CONTINUE
C                                       Number of rows to FFT
C                                       Find max populated row
C                                       Set Y scaling to row for highest
C                                       freq.
         ZZSCLU = ZSCLU * FREQHI / FREQUV
         IROW = ABS (MAXROW * ZZSCLU) + 0.5
         NROWFT(LFIELD) = IROW + MO2
C                                       Set GRID to point to zero row
         GRID(LFIELD) = GRID(LFIELD) + MO2 * LROW(LFIELD) * IFACT
C                                       Set sum of weights
         IF (BEAM) THEN
            IF (FIELDS(LFIELD).GT.-1000000) BEMMAX(IFIELD) = SUMWT
            WRITE (MSGTXT,1601) IFIELD, SUMWT
            CALL MSGWRT (4)
            END IF
 400     CONTINUE
C                                       Close file
      CALL ZCLOSE (LUNUV, INDS2, IRET)
      GO TO 999
C                                       Error
 995  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1040 FORMAT ('GRDMEM: RAN OUT OF AP MEMORY WITH GRID',I3,' OF',I3)
 1041 FORMAT ('GRDMEM: NEED AT LEAST',I9,' MORE WORDS OF AP MEMORY')
 1050 FORMAT ('GRDMEM: TROUBLE OPENING UV DATA FILE, IER=',I3)
 1060 FORMAT ('GRDMEM: ERROR',I3,' INITIALIZING FILE')
 1090 FORMAT ('GRDMEM: FREQUENCIES UNKNOWN BEGINNING WITH CHANNEL',I6)
 1100 FORMAT ('GRDMEM: Ave',I5,' Channels; ',1PE12.6,' to ',E12.6,' Hz')
 1101 FORMAT ('GRDMEM: Frequency ',1PE12.6,' Hz')
 1150 FORMAT ('GRDMEM: READ ERROR IN VISIBILITY FILE, IER=',I3)
 1600 FORMAT ('GRDMEM: TROUBLE CLOSING FILE, IER=',I3)
 1601 FORMAT ('Field',I5,' Sum of gridding weights = ',1PE12.5)
      END
