      SUBROUTINE UVTBGD (IFIELD, DISKI, CNOSCI, SCROUT, CATUVR, JBUFSZ,
     *   FREQID, BUFF1, BUFF2, IRET)
C-----------------------------------------------------------------------
C! Grids uv data in arbitrary sort order to be FFTed.
C# AP-util UV
C-----------------------------------------------------------------------
C;  Copyright (C) 1995, 1997, 1999-2000, 2006-2007
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
C   UVTBGD convolves uv data onto a grid using AP routines.
C   The visibilities are convolved onto the grid using the convolving
C   function specified by CTYPX,CTYPY,XPARM,YPARM.  The defaults for
C   these values must be filled in by a call to GRDFLT.
C   The gridded data is phase rotated so that the map center comes out
C   at location ICNTRX,ICNTRY.  If requested, a uv taper is applied to
C   the visibility weights before gridding.  If necessary, a three
C   dimension phase reference position  shift is done in AP1GRD.
C      This routine will grid data in arbitrary sord order if there is
C   sufficient memory.  All gridding is done in a single pass and the
C   data is written to disk.
C      Zero spacing flux densities are gridded if provided.
C   Uses AIPS LUNs 18, 20, 21
C   Input uv data file in UV file CNOSCI.
C   Output grid file in image file SCROUT although SCROUT and SCRWRK may
C   be switched if bandwidth synthesis is used.
C   Inputs:
C      IFIELD      I     Field number to grid, if 0 then grid a beam.
C      DISKI       I     Input file disk number for catalogd files,
C                        .LE. 0 => /CFILES/ scratch file.
C      CNOSCI      I     Input file catalog slot number or /CFILES/
C                        scratch file number.
C      DISKO       I     Output file disk number for catalogd files,
C                        .LE. 0 => /CFILES/ scratch file.
C      SCROUT      I     /CFILES/ scratch file number of output file
C      CATUVR(256) R     UV data catalog header record.
C      JBUFSZ      I     Size in bytes of buffers. Dimension of
C                        BUFF1,2,3  must be at least 4096 words
C      FREQID      I     Freq ID number, if it exists.
C   From commons: (Includes DGDS, DMPR, DUVH, CGDS, CMPR, CUVH)
C      NVIS         I    Number of visibility records (/UVHDR/)
C      LREC         I    Number of (real) words per visibility record
C                        (/UVHDR/)
C      NCHAVG       I    Number of frequency channels to grid
C                        together.
C      FLDSZ(2,*)   I    Dimension of map in RA, Dec (cells)
C      CELLSG(2)    R    The cell spacing in X and Y in arcseconds.
C      CHUV1        I    First channel number in file to grid
C                        (1 relative)
C      FREQ         D    Reference frequency (Hz) (/UVHDR/)
C      JLOCF        I    0 relative number of the frequency axis,
C                        (/UVHDR/)
C      TFLUXG       R    The total flux density removed from the data,
C                        this will be subtracted from the zero spacing
C                        flux before gridding.
C      CTYPX,CTYPY  I    Convolving function types for RA and Dec
C      XPARM(10)    R    Convolving function parameters for RA
C                        XPARM(1) = support half width.
C      YPARM(10)    R    Convolving function parameters for Dec.
C      BLMAX        R    Maximum baseline length allowed in 1000s of
C                        wavelengths.
C      BLMIN        R    Minimum baseline length allowed in 1000s of
C                        wavelengths.
C      GUARDB       R(2) Fraction of UMAX and VMAX to blank on edge of
C                        field
C      DOZERO       L    If true then do zero spacing flux.
C      ZEROSP(5)    R    Zero spacing flux, 1=>flux density (Jy)
C                        5 => weight to use.
C                        polarization.
C      DOTAPE       L    True if taper requested.
C      TAPERU,TAPERV R   TAPER ( to 30%) in u and v (kilolamda)
C      NXBEM,NYBEM  I    The size of the BEAM in pixels.
C      FREQG(*)     D    Frequencies of the channels
C      FREQUV       D    Reference frequency for u,v, and w.
C      NGRDAT       L    If FALSE get map size, scaling etc. parms
C                        from the model map cat. header. If TRUE
C                        then the values filled in by GRDAT must
C                        already be filled into the common.
C   The following must be provided if NGRDAT is .TRUE.
C      XFLD,YFLD(*)    R    Field of view in RA and Dec (arcseconds)
C      DXCG,DYCG,DZCG  R    2*pi*(delta ra, delta dec, and delta z)
C                           to be used in AP1GRD to shift positions.
C                           (u,v and w are in cells). one per field.
C      SCLUG,SCLVG,SCLWG R   Conversion factors for u,v and w from
C                           wavelengths at the reference frequency
C                           to cells. one set per field.
C      ICNTRX,ICNTRY(*) I   The center pixel in X and Y for each
C                           field.
C   The following must be provided if NGRDAT is .FALSE.
C      CCDISK(16)  I     Disk numbers of the output images.
C      CCCNO(16)   I     Catalog slot numbers of output images.
C   Output:
C      SCROUT      I     /CFILES/ scratch file number of output file
C      BUFF1       R     Working buffer
C      BUFF2       R     Working buffer
C      IRET        I     Return error code. 0=>OK, error otherwise.
C   Output via common:
C      BEMMAX      R     Sum of weights = normalization factor
C      GUAXAU,GUAXAV R   Max U, V in data set
C      GUAXBU,GUAXBV R   Max U, V used in grid
C   Usage Notes:
C    1) The input uvdata file is, with one exception, assumed to be
C     accurately described by the contents of CATUVR and the common
C     /UVHDR/ (includes DUVH, CUVH).  The exception is that the
C     frequencies of the channels are given by the common array FREQG.
C     The u,v, and w are assumes to be given by the common variable
C     FREQUV.
C    2) the contents of common /UVHDR/ (=includes DUVH, CUVH)
C     are filled in by UVPGET from the catalog header; UVPGET should
C     be called before calling UVTBGD.
C    3) if NGRDAT is .FALSE. then the properties (e.g. shift) of the
C     desired output image are assumed to be described in the catalog
C     header of the existant file pointed to by CCDISK,CCCNO(IFIELD).
C    4) only one polarization will be processed and the input data is
C     assumed to be in the desired Stokes' type (i.e. I, Q, U, V etc.)
C     In the general case this will require reformatting the data.
C     This can be accomplished via CALCOP to do the whole file or
C     UVGET or SET1VS & GET1VS which work a record at a time.
C     Multiple channels may be gridded together a la bandwidth
C     synthesis by specifying NCHAVG > 1. One channel of several
C     channels may be gridded using CHUV1 > 1.
C    5) the random parameters in the data should include, in order,
C     u, v, w, weight (optional), time (optional) and baseline
C     (optional).  While the last are optional and not used, the last
C     words of random parameters are used as work space and, if they
C     are missing, u, v, and w may be clobbered.  The weights are
C     required but may be passed either as random parameters or as
C     part of the regular data array, CATUVR should tell which.
C    6) The necessary image normalization constant for proper
C     normalization of the FFTed image is produced only by a call
C     with IFIELD=0 to grid the sampling function.  Therefore,
C     UVTBGD must be called to grid the sampling function IRREGARDLESS
C     of whether or not a beam will be produced.
C    7) The gridding convolution function parameters must be completely
C     specified.  The defaults should be filled in by a call to
C     GRDFLT before calling UVTBGD.
C    8) Multiple IFs can be processed using the common frequency table
C     FREQG
C-----------------------------------------------------------------------
      INTEGER   IFIELD, DISKI, CNOSCI, SCROUT, JBUFSZ, FREQID, IRET
      REAL      BUFF1(*), BUFF2(*), CATUVR(256)
C
      CHARACTER NAME*48
      INTEGER   VO, BO, LLVIS, VOL, LUNUV, LUN1, JJ, IFACT, INPTR, IERR,
     *   IU, JNPTR, NIO, NPOINT, NX2, BIND2, ILENBU, ICENX, ICENY,
     *   INDS2, NX, NY, JNDX, FLIST(22), VSOFF, LVIS, WTOFF, IWOFF,
     *   NUMROW, NCHGRD, KAP, U, GRID, ROW, NUM, CY, CX, VIS,  WT,
     *   IDATA, UV, ITEMP, LLREC, NO2, M, LROW, TOLROW,  MO2, INDEX,
     *   CNT, MAXREC, APSIZE, APSMWT, I, II, INCNT,  NEEDED, LIMIT,
     *   MCHGRD, APCONI(3), LFIELD, NEED
      LOGICAL   T, F, DOSHFT, BEAM, ONEWT
      REAL      APCONS(20), TUC, TVC, XMAX, ZSCLU, ZSCLV, ZSCLW, XXFLD,
     *   YYFLD, DDXS(3), FFRAC, SUMWT, APXTMP, BLEN, BLMN2, BLMX2,
     *   UUMAX, VVMAX, DU, DV, DW, UIN, VIN, UUMAXG, VVMAXG, UMAT(3,3),
     *   PMAT(3,3)
      DOUBLE PRECISION   WX, WY, TWOPI, FREQLO, FREQHI, XRA, XDEC
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DAPM.INC'
      INCLUDE 'INCS:DGDS.INC'
      INCLUDE 'INCS:DMPR.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DHDR.INC'
      DATA T, F /.TRUE.,.FALSE./
      DATA VO, BO /0, 1/
      DATA LUNUV, LUN1 /18, 20/
C-----------------------------------------------------------------------
      IRET = 0
      IF ((GUARDB(1).LT.0.0) .OR. (GUARDB(1).GT.0.9)) GUARDB(1) = 0.0
      IF ((GUARDB(2).LT.0.0) .OR. (GUARDB(2).GT.0.9)) GUARDB(2) = 0.0
      GUAXAU = 0.
      GUAXAV = 0.
      GUAXBU = 0.
      GUAXBV = 0.
      GUACNT = 0
      SUMWT = 0.0
C                                       Get field info. if nec.
      IF (.NOT.NGRDAT) THEN
         CALL GRDAT (F, IFIELD, CATUVR, BUFF1, IRET)
         IF (IRET.NE.0) GO TO 999
         END IF
C                                       Fill channel table.
      LFIELD = MAX (1, ABS(IFIELD))
      IF ((IFIELD.LE.0) .AND. (LFIELD.EQ.1)) THEN
         CALL FRQTAB (DISKI, CNOSCI, LUNUV, CATUVR, FREQID, BUFF1, IRET)
         IF (IRET.NE.0) GO TO 999
         END IF
      IF (DO3DIM) THEN
         MSGTXT = 'WARNING: UVTBGD CALLED WITH DO3DIM TRUE'
         CALL MSGWRT (6)
         CALL XYSHFT (RA, DEC, XSHIFT(LFIELD), YSHIFT(LFIELD), MAPROT,
     *      XRA, XDEC)
         CALL PRJMAT (RA, DEC, UVROT, XRA, XDEC, MAPROT, UMAT, PMAT)
         END IF
C                                       Set parameters for beam/map.
      BEAM = IFIELD.LE.0
C                                       Map.
      IF (IFIELD.GT.0) THEN
         NX = FLDSZ(1,IFIELD)
         NY = FLDSZ(2,IFIELD)
         ICENX = ICNTRX(IFIELD)
         ICENY = ICNTRY(IFIELD)
         ZSCLU = 1.0 / (2.06264E5 / (NX * ABS (CELLSG(1))))
C                                       Flip sign on v to make maps come
C                                       out upside down.
         ZSCLV =  - 1.0 / (2.06264E5 / (NY * CELLSG(2)))
         ZSCLW = 1.0
         XXFLD = XFLD(IFIELD)
         YYFLD = YFLD(IFIELD)
         DDXS(1) = -DXCG(IFIELD)
         DDXS(2) = -DYCG(IFIELD)
         DDXS(3) = -DZCG(IFIELD)
         IF (DO3DIM) CALL PRJMUL (2, DDXS, PMAT, DDXS)
C                                       Beam
      ELSE
         NX = NXBEM(LFIELD)
         NY = NYBEM(LFIELD)
         ICENX = NX / 2
         ICENY = NY / 2 + 1
         ZSCLU = SCLUG(1) * NX / FLDSZ(1,LFIELD)
         ZSCLV = SCLVG(1) * NY / FLDSZ(2,LFIELD)
         ZSCLW = 1.0E-6
         XXFLD = ABS (CELLSG(1)) * NX
         YYFLD = ABS (CELLSG(2)) * NY
         DDXS(1) = 0.0
         DDXS(2) = 0.0
         DDXS(3) = 0.0
         END IF
      DOSHFT = (DDXS(1).NE.0.0) .OR. (DDXS(2).NE.0.0)
C                                       Set baseline limits
C                                       Determine some constants.
      DU = 2.06264E5 / (NX * ABS (CELLSG(1)))
      DV = 2.06264E5 / (NY * CELLSG(2))
      DW = 1.0
C                                       Setup I/O
      ILENBU = 0
C                                       Set up for APIO to transfer
C                                       the GRID to and from the AP.
C                                       Init FLIST
      CALL FILL (22, 0, FLIST)
      FLIST(1) = LUN1
      FLIST(2) = SCROUT
      FLIST(13) = JBUFSZ
C                                       Set grid size
      FLIST(5) = NY * 2
      FLIST(6) = NX / 2 + 1
C                                       Set window - write backwards
      FLIST(9) = 1
      FLIST(10) = FLIST(6)
      FLIST(11) = FLIST(5)
      FLIST(12) = 1
      NCHGRD = NCHAVG
      MCHGRD = NCHAVG
C-----------------------------------------------------------------------
C                                       Set various constants
C                                       Frequency scaling constants
      NX2 = NX / 2
C                                       Set miscellaneous AP values.
      NO2 = MAX (YPARM(1) , 1.0) + 0.1
      M = MAX (XPARM(1) , 1.0) + 0.1
      M = M * 2 + 1
      MO2 = M / 2
      LLREC = LREC
      LROW = NY
      TOLROW = 2 * LROW
      NUMROW = NX / 2 + 1 + M
C                                       IFACT is the number of REAL
C                                       accumulators in GRID per cell
      IFACT = 2
C                                       Is there sufficient memory?
      NEEDED = (NY * IFACT * NUMROW) + ((NO2*2+1) * 100)  + (M * 100)
     *   + NCHAVG + 100 +  (JBUFSZ/2)
      NEED = NEEDED / 1024 + 2
C                                       Grab AP
      CALL QINIT (NEED, 0, KAP)
      IF ((KAP.EQ.0) .OR. (PSAPNW.EQ.0)) THEN
         MSGTXT = 'UVTBGD CANNOT GET NEEDED MEMORY'
         IRET = 8
         GO TO 995
         END IF
      APSIZE = PSAPNW * 1024
      IF (NEEDED.GT.APSIZE) THEN
         IRET = 9
         WRITE (MSGTXT,1040) NEEDED
         GO TO 995
         END IF
C                                       Set AP pointers
      GRID = APSIZE - (NUMROW * IFACT) * NY - 1
C                                       Set size of grid to clear
      NUM = IFACT * NY * NUMROW
C                                       Set AP locations for conv. fn.
      CY = GRID - 100 * (2 * NO2 + 1) - 1
      CX = CY - 100 * M - 1
C                                       Location of sum of weights for
C                                       beam.
      APSMWT = 21 + NCHAVG
C                                       Set AP data locations
      UV = APSMWT + 1
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
      TWOPI = 8.0D0 * ATAN (1.0D0)
      WX = (-TWOPI * (ICENX-1-NX/2)) / NX
      WY = (-TWOPI * (ICENY-1-NY/2)) / NY
C                                       Convert taper widths to
C                                       convenient constants.
      TUC = 0.0
      TVC = 0.0
      IF (DOTAPE) THEN
         IF (TAPERU.GT.0.) TUC = LOG(.3) / (TAPERU * 1E3 /
     *      206265. * XXFLD) ** 2
         IF (TAPERV.GT.0.) TVC = LOG(.3) / (TAPERV * 1E3 /
     *      206265. * YYFLD) ** 2
         END IF
C                                       Set AP gridding constants.
      APCONS(1) = COS ((NX / 2) * WX)
      APCONS(2) = SIN ((NX / 2) * WX)
      APCONS(3) = COS (WY)
      APCONS(4) = SIN (WY)
      APCONS(5) = COS (-WX)
      APCONS(6) = SIN (-WX)
      APCONS(7) = 1.0
      APCONS(8) = 0.0
      APCONS(9) = 0.0
      APCONS(10) = TUC
      APCONS(11) = TVC
      APCONI(1) =  CX
      APCONI(2) =  CY
      APCONI(3) =  GRID
C                                       Determine the maximum number
C                                       of visibility points which
C                                       fit in the AP.
      MAXREC = (CX - UV) / LREC - 5
C                                       Be sure MAXREC.GT.10
      IF (MAXREC.LE.10) THEN
         XMAX = - MAXREC * LREC
         IF (MAXREC.GT.0) XMAX = (11 - MAXREC) * LREC
         WRITE (MSGTXT,1040) XMAX
         IRET = 1
         GO TO 995
         END IF
C                                       Open files. Use UVINIT and
C                                       UVDISK to read the
C                                       visibility file,
C                                       APIO to read and write the
C                                       grid file.
      VOL = DISKI
      IF (DISKI.LE.0) VOL = SCRVOL(CNOSCI)
      IF (DISKI.GT.0) CALL ZPHFIL ('UV', VOL, CNOSCI, 1, NAME, IRET)
      IF (DISKI.LE.0) CALL ZPHFIL ('SC', VOL, SCRCNO(CNOSCI), 1,
     *   NAME, IRET)
      CALL ZOPEN (LUNUV, INDS2, VOL, NAME, T, F, T, IRET)
      IF (IRET.GT.0) THEN
         WRITE (MSGTXT,1050) IRET
         GO TO 995
         END IF
C-----------------------------------------------------------------------
C                                       Grid data.
C                                       Init UV file.
      CALL UVINIT ('READ', LUNUV, INDS2, NVIS, VO, LREC, ILENBU,
     *   JBUFSZ, BUFF1, BO, BIND2, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1060) IRET
         GO TO 995
         END IF
      FLIST(13) = JBUFSZ
C                                       Clear grid.
      CALL QVCLR (GRID, 1, NUM)
C                                       Offset VIS to freq channel.
      VSOFF = NRPARM + (CHUV1-1) * LVIS
      VIS = UV + VSOFF
C                                       Set weight pointer.
      WTOFF = IWOFF
      IF (IWOFF.LE.0) WTOFF = VSOFF - IWOFF
      WT = UV + WTOFF
C                                       Convolving functions
      CALL CONVFN (CX, CTYPX, XPARM, BUFF2)
      CALL CONVFN (CY, CTYPY, YPARM, BUFF2)
C                                       Frequency scaling table
      FREQLO = FREQG(CHUV1)
      FREQHI = FREQG(CHUV1)
      LIMIT = CHUV1 + NCHAVG - 1
      DO 100 II = CHUV1,LIMIT
C                                       Use BUFF2.
         IF (II.GT.CHUV1) BUFF2(II-CHUV1) =
     *      (FREQG(II) / FREQG(II-1)) - 1.0D0
         FREQLO = MIN (FREQLO, FREQG(II))
         FREQHI = MAX (FREQHI, FREQG(II))
 100     CONTINUE
      CALL QPUT (BUFF2, 20, NCHGRD, 2)
      IF (DOTAPE) NCHGRD = -NCHGRD
C                                       Set freq. scaling factors
      FFRAC = (FREQG(CHUV1) / FREQUV) - 1.0D0
C                                       Set limits.
C                                       Stay away from edge of grid.
      UUMAX = (NX/2-0.5) * DU - MAX (3.0, XPARM(1)) * DU
      VVMAX = (NY/2-0.5) * DV - MAX (3.0, YPARM(1)) * DV
      BLMX2 = 1.0E6 * BLMAX * BLMAX
      IF (BLMAX.LE.1.0) BLMX2 = 1.0E20
      BLMN2 = 1.0E6 * BLMIN * BLMIN
C                                       Rescale baseline limits.
      BLMN2 = BLMN2 / ((1.0D0 + FFRAC) ** 2)
      BLMX2 = BLMX2 / ((1.0D0 + FFRAC) ** 2)
      UUMAX = UUMAX / (1.0D0 + FFRAC)
      VVMAX = VVMAX / (1.0D0 + FFRAC)
      UUMAXG = NX/2 * DU * (1. - GUARDB(1)) / (1.0D0 + FFRAC)
      VVMAXG = NY/2 * DV * (1. - GUARDB(2)) / (1.0D0 + FFRAC)
      UUMAXG = MIN (UUMAXG, UUMAX)
      VVMAXG = MIN (VVMAXG, VVMAX)
C                                       Put gridding constants into AP
      APCONS(12) = FFRAC*ZSCLU + ZSCLU
      APCONS(13) = FFRAC*ZSCLV + ZSCLV
      APCONS(14) = FFRAC*ZSCLW + ZSCLW
      APCONS(15) = DDXS(1) / ZSCLU
      APCONS(16) = DDXS(2) / ZSCLV
      APCONS(17) = DDXS(3) / ZSCLW
      APCONI(3) = GRID
      CALL QPUT (APCONS, 0, 17, 2)
      CALL QPUT (APCONI, 17, 3, 1)
      NPOINT = 0
      INCNT = 1
      CNT = 0
      IDATA = UV
C                                       Loop through data.
 150     CALL UVDISK ('READ', LUNUV, INDS2, BUFF1, NIO, BIND2, IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1100) IRET
            GO TO 995
            END IF
         INPTR = BIND2
         IF (NIO.LE.0) GO TO 500
C                                       Munge Buffer
         DO 200 IU = 1,NIO
            JNPTR = INPTR + ILOCU
            IF (DO3DIM) CALL PRJMUL (1, BUFF1(JNPTR), UMAT,
     *         BUFF1(JNPTR))
            UIN = ABS (BUFF1(JNPTR))
            VIN = ABS (BUFF1(JNPTR+1))
C                                       Check UV location
            BLEN = UIN * UIN + VIN * VIN
            IF (BLEN.LE.BLMX2) THEN
               GUAXAU = MAX (GUAXAU, UIN)
               GUAXAV = MAX (GUAXAV, VIN)
               END IF
C                                       Dummy data, zero weight.
            IF (((BLEN.LT.BLMN2) .OR. (BLEN.GT.BLMX2)) .OR.
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
               GUACNT = GUACNT + 1
               GUAXBU = MAX (GUAXBU, UIN)
               GUAXBV = MAX (GUAXBV, VIN)
               END IF
C                                       BEAM replace data with (1,0)
            IF (BEAM) THEN
               JNDX = INPTR + VSOFF
               DO 170 JJ = 1,MCHGRD
                  BUFF1(JNDX) = 1.0
                  BUFF1(JNDX+1) = 0.0
                  JNDX = JNDX + LVIS
 170              CONTINUE
               END IF
C                                       Check if U neg.; if so then flip
C                                       data to other half plane.
            IF (BUFF1(JNPTR).LT.0.0) THEN
C                                       Flip U, V, W, conjugate data.
               BUFF1(JNPTR) = -BUFF1(JNPTR)
               BUFF1(JNPTR+1) = -BUFF1(JNPTR+1)
               BUFF1(JNPTR+2) = -BUFF1(JNPTR+2)
               JNDX = INPTR + VSOFF + 1
               DO 180 JJ = 1,MCHGRD
                  BUFF1(JNDX) = -BUFF1(JNDX)
                  JNDX = JNDX + LVIS
 180              CONTINUE
               END IF
            NPOINT = NPOINT + 1
            INPTR = INPTR + LREC
 200        CONTINUE
C                                       Move to  AP.
C                                       Time to grid
         IF ((CNT+NPOINT).GT.MAXREC) THEN
            ITEMP = -CNT
            IF (DOSHFT) ITEMP = -ITEMP
            CALL Q1GRD (UV ,VIS, WT, LLREC, LLVIS, NO2, M, LROW, ITEMP,
     *         NCHGRD)
            CALL QWR
            IDATA = UV
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
C                                       Check if AP full or row finished
 500  IF (CNT.GT.0) THEN
         ITEMP = -CNT
         IF (DOSHFT) ITEMP = -ITEMP
         CALL Q1GRD (UV ,VIS, WT, LLREC, LLVIS, NO2, M,
     *      LROW, ITEMP, NCHGRD)
         CALL QWR
         END IF
C                                       Do zero spacing flux densities
      IF (DOZERO) THEN
C                                       Fill values
         CALL RFILL (NRPARM, 0.0, BUFF1)
         IF (LVIS.LE.2) BUFF1(1+WTOFF) = ZEROSP(5)
         JNDX = NRPARM + 1
         DO 550 II = 1,NCHAVG
            BUFF1(JNDX) = ZEROSP(1) - TFLUXG
            BUFF1(JNDX+1) = 0.0
            IF (LVIS.GT.2) BUFF1(JNDX+2) = ZEROSP(5)
            JNDX = JNDX + LVIS
 550        CONTINUE
C                                       Put into AP
         ITEMP = NRPARM + LVIS * NCHAVG
         CALL QWAIT
         CALL QPUT (BUFF1, UV, ITEMP, 2)
         ITEMP = -1
         IF (DOSHFT) ITEMP = -ITEMP
         CALL QWAIT
C                                       Grid
         CALL Q1GRD (UV, VIS, WT, LLREC, LLVIS, NO2, M, LROW, ITEMP,
     *      NCHGRD)
         END IF
      LIMIT = NX / 2 + M - 1
C                                       If beam sum weights
      IF (BEAM) THEN
         DO 600 IU = MO2,LIMIT
            U = LIMIT - IU - MO2
            ROW = GRID + (U + MO2) * IFACT * NY
            CALL QWAIT
            CALL QSVE (ROW, 2, APSMWT, LROW)
C                                       Read sum of weights
            CALL QWR
            CALL QGET (APXTMP, APSMWT, 1, 2)
            CALL QWD
C                                       Double sum wt. since both
C                                       halves of plane used.
            SUMWT = SUMWT + 2 * APXTMP
 600        CONTINUE
         END IF
C                                       Finish grid and write to disk
      DO 700 IU = MO2,LIMIT
         U = LIMIT - IU - MO2
         ROW = GRID + (U + MO2) * IFACT * NY
C                                       Finish grid
         IF ((U.LE.(NX/2)) .AND. (U.GE.0)) THEN
            CALL Q1FIN (U, ROW, MO2, LROW)
C                                       Write new GRID file values.
            CALL QWAIT
            CALL APIO ('WRIT', FLIST, ROW, BUFF2, IRET)
            IF (IRET.NE.0) THEN
               WRITE (MSGTXT,1550) IRET, I
               GO TO 995
               END IF
            END IF
 700     CONTINUE
C                                       Give up AP
      CALL QRLSE
C                                       Close files
      CALL ZCLOSE (LUNUV, INDS2, IRET)
      CALL APIO ('CLOS', FLIST, INDEX, BUFF2, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1600) IRET
         GO TO 995
         END IF
C                                       Set sum of weights
      IF (BEAM) THEN
         BEMMAX(LFIELD) = SUMWT
         WRITE (MSGTXT,1601) LFIELD, BEMMAX(LFIELD)
         CALL MSGWRT (4)
         END IF
      GO TO 999
C                                       Error
 995  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1040 FORMAT ('UVTBGD: NEED AT LEAST ',I9,' WORDS OF AP MEMORY')
 1050 FORMAT ('UVTBGD: TROUBLE OPENING UV DATA FILE, IER=',I3)
 1060 FORMAT ('UVTBGD: ERROR',I3,' INITIALIZING FILE')
 1100 FORMAT ('UVTBGD: READ ERROR IN VISIBILITY FILE, IER=',I3)
 1550 FORMAT ('UVTBGD: ERROR',I3,' WRITING GRID ROW ',I5)
 1600 FORMAT ('UVTBGD: TROUBLE CLOSING FILE, IER=',I3)
 1601 FORMAT ('Field',I5,' Sum of gridding weights = ',1PE12.5)
      END
