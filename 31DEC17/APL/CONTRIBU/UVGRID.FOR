      SUBROUTINE UVGRID (IFIELD, SCRWRK, DISKI, CNOSCI, SCROUT, CATUVR,
     *   JBUFSZ, FREQID, BUFF1, BUFF2, BUFF3, IRET)
C-----------------------------------------------------------------------
C! Grids uv data to be FFTed.
C# AP-util UV
C-----------------------------------------------------------------------
C;  Copyright (C) 1995, 1997, 1999-2000, 2006-2008
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
C   UVGRID convolves uv data onto a grid using AP routines.
C   The visibilities are convolved onto the grid using the convolving
C   function specified by CTYPX,CTYPY,XPARM,YPARM.  The defaults for
C   these values must be filled in by a call to GRDFLT.
C   The gridded data is phase rotated so that the map center comes out
C   at location ICNTRX,ICNTRY.  If requested, a uv taper is applied to
C   the visibility weights before gridding.  If necessary, a three
C   dimension phase reference position  shift is done in AP1GRD.
C   If more than one channels are to be gridded together, UVGRID loops
C   over the frequency channels in an outer loop, reading the grid and
C   uv data several times and writing the grid several times.  This
C   bandwidth synthesis (BS) process will use the SCRWRK file.
C   For bandwidth synthesis both the SCROUT and SCRWRK files should be
C   big enough for an extra m rows, where m is the half width of the
C   X convolving function.
C      This version uses a scrolling buffer as large as possible using
C   both the primary and secondary AP memory; 1/4 of the memory is used
C   as the uv data bufferwhen possible.  Bandwidth synthesis is done
C   using as many channels as possible.  The number of channels is
C   determined by the width of the scrolling buffer, the channel
C   separations and the highest row number on which data occurs.
C      Zero spacing flux densities are gridded if provided.
C   Uses AIPS LUNs 18, 20, 21
C   Input uv data file in UV file CNOSCI.
C   Output grid file in image file SCROUT although SCROUT and SCRWRK may
C   be switched if bandwidth synthesis is used.
C   Inputs:
C      IFIELD      I     Field number to grid, if 0 then grid a beam.
C      SCRWRK      I     /CFILES/ file number for work file,
C                        (used for bandwidth synthesis)
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
C      FREQID      I     Id number of FQ entry, if it exists.
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
C      TAPERU,TAPERV R   TAPER (to 30%) in u and v (kilolamda)
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
C      SCRWRK      I     /CFILES/ file number for work file,
C                        (used for bandwidth synthesis)
C      SCROUT      I     /CFILES/ scratch file number of output file
C      BUFF1       R     Working buffer
C      BUFF2       R     Working buffer
C      BUFF3       R     Working buffer (buffers should be contigious
C                        in memory)
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
C     be called before calling UVGRID.
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
C     UVGRID must be called to grid the sampling function IRREGARDLESS
C     of whether or not a beam will be produced.
C    7) The gridding convolution function parameters must be completely
C     specified.  The defaults should be filled in by a call to
C     GRDFLT before calling UVGRID.
C    8) Multiple IFs can be processed using the common frequency table
C     FREQG
C-----------------------------------------------------------------------
      INTEGER   IFIELD, SCRWRK, DISKI, CNOSCI, SCROUT, JBUFSZ, FREQID,
     *   IRET
      REAL      CATUVR(256), BUFF1(*), BUFF2(*), BUFF3(*)
C
      INTEGER   VO, BO, VREAD, LLVIS, LUNUV, LUN1, LUN2, IFACT, INPTR,
     *   IERR, KNPTR, JNPTR, NIO, NPOINT, NXO2, ILENBU, ICENX, ICENY,
     *   INDS2, NX, NY, IFILE1, IFILE2, FLIST(22,2), VSOFF, LVIS, WTOFF,
     *   IWOFF, LOCHAN, MXROGD, NROGRD, NCHGRD, KAP, U, CY, CX, VIS, WT,
     *   IVIS, ILOOP, I, J, IDATA, UV, ITEMP, LLREC, N, NO2, M, MO2,
     *   LROW, ROWSIZ, MCHGRD, INDEX, NROWS, MAXREC, APSIZE, APSMWT,
     *   EXCESS, LOGRID, CPASS, NCPASS, RPASS, NRPASS, IUMIN, IUMAX,
     *   USTRT, USTOP, UCOUNT, MAXU, ERRCNT, APCONI(3), LFIELD, NEED,
     *   MSGSAV, OUTD, OUTC
      LOGICAL   T, F, DOSHFT, BEAM, ALLROW, ONZE
      REAL      APCONS(20), UMIN, UMAX, UIN, VIN, WEIGHT, FLUXZ, TUC,
     *   TVC, SSCLU, FRSTU, UUMAXO, VVMAXO, BLMN2O, BLMX2O, ZSCLU,
     *   ZSCLV, ZSCLW, XXFLD, YYFLD, DDXS(3), FFRAC, SUMWT,
     *   ONESUM, BLEN, BLMN2, BLMX2, UUMAX, VVMAX, DU, DV, UUMAXG,
     *   VVMAXG, UMAT(3,3), PMAT(3,3)
      DOUBLE PRECISION   WX, WY, GFACT, FREQLO, FREQHI, XRA, XDEC
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DAPM.INC'
      INCLUDE 'INCS:DGDS.INC'
      INCLUDE 'INCS:DMPR.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:PSTD.INC'
      SAVE VO, BO, ERRCNT
      DATA T, F /.TRUE.,.FALSE./
      DATA VO, BO /0, 1/,  ERRCNT /0/
      DATA LUNUV, LUN1, LUN2 /18, 20, 21/
C-----------------------------------------------------------------------
      IRET = 0
      SUMWT = 0.0
      UCOUNT = 0
      IF ((GUARDB(1).LT.0.0) .OR. (GUARDB(1).GT.0.9)) GUARDB(1) = 0.0
      IF ((GUARDB(2).LT.0.0) .OR. (GUARDB(2).GT.0.9)) GUARDB(2) = 0.0
      GUAXAU = 0.
      GUAXAV = 0.
      GUAXBU = 0.
      GUAXBV = 0.
      GUACNT = 0
      IF (DISKI.GT.0) THEN
         OUTD = DISKI
         OUTC = CNOSCI
      ELSE
         OUTD = SCRVOL(CNOSCI)
         OUTC = SCRCNO(CNOSCI)
         END IF
C                                       Do zero spacing once each pass
      ONZE = F
      LFIELD = MAX (1, ABS(IFIELD))
C                                       Get field info. if nec.
      IF (.NOT.NGRDAT) THEN
         CALL GRDAT (T, IFIELD, CATUVR, BUFF1, IRET)
         IF (IRET.NE.0) GO TO 999
         END IF
C                                       Fill channel table.

      IF ((IFIELD.LE.0) .AND. (LFIELD.EQ.1)) THEN
         CALL FRQTAB (DISKI, CNOSCI, LUNUV, CATUVR, FREQID, BUFF1, IRET)
         IF (IRET.NE.0) GO TO 999
         END IF
      IF (DO3DIM) THEN
         MSGTXT = 'WARNING: UVGRID CALLED WITH DO3DIM TRUE'
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
         ZSCLU = 1.0 / (RAD2AS / (NX * ABS (CELLSG(1))))
C                                       Flip sign on v to make maps come
C                                       out upside down.
         ZSCLV =  - 1.0 / (RAD2AS / (NY * CELLSG(2)))
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
C                                       Setup I/O
C                                       Set UV data I/O length.
      ILENBU = ((JBUFSZ - NBPS) / 2) / LREC - 2
      ILENBU = MAX (ILENBU, 1)
      NXO2 = NX / 2
C                                       Make sure an ODD number of rows
C                                       is being kept in the AP.
      NO2 = MAX (YPARM(1) , 1.0) + 0.1
      N   = NO2 * 2 + 1
      MO2 = MAX (XPARM(1) , 1.0) + 0.1
      M   = MO2 * 2 + 1
      LLREC = LREC
      LROW = NY
C                                       IFACT is the number of REAL
C                                       accumulators in GRID per cell
      IFACT = 2
      ROWSIZ = IFACT * LROW
C                                       Frequency scaling constants
      DV = RAD2AS / (NY * CELLSG(2))
      DU = RAD2AS / (NX * ABS (CELLSG(1)))
C                                       Stay away from edge of grid.
      UUMAXO = (NXO2 - 0.5 - MAX (3, MO2)) * DU
      VVMAXO = (NY/2 - 0.5 - MAX (3, NO2)) * DV
C                                       Set baseline limits (round out)
      BLMN2O = 0.999E6 * BLMIN * BLMIN
      BLMX2O = 1.001E6 * BLMAX * BLMAX
      IF (BLMX2O.LE.1.0) BLMX2O = 1.0E20
C                                       Set up for APIO to transfer
C                                       the GRID to and from the AP.
      IFILE1 = 2
      IFILE2 = 1
C                                       Init FLIST
      CALL FILL (22, 0, FLIST(1,1))
      CALL FILL (22, 0, FLIST(1,2))
      FLIST(1,1) = LUN1
      FLIST(1,2) = LUN2
      FLIST(2,1) = SCROUT
      FLIST(2,2) = SCRWRK
      FLIST(13,1) = JBUFSZ
C                                       Set grid size
      FLIST(5,1) = ROWSIZ
C                                       Number of rows for write
      FLIST (6,1) = NXO2 + 1
      FLIST(10,1) = FLIST(6,1)
C                                       Set window - write backwards
      FLIST(9,1) = 1
      FLIST(11,1) = FLIST(5,1)
      FLIST(12,1) = 1
C                                       Copy first file FLIST to second
      CALL COPY (10, FLIST(4,1), FLIST(4,2))
C                                       Determine no. rows in AP -
C                                       any excess over allowing 1/4 for
C                                       data buffer goes to extra rows.
      NEED = 1.34 * (30 + NCHAVG + 100 * (N + M) + (NXO2+M)*ROWSIZ)
      NEED = NEED / 1024
C                                       Grab AP to init PSAPNW
      MSGSAV = MSGSUP
      MSGSUP = 32000
      CALL QINIT (NEED, 0, KAP)
      MSGSUP = MSGSAV
      IF ((KAP.EQ.0) .OR. (PSAPNW.EQ.0)) THEN
         NEED = 1.34 * (30 + NCHAVG + 100 * (N + M) + (NXO2/8+M)*ROWSIZ)
         NEED = NEED / 1024
         NEED = MIN (32*1024, NEED) + 2
         CALL QINIT (NEED, 0, KAP)
         IF ((KAP.EQ.0) .OR. (PSAPNW.EQ.0)) THEN
            IRET = 8
            MSGTXT = 'UVGRID UNABLE TO GET DESIRED MEMORY'
            GO TO 995
            END IF
         END IF
      APSIZE = PSAPNW * 1024
      EXCESS = ((APSIZE * 3) / 4) - 30 - NCHAVG - (100 * (N + M))
      MXROGD = MAX ((EXCESS/ROWSIZ)-1, M)
C                                       grid no more than all rows
      MXROGD = MIN (MXROGD, NXO2+M)
C                                       Set AP pointers
      LOGRID = APSIZE - ((MXROGD+1) * ROWSIZ) - 1
C                                       Set AP locations for conv. fn.
      CY = LOGRID - 100 * N - 5
      CX = CY     - 100 * M - 1
C                                       AP index to beam sum of weights
      APSMWT = 21 + NCHAVG
C                                       UV data starts after weights
      UV = APSMWT + 2
C                                       Set AP data freq. increment.
      LVIS = INCF
      IF ((INCIF.GT.0) .AND. (INCIF.LT.INCF)) LVIS = INCIF
      LLVIS = LVIS
C                                       Find where weight is.
C                                       IWOFF + => Offset in rec.
C                                       IWOFF - => Offset in vis.
      IWOFF = 0
      CALL AXEFND (4, 'WEIG', KIPTPN, CATUVR(KHPTP), IWOFF, IERR)
C                                       Make Taper widths convenient
      TUC = 0.0
      TVC = 0.0
      IF (DOTAPE) THEN
         IF (TAPERU.GT.0.) TUC = LOG(.3) / (TAPERU * 1E3 /
     *      206265. * XXFLD) ** 2
         IF (TAPERV.GT.0.) TVC = LOG(.3) / (TAPERV * 1E3 /
     *      206265. * YYFLD) ** 2
         END IF
C                                       Shift the map center
C                                       here in the gridding routine.
C                                       ICENX and ICENY define the
C                                       pixel number of the map center.
      WX = (-TWOPI * (ICENX-1-NX/2)) / NX
      WY = (-TWOPI * (ICENY-1-NY/2)) / NY
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
C                                       Determine the maximum number
C                                       of visibility points which
C                                       fit in the AP.
      MAXREC = (CX - UV) / LREC - 5
      ILENBU = MIN (MAXREC, ILENBU)
C                                       Open UV files
      IF (DO3DIM) THEN
         CALL UVPROT ('READ', DISKI, CNOSCI, LUNUV, INDS2, DO3DIM,
     *      MFIELD, UMATS, 1.0, 0.0, NVIS, LREC, ILENBU, JBUFSZ, BUFF1,
     *      NIO, INPTR, FRSTU, OUTD, OUTC, IRET)
      ELSE
         CALL UVPREP ('READ', DISKI, CNOSCI, LUNUV, INDS2, NVIS, LREC,
     *      ILENBU, JBUFSZ, BUFF1, NIO, INPTR, FRSTU, IRET)
         END IF
      IF (IRET.NE.0) GO TO 999
C                                       Rescale FRSTU by max frequency
      FRSTU = FRSTU * MAX (FREQG(CHUV1), FREQG(CHUV1+NCHAVG-1)) / FREQUV
C                                       Determine Maximum U (in cells)
      MAXU  = INT ((ABS(FRSTU) * ZSCLU) + 2.)
C                                       limit U to max that will fit
      MAXU  = MIN (MAXU, NXO2)
C                                       calc number needed
      NROGRD = MAXU + M
C                                       do all rows fit in ap?
      ALLROW = (MXROGD.GE.NROGRD)
C                                       number of rows in a pass
      NROWS  = MIN(NROGRD,MXROGD)
C                                       if all rows fit, 1 pass
      IF (ALLROW) THEN
         NRPASS = 1
      ELSE
C                                       else, how many row passes?
         NRPASS = (MAXU + 1)/(NROWS - M + 1)
C                                       round up if not equal
         IF (NRPASS*(NROWS-M+1).LT.(MAXU+1)) NRPASS = NRPASS + 1
         END IF
C                                       Determine Number of chan Passes
      CALL UVAPAS (NCHAVG, MAXU, NROWS-M+1, FREQG(CHUV1), FREQUV, 0,
     *   NCPASS, LOCHAN, NCHGRD, FREQLO, FREQHI, BUFF2)
C                                       tell user rows in AP
      IF ((BEAM) .AND. (LFIELD.EQ.1)) THEN
         WRITE (MSGTXT,1010,ERR=100) NCPASS, NRPASS
         IF ((NCPASS.NE.1) .OR. (NRPASS.NE.1)) CALL MSGWRT (3)
         IF (ALLROW) THEN
            WRITE (MSGTXT,1020,ERR=100) NROWS, PSAPNW, MXROGD
         ELSE
            WRITE (MSGTXT,1030,ERR=100) NROWS, PSAPNW
            END IF
         CALL MSGWRT (3)
C                                       tell user max base line
         WRITE (MSGTXT,1035,ERR=100) FRSTU, MAXU
         CALL MSGWRT (3)
C                                       tell user channels
        IF (NCHAVG.GT.1) THEN
            WRITE (MSGTXT,1036,ERR=100) NCHAVG, FREQG(CHUV1),
     *         FREQG(CHUV1+NCHAVG-1)
         ELSE
            WRITE (MSGTXT,1037,ERR=100) FREQG(CHUV1)
            END IF
         CALL MSGWRT (3)
         END IF
C                                       jump on write error
 100  CONTINUE
C-----------------------------------------------------------------------
C                                       Clear grid.
      CALL QVCLR (LOGRID, 1, APSIZE-LOGRID-1)
      CALL QWAIT
C                                       Grid data.
C                                       Bandwidth synthesis loop
      DO 600 CPASS = 1,NCPASS
C                                       Determine Num Chan to average
         CALL UVAPAS (NCHAVG, MAXU, NROWS-M+1, FREQG(CHUV1), FREQUV,
     *      CPASS, NCPASS, LOCHAN, NCHGRD, FREQLO, FREQHI, BUFF2)
         MCHGRD = NCHGRD
C                                       Load frequency scaling table
         CALL QWAIT
         CALL QPUT (BUFF2, 20, MCHGRD, 2)
         IF (DOTAPE) MCHGRD = -MCHGRD
C                                       Offset VIS to freq channel.
         VSOFF = NRPARM + (LOCHAN+CHUV1-2) * LVIS
         VIS = UV + VSOFF
C                                       If COMPLEX axis more then
C                                       2 long Vis has weight.
C                                       weight is in random parameters
         IF (LVIS.LE.2) THEN
            WTOFF = IWOFF
C                                       Weight in vis, index to channel
         ELSE
            WTOFF = VSOFF + 2
            END IF
C                                       set index relative to AP buffer
         WT = UV + WTOFF
C                                       Create Convolving function
         CALL CONVFN (CX, CTYPX, XPARM, BUFF2)
         CALL CONVFN (CY, CTYPY, YPARM, BUFF2)
C                                       write needed rows
         FLIST(6,IFILE1) = NXO2+1
         FLIST(10,IFILE1) = FLIST(6,IFILE1)
C                                       Set freq. scaling factors
         FFRAC = (FREQG(LOCHAN+CHUV1-1) / FREQUV) - 1.0D0
         SSCLU = FFRAC * ZSCLU + ZSCLU
         GFACT = 1.0D0 / ((FREQLO/FREQUV) * ZSCLU)
C                                       Set limits.
C                                       Rescale baseline limits.
         BLMN2 = BLMN2O / ((1.0D0 + FFRAC) ** 2)
         BLMX2 = BLMX2O / ((1.0D0 + FFRAC) ** 2)
         UUMAX = UUMAXO / (1.0D0 + FFRAC)
         VVMAX = VVMAXO / (1.0D0 + FFRAC)
         UUMAXG = NXO2 * DU * (1. - GUARDB(1)) / (1.0D0 + FFRAC)
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
         CALL QWAIT
         CALL QPUT (APCONS, 0, 17, 2)
         CALL QPUT (APCONI, 17, 3, 1)
C                                       IF Second file is used
C                                       Switch files: 2=>1, 1=>2
         IF ((CPASS.EQ.1) .OR. (.NOT.ALLROW)) THEN
            IFILE1 = 3 - IFILE1
            IFILE2 = 3 - IFILE2
            END IF
C                                       for all row passes
         DO 500 RPASS = 1, NRPASS
C                                       Calc min and max U for pass
C                                       Must be room for M extra rows
            IUMAX = MAXU - (NROWS-M+1)*(RPASS-1)
            IUMIN = MAX(IUMAX - (NROWS-M), 0)
C                                       Init for next read.
            VREAD = VO
            CALL UVINIT ('READ', LUNUV, INDS2, NVIS, VO, LREC, ILENBU,
     *         JBUFSZ, BUFF1, BO, INPTR, IRET)
            IF (IRET.NE.0) THEN
               WRITE (MSGTXT,1060) IRET
               GO TO 995
               END IF
C                                       if not first row pass, shift
            IF (RPASS.GT.1) THEN
C                                       always move M-1=2*MO2 rows
               USTOP = IUMAX+MO2
               CALL APSHFT (MO2, USTOP, IUMIN-MO2, ROWSIZ, LOGRID,
     *           APSIZE)
               END IF
C                                       If not first BS Pass
            IF (CPASS.GT.1 .AND. (.NOT.ALLROW)) THEN
C                                       load rows not shifted
               USTRT = IUMIN-MO2
               USTOP = IUMAX-MO2
C                                       if first pass, load all rows
               IF (RPASS.EQ.1) USTOP = NXO2
C                                       if last pass, stop at U=-MO2
               IF (RPASS.EQ.NRPASS) USTRT = IUMIN - MO2
C                                       READ needed rows
               FLIST(6,IFILE2) = NXO2+1+MO2
               FLIST(10,IFILE2) = FLIST(6,IFILE2)
C                                       load previous values in AP
               CALL APLOAD (USTRT, USTOP, IUMIN-MO2, ROWSIZ, LOGRID,
     *            APSIZE, FLIST(1, IFILE2), BUFF3, IRET)
               IF (IRET.NE.0) GO TO 999
C                                       end if loading previous pass
               END IF
C                                       set min and max this pass
            UMAX = (IUMAX + 0.5) * GFACT
            UMIN = (IUMIN - 0.5) * GFACT
C                                       loop forever, read all UVdata
            DO 270 ILOOP = 1,1000000
C                                       read one UVdata Buffer
               CALL UVDISK ('READ', LUNUV, INDS2, BUFF1, NIO, INPTR,
     *            IRET)
               IF (IRET.NE.0) THEN
                  WRITE (MSGTXT,1100) IRET
                  GO TO 995
                  END IF
C                                       if no more data, exit
               IF (NIO.LE.0) GO TO 275
C                                       set UV buffer indicies
               IDATA = UV
               NPOINT = 0
               JNPTR = INPTR
               KNPTR = INPTR
C                                       for all vis points in buffer
               DO 260 IVIS = 1,NIO
                  IF (DO3DIM) CALL PRJMUL (1, BUFF1(JNPTR), UMAT,
     *               BUFF1(JNPTR))
C                                       input U value
                  UIN = ABS (BUFF1(JNPTR))
                  VIN = ABS (BUFF1(JNPTR+1))
                  GUAXAU = MAX (GUAXAU, UIN)
                  GUAXAV = MAX (GUAXAV, VIN)
C                                       If datum on current rows
                  IF ((UIN.GT.UMIN) .AND. (UIN.LE.UMAX)) THEN
C                                       calc baseline length squared
                     BLEN = UIN*UIN + VIN*VIN
C                                       Check if uv in range.
                     IF ((BLEN.GE.BLMN2) .AND. (BLEN.LE.BLMX2)) THEN
C                                       If inside guard band
                        IF ((UIN.LE.UUMAXG) .AND. (VIN.LE.VVMAXG)) THEN
C                                       if making the synthesized beam
C                                       BEAM replace data with (1,0)
                           IF (BEAM) CALL UVSETB (VSOFF, LVIS, UMAX,
     *                        UMIN, SSCLU, NCHGRD, BUFF1(JNPTR))
C                                       Check if U neg.
C                                       Flip U, V, W, conjugate data.
                           IF (BUFF1(JNPTR).LT.0.0) CALL UVCONJ (VSOFF,
     *                        LVIS, NCHGRD, 1, 2, BUFF1(JNPTR))
C                                       count points, update pointer
                           NPOINT = NPOINT + 1
C                                       if not same point, copy
                           IF (KNPTR.NE.JNPTR) CALL RCOPY (LREC,
     *                        BUFF1(JNPTR), BUFF1(KNPTR))
C                                       update output pointer
                           KNPTR  = KNPTR + LREC
                           GUAXBU = MAX (GUAXBU, UIN)
                           GUAXBV = MAX (GUAXBV, VIN)
                           GUACNT = GUACNT + 1
                           END IF
C                                       Error: outside UVRANGE
                     ELSE
                        CALL UVLIMT (SQRT(BLMN2O), SQRT(BLMX2O), UUMAXO,
     *                     VVMAXO, IVIS, UIN*(1.+FFRAC), VIN*(1.+FFRAC),
     *                     ERRCNT)
                        END IF
C                                       end if in current rows
                     END IF
                  JNPTR = JNPTR + LREC
C                                       end for all vis loop
 260              CONTINUE
C                                       if some good points found
               IF (NPOINT.GT.0) THEN
C                                       Load into AP.
                  CALL QWR
C                                       Keep track of number of vis read
                  VREAD = VREAD + NPOINT
                  ITEMP = LREC * NPOINT
                  CALL QPUT (BUFF1(INPTR), IDATA, ITEMP, 2)
                  IDATA = IDATA + ITEMP
                  CALL QWAIT
                  END IF
C                                       Do zero spacing flux densities
               IF ((DOZERO) .AND. (.NOT.ONZE) .AND. (IUMIN.LE.0)) THEN
C                                       for cells around u=v=0
                  DO 267 I = -1,1
C                                       for V = -? to ? lambdas
                     VIN = I * GFACT
                     DO 265 J = 0,1
C                                       scale cell to UV lambdas
                        UIN = J * GFACT
C                                       calc zerospacing weight
                        CALL UVZRWT (UIN, VIN, BEAM, ZEROSP, TFLUXG,
     *                     FLUXZ, WEIGHT)
C                                       If significant Weight
                        IF (WEIGHT.GT.1.0E-6) THEN
C                                       put zerospacing in UV data
                           CALL UVSETZ (UIN, VIN, WTOFF, VSOFF, LVIS,
     *                        FLUXZ, WEIGHT, NCHGRD, BUFF1)
                           CALL QWR
C                                       put in zerosp in ap
                           CALL QPUT (BUFF1(1), IDATA, LREC, 2)
C                                       count points
                           NPOINT = NPOINT + 1
                           IDATA = IDATA + LREC
                           CALL QWAIT
                           END IF
 265                    CONTINUE
 267                 CONTINUE
                  ONZE = T
C                                       end if do zero spacing flux
                  END IF
C                                       if some good points found
               IF (NPOINT.GT.0) THEN
C                                       Set GRID in AP MD(20)
                  APCONI(3) = LOGRID
C                                       Put row number in AP MD(8)
                  APCONS(9) = IUMIN
                  CALL QPUT (APCONS(9), 8, 1, 2)
                  CALL QPUT (APCONI, 17, 3, 1)
                  CALL QWAIT
                  ITEMP = - NPOINT
                  IF (DOSHFT) ITEMP = -ITEMP
                  CALL Q1GRD (UV ,VIS, WT, LLREC, LLVIS, NO2, M,
     *               LROW, ITEMP, MCHGRD)
                  CALL QWR
C                                       end if points found
                  END IF
C                                       All UV data finished, process.
  270          CONTINUE
C                                       Jump here on end of UV data
  275       CONTINUE
C                                       calc AP rows to sum/write
            USTRT = IUMIN + MO2
C                                       If last pass, sum negative rows
            IF (RPASS.EQ.NRPASS) USTRT = -MO2
            USTOP = IUMAX + MO2
C                                       If sum weights and last chan.
            IF (BEAM .AND. (CPASS.EQ.NCPASS)) THEN
               CALL APSUM (USTRT, USTOP, IUMIN-MO2, ROWSIZ, LOGRID,
     *            APSMWT, LROW, ONESUM)
C                                       double sum for conjugation
               SUMWT = SUMWT + (2 * ONESUM)
C                                       end if beam grid done
               END IF
C                                       if first pass stop at top rows
            IF (RPASS.EQ.1) USTOP = NXO2
C                                       if very last pass start at u=0
            IF (NRPASS.EQ.RPASS) USTRT = 0
C                                       if last channel pass
            IF (CPASS.GE.NCPASS) THEN
               CALL APFNSH (USTRT, USTOP, IUMIN-MO2, MAXU, MO2, ROWSIZ,
     *            LROW, LOGRID, APSIZE, WX)
               END IF
C                                       if time to write
            IF (CPASS.EQ.NCPASS .OR. (.NOT. ALLROW)) THEN
C                                       if final pass
               IF (CPASS.EQ.NCPASS) THEN
C                                       Write only from NXO2 to 0
                  FLIST(6,IFILE1) = NXO2+1
               ELSE
C                                       Else write from NXO2 to - MO2
                  FLIST(6,IFILE1) = NXO2+1+MO2
                  IF (NRPASS.EQ.RPASS) USTRT = -MO2
                  END IF
               FLIST(10,IFILE1) = FLIST(6,IFILE1)
C                                       write out finished rows
               CALL APWRIT (USTRT, USTOP, IUMIN-MO2, ROWSIZ, LOGRID,
     *            FLIST(1,IFILE1), APSIZE, BUFF2, IRET)
               IF (IRET.NE.0) GO TO 999
               END IF
C                                       tell progress
            IF ((BEAM) .AND. (LFIELD.EQ.1)) THEN
               WRITE (MSGTXT,1300) CPASS, RPASS, IUMAX, IUMIN, VREAD
               CALL MSGWRT (4)
               END IF
C                                       End Rows Loop
 500        CONTINUE
C                                       if all rows not in AP
      IF ((CPASS.LT.NCPASS) .AND. (.NOT.ALLROW)) THEN
         CALL QWAIT
C                                       at end, Flush buffer with 'INIT'
         CALL APIO ('INIT', FLIST(1, IFILE1), LOGRID, BUFF2, IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1210) IRET, U
            GO TO 995
            END IF
         END IF
C                                       End Band. Synth. (BS) loop
 600     CONTINUE
      CALL QWAIT
C                                       Give up AP
      CALL QRLSE
C                                       Close files
      CALL ZCLOSE (LUNUV, INDS2, IRET)
      CALL APIO ('CLOS', FLIST(1,IFILE1), LOGRID, BUFF2, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1340) IFILE1, IRET
         GO TO 995
         END IF
      CALL APIO ('CLOS', FLIST(1,IFILE2), LOGRID, BUFF3, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1330) IFILE2, IRET
         GO TO 995
         END IF
C                                       Set sum of weights
      IF (BEAM) THEN
         BEMMAX(LFIELD) = SUMWT
         WRITE (MSGTXT,1350) LFIELD, BEMMAX(LFIELD)
         CALL MSGWRT (4)
         END IF
C                                       Must switch SCRWRK and SCROUT
      IF (IFILE1.NE.1) THEN
         INDEX = SCRWRK
         SCRWRK = SCROUT
         SCROUT = INDEX
         END IF
      GO TO 999
C                                       Error
 995  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1010 FORMAT ('UVGRID: ',I4,' Channel Pass(es);',I4,' Row Pass(es)')
 1020 FORMAT ('UVGRID: All  ',I5,' Rows in',I6,'k AP (',I5,' Rows Max)')
 1030 FORMAT ('UVGRID: Only ',I5,' Rows in',I6,'k AP')
 1035 FORMAT ('UVGRID: Max U Baseline ',F12.0,' lambda (=',I5,' cells)')
 1036 FORMAT ('UVGRID: Ave',I5,' Channels; ',1PE12.6,' to ',E12.6,' Hz')
 1037 FORMAT ('UVGRID: Frequency ',1PE12.6,' Hz')
 1060 FORMAT ('UVGRID: ERROR',I3,' INITILIZING FILE')
 1100 FORMAT ('UVGRID: READ ERROR IN VISIBILITY FILE, IER=',I3)
 1210 FORMAT ('UVGRID: ERROR',I3,' WRITING GRID ROW ',I5)
 1220 FORMAT ('UVGRID: MIN,MAX R=',2F12.0,', MAX U,V=',2F12.0)
 1300 FORMAT ('UVGRID: End Pass ',I3,',',I3,': Rows',I5,'-',I4,
     *        ', with',I8,' Pts')
 1330 FORMAT ('UVGRID: TROUBLE CLOSING OUTPUT FILE',I3,', IER=',I3)
 1340 FORMAT ('UVGRID: TROUBLE CLOSING INPUT  FILE',I3,', IER=',I3)
 1350 FORMAT ('Field',I5,' Sum of gridding weights = ',1PE12.5)
      END
