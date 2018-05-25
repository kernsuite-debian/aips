      SUBROUTINE GR3DUV (IFIELD, DISKI, CNOSCI, SCROUT, CATUVR, JBUFSZ,
     *   FREQID, BUFF1, BUFF2, IRET)
C-----------------------------------------------------------------------
C! Grids uv data to be FFTed, allows partial mis-sort after reprojection
C# AP-util UV
C-----------------------------------------------------------------------
C;  Copyright (C) 1997, 1999-2000, 2006-2009, 2017
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
C   GR3DUV convolves uv data onto a grid using AP routines.  It differs
C   from older gridding subroutines in that it expects the data to be
c   only roughly sorted (after re-projecting uvw to the current field)
C   rather than exactly sorted as assumed by the older routines.  (Sort
C   is irrelevant if the grid fits in the AP of course.)
C   The visibilities are convolved onto the grid using the convolving
C   function specified by CTYPX,CTYPY,XPARM,YPARM.  The defaults for
C   these values must be filled in by a call to GRDFLT.
C   The gridded data is phase rotated so that the map center comes out
C   at location ICNTRX,ICNTRY.  If requested, a uv taper is applied to
C   the visibility weights before gridding.  If necessary, a three
C   dimension phase reference position shift is done in AP1GRD.
C   If multiple channels are to be gridded together, GR3DUV loops
C   over the frequency channels in an inner loop, reading the grid only
C   onces and the uv data only a couple of times at most.  For bandwidth
C   synthesis the SCROUT file should be big enough for an extra m rows,
C   where m is the half width of the X convolving function.
C      This version uses a scrolling buffer as large as possible using
C   both the primary and secondary AP memory; 1/4 of the memory is used
C   as the uv data buffer when possible.
C      Zero spacing flux densities are gridded if provided.
C   Uses AIPS LUNs 18, 20
C   Input uv data file in UV file CNOSCI.
C   Output grid file in image file SCROUT.
C   Inputs:
C      IFIELD      I     Field number to grid, if 0 then grid a beam.
C      DISKI       I     Input file disk number for catalogd files,
C                        .LE. 0 => /CFILES/ scratch file.
C      CNOSCI      I     Input file catalog slot number or /CFILES/
C                        scratch file number.
C      SCROUT      I     /CFILES/ scratch file number of output file
C      CATUVR(256) R     UV data catalog header record.
C      JBUFSZ      I     Size in bytes of buffers. Dimension of
C                        BUFF1,2,3  must be at least 4096 words
C      FREQID      I     Id number of FQ entry, if it exists.
C   From commons: (Includes DGDS, DMPR, DUVH, CGDS, CMPR, CUVH)
C      NVIS         I    Number of visibility records (/UVHDR/)
C      LREC         I    Number of (real) words per visibility record
C                        (/UVHDR/)
C      NCHAVG       I    Number of frequency channels to grid together.
C      FLDSZ(2,*)   I    Dimension of map in RA, Dec (cells)
C      CELLSG(2)    R    The cell spacing in X and Y in arcseconds.
C      DO3DIM       L    Do 3d rotation of uvw's as well as phases.
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
C   GR3DUV will still do zero-spacing insertion.  This is not
C   recommended.  Do it in copying the uv data to start with.
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
C      UVROT, MAPROT        Rotations (usually = here)
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
C     be called before calling GR3DUV.
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
C     GR3DUV must be called to grid the sampling function IRREGARDLESS
C     of whether or not a beam will be produced.
C    7) The gridding convolution function parameters must be completely
C     specified.  The defaults should be filled in by a call to
C     GRDFLT before calling GR3DUV.
C    8) Multiple IFs can be processed using the common frequency table
C     FREQG
C    9) No sort order is assumed, but the data must be in approximate XY
C     order if the grid is too large to fit in the AP otherwise.  The
C     routine will die if there is a failure to fit for any reason.
C-----------------------------------------------------------------------
      INTEGER   IFIELD, DISKI, CNOSCI, SCROUT, JBUFSZ, FREQID, IRET
      REAL      CATUVR(256), BUFF1(*), BUFF2(*)
C
      INTEGER   VO, BO, VREAD, LLVIS, IFACT, IERR, KNPTR, JNPTR,
     *   NIO, NXO2, ILENBU, ICENX, ICENY, NX, NY, WIN(4), LOCS, KEYTYP,
     *   VSOFF, LVIS, WTOFF, I1, I2, END1, END2, IROUND, IWOFF, MXROGD,
     *   NROGRD, KAP, CY, CX, VIS, WT, IVIS, I, UV, LUV, INCNT, CNT,
     *   ITEMP, LLREC, N, NO2, M, MO2, LROW, ROWSIZ, MCHGRD, NROWS,
     *   MAXREC, SIZEAP, APSMWT, EXCESS, LOGRID, USTRT, USTOP, UCOUNT,
     *   MAXU, APCONI(3), UHI, ULO, VOL(2), FILCNO(2), LUN(2), IND(2),
     *   BIND(2), IU, LFIELD, VLAST, NFAIL, MAXVIS, I2FMAX, NEED
      LOGICAL   T, F, DOSHFT, BEAM, ALLROW, DONE
      REAL      APCONS(20), UMIN, UMAX, UIN, VIN, WEIGHT, FLUXZ, TUC,
     *   TVC, SSCLU, FRSTU, UUMAXO, VVMAXO, BLMN2O, BLMX2O, ZSCLU,
     *   ZSCLV, ZSCLW, XXFLD, YYFLD, DDXS(3), FFRAC, SUMWT,
     *   ONESUM, BLEN, BLMN2, BLMX2, UUMAX, VVMAX, DU, DV, UUMAXG,
     *   VVMAXG, TEMP, UU, UMAT(3,3), PMAT(3,3)
      DOUBLE PRECISION   WX, WY, GFACT, FREQLO, FREQHI, XRA, XDEC
      CHARACTER PHNAME*48
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
      DATA T, F /.TRUE.,.FALSE./
      DATA LUN /18, 20/
C-----------------------------------------------------------------------
      IRET = 0
      BO = 1
      VO = 0
      VREAD = 0
      SUMWT = 0.0
      UCOUNT = 0
      IF ((GUARDB(1).LT.0.0) .OR. (GUARDB(1).GT.0.9)) GUARDB(1) = 0.0
      IF ((GUARDB(2).LT.0.0) .OR. (GUARDB(2).GT.0.9)) GUARDB(2) = 0.0
      GUAXAU = 0.
      GUAXAV = 0.
      GUAXBU = 0.
      GUAXBV = 0.
      GUACNT = 0
C                                       Set parameters for beam/map.
      BEAM = IFIELD.LE.0
      LFIELD = MAX (1, ABS(IFIELD))
C                                       Get field info. if nec.
      IF (.NOT.NGRDAT) THEN
         CALL GRDAT (T, IFIELD, CATUVR, BUFF1, IRET)
         IF (IRET.NE.0) GO TO 999
         END IF
C                                       Fill channel table.
      IF ((IFIELD.LT.0) .AND. (LFIELD.EQ.1)) THEN
         CALL FRQTAB (DISKI, CNOSCI, LUN, CATUVR, FREQID, BUFF1, IRET)
         IF (IRET.NE.0) GO TO 999
         END IF
      CALL XYSHFT (RA, DEC, XSHIFT(LFIELD), YSHIFT(LFIELD), MAPROT,
     *      XRA, XDEC)
      IF (DO3DIM) THEN
         CALL PRJMAT (RA, DEC, UVROT, XRA, XDEC, MAPROT, UMAT, PMAT)
      ELSE
         CALL P2DMAT (RA, DEC, UVROT, XRA, XDEC, MAPROT, UMAT, PMAT)
         END IF
C                                       Max, min Freq
      FREQHI = FREQG(CHUV1)
      FREQLO = FREQG(CHUV1)
      DO 10 I = 1,NCHAVG
         FREQHI = MAX (FREQHI, FREQG(CHUV1+I-1))
         FREQLO = MIN (FREQLO, FREQG(CHUV1+I-1))
 10      CONTINUE
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
         CALL PRJMUL (2, DDXS, PMAT, DDXS)
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
C                                       Setup I/O:
C                                       Open UV data file IO
      IF (DISKI.GT.0) THEN
         VOL(1) = DISKI
         FILCNO(1) = CNOSCI
         CALL ZPHFIL ('UV', VOL(1), FILCNO(1), 1, PHNAME, IERR)
      ELSE
         VOL(1) = SCRVOL(CNOSCI)
         FILCNO(1) = SCRCNO(CNOSCI)
         CALL ZPHFIL ('SC', VOL(1), FILCNO(1), 1, PHNAME, IERR)
         END IF
      CALL ZOPEN (LUN(1), IND(1), VOL(1), PHNAME, T, F, T, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1000) IRET, 'OPEN', 'UV data scratch file'
         GO TO 990
         END IF
C                                       Open output grid (SC always)
      VOL(2) = SCRVOL(SCROUT)
      FILCNO(2) = SCRCNO(SCROUT)
      CALL ZPHFIL ('SC', VOL(2), FILCNO(2), 1, PHNAME, IERR)
      CALL ZOPEN (LUN(2), IND(2), VOL(2), PHNAME, T, T, T, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1000) IRET, 'OPEN', 'Grid file'
         GO TO 990
         END IF
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
C                                       Set window - write backwards
      WIN(1) = 1
      WIN(2) = NXO2 + 1
      WIN(3) = ROWSIZ
      WIN(4) = 1
C                                       Determine no. rows in AP -
C                                       any excess over allowing 1/4 for
C                                       data buffer goes to extra rows.
      MAXREC = 256
      NEED = MAXREC * LREC + 30 + NCHAVG + (100 * (N + M)) +
     *   (NXO2+M) + ROWSIZ
      NEED = NEED / 1024
      CALL QINIT (NEED, 0, KAP)
      IF ((KAP.EQ.0) .OR. (PSAPNW.EQ.0)) THEN
         NEED = MAXREC * LREC + 30 + NCHAVG + (100 * (N + M)) +
     *      (NXO2/10+M) + ROWSIZ
         NEED = NEED / 1024
         CALL QINIT (NEED, 0, KAP)
         IF ((KAP.EQ.0) .OR. (PSAPNW.EQ.0)) THEN
            IRET = 8
            MSGTXT = 'GR3DUV CANNOT GET DESIRED MEMORY'
            CALL MSGWRT (8)
            GO TO 999
            END IF
         END IF
      SIZEAP = PSAPNW * 1024
      MAXREC = MIN (2048, (SIZEAP/40)/LREC) + 5
      EXCESS = SIZEAP - MAXREC * LREC - 30 - NCHAVG - (100 * (N + M))
      MXROGD = EXCESS / ROWSIZ - 1
      IF (MXROGD.LT.M) THEN
         IRET = 8
         MSGTXT = 'GR3DUV CANNOT GET ENOUGH MEMORY'
         CALL MSGWRT (8)
         GO TO 999
         END IF
C                                       grid no more than all rows
      MXROGD = MIN (MXROGD, NXO2+M)
C                                       Set AP pointers
      LOGRID = SIZEAP - ((MXROGD+1) * ROWSIZ) - 1
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
      APCONI(1) = CX
      APCONI(2) = CY
      APCONI(3) = LOGRID
C                                       Determine the maximum number
C                                       of visibility points which
C                                       fit in the AP.
      MAXREC = (CX - UV) / LREC - 5
      ILENBU = MIN (MAXREC, ILENBU)
C                                       get the max U
      FRSTU = -1.0E3
      CALL CATKEY ('REED', VOL(1), FILCNO(1), 'MAXBLINE', 1, LOCS,
     *   FRSTU, KEYTYP, BUFF1, IERR)
      IF (IERR.NE.0) FRSTU = -1.E3
      IF (FRSTU.LE.0.0) FRSTU = NXO2 / ZSCLU
C                                       Rescale FRSTU by max frequency
      ULO = FRSTU * FREQLO / FREQUV * ZSCLU
      UHI = FRSTU * FREQHI / FREQUV * ZSCLU + 2.0
      IF ((UHI.GT.NXO2) .OR. (UHI.LE.0.0)) THEN
         NROWS = NXO2 * (FREQHI - FREQLO) / FREQUV + M + 0.99
      ELSE
         NROWS = UHI - ULO + M
         END IF
      IF (NROWS.GE.MXROGD) THEN
         WRITE (MSGTXT,1010) NROWS, MXROGD
         CALL MSGWRT (8)
         IRET = 8
         GO TO 999
         END IF
      FRSTU = FRSTU * FREQHI / FREQUV
C                                       Determine Maximum U (in cells)
      MAXU = INT ((ABS(FRSTU) * ZSCLU) + 2.)
      UHI = MAXU + MO2 + 1
      UHI = MIN (UHI, NXO2)
C                                       limit U to max that will fit
      MAXU = MIN (MAXU, NXO2)
C                                       calc number needed
      NROGRD = MAXU + M + 2
C                                       do all rows fit in ap?
      ALLROW = (MXROGD.GE.NROGRD)
C                                       number of rows at a time
      NROWS = MIN (NROGRD, MXROGD)
      ULO = UHI - NROWS
      ULO = MAX (ULO, -MO2-1)
      NROWS = UHI - ULO + 1
C                                       tell user rows in AP
      IF ((BEAM) .AND. (LFIELD.EQ.1)) THEN
         IF (ALLROW) THEN
            WRITE (MSGTXT,1020,ERR=20) NROWS, PSAPNW, MXROGD
         ELSE
            WRITE (MSGTXT,1030,ERR=20) NROWS, PSAPNW
            IF (ISORT(1:1).NE.'X') THEN
               CALL MSGWRT (8)
               MSGTXT = 'GR3DUV: DATA MUST BE XY SORTED, NOT ' // ISORT
               GO TO 990
               END IF
            END IF
         CALL MSGWRT (2)
C                                       tell user max base line
         WRITE (MSGTXT,1035,ERR=20) FRSTU, MAXU
         CALL MSGWRT (2)
C                                       tell user channels
         IF (NCHAVG.GT.1) THEN
            WRITE (MSGTXT,1036,ERR=20) NCHAVG, FREQLO, FREQHI
         ELSE
            WRITE (MSGTXT,1037,ERR=20) FREQG(CHUV1)
            END IF
         CALL MSGWRT (2)
         END IF
C                                       jump on write error
C                                       Clear grid.
 20   CALL QVCLR (LOGRID, 1, SIZEAP-LOGRID-1)
      CALL QWAIT
C                                       freq scaling array
      MCHGRD = NCHAVG
      IF (NCHAVG.GT.1) THEN
         DO 25 I = 2,NCHAVG
            BUFF2(I-1) = 0.0
            IF (FREQG(I-1).GT.0.0D0) BUFF2(I-1) = FREQG(I) / FREQG(I-1)
     *         - 1.0D0
 25         CONTINUE
         CALL QWAIT
         CALL QPUT (BUFF2, 20, MCHGRD, 2)
         END IF
C                                       Grid data.
      IF (DOTAPE) MCHGRD = -MCHGRD
C                                       Offset VIS to freq channel.
      VSOFF = NRPARM + (CHUV1-1) * LVIS
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
C                                       Set freq. scaling factors
      FFRAC = (FREQG(CHUV1) / FREQUV) - 1.0D0
      SSCLU = FFRAC * ZSCLU + ZSCLU
      GFACT = 1.0D0 / ((FREQLO/FREQUV) * ZSCLU)
      UMAX = (UHI + 0.5) * GFACT
      UMIN = (ULO - 0.5) * GFACT
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
C                                       Init for write
      CALL MINIT ('WRIT', LUN(2), IND(2), ROWSIZ, NXO2+1, WIN, BUFF2,
     *   JBUFSZ, BO, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1000) IRET, 'INIT', 'WRITE GRIDDED DATA'
         GO TO 990
         END IF
      IF (NXO2.GT.UHI) THEN
         N = JBUFSZ / 2
         CALL RFILL (N, 0.0, BUFF2)
         N = NXO2 - UHI
         DO 90 I = 1,N
            CALL MDISK ('WRIT', LUN(2), IND(2), BUFF2, BIND(2), IRET)
            IF (IRET.NE.0) THEN
               WRITE (MSGTXT,1000) IRET, 'WRIT', 'GRIDDED DATA'
               GO TO 990
               END IF
 90         CONTINUE
         END IF
      LUV = UV
      INCNT = 0
      CNT = 0
      VLAST = 0
C                                       Init for UV read.
 95   VO = VLAST
      MAXVIS = NVIS - VO
      CALL UVINIT ('READ', LUN(1), IND(1), MAXVIS, VO, LREC, ILENBU,
     *   JBUFSZ, BUFF1, BO, BIND(1), IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1000) IRET, 'INIT', 'UV DATA'
         GO TO 990
         END IF
      NFAIL = 0
      VLAST = NVIS + 1
      I2FMAX = -MO2 - 1
C                                       read one UVdata Buffer
 100  CALL UVDISK ('READ', LUN(1), IND(1), BUFF1, NIO, BIND(1), IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1000) IRET, 'READ', 'UV DATA'
         GO TO 990
         END IF
C                                       if more data
      IF (NIO.GT.0) THEN
C                                       set UV buffer indicies
         JNPTR = BIND(1)
         KNPTR = BIND(1)
C                                       for all vis points in buffer
         DO 150 IVIS = 1,NIO
            VO = VO + 1
C                                       input U value
            CALL PRJMUL (1, BUFF1(JNPTR), UMAT, BUFF1(JNPTR))
            UIN = ABS (BUFF1(JNPTR))
            VIN = ABS (BUFF1(JNPTR+1))
            GUAXAU = MAX (GUAXAU, UIN)
            GUAXAV = MAX (GUAXAV, VIN)
            UU = UIN * ZSCLU
            TEMP = UU * FREQHI / FREQUV + 1.0
            I2 = IROUND (TEMP) + MO2
            TEMP = UU * FREQLO / FREQUV - 1.0
            I1 = IROUND (TEMP) - MO2
C                                       not in current buffer
            IF (I1.LT.ULO) THEN
               VLAST = MIN (VLAST, VO-1)
               I2FMAX = MAX (I2FMAX, I2)
               NFAIL = NFAIL + 1
               IF (NFAIL.GT.1000) GO TO 160
C                                       is in current buffer
            ELSE IF (I2.LE.UHI) THEN
               NFAIL = 0
C                                       calc baseline length squared
               BLEN = UIN*UIN + VIN*VIN
C                                       Check if uv in range.
               IF ((BLEN.GE.BLMN2) .AND. (BLEN.LE.BLMX2)) THEN
C                                       If inside guard band
                  IF ((UIN.LE.UUMAXG) .AND. (VIN.LE.VVMAXG)) THEN
C                                       if making the synthesized beam
C                                       BEAM replace data with (1,0)
                     IF (BEAM) CALL UVSETB (VSOFF, LVIS, UMAX, UMIN,
     *                  SSCLU, NCHAVG, BUFF1(JNPTR))
C                                       Flip U, V, W, conjugate data.
                     IF (BUFF1(JNPTR).LT.0.0) CALL UVCONJ (VSOFF, LVIS,
     *                  NCHAVG, 1, 2, BUFF1(JNPTR))
C                                       count points, update pointer
                     CNT = CNT + 1
C                                       if not same point, copy
                     IF (KNPTR.NE.JNPTR) CALL RCOPY (LREC, BUFF1(JNPTR),
     *                  BUFF1(KNPTR))
C                                       update output pointer
                     KNPTR  = KNPTR + LREC
                     GUAXBU = MAX (GUAXBU, UIN)
                     GUAXBV = MAX (GUAXBV, VIN)
                     GUACNT = GUACNT + 1
                     END IF
                  END IF
               END IF
            JNPTR = JNPTR + LREC
C                                       does this fill the AP?
            IF (CNT+INCNT.GE.MAXREC) THEN
C                                       load any data
               IF (CNT.GT.0) THEN
                  N = LREC * CNT
                  VREAD = VREAD + CNT
                  CALL QPUT (BUFF1(KNPTR-N), LUV, N, 2)
                  CALL QWD
                  INCNT = INCNT + CNT
                  LUV = LUV + N
                  CNT = 0
                  KNPTR = BIND(1)
                  END IF
C                                       grid loaded data
               IF (INCNT.GT.0) THEN
C                                       Put row number in AP MD(8)
                  APCONS(9) = ULO + MO2
                  CALL QPUT (APCONS(9), 8, 1, 2)
                  CALL QWAIT
                  ITEMP = -INCNT
                  IF (DOSHFT) ITEMP = -ITEMP
                  CALL Q1GRD (UV ,VIS, WT, LLREC, LLVIS, NO2, M, LROW,
     *               ITEMP, MCHGRD)
                  CALL QWR
                  INCNT = 0
                  LUV = UV
                  END IF
               END IF
 150        CONTINUE
C                                       if some good points found
         IF (CNT.GT.0) THEN
C                                       Load into AP.
            CALL QWR
            VREAD = VREAD + CNT
            N = LREC * CNT
            CALL QPUT (BUFF1(BIND(1)), LUV, N, 2)
            LUV = LUV + N
            INCNT = INCNT + CNT
            CALL QWAIT
            CNT = 0
            END IF
C                                       loop for data
         GO TO 100
         END IF
C                                       need to finish part of grid
 160  DONE = (ULO.LE.(-MO2)) .OR. (VLAST.GT.NVIS)
C                                       load any data
      IF (CNT.GT.0) THEN
         N = LREC * CNT
         VREAD = VREAD + CNT
         CALL QPUT (BUFF1(KNPTR-N), LUV, N, 2)
         CALL QWD
         LUV = LUV + N
         INCNT = INCNT + CNT
         CNT = 0
         KNPTR = BIND(1)
         END IF
C                                       Do zero spacing flux densities
      IF ((DONE) .AND. (DOZERO)) THEN
         VIN = 0.0
         UIN = 0.0
C                                       calc zerospacing weight
         IF (BEAM) THEN
            FLUXZ = 1.
         ELSE
            FLUXZ = ZEROSP(1) - TFLUXG
            END IF
         WEIGHT = ZEROSP(5)
         CALL UVZRSP (WTOFF, VSOFF, LVIS, FLUXZ, WEIGHT, NCHAVG, BUFF1)
         CNT = CNT + 1
         VREAD = VREAD + CNT
         N = LREC * CNT
         CALL QPUT (BUFF1, LUV, N, 2)
         LUV = LUV + N
         INCNT = INCNT + CNT
         CALL QWAIT
         END IF
C                                       grid loaded data
      IF (INCNT.GT.0) THEN
C                                       Put row number in AP MD(8)
         APCONS(9) = ULO + MO2
         CALL QPUT (APCONS(9), 8, 1, 2)
         CALL QWAIT
         ITEMP = -INCNT
         IF (DOSHFT) ITEMP = -ITEMP
         CALL Q1GRD (UV ,VIS, WT, LLREC, LLVIS, NO2, M, LROW, ITEMP,
     *      MCHGRD)
         CALL QWR
         INCNT = 0
         LUV = UV
         END IF
C                                       Finish computations
      IF (DONE) THEN
         USTRT = -MO2
      ELSE
         USTRT = MAX (ULO, I2FMAX+1)
         END IF
      USTOP = UHI
      IF (BEAM) THEN
         CALL APSUM (USTRT, USTOP, ULO, ROWSIZ, LOGRID, APSMWT, LROW,
     *      ONESUM)
         SUMWT = SUMWT + (2 * ONESUM)
         END IF
      CALL APFNSH (USTRT, USTOP, ULO, MAXU, MO2, ROWSIZ, LROW, LOGRID,
     *   SIZEAP, WX)
      N = USTOP - MAX (-1, I2FMAX)
      DO 165 I = 1,N
         IU = UHI + 1 - I
         CALL MDISK ('WRIT', LUN(2), IND(2), BUFF2, BIND(2), IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1000) IRET, 'WRIT', 'GRIDDED DATA'
            GO TO 990
            END IF
         IF (IU.GE.ULO) THEN
            END1 = LOGRID + (IU - ULO) * ROWSIZ
            CALL QGET (BUFF2(BIND(2)), END1, ROWSIZ, 2)
         ELSE
            CALL RFILL (ROWSIZ, 0.0, BUFF2(BIND(2)))
            END IF
 165     CONTINUE
      CALL QWAIT
C                                       move remaining grid up
      IF (.NOT.DONE) THEN
         END2 = (UHI - ULO + 1) * ROWSIZ - 1 + LOGRID
         IF (I2FMAX.GE.ULO) THEN
            N = (I2FMAX - ULO + 1) * ROWSIZ
            END1 = LOGRID + N - 1
            CALL QVMOV (END1, -1, END2, -1, N)
         ELSE
            N = 0
            END IF
C                                       and zero new part
         END2 = END2 - N - LOGRID + 1
         CALL QVCLR (LOGRID, 1, END2)
         UHI = I2FMAX
         ULO = UHI - NROWS + 1
         UMAX = (UHI + 0.5) * GFACT
         UMIN = (ULO - 0.5) * GFACT
         GO TO 95
         END IF
C                                       Give up AP
      CALL QRLSE
C                                       Close files
      CALL ZCLOSE (LUN(1), IND(1), IERR)
      CALL MDISK ('FINI', LUN(2), IND(2), BUFF2, BIND(2), IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1000) IRET, 'FINISH', 'GRIDDED DATA'
         GO TO 990
         END IF
      IRET = IERR
      CALL ZCLOSE (LUN(2), IND(2), IRET)
C                                       Set sum of weights
      IF (BEAM) THEN
         BEMMAX(LFIELD) = SUMWT
         WRITE (MSGTXT,1170) LFIELD, BEMMAX(LFIELD)
         CALL MSGWRT (4)
         END IF
      GO TO 999
C                                       Error
 990  CALL MSGWRT (8)
      IRET = MAX (1, IRET)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('GR3DUV ERROR',I4,1X,A,'ING ',A)
 1010 FORMAT ('GR3DUV: REQUIRES',I5,' ROWS, BUT ONLY',I5,' WILL FIT')
 1020 FORMAT ('GR3DUV: All  ',I5,' Rows in',I6,'k AP (',I5,' Rows Max)')
 1030 FORMAT ('GR3DUV: Only ',I5,' Rows in',I6,'k AP')
 1035 FORMAT ('GR3DUV: Max U Baseline ',F12.0,' lambda (=',I5,' cells)')
 1036 FORMAT ('GR3DUV: Ave',I5,' Channels:',1PE13.6,' to',E13.6,' Hz')
 1037 FORMAT ('GR3DUV: Frequency',1PE13.6,' Hz')
 1100 FORMAT ('GR3DUV: SORT ERROR: ROWS',2I6,' OUTSIDE',2I6)
 1170 FORMAT ('Field',I5,' Sum of gridding weights =',1PE132.5)
      END
