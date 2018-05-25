      SUBROUTINE UVTBUW (DISKI, CNOSCI, DISKO, CNOSCO, CATUVR, JBUFSZ,
     *   BUFF1, BUFF2, IRET)
C-----------------------------------------------------------------------
C! Determines and applies uniform weighting (by weight) to uv data.
C# AP-util UV
C-----------------------------------------------------------------------
C;  Copyright (C) 1995, 1997, 2000, 2006, 2012, 2015
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
C   UVTBUW computes uniform weighting corrections and applies them to
C   the weights in the visibility data base.  The corrections are done
C   by summing the weights of the data in a box rather than the counts
C   of the visibilities.
C      The visibility weights are divided by the number of visibilities
C   occuring in cells within a box of half width UNFBOX centered on the
C   cell in which a given visibility resides.  UNFBOX is implemented by
C   increasing the cellsize of the counting grid (i.e. reducing the grid
C   element size). The uniform weighting correction is done for the uv
C   cellsize defined by CELLSG, NXUNF, NYUNF and UNFBOX.  All channels
C   are included in the weighting although the summing boxes are
C   determined by the first channel.
C      This version will work on arbitrary sort orders but only if
C   sufficient "AP" memory is available.
C   Input uv data file in uv file DISKI, CNOSCI.
C   Output uv data file in uv file DISKO, CNOSCO.
C   Uses AIPS LUNs 18, 20 (all files closed on successful return)
C   Inputs:
C      DISKI    I       Input file disk number for cataloged files,
C                       if .LE. 0 => scratch file.
C      CNOSCI   I       Output file catalog slot number or /CFILES/
C                       scratch file number.
C      DISKO    I       Output file disk number for cataloged files,
C                       if .LE. 0 => scratch file.
C      CNOSCO   I       Output file catalog slot number or /CFILES/
C                       scratch file number.  Can be input file
C      CATUVR   R(256)  UV data catalog header record.
C      JBUFSZ   I       Size in bytes of buffers. Dimension of
C                       BUFF1,2,BUFF3  must be at least 4096 words.
C   From commons: (Includes DGDS, DMPR, DUVH)
C      UNFBOX   I       Half width of unif. wt. counting box size.
C      NVIS     I       Number of visibility measurments. (/UVHDR/)
C      LREC     I       Number of words per visibility record (/UVHDR/)
C      NCHAVG   I       Number of continuum channels to grid
C                       together. (Used to determine number of weights
C                       per visibility to correct.)
C      CHUV1    I       First channel number in file to correct weight
C                       (1 relative) (first ch. to be gridded)
C      FREQG(*) D       Frequencies of the channels
C      FREQUV   D       Reference frequency of the u, v, w
C      NGRDAT   L       If FALSE get map size, scaling etc. parms
C                       from the model map cat. header. If TRUE
C                       then the values filled in by GRDAT must
C                       already be filled into the common.
C      GUARDB       R(2) Fraction of UMAX and VMAX to blank on edge of
C                        field
C   The following must be provided if NGRDAT is .TRUE.
C      CELLSG   R(2)    The cell spacing in X and Y in arcseconds.
C      NXUNF    I       X-dimension (cells) of the map in RA to be used
C                       to determine uniform wt. counting box
C      NYUNF    I       Y-dimension (cells) of the map in Dec to be used
C                       to determine uniform wt. counting box
C   The following must be provided if NGRDAT is .FALSE.
C      CCDISK   I(16)   Disk numbers of the output images.
C      CCCNO    I(16)   Catalog slot numbers of output images.
C
C   Output:
C      BUFF1    R(*)    Working buffer
C      BUFF2    R(*)    Working buffer
C      IRET     I       Return error code, 0=>OK, error otherwise.
C   Usage Notes:
C    1) The input uvdata file is, with one exception, assumed to be
C     accurately described by the contents of CATUVR and the common
C     /UVHDR/ (include DUVH).  The exception is that the u, v and
C     w may refer to a different frequency.  The common input variable
C     FREQUV gives the reference frequency for the u, v, and w.
C    2) the contents of common /UVHDR/ (=include DUVH)
C     are filled in by UVPGET from the catalog header; UVPGET should
C     be called before calling UVTBUW.
C    3) if NGRDAT is .FALSE. then the properties (e.g. cellsize) of the
C     desired output image are assumed to be described in the catalog
C     header of the existant file pointed to by CCDISK,CCCNO(IFIELD).
C    4) the random parameters in the data should include, in order,
C     u, v, w, weight (optional), time (optional) and baseline
C     (optional).  The weights are required but may be passed
C     either as random parameters or as part of the regular data
C     array, CATUVR should tell which.
C-----------------------------------------------------------------------
      INTEGER   DISKI, CNOSCI, DISKO, CNOSCO, JBUFSZ, IRET
      REAL      BUFF1(*), BUFF2(*), CATUVR(256)
C
      INTEGER   BO, IER, II, IBIND, KBIND, OPTR, IXX, IYY, IU, JNPTR,
     *   NIO, NIOUT, NPOINT, INPTR, FIND1, FIND2, ILENBU, LUNUVI,
     *   LUNUVO, JJ, UV, AMAX, AMIN, GRID, IVMAX, WMAX, WMIN, KAP,
     *   ITEMP, APSIZE, VO, IVN2, SIZGRD, MAXREC, CNT, XMAX, NEEDED,
     *   NUMBIN, APLOC, IP, IVMIN, NXUNFS, NYUNFS, IUMIN, IUMAX, NUMWT,
     *   WTOFF, MXUNF, MYUNF, NWORD, ICELL, LTEMP(2), NEED, MSGSAV
      LOGICAL   F
      REAL      TEMP(10), FFRAC, ZSCLU, ZSCLV, SCLU, SCLV, XX, YY,
     *   MAXBLN, APSCAL, UMAX, VMAX, UUMAXG, VVMAXG, RCELL
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'INCS:PSTD.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DGDS.INC'
      INCLUDE 'INCS:DMPR.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DAPM.INC'
      EQUIVALENCE (ICELL, RCELL)
      DATA F /.FALSE./
      DATA UV /10/
      DATA VO, BO /0, 1/
      DATA LUNUVI, LUNUVO /18, 20/
C-----------------------------------------------------------------------
      IRET = 0
      IF ((GUARDB(1).LT.0.0) .OR. (GUARDB(1).GT.0.9)) GUARDB(1) = 0.0
      IF ((GUARDB(2).LT.0.0) .OR. (GUARDB(2).GT.0.9)) GUARDB(2) = 0.0
C                                       Get field info. if nec.
      IF (.NOT.NGRDAT) THEN
         CALL GRDAT (F, 0, CATUVR, BUFF1, IRET)
         IF (IRET.NE.0) GO TO 999
         END IF
C                                       Implement UNFBOX by reducing the
C                                       size of the grid.
      IF (UNFBOX.LT.0) UNFBOX = 0
      MXUNF = NXUNF / (1 + UNFBOX)
      MYUNF = NYUNF / (1 + UNFBOX)
C                                       Find where weight is.
C                                       WTOFF + => Offset in rec.
C                                       WTOFF - => Offset in vis.
      WTOFF = 0
      CALL AXEFND (8, 'WEIGHT  ', KIPTPN, CATUVR(KHPTP), WTOFF, IER)
      NUMWT = 1
C                                       If COMPLEX axis more then
C                                       2 long assume it has weight.
      IF (INCF.GT.2) THEN
C                                       Include offset to first channel
         WTOFF = NRPARM + (CHUV1-1) * INCF + 2
C                                       Set number of weights.
         NUMWT = NCHAVG
         END IF
C                                       Is there sufficient memory?
      SIZGRD = ((MXUNF / 2) + 1) * MYUNF
      NEEDED = SIZGRD + (JBUFSZ/2) + 100
      NEED = NEEDED / 1024 + 2
C                                       Grab AP.
      MSGSAV = MSGSUP
      MSGSUP = 32000
      CALL QINIT (NEED, 0, KAP)
      MSGSUP = MSGSAV
      IF ((KAP.EQ.0) .OR. (PSAPNW.EQ.0)) THEN
         NEED = NEED / 2
         CALL QINIT (NEED, 0, KAP)
         IF ((KAP.EQ.0) .OR. (PSAPNW.EQ.0)) THEN
            MSGTXT = 'UVTBUN CANNOT GET NEEDED MEMORY'
            IRET = 8
            GO TO 999
            END IF
         END IF
      APSIZE = PSAPNW * 1024
C                                       If only part of grid fits in AP
      IF (NEEDED .GT. APSIZE) THEN
C                                       rescale to fit in AP
         GRID   = (JBUFSZ/2) + 100
C                                       number of bins for weighting
         NUMBIN = APSIZE - GRID
C                                       convert grid size to AP available
         APSCAL = SQRT(REAL(NUMBIN)/REAL(SIZGRD))
C                                       scale uniform grid
         NXUNFS = APSCAL * MXUNF / 2.
         NYUNFS = APSCAL * MYUNF / 2.
C                                       make X and Y grid even
         NXUNFS = 2 * NXUNFS
         NYUNFS = 2 * NYUNFS
C                                       Recalculate grid size
         SIZGRD = ((NXUNFS / 2) + 1) * NYUNFS
C                                       tell user
         WRITE (MSGTXT,1002) NEEDED, APSCAL
         CALL MSGWRT (7)
C                                       All fits in AP
      ELSE
         NXUNFS = MXUNF
         NYUNFS = MYUNF
         END IF
C                                       Set AP pointers
      GRID   = APSIZE - SIZGRD - 1
      NUMBIN = SIZGRD
C                                       Clear grid
      CALL QVCLR ( GRID, 1, NUMBIN)
C                                       Tell about weighting
      WRITE (MSGTXT,1000) NXUNFS, NYUNFS, UNFBOX
      CALL MSGWRT (2)
C                                       Maximum number of visibility
C                                       points which fit in the AP.
      MAXREC = (GRID - UV) / LREC - 5
      IF (MAXREC.LE.10) THEN
         XMAX = - MAXREC * LREC
         IF (MAXREC.GT.0) XMAX = (11 - MAXREC) * LREC
         WRITE (MSGTXT,1060) XMAX
         IRET = 1
         GO TO 995
         END IF
      ILENBU = MIN (NVIS, MAXREC)
C                                       Open visibility file.
      CALL UVPREP ('READ', DISKI, CNOSCI, LUNUVI, FIND1, NVIS,
     *   LREC, ILENBU, JBUFSZ, BUFF1, NIO, IBIND, MAXBLN, IRET)
       IF (IRET.NE.0) GO TO 999
C                                       Load AP values: Maximum u, v.
      IUMAX   =  (NXUNFS / 2)
      IVMAX   =  (NYUNFS / 2 - 1)
C                                       Set AP location and value
      AMAX    = 0
      LTEMP(1) = IVMAX + ((NXUNFS / 2) * NYUNFS)
      TEMP(1) = LTEMP(1)
C                                       Minimum u, v.
      IUMIN   = 0
      IVMIN   = -(NYUNFS / 2)
      AMIN    = 1
      LTEMP(2) = IVMIN
      TEMP(2) = IVMIN
C                                       AP loc, value of Minimum sum wt.
      WMIN = 2
      TEMP(3) = 1.0E-20
C                                       AP loc, value of Maximum sum wt.
      WMAX = 3
      TEMP(4) = 1.0E20
C                                       Put limits in AP
      CALL QPUT (TEMP, 0, 10, 2)
      CALL QWD
      CALL QPUT (LTEMP, 0, 2, 1)
      CALL QWD
C                                       Scale factors for u,v to cells
      ZSCLU =  1.0 / (RAD2AS / (NXUNFS * ABS (CELLSG(1))))
      ZSCLV =  1.0 / (RAD2AS / (NYUNFS * CELLSG(2)))
C                                       Find frequency of obs
      IF (FREQG(CHUV1).GT.0.0) THEN
         FFRAC = FREQG(CHUV1)
C                                       Else, freq table not set,
      ELSE
         FFRAC = FREQ
         END IF
C                                       Frequency correction factor.
      FFRAC = (FFRAC / FREQUV) - 1.0D0
      SCLV = ZSCLV + FFRAC * ZSCLV
      SCLU = ZSCLU + FFRAC * ZSCLU
C                                       Convert grid maxs to lambda
      UMAX    =  (IUMAX + 0.5) / ABS (SCLU)
      VMAX    =  (IVMAX + 0.5) / ABS (SCLV)
      UUMAXG = (1. - GUARDB(1)) * UMAX
      VVMAXG = (1. - GUARDB(2)) * VMAX
C                                       Init counters
      NPOINT = 0
      CNT = 0
      INPTR = IBIND
C                                       Begin read through data loop
 150  CONTINUE
         IP = IBIND
C                                       Munge Buffer
         DO 200 IU = 1,NIO
            JNPTR = INPTR + ILOCU
C                                       Check if U neg.; if so then flip
C                                       data to other half plane.
            IF (BUFF1(JNPTR).LT.0.0) THEN
C                                       Flip U, V
               BUFF1(JNPTR) = -BUFF1(JNPTR)
               BUFF1(JNPTR+1) = -BUFF1(JNPTR+1)
               END IF
C                                       Scale to cell
            XX = BUFF1(JNPTR) * SCLU
            YY = BUFF1(JNPTR+1) * SCLV
            IYY = YY + SIGN (0.5, YY)
            IYY = MIN ( MAX (IYY, IVMIN), IVMAX)
            IXX = XX + 0.5
            IXX = MIN ( MAX (IXX, IUMIN), IUMAX)
            ICELL = (IXX * NYUNFS) + IYY - IVMIN + 1
C                                       Pass Histogram cell number
C                                       Note equivalence use
            BUFF1(IP)  = RCELL
            IP = IP + LREC
            NPOINT = NPOINT + 1
            INPTR = INPTR + LREC
 200        CONTINUE
C                                       Move to  AP.
         CNT = NPOINT
         NWORD = CNT * LREC
         CALL QPUT (BUFF1(IBIND), UV, NWORD, 1)
         NPOINT = 0
C                                       Count using QWTCNT
         IP = UV + WTOFF
         DO 210 JJ = 1,NUMWT
            CALL QWTCNT (UV, LREC, IP, LREC, GRID, CNT, NUMBIN, AMAX,
     *         AMIN)
            IP = IP + INCF
 210        CONTINUE
C                                       Read more data
         CALL UVDISK ('READ', LUNUVI, FIND1, BUFF1, NIO, IBIND, IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1070) IRET
            GO TO 995
            END IF
         INPTR = IBIND
C                                       If more data jump back to beginning
         IF (NIO.GT.0) GO TO 150
C                                       Conjugate U=0 row
C                                       Calc length of V=1 to V=N/2-1
      ITEMP = (NYUNFS/2) - 1
C                                       Cal position of V=N/2
      IVN2 = GRID + NYUNFS - 1
C                                       Add V<0 to V>0
      CALL QVADD (GRID+1, 1, IVN2, -1, IVN2, -1, ITEMP)
      CALL QWAIT
C                                       Copy V>0 to V<0
      CALL QVMOV (IVN2, -1, GRID+1, 1, ITEMP)
      CALL QWAIT
C                                       Clip
      CALL QVCLIP (GRID, 1, WMIN, WMAX, GRID, 1, NUMBIN)
      CALL QWAIT
C                                       Correct data.
      ILENBU = 0
C                                       Open visibility file for writing
      CALL UVPREP ('WRIT', DISKO, CNOSCO, LUNUVO, FIND2, NVIS, LREC,
     *   ILENBU, JBUFSZ, BUFF2, NIO, KBIND, MAXBLN, IRET)
      IF (IRET.NE.0) GO TO 999
      OPTR = KBIND
C                                       Initialize vis. file for read.
      CALL UVINIT ('READ', LUNUVI, FIND1, NVIS, VO, LREC, ILENBU,
     *   JBUFSZ, BUFF1, BO, IBIND, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1040) IRET
         GO TO 995
         END IF
C                                       Init Counters
      NPOINT = 0
C                                       AP Loc of U=V=0
      APLOC = GRID + (NYUNFS / 2)
C                                       Loop through data.
 750     CALL UVDISK ('READ', LUNUVI, FIND1, BUFF1, NIO, IBIND, IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1070) IRET
            GO TO 995
            END IF
         INPTR = IBIND
         IF (NIO.LE.0) GO TO 900
         IP = IBIND
C                                       Copy to output buffer
         CALL RCOPY (NIO*LREC, BUFF1(IBIND), BUFF2(KBIND))
C                                       Munge Buffer
         DO 800 IU = 1,NIO
            JNPTR = INPTR + ILOCU
C                                       Check if U neg.; if so then flip
C                                       data to other half plane.
            IF (BUFF1(JNPTR).LT.0.0) THEN
C                                       Flip U, V
               BUFF1(JNPTR) = -BUFF1(JNPTR)
               BUFF1(JNPTR+1) = -BUFF1(JNPTR+1)
               END IF
C                                       Scale to cell
            XX = BUFF1(JNPTR) * SCLU
            YY = BUFF1(JNPTR+1) * SCLV
            IYY = YY + SIGN (0.5, YY)
            IXX = XX + 0.5
            IYY = MIN (MAX (IYY, IVMIN), IVMAX)
            IXX = MIN (MAX (IXX, IUMIN), IUMAX)
            ICELL = (IXX * NYUNFS) + IYY - IVMIN
C                                       Pass Histogram 0-rel cell number
C                                       Note use of equivalence
            BUFF1(IP)  = RCELL
            IP = IP + 1
            NPOINT = NPOINT + 1
            INPTR = INPTR + LREC
            OPTR = OPTR + LREC
 800        CONTINUE
C                                       Move to AP as INTEGER!
         CNT = NPOINT
         CALL QPUT (BUFF1(IBIND), UV, CNT, 1)
C                                       Look up counts
         CALL QWD
         CALL QVINDE (GRID, UV, 1, UV, 1, CNT)
         CALL QWR
         CALL QGET (BUFF1(IBIND), UV, CNT, 2)
         CALL QWD
         NPOINT = 0
C                                       Loop over buffer
         OPTR = KBIND
         DO 830 II = 1,NIO
C                                       Correct by weights
            IP = OPTR + WTOFF
C                                       Read U,V for grid test
            XX = ABS( BUFF2(OPTR))
            YY = ABS( BUFF2(OPTR+1))
C                                       If inside grid
            IF ((XX.LE.UUMAXG) .AND. (YY.LE.VVMAXG)) THEN
               IF (BUFF1(IBIND+II-1).LE.0.0) BUFF1(IBIND+II-1) = 1.0
C                                       Scale change weight
               DO 820 JJ = 1,NUMWT
                  IF (BUFF2(IP).GT.0.0)
     *               BUFF2(IP) = BUFF2(IP) / BUFF1(IBIND+II-1)
                  IP = IP + INCF
 820              CONTINUE
               END IF
            OPTR = OPTR + LREC
 830        CONTINUE
C                                      Write
         NIOUT = NIO
         CALL UVDISK ('WRIT', LUNUVO, FIND2, BUFF2, NIOUT, KBIND, IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1505) IRET
            GO TO 995
            END IF
         OPTR = KBIND
C                                       Read more data
         IF (NIO.GT.0) GO TO 750
 900  NIOUT = 0
      CALL UVDISK ('FLSH', LUNUVO, FIND2, BUFF2, NIOUT, KBIND, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1505) IRET
         GO TO 995
         END IF
C                                       Close files.
      CALL ZCLOSE (LUNUVI, FIND1, IER)
      CALL ZCLOSE (LUNUVO, FIND2, IER)
      GO TO 999
C                                       Error
 995  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('UVTBUW: Weighting grid = ',I5,' X ',I5,', Box = ',I3)
 1002 FORMAT ('UVTBUW: Need ',I9,' words AP memory. Scaling by',F8.4)
 1040 FORMAT ('UVTBUW: ERROR',I3,' INIT. VIS. FILE FOR READ')
 1060 FORMAT ('UVTBUW:',I8,' TOO FEW AP WORDS AVAILABLE')
 1070 FORMAT ('UVTBUW: ERROR',I3,' READING VIS RECORD')
 1505 FORMAT ('UVTBUW: ERROR',I3,' WRITING VIS. RECORD')
      END
