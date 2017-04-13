      SUBROUTINE UVUNIF (DISKI, CNOSCI, DISKO, CNOSCO, SCRWRK, CATUVR,
     *   JBUFSZ, BUFF1, BUFF2, BUFF3, IRET)
C-----------------------------------------------------------------------
C! Determines and applies uniform weighting to a uv data set.
C# AP-util UV
C-----------------------------------------------------------------------
C;  Copyright (C) 1995, 1997, 2006, 2008
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
C   UVUNIF computes uniform weighting corrections and applies them to
C   the weights in the visibility data base.  The visibility weights
C   are divided by the number of visibilities occuring in cells within
C   a box of half width UNFBOX centered on the cell in which a given
C   visibility resides.  Does the uniform weighting correction for the
C   uv cellsize defined by CELLSG, NXUNF, NYUNF and UNFBOX; and only
C   includes the first channel.
C   Input uv data file in uv file DISKI, CNOSCI.
C   Output uv data file in uv file DISKO, CNOSCO.
C   Uses AIPS LUNs 18, 20, 21 (all files closed on successful return)
C   Inputs:
C      SCRWRK   I       /CFILES/ file number for work file,
C                       Big enough for NXUNFx(NYUNF/2+1) image.
C      DISKI    I       Input file disk number for cataloged files,
C                       if .LE. 0 => scratch file.
C      CNOSCI   I       Output file catalog slot number or /CFILES/
C                       scratch file number.
C      DISKO    I       Output file disk number for cataloged files,
C                       if .LE. 0 => scratch file.
C      CNOSCO   I       Output file catalog slot number or /CFILES/
C                       scratch file number.
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
C      CHUV1    C*2     First channel number in file to correct weight
C                       (1 relative) (first ch. to be gridded)
C      FREQG(*) D       Frequencies of the channels
C      FREQUV   D       Reference frequency of the u, v, w
C      NGRDAT   L       If FALSE get map size, scaling etc. parms
C                       from the model map cat. header. If TRUE
C                       then the values filled in by GRDAT must
C                       already be filled into the common.
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
C      BUFF3    R(*)    Working buffer
C      IRET     I       Return error code, 0=>OK, error otherwise.
C   Usage Notes:
C    1) The input uvdata file is, with one exception, assumed to be
C     accurately described by the contents of CATUVR and the common
C     /UVHDR/ (include DUVH).  The exception is that the u, v and
C     w may refer to a different frequency.  The common input variable
C     FREQUV gives the reference frequency for the u, v, and w.
C    2) the contents of common /UVHDR/ (=include DUVH)
C     are filled in by UVPGET from the catalog header; UVPGET should
C     be called before calling UVUNIF.
C    3) if NGRDAT is .FALSE. then the properties (e.g. cellsize) of the
C     desired output image are assumed to be described in the catalog
C     header of the existant file pointed to by CCDISK,CCCNO(IFIELD).
C    4) the random parameters in the data should include, in order,
C     u, v, w, weight (optional), time (optional) and baseline
C     (optional).  The weights are required but may be passed
C     either as random parameters or as part of the regular data
C     array, CATUVR should tell which.
C    5) The uniform correction made is to divide the weight of each
C     visibility by the number of occurrences in its counting box
C     irregardless of the weights of the visibilities.
C-----------------------------------------------------------------------
      INTEGER   DISKI, CNOSCI, DISKO, CNOSCO, SCRWRK, JBUFSZ, IRET
      REAL      BUFF1(*), BUFF2(*), BUFF3(*), CATUVR(256)
C
      CHARACTER PHNAME*48
      INTEGER   BO, IER, INIO, WIN(4), I, II, IBIND, JBIND, KBIND, OPTR,
     *   U, INCNT, IU, J, JNPTR, KROW, LIM, LIMIT, NIO, NIOUT, NPOINT,
     *   NXUNF2, NYUNF2, NYUNF1, INDEX, INPTR, FIND1, FIND2, FIND3,
     *   JJPTR, ILENBU, LUNUVI, LUNUVO, LUNWT, WTOFF, IERR, LVIS, NUMWT,
     *   UV, AMAX, AMIN, GRID, ROW, NROW, ROW2, END1, IV, END2, END3,
     *   WMAX, NMOV, IROW, WMIN, NUM, IDATA, KTEMP, KAP, LLREC, JCNT,
     *   JNYUNF, ITEMP, APSCLV, APSIZ, VO, BO4, MAXREC, CNT, XMAX, IVN2,
     *   NEED
      LOGICAL   T, F, ENDROW
      REAL      TEMP(10), SSCLV, SSCLU, UMIN, FFRAC, ZSCLU, ZSCLV,
     *   XNDEX, MAXBLN, UUMAXG, VVMAXG
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'INCS:PSTD.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DGDS.INC'
      INCLUDE 'INCS:DMPR.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DAPM.INC'
      DATA T, F /.TRUE.,.FALSE./
      DATA WIN /4*0/
      DATA UV /10/
      DATA BO4, VO, BO /1, 0, 1/
      DATA LUNUVI, LUNUVO, LUNWT /18, 20, 21/
C-----------------------------------------------------------------------
      IF ((GUARDB(1).LT.0.0) .OR. (GUARDB(1).GT.0.9)) GUARDB(1) = 0.0
      IF ((GUARDB(2).LT.0.0) .OR. (GUARDB(2).GT.0.9)) GUARDB(2) = 0.0
C                                       Get field info. if nec.
      IF (.NOT.NGRDAT) THEN
         CALL GRDAT (T, 0, CATUVR, BUFF1, IRET)
         IF (IRET.NE.0) GO TO 999
         END IF
C                                       Tell about weighting
      WRITE (MSGTXT,1000) NXUNF, NYUNF, UNFBOX
      CALL MSGWRT (4)

      NYUNF1 = NYUNF - 1
      JNYUNF = NYUNF
      NXUNF2 = NXUNF / 2
      NYUNF2 = NYUNF / 2
C                                       Single buffer uv data.
      ILENBU = MAX(((JBUFSZ-2*NBPS) / 2) / LREC, 1)
      LLREC = LREC
C                                       Open visibility files for Read
      CALL UVPREP ('READ', DISKI, CNOSCI, LUNUVI, FIND1, NVIS,
     *   LREC, ILENBU, JBUFSZ, BUFF1, NIO, IBIND, MAXBLN, IRET)
       IF (IRET.NE.0) GO TO 999
C                                       Open grid file.
      CALL ZPHFIL ('SC', SCRVOL(SCRWRK), SCRCNO(SCRWRK), 1, PHNAME,
     *   IRET)
      CALL ZOPEN (LUNWT, FIND2, SCRVOL(SCRWRK), PHNAME, T, T, T, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1030) IRET
         GO TO 995
         END IF
C                                       Init. weight file.
      CALL MINIT ('WRIT', LUNWT, FIND2, NYUNF, NXUNF2, WIN, BUFF3,
     *   JBUFSZ, BO, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1050) IRET
         GO TO 995
         END IF
C                                       Load AP values: Maximum v.
      AMAX = 0
      TEMP(1) = NYUNF / 2 - 1
C                                       Minimum v.
      AMIN = 1
      TEMP(2) = - NYUNF / 2
C                                       Minimum count.
      WMIN = 2
      TEMP(3) = 1.0
C                                       Maximum count
      WMAX = 3
      TEMP(4) = 1.0E20
      IV = UV + 1
C                                       Scaling factors for u, v
C                                       to cells.
      APSCLV = 4
      ZSCLU = 1.0 / (RAD2AS / (NXUNF * ABS (CELLSG(1))))
      ZSCLV = 1.0 / (RAD2AS / (NYUNF * CELLSG(2)))
C                                       Find frequency of obs
      IF (FREQG(CHUV1).GT.0.0) THEN
         FFRAC = FREQG(CHUV1)
C                                       Else, freq table not set,
      ELSE
         FFRAC = FREQ
         END IF
C                                       Frequency correction factor.
      FFRAC = (FFRAC / FREQUV) - 1.0D0
      TEMP(5) = ZSCLV + FFRAC * ZSCLV
      SSCLV = ZSCLV + FFRAC * ZSCLV
      SSCLU = 1.0 / (ZSCLU + FFRAC * ZSCLU)
      UUMAXG = (1. - GUARDB(1)) * (NXUNF / 2) * ABS (SSCLU)
      VVMAXG = (1. - GUARDB(2)) * (NYUNF / 2) / ABS (SSCLV)
C                                       Set AP data freq. increment.
      LVIS = INCF
C                                       Find where weight is.
C                                       WTOFF + => Offset in rec.
C                                       WTOFF - => Offset in vis.
      WTOFF = 0
      CALL AXEFND (8, 'WEIGHT  ', KIPTPN, CATUVR(KHPTP), WTOFF, IERR)
      NUMWT = 1
C                                       If COMPLEX axis more then
C                                       2 long assume it has weight.
      IF (INCF.GT.2) THEN
C                                       Include offset to first channel
         WTOFF = NRPARM + (CHUV1-1) * INCF + 2
C                                       Set number of weights.
         NUMWT = NCHAVG
         END IF
C                                       Grab AP.
      NROW = 2 * UNFBOX + 1
      NEED = (NROW + 2) * NYUNF
      NEED = NEED/1024 + 2
      CALL QINIT (NEED, 0, KAP)
      IF ((KAP.EQ.0) .OR. (PSAPNW.LE.0)) THEN
         IRET = 10
         MSGTXT = 'UVUNIF: DID NOT GET ANY AP MEMORY'
         CALL MSGWRT (8)
         GO TO 999
         END IF
      APSIZ = PSAPNW * 1024
      CALL QPUT (TEMP, 0, 10, 2)
      CALL QWD
C                                       Set pointers for AP
      GRID = (APSIZ-1) - NROW * JNYUNF
C                                       Set pointer for temporary row
      ROW = GRID - NYUNF
C                                       Set pointers for output buffer
      ROW2 = ROW - NYUNF
      END1 = ROW2 + NYUNF - 1
C                                       Set pointer for shifting grid
      END2 = GRID + JNYUNF * NROW
      END3 = END2 - NYUNF
C                                      Determine no. points to shift.
      NMOV = NROW * NYUNF
C                                       Clear AP
      NUM = NROW * JNYUNF
      CALL QVCLR (GRID, 1, NUM)
      CALL QWR
      CALL QWD
C                                       Determine max. no. of vis.
C                                       points which will fit in AP.
      MAXREC = (ROW2 - 10) / LREC - 5
C                                       Make sure MAXREC.GT.0
      IF (MAXREC.LE.0) THEN
         XMAX = - MAXREC * LREC
         WRITE (MSGTXT,1060) XMAX
         IRET = 1
         GO TO 995
         END IF
C                                       Init Counters
      INCNT = 1
      INPTR = IBIND
C                                       Begin counting loop.
      DO 200 I = 1,NXUNF2
         IU = NXUNF2 - I
         UMIN = (IU - 0.5) * SSCLU
         IDATA = UV
         CNT = 0
C                                       Return to here if more than one
C                                       record is loaded at a time.
  100    ENDROW = T
         NPOINT = 0
C                                       Check if all data read.
      IF (NIO.LE.0) GO TO 140
C                                       If some data on this row.
         IF (ABS (BUFF1(INPTR)).LT.UMIN) GO TO 140
C                                       If buffer fills on this row.
         LIM = INCNT + MAXREC - CNT - 1
         LIM = MIN (LIM, NIO)
         JNPTR = INPTR
         DO 110 II = INCNT,LIM
C                                       Jump out on new row
            IF (ABS (BUFF1(JNPTR)).LT.UMIN) GO TO 120
               NPOINT = NPOINT + 1
               JJPTR = JNPTR
               JNPTR = JNPTR + LREC
C                                       Check if U neg.; if so then flip
C                                       to other half plane.
               IF (BUFF1(JJPTR).LT.0.0) THEN
                  BUFF1(JJPTR) = -BUFF1(JJPTR)
                  BUFF1(JJPTR+1) = -BUFF1(JJPTR+1)
                  END IF
 110           CONTINUE
C                                       Rest of record is on same row.
         ENDROW = F
 120     CONTINUE
         CNT = CNT + NPOINT
C                                       Load into AP.
         CALL QWR
         KTEMP = NPOINT
         KTEMP = KTEMP * LREC
         CALL QPUT (BUFF1(INPTR), IDATA, KTEMP, 2)
         IDATA = IDATA + KTEMP
         INPTR = INPTR + NPOINT * LREC
         INCNT = INCNT + NPOINT
C                                       Check if AP full or row finished
         IF ((ENDROW) .OR. (CNT.GE.MAXREC)) GO TO 140
C                                       Read next record.
  125    INCNT = 1
C                                       Check if all records read.
         IF (NIO.LE.0) THEN
            ENDROW = T
         ELSE
            CALL UVDISK ('READ', LUNUVI, FIND1, BUFF1, NIO, IBIND, IRET)
            IF (IRET.NE.0) THEN
               WRITE (MSGTXT,1070) IRET
               GO TO 995
               END IF
            INPTR = IBIND
C                                       Loop back
            GO TO 100
C                                       end if more data read
            END IF
  140       CONTINUE
C                                       if data to Grid
         IF (CNT.GT.0) THEN
            CALL QWD
C                                       Use HIST for counting.
            JCNT = CNT
C                                       Scale v to cells.
            CALL QVSMUL (IV, LLREC, APSCLV, IV, LLREC, JCNT)
            CALL QHIST (IV, LLREC, GRID, JCNT, JNYUNF, AMAX, AMIN)
            IDATA = UV
            CNT = 0
C                                       Check if row finished.
            IF (.NOT.ENDROW) THEN
               IF (INCNT.LT.NIO) GO TO 100
               GO TO 125
               END IF
C                                       end if data to grid
            END IF
C                                       Row finished, process.
  145    CONTINUE
         U = IU + NROW / 2
C                                       If U = 0 conjugate row.
         IF (U.EQ.0) THEN
C                                       Calc length of V=1 to V=N/2-1
            ITEMP = (NYUNF/2) - 1
C                                       Cal position of V=N/2
            IVN2 = GRID + NYUNF - 1
C                                       Add V<0 to V>0
            CALL QVADD (GRID+1, 1, IVN2, -1, IVN2, -1, ITEMP)
C                                       Copy V>0 to V<0
            CALL QVMOV (IVN2, -1, GRID+1, 1, ITEMP)
            END IF
C                                       Write rows after start of file
         IF (U.LT.NXUNF2) THEN
C                                       Sum rows.
            CALL MDISK ('WRIT', LUNWT, FIND2, BUFF3, JBIND, IRET)
            IF (IRET.NE.0) THEN
               WRITE (MSGTXT,1160) IRET, I
               GO TO 995
               END IF
            CALL QVMOV (GRID, 1, ROW, 1, JNYUNF)
C                                       If a weighting box
            IF (UNFBOX.GT.0) THEN
               LIMIT = NROW
               DO 160 J = 2,LIMIT
                  IROW = GRID + JNYUNF * (J-1)
                  CALL QVADD (IROW, 1, ROW, 1, ROW, 1, JNYUNF)
  160             CONTINUE
               END IF
C                                       Boxsum sum of rows.
            CALL QBOXSU (ROW, 1, NROW, ROW2, 1, JNYUNF)
C                                       Make sure values reasonable.
            CALL QVCLIP (ROW2, 1, WMIN, WMAX, ROW2, 1, JNYUNF)
C                                       Read out row.
            CALL QWR
            CALL QGET (BUFF3(JBIND), ROW2, JNYUNF, 2)
            CALL QWD
C                                       Check if not last row.
            IF (I.NE.NXUNF2) THEN
C                                       Prepare AP for next row.
               CALL QVMOV (END3, -1, END2, -1, NMOV)
               CALL QVCLR (GRID, 1, JNYUNF)
               END IF
         ELSE
C                                       Else before grid file starts
C                                       Prepare AP for next row.
            CALL QVMOV (END3, -1, END2, -1, NMOV)
            CALL QVCLR (GRID, 1, JNYUNF)
C                                       End if after grid file starts
            END IF
C                                       End for all Rows in grid loop
 200     CONTINUE
C                                       If a weighting box
      IF (UNFBOX.GT.0) THEN
C                                       Finish reading out grid
         DO 300 I = 1,UNFBOX
            CALL MDISK ('WRIT', LUNWT, FIND2, BUFF3, JBIND, IRET)
            IF (IRET.NE.0) THEN
               WRITE (MSGTXT,1160) IRET, I
               GO TO 995
               END IF
C                                       Add conjugate to next row.
            IROW = GRID + JNYUNF * I + 1
            KTEMP = IROW + 1
            ITEMP = NYUNF - 1
            CALL QVMOV (KTEMP, 1, END1, -1, ITEMP)
            CALL QVMOV (IROW, 1, ROW2, 1, 1)
            CALL QVADD (IROW, 1, ROW2, 1, IROW, 1, JNYUNF)
C                                       Sum rows.
            CALL QVMOV (GRID, 1, ROW, 1, JNYUNF)
            LIMIT = NROW - I
            DO 210 J = 2,LIMIT
               IROW = GRID + JNYUNF * (J-1)
               CALL QVADD (IROW, 1, ROW, 1, ROW, 1, JNYUNF)
  210          CONTINUE
C                                       Boxsum sum of rows.
            CALL QBOXSU (ROW, 1, NROW, ROW2, 1, JNYUNF)
C                                       Make sure values reasonable.
            CALL QVCLIP (ROW2, 1, WMIN, WMAX, ROW2, 1, JNYUNF)
C                                       Read row back out.
            CALL QWR
            CALL QGET (BUFF3(JBIND), ROW2, JNYUNF, 2)
            CALL QWD
  300       CONTINUE
C                                       End if summing weight in a box
         END IF
C                                       Finish write
      CALL MDISK ('FINI', LUNWT, FIND2, BUFF3, JBIND, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1160) IRET, I
         GO TO 995
         END IF
      CALL QRLSE
C                                       Apply corrections.
C                                       Open visibility files for Write
      KBIND = 1
      CALL UVPREP ('WRIT', DISKO, CNOSCO, LUNUVO, FIND3, NVIS,
     *   LREC, ILENBU, JBUFSZ, BUFF2, NIOUT, KBIND, MAXBLN, IRET)
       IF (IRET.NE.0) GO TO 999
      OPTR = KBIND
C                                       Re-Init vis. file for read.
      CALL UVINIT ('READ', LUNUVI, FIND1, NVIS, VO, LREC, ILENBU,
     *   JBUFSZ, BUFF1, BO4, IBIND, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1040) IRET
         GO TO 995
         END IF
C                                       Initialize grid file for read.
      CALL MINIT ('READ', LUNWT, FIND2, NYUNF, NXUNF2, WIN, BUFF3,
     *   JBUFSZ, BO, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1050) IRET
         GO TO 995
         END IF
C                                       Read first row of grid.
      KROW = NXUNF / 2 - 1
      CALL MDISK ('READ', LUNWT, FIND2, BUFF3, JBIND, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1360) IRET, KROW
         GO TO 995
         END IF
      UMIN = (KROW - 0.5) * SSCLU
C                                       Begin weighting loop.
 400  CONTINUE
C                                       Read vis record.
      CALL UVDISK ('READ', LUNUVI, FIND1, BUFF1, INIO, IBIND, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1070) IRET
         GO TO 995
         END IF
      INPTR = IBIND
C                                       Exit if end of UV data
      IF (INIO.LE.0) GO TO 510
C                                       Else process all points in buff
      DO 500 I = 1,INIO
C                                       Copy vis from in to output buff
         CALL RCOPY (LREC, BUFF1(INPTR),  BUFF2(OPTR))
C                                       While data not on row
  430    CONTINUE
C                                       if data on lower row.
         IF (ABS (BUFF2(OPTR)).LT.UMIN) THEN
C                                       Read next grid row.
            KROW = KROW - 1
C                                       Check if gone too far.
            IF (KROW.LT.0) THEN
               WRITE (MSGTXT,1435) KROW
               CALL MSGWRT (7)
C                                       Read next row in
            ELSE
               CALL MDISK ('READ', LUNWT, FIND2, BUFF3, JBIND, IRET)
               IF (IRET.NE.0) THEN
                  WRITE (MSGTXT,1360) IRET, KROW
                  GO TO 995
                  END IF
C                                       Re-calc UV points on this row
               UMIN = (KROW - 0.5) * SSCLU
C                                       while row not needed
               GO TO 430
               END IF
            END IF
C                                       Apply uniform weight correction
C                                       to weight.
         XNDEX = BUFF2(OPTR+1) * SSCLV
C                                       Negate V if on wrong plane half
         IF (BUFF2(OPTR).LT.0.0) XNDEX = -XNDEX
         INDEX = XNDEX + NYUNF2 + 0.5
         INDEX = MAX( MIN (INDEX, NYUNF1), 0)
C                                       Loop thru weights
         IF ((ABS(BUFF2(OPTR)).LE.UUMAXG) .AND.
     *      (ABS(BUFF2(OPTR+1)).LE.VVMAXG)) THEN
            JJPTR = OPTR + WTOFF
            DO 470 II = 1,NUMWT
               BUFF2(JJPTR) = BUFF2(JJPTR) / BUFF3(INDEX+JBIND)
               JJPTR = JJPTR + LVIS
  470          CONTINUE
            END IF
         OPTR = OPTR + LREC
         INPTR = INPTR + LREC
  500    CONTINUE
C                                      Write
      NIOUT = INIO
      CALL UVDISK ('WRIT', LUNUVO, FIND3, BUFF2, NIOUT, KBIND, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1505) IRET
         GO TO 995
         END IF
      OPTR = KBIND
C                                       While more uv data, loop back.
      GO TO 400
C                                       Finish write.
 510  CONTINUE
      NIOUT = 0
      CALL UVDISK ('FLSH', LUNUVO, FIND3, BUFF2, NIOUT, KBIND, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1505) IRET
         GO TO 995
         END IF
C                                       Close files.
      CALL ZCLOSE (LUNUVI, FIND1, IER)
      CALL ZCLOSE (LUNWT, FIND2, IER)
      CALL ZCLOSE (LUNUVO, FIND3, IER)
      GO TO 999
C                                       Error
 995  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('UVUNIF: Weighting grid = ',I5,' X ',I5,', Box = ',I5)
 1030 FORMAT ('UVUNIF: ERROR',I3,' OPENING GRID FILE FOR WRITE')
 1040 FORMAT ('UVUNIF: ERROR',I3,' INIT. VIS. FILE FOR READ')
 1050 FORMAT ('UVUNIF: ERROR',I3,' INIT GRID FILE FOR WRITE')
 1060 FORMAT ('UVUNIF:',I8,' TOO FEW AP WORDS AVAILABLE')
 1070 FORMAT ('UVUNIF: ERROR',I3,' READING VIS RECORD')
 1160 FORMAT ('UVUNIF: ERROR',I3,' WRITING GRID ROW',I5)
 1360 FORMAT ('UVUNIF: ERROR',I3,' READING GRID ROW',I5)
 1435 FORMAT ('UVUNIF: ATTEMPTED TO READ ROW',I6)
 1505 FORMAT ('UVUNIF: ERROR',I3,' WRITING VIS. RECORD')
      END
